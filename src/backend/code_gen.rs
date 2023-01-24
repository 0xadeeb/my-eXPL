use crate::ast::*;
use crate::symbol_table::Symbol;
use crate::utils::{
    err_from_str, label::LabelGenerator, loop_util::LoopStack, register::RegisterPool,
};
use crate::SYMBOL_TABLE;
use lazy_static::lazy_static;
use std::{
    error::Error,
    fs::{File, OpenOptions},
    io::Write,
    path::PathBuf,
    sync::Mutex,
};

lazy_static! {
    static ref REGISTERS: Mutex<RegisterPool> = Mutex::new(RegisterPool::default());
    static ref LABELS: Mutex<LabelGenerator> = Mutex::new(LabelGenerator::default());
    static ref LP: Mutex<LoopStack> = Mutex::new(LoopStack::default());
}

fn pre_call(fd: &mut File) {
    for reg in REGISTERS.lock().unwrap().accquired_regs() {
        writeln!(fd, "PUSH R{}", reg).unwrap();
    }
}

fn post_call(fd: &mut File) {
    for reg in REGISTERS.lock().unwrap().accquired_regs().rev() {
        writeln!(fd, "POP R{}", reg).unwrap();
    }
}

fn evaluate(node: &Tnode, fd: &mut File) -> Result<Option<u8>, Box<dyn Error>> {
    match node {
        Tnode::Constant { value, .. } => {
            let reg1 = match REGISTERS.lock().unwrap().get_reg() {
                Some(r) => r,
                None => return Err(err_from_str("No registers left!")),
            };
            writeln!(fd, "MOV R{}, {}", reg1, value)?;
            Ok(Some(reg1))
        }
        Tnode::Var {
            symbol,
            access,
            ref_type,
            ..
        } => {
            let reg1 = match REGISTERS.lock().unwrap().get_reg() {
                Some(r) => r,
                None => return Err(err_from_str("No registers left!")),
            };
            match symbol {
                var @ Symbol::Variable { .. } => {
                    writeln!(fd, "MOV R{}, {}", reg1, var.base_address())?;
                }
                arr @ Symbol::Array { dim, .. } => {
                    writeln!(fd, "MOV R{}, {}", reg1, arr.base_address())?;
                    for (i, exp) in access.iter().enumerate() {
                        let reg2 = evaluate(exp, fd)?.unwrap();
                        for d in dim[(i + 1)..].iter() {
                            writeln!(fd, "MUL R{}, {}", reg2, d)?;
                        }
                        writeln!(fd, "ADD R{}, R{}", reg1, reg2)?;
                        REGISTERS.lock().unwrap().free_reg(reg2);
                    }
                }
            }
            if let RefType::RHS = ref_type {
                writeln!(fd, "MOV R{}, [R{}]", reg1, reg1)?;
            }
            Ok(Some(reg1))
        }
        Tnode::DeRefOperator { ref_type, var, .. } => {
            let reg1 = evaluate(var, fd)?.unwrap();
            if let RefType::RHS = ref_type {
                writeln!(fd, "MOV R{}, [R{}]", reg1, reg1)?;
            }
            Ok(Some(reg1))
        }
        Tnode::RefOperator { var, .. } => {
            let reg1 = match REGISTERS.lock().unwrap().get_reg() {
                Some(r) => r,
                None => return Err(err_from_str("No registers left!")),
            };
            writeln!(fd, "MOV R{}, {}", reg1, var.get_address()?)?;
            Ok(Some(reg1))
        }
        Tnode::BinaryOperator { op, lhs, rhs, .. } => {
            let reg1 = evaluate(lhs, fd)?.unwrap();
            let reg2 = evaluate(rhs, fd)?.unwrap();
            match op {
                BinaryOpType::Add => writeln!(fd, "ADD R{}, R{}", reg1, reg2)?,
                BinaryOpType::Sub => writeln!(fd, "SUB R{}, R{}", reg1, reg2)?,
                BinaryOpType::Mul => writeln!(fd, "MUL R{}, R{}", reg1, reg2)?,
                BinaryOpType::Div => writeln!(fd, "DIV R{}, R{}", reg1, reg2)?,
                BinaryOpType::Mod => writeln!(fd, "MOD R{}, R{}", reg1, reg2)?,
                BinaryOpType::EQ => writeln!(fd, "EQ R{}, R{}", reg1, reg2)?,
                BinaryOpType::NE => writeln!(fd, "NE R{}, R{}", reg1, reg2)?,
                BinaryOpType::GT => writeln!(fd, "GT R{}, R{}", reg1, reg2)?,
                BinaryOpType::GE => writeln!(fd, "GE R{}, R{}", reg1, reg2)?,
                BinaryOpType::LT => writeln!(fd, "LT R{}, R{}", reg1, reg2)?,
                BinaryOpType::LE => writeln!(fd, "LE R{}, R{}", reg1, reg2)?,
            }
            REGISTERS.lock().unwrap().free_reg(reg2);
            Ok(Some(reg1))
        }
        Tnode::Read { var, .. } => {
            let reg1 = evaluate(var, fd)?.unwrap();
            pre_call(fd);
            let reg2 = match REGISTERS.lock().unwrap().get_reg() {
                Some(r) => r,
                None => return Err(err_from_str("No registers left!")),
            };
            writeln!(fd, "MOV R{}, \"Read\"", reg2)?;
            writeln!(fd, "PUSH R{}", reg2)?;
            writeln!(fd, "MOV R{}, -1", reg2)?;
            writeln!(fd, "PUSH R{}", reg2)?;
            writeln!(fd, "MOV R{}, R{}", reg2, reg1)?;
            writeln!(fd, "PUSH R{}", reg2)?;
            writeln!(fd, "ADD SP, 2")?;
            writeln!(fd, "CALL 0")?;
            writeln!(fd, "SUB SP, 5")?;
            REGISTERS.lock().unwrap().free_reg(reg2);
            post_call(fd);
            REGISTERS.lock().unwrap().free_reg(reg1);
            Ok(None)
        }
        Tnode::Write { expression, .. } => {
            let reg1 = evaluate(expression, fd)?.unwrap();
            pre_call(fd);
            let reg2 = match REGISTERS.lock().unwrap().get_reg() {
                Some(r) => r,
                None => return Err(err_from_str("No registers left!")),
            };

            writeln!(fd, "MOV R{}, \"Write\"", reg2)?;
            writeln!(fd, "PUSH R{}", reg2)?;
            writeln!(fd, "MOV R{}, -2", reg2)?;
            writeln!(fd, "PUSH R{}", reg2)?;
            writeln!(fd, "PUSH R{}", reg1)?;
            writeln!(fd, "ADD SP, 2")?;
            writeln!(fd, "CALL 0")?;
            writeln!(fd, "SUB SP, 5")?;

            REGISTERS.lock().unwrap().free_reg(reg2);
            post_call(fd);
            REGISTERS.lock().unwrap().free_reg(reg1);
            Ok(None)
        }
        Tnode::Asgn { lhs, rhs, .. } => {
            let reg1 = evaluate(lhs, fd)?.unwrap();
            let reg2 = evaluate(rhs, fd)?.unwrap();
            writeln!(fd, "MOV [R{}], R{}", reg1, reg2)?;
            REGISTERS.lock().unwrap().free_reg(reg2);
            REGISTERS.lock().unwrap().free_reg(reg1);
            Ok(None)
        }
        Tnode::Continue => {
            if let Some(label) = LP.lock().unwrap().condition_label() {
                writeln!(fd, "JMP <L{}>", label)?;
            }
            Ok(None)
        }
        Tnode::Break => {
            if let Some(label) = LP.lock().unwrap().exit_label() {
                writeln!(fd, "JMP <L{}>", label)?;
            }
            Ok(None)
        }
        Tnode::If {
            condition,
            if_stmt,
            else_stmt,
            ..
        } => {
            let mut labels = LABELS.lock().unwrap();
            let label1 = labels.get();
            let label2 = labels.get();
            drop(labels);
            let reg1 = evaluate(condition, fd)?.unwrap();
            writeln!(fd, "JZ R{}, <L{}>", reg1, label1)?;
            evaluate(if_stmt, fd)?;
            writeln!(fd, "JMP <L{}>", label2)?;
            write!(fd, "L{}:", label1)?;
            if let Some(stmts) = else_stmt {
                evaluate(stmts, fd)?;
            }
            write!(fd, "L{}:", label2)?;
            REGISTERS.lock().unwrap().free_reg(reg1);
            Ok(None)
        }
        Tnode::While {
            condition, stmts, ..
        } => {
            let mut labels = LABELS.lock().unwrap();
            let condition_label = labels.get();
            let exit_label = labels.get();
            drop(labels);
            LP.lock().unwrap().push((condition_label, exit_label));
            write!(fd, "L{}:", condition_label)?;
            let reg1 = evaluate(condition, fd)?.unwrap();
            writeln!(fd, "JZ R{}, <L{}>", reg1, exit_label)?;
            evaluate(stmts, fd)?;
            writeln!(fd, "JMP <L{}>", condition_label)?;
            write!(fd, "L{}:", exit_label)?;
            LP.lock().unwrap().pop();
            REGISTERS.lock().unwrap().free_reg(reg1);
            Ok(None)
        }
        Tnode::Repeat {
            stmts, condition, ..
        } => {
            let mut labels = LABELS.lock().unwrap();
            let stmt_label = labels.get();
            let condition_label = labels.get();
            let exit_label = labels.get();
            drop(labels);
            LP.lock().unwrap().push((condition_label, exit_label));
            write!(fd, "L{}:", stmt_label)?;
            evaluate(stmts, fd)?;
            write!(fd, "L{}:", condition_label)?;
            let reg1 = evaluate(condition, fd)?.unwrap();
            writeln!(fd, "JZ R{}, <L{}>", reg1, stmt_label)?;
            write!(fd, "L{}:", exit_label)?;
            LP.lock().unwrap().pop();
            Ok(None)
        }
        Tnode::Connector { left, right, .. } => {
            evaluate(left, fd)?;
            evaluate(right, fd)?;
            Ok(None)
        }
        Tnode::Empty => Ok(None),
    }
}

pub fn emit_code(root: &Tnode, file_name: &PathBuf) -> Result<(), Box<dyn Error>> {
    let mut fd = OpenOptions::new()
        .create(true)
        .truncate(true)
        .write(true)
        .open(file_name)?;
    write!(
        fd,
        "{}\n{}\n{}\n{}\n{}\n{}\n{}\n{}\n",
        0, 2056, 0, 0, 0, 0, 0, 0
    )?;
    write!(
        fd,
        "MOV SP, {}\n",
        4095 + SYMBOL_TABLE.lock().unwrap().get_size()
    )?;
    match evaluate(root, &mut fd) {
        Ok(_) => {
            writeln!(fd, "MOV R0, 10")?;
            writeln!(fd, "PUSH R0")?;
            writeln!(fd, "ADD SP, 4")?;
            writeln!(fd, "INT 10")?;
            Ok(())
        }
        Err(e) => {
            eprint!("{e}");
            Err(err_from_str("Error in code gen stage!"))
        }
    }
}
