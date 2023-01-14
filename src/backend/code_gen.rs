use crate::ast::*;
use crate::utils::{err_from_str, label::Label, loop_util::LoopStack, register::RegisterPool};
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
    static ref LABELS: Mutex<Label> = Mutex::new(Label::default());
    static ref LP: Mutex<LoopStack> = Mutex::new(LoopStack::default());
}

fn pre_call(fd: &mut File) {
    for reg in REGISTERS.lock().unwrap().accquired_regs() {
        write!(fd, "PUSH R{}\n", reg).unwrap();
    }
}

fn post_call(fd: &mut File) {
    for reg in REGISTERS.lock().unwrap().accquired_regs().rev() {
        write!(fd, "POP R{}\n", reg).unwrap();
    }
}

fn evaluate(node: &Tnode, fd: &mut File) -> Result<Option<u8>, Box<dyn Error>> {
    match node {
        Tnode::Constant { value, .. } => {
            let reg1 = match REGISTERS.lock().unwrap().get_reg() {
                Some(r) => r,
                None => return Err(err_from_str("No registers left!")),
            };
            write!(fd, "MOV R{}, {}\n", reg1, value)?;
            Ok(Some(reg1))
        }
        var @ Tnode::Var { .. } => {
            let reg1 = match REGISTERS.lock().unwrap().get_reg() {
                Some(r) => r,
                None => return Err(err_from_str("No registers left!")),
            };
            write!(fd, "MOV R{}, {}\n", reg1, var.get_address().unwrap())?;
            write!(fd, "MOV R{}, [R{}]\n", reg1, reg1)?;
            Ok(Some(reg1))
        }
        Tnode::Operator { op, lhs, rhs, .. } => {
            let reg1 = evaluate(lhs, fd)?.unwrap();
            let reg2 = evaluate(rhs, fd)?.unwrap();
            match op {
                Op::Add => write!(fd, "ADD R{}, R{}\n", reg1, reg2)?,
                Op::Sub => write!(fd, "SUB R{}, R{}\n", reg1, reg2)?,
                Op::Mult => write!(fd, "MUL R{}, R{}\n", reg1, reg2)?,
                Op::Div => write!(fd, "DIV R{}, R{}\n", reg1, reg2)?,
                Op::Mod => write!(fd, "MOD R{}, R{}\n", reg1, reg2)?,
                Op::EQ => write!(fd, "EQ R{}, R{}\n", reg1, reg2)?,
                Op::NE => write!(fd, "NE R{}, R{}\n", reg1, reg2)?,
                Op::GT => write!(fd, "GT R{}, R{}\n", reg1, reg2)?,
                Op::GE => write!(fd, "GE R{}, R{}\n", reg1, reg2)?,
                Op::LT => write!(fd, "LT R{}, R{}\n", reg1, reg2)?,
                Op::LE => write!(fd, "LE R{}, R{}\n", reg1, reg2)?,
            }
            REGISTERS.lock().unwrap().free_reg(reg2);
            Ok(Some(reg1))
        }
        Tnode::Read { var, .. } => {
            let reg1 = match REGISTERS.lock().unwrap().get_reg() {
                Some(r) => r,
                None => return Err(err_from_str("No registers left!")),
            };
            pre_call(fd);
            write!(fd, "MOV R{}, \"Read\"\n", reg1)?;
            write!(fd, "PUSH R{}\n", reg1)?;
            write!(fd, "MOV R{}, -1\n", reg1)?;
            write!(fd, "PUSH R{}\n", reg1)?;
            write!(fd, "MOV R{}, {}\n", reg1, var.get_address().unwrap())?;
            write!(fd, "PUSH R{}\n", reg1)?;
            write!(fd, "ADD SP, 2\n")?;
            write!(fd, "CALL 0\n")?;
            write!(fd, "SUB SP, 5\n")?;
            post_call(fd);
            REGISTERS.lock().unwrap().free_reg(reg1);
            Ok(None)
        }
        Tnode::Write { expression, .. } => {
            let reg1 = match REGISTERS.lock().unwrap().get_reg() {
                Some(r) => r,
                None => return Err(err_from_str("No registers left!")),
            };
            let reg2 = evaluate(expression, fd)?.unwrap();

            pre_call(fd);
            write!(fd, "MOV R{}, \"Write\"\n", reg1)?;
            write!(fd, "PUSH R{}\n", reg1)?;
            write!(fd, "MOV R{}, -2\n", reg1)?;
            write!(fd, "PUSH R{}\n", reg1)?;
            write!(fd, "PUSH R{}\n", reg2)?;
            write!(fd, "ADD SP, 2\n")?;
            write!(fd, "CALL 0\n")?;
            write!(fd, "SUB SP, 5\n")?;
            post_call(fd);

            REGISTERS.lock().unwrap().free_reg(reg1);
            REGISTERS.lock().unwrap().free_reg(reg2);
            Ok(None)
        }
        Tnode::Asgn { lhs, rhs, .. } => {
            let reg1 = evaluate(rhs, fd)?.unwrap();
            write!(fd, "MOV [{}], R{}\n", lhs.get_address()?, reg1)?;
            REGISTERS.lock().unwrap().free_reg(reg1);
            Ok(None)
        }
        Tnode::Continue => {
            if let Some(label) = LP.lock().unwrap().condition_label() {
                write!(fd, "JMP <L{}>\n", label)?;
            }
            Ok(None)
        }
        Tnode::Break => {
            if let Some(label) = LP.lock().unwrap().exit_label() {
                write!(fd, "JMP <L{}>\n", label)?;
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
            write!(fd, "JZ R{}, <L{}>\n", reg1, label1)?;
            evaluate(if_stmt, fd)?;
            write!(fd, "JMP <L{}>\n", label2)?;
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
            write!(fd, "JZ R{}, <L{}>\n", reg1, exit_label)?;
            evaluate(stmts, fd)?;
            write!(fd, "JMP <L{}>\n", condition_label)?;
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
            write!(fd, "JZ R{}, <L{}>\n", reg1, stmt_label)?;
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

pub fn generate_code(root: &Tnode, file_name: &PathBuf) -> Result<(), Box<dyn Error>> {
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
    write!(fd, "MOV SP, {}\n", 4095 + 26)?;
    match evaluate(root, &mut fd) {
        Ok(_) => {
            write!(fd, "MOV R0, 10\n")?;
            write!(fd, "PUSH R0\n")?;
            write!(fd, "ADD SP, 4\n")?;
            write!(fd, "INT 10\n")?;
            Ok(())
        }
        Err(e) => {
            eprint!("{e}");
            Err(err_from_str("Error in code gen stage!"))
        }
    }
}
