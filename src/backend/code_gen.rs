use crate::ast::*;
use crate::symbol::*;
use crate::utils::{
    err_from_str, label::LabelGenerator, loop_util::LoopStack, register::RegisterPool,
};
use std::collections::LinkedList;
use std::{
    error::Error,
    fs::{File, OpenOptions},
    io::Write,
    path::PathBuf,
};

pub struct CodeGen {
    registers: RegisterPool,
    labels: LabelGenerator,
    lp: LoopStack,
    fd: File,
    gst_size: i16,
}

impl CodeGen {
    pub fn new(file_name: &PathBuf, gst: i16) -> Result<CodeGen, Box<dyn Error>> {
        let fd = OpenOptions::new()
            .create(true)
            .truncate(true)
            .write(true)
            .open(file_name)?;
        Ok(CodeGen {
            registers: RegisterPool::default(),
            labels: LabelGenerator::default(),
            lp: LoopStack::default(),
            fd,
            gst_size: gst,
        })
    }

    fn pre_call(&mut self) {
        for reg in self.registers.save_context() {
            writeln!(self.fd, "PUSH R{}", reg).unwrap();
        }
    }

    fn post_call(&mut self) {
        for reg in self.registers.restore_context().rev() {
            writeln!(self.fd, "POP R{}", reg).unwrap();
        }
    }

    fn evaluate(&mut self, node: &Tnode) -> Result<Option<u8>, Box<dyn Error>> {
        match node {
            Tnode::Constant { value, .. } => {
                let reg1 = self
                    .registers
                    .get_reg()
                    .ok_or(err_from_str("No registers left!"))?;
                writeln!(self.fd, "MOV R{}, {}", reg1, value)?;
                Ok(Some(reg1))
            }
            Tnode::Var {
                symbol,
                access,
                ref_type,
                ..
            } => {
                let reg1 = self
                    .registers
                    .get_reg()
                    .ok_or(err_from_str("No registers left!"))?;
                match symbol {
                    var @ Symbol::Variable { .. } => {
                        writeln!(self.fd, "MOV R{}, {}", reg1, var.get_address())?;
                        if var.is_local() {
                            writeln!(self.fd, "ADD R{}, BP", reg1)?;
                        }
                    }
                    arr @ Symbol::Array { dim, .. } => {
                        writeln!(self.fd, "MOV R{}, {}", reg1, arr.get_address())?;
                        for (i, exp) in access.iter().enumerate() {
                            let reg2 = self.evaluate(exp)?.unwrap();
                            for d in dim[(i + 1)..].iter() {
                                writeln!(self.fd, "MUL R{}, {}", reg2, d)?;
                            }
                            writeln!(self.fd, "ADD R{}, R{}", reg1, reg2)?;
                            self.registers.free_reg(reg2);
                        }
                    }
                    _ => {}
                }
                if let RefType::RHS = ref_type {
                    writeln!(self.fd, "MOV R{}, [R{}]", reg1, reg1)?;
                }
                Ok(Some(reg1))
            }
            Tnode::DeRefOperator { ref_type, var, .. } => {
                let reg1 = self.evaluate(var)?.unwrap();
                if let RefType::RHS = ref_type {
                    writeln!(self.fd, "MOV R{}, [R{}]", reg1, reg1)?;
                }
                Ok(Some(reg1))
            }
            Tnode::RefOperator { var, .. } => {
                // let reg1 = self
                //     .registers
                //     .get_reg()
                //     .ok_or(err_from_str("No registers left!"))?;
                // writeln!(self.fd, "MOV R{}, {}", reg1, var.get_address()?)?;
                // if var.is_local().unwrap() {
                //     writeln!(self.fd, "ADD R{}, BP", reg1)?;
                // }
                let reg1 = self.evaluate(var)?.unwrap();
                Ok(Some(reg1))
            }
            Tnode::BinaryOperator { op, lhs, rhs, .. } => {
                let reg1 = self.evaluate(lhs)?.unwrap();
                let reg2 = self.evaluate(rhs)?.unwrap();
                match op {
                    BinaryOpType::Add => writeln!(self.fd, "ADD R{}, R{}", reg1, reg2)?,
                    BinaryOpType::Sub => writeln!(self.fd, "SUB R{}, R{}", reg1, reg2)?,
                    BinaryOpType::Mul => writeln!(self.fd, "MUL R{}, R{}", reg1, reg2)?,
                    BinaryOpType::Div => writeln!(self.fd, "DIV R{}, R{}", reg1, reg2)?,
                    BinaryOpType::Mod => writeln!(self.fd, "MOD R{}, R{}", reg1, reg2)?,
                    BinaryOpType::EQ => writeln!(self.fd, "EQ R{}, R{}", reg1, reg2)?,
                    BinaryOpType::NE => writeln!(self.fd, "NE R{}, R{}", reg1, reg2)?,
                    BinaryOpType::GT => writeln!(self.fd, "GT R{}, R{}", reg1, reg2)?,
                    BinaryOpType::GE => writeln!(self.fd, "GE R{}, R{}", reg1, reg2)?,
                    BinaryOpType::LT => writeln!(self.fd, "LT R{}, R{}", reg1, reg2)?,
                    BinaryOpType::LE => writeln!(self.fd, "LE R{}, R{}", reg1, reg2)?,
                }
                self.registers.free_reg(reg2);
                Ok(Some(reg1))
            }
            Tnode::Read { var, .. } => {
                let reg1 = self.evaluate(var)?.unwrap();
                self.pre_call();
                let reg2 = self
                    .registers
                    .get_reg()
                    .ok_or(err_from_str("No registers left!"))?;
                writeln!(self.fd, "MOV R{}, \"Read\"", reg2)?;
                writeln!(self.fd, "PUSH R{}", reg2)?;
                writeln!(self.fd, "MOV R{}, -1", reg2)?;
                writeln!(self.fd, "PUSH R{}", reg2)?;
                writeln!(self.fd, "MOV R{}, R{}", reg2, reg1)?;
                writeln!(self.fd, "PUSH R{}", reg2)?;
                writeln!(self.fd, "ADD SP, 2")?;
                writeln!(self.fd, "CALL 0")?;
                writeln!(self.fd, "SUB SP, 5")?;
                self.registers.free_reg(reg2);
                self.post_call();
                self.registers.free_reg(reg1);
                Ok(None)
            }
            Tnode::Write { expression, .. } => {
                let reg1 = self.evaluate(expression)?.unwrap();
                self.pre_call();
                let reg2 = self
                    .registers
                    .get_reg()
                    .ok_or(err_from_str("No registers left!"))?;

                writeln!(self.fd, "MOV R{}, \"Write\"", reg2)?;
                writeln!(self.fd, "PUSH R{}", reg2)?;
                writeln!(self.fd, "MOV R{}, -2", reg2)?;
                writeln!(self.fd, "PUSH R{}", reg2)?;
                writeln!(self.fd, "PUSH R{}", reg1)?;
                writeln!(self.fd, "ADD SP, 2")?;
                writeln!(self.fd, "CALL 0")?;
                writeln!(self.fd, "SUB SP, 5")?;

                self.registers.free_reg(reg2);
                self.post_call();
                self.registers.free_reg(reg1);
                Ok(None)
            }
            Tnode::Asgn { lhs, rhs, .. } => {
                let reg1 = self.evaluate(lhs)?.unwrap();
                let reg2 = self.evaluate(rhs)?.unwrap();
                writeln!(self.fd, "MOV [R{}], R{}", reg1, reg2)?;
                self.registers.free_reg(reg2);
                self.registers.free_reg(reg1);
                Ok(None)
            }
            Tnode::Continue => {
                if let Some(label) = self.lp.condition_label() {
                    writeln!(self.fd, "JMP <L{}>", label)?;
                }
                Ok(None)
            }
            Tnode::Break => {
                if let Some(label) = self.lp.exit_label() {
                    writeln!(self.fd, "JMP <L{}>", label)?;
                }
                Ok(None)
            }
            Tnode::If {
                condition,
                if_stmt,
                else_stmt,
                ..
            } => {
                let label1 = self.labels.get();
                let label2 = self.labels.get();
                let reg1 = self.evaluate(condition)?.unwrap();
                writeln!(self.fd, "JZ R{}, <L{}>", reg1, label1)?;
                self.evaluate(if_stmt)?;
                writeln!(self.fd, "JMP <L{}>", label2)?;
                write!(self.fd, "L{}:", label1)?;
                if let Some(stmts) = else_stmt {
                    self.evaluate(stmts)?;
                }
                write!(self.fd, "L{}:", label2)?;
                self.registers.free_reg(reg1);
                Ok(None)
            }
            Tnode::While {
                condition, stmts, ..
            } => {
                let condition_label = self.labels.get();
                let exit_label = self.labels.get();
                self.lp.push((condition_label, exit_label));
                write!(self.fd, "L{}:", condition_label)?;
                let reg1 = self.evaluate(condition)?.unwrap();
                writeln!(self.fd, "JZ R{}, <L{}>", reg1, exit_label)?;
                self.evaluate(stmts)?;
                writeln!(self.fd, "JMP <L{}>", condition_label)?;
                write!(self.fd, "L{}:", exit_label)?;
                self.lp.pop();
                self.registers.free_reg(reg1);
                Ok(None)
            }
            Tnode::Repeat {
                stmts, condition, ..
            } => {
                let stmt_label = self.labels.get();
                let condition_label = self.labels.get();
                let exit_label = self.labels.get();
                self.lp.push((condition_label, exit_label));
                write!(self.fd, "L{}:", stmt_label)?;
                self.evaluate(stmts)?;
                write!(self.fd, "L{}:", condition_label)?;
                let reg1 = self.evaluate(condition)?.unwrap();
                writeln!(self.fd, "JZ R{}, <L{}>", reg1, stmt_label)?;
                write!(self.fd, "L{}:", exit_label)?;
                self.lp.pop();
                Ok(None)
            }
            Tnode::Connector { left, right, .. } => {
                self.evaluate(left)?;
                self.evaluate(right)?;
                Ok(None)
            }
            Tnode::FnCall { symbol, args, .. } => {
                self.pre_call();
                for exp in args.iter() {
                    let reg2 = self.evaluate(exp)?.unwrap();
                    writeln!(self.fd, "PUSH R{}", reg2)?;
                    self.registers.free_reg(reg2);
                }
                writeln!(self.fd, "INR SP")?;
                writeln!(self.fd, "CALL <F{}>", symbol.get_address())?;
                let reg1 = self
                    .registers
                    .get_ret_reg()
                    .ok_or(err_from_str("No self.registers left!"))?;
                writeln!(self.fd, "POP R{}", reg1)?;
                writeln!(self.fd, "SUB SP, {}", args.len())?;
                self.post_call();
                Ok(Some(reg1))
            }
            Tnode::Return { exp, .. } => {
                let reg1 = self.evaluate(exp)?.unwrap();
                let reg2 = self
                    .registers
                    .get_reg()
                    .ok_or(err_from_str("No self.registers left!"))?;
                writeln!(self.fd, "MOV R{}, BP", reg2)?;
                writeln!(self.fd, "SUB R{}, 2", reg2)?;
                writeln!(self.fd, "MOV [R{}], R{}", reg2, reg1)?;
                self.registers.free_reg(reg1);
                self.registers.free_reg(reg2);
                Ok(None)
            }
            Tnode::Empty => Ok(None),
        }
    }

    pub fn emit_code(&mut self, fns: &LinkedList<FnAst>) -> Result<(), Box<dyn Error>> {
        write!(
            self.fd,
            "{}\n{}\n{}\n{}\n{}\n{}\n{}\n{}\n",
            0, 2056, 0, 0, 0, 0, 0, 0
        )?;
        writeln!(self.fd, "MOV SP, {}", 4098 + self.gst_size)?;
        writeln!(self.fd, "MOV BP, SP")?;
        writeln!(self.fd, "CALL <F0>")?;
        writeln!(self.fd, "MOV R0, 10")?;
        writeln!(self.fd, "PUSH R0")?;
        writeln!(self.fd, "ADD SP, 4")?;
        writeln!(self.fd, "INT 10")?;
        for fn_def in fns.iter() {
            write!(self.fd, "F{}:", fn_def.get_label())?;
            writeln!(self.fd, "PUSH BP")?;
            writeln!(self.fd, "MOV BP, SP")?;
            writeln!(self.fd, "ADD SP, {}", fn_def.get_lvar_count())?;
            match self.evaluate(fn_def.get_root()) {
                Ok(_) => {
                    writeln!(self.fd, "SUB SP, {}", fn_def.get_lvar_count())?;
                    writeln!(self.fd, "POP BP")?;
                    writeln!(self.fd, "RET")?;
                }
                Err(e) => {
                    eprint!("{e}");
                    return Err(err_from_str("Error in code gen stage!"));
                }
            }
        }
        Ok(())
    }
}
