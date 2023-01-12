use super::ast::*;
use crate::utils::{err_from_str, Label, RegisterPool};
use lrlex::DefaultLexeme;
use lrpar::NonStreamingLexer;
use std::{
    error::Error,
    fs::{File, OpenOptions},
    io::Write,
    path::PathBuf,
};

fn pre_call(fd: &mut File, registers: &mut RegisterPool) {
    for reg in registers.accquired_regs() {
        write!(fd, "PUSH R{}\n", reg).unwrap();
    }
}

fn post_call(fd: &mut File, registers: &mut RegisterPool) {
    for reg in registers.accquired_regs().rev() {
        write!(fd, "POP R{}\n", reg).unwrap();
    }
}

fn evaluator(
    node: &Tnode,
    lexer: &dyn NonStreamingLexer<DefaultLexeme, u32>,
    fd: &mut File,
    registers: &mut RegisterPool,
    labels: &mut Label,
) -> Result<Option<u8>, Box<dyn Error>> {
    match node {
        Tnode::Constant { span, .. } => match lexer.span_str(*span).parse::<u32>() {
            Ok(val) => {
                let reg1 = match registers.get_reg() {
                    Some(r) => r,
                    None => return Err(err_from_str("No registers left!")),
                };
                write!(fd, "MOV R{}, {}\n", reg1, val)?;
                Ok(Some(reg1))
            }
            Err(_) => Err(err_from_str("cannot be represented as a u32")),
        },
        var @ Tnode::Var { .. } => {
            let reg1 = match registers.get_reg() {
                Some(r) => r,
                None => return Err(err_from_str("No registers left!")),
            };
            write!(fd, "MOV R{}, {}\n", reg1, var.get_address().unwrap())?;
            write!(fd, "MOV R{}, [R{}]\n", reg1, reg1)?;
            Ok(Some(reg1))
        }
        Tnode::Operator { op, lhs, rhs, .. } => {
            let reg1 = evaluator(lhs, lexer, fd, registers, labels)?.unwrap();
            let reg2 = evaluator(rhs, lexer, fd, registers, labels)?.unwrap();
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
            registers.free_reg(reg2);
            Ok(Some(reg1))
        }
        Tnode::Read { var, .. } => {
            let reg1 = match registers.get_reg() {
                Some(r) => r,
                None => return Err(err_from_str("No registers left!")),
            };
            pre_call(fd, registers);
            write!(fd, "MOV R{}, \"Read\"\n", reg1)?;
            write!(fd, "PUSH R{}\n", reg1)?;
            write!(fd, "MOV R{}, -1\n", reg1)?;
            write!(fd, "PUSH R{}\n", reg1)?;
            write!(fd, "MOV R{}, {}\n", reg1, var.get_address().unwrap())?;
            write!(fd, "PUSH R{}\n", reg1)?;
            write!(fd, "ADD SP, 2\n")?;
            write!(fd, "CALL 0\n")?;
            write!(fd, "SUB SP, 5\n")?;
            post_call(fd, registers);
            registers.free_reg(reg1);
            Ok(None)
        }
        Tnode::Write { expression, .. } => {
            let reg1 = match registers.get_reg() {
                Some(r) => r,
                None => return Err(err_from_str("No registers left!")),
            };
            let reg2 = evaluator(expression, lexer, fd, registers, labels)?.unwrap();

            pre_call(fd, registers);
            write!(fd, "MOV R{}, \"Write\"\n", reg1)?;
            write!(fd, "PUSH R{}\n", reg1)?;
            write!(fd, "MOV R{}, -2\n", reg1)?;
            write!(fd, "PUSH R{}\n", reg1)?;
            write!(fd, "PUSH R{}\n", reg2)?;
            write!(fd, "ADD SP, 2\n")?;
            write!(fd, "CALL 0\n")?;
            write!(fd, "SUB SP, 5\n")?;
            post_call(fd, registers);

            registers.free_reg(reg1);
            registers.free_reg(reg2);
            Ok(None)
        }
        Tnode::Connector { left, right, .. } => {
            evaluator(left, lexer, fd, registers, labels)?;
            evaluator(right, lexer, fd, registers, labels)?;
            Ok(None)
        }
        Tnode::Asgn { lhs, rhs, .. } => {
            let reg1 = evaluator(rhs, lexer, fd, registers, labels)?.unwrap();
            write!(fd, "MOV [{}], R{}\n", lhs.get_address()?, reg1)?;
            registers.free_reg(reg1);
            Ok(None)
        }
        Tnode::If {
            condition,
            if_stmt,
            else_stmt,
            ..
        } => {
            let label1 = labels.get();
            let label2 = labels.get();
            let reg1 = evaluator(condition, lexer, fd, registers, labels)?.unwrap();
            write!(fd, "JZ R{}, <L{}>\n", reg1, label1)?;
            evaluator(if_stmt, lexer, fd, registers, labels)?;
            write!(fd, "JMP <L{}>\n", label2)?;
            write!(fd, "L{}:", label1)?;
            if let Some(stmts) = else_stmt {
                evaluator(stmts, lexer, fd, registers, labels)?;
            }
            write!(fd, "L{}:", label2)?;
            registers.free_reg(reg1);
            Ok(None)
        }
        Tnode::While {
            condition, stmts, ..
        } => {
            let label1 = labels.get();
            let label2 = labels.get();
            write!(fd, "L{}:", label1)?;
            let reg1 = evaluator(condition, lexer, fd, registers, labels)?.unwrap();
            write!(fd, "JZ R{}, <L{}>\n", reg1, label2)?;
            evaluator(stmts, lexer, fd, registers, labels)?;
            write!(fd, "JMP <L{}>\n", label1)?;
            write!(fd, "L{}:", label2)?;
            registers.free_reg(reg1);
            Ok(None)
        }
        Tnode::Empty { .. } => Ok(None),
    }
}

pub fn generate_code(
    root: &Tnode,
    lexer: &dyn NonStreamingLexer<DefaultLexeme, u32>,
    file_name: &PathBuf,
) -> Result<(), Box<dyn Error>> {
    let mut fd = OpenOptions::new()
        .create(true)
        .truncate(true)
        .write(true)
        .open(file_name)?;
    let mut registers = RegisterPool::default();
    let mut labels = Label::default();
    write!(
        fd,
        "{}\n{}\n{}\n{}\n{}\n{}\n{}\n{}\n",
        0, 2056, 0, 0, 0, 0, 0, 0
    )?;
    write!(fd, "MOV SP, {}\n", 4095 + 26)?;
    match evaluator(root, lexer, &mut fd, &mut registers, &mut labels) {
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
