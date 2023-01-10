use super::ast::*;
use crate::general::RegisterPool;
use lrlex::DefaultLexeme;
use lrpar::{NonStreamingLexer, Span};
use std::{
    error::Error,
    fs::{File, OpenOptions},
    io::Write,
    path::PathBuf,
};

macro_rules! mapped_write {
    ($f:expr, $($arg:tt)*) => (
        write!($f, $($arg)*).map_err(|_| (None::<Span>, "Can't write to file"))
    )
}

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

fn generate_code_from_ast(
    node: &Tnode,
    lexer: &dyn NonStreamingLexer<DefaultLexeme, u32>,
    fd: &mut File,
    registers: &mut RegisterPool,
) -> Result<Option<u8>, (Option<Span>, &'static str)> {
    match node {
        Tnode::Constant { span } => match lexer.span_str(*span).parse::<u64>() {
            Ok(val) => {
                let reg1 = match registers.get_reg() {
                    Some(r) => r,
                    None => return Err((None, "No registers left!")),
                };
                mapped_write!(fd, "MOV R{}, {}\n", reg1, val)?;
                Ok(Some(reg1))
            }
            Err(_) => Err((Some(*span), "cannot be represented as a u64")),
        },
        var @ Tnode::Var { span: _, name: _ } => {
            let reg1 = match registers.get_reg() {
                Some(r) => r,
                None => return Err((None, "No registers left!")),
            };
            mapped_write!(fd, "MOV R{}, {}\n", reg1, var.get_address().unwrap())?;
            mapped_write!(fd, "MOV R{}, [R{}]\n", reg1, reg1)?;
            Ok(Some(reg1))
        }
        Tnode::Operator {
            op,
            span: _,
            left,
            right,
        } => match op {
            Op::Add => {
                let reg1 = generate_code_from_ast(left, lexer, fd, registers)?.unwrap();
                let reg2 = generate_code_from_ast(right, lexer, fd, registers)?.unwrap();
                mapped_write!(fd, "ADD R{}, R{}\n", reg1, reg2)?;
                registers.free_reg(reg2);
                Ok(Some(reg1))
            }
            Op::Sub => {
                let reg1 = generate_code_from_ast(left, lexer, fd, registers)?.unwrap();
                let reg2 = generate_code_from_ast(right, lexer, fd, registers)?.unwrap();
                mapped_write!(fd, "SUB R{}, R{}\n", reg1, reg2)?;
                registers.free_reg(reg2);
                Ok(Some(reg1))
            }
            Op::Mult => {
                let reg1 = generate_code_from_ast(left, lexer, fd, registers)?.unwrap();
                let reg2 = generate_code_from_ast(right, lexer, fd, registers)?.unwrap();
                mapped_write!(fd, "MUL R{}, R{}\n", reg1, reg2)?;
                registers.free_reg(reg2);
                Ok(Some(reg1))
            }
            Op::Div => {
                let reg1 = generate_code_from_ast(left, lexer, fd, registers)?.unwrap();
                let reg2 = generate_code_from_ast(right, lexer, fd, registers)?.unwrap();
                mapped_write!(fd, "DIV R{}, R{}\n", reg1, reg2)?;
                registers.free_reg(reg2);
                Ok(Some(reg1))
            }
            Op::Eq => {
                let reg1 = generate_code_from_ast(right, lexer, fd, registers)?.unwrap();
                mapped_write!(fd, "MOV [{}], R{}\n", left.get_address().unwrap(), reg1)?;
                registers.free_reg(reg1);
                Ok(None)
            }
        },
        Tnode::Read { span: _, var } => {
            let reg1 = match registers.get_reg() {
                Some(r) => r,
                None => return Err((None, "No registers left!")),
            };
            pre_call(fd, registers);
            mapped_write!(fd, "MOV R{}, \"Read\"\n", reg1)?;
            mapped_write!(fd, "PUSH R{}\n", reg1)?;
            mapped_write!(fd, "MOV R{}, -1\n", reg1)?;
            mapped_write!(fd, "PUSH R{}\n", reg1)?;
            mapped_write!(fd, "MOV R{}, {}\n", reg1, var.get_address().unwrap())?;
            mapped_write!(fd, "PUSH R{}\n", reg1)?;
            mapped_write!(fd, "ADD SP, 2\n")?;
            mapped_write!(fd, "CALL 0\n")?;
            mapped_write!(fd, "SUB SP, 5\n")?;
            post_call(fd, registers);
            registers.free_reg(reg1);
            Ok(None)
        }
        Tnode::Write {
            span: _,
            expression,
        } => {
            let reg1 = match registers.get_reg() {
                Some(r) => r,
                None => return Err((None, "No registers left!")),
            };
            let reg2 = generate_code_from_ast(expression, lexer, fd, registers)?.unwrap();

            pre_call(fd, registers);
            mapped_write!(fd, "MOV R{}, \"Write\"\n", reg1)?;
            mapped_write!(fd, "PUSH R{}\n", reg1)?;
            mapped_write!(fd, "MOV R{}, -2\n", reg1)?;
            mapped_write!(fd, "PUSH R{}\n", reg1)?;
            mapped_write!(fd, "PUSH R{}\n", reg2)?;
            mapped_write!(fd, "ADD SP, 2\n")?;
            mapped_write!(fd, "CALL 0\n")?;
            mapped_write!(fd, "SUB SP, 5\n")?;
            post_call(fd, registers);

            registers.free_reg(reg1);
            registers.free_reg(reg2);
            Ok(None)
        }
        Tnode::Connector {
            span: _,
            left,
            right,
        } => {
            generate_code_from_ast(left, lexer, fd, registers)?;
            generate_code_from_ast(right, lexer, fd, registers)?;
            Ok(None)
        }
        Tnode::Empty => Ok(None),
    }
}

pub fn generate_code(
    root: &Tnode,
    lexer: &dyn NonStreamingLexer<DefaultLexeme, u32>,
    file_name: &PathBuf,
) -> Result<(), Box<dyn Error>> {
    let mut fd = match OpenOptions::new()
        .create(true)
        .truncate(true)
        .write(true)
        .open(file_name)
    {
        Ok(f) => f,
        Err(e) => {
            println!("{e}");
            std::process::exit(1);
        }
    };
    let mut registers = RegisterPool::default();
    write!(
        fd,
        "{}\n{}\n{}\n{}\n{}\n{}\n{}\n{}\n",
        0, 2056, 0, 0, 0, 0, 0, 0
    )?;
    write!(fd, "MOV SP, {}\n", 4096 + 26)?;
    match generate_code_from_ast(root, lexer, &mut fd, &mut registers) {
        Ok(_) => {
            write!(fd, "MOV R0, 10\n")?;
            write!(fd, "PUSH R0\n")?;
            write!(fd, "ADD SP, 4\n")?;
            write!(fd, "INT 10\n")?;
            Ok(())
        }
        Err((Some(span), e)) => {
            let ((line, col), _) = lexer.line_col(span);
            eprintln!(
                "Evaluation error at line {} column {}, '{}' {}.",
                line,
                col,
                lexer.span_str(span),
                e
            );
            Err(Box::<dyn Error>::from("Error in code gen stage!"))
        }
        Err((None, e)) => {
            eprint!("{e}");
            Err(Box::<dyn Error>::from("Error in code gen stage!"))
        }
    }
}
