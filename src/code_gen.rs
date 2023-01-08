use super::general::RegisterPool;
use lrlex::DefaultLexeme;
use lrpar::{NonStreamingLexer, Span};
use std::{
    error::Error,
    fs::{File, OpenOptions},
    io::Write,
    path::PathBuf,
};

#[derive(Debug)]
pub enum Tnode {
    Constant {
        span: Span,
    },
    Operator {
        op: Op,
        span: Span,
        left: Box<Tnode>,
        right: Box<Tnode>,
    },
}

#[derive(Debug)]
pub enum Op {
    Add,
    Sub,
    Mult,
    Div,
}

fn generate_code_from_ast(
    node: Tnode,
    lexer: &dyn NonStreamingLexer<DefaultLexeme, u32>,
    fd: &mut File,
    registers: &mut RegisterPool,
) -> Result<u8, (Span, &'static str)> {
    match node {
        Tnode::Constant { span } => match lexer.span_str(span).parse::<u64>() {
            Ok(val) => {
                let reg1 = match registers.get_reg() {
                    Some(r) => r,
                    None => return Err((span, "No registers left!")),
                };
                write!(fd, "MOV R{}, {}\n", reg1, val).expect("Coundn't write to file!");
                Ok(reg1)
            }
            Err(_) => Err((span, "cannot be represented as a u64")),
        },
        Tnode::Operator {
            op,
            span: _,
            left,
            right,
        } => {
            let reg1 = generate_code_from_ast(*left, lexer, fd, registers)?;
            let reg2 = generate_code_from_ast(*right, lexer, fd, registers)?;
            match op {
                Op::Add => {
                    write!(fd, "ADD R{}, R{}\n", reg1, reg2).expect("Couldn't write to file");
                }
                Op::Sub => {
                    write!(fd, "SUB R{}, R{}\n", reg1, reg2).expect("Couldn't write to file");
                }
                Op::Mult => {
                    write!(fd, "MUL R{}, R{}\n", reg1, reg2).expect("Couldn't write to file");
                }
                Op::Div => {
                    write!(fd, "DIV R{}, R{}\n", reg1, reg2).expect("Couldn't write to file");
                }
            }
            registers.free_reg(reg2);
            return Ok(reg1);
        }
    }
}

pub fn generate_code(
    root: Tnode,
    lexer: &dyn NonStreamingLexer<DefaultLexeme, u32>,
    file_name: &PathBuf,
) -> Result<(), Box<dyn Error>> {
    let mut fd = match OpenOptions::new().create(true).write(true).open(file_name) {
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
    match generate_code_from_ast(root, lexer, &mut fd, &mut registers) {
        Ok(reg) => {
            write!(fd, "MOV R19, R{}\n", reg)?;
            write!(fd, "MOV R0, \"Write\"\n")?;
            write!(fd, "PUSH R0\n")?;
            write!(fd, "MOV R0, -2\n")?;
            write!(fd, "PUSH R0\n")?;
            write!(fd, "PUSH R19\n")?;
            write!(fd, "PUSH R0\n")?;
            write!(fd, "PUSH R0\n")?;
            write!(fd, "CALL 0\n")?;
            for _ in 0..5 {
                write!(fd, "POP R0\n")?;
            }
            write!(fd, "MOV R0, 10\n")?;
            for _ in 0..5 {
                write!(fd, "PUSH R0\n")?;
            }
            write!(fd, "INT 10\n")?;
            Ok(())
        }
        Err((span, e)) => {
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
    }
}
