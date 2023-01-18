use crate::symbol_table::*;
use lrpar::Span;
use std::error::Error;

#[derive(Debug)]
pub enum Tnode {
    Constant {
        span: Span,
        ttype: Type,
        value: String,
    },
    Operator {
        op: Op,
        span: Span,
        ttype: Type,
        lhs: Box<Tnode>,
        rhs: Box<Tnode>,
    },
    Var {
        span: Span,
        symbol: Gsymbol,
    },
    Asgn {
        lhs: Box<Tnode>,
        rhs: Box<Tnode>,
    },
    Read {
        span: Span,
        var: Box<Tnode>,
    },
    Write {
        span: Span,
        expression: Box<Tnode>,
    },
    Connector {
        left: Box<Tnode>,
        right: Box<Tnode>,
    },
    If {
        condition: Box<Tnode>,
        if_stmt: Box<Tnode>,
        else_stmt: Option<Box<Tnode>>,
    },
    While {
        condition: Box<Tnode>,
        stmts: Box<Tnode>,
    },
    Repeat {
        stmts: Box<Tnode>,
        condition: Box<Tnode>,
    },
    Continue,
    Break,
    Empty,
}

#[derive(Debug)]
pub enum Op {
    Add,
    Sub,
    Mult,
    Div,
    Mod,
    EQ,
    NE,
    GT,
    GE,
    LT,
    LE,
}

#[derive(Debug, Clone, Copy)]
pub enum Type {
    Void,
    Int,
    Bool,
    String,
}

impl Tnode {
    pub fn get_address(&self) -> Result<u16, Box<dyn Error>> {
        match self {
            Tnode::Var { symbol, .. } => Ok(symbol.get_address()),
            _ => Err(Box::<dyn Error>::from(
                "LHS of assign statment not variable",
            )),
        }
    }

    pub fn get_type(&self) -> Type {
        match self {
            Tnode::Var { symbol, .. } => symbol.get_type(),
            Tnode::Operator { ttype, .. } | Tnode::Constant { ttype, .. } => ttype.clone(),
            _ => Type::Void,
        }
    }

    pub fn get_span(&self) -> Option<Span> {
        match self {
            Tnode::Var { span, .. }
            | Tnode::Operator { span, .. }
            | Tnode::Constant { span, .. } => Some(span.clone()),
            _ => None,
        }
    }
}
