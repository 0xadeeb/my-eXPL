use crate::symbol_table::*;
use lrpar::Span;
use std::error::Error;

#[derive(Debug)]
pub enum Tnode {
    Constant {
        span: Span,
        dtype: Type,
        value: String,
    },
    Operator {
        op: Op,
        span: Span,
        dtype: Type,
        lhs: Box<Tnode>,
        rhs: Box<Tnode>,
    },
    Var {
        span: Span,
        symbol: Symbol,
        access: Vec<Box<Tnode>>,
        ref_type: RefType,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    Void,
    Int,
    Bool,
    String,
}

#[derive(Debug, Clone, Copy)]
pub enum RefType {
    RHS,
    LHS,
}

impl Tnode {
    pub fn get_address(&self) -> Result<u16, Box<dyn Error>> {
        match self {
            Tnode::Var { symbol, .. } => Ok(symbol.base_address()),
            _ => Err(Box::<dyn Error>::from(
                "LHS of assign statment not variable",
            )),
        }
    }

    pub fn get_type(&self) -> Type {
        match self {
            Tnode::Var { symbol, .. } => symbol.get_type(),
            Tnode::Operator { dtype, .. } | Tnode::Constant { dtype, .. } => dtype.clone(),
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

    pub fn set_ref(&mut self, r: RefType) -> Result<(), &'static str> {
        match self {
            Tnode::Var {
                ref mut ref_type, ..
            } => {
                *ref_type = r;
                Ok(())
            }
            _ => Err("Not a variable"),
        }
    }
}
