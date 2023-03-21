use crate::{symbol::*, type_table::*};
use lrpar::Span;
use std::{collections::LinkedList, error::Error};

#[derive(Debug, Clone, Copy)]
pub enum BinaryOpType {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    EQ,
    NE,
    GT,
    GE,
    LT,
    LE,
    AND,
    OR,
}

#[derive(Debug, Clone, Copy)]
pub enum RefType {
    RHS,
    LHS,
}

#[derive(Debug, Clone)]
pub enum Tnode {
    Constant {
        span: Span,
        dtype: Type,
        value: String,
    },
    RefOperator {
        span: Span,
        dtype: Type,
        var: Box<Tnode>,
    },
    DeRefOperator {
        span: Span,
        dtype: Type,
        ref_type: RefType,
        var: Box<Tnode>,
    },
    BinaryOperator {
        op: BinaryOpType,
        span: Span,
        dtype: Type,
        lhs: Box<Tnode>,
        rhs: Box<Tnode>,
    },
    Var {
        span: Span,
        symbol: Symbol,
        array_access: Vec<Tnode>,
        field_access: Vec<u8>,
        ref_type: RefType,
        dtype: Type,
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
    FnCall {
        span: Span,
        symbol: Symbol,
        args: LinkedList<Tnode>,
    },
    Return {
        span: Span,
        exp: Box<Tnode>,
    },
    Alloc {
        span: Span,
        var: Box<Tnode>,
    },
    Free {
        span: Span,
        var: Box<Tnode>,
    },
    Null,
    Initialize,
    Continue,
    Break,
    Empty,
}

impl Tnode {
    pub fn get_address(&self) -> Result<&i16, Box<dyn Error>> {
        match self {
            Tnode::Var { symbol, .. } => Ok(symbol.get_binding().unwrap()),
            _ => Err(Box::<dyn Error>::from(
                "LHS of assign statment not variable",
            )),
        }
    }

    pub fn get_type(&self) -> &Type {
        match self {
            Tnode::Var { dtype, .. } => dtype,
            Tnode::FnCall { symbol, .. } => symbol.get_type(),
            Tnode::BinaryOperator { dtype, .. }
            | Tnode::Constant { dtype, .. }
            | Tnode::DeRefOperator { dtype, .. }
            | Tnode::RefOperator { dtype, .. } => dtype,
            Tnode::Null => &Type::Null,
            _ => &Type::Void,
        }
    }

    pub fn get_span(&self) -> Option<&Span> {
        match self {
            Tnode::Var { span, .. }
            | Tnode::BinaryOperator { span, .. }
            | Tnode::Constant { span, .. }
            | Tnode::RefOperator { span, .. }
            | Tnode::DeRefOperator { span, .. } => Some(span),
            _ => None,
        }
    }

    pub fn set_ref(&mut self, r: RefType) -> Result<(), String> {
        match self {
            Tnode::Var {
                ref mut ref_type, ..
            }
            | Tnode::DeRefOperator {
                ref mut ref_type, ..
            } => {
                *ref_type = r;
                Ok(())
            }
            _ => Err("Not a variable".to_owned()),
        }
    }

    pub fn is_local(&self) -> Result<bool, String> {
        match self {
            Tnode::Var { symbol, .. } => Ok(symbol.is_local()),
            _ => Err("Node not variable".to_owned()),
        }
    }
}

// DS used to store all data about a parsed function
pub struct FnAst {
    root: Tnode,    // AST root
    label: u8,      // Label of the function
    lvar_count: u8, // local variable count
}

impl FnAst {
    pub fn new(root: Tnode, label: u8, lvar_count: u8) -> Self {
        Self {
            root,
            label,
            lvar_count,
        }
    }
    pub fn get_label(&self) -> u8 {
        self.label
    }
    pub fn get_root(&self) -> &Tnode {
        &self.root
    }
    pub fn get_lvar_count(&self) -> u8 {
        self.lvar_count
    }
}
