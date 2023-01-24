use crate::symbol_table::*;
use lrpar::Span;
use std::error::Error;

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
            Tnode::BinaryOperator { dtype, .. }
            | Tnode::Constant { dtype, .. }
            | Tnode::DeRefOperator { dtype, .. }
            | Tnode::RefOperator { dtype, .. } => dtype.clone(),
            _ => Type::Void,
        }
    }

    pub fn get_span(&self) -> Option<Span> {
        match self {
            Tnode::Var { span, .. }
            | Tnode::BinaryOperator { span, .. }
            | Tnode::Constant { span, .. }
            | Tnode::RefOperator { span, .. }
            | Tnode::DeRefOperator { span, .. } => Some(span.clone()),
            _ => None,
        }
    }

    pub fn set_ref(&mut self, r: RefType) -> Result<(), &'static str> {
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
            _ => Err("Not a variable"),
        }
    }
}

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
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    Void,
    Int,
    Bool,
    Str,
    IntPtr,
    StrPtr,
}

impl Type {
    pub fn rref(&self) -> Result<Self, &'static str> {
        match self {
            Type::Int => Ok(Type::IntPtr),
            Type::Str => Ok(Type::StrPtr),
            _ => Err("Referencing not defined for this variable type"),
        }
    }
    pub fn deref(&self) -> Result<Self, &'static str> {
        match self {
            Type::IntPtr => Ok(Type::Int),
            Type::StrPtr => Ok(Type::Str),
            _ => Err("Dereferencing not defined for this variable type"),
        }
    }
}

pub struct TypeBuilder {
    is_pointer: bool,
    dtype: Option<Type>,
}

impl TypeBuilder {
    pub fn new() -> Self {
        TypeBuilder {
            is_pointer: false,
            dtype: None,
        }
    }

    pub fn set_pointer(&mut self, is_pointer: bool) -> &mut Self {
        self.is_pointer = is_pointer;
        self
    }

    pub fn dtype(&mut self, inner_type: Type) -> &mut Self {
        self.dtype = Some(inner_type);
        self
    }

    pub fn build(self) -> Result<Type, &'static str> {
        if self.is_pointer {
            match self.dtype {
                Some(Type::Int) => Ok(Type::IntPtr),
                Some(Type::Str) => Ok(Type::StrPtr),
                _ => Err("Pointer type only defined for int and string"),
            }
        } else {
            match self.dtype {
                Some(Type::Int) => Ok(Type::Int),
                Some(Type::Str) => Ok(Type::Str),
                Some(_) => Err("Variable can be only of int or string type"),
                _ => Err("Inner type must be defined"),
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum RefType {
    RHS,
    LHS,
}
