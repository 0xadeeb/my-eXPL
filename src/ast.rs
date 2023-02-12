use crate::{frontend::PARSER, symbol::*, type_table::*};
use lrlex::DefaultLexeme;
use lrpar::{Lexeme, NonStreamingLexer, Span};
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
    FnCall {
        span: Span,
        symbol: Symbol,
        args: LinkedList<Box<Tnode>>,
    },
    Return {
        span: Span,
        exp: Box<Tnode>,
    },
    Continue,
    Break,
    Empty,
}

impl Tnode {
    pub fn get_address(&self) -> Result<i16, Box<dyn Error>> {
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
            Tnode::FnCall { symbol, .. } => symbol.get_type(),
            Tnode::BinaryOperator { dtype, .. }
            | Tnode::Constant { dtype, .. }
            | Tnode::DeRefOperator { dtype, .. }
            | Tnode::RefOperator { dtype, .. } => dtype.clone(),
            Tnode::Return { exp, .. } => exp.get_type(),
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

    pub fn is_local(&self) -> Result<bool, &'static str> {
        match self {
            Tnode::Var { symbol, .. } => Ok(symbol.is_local()),
            _ => Err("Node not variable"),
        }
    }

    pub fn create_int(
        op: BinaryOpType,
        span: Span,
        left: Tnode,
        right: Tnode,
    ) -> Result<Tnode, (Option<Span>, &'static str)> {
        match (left.get_type(), right.get_type()) {
            (Type::Int, Type::Int) => {}
            _ => return Err((Some(span), "Type mismatch, expected integer")),
        }
        Ok(Tnode::BinaryOperator {
            op,
            span,
            dtype: Type::Int,
            lhs: Box::new(left),
            rhs: Box::new(right),
        })
    }

    pub fn create_bool(
        op: BinaryOpType,
        span: Span,
        left: Tnode,
        right: Tnode,
    ) -> Result<Tnode, (Option<Span>, &'static str)> {
        match (left.get_type(), right.get_type()) {
            (Type::Int, Type::Int) | (Type::Str, Type::Str) => {}
            _ => return Err((Some(span), "Type mismatch, expected integer")),
        }
        Ok(Tnode::BinaryOperator {
            op,
            span,
            dtype: Type::Bool,
            lhs: Box::new(left),
            rhs: Box::new(right),
        })
    }

    pub fn create_logical_op(
        op: BinaryOpType,
        span: Span,
        left: Tnode,
        right: Tnode,
    ) -> Result<Tnode, (Option<Span>, &'static str)> {
        match (left.get_type(), right.get_type()) {
            (Type::Bool, Type::Bool) => {}
            _ => return Err((Some(span), "Type mismatch, expected boolean")),
        }
        Ok(Tnode::BinaryOperator {
            op,
            span,
            dtype: Type::Bool,
            lhs: Box::new(left),
            rhs: Box::new(right),
        })
    }

    pub fn create_asg(
        span: Span,
        mut left: Tnode,
        right: Tnode,
    ) -> Result<Tnode, (Option<Span>, &'static str)> {
        if right.get_type() != left.get_type() {
            return Err((
                Some(span),
                "LHS and RHS types of assignment statment don't match",
            ));
        }
        left.set_ref(RefType::LHS)
            .map_err(|msg| (left.get_span(), msg))?;
        Ok(Tnode::Asgn {
            lhs: Box::new(left),
            rhs: Box::new(right),
        })
    }

    pub fn create_ref(span: Span, mut exp: Tnode) -> Result<Tnode, (Option<Span>, &'static str)> {
        match exp {
            Tnode::Var { .. } => {
                exp.set_ref(RefType::LHS).unwrap();
            }
            _ => return Err((Some(span), "Referencing just defined for variables")),
        }
        Ok(Tnode::RefOperator {
            span,
            dtype: exp.get_type().rref().map_err(|msg| (Some(span), msg))?,
            var: Box::new(exp),
        })
    }

    pub fn create_deref(span: Span, exp: Tnode) -> Result<Tnode, (Option<Span>, &'static str)> {
        match exp {
            Tnode::Var { .. } => {}
            _ => return Err((Some(span), "Dereferencing just defined for variables")),
        }
        Ok(Tnode::DeRefOperator {
            span,
            dtype: exp.get_type().deref().map_err(|msg| (Some(span), msg))?,
            ref_type: RefType::RHS,
            var: Box::new(exp),
        })
    }

    pub fn create_read(span: Span, mut var: Tnode) -> Result<Tnode, (Option<Span>, &'static str)> {
        var.set_ref(RefType::LHS)
            .map_err(|msg| (var.get_span(), msg))?;
        Ok(Tnode::Read {
            span,
            var: Box::new(var),
        })
    }

    pub fn create_write(span: Span, e: Tnode) -> Result<Tnode, (Option<Span>, &'static str)> {
        match e.get_type() {
            Type::Int | Type::Str | Type::IntPtr | Type::StrPtr => {}
            _ => return Err((e.get_span(), "Type mismatch, expected integer or string")),
        }
        Ok(Tnode::Write {
            span,
            expression: Box::new(e),
        })
    }

    pub fn create_while(
        _span: Span,
        condition: Tnode,
        stmts: Tnode,
    ) -> Result<Tnode, (Option<Span>, &'static str)> {
        match condition.get_type() {
            Type::Bool => {}
            _ => return Err((condition.get_span(), "Type mismatch, excepted boolen")),
        }
        Ok(Tnode::While {
            condition: Box::new(condition),
            stmts: Box::new(stmts),
        })
    }

    pub fn create_repeat(
        _span: Span,
        stmts: Tnode,
        condition: Tnode,
    ) -> Result<Tnode, (Option<Span>, &'static str)> {
        match condition.get_type() {
            Type::Bool => {}
            _ => return Err((condition.get_span(), "Type mismatch, excepted boolen")),
        }
        Ok(Tnode::Repeat {
            stmts: Box::new(stmts),
            condition: Box::new(condition),
        })
    }

    pub fn create_if(
        _span: Span,
        condition: Tnode,
        if_stmts: Tnode,
        else_stmts: Option<Tnode>,
    ) -> Result<Tnode, (Option<Span>, &'static str)> {
        match condition.get_type() {
            Type::Bool => {}
            _ => return Err((condition.get_span(), "Type mismatch, excepted boolen")),
        }
        Ok(Tnode::If {
            condition: Box::new(condition),
            if_stmt: Box::new(if_stmts),
            else_stmt: else_stmts.map(|val| Box::new(val)),
        })
    }

    pub fn create_constant(
        lexer: &dyn NonStreamingLexer<DefaultLexeme, u32>,
        token: &DefaultLexeme,
        dtype: Type,
    ) -> Result<Tnode, (Option<Span>, &'static str)> {
        match dtype {
            Type::Int => match lexer.span_str(token.span()).parse::<i32>() {
                Ok(val) => Ok(Tnode::Constant {
                    span: token.span(),
                    dtype: Type::Int,
                    value: val.to_string(),
                }),
                Err(_) => Err((Some(token.span()), "Can't parse to i32")),
            },
            Type::Str => Ok(Tnode::Constant {
                span: token.span(),
                dtype: Type::Str,
                value: lexer.span_str(token.span()).to_string(),
            }),
            _ => Err((
                Some(token.span()),
                "Only variables of type int and string can be stored in a variable",
            )),
        }
    }

    pub fn create_fn(
        rtype: Type,
        body: Tnode,
        rstmt: Tnode,
        span: Span,
    ) -> Result<FnAst, (Option<Span>, &'static str)> {
        let mut parser = PARSER.lock().unwrap();
        if parser.cfn_rtype() != rtype {
            return Err((Some(span), "Return type don't match"));
        }
        if rtype != rstmt.get_type() {
            return Err((
                Some(span),
                "Return expression of this function doesn't match the defined return type",
            ));
        }

        Ok(FnAst::new(
            Tnode::Connector {
                left: Box::new(body),
                right: Box::new(rstmt),
            },
            parser.cfn().unwrap().get_address() as i16,
            parser.lst().get_size(),
        ))
    }

    pub fn create_fncall(
        fname: &str,
        args: LinkedList<Tnode>,
        span: Span,
    ) -> Result<Tnode, (Option<Span>, &'static str)> {
        let symbol = PARSER
            .lock()
            .unwrap()
            .gst()
            .get(fname)
            .filter(|s| matches!(s, Symbol::Function { .. }))
            .ok_or((Some(span), "Function was not defined"))?
            .clone();

        let mut i = symbol.get_params().unwrap().into_iter();
        let mut j = args.iter();

        loop {
            match (i.next(), j.next()) {
                (Some((t, _)), Some(e)) if t == e.get_type() => {}
                (None, None) => break,
                _ => {
                    return Err((
                        Some(span),
                        "Arguments of function don't match with definition",
                    ))
                }
            }
        }

        Ok(Tnode::FnCall {
            span,
            symbol,
            args: LinkedList::from_iter(args.iter().map(|node| Box::new(node.clone()))),
        })
    }

    pub fn create_main_block(
        rtype: Type,
        body: Tnode,
        rstmt: Tnode,
        span: Span,
    ) -> Result<FnAst, (Option<Span>, &'static str)> {
        if rtype != Type::Int {
            return Err((Some(span), "Main function should have return type of Int"));
        }
        if rstmt.get_type() != Type::Int {
            return Err((
                Some(span),
                "Return value of main function doesn't match defined type",
            ));
        }

        Ok(FnAst::new(
            Tnode::Connector {
                left: Box::new(body),
                right: Box::new(rstmt),
            },
            0,
            PARSER.lock().unwrap().lst().get_size(),
        ))
    }
}

pub struct FnAst {
    root: Tnode,
    label: i16,
    lvar_count: i16,
}

impl FnAst {
    pub fn new(root: Tnode, label: i16, lvar_count: i16) -> Self {
        Self {
            root,
            label,
            lvar_count,
        }
    }
    pub fn get_label(&self) -> i16 {
        self.label
    }
    pub fn get_root(&self) -> &Tnode {
        &self.root
    }
    pub fn get_lvar_count(&self) -> i16 {
        self.lvar_count
    }
}
