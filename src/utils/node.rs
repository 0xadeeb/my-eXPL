use crate::{ast::*, SYMBOL_TABLE};
use lrlex::DefaultLexeme;
use lrpar::{Lexeme, NonStreamingLexer, Span};
use std::collections::LinkedList;

pub fn create_int_node(
    op: Op,
    span: Span,
    left: Tnode,
    right: Tnode,
) -> Result<Tnode, (Option<Span>, &'static str)> {
    match (left.get_type(), right.get_type()) {
        (Type::Int, Type::Int) => {}
        _ => return Err((Some(span), "Type mismatch, expected integer")),
    }
    Ok(Tnode::Operator {
        op,
        span,
        dtype: Type::Int,
        lhs: Box::new(left),
        rhs: Box::new(right),
    })
}

pub fn create_bool_node(
    op: Op,
    span: Span,
    left: Tnode,
    right: Tnode,
) -> Result<Tnode, (Option<Span>, &'static str)> {
    match (left.get_type(), right.get_type()) {
        (Type::Int, Type::Int) => {}
        _ => return Err((Some(span), "Type mismatch, expected integer")),
    }
    Ok(Tnode::Operator {
        op,
        span,
        dtype: Type::Bool,
        lhs: Box::new(left),
        rhs: Box::new(right),
    })
}

pub fn create_asg_node(
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

pub fn create_read_node(span: Span, mut var: Tnode) -> Result<Tnode, (Option<Span>, &'static str)> {
    var.set_ref(RefType::LHS)
        .map_err(|msg| (var.get_span(), msg))?;
    Ok(Tnode::Read {
        span,
        var: Box::new(var),
    })
}

pub fn create_write_node(span: Span, e: Tnode) -> Result<Tnode, (Option<Span>, &'static str)> {
    match e.get_type() {
        Type::Int | Type::String => {}
        _ => return Err((e.get_span(), "Type mismatch, expected integer or string")),
    }
    Ok(Tnode::Write {
        span,
        expression: Box::new(e),
    })
}

pub fn create_while_node(
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

pub fn create_repeat_node(
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

pub fn create_if_node(
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

pub fn create_constant_node(
    lexer: &dyn NonStreamingLexer<DefaultLexeme, u32>,
    token: &DefaultLexeme,
    dtype: Type,
) -> Result<Tnode, (Option<Span>, &'static str)> {
    match dtype {
        Type::Int => match lexer.span_str(token.span()).parse::<u32>() {
            Ok(val) => Ok(Tnode::Constant {
                span: token.span(),
                dtype: Type::Int,
                value: val.to_string(),
            }),
            Err(_) => Err((Some(token.span()), "Can't parse to u32")),
        },
        Type::String => Ok(Tnode::Constant {
            span: token.span(),
            dtype: Type::String,
            value: lexer.span_str(token.span()).to_string(),
        }),
        _ => Err((
            Some(token.span()),
            "Only variables of type int and string can be stored in a variable",
        )),
    }
}

pub fn parse_int(
    lexer: &dyn NonStreamingLexer<DefaultLexeme, u32>,
    token: &DefaultLexeme,
) -> Result<u32, (Option<Span>, &'static str)> {
    match lexer.span_str(token.span()).parse::<u32>() {
        Ok(val) => Ok(val),
        Err(_) => Err((Some(token.span()), "Can't parse to u32")),
    }
}

pub fn insert_variables(
    lexer: &dyn NonStreamingLexer<DefaultLexeme, u32>,
    dtype: Type,
    var_list: LinkedList<(Span, Vec<u32>)>,
) -> Result<(), (Option<Span>, &'static str)> {
    let mut s_table = SYMBOL_TABLE.lock().unwrap();
    for var in var_list.iter() {
        let name = lexer.span_str(var.0);
        if let Err(()) = s_table.insert(name.to_string(), dtype, &var.1) {
            return Err((Some(var.0), "Varible is declared multiple times"));
        }
    }
    Ok(())
}

pub fn get_variable(
    lexer: &dyn NonStreamingLexer<DefaultLexeme, u32>,
    token: &DefaultLexeme,
    access: Vec<Box<Tnode>>,
    ref_type: RefType,
) -> Result<Tnode, (Option<Span>, &'static str)> {
    let name = lexer.span_str(token.span());
    let entry = SYMBOL_TABLE
        .lock()
        .unwrap()
        .get(name)
        .ok_or((Some(token.span()), "Varible was not declared"))?
        .clone();

    Ok(Tnode::Var {
        span: token.span(),
        symbol: entry,
        ref_type,
        access,
    })
}
