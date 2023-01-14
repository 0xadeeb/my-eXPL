use crate::ast::*;
use lrlex::DefaultLexeme;
use lrpar::{Lexeme, NonStreamingLexer, Span};

pub fn create_int_node(
    op: Op,
    span: Span,
    left: Tnode,
    right: Tnode,
) -> Result<Tnode, (Option<Span>, &'static str)> {
    match (left.get_type(), right.get_type()) {
        (Type::Int, Type::Int) => {}
        _ => return Err((Some(span), "Type mismatch, excepted integer")),
    }
    Ok(Tnode::Operator {
        op,
        span,
        ttype: Type::Int,
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
        _ => return Err((Some(span), "Type mismatch, excepted integer")),
    }
    Ok(Tnode::Operator {
        op,
        span,
        ttype: Type::Bool,
        lhs: Box::new(left),
        rhs: Box::new(right),
    })
}

pub fn create_asg_node(
    _span: Span,
    left: Tnode,
    right: Tnode,
) -> Result<Tnode, (Option<Span>, &'static str)> {
    match right.get_type() {
        Type::Int => {}
        _ => return Err((right.get_span(), "Type mismatch, excepted integer")),
    }
    Ok(Tnode::Asgn {
        lhs: Box::new(left),
        rhs: Box::new(right),
    })
}

pub fn create_write_node(span: Span, e: Tnode) -> Result<Tnode, (Option<Span>, &'static str)> {
    match e.get_type() {
        Type::Int => {}
        _ => return Err((e.get_span(), "Type mismatch, excepted integer")),
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
) -> Result<Tnode, (Option<Span>, &'static str)> {
    match lexer.span_str(token.span()).parse::<u32>() {
        Ok(val) => Ok(Tnode::Constant {
            span: token.span(),
            ttype: Type::Int,
            value: val.to_string(),
        }),
        Err(_) => Err((Some(token.span()), "Can't parse to u32")),
    }
}
