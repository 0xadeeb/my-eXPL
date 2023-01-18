use crate::symbol_table::Gsymbol;
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

pub fn create_read_node(
    lexer: &dyn NonStreamingLexer<DefaultLexeme, u32>,
    token: &DefaultLexeme,
) -> Result<Tnode, (Option<Span>, &'static str)> {
    Ok(Tnode::Read {
        span: token.span(),
        var: Box::new(get_variable(lexer, token)?),
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

pub fn insert_variables(
    lexer: &dyn NonStreamingLexer<DefaultLexeme, u32>,
    ttype: Type,
    var_list: LinkedList<Span>,
) -> Result<(), (Option<Span>, &'static str)> {
    let mut s_table = SYMBOL_TABLE.lock().unwrap();
    for var in var_list.iter() {
        let name = lexer.span_str(*var);
        if s_table.contains_key(name) {
            return Err((Some(*var), "The variable was already declared"));
        }
        let len = s_table.len() as u16;
        s_table.insert(
            name.to_string(),
            Gsymbol::Variable {
                name: name.to_string(),
                binding: 4096 + len,
                size: 1,
                ttype,
            },
        );
    }
    Ok(())
}

pub fn get_variable(
    lexer: &dyn NonStreamingLexer<DefaultLexeme, u32>,
    token: &DefaultLexeme,
) -> Result<Tnode, (Option<Span>, &'static str)> {
    let name = lexer.span_str(token.span());
    let var_entry = SYMBOL_TABLE
        .lock()
        .unwrap()
        .get(name)
        .ok_or((Some(token.span()), "Varible was not declared"))?
        .clone();

    Ok(Tnode::Var {
        span: token.span(),
        symbol: var_entry,
    })
}
