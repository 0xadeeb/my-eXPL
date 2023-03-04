// A lot of parser helper functions which also does semantic analysis

use crate::{ast::*, frontend::PARSER, symbol::*, type_table::*};
use lrlex::DefaultLexeme;
use lrpar::{Lexeme, NonStreamingLexer, Span};
use std::collections::LinkedList;

pub fn parse_int(
    lexer: &dyn NonStreamingLexer<DefaultLexeme, u32>,
    token: &DefaultLexeme,
) -> Result<u32, (Option<Span>, &'static str)> {
    match lexer.span_str(token.span()).parse::<u32>() {
        Ok(val) => Ok(val),
        Err(_) => Err((Some(token.span()), "Can't parse to u32")),
    }
}

fn insert(
    mut var_list: LinkedList<SymbolBuilder>,
    inner_type: Type,
    lexer: &dyn NonStreamingLexer<DefaultLexeme, u32>,
    s_table: &mut SymbolTable,
    base: i16,
) -> Result<(), (Option<Span>, &'static str)> {
    while let Some(mut var) = var_list.pop_front() {
        var.dtype(inner_type.clone());
        let span = var.get_name();

        s_table
            .insert_builder(var, base, lexer)
            .map_err(|msg| (Some(span), msg))?;
    }
    Ok(())
}

pub fn insert_gst(
    var_list: LinkedList<SymbolBuilder>,
    inner_type: Type,
    lexer: &dyn NonStreamingLexer<DefaultLexeme, u32>,
) -> Result<(), (Option<Span>, &'static str)> {
    let mut parser = PARSER.lock().unwrap();
    insert(var_list, inner_type, lexer, parser.gst(), 4099)
}

pub fn insert_lst(
    var_list: LinkedList<SymbolBuilder>,
    inner_type: Type,
    lexer: &dyn NonStreamingLexer<DefaultLexeme, u32>,
) -> Result<(), (Option<Span>, &'static str)> {
    let mut parser = PARSER.lock().unwrap();
    insert(var_list, inner_type, lexer, parser.lst(), 1)
}

pub fn get_variable(
    lexer: &dyn NonStreamingLexer<DefaultLexeme, u32>,
    token: &DefaultLexeme,
    array_access: Vec<Box<Tnode>>,
    fields: LinkedList<Span>,
    ref_type: RefType,
) -> Result<Tnode, (Option<Span>, &'static str)> {
    let mut parser = PARSER.lock().unwrap();
    let name = lexer.span_str(token.span());
    let entry = parser
        .get_var(name)
        .map_err(|msg| (Some(token.span()), msg))?;

    if entry.get_dim() != array_access.len() as i16 {
        return Err((
            Some(token.span()),
            "Array access dimension does not match the declared dimension",
        ));
    }

    let mut tinstance = entry.get_type();
    let mut field_access = vec![];
    for fspan in fields.iter() {
        let fname = lexer.span_str(*fspan);
        let (idx, tname) = tinstance
            .field_list()
            .map_err(|msg| (Some(token.span()), msg))?
            .get(fname)
            .ok_or((Some(*fspan), "This field is not present in the type"))?;
        field_access.push(*idx as u8);
        tinstance = parser
            .tt()
            .get(tname)
            .ok_or((Some(*fspan), "This field is not present in the type"))?;
    }

    Ok(Tnode::Var {
        span: token.span(),
        symbol: entry,
        array_access,
        field_access,
        ref_type,
        dtype: tinstance,
    })
}

pub fn check_access_vec(
    exp: Vec<Box<Tnode>>,
) -> Result<Vec<Box<Tnode>>, (Option<Span>, &'static str)> {
    for e in &exp {
        match e.get_type() {
            Type::Primitive(PrimitiveType::Int) => {}
            _ => return Err((e.get_span(), "Index should be of type integer")),
        }
    }
    Ok(exp)
}

pub fn insert_args(
    params: LinkedList<(Type, String)>,
    span: Span,
) -> Result<(), (Option<Span>, &'static str)> {
    let mut parser = PARSER.lock().unwrap();

    let mut i = parser.cfn_params().into_iter();
    let mut j = params.iter();
    let mut counter = params.len() as i16 + 2;

    loop {
        match (&i.next(), j.next()) {
            (Some((t1, n1)), Some((t2, n2))) if t1 == t2 && n1 == n2 => {
                parser
                    .lst()
                    .insert_arg(Symbol::Variable {
                        name: n1.to_string(),
                        binding: -1 * counter,
                        dtype: t1.clone(),
                        is_static: false,
                    })
                    .map_err(|msg| (Some(span), msg))?;
                counter -= 1;
            }
            (None, None) => break,
            _ => {
                return Err((
                    Some(span),
                    "Arguments of function don't match with definition",
                ));
            }
        }
    }
    Ok(())
}

pub fn create_int(
    op: BinaryOpType,
    span: Span,
    left: Tnode,
    right: Tnode,
) -> Result<Tnode, (Option<Span>, &'static str)> {
    match (left.get_type(), right.get_type()) {
        (
            Type::Primitive(PrimitiveType::Int) | Type::Primitive(PrimitiveType::Null),
            Type::Primitive(PrimitiveType::Int) | Type::Primitive(PrimitiveType::Null),
        ) => {}
        _ => return Err((Some(span), "Type mismatch, expected integer")),
    }
    Ok(Tnode::BinaryOperator {
        op,
        span,
        dtype: Type::Primitive(PrimitiveType::Int),
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
    if left.get_type() != right.get_type()
        && left.get_type() != Type::Primitive(PrimitiveType::Null)
        && right.get_type() != Type::Primitive(PrimitiveType::Null)
    {
        return Err((Some(span), "Type mismatch"));
    }
    Ok(Tnode::BinaryOperator {
        op,
        span,
        dtype: Type::Primitive(PrimitiveType::Bool),
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
        (
            Type::Primitive(PrimitiveType::Bool) | Type::Primitive(PrimitiveType::Null),
            Type::Primitive(PrimitiveType::Bool) | Type::Primitive(PrimitiveType::Null),
        ) => {}
        _ => return Err((Some(span), "Type mismatch, expected boolean")),
    }
    Ok(Tnode::BinaryOperator {
        op,
        span,
        dtype: Type::Primitive(PrimitiveType::Bool),
        lhs: Box::new(left),
        rhs: Box::new(right),
    })
}

pub fn create_asg(
    span: Span,
    mut left: Tnode,
    right: Tnode,
) -> Result<Tnode, (Option<Span>, &'static str)> {
    if right.get_type() != left.get_type()
        && right.get_type() != Type::Primitive(PrimitiveType::Null)
    {
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
        Type::Primitive(PrimitiveType::Int)
        | Type::Primitive(PrimitiveType::Str)
        | Type::Pointer(..) => {}
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
        Type::Primitive(PrimitiveType::Bool) => {}
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
        Type::Primitive(PrimitiveType::Bool) => {}
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
        Type::Primitive(PrimitiveType::Bool) => {}
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
        Type::Primitive(PrimitiveType::Int) => match lexer.span_str(token.span()).parse::<i32>() {
            Ok(val) => Ok(Tnode::Constant {
                span: token.span(),
                dtype: Type::Primitive(PrimitiveType::Int),
                value: val.to_string(),
            }),
            Err(_) => Err((Some(token.span()), "Can't parse to i32")),
        },
        Type::Primitive(PrimitiveType::Str) => Ok(Tnode::Constant {
            span: token.span(),
            dtype: Type::Primitive(PrimitiveType::Str),
            value: lexer.span_str(token.span()).to_string(),
        }),
        _ => Err((
            Some(token.span()),
            "Only variables of type int and string can be stored in a variable",
        )),
    }
}

pub fn create_return(span: Span, ret_stmt: Tnode) -> Result<Tnode, (Option<Span>, &'static str)> {
    if PARSER.lock().unwrap().cfn_rtype() != ret_stmt.get_type() {
        return Err((
            Some(span),
            "Return expression of this function doesn't match the defined return type",
        ));
    }
    Ok(Tnode::Return {
        span,
        exp: Box::new(ret_stmt),
    })
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
    if rtype != Type::Primitive(PrimitiveType::Int) {
        return Err((Some(span), "Main function should have return type of Int"));
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

pub fn create_alloc(span: Span, mut var: Tnode) -> Result<Tnode, (Option<Span>, &'static str)> {
    match &var {
        Tnode::Var { dtype, .. } => match dtype {
            Type::Primitive(..) => {
                return Err((Some(span), "Can't assign memory to stack variable"))
            }
            _ => {}
        },
        _ => return Err((Some(span), "Memory can be assigned to only variables")),
    }
    var.set_ref(RefType::LHS).unwrap();
    Ok(Tnode::Alloc {
        span,
        var: Box::new(var),
    })
}

pub fn create_free(span: Span, var: Tnode) -> Result<Tnode, (Option<Span>, &'static str)> {
    match &var {
        Tnode::Var { dtype, .. } => match dtype {
            Type::Primitive(..) => return Err((Some(span), "Can't free memory of stack variable")),
            _ => {}
        },
        _ => return Err((Some(span), "Memory can be freed only for variables")),
    }
    Ok(Tnode::Free {
        span,
        var: Box::new(var),
    })
}
