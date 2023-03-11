// A lot of parser helper functions which also does semantic analysis

use crate::{ast::*, error::SemanticError, symbol::*, type_table::*};
use lrlex::DefaultLexeme;
use lrpar::{Lexeme, NonStreamingLexer, Span};
use std::collections::LinkedList;

use super::parser_state::ParserState;

pub fn insert(
    mut var_list: LinkedList<SymbolBuilder>,
    inner_type: Type,
    s_table: &mut SymbolTable,
    base: i16,
    tt: &TypeTable,
    lexer: &dyn NonStreamingLexer<DefaultLexeme, u32>,
) -> Result<(), SemanticError> {
    while let Some(mut var) = var_list.pop_front() {
        var.dtype(inner_type.clone());
        let span = var.get_name();

        s_table
            .insert_builder(var, base, tt, lexer)
            .map_err(|msg| SemanticError::new(Some(span), &msg))?;
    }
    Ok(())
}

pub fn get_variable(
    lexer: &dyn NonStreamingLexer<DefaultLexeme, u32>,
    token: &DefaultLexeme,
    array_access: Vec<Box<Tnode>>,
    fields: LinkedList<Span>,
    ref_type: RefType,
    parser: &ParserState,
) -> Result<Tnode, SemanticError> {
    let name = lexer.span_str(token.span());
    let tt = parser.tt();
    let entry = parser
        .get_var(name)
        .map_err(|msg| SemanticError::new(Some(token.span()), &msg))?;

    if entry.get_dim().unwrap() != array_access.len() as u8 {
        return Err(SemanticError::new(
            Some(token.span()),
            "Array access dimension does not match the declared dimension",
        ));
    }

    let mut tinstance = entry.get_type().clone();
    let mut field_access = vec![];
    for fspan in fields.iter() {
        let fname = lexer.span_str(*fspan);
        let symbol = tinstance
            .symbol_list(tt)
            .map_err(|msg| SemanticError::new(Some(token.span()), &msg))?
            .get(fname)
            .ok_or(SemanticError::new(
                Some(*fspan),
                "This field is not present in the type",
            ))?;
        field_access.push(symbol.get_idx().unwrap());
        tinstance = tt
            .get(symbol.get_name())
            .ok_or(SemanticError::new(
                Some(*fspan),
                "This field is not present in the type",
            ))?
            .to_type();
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

pub fn check_access_vec(exp: Vec<Box<Tnode>>) -> Result<Vec<Box<Tnode>>, SemanticError> {
    for e in &exp {
        match e.get_type() {
            Type::Int => {}
            _ => {
                return Err(SemanticError::new(
                    e.get_span().cloned(),
                    "Index should be of type integer",
                ))
            }
        }
    }
    Ok(exp)
}

pub fn insert_args(
    lexer: &dyn NonStreamingLexer<DefaultLexeme, u32>,
    params: LinkedList<(Type, Span)>,
    span: Span,
    parser: &mut ParserState,
) -> Result<(), SemanticError> {
    let mut i = parser.cfn_params().into_iter();
    let mut j = params.iter();
    let mut counter = params.len() as i16 + 2;

    loop {
        match (&i.next(), j.next()) {
            (Some((t1, n1)), Some((t2, n2))) if t1 == t2 && n1 == lexer.span_str(*n2) => {
                parser
                    .lst_mut()
                    .insert_symbol(
                        Symbol::Variable {
                            name: n1.to_string(),
                            binding: -1 * counter,
                            dtype: t1.clone(),
                            is_static: false,
                        },
                        true,
                    )
                    .map_err(|msg| SemanticError::new(Some(span), &msg))?;
                counter -= 1;
            }
            (None, None) => break,
            _ => {
                return Err(SemanticError::new(
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
) -> Result<Tnode, SemanticError> {
    match (left.get_type(), right.get_type()) {
        (Type::Int, Type::Int) => {}
        t @ _ => {
            return Err(SemanticError::new(
                Some(span),
                &format!(
                    "Type mismatch, expected integer, found {:?} and {:?}",
                    t.0, t.1
                ),
            ))
        }
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
) -> Result<Tnode, SemanticError> {
    if left.get_type() != right.get_type()
        && left.get_type() != &Type::Null
        && right.get_type() != &Type::Null
    {
        return Err(SemanticError::new(
            Some(span),
            &format!(
                "Type mismatch, found {:?} and {:?}",
                left.get_type(),
                right.get_type()
            ),
        ));
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
) -> Result<Tnode, SemanticError> {
    match (left.get_type(), right.get_type()) {
        (Type::Bool, Type::Bool) => {}
        t @ _ => {
            return Err(SemanticError::new(
                Some(span),
                &format!(
                    "Type mismatch, expected boolean found {:?} and {:?}",
                    t.0, t.1
                ),
            ))
        }
    }
    Ok(Tnode::BinaryOperator {
        op,
        span,
        dtype: Type::Bool,
        lhs: Box::new(left),
        rhs: Box::new(right),
    })
}

pub fn create_asg(span: Span, mut left: Tnode, right: Tnode) -> Result<Tnode, SemanticError> {
    if right.get_type() != left.get_type() && right.get_type() != &Type::Null {
        return Err(SemanticError::new(
            Some(span),
            "LHS and RHS types of assignment statment don't match",
        ));
    }
    left.set_ref(RefType::LHS)
        .map_err(|msg| SemanticError::new(left.get_span().cloned(), &msg))?;
    Ok(Tnode::Asgn {
        lhs: Box::new(left),
        rhs: Box::new(right),
    })
}

pub fn create_ref(span: Span, mut exp: Tnode) -> Result<Tnode, SemanticError> {
    match exp {
        Tnode::Var { .. } => {
            exp.set_ref(RefType::LHS).unwrap();
        }
        _ => {
            return Err(SemanticError::new(
                Some(span),
                "Referencing just defined for variables",
            ))
        }
    }
    Ok(Tnode::RefOperator {
        span,
        dtype: exp
            .get_type()
            .rref()
            .map_err(|msg| SemanticError::new(Some(span), &msg))?,
        var: Box::new(exp),
    })
}

pub fn create_deref(span: Span, exp: Tnode) -> Result<Tnode, SemanticError> {
    match exp {
        Tnode::Var { .. } => {}
        _ => {
            return Err(SemanticError::new(
                Some(span),
                "Dereferencing just defined for variables",
            ))
        }
    }
    Ok(Tnode::DeRefOperator {
        span,
        dtype: exp
            .get_type()
            .deref()
            .map_err(|msg| SemanticError::new(Some(span), &msg))?,
        ref_type: RefType::RHS,
        var: Box::new(exp),
    })
}

pub fn create_read(span: Span, mut var: Tnode) -> Result<Tnode, SemanticError> {
    var.set_ref(RefType::LHS)
        .map_err(|msg| SemanticError::new(var.get_span().cloned(), &msg))?;
    Ok(Tnode::Read {
        span,
        var: Box::new(var),
    })
}

pub fn create_write(span: Span, e: Tnode) -> Result<Tnode, SemanticError> {
    match e.get_type() {
        Type::Int | Type::Str | Type::Pointer(..) => {}
        _ => {
            return Err(SemanticError::new(
                e.get_span().cloned(),
                "Type mismatch, expected integer or string",
            ))
        }
    }
    Ok(Tnode::Write {
        span,
        expression: Box::new(e),
    })
}

pub fn create_while(_span: Span, condition: Tnode, stmts: Tnode) -> Result<Tnode, SemanticError> {
    match condition.get_type() {
        Type::Bool => {}
        _ => {
            return Err(SemanticError::new(
                condition.get_span().cloned(),
                "Type mismatch, excepted boolen",
            ))
        }
    }
    Ok(Tnode::While {
        condition: Box::new(condition),
        stmts: Box::new(stmts),
    })
}

pub fn create_repeat(_span: Span, stmts: Tnode, condition: Tnode) -> Result<Tnode, SemanticError> {
    match condition.get_type() {
        Type::Bool => {}
        _ => {
            return Err(SemanticError::new(
                condition.get_span().cloned(),
                "Type mismatch, excepted boolen",
            ))
        }
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
) -> Result<Tnode, SemanticError> {
    match condition.get_type() {
        Type::Bool => {}
        _ => {
            return Err(SemanticError::new(
                condition.get_span().cloned(),
                "Type mismatch, excepted boolen",
            ))
        }
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
) -> Result<Tnode, SemanticError> {
    match dtype {
        Type::Int => match lexer.span_str(token.span()).parse::<i32>() {
            Ok(val) => Ok(Tnode::Constant {
                span: token.span(),
                dtype: Type::Int,
                value: val.to_string(),
            }),
            Err(_) => Err(SemanticError::new(Some(token.span()), "Can't parse to i32")),
        },
        Type::Str => Ok(Tnode::Constant {
            span: token.span(),
            dtype: Type::Str,
            value: lexer.span_str(token.span()).to_string(),
        }),
        _ => Err(SemanticError::new(
            Some(token.span()),
            "Only variables of type int and string can be stored in a variable",
        )),
    }
}

pub fn create_return(
    span: Span,
    ret_stmt: Tnode,
    parser: &ParserState,
) -> Result<Tnode, SemanticError> {
    if parser.cfn_rtype() != ret_stmt.get_type() {
        return Err(SemanticError::new(
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
    lst: &SymbolTable,
    cfn: &Symbol,
) -> Result<FnAst, SemanticError> {
    if cfn.get_type() != &rtype {
        return Err(SemanticError::new(Some(span), "Return type don't match"));
    }
    Ok(FnAst::new(
        Tnode::Connector {
            left: Box::new(body),
            right: Box::new(rstmt),
        },
        cfn.get_label().cloned().unwrap(),
        lst.get_size().to_owned() as u8,
    ))
}

pub fn create_main_block(
    rtype: Type,
    body: Tnode,
    rstmt: Tnode,
    span: Span,
    lst: &SymbolTable,
) -> Result<FnAst, SemanticError> {
    if rtype != Type::Int {
        return Err(SemanticError::new(
            Some(span),
            "Main function should have return type of Int",
        ));
    }

    Ok(FnAst::new(
        Tnode::Connector {
            left: Box::new(body),
            right: Box::new(rstmt),
        },
        0,
        lst.get_size().to_owned() as u8,
    ))
}

pub fn create_fncall(
    fname: &str,
    args: LinkedList<Tnode>,
    span: Span,
    gst: &SymbolTable,
) -> Result<Tnode, SemanticError> {
    let symbol = gst
        .get(fname)
        .filter(|s| matches!(s, Symbol::Function { .. }))
        .ok_or(SemanticError::new(Some(span), "Function was not defined"))?
        .clone();

    let mut i = symbol.get_params().unwrap().into_iter();
    let mut j = args.iter();

    loop {
        match (i.next(), j.next()) {
            (Some((t, _)), Some(e)) if t == e.get_type() => {}
            (None, None) => break,
            t @ _ => return Err(SemanticError::new(
                Some(span),
                &format!(
                    "Arguments of function don't match with definition, expected {:?} found {:?}",
                    t.0, t.1
                ),
            )),
        }
    }

    Ok(Tnode::FnCall {
        span,
        symbol,
        args: LinkedList::from_iter(args.iter().map(|node| Box::new(node.clone()))),
    })
}

pub fn create_alloc(span: Span, mut var: Tnode) -> Result<Tnode, SemanticError> {
    match var {
        Tnode::Var { ref dtype, .. } => match dtype {
            Type::UserDef(..) => {}
            _ => {
                return Err(SemanticError::new(
                    Some(span),
                    "Can't assign memory to stack variable",
                ))
            }
        },
        _ => {
            return Err(SemanticError::new(
                Some(span),
                "Memory can be assigned to only variables",
            ))
        }
    }
    var.set_ref(RefType::LHS).unwrap();
    Ok(Tnode::Alloc {
        span,
        var: Box::new(var),
    })
}

pub fn create_free(span: Span, var: Tnode) -> Result<Tnode, SemanticError> {
    match &var {
        Tnode::Var { dtype, .. } => match dtype {
            Type::UserDef(..) => {}
            _ => {
                return Err(SemanticError::new(
                    Some(span),
                    "Can't free memory of stack variable",
                ))
            }
        },
        _ => {
            return Err(SemanticError::new(
                Some(span),
                "Memory can be freed only for variables",
            ))
        }
    }
    Ok(Tnode::Free {
        span,
        var: Box::new(var),
    })
}
