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
    lexer: &dyn NonStreamingLexer<DefaultLexeme, u32>,
) -> Result<(), SemanticError> {
    while let Some(mut var) = var_list.pop_front() {
        var.dtype(inner_type.clone());
        let span = var.get_name();

        s_table
            .insert_builder(var, base, lexer)
            .map_err(|msg| SemanticError::new(Some(span), &msg))?;
    }
    Ok(())
}

pub fn get_variable(
    lexer: &dyn NonStreamingLexer<DefaultLexeme, u32>,
    token: &DefaultLexeme,
    array_access: Vec<Tnode>,
    fields: LinkedList<Span>,
    parser: &ParserState,
) -> Result<Tnode, SemanticError> {
    let name = lexer.span_str(token.span());
    let tt = &parser.type_table;
    let entry = parser
        .get_var(name)
        .map_err(|msg| SemanticError::new(Some(token.span()), &msg))?;

    if entry.get_dim().unwrap() != array_access.len() as u8 {
        return Err(SemanticError::new(
            Some(token.span()),
            "Array access dimension does not match the declared dimension",
        ));
    }

    let mut tinstance = entry.get_type().inner_type();
    let mut field_access = vec![];
    for fspan in fields.iter() {
        let fname = lexer.span_str(*fspan);
        let symbol = tinstance
            .symbol_list(tt)
            .map_err(|msg| {
                SemanticError::new(Some(Span::new(token.span().start(), fspan.start())), &msg)
            })?
            .get(fname)
            .filter(|s| !matches!(s, Symbol::Function { .. }))
            .ok_or(SemanticError::new(
                Some(*fspan),
                &format!("This field is not present in the type {:?}", tinstance),
            ))?;
        field_access.push(symbol.get_idx().unwrap());
        tinstance = symbol.get_type();
    }

    Ok(Tnode::Var {
        span: token.span(),
        symbol: entry.clone(),
        array_access,
        field_access,
        ref_type: RefType::RHS,
        dtype: tinstance.clone(),
    })
}

pub fn check_access_vec(exp: Vec<Tnode>) -> Result<Vec<Tnode>, SemanticError> {
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
    let mut i = parser.cfn_params().into_iter().rev();
    let mut j = params.iter().rev();
    let mut counter = -2;

    loop {
        match (&i.next(), j.next()) {
            (Some((t1, n1)), Some((t2, n2))) if t1 == t2 && n1 == lexer.span_str(*n2) => {
                counter -= t1.get_size() as i16;
                parser
                    .lst
                    .insert_symbol(
                        Symbol::Variable {
                            name: n1.to_string(),
                            binding: counter,
                            dtype: t1.clone(),
                            is_static: false,
                        },
                        true,
                        true,
                    )
                    .map_err(|msg| SemanticError::new(Some(span), &msg))?;
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

    if let Some(ref cinst) = parser.cur_class {
        parser
            .lst
            .insert_symbol(
                Symbol::Variable {
                    name: "self".to_owned(),
                    binding: counter - 2,
                    dtype: cinst.clone(),
                    is_static: false,
                },
                true,
                true,
            )
            .map_err(|msg| SemanticError::new(Some(span), &msg))?;
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
    if left.get_type() != right.get_type() && right.get_type() != &Type::Null {
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

// Checks if class B is decendant class A
fn is_descendent(tt: &TypeTable, a: &Type, b: &Type) -> bool {
    match (a, b) {
        (Type::UserDef { size: s1, .. }, Type::UserDef { size: s2, .. })
            if *s1 == 2 && *s2 == 2 => {}
        _ => return false,
    }
    let mut inst = b;
    loop {
        if a == inst {
            return true;
        }
        inst = match tt
            .get(inst.get_name().unwrap())
            .unwrap()
            .get_parent()
            .unwrap()
        {
            Some(c) => c,
            None => break,
        };
    }
    false
}

pub fn create_asg(
    span: Span,
    mut left: Tnode,
    mut right: Tnode,
    tt: &TypeTable,
) -> Result<Tnode, SemanticError> {
    let lt = left.get_type();
    let rt = right.get_type();
    let is_desc = is_descendent(tt, lt, rt);
    if lt != rt && rt != &Type::Null && !is_desc {
        return Err(SemanticError::new(
            Some(span),
            &format!(
                "LHS type ({:?}) and RHS type ({:?}) of assignment statment don't match",
                lt, rt
            ),
        ));
    }
    left.set_ref(RefType::LHS)
        .map_err(|msg| SemanticError::new(left.get_span().cloned(), &msg))?;
    if is_desc {
        right
            .set_ref(RefType::LHS)
            .map_err(|msg| SemanticError::new(right.get_span().cloned(), &msg))?;
    }
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
    if var.get_type() != &Type::Int && var.get_type() != &Type::Str {
        return Err(SemanticError::new(
            Some(span),
            &format!(
                "Type mismatch, can't read into variable of type {:?}",
                var.get_type()
            ),
        ));
    }
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
        dt @ _ => {
            return Err(SemanticError::new(
                e.get_span().cloned(),
                &format!("Type mismatch, expected integer or string, found {:?}", dt),
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
        dt @ _ => {
            return Err(SemanticError::new(
                condition.get_span().cloned(),
                &format!("Type mismatch, excepted boolen, found {:?}", dt),
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
        dt @ _ => {
            return Err(SemanticError::new(
                condition.get_span().cloned(),
                &format!("Type mismatch, excepted boolen, found {:?}", dt),
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
        dt @ _ => {
            return Err(SemanticError::new(
                condition.get_span().cloned(),
                &format!("Type mismatch, excepted boolen, found {:?}", dt),
            ))
        }
    }
    Ok(Tnode::If {
        condition: Box::new(condition),
        if_stmt: Box::new(if_stmts),
        else_stmt: else_stmts.map(Box::new),
    })
}

pub fn create_constant(
    lexer: &dyn NonStreamingLexer<DefaultLexeme, u32>,
    span: Span,
    token: &DefaultLexeme,
    dtype: Type,
    is_neg: bool,
) -> Result<Tnode, SemanticError> {
    match dtype {
        Type::Int => match lexer.span_str(token.span()).parse::<i32>() {
            Ok(mut val) => {
                if is_neg {
                    val *= -1;
                }
                Ok(Tnode::Constant {
                    span,
                    dtype: Type::Int,
                    value: val.to_string(),
                })
            }
            Err(_) => Err(SemanticError::new(Some(token.span()), "Can't parse to i32")),
        },
        Type::Str => Ok(Tnode::Constant {
            span,
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
    if cfn.get_type() != &rtype && rtype != Type::Null {
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

    let mut i = symbol.get_params().unwrap().iter();
    let mut j = args.iter();

    loop {
        match (i.next(), j.next()) {
            (Some((t, _)), Some(e)) if t == e.get_type() => {}
            (None, None) => break,
            t @ _ => {
                return Err(SemanticError::new(
                    Some(span),
                    &format!(
                    "Arguments of function don't match with definition, expected {:?} found {:?}",
                    t.0, t.1
                ),
                ))
            }
        }
    }

    Ok(Tnode::FnCall { span, symbol, args })
}

pub fn create_method_call(
    lexer: &dyn NonStreamingLexer<DefaultLexeme, u32>,
    var: Span,
    array_access: Vec<Tnode>,
    mut fields: LinkedList<Span>,
    args: LinkedList<Tnode>,
    span: Span,
    parser: &ParserState,
) -> Result<Tnode, SemanticError> {
    let vname = lexer.span_str(var);
    let entry = parser
        .get_var(vname)
        .map_err(|msg| SemanticError::new(Some(var), &msg))?;

    if entry.get_dim().unwrap() != array_access.len() as u8 {
        return Err(SemanticError::new(
            Some(span),
            "Array access dimension does not match the declared dimension",
        ));
    }

    let method_span = fields.pop_back().unwrap();
    let tt = &parser.type_table;
    let mut tinstance = entry.get_type().inner_type();
    let mut field_access = vec![];

    for fspan in fields.iter() {
        let fname = lexer.span_str(*fspan);
        let symbol = tinstance
            .symbol_list(tt)
            .map_err(|msg| SemanticError::new(Some(Span::new(var.start(), fspan.start())), &msg))?
            .get(fname)
            .filter(|s| !matches!(s, Symbol::Function { .. }))
            .ok_or(SemanticError::new(
                Some(*fspan),
                &format!("This field is not present in the type {:?}", tinstance),
            ))?;
        field_access.push(symbol.get_idx().unwrap());
        tinstance = symbol.get_type();
    }

    let method = tinstance
        .symbol_list(tt)
        .map_err(|msg| SemanticError::new(Some(Span::new(var.start(), method_span.start())), &msg))?
        .get(lexer.span_str(method_span))
        .filter(|s| matches!(s, Symbol::Function { .. }))
        .ok_or(SemanticError::new(
            Some(method_span),
            &format!("This method is not present in the type {:?}", tinstance),
        ))?;

    Ok(Tnode::MethodCall {
        span,
        var: entry,
        array_access,
        field_access,
        args,
        symbol: method.clone(),
    })
}

pub fn create_alloc(span: Span, mut var: Tnode) -> Result<Tnode, SemanticError> {
    match var {
        Tnode::Var { ref dtype, .. } | Tnode::DeRefOperator { ref dtype, .. } => match dtype {
            Type::UserDef { size, .. } => {
                if *size == 2 {
                    return Err(SemanticError::new(
                        Some(span),
                        "Alloc is for struct type variables not class variables",
                    ));
                }
            }
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

pub fn create_new(
    lexer: &dyn NonStreamingLexer<DefaultLexeme, u32>,
    span: Span,
    mut var: Tnode,
    cspan: Span,
    tt: &TypeTable,
) -> Result<Tnode, SemanticError> {
    let c_idx = match var {
        Tnode::Var { ref dtype, .. } | Tnode::DeRefOperator { ref dtype, .. } => match dtype {
            Type::UserDef { size, .. } => {
                if *size == 1 {
                    return Err(SemanticError::new(
                        Some(span),
                        "New is for class variables not struct type variables",
                    ));
                }
                let cinst = tt
                    .get(lexer.span_str(cspan))
                    .filter(|inst| matches!(inst, UserDefType::Class { .. }))
                    .ok_or(SemanticError::new(Some(cspan), "This class was not found"))?;

                if !is_descendent(tt, var.get_type(), &cinst.to_type()) {
                    return Err(SemanticError::new(
                        Some(span),
                        &format!(
                            "Type {:?} is not descendant of type of this varaible ({:?})",
                            cinst.to_type(),
                            var.get_type(),
                        ),
                    ));
                }
                cinst.get_idx().unwrap()
            }
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
    };
    var.set_ref(RefType::LHS).unwrap();
    Ok(Tnode::New {
        span,
        c_idx,
        var: Box::new(var),
    })
}

pub fn free_memory(span: Span, var: Tnode, is_delete: bool) -> Result<Tnode, SemanticError> {
    match &var {
        Tnode::Var { dtype, .. } | Tnode::DeRefOperator { dtype, .. } => match dtype {
            Type::UserDef { size, .. } => {
                if (is_delete && *size == 1) || (!is_delete && *size == 2) {
                    let error_message = if is_delete {
                        "Delete is just for variables of class type"
                    } else {
                        "Free is just for variables of struct type"
                    };
                    return Err(SemanticError::new(Some(span), error_message));
                }
            }
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
    let node = if is_delete {
        Tnode::Delete {
            span,
            var: Box::new(var),
        }
    } else {
        Tnode::Free {
            span,
            var: Box::new(var),
        }
    };
    Ok(node)
}

pub fn create_exposcall(
    span: Span,
    fn_code: Box<Tnode>,
    args: Vec<Tnode>,
) -> Result<Tnode, SemanticError> {
    if fn_code.get_type() != &Type::Str {
        return Err(SemanticError::new(
            Some(span),
            &format!(
                "Function code of system call should have type string, found {:?}",
                fn_code.get_type()
            ),
        ));
    }

    Ok(Tnode::SysCall {
        span,
        fn_code,
        args,
    })
}
