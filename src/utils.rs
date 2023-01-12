use super::code_gen::*;
use lrpar::Span;
use std::error::Error;

#[derive(Debug)]
pub struct RegisterPool {
    available: [bool; 20],
}

impl Default for RegisterPool {
    fn default() -> Self {
        Self {
            available: [true; 20],
        }
    }
}

impl RegisterPool {
    pub fn get_reg(&mut self) -> Option<u8> {
        for (i, reg) in self.available.iter_mut().enumerate() {
            if *reg {
                *reg = false;
                return Some(i as u8);
            }
        }
        None
    }

    pub fn free_reg(&mut self, reg: u8) {
        if reg < 20 {
            self.available[reg as usize] = true;
        }
    }

    pub fn accquired_regs<'t>(&'t self) -> impl Iterator<Item = u8> + DoubleEndedIterator + 't {
        self.available
            .iter()
            .enumerate()
            .filter(|(_, &v)| !v)
            .map(|(i, _)| i as u8)
    }
}

#[derive(Debug)]
pub struct Label {
    counter: u16,
}
impl Label {
    pub fn default() -> Self {
        Self { counter: 0 }
    }
    pub fn get(&mut self) -> u16 {
        let label = self.counter;
        self.counter += 1;
        label
    }
}

pub fn err_from_str(e: &str) -> Box<dyn Error> {
    Box::<dyn Error>::from(e)
}

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
    span: Span,
    left: Tnode,
    right: Tnode,
) -> Result<Tnode, (Option<Span>, &'static str)> {
    match right.get_type() {
        Type::Int => {}
        _ => return Err((right.get_span(), "Type mismatch, excepted integer")),
    }
    Ok(Tnode::Asgn {
        span,
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
    span: Span,
    condition: Tnode,
    stmts: Tnode,
) -> Result<Tnode, (Option<Span>, &'static str)> {
    match condition.get_type() {
        Type::Bool => {}
        _ => return Err((condition.get_span(), "Type mismatch, excepted boolen")),
    }
    Ok(Tnode::While {
        span,
        condition: Box::new(condition),
        stmts: Box::new(stmts),
    })
}

pub fn create_if_node(
    span: Span,
    condition: Tnode,
    if_stmts: Tnode,
    else_stmts: Option<Tnode>,
) -> Result<Tnode, (Option<Span>, &'static str)> {
    match condition.get_type() {
        Type::Bool => {}
        _ => return Err((condition.get_span(), "Type mismatch, excepted boolen")),
    }
    Ok(Tnode::If {
        span,
        condition: Box::new(condition),
        if_stmt: Box::new(if_stmts),
        else_stmt: else_stmts.map(|val| Box::new(val)),
    })
}
