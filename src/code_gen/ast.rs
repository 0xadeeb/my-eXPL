use lrpar::Span;

#[derive(Debug)]
pub enum Tnode {
    Constant {
        span: Span,
    },
    Operator {
        op: Op,
        span: Span,
        left: Box<Tnode>,
        right: Box<Tnode>,
    },
    Var {
        span: Span,
        name: String,
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
        span: Span,
        left: Box<Tnode>,
        right: Box<Tnode>,
    },
    Empty,
}

#[derive(Debug)]
pub enum Op {
    Add,
    Sub,
    Mult,
    Div,
    Eq,
}

impl Tnode {
    pub fn get_address(&self) -> Result<u32, ()> {
        match self {
            Tnode::Var { span: _, ref name } => Ok(name.as_bytes()[0] as u32 + 4096 - 97),
            _ => Err(()),
        }
    }
}
