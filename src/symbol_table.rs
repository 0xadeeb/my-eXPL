use crate::ast::*;

#[derive(Debug, Clone)]
pub enum Gsymbol {
    Variable {
        name: String,
        binding: u16,
        size: usize,
        ttype: Type,
    },
}

impl Gsymbol {
    pub fn get_type(&self) -> Type {
        match self {
            Self::Variable { ttype, .. } => ttype.clone(),
        }
    }

    pub fn get_address(&self) -> u16 {
        match self {
            Self::Variable { binding, .. } => binding.clone(),
        }
    }
}
