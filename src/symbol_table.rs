use crate::ast::*;
use std::collections::HashMap;

pub struct SymbolTable {
    table: HashMap<String, Symbol>,
    size: usize,
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self {
            table: HashMap::new(),
            size: 0,
        }
    }
}

impl SymbolTable {
    pub fn get_size(&self) -> usize {
        self.size
    }
    pub fn get(&self, name: &str) -> Option<&Symbol> {
        self.table.get(name)
    }
    pub fn insert(&mut self, name: String, dtype: Type, dim: &Vec<u32>) -> Result<(), ()> {
        if self.table.contains_key(&name) {
            return Err(());
        }
        match dim.len() {
            0 => self.table.insert(
                name.clone(),
                Symbol::Variable {
                    name,
                    binding: 4096 + self.size as u16,
                    dtype,
                },
            ),
            _ => self.table.insert(
                name.clone(),
                Symbol::Arr {
                    name,
                    binding: 4096 + self.size as u16,
                    dim: dim.iter().map(|num| *num as usize).collect(),
                    dtype,
                },
            ),
        };
        self.size += dim.iter().fold(1, |acc, &x| acc * x) as usize;
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub enum Symbol {
    Variable {
        name: String,
        binding: u16,
        dtype: Type,
    },
    Arr {
        name: String,
        binding: u16,
        dim: Vec<usize>,
        dtype: Type,
    },
}

impl Symbol {
    pub fn get_type(&self) -> Type {
        match self {
            Self::Variable { dtype, .. } | Self::Arr { dtype, .. } => dtype.clone(),
        }
    }

    pub fn base_address(&self) -> u16 {
        match self {
            Self::Variable { binding, .. } | Self::Arr { binding, .. } => binding.clone(),
        }
    }
}
