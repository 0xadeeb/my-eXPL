use lrlex::DefaultLexeme;
use lrpar::{NonStreamingLexer, Span};

use crate::ast::*;
use std::collections::HashMap;

pub struct SymbolTable {
    table: HashMap<String, Symbol>,
    size: u16,
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
    pub fn get_size(&self) -> u16 {
        self.size
    }
    pub fn get(&self, name: &str) -> Option<&Symbol> {
        self.table.get(name)
    }
    pub fn insert(
        &mut self,
        mut s: SymbolBuilder,
        lexer: &dyn NonStreamingLexer<DefaultLexeme, u32>,
    ) -> Result<(), ()> {
        let name = lexer.span_str(s.get_name());
        if self.table.contains_key(name) {
            return Err(());
        }
        s.binding(4096 + self.size);
        self.size += s.get_dim().iter().fold(1, |acc, &x| acc * x);
        self.table.insert(name.to_string(), s.build(lexer).unwrap());
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
    Array {
        name: String,
        binding: u16,
        dim: Vec<u16>,
        dtype: Type,
    },
}

impl Symbol {
    pub fn get_type(&self) -> Type {
        match self {
            Self::Variable { dtype, .. } | Self::Array { dtype, .. } => dtype.clone(),
        }
    }

    pub fn base_address(&self) -> u16 {
        match self {
            Self::Variable { binding, .. } | Self::Array { binding, .. } => binding.clone(),
        }
    }

    pub fn get_dim(&self) -> u16 {
        match self {
            Self::Array { dim, .. } => dim.len() as u16,
            _ => 0,
        }
    }
}

pub struct SymbolBuilder {
    name: Span,
    binding: Option<u16>,
    dtype: TypeBuilder,
    dim: Option<Vec<u16>>,
}

impl SymbolBuilder {
    pub fn new(name: Span) -> SymbolBuilder {
        SymbolBuilder {
            name,
            binding: None,
            dtype: TypeBuilder::new(),
            dim: None,
        }
    }

    pub fn get_name(&self) -> Span {
        self.name
    }

    pub fn get_dim(&self) -> Vec<u16> {
        match &self.dim {
            Some(d) => d.clone(),
            None => Vec::new(),
        }
    }

    pub fn dim(&mut self, dim: Vec<u16>) -> &mut Self {
        self.dim = Some(dim);
        self
    }

    pub fn ptr(&mut self, ptr: bool) -> &mut SymbolBuilder {
        self.dtype.set_pointer(ptr);
        self
    }

    pub fn dtype(&mut self, inner_type: Type) -> &mut SymbolBuilder {
        self.dtype.dtype(inner_type);
        self
    }

    pub fn binding(&mut self, binding: u16) -> &mut SymbolBuilder {
        self.binding = Some(binding);
        self
    }

    pub fn build(
        self,
        lexer: &dyn NonStreamingLexer<DefaultLexeme, u32>,
    ) -> Result<Symbol, &'static str> {
        if let Some(binding) = self.binding {
            match self.dim {
                None => Ok(Symbol::Variable {
                    name: lexer.span_str(self.name).to_string(),
                    binding,
                    dtype: self.dtype.build()?,
                }),
                Some(dim) => Ok(Symbol::Array {
                    name: lexer.span_str(self.name).to_string(),
                    binding,
                    dim,
                    dtype: self.dtype.build()?,
                }),
            }
        } else {
            Err("Binding couldn't be set for the symbol")
        }
    }
}
