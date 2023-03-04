use lrlex::DefaultLexeme;
use lrpar::{NonStreamingLexer, Span};

use crate::{frontend::PARSER, type_table::*};
use std::collections::{HashMap, LinkedList};

// This DS should trivial
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SymbolTable {
    table: HashMap<String, Symbol>,
    size: i16,
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
    pub fn get_size(&self) -> i16 {
        self.size
    }
    pub fn get(&self, name: &str) -> Option<&Symbol> {
        self.table.get(name)
    }
    pub fn insert_builder(
        &mut self,
        mut s: SymbolBuilder,
        base: i16,
        lexer: &dyn NonStreamingLexer<DefaultLexeme, u32>,
    ) -> Result<(), &'static str> {
        let name = lexer.span_str(s.get_name());
        if self.table.contains_key(name) {
            return Err("Variable declared multiple times");
        }
        if !s.is_func() {
            s.binding(base + self.size);
            self.size += s.get_dim().iter().fold(1, |acc, &x| acc * x);
        }
        self.table.insert(name.to_string(), s.build(lexer).unwrap());
        Ok(())
    }

    pub fn insert_arg(&mut self, s: Symbol) -> Result<(), &'static str> {
        if self.table.contains_key(s.get_name()) {
            return Err("Multiple variables with same name in scope of this function");
        }
        self.table.insert(s.get_name().to_string(), s);
        Ok(())
    }
}

// This stored data about each symbol
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Symbol {
    Variable {
        name: String,
        binding: i16,
        dtype: Type,
        is_static: bool,
    },
    Array {
        name: String,
        binding: i16,
        dim: Vec<i16>,
        dtype: Type,
    },
    Function {
        name: String,
        label: i16,
        ret_type: Type,
        params: LinkedList<(Type, String)>,
    },
}

impl Symbol {
    pub fn get_name(&self) -> &str {
        match self {
            Self::Variable { name, .. }
            | Self::Array { name, .. }
            | Self::Function { name, .. } => name,
        }
    }

    pub fn get_type(&self) -> Type {
        match self {
            Self::Variable { dtype, .. }
            | Self::Array { dtype, .. }
            | Self::Function {
                ret_type: dtype, ..  // Ideally should have another method, but does it really matter?
            } => dtype.clone(),
        }
    }

    pub fn get_address(&self) -> i16 {
        match self {
            Self::Variable { binding, .. } | Self::Array { binding, .. } => binding.clone(),
            Self::Function { label, .. } => label.clone() as i16, // Ideally should have another method, but does it really matter?
        }
    }

    pub fn get_dim(&self) -> i16 {
        match self {
            Self::Array { dim, .. } => dim.len() as i16,
            _ => 0,
        }
    }

    pub fn get_params(&self) -> Result<LinkedList<(Type, String)>, &'static str> {
        match self {
            Self::Function { params, .. } => Ok(params.clone()),
            _ => Err("The symbol was not of a function"),
        }
    }

    pub fn is_local(&self) -> bool {
        match self {
            Self::Variable { is_static, .. } => !is_static,
            _ => false,
        }
    }
}

// At times data about the whole symbol will only be parsered in mutpile rule
// for example to parse the symbol "bar" in
// int foo, **bar
// "*'s are parser in a rule then the name of the symbol in another rule and then the type
// "int" will be only parser at last, and the type name "int",
// so I thought to use a type builder to build the type incrementally
pub struct SymbolBuilder {
    name: Span,
    binding: Option<i16>,
    is_static: bool,
    dtype: TypeBuilder,
    dim: Option<Vec<i16>>,
    label: Option<i16>,
    params: Option<LinkedList<(Type, String)>>,
}

impl SymbolBuilder {
    pub fn new(name: Span, is_static: bool) -> SymbolBuilder {
        SymbolBuilder {
            name,
            binding: None,
            is_static,
            dtype: TypeBuilder::new(),
            dim: None,
            label: None,
            params: None,
        }
    }

    pub fn get_name(&self) -> Span {
        self.name
    }

    pub fn get_dim(&self) -> Vec<i16> {
        match &self.dim {
            Some(d) => d.clone(),
            None => Vec::new(),
        }
    }

    pub fn is_func(&self) -> bool {
        !matches!(self.params, None)
    }

    pub fn dim(&mut self, dim: Vec<i16>) -> &mut Self {
        self.dim = Some(dim);
        self
    }

    pub fn ptr(&mut self) -> &mut SymbolBuilder {
        self.dtype.set_pointer();
        self
    }

    pub fn params(&mut self, params: LinkedList<(Type, String)>) -> &mut SymbolBuilder {
        self.params = Some(params);
        self.label = Some(PARSER.lock().unwrap().flabel().get());
        self
    }

    pub fn dtype(&mut self, inner_type: Type) -> &mut SymbolBuilder {
        self.dtype.dtype(inner_type);
        self
    }

    pub fn binding(&mut self, binding: i16) -> &mut SymbolBuilder {
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
                    binding: binding as i16,
                    dtype: self.dtype.build()?,
                    is_static: self.is_static,
                }),
                Some(dim) => Ok(Symbol::Array {
                    name: lexer.span_str(self.name).to_string(),
                    binding,
                    dim,
                    dtype: self.dtype.build()?,
                }),
            }
        } else if let Some(params) = self.params {
            Ok(Symbol::Function {
                name: lexer.span_str(self.name).to_string(),
                label: self.label.unwrap(),
                ret_type: self.dtype.build()?,
                params,
            })
        } else {
            Err("Couldn't create symbol from builder")
        }
    }
}
