use lrlex::DefaultLexeme;
use lrpar::{NonStreamingLexer, Span};

use crate::{type_table::*, utils::label::LabelGenerator};
use std::collections::{HashMap, LinkedList};

// This DS should trivial
#[derive(Debug, Clone)]
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
    pub fn get_size(&self) -> &u16 {
        &self.size
    }

    pub fn get(&self, name: &str) -> Option<&Symbol> {
        self.table.get(name)
    }

    pub fn insert_builder(
        &mut self,
        mut s: SymbolBuilder,
        base: i16,
        lexer: &dyn NonStreamingLexer<DefaultLexeme, u32>,
    ) -> Result<(), String> {
        let name = lexer.span_str(s.get_name());
        if self.table.contains_key(name) {
            return Err(format!("Variable declared multiple times"));
        }
        if !s.is_func() {
            s.binding(base + self.size as i16);
            self.size += s.get_size();
        }
        self.table.insert(name.to_string(), s.build(lexer).unwrap());
        Ok(())
    }

    pub fn insert_symbol(&mut self, s: Symbol, check: bool, is_arg: bool) -> Result<(), String> {
        if check && self.table.contains_key(s.get_name()) {
            return Err(format!("Multiple variables with same name defined"));
        }
        if !is_arg {
            self.size += s.get_size();
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
    Function {
        name: String,
        label: u8,
        idx: Option<u8>,
        ret_type: Type,
        params: LinkedList<(Type, String)>,
    },
}

impl Symbol {
    pub fn get_name(&self) -> &str {
        match self {
            Self::Variable { name, .. } | Self::Function { name, .. } => name,
        }
    }

    pub fn get_type(&self) -> &Type {
        match self {
            Self::Variable { dtype, .. }
            | Self::Function {
                ret_type: dtype, ..  // Ideally should have another method, but does it really matter?
            } => dtype,
        }
    }

    pub fn get_size(&self) -> u16 {
        match self {
            Self::Variable { dtype, .. } => dtype.get_size(),
            Self::Function { .. } => 0,
        }
    }

    pub fn get_dim(&self) -> Result<u8, String> {
        match self {
            Self::Variable { dtype, .. } => Ok(dtype.get_dim()),
            _ => Err(format!("No dimention for functions")),
        }
    }

    pub fn get_binding(&self) -> Result<&i16, String> {
        match self {
            Self::Variable { binding, .. } => Ok(binding),
            _ => Err(format!("This symbol is bound to an address")),
        }
    }

    pub fn get_label(&self) -> Result<&u8, String> {
        match self {
            Self::Function { label, .. } => Ok(label),
            Self::Variable { .. } => Err(format!("This symbol is has no label")),
        }
    }

    pub fn get_idx(&self) -> Result<u8, String> {
        match self {
            Self::Function { idx, .. } => idx.ok_or("This function has no index".to_owned()),
            Self::Variable { binding, .. } => Ok(*binding as u8),
        }
    }

    pub fn get_params(&self) -> Result<&LinkedList<(Type, String)>, String> {
        match self {
            Self::Function { params, .. } => Ok(params),
            _ => Err(format!("The symbol was not of a function")),
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
    label: Option<u8>,
    idx: Option<u8>,
    params: Option<LinkedList<(Type, String)>>,
}

impl SymbolBuilder {
    pub fn new(name: Span, is_static: bool) -> SymbolBuilder {
        SymbolBuilder {
            name,
            binding: None,
            is_static,
            dtype: TypeBuilder::new(),
            idx: None,
            label: None,
            params: None,
        }
    }

    pub fn get_name(&self) -> Span {
        self.name
    }

    pub fn get_size(&self) -> u16 {
        self.dtype.get_size()
    }

    pub fn is_func(&self) -> bool {
        !matches!(self.params, None)
    }

    pub fn dim(&mut self, dim: Vec<u8>) -> &mut Self {
        self.dtype.dim(dim);
        self
    }

    pub fn ptr(&mut self) -> &mut SymbolBuilder {
        self.dtype.set_pointer();
        self
    }

    pub fn params(
        &mut self,
        params: LinkedList<(Type, Span)>,
        flabel: &mut LabelGenerator,
        lexer: &dyn NonStreamingLexer<DefaultLexeme, u32>,
    ) -> &mut SymbolBuilder {
        self.params = Some(
            params
                .into_iter()
                .map(|(dtype, span)| (dtype, lexer.span_str(span).to_owned()))
                .collect(),
        );
        self.label = Some(flabel.get() as u8);
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
    ) -> Result<Symbol, String> {
        if let Some(binding) = self.binding {
            Ok(Symbol::Variable {
                name: lexer.span_str(self.name).to_string(),
                binding: binding as i16,
                dtype: self.dtype.build()?,
                is_static: self.is_static,
            })
        } else if let Some(params) = self.params {
            Ok(Symbol::Function {
                name: lexer.span_str(self.name).to_string(),
                label: self.label.unwrap(),
                ret_type: self.dtype.build()?,
                idx: self.idx,
                params,
            })
        } else {
            Err(format!("Couldn't create symbol from builder"))
        }
    }
}
