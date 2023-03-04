use std::{
    collections::{HashMap, LinkedList},
    sync::{Arc, Weak},
};

use lrlex::DefaultLexeme;
use lrpar::{NonStreamingLexer, Span};

use crate::symbol::SymbolTable;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrimitiveType {
    Void,
    Int,
    Bool,
    Str,
    Null,
}

// This ds is used to store information about types
#[derive(Debug, Clone)]
pub enum Type {
    Primitive(PrimitiveType),
    Struct {
        name: String,
        size: u16,
        field_list: HashMap<String, (u8, String)>, // map(variable name -> (field idx, type name))
    },
    Class {
        name: String,
        cst: SymbolTable,
        idx: u8,
        size: u16,
        parent: Option<Weak<Type>>,
        fn_list: Vec<u8>,
    },
    Pointer(Box<Type>),
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Primitive(p1), Self::Primitive(p2)) => p1 == p2,
            (Self::Struct { name: n1, .. }, Self::Struct { name: n2, .. }) => n1 == n2,
            (Self::Class { name: n1, .. }, Self::Class { name: n2, .. }) => n1 == n2,
            (Self::Pointer(t1), Self::Pointer(t2)) => t1 == t2,
            _ => false,
        }
    }
}

impl Eq for Type {}

impl Type {
    pub fn deref(&self) -> Result<Self, &'static str> {
        match self {
            Self::Pointer(t) => Ok(*t.clone()),
            _ => Err("Dereferencing not defined for this variable type"),
        }
    }
    pub fn rref(&self) -> Result<Self, &'static str> {
        Ok(Type::Pointer(Box::new(self.clone())))
    }

    pub fn get_size(&self) -> u16 {
        match self {
            Self::Struct { size, .. } => *size,
            _ => 1,
        }
    }

    pub fn field_list(&self) -> Result<&HashMap<String, (u8, String)>, &'static str> {
        match self {
            Self::Struct { field_list, .. } => Ok(field_list),
            _ => Err("No field list for this type"),
        }
    }
}

// At times data about the whole type will only be parsered in mutpile rule
// for example to parse a type ***int, it should recccursivly parser "*"s
// and the type name "int", so I thought to use a type builder to build the type incrementally
pub struct TypeBuilder {
    pointer: u16,
    dtype: Option<Type>,
}

impl TypeBuilder {
    pub fn new() -> Self {
        TypeBuilder {
            pointer: 0,
            dtype: None,
        }
    }

    pub fn set_pointer(&mut self) -> &mut Self {
        self.pointer += 1;
        self
    }

    pub fn dtype(&mut self, inner_type: Type) -> &mut Self {
        self.dtype = Some(inner_type);
        self
    }

    pub fn build(self) -> Result<Type, &'static str> {
        match self.dtype {
            Some(t) => Ok((0..self.pointer)
                .into_iter()
                .fold(t, |acc, _| acc.rref().unwrap())),
            None => Err("Inner type was not set!"),
        }
    }
}

// This DS should trivial
pub struct TypeTable {
    table: HashMap<String, Arc<Type>>,
}

impl Default for TypeTable {
    fn default() -> Self {
        let mut table = HashMap::new();
        table.insert(
            "int".to_string(),
            Arc::new(Type::Primitive(PrimitiveType::Int)),
        );
        table.insert(
            "str".to_string(),
            Arc::new(Type::Primitive(PrimitiveType::Str)),
        );
        Self { table }
    }
}

impl TypeTable {
    pub fn get(&self, tname: &str) -> Option<Type> {
        self.table.get(tname).map(|ptr| ptr.as_ref().clone())
    }

    pub fn get_pointer(&self, tname: &str) -> Option<Arc<Type>> {
        self.table.get(tname).cloned()
    }

    pub fn insert_struct(
        &mut self,
        lexer: &dyn NonStreamingLexer<DefaultLexeme, u32>,
        tspan: Span,
        fspan_list: LinkedList<(Span, Span)>,
    ) -> Result<(), (Option<Span>, &'static str)> {
        let tname = lexer.span_str(tspan);
        if self.table.contains_key(tname) {
            return Err((Some(tspan), "Type declared multiple times"));
        }
        if fspan_list.len() > 8 {
            return Err((Some(tspan), "This type have more than 8 fields"));
        }
        let mut field_list = HashMap::new();
        self.table.insert(
            tname.to_string(),
            Arc::new(Type::Primitive(PrimitiveType::Void)),
        );
        for (i, (ftype_span, fname_span)) in fspan_list.iter().enumerate() {
            let ftype = lexer.span_str(*ftype_span);
            let fname = lexer.span_str(*fname_span);
            if self.table.contains_key(ftype) {
                if !field_list.contains_key(fname) {
                    field_list.insert(fname.to_string(), (i as u8 + 1, ftype.to_string()));
                } else {
                    return Err((Some(*fname_span), "This field is defined multiple times"));
                }
            } else {
                return Err((Some(*ftype_span), "This type is not defined"));
            }
        }
        self.table.insert(
            tname.to_string(),
            Arc::new(Type::Struct {
                name: tname.to_string(),
                size: fspan_list.len() as u16,
                field_list,
            }),
        );
        Ok(())
    }
}
