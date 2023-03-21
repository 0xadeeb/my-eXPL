use std::collections::{HashMap, HashSet, LinkedList};

use lrlex::DefaultLexeme;
use lrpar::{NonStreamingLexer, Span};

use crate::{
    error::SemanticError,
    symbol::{Symbol, SymbolTable},
    utils::label::LabelGenerator,
};

#[derive(Debug, Clone)]
pub enum UserDefType {
    Struct {
        name: String,
        sst: SymbolTable, // struct symbol table
    },
    Class {
        name: String,
        cst: SymbolTable, // class symbol table
        idx: u8,
        parent: Option<String>,
        fn_list: Vec<u8>,
    },
}

impl UserDefType {
    pub fn get_name(&self) -> &str {
        match self {
            Self::Struct { name, .. } | Self::Class { name, .. } => name,
        }
    }

    pub fn get_parent(&self) -> Result<&Option<String>, ()> {
        match self {
            Self::Class { parent, .. } => Ok(parent),
            Self::Struct { .. } => Err(()),
        }
    }

    pub fn get_fns(&self) -> Result<&Vec<u8>, String> {
        match self {
            Self::Class { fn_list, .. } => Ok(fn_list),
            _ => Err("Struct has no methods".to_owned()),
        }
    }

    pub fn to_type(&self) -> Type {
        match self {
            Self::Struct { name, .. } => Type::UserDef {
                name: name.to_owned(),
                size: 1,
            },
            Self::Class { name, .. } => Type::UserDef {
                name: name.to_owned(),
                size: 2,
            },
        }
    }

    pub fn get_ptr_size(&self) -> &u16 {
        match self {
            Self::Struct { sst, .. } => sst.get_size(),
            Self::Class { cst, .. } => cst.get_size(),
        }
    }

    pub fn get_st(&self) -> &SymbolTable {
        match self {
            Self::Struct { sst, .. } => sst,
            Self::Class { cst, .. } => cst,
        }
    }

    pub fn get_st_mut(&mut self) -> &mut SymbolTable {
        match self {
            Self::Struct { sst, .. } => sst,
            Self::Class { cst, .. } => cst,
        }
    }

    pub fn get_parent_st(&self, tt: &TypeTable) -> Result<SymbolTable, String> {
        match self {
            Self::Class { parent, .. } => match parent {
                Some(p) => Ok(tt.get(p).unwrap().get_st().clone()),
                None => Ok(SymbolTable::default()),
            },
            _ => Err("No parent for structure".to_owned()),
        }
    }

    pub fn set_id(&mut self, id: u8) {
        match self {
            Self::Class { idx, .. } => *idx = id,
            _ => {}
        }
    }
}

// This ds is used to store information about types
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Void,
    Int,
    Bool,
    Str,
    Null,
    UserDef { name: String, size: u16 },
    Pointer(Box<Type>),
    Array { dtype: Box<Type>, dim: Vec<u8> },
}

impl Type {
    pub fn deref(&self) -> Result<Self, String> {
        match self {
            Self::Pointer(t) => Ok(*t.clone()),
            _ => Err(format!(
                "Dereferencing not defined for this variable type \"{:?}\"",
                self
            )),
        }
    }

    pub fn rref(&self) -> Result<Self, String> {
        Ok(Type::Pointer(Box::new(self.clone())))
    }

    pub fn get_name(&self) -> Result<&str, String> {
        match self {
            Type::UserDef { name, .. } => Ok(name),
            _ => Err(format!("No name for symbol of type \"{:?}\"", self)),
        }
    }

    pub fn get_size(&self) -> u16 {
        match self {
            Self::UserDef { size, .. } => *size,
            Self::Array { dtype, dim } => {
                let acc = dtype.get_size();
                dim.clone().into_iter().fold(acc, |acc, d| acc * (d as u16))
            }
            _ => 1,
        }
    }

    pub fn get_dim(&self) -> u8 {
        match self {
            Self::Array { dim, .. } => dim.len() as u8,
            _ => 0,
        }
    }

    pub fn symbol_list<'t>(&self, tt: &'t TypeTable) -> Result<&'t SymbolTable, String> {
        match self {
            Self::UserDef { name, .. } => Ok(tt.get(name).unwrap().get_st()),
            _ => Err(format!("No symbol table for symbol of type \"{:?}\"", self)),
        }
    }
}

// At times data about the whole type will only be parsered in mutpile rule
// for example to parse a type ***int, it should recccursivly parser "*"s
// and the type name "int", so I thought to use a type builder to build the type incrementally
pub struct TypeBuilder {
    pointer: u16,
    dim: Option<Vec<u8>>,
    dtype: Option<Type>,
}

impl TypeBuilder {
    pub fn new() -> Self {
        TypeBuilder {
            pointer: 0,
            dim: None,
            dtype: None,
        }
    }

    pub fn get_size(&self) -> u16 {
        self.dtype.as_ref().unwrap().get_size()
    }

    pub fn set_pointer(&mut self) -> &mut Self {
        self.pointer += 1;
        self
    }

    pub fn dtype(&mut self, inner_type: Type) -> &mut Self {
        self.dtype = Some(inner_type);
        self
    }

    pub fn dim(&mut self, dim: Vec<u8>) -> &mut Self {
        self.dim = Some(dim);
        self
    }

    pub fn build(self) -> Result<Type, String> {
        match self.dtype {
            Some(t) => {
                let new_type = (0..self.pointer)
                    .into_iter()
                    .fold(t, |acc, _| acc.rref().unwrap());
                match self.dim {
                    Some(d) => Ok(Type::Array {
                        dtype: Box::new(new_type),
                        dim: d,
                    }),
                    None => Ok(new_type),
                }
            }
            None => Err("Inner type was not set!".to_owned()),
        }
    }
}

// This DS should trivial
pub struct TypeTable {
    table: HashMap<String, UserDefType>,
}

impl Default for TypeTable {
    fn default() -> Self {
        Self {
            table: HashMap::new(),
        }
    }
}

impl TypeTable {
    pub fn get(&self, tname: &str) -> Option<&UserDefType> {
        self.table.get(tname)
    }

    pub fn new_struct(
        &mut self,
        lexer: &dyn NonStreamingLexer<DefaultLexeme, u32>,
        tspan: Span,
    ) -> Result<Type, SemanticError> {
        let tname = lexer.span_str(tspan);
        if self.table.contains_key(tname) {
            return Err(SemanticError::new(
                Some(tspan),
                "Type declared multiple times",
            ));
        }

        self.table.insert(
            tname.to_string(),
            UserDefType::Struct {
                name: tname.to_string(),
                sst: SymbolTable::default(),
            },
        );
        Ok(Type::UserDef {
            name: tname.to_owned(),
            size: 1,
        })
    }

    pub fn set_cidx(&mut self, cname: &str, idx: u8) {
        self.table.get_mut(cname).unwrap().set_id(idx)
    }

    pub fn insert_struct(
        &mut self,
        lexer: &dyn NonStreamingLexer<DefaultLexeme, u32>,
        span: Span,
        dtype: Type,
        field_list: LinkedList<(Type, Span)>, // list of type, name of the fields
    ) -> Result<(), SemanticError> {
        let tname = dtype.get_name().unwrap();
        let sst = self.table.get_mut(tname).unwrap().get_st_mut();
        for (i, (ftype, fname_span)) in field_list.iter().enumerate() {
            let fname = lexer.span_str(*fname_span);
            sst.insert_symbol(
                Symbol::Variable {
                    name: fname.to_string(),
                    binding: i as i16,
                    dtype: ftype.clone(),
                    is_static: false,
                },
                true,
            )
            .map_err(|msg| SemanticError::new(Some(*fname_span), &msg))?;
        }
        if sst.get_size().to_owned() > 8 {
            return Err(SemanticError::new(
                Some(span),
                "This type takes up more than 8 words",
            ));
        }
        Ok(())
    }

    pub fn set_class(
        &mut self,
        lexer: &dyn NonStreamingLexer<DefaultLexeme, u32>,
        class: Span,
        pspan: Option<Span>,
    ) -> Result<Type, SemanticError> {
        let cname = lexer.span_str(class);
        if self.table.contains_key(cname) {
            return Err(SemanticError::new(
                Some(class),
                "Type with same name previously defined",
            ));
        }

        let parent = match pspan {
            Some(p) => self
                .table
                .get(lexer.span_str(p))
                .filter(|t| matches!(t, UserDefType::Class { .. }))
                .ok_or(SemanticError::new(Some(p), "Parent class not defined"))?
                .into(),
            None => None,
        };

        self.table.insert(
            cname.to_owned(),
            UserDefType::Class {
                name: cname.to_owned(),
                cst: parent.map(|p| p.get_st().clone()).unwrap_or_default(),
                idx: 0,
                parent: parent.map(|p| p.get_name().to_owned()),
                fn_list: parent
                    .map(|p| p.get_fns().unwrap().clone())
                    .unwrap_or_default(),
            },
        );

        Ok(Type::UserDef {
            name: cname.to_owned(),
            size: 2,
        })
    }

    pub fn insert_cst(
        &mut self,
        span: Span,
        cname: &str,
        lexer: &dyn NonStreamingLexer<DefaultLexeme, u32>,
        field_list: LinkedList<(Type, Span)>,
        method_list: LinkedList<(Type, Span, LinkedList<(Type, Span)>)>,
        flabel: &mut LabelGenerator,
    ) -> Result<Vec<u8>, SemanticError> {
        let entry = self.table.get(cname).unwrap();
        let mut cst = entry.get_st().clone();
        let mut fn_list = entry.get_fns().unwrap().clone();

        for (i, (ftype, fname_span)) in field_list.iter().enumerate() {
            let fname = lexer.span_str(*fname_span);
            cst.insert_symbol(
                Symbol::Variable {
                    name: fname.to_string(),
                    binding: i as i16,
                    dtype: ftype.clone(),
                    is_static: false,
                },
                true,
            )
            .map_err(|msg| SemanticError::new(Some(*fname_span), &msg))?;
        }

        let parent_st = entry.get_parent_st(&self).unwrap();
        let mut method_set = HashSet::new();

        for (rtype, name_span, param_list) in method_list.iter() {
            let name = lexer.span_str(*name_span);
            if method_set.contains(name) {
                return Err(SemanticError::new(
                    Some(*name_span),
                    "This method is defined multiple times in this class",
                ));
            }
            let label = flabel.get();
            match parent_st.get(name) {
                Some(method) => {
                    let idx = method.get_idx().unwrap();
                    fn_list[idx as usize] = label as u8;
                }
                None => {
                    fn_list.push(label as u8);
                    cst.insert_symbol(
                        Symbol::Function {
                            name: name.to_owned(),
                            label: label as u8,
                            idx: Some(fn_list.len() as u8 - 1),
                            ret_type: rtype.clone(),
                            params: param_list
                                .iter()
                                .map(|(dtype, pname)| {
                                    (dtype.clone(), lexer.span_str(*pname).to_string())
                                })
                                .collect(),
                        },
                        false,
                    )
                    .map_err(|msg| SemanticError::new(Some(*name_span), &msg))?;
                }
            }
            method_set.insert(name.to_owned());
        }

        if cst.get_size().to_owned() > 8 {
            return Err(SemanticError::new(
                Some(span),
                "This type takes up more than 8 words",
            ));
        }
        if fn_list.len() > 8 {
            return Err(SemanticError::new(
                Some(span),
                "This type has more than 8 methods",
            ));
        }
        self.table.insert(
            cname.to_owned(),
            UserDefType::Class {
                name: cname.to_owned(),
                cst,
                idx: 0,
                parent: entry.get_parent().unwrap().clone(),
                fn_list: fn_list.clone(),
            },
        );
        Ok(fn_list.clone())
    }
}
