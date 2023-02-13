use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrimitiveType {
    Void,
    Int,
    Bool,
    Str,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Primitive(PrimitiveType),
    UserDef {
        size: i16,
        field_list: HashMap<String, Box<Type>>, // map(variable name -> type)
    },
    Pointer(Box<Type>),
}

impl Type {
    pub fn deref(&self) -> Result<Self, &'static str> {
        match self {
            Self::Pointer(t) => Ok(*t.clone()),
            _ => Err("Referencing not defined for this variable type"),
        }
    }
    pub fn rref(&self) -> Result<Self, &'static str> {
        Ok(Type::Pointer(Box::new(self.clone())))
    }
}

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
