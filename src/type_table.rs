#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    Void,
    Int,
    Bool,
    Str,
    IntPtr,
    StrPtr,
}

impl Type {
    pub fn rref(&self) -> Result<Self, &'static str> {
        match self {
            Type::Int => Ok(Type::IntPtr),
            Type::Str => Ok(Type::StrPtr),
            _ => Err("Referencing not defined for this variable type"),
        }
    }
    pub fn deref(&self) -> Result<Self, &'static str> {
        match self {
            Type::IntPtr => Ok(Type::Int),
            Type::StrPtr => Ok(Type::Str),
            _ => Err("Dereferencing not defined for this variable type"),
        }
    }
}

pub struct TypeBuilder {
    is_pointer: bool,
    dtype: Option<Type>,
}

impl TypeBuilder {
    pub fn new() -> Self {
        TypeBuilder {
            is_pointer: false,
            dtype: None,
        }
    }

    pub fn set_pointer(&mut self, is_pointer: bool) -> &mut Self {
        self.is_pointer = is_pointer;
        self
    }

    pub fn dtype(&mut self, inner_type: Type) -> &mut Self {
        self.dtype = Some(inner_type);
        self
    }

    pub fn build(self) -> Result<Type, &'static str> {
        if self.is_pointer {
            match self.dtype {
                Some(Type::Int) => Ok(Type::IntPtr),
                Some(Type::Str) => Ok(Type::StrPtr),
                _ => Err("Pointer type only defined for int and string"),
            }
        } else {
            match self.dtype {
                Some(Type::Int) => Ok(Type::Int),
                Some(Type::Str) => Ok(Type::Str),
                Some(_) => Err("Variable can be only of int or string type"),
                _ => Err("Inner type must be defined"),
            }
        }
    }
}
