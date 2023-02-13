use std::collections::{HashMap, LinkedList};
use std::default::Default;

use crate::symbol::{Symbol, SymbolTable};
use crate::type_table::{PrimitiveType, Type};
use crate::utils::label::LabelGenerator;

pub struct ParserState {
    gst: SymbolTable,
    lst: SymbolTable,
    label: LabelGenerator,
    tt: HashMap<String, Type>,
    cfn: Option<Symbol>,
}

impl Default for ParserState {
    fn default() -> Self {
        let mut tt = HashMap::new();
        tt.insert("int".to_string(), Type::Primitive(PrimitiveType::Int));
        tt.insert("str".to_string(), Type::Primitive(PrimitiveType::Str));
        tt.insert("bool".to_string(), Type::Primitive(PrimitiveType::Bool));
        tt.insert("Void".to_string(), Type::Primitive(PrimitiveType::Void));
        ParserState {
            gst: SymbolTable::default(),
            lst: SymbolTable::default(),
            tt,
            label: LabelGenerator::default(),
            cfn: None,
        }
    }
}

impl ParserState {
    pub fn gst(&mut self) -> &mut SymbolTable {
        &mut self.gst
    }

    pub fn lst(&mut self) -> &mut SymbolTable {
        &mut self.lst
    }

    pub fn flabel(&mut self) -> &mut LabelGenerator {
        &mut self.label
    }

    pub fn cfn_rtype(&self) -> Type {
        match self.cfn {
            Some(ref s) => s.get_type(),
            None => Type::Primitive(PrimitiveType::Int),
        }
    }

    pub fn cfn_params(&self) -> LinkedList<(Type, String)> {
        match self.cfn {
            Some(ref s) => s.get_params().unwrap(),
            None => LinkedList::new(),
        }
    }

    pub fn cfn(&self) -> Option<&Symbol> {
        self.cfn.as_ref()
    }

    pub fn update_state<'t>(&mut self, fname: &'t str) -> Result<&'t str, &'static str> {
        self.lst = SymbolTable::default();
        self.cfn = match fname {
            "" => None,
            _ => Some(
                self.gst
                    .get(fname)
                    .filter(|s| matches!(s, Symbol::Function { .. }))
                    .ok_or("Function was not defined")?
                    .clone(),
            ),
        };
        Ok(fname)
    }

    pub fn get_var(&self, name: &str) -> Result<Symbol, &'static str> {
        self.lst
            .get(name)
            .or(self
                .gst
                .get(name)
                .filter(|s| !matches!(s, Symbol::Function { .. })))
            .cloned()
            .ok_or("Variable was not declared")
    }
}
