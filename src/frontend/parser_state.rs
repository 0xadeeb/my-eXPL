use std::collections::LinkedList;
use std::default::Default;
use std::sync::Arc;

use lrlex::DefaultLexeme;
use lrpar::{NonStreamingLexer, Span};

use crate::ast::FnAst;
use crate::symbol::{Symbol, SymbolTable};
use crate::type_table::{PrimitiveType, Type, TypeTable};
use crate::utils::label::LabelGenerator;

// This DS was basically made so that I don't have to keep track of multiple
// global variables and just one global variable which will return references
// of global symbol table, type table, label generator etc when a function needs to use it
// Ideally only some fields like local symbol table and current fn/class is required in this DS
pub struct ParserState {
    global_symbtab: SymbolTable,
    local_symbtab: SymbolTable,
    label: LabelGenerator, // Lable generator for a new function which is being parser
    type_table: TypeTable,
    current_fn: Option<Symbol>, // Current function that is being parser
    cur_class: Option<Type>,    // Current class that is being parser
    fn_ast_list: LinkedList<FnAst>, // Linked list of all functions that is pasered
}

impl Default for ParserState {
    fn default() -> Self {
        ParserState {
            global_symbtab: SymbolTable::default(),
            local_symbtab: SymbolTable::default(),
            type_table: TypeTable::default(),
            label: LabelGenerator::default(),
            current_fn: None,
            cur_class: None,
            fn_ast_list: LinkedList::new(),
        }
    }
}

impl ParserState {
    pub fn gst(&mut self) -> &mut SymbolTable {
        &mut self.global_symbtab
    }

    pub fn lst(&mut self) -> &mut SymbolTable {
        &mut self.local_symbtab
    }

    pub fn flabel(&mut self) -> &mut LabelGenerator {
        &mut self.label
    }

    // Returns the defined return type of the current function bieng parser
    pub fn cfn_rtype(&self) -> Type {
        match self.current_fn {
            Some(ref s) => s.get_type(),
            None => Type::Primitive(PrimitiveType::Int),
        }
    }

    // Returns the defined parameter list of the current function bieng parser
    pub fn cfn_params(&self) -> LinkedList<(Type, String)> {
        match self.current_fn {
            Some(ref s) => s.get_params().unwrap(),
            None => LinkedList::new(),
        }
    }

    pub fn cfn(&self) -> Option<&Symbol> {
        self.current_fn.as_ref()
    }

    pub fn tt(&mut self) -> &mut TypeTable {
        &mut self.type_table
    }

    // When a new function is about to be parser (after an fname is parser)
    // the state of the parser is update to store the symbol of the
    // new function in the parser state DS
    pub fn update_state<'t>(&mut self, fname: &'t str) -> Result<&'t str, &'static str> {
        self.local_symbtab = SymbolTable::default();
        self.current_fn = match fname {
            "" => None,
            _ => Some(
                self.global_symbtab
                    .get(fname)
                    .filter(|s| matches!(s, Symbol::Function { .. }))
                    .ok_or("Function was not defined")?
                    .clone(),
            ),
        };
        Ok(fname)
    }

    pub fn get_var(&self, name: &str) -> Result<Symbol, &'static str> {
        self.local_symbtab
            .get(name)
            .or(self
                .global_symbtab
                .get(name)
                .filter(|s| !matches!(s, Symbol::Function { .. })))
            .cloned()
            .ok_or("Variable was not declared")
    }

    // Not complete, indented to set new class in cur_class field of the parser state DS
    pub fn set_class(
        &mut self,
        lexer: &dyn NonStreamingLexer<DefaultLexeme, u32>,
        class: Span,
        parent: Option<Span>,
    ) -> Result<(), (Option<Span>, &'static str)> {
        let cname = lexer.span_str(class);
        let class_inst = match parent {
            Some(pspan) => {
                let pname = lexer.span_str(pspan);
                match self.tt().get_pointer(pname) {
                    Some(inst) => Type::Class {
                        name: cname.to_string(),
                        cst: SymbolTable::default(),
                        idx: 0,
                        size: 0,
                        parent: Some(Arc::downgrade(&inst)),
                        fn_list: Vec::new(),
                    },
                    None => return Err((parent, "Parent class not defined")),
                }
            }
            None => Type::Class {
                name: cname.to_string(),
                cst: SymbolTable::default(),
                idx: 0,
                size: 0,
                parent: None,
                fn_list: Vec::new(),
            },
        };
        self.cur_class = Some(class_inst);
        Ok(())
    }
}
