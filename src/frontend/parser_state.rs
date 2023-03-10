use std::collections::LinkedList;
use std::default::Default;

use lrlex::DefaultLexeme;
use lrpar::{Lexeme, NonStreamingLexer, Span};

use crate::ast::FnAst;
use crate::symbol::{Symbol, SymbolTable};
use crate::type_table::{Type, TypeTable};
use crate::utils::label::LabelGenerator;

// This DS was basically made so that I don't have to keep track of multiple
// global variables and just one global variable which will return references
// of global symbol table, type table, label generator etc when a function needs to use it
// Ideally only some fields like local symbol table and current fn/class is required in this DS
pub struct ParserState {
    gst: SymbolTable,
    lst: SymbolTable,
    type_table: TypeTable,
    fn_list: LinkedList<FnAst>,
    current_fn: Option<Symbol>, // Current function that is being parser
    cur_class: Option<Type>,    // Current class that is being parser
}

impl Default for ParserState {
    fn default() -> Self {
        ParserState {
            gst: SymbolTable::default(),
            lst: SymbolTable::default(),
            type_table: TypeTable::default(),
            fn_list: LinkedList::new(),
            current_fn: None,
            cur_class: None,
        }
    }
}

impl ParserState {
    pub fn gst(&self) -> &SymbolTable {
        &self.gst
    }

    pub fn gst_mut(&mut self) -> &mut SymbolTable {
        &mut self.gst
    }

    pub fn lst(&self) -> &SymbolTable {
        &self.lst
    }

    pub fn lst_mut(&mut self) -> &mut SymbolTable {
        &mut self.lst
    }

    pub fn tt(&self) -> &TypeTable {
        &self.type_table
    }

    pub fn tt_mut(&mut self) -> &mut TypeTable {
        &mut self.type_table
    }

    pub fn fn_list(&mut self) -> &mut LinkedList<FnAst> {
        &mut self.fn_list
    }

    // Returns the defined return type of the current function bieng parser
    pub fn cfn_rtype(&self) -> &Type {
        match self.current_fn {
            Some(ref s) => s.get_type(),
            None => &Type::Int,
        }
    }

    // Returns the defined parameter list of the current function bieng parser
    pub fn cfn_params(&self) -> LinkedList<(Type, String)> {
        match self.current_fn {
            Some(ref s) => s.get_params().unwrap().clone(),
            None => LinkedList::new(),
        }
    }

    pub fn cfn(&self) -> Option<&Symbol> {
        self.current_fn.as_ref()
    }

    // When a new function is about to be parser (after an fname is parser)
    // the state of the parser is update to store the symbol of the
    // new function in the parser state DS
    pub fn update_state<'t>(&mut self, fname: &'t str) -> Result<&'t str, &'static str> {
        self.lst = SymbolTable::default();
        self.current_fn = match fname {
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

    pub fn end_class(&mut self) {
        self.cur_class = None;
    }

    pub fn check_self(
        &self,
        token: DefaultLexeme,
    ) -> Result<DefaultLexeme<u32>, (Option<Span>, &'static str)> {
        match self.cur_class {
            Some(_) => Ok(token),
            None => Err((Some(token.span()), "No self in this scope")),
        }
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

    pub fn set_class<'ip>(
        &mut self,
        lexer: &dyn NonStreamingLexer<'ip, DefaultLexeme, u32>,
        class: Span,
        parent: Option<Span>,
    ) -> Result<&'ip str, (Option<Span>, &'static str)> {
        self.cur_class = Some(self.type_table.set_class(lexer, class, parent)?);
        Ok(lexer.span_str(class))
    }

    pub fn insert_cst(
        &mut self,
        span: Span,
        lexer: &dyn NonStreamingLexer<DefaultLexeme, u32>,
        field_list: LinkedList<(Type, Span)>,
        method_list: LinkedList<(Type, Span, LinkedList<(Type, Span)>)>,
        flabel: &mut LabelGenerator,
    ) -> Result<Vec<u8>, (Option<Span>, &'static str)> {
        self.type_table.insert_cst(
            span,
            self.cur_class.as_ref().unwrap().get_name().unwrap(),
            lexer,
            field_list,
            method_list,
            flabel,
        )
    }
}
