use std::collections::LinkedList;
use std::default::Default;
use std::sync::Arc;

use lrlex::DefaultLexeme;
use lrpar::{NonStreamingLexer, Span};

use crate::ast::FnAst;
use crate::symbol::{Symbol, SymbolTable};
use crate::type_table::{PrimitiveType, Type, TypeTable};
use crate::utils::label::LabelGenerator;

pub struct ParserState {
    gst: SymbolTable,
    lst: SymbolTable,
    label: LabelGenerator,
    tt: TypeTable,
    cfn: Option<Symbol>,
    cur_class: Option<Type>,
    fn_ast_list: LinkedList<FnAst>,
}

impl Default for ParserState {
    fn default() -> Self {
        ParserState {
            gst: SymbolTable::default(),
            lst: SymbolTable::default(),
            tt: TypeTable::default(),
            label: LabelGenerator::default(),
            cfn: None,
            cur_class: None,
            fn_ast_list: LinkedList::new(),
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

    pub fn tt(&mut self) -> &mut TypeTable {
        &mut self.tt
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
