use lazy_static::lazy_static;
use std::sync::Mutex;
use symbol_table::SymbolTable;

lazy_static! {
    pub static ref SYMBOL_TABLE: Mutex<SymbolTable> = Mutex::new(SymbolTable::default());
}

pub mod ast;
pub mod backend;
pub mod symbol_table;
pub mod utils;
