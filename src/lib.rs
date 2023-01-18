use std::{collections::HashMap, sync::Mutex};

use lazy_static::lazy_static;

lazy_static! {
    pub static ref SYMBOL_TABLE: Mutex<HashMap<String, symbol_table::Gsymbol>> =
        Mutex::new(HashMap::new());
}

pub mod ast;
pub mod backend;
pub mod symbol_table;
pub mod utils;
