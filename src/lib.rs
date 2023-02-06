use frontend::parser_state::ParserState;
use lazy_static::lazy_static;
use std::sync::Mutex;

lazy_static! {
    pub static ref PARSER: Mutex<ParserState> = Mutex::new(ParserState::default());
}

pub mod ast;
pub mod backend;
pub mod frontend;
pub mod symbol;
pub mod type_table;
pub mod utils;
