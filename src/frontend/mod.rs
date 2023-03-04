use self::parser_state::ParserState;
use lazy_static::lazy_static;
use std::sync::Mutex;

lazy_static! {
    pub static ref PARSER: Mutex<ParserState> = Mutex::new(ParserState::default());
}

pub mod parser_state;
pub mod semantics;
