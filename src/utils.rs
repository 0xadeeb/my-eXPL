use std::error::Error;

pub mod label;
pub mod loop_util;
pub mod node;
pub mod register;

// pub use self::label::Label;
// pub use self::loop_util::LoopStack;
// pub use self::node::*;
// pub use self::register::RegisterPool;

pub fn err_from_str(e: &str) -> Box<dyn Error> {
    Box::<dyn Error>::from(e)
}
