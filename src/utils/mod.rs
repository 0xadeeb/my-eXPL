use std::error::Error;

pub mod label;
pub mod loop_util;
pub mod register;

pub fn err_from_str(e: &str) -> Box<dyn Error> {
    Box::<dyn Error>::from(e)
}
