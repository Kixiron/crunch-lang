#![no_std]

extern crate alloc;
pub extern crate string_interner;

pub mod parser;
pub mod token;

pub use parser::Parser;
