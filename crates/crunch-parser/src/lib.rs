#![feature(track_caller)]

// TODO: no_std candidate

extern crate alloc;
pub extern crate string_interner;

pub mod ast;
mod ast_pass;
pub mod hir;
pub mod parser;
pub mod pratt_parser;
pub mod symbol_table;
pub mod token;

pub use ast_pass::{AstPass, AstPassExtra, AstPassRequires};
pub use parser::Parser;
pub use symbol_table::SymbolTable;
