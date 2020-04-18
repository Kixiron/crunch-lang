#![cfg_attr(all(feature = "no-std", not(test)), no_std)]

extern crate alloc;

pub mod error;
pub mod files;
mod interner;
pub mod parser;
mod pretty_printer;
mod symbol_table;
#[cfg(test)]
mod tests;
mod token;

pub use error::ErrorHandler;
pub use files::{FileId, Files};
pub use interner::{Interner, SmallSpur};
pub use parser::{Parser, SyntaxTree};
pub use pretty_printer::PrettyPrinter;
pub use symbol_table::SymbolTable;
