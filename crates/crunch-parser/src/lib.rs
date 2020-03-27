// #![cfg_attr(not(feature = "std"), no_std)] Waiting on thiserror no_std support

extern crate alloc;

mod error;
pub mod files;
mod interner;
pub mod parser;
mod pretty_printer;
pub mod symbol_table;
#[cfg(test)]
mod tests;
mod token;

pub use error::ErrorHandler;
pub use files::{FileId, Files};
pub use interner::{Interner, Sym};
pub use parser::{Parser, SyntaxTree};
pub use pretty_printer::PrettyPrinter;
