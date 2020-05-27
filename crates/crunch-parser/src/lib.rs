#![cfg_attr(all(feature = "no-std", not(test)), no_std)]
#![warn(
    missing_copy_implementations,
    missing_debug_implementations,
    clippy::dbg_macro,
    clippy::missing_safety_doc,
    clippy::wildcard_imports,
    clippy::shadow_unrelated
)]

extern crate alloc;

mod context;
pub mod error;
pub mod files;
pub mod parser;
mod pretty_printer;
pub mod symbol_table;
#[cfg(test)]
mod tests;
mod token;

pub use context::{Context, StrT};
pub use error::ErrorHandler;
pub use files::{FileId, Files};
pub use parser::{CurrentFile, Parser};
pub use pretty_printer::PrettyPrinter;
pub use symbol_table::{Graph, Node, NodeId, Scope};
