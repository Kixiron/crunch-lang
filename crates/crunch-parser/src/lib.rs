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

pub mod parser;
mod pretty_printer;
#[cfg(test)]
mod tests;
mod token;

pub use parser::{ParseConfig, Parser};
pub use pretty_printer::PrettyPrinter;
