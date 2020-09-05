#![cfg_attr(all(feature = "no-std", not(test)), no_std)]
#![warn(
    missing_copy_implementations,
    missing_debug_implementations,
    clippy::dbg_macro,
    clippy::missing_safety_doc,
    clippy::wildcard_imports,
    clippy::shadow_unrelated
)]
// Clippy was giving false positives with no source location,
// so this is just to make the damn thing shut the hell up
#![allow(clippy::suspicious_else_formatting, clippy::iter_next_slice)]

extern crate alloc;

pub mod database;
pub mod parser;
#[cfg(test)]
mod tests;
mod token;
mod unnest_externs;

pub use parser::{Parser, ParserReturn};
pub use unnest_externs::FlattenExternals;
