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
#[cfg(test)]
mod tests;
mod token;
mod unnest_externs;

pub use parser::{ParseConfig, Parser};
pub use unnest_externs::ExternUnnester;
