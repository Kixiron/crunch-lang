#![cfg_attr(feature = "no-std", no_std)]

extern crate alloc;

#[cfg(feature = "logging")]
pub use log::{debug, error, info, trace, warn};

pub mod ast;
pub mod context;
pub mod error;
pub mod files;
pub mod strings;
pub mod symbol_table;
pub mod utils;
pub mod visitors;
