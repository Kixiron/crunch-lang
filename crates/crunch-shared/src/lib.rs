#![cfg_attr(feature = "no-std", no_std)]
#![warn(
    missing_copy_implementations,
    missing_debug_implementations,
    clippy::dbg_macro,
    clippy::missing_safety_doc,
    clippy::wildcard_imports,
    clippy::shadow_unrelated
)]

extern crate alloc;
pub extern crate crunch_proc;

pub use log::{debug, error, info, trace, warn};

pub mod context;
pub mod error;
pub mod file_hash;
pub mod files;
pub mod meta;
// mod passes;
pub mod allocator;
pub mod strings;
pub mod symbol_table;
pub mod trees;
pub mod utils;
pub mod visitors;
