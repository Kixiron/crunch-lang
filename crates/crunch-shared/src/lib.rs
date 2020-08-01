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
// TODO: Better abstraction for this
pub extern crate codespan_reporting;
pub extern crate crunch_proc;
pub extern crate salsa;

pub use log::{debug, error, info, trace, warn};

pub mod context;
pub mod error;
pub mod file_hash;
pub mod files;
pub mod meta;
// mod passes;
pub mod allocator;
pub mod config;
pub mod strings;
pub mod trees;
pub mod utils;
pub mod visitors;
