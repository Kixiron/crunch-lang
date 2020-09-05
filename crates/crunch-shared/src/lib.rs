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
#[doc(hidden)]
pub extern crate codespan_reporting;
pub use crunch_proc;
pub extern crate inventory;
#[doc(hidden)]
pub extern crate salsa;
#[doc(hidden)]
pub extern crate tracing;

pub use tracing::{
    debug, debug_span, error, error_span, event, info, info_span, instrument, span, trace,
    trace_span, warn, warn_span,
};

pub mod allocator;
pub mod config;
pub mod context;
pub mod databases;
pub mod distance;
pub mod error;
pub mod file_hash;
pub mod files;
pub mod meta;
pub mod strings;
pub mod trees;
pub mod utils;
pub mod visitors;
