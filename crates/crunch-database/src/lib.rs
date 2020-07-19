extern crate alloc;

mod context;
mod parse;
mod source;

pub use context::ContextDatabase;
pub use parse::ParseDatabase;
pub use source::SourceDatabase;

use context::ContextDatabaseStorage;
use parse::ParseDatabaseStorage;
use salsa::{Database, Storage};
use source::SourceDatabaseStorage;

#[salsa::database(SourceDatabaseStorage, ContextDatabaseStorage, ParseDatabaseStorage)]
#[derive(Default)]
pub struct CrunchDatabase {
    storage: Storage<Self>,
}

// TODO: Parallel queries
impl Database for CrunchDatabase {}
