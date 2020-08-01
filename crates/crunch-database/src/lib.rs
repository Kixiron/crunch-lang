extern crate alloc;

pub use crunch_parser::database::{ParseDatabase, SourceDatabase};
pub use crunch_shared::{config::ConfigDatabase, context::ContextDatabase};
pub use crunch_typecheck::TypecheckDatabase;
pub use ladder::HirDatabase;

use crunch_parser::database::{ParseDatabaseStorage, SourceDatabaseStorage};
use crunch_shared::{
    config::ConfigDatabaseStorage,
    context::ContextDatabaseStorage,
    salsa::{self, Database, Storage},
};
use crunch_typecheck::TypecheckDatabaseStorage;
use ladder::HirDatabaseStorage;

#[salsa::database(
    ConfigDatabaseStorage,
    ContextDatabaseStorage,
    SourceDatabaseStorage,
    ParseDatabaseStorage,
    TypecheckDatabaseStorage,
    HirDatabaseStorage
)]
#[derive(Default)]
pub struct CrunchDatabase {
    storage: Storage<Self>,
}

// TODO: Parallel queries
impl Database for CrunchDatabase {}
