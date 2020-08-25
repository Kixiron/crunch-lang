pub use crunch_codegen::CodegenDatabase;
pub use crunch_mir::MirDatabase;
pub use crunch_parser::database::ParseDatabase;
pub use crunch_shared::{
    config::ConfigDatabase, context::ContextDatabase, databases::SourceDatabase,
};
pub use crunch_typecheck::TypecheckDatabase;
pub use ladder::HirDatabase;

use crunch_codegen::CodegenDatabaseStorage;
use crunch_mir::MirDatabaseStorage;
use crunch_parser::database::ParseDatabaseStorage;
use crunch_shared::{
    config::ConfigDatabaseStorage,
    context::ContextDatabaseStorage,
    databases::SourceDatabaseStorage,
    salsa::{self, Database, Storage},
    utils::Upcast,
};
use crunch_typecheck::TypecheckDatabaseStorage;
use ladder::HirDatabaseStorage;

#[salsa::database(
    ConfigDatabaseStorage,
    ContextDatabaseStorage,
    SourceDatabaseStorage,
    ParseDatabaseStorage,
    HirDatabaseStorage,
    TypecheckDatabaseStorage,
    MirDatabaseStorage,
    CodegenDatabaseStorage
)]
#[derive(Default)]
pub struct CrunchDatabase {
    storage: Storage<Self>,
}

impl Upcast<dyn ConfigDatabase> for CrunchDatabase {
    fn upcast(&self) -> &dyn ConfigDatabase {
        &*self
    }
}

impl Upcast<dyn ContextDatabase> for CrunchDatabase {
    fn upcast(&self) -> &dyn ContextDatabase {
        &*self
    }
}

impl Upcast<dyn SourceDatabase> for CrunchDatabase {
    fn upcast(&self) -> &dyn SourceDatabase {
        &*self
    }
}

impl Upcast<dyn ParseDatabase> for CrunchDatabase {
    fn upcast(&self) -> &dyn ParseDatabase {
        &*self
    }
}

impl Upcast<dyn HirDatabase> for CrunchDatabase {
    fn upcast(&self) -> &dyn HirDatabase {
        &*self
    }
}

impl Upcast<dyn TypecheckDatabase> for CrunchDatabase {
    fn upcast(&self) -> &dyn TypecheckDatabase {
        &*self
    }
}

impl Upcast<dyn MirDatabase> for CrunchDatabase {
    fn upcast(&self) -> &dyn MirDatabase {
        &*self
    }
}

impl Upcast<dyn CodegenDatabase> for CrunchDatabase {
    fn upcast(&self) -> &dyn CodegenDatabase {
        &*self
    }
}

// TODO: Parallel queries
impl Database for CrunchDatabase {}
