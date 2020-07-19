use crunch_shared::context::Context;

#[salsa::query_group(ContextDatabaseStorage)]
pub trait ContextDatabase: salsa::Database {
    #[salsa::input]
    fn context(&self) -> Context;
}
