use crunch_shared::context::Context;

#[salsa::query_group(ContextDatabaseStorage)]
pub trait ContextDatabase: salsa::Database {
    // FIXME: Salsa won't allow bounded lifetimes, so we have to do this shit
    #[salsa::input]
    fn context(&self) -> Context<'static>;
}
