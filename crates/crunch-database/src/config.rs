use salsa::Database;

#[salsa::query_group(ConfigDatabaseStorage)]
pub trait ConfigDatabase: Database {
    #[salsa::input]
    fn max_errors(&self) -> usize;
}
