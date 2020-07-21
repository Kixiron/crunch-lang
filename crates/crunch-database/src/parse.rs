use crate::{ContextDatabase, SourceDatabase};
use alloc::sync::Arc;
use crunch_parser::{ExternUnnester, ParseConfig, Parser as ParserBackend, ParserReturn};
use crunch_shared::{
    error::ErrorHandler,
    files::{CurrentFile, FileId},
};
use salsa::Database;

#[salsa::query_group(ParseDatabaseStorage)]
pub trait ParseDatabase: Database + SourceDatabase + ContextDatabase {
    /// Parses a single source file, returning the result
    fn parse(&self, file: FileId) -> Result<Arc<ParserReturn>, Arc<ErrorHandler>>;
}

#[inline]
fn parse(db: &dyn ParseDatabase, file: FileId) -> Result<Arc<ParserReturn>, Arc<ErrorHandler>> {
    let current_file = CurrentFile::new(file, db.source_length(file));
    let source = db.source_text(file);

    // TODO: ParseConfig from options or query
    let parser = ParserBackend::new(&source, ParseConfig::default(), current_file, db.context());
    parser
        .parse()
        .map(|(ast, warnings)| {
            // TODO: Better scheme for ast preprocessing passes
            let ast = ExternUnnester::new().unnest(ast);

            Arc::new((ast, warnings))
        })
        .map_err(Arc::new)
}
