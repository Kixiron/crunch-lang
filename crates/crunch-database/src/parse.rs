use crate::{ContextDatabase, SourceDatabase};
use alloc::sync::Arc;
use crunch_parser::{FlattenExternals, ParseConfig, Parser as ParserBackend, ParserReturn};
use crunch_shared::{
    error::ErrorHandler,
    files::{CurrentFile, FileId},
};
use salsa::Database;

#[salsa::query_group(ParseDatabaseStorage)]
pub trait ParseDatabase: Database + SourceDatabase + ContextDatabase {
    /// Parses a single source file, returning the result
    // FIXME: Real lifetime when salsa allows
    fn parse(&self, file: FileId) -> Result<Arc<ParserReturn<'static>>, Arc<ErrorHandler>>;
}

#[inline]
fn parse(
    db: &dyn ParseDatabase,
    file: FileId,
) -> Result<Arc<ParserReturn<'static>>, Arc<ErrorHandler>> {
    let current_file = CurrentFile::new(file, db.source_length(file));
    let source = db.source_text(file);

    // TODO: ParseConfig from options or query
    let parser = ParserBackend::new(&source, ParseConfig::default(), current_file, db.context());

    crunch_shared::allocator::CRUNCHC_ALLOCATOR
        .record_region("parsing", || parser.parse())
        .map_or_else(
            |err| Err(Arc::new(err)),
            |(ast, warnings)| {
                // TODO: Better scheme for ast preprocessing passes
                let ast = FlattenExternals::new().flatten(ast);

                Ok(Arc::new((ast, warnings)))
            },
        )
}
