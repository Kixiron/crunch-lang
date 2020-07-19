use crate::{ContextDatabase, SourceDatabase};
use alloc::sync::Arc;
use crunch_parser::{ExternUnnester, Parser as ParserBackend, ParserReturn};
use crunch_shared::{
    error::ErrorHandler,
    files::{CurrentFile, FileId},
};
use salsa::Database;

#[salsa::query_group(ParseDatabaseStorage)]
pub trait ParseDatabase: Database + SourceDatabase + ContextDatabase {
    // TODO: Use a parser config
    // #[salsa::input]
    // fn config(&self) -> ParseConfig;

    fn parse(&self, file: FileId) -> Result<Arc<ParserReturn>, Arc<ErrorHandler>>;
}

#[inline]
fn parse(db: &dyn ParseDatabase, file: FileId) -> Result<Arc<ParserReturn>, Arc<ErrorHandler>> {
    let current_file = CurrentFile::new(file, db.source_length(file));
    let source = db.source_text(file);

    // TODO: Use a parser config
    let parser = ParserBackend::new(&source, current_file, db.context());
    parser
        .parse()
        .map(|(ast, warnings)| {
            let ast = ExternUnnester::new().unnest(ast);

            Arc::new((ast, warnings))
        })
        .map_err(Arc::new)
}
