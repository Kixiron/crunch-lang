use crate::{FlattenExternals, Parser as ParserBackend};
use alloc::sync::Arc;
use crunch_shared::{
    config::{ConfigDatabase, EmissionKind},
    context::ContextDatabase,
    databases::SourceDatabase,
    error::ErrorHandler,
    files::{CurrentFile, FileCache, FileId},
    salsa,
    trees::ast::Item,
    utils::Upcast,
};

type ArcError = Arc<ErrorHandler>;

#[salsa::query_group(ParseDatabaseStorage)]
pub trait ParseDatabase:
    salsa::Database + ConfigDatabase + SourceDatabase + ContextDatabase + Upcast<dyn SourceDatabase>
{
    /// Parses a single source file, returning the result
    // FIXME: Real lifetime when salsa allows
    fn parse(&self, file: FileId) -> Result<Arc<Vec<&'static Item<'static>>>, ArcError>;
}

fn parse(
    db: &dyn ParseDatabase,
    file: FileId,
) -> Result<Arc<Vec<&'static Item<'static>>>, ArcError> {
    let current_file = CurrentFile::new(file, db.source_length(file));
    let source = db.source_text(file);
    let config = db.config();

    let parser = ParserBackend::new(&source, config.clone(), current_file, &db.context());

    crunch_shared::allocator::CRUNCHC_ALLOCATOR
        .record_region("parsing", || parser.parse())
        .map(|(ast, mut warnings)| {
            warnings.emit(
                &FileCache::upcast(db),
                &**db.writer(),
                &**db.stdout_config(),
            );

            let ast = FlattenExternals::new().flatten(ast);

            if config.emit.contains(&EmissionKind::Ast) {
                let path = db
                    .config()
                    .out_dir
                    .join(&*db.file_name(file))
                    .with_extension("ast");

                std::fs::write(&path, format!("{:#?}", &ast)).unwrap();
            }

            if config.print.contains(&EmissionKind::Ast) {
                println!("{:#?}", &ast);
            }

            Arc::new(ast)
        })
        .map_err(Arc::new)
}
