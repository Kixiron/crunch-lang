use crate::{FlattenExternals, Parser as ParserBackend};
use alloc::sync::Arc;
use core::{cmp::Ordering, iter::FromIterator, ops::Range};
use crunch_shared::{
    config::{ConfigDatabase, EmissionKind},
    context::ContextDatabase,
    error::ErrorHandler,
    files::{CurrentFile, File, FileId, Files},
    salsa::{self, debug::DebugQueryTable},
    trees::ast::Item,
    utils::HashMap,
};
use std::path::PathBuf;

type ArcError = Arc<ErrorHandler>;

#[salsa::query_group(ParseDatabaseStorage)]
pub trait ParseDatabase:
    salsa::Database + ConfigDatabase + SourceDatabase + ContextDatabase
{
    /// Parses a single source file, returning the result
    // FIXME: Real lifetime when salsa allows
    fn parse(&self, file: FileId) -> Result<Arc<Vec<&'static Item<'static>>>, ArcError>;
}

#[inline]
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
            warnings.emit(&*db.codespan_files(), &**db.writer(), &**db.stdout_config());

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

/// The database that contains all the source files of the compiler
#[salsa::query_group(SourceDatabaseStorage)]
pub trait SourceDatabase: salsa::Database {
    /// The name of a source file
    // TODO: Make this return a FileId when setting the path
    #[salsa::input]
    fn file_path(&self, file: FileId) -> Arc<PathBuf>;

    /// Get the name of a file relative to the file root
    fn file_name(&self, file: FileId) -> Arc<String>;

    /// The source text of a file
    fn source_text(&self, file: FileId) -> Arc<String>;

    fn codespan_files(&self) -> Arc<Files>;

    /// Get the codespan representation of a file for error emission
    fn codespan_file(&self, file: FileId) -> File;

    /// The length of a source file
    fn source_length(&self, file: FileId) -> usize;

    /// The indices of every line start for the file
    fn line_starts(&self, file: FileId) -> Arc<Vec<usize>>;

    /// The index a line starts at
    fn line_start(&self, file: FileId, line_index: usize) -> Option<usize>;

    /// The line which a byte index falls on
    fn line_index(&self, file: FileId, byte_index: usize) -> Option<usize>;

    /// The range of a single line
    fn line_range(&self, file: FileId, line_index: usize) -> Option<Range<usize>>;
}

fn file_name(db: &dyn SourceDatabase, file: FileId) -> Arc<String> {
    // FIXME: Make this get the actual relative path
    Arc::new(
        db.file_path(file)
            .file_stem()
            .unwrap()
            .to_string_lossy()
            .to_string(),
    )
}

fn source_text(db: &dyn SourceDatabase, file: FileId) -> Arc<String> {
    Arc::new(std::fs::read_to_string(&*db.file_path(file)).unwrap())
}

fn codespan_files(db: &dyn SourceDatabase) -> Arc<Files> {
    let entries: Vec<_> = FilePathQuery.in_db(db).entries();
    let files = HashMap::from_iter(
        entries
            .into_iter()
            .map(|entry| (entry.key, db.codespan_file(entry.key))),
    );

    Arc::new(Files::from(files))
}

fn codespan_file(db: &dyn SourceDatabase, file: FileId) -> File {
    File::new(
        db.file_name(file),
        db.source_text(file),
        db.line_starts(file),
    )
}

#[inline]
fn source_length(db: &dyn SourceDatabase, file: FileId) -> usize {
    db.source_text(file).len()
}

#[inline]
fn line_starts(db: &dyn SourceDatabase, file: FileId) -> Arc<Vec<usize>> {
    Arc::new(
        core::iter::once(0)
            .chain(db.source_text(file).match_indices('\n').map(|(i, _)| i + 1))
            .collect(),
    )
}

#[inline]
fn line_start(db: &dyn SourceDatabase, file: FileId, line_index: usize) -> Option<usize> {
    let line_starts = db.line_starts(file);

    match line_index.cmp(&line_starts.len()) {
        Ordering::Less => line_starts.get(line_index).cloned(),
        Ordering::Equal => Some(db.source_length(file)),
        Ordering::Greater => None,
    }
}

#[inline]
fn line_index(db: &dyn SourceDatabase, file: FileId, byte_index: usize) -> Option<usize> {
    match db.line_starts(file).binary_search(&byte_index) {
        Ok(line) => Some(line),
        Err(next_line) => Some(next_line - 1),
    }
}

#[inline]
fn line_range(db: &dyn SourceDatabase, file: FileId, line_index: usize) -> Option<Range<usize>> {
    let line_start = db.line_start(file, line_index)?;
    let next_line_start = db.line_start(file, line_index)?;

    Some(line_start..next_line_start)
}
