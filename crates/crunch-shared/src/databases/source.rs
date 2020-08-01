use crate::files::FileId;
use alloc::sync::Arc;
use core::{cmp::Ordering, ops::Range};
use std::path::PathBuf;

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

fn source_length(db: &dyn SourceDatabase, file: FileId) -> usize {
    db.source_text(file).len()
}

fn line_starts(db: &dyn SourceDatabase, file: FileId) -> Arc<Vec<usize>> {
    Arc::new(
        core::iter::once(0)
            .chain(db.source_text(file).match_indices('\n').map(|(i, _)| i + 1))
            .collect(),
    )
}

fn line_start(db: &dyn SourceDatabase, file: FileId, line_index: usize) -> Option<usize> {
    let line_starts = db.line_starts(file);

    match line_index.cmp(&line_starts.len()) {
        Ordering::Less => line_starts.get(line_index).cloned(),
        Ordering::Equal => Some(db.source_length(file)),
        Ordering::Greater => None,
    }
}

fn line_index(db: &dyn SourceDatabase, file: FileId, byte_index: usize) -> Option<usize> {
    match db.line_starts(file).binary_search(&byte_index) {
        Ok(line) => Some(line),
        Err(next_line) => Some(next_line - 1),
    }
}

fn line_range(db: &dyn SourceDatabase, file: FileId, line_index: usize) -> Option<Range<usize>> {
    let start = db.line_start(file, line_index)?;
    let end = db.line_start(file, line_index + 1)?;

    Some(start..end)
}
