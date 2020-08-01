use crate::{
    error::{Location, Span},
    utils::{HashMap, Hasher},
};
use alloc::{string::String, sync::Arc, vec::Vec};
use codespan_reporting::files;
use core::ops::Range;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct File {
    name: Arc<String>,
    source: Arc<String>,
    line_starts: Arc<Vec<usize>>,
}

impl File {
    pub fn new(name: Arc<String>, source: Arc<String>, line_starts: Arc<Vec<usize>>) -> Self {
        Self {
            name,
            source,
            line_starts,
        }
    }

    #[inline]
    fn line_start(&self, line_index: usize) -> Option<usize> {
        use core::cmp::Ordering;

        match line_index.cmp(&self.line_starts.len()) {
            Ordering::Less => self.line_starts.get(line_index).cloned(),
            Ordering::Equal => Some(self.source.len()),
            Ordering::Greater => None,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Deserialize, Serialize)]
#[repr(transparent)]
pub struct FileId(pub u32);

impl FileId {
    #[inline]
    pub fn new(id: u32) -> Self {
        Self(id)
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
#[repr(transparent)]
pub struct Files {
    files: HashMap<FileId, File>,
}

impl Files {
    pub fn new() -> Files {
        Files {
            files: HashMap::with_hasher(Hasher::default()),
        }
    }

    pub fn add(&mut self, name: impl Into<String>, source: impl Into<String>) -> Option<FileId> {
        use core::convert::TryFrom;

        let file_id = FileId(u32::try_from(self.files.len()).ok()?);
        let name = Arc::new(name.into());
        let source = Arc::new(source.into());
        let line_starts = Arc::new(files::line_starts(&source).collect());

        self.files
            .insert(file_id, File::new(name, source, line_starts));

        Some(file_id)
    }

    fn get(&self, file_id: FileId) -> Option<&File> {
        self.files.get(&file_id)
    }
}

impl From<HashMap<FileId, File>> for Files {
    fn from(files: HashMap<FileId, File>) -> Self {
        Self { files }
    }
}

impl<'files> files::Files<'files> for Files {
    type FileId = FileId;
    type Name = &'files str;
    type Source = &'files str;

    fn name(&self, file_id: FileId) -> Option<&str> {
        Some(self.get(file_id)?.name.as_str())
    }

    fn source(&self, file_id: FileId) -> Option<&str> {
        Some(&self.get(file_id)?.source)
    }

    fn line_index(&self, file_id: FileId, byte_index: usize) -> Option<usize> {
        match self.get(file_id)?.line_starts.binary_search(&byte_index) {
            Ok(line) => Some(line),
            Err(next_line) => Some(next_line - 1),
        }
    }

    fn line_range(&self, file_id: FileId, line_index: usize) -> Option<Range<usize>> {
        let file = self.get(file_id)?;
        let line_start = file.line_start(line_index)?;
        let next_line_start = file.line_start(line_index + 1)?;

        Some(line_start..next_line_start)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct CurrentFile {
    file: FileId,
    length: usize,
}

impl CurrentFile {
    pub const fn new(file: FileId, length: usize) -> Self {
        Self { file, length }
    }

    pub const fn file(&self) -> FileId {
        self.file
    }

    pub const fn length(&self) -> usize {
        self.length
    }

    pub fn eof(&self) -> Location {
        Location::new(Span::new(self.length, self.length), self.file)
    }
}

impl Into<FileId> for CurrentFile {
    fn into(self) -> FileId {
        self.file
    }
}
