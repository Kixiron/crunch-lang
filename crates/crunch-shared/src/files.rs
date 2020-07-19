use crate::error::{Location, Span};
use alloc::{string::String, vec::Vec};
use codespan_reporting::files;
use core::ops::Range;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Serialize)]
pub struct File {
    name: String,
    source: String,
    line_starts: Vec<usize>,
}

impl File {
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

#[derive(Debug, Default, Clone, PartialEq, Eq, Deserialize, Serialize)]
#[repr(transparent)]
pub struct Files {
    files: Vec<File>,
}

impl Files {
    #[inline]
    pub fn new() -> Files {
        Files { files: Vec::new() }
    }

    #[inline]
    pub fn add(&mut self, name: impl Into<String>, source: impl Into<String>) -> Option<FileId> {
        use core::convert::TryFrom;

        let file_id = FileId(u32::try_from(self.files.len()).ok()?);
        let name = name.into();
        let source = source.into();
        let line_starts = files::line_starts(&source).collect();

        self.files.push(File {
            name,
            line_starts,
            source,
        });

        Some(file_id)
    }

    #[inline]
    fn get(&self, file_id: FileId) -> Option<&File> {
        self.files.get(file_id.0 as usize)
    }
}

impl<'files> files::Files<'files> for Files {
    type FileId = FileId;
    type Name = &'files str;
    type Source = &'files str;

    #[inline]
    fn name(&self, file_id: FileId) -> Option<&str> {
        Some(self.get(file_id)?.name.as_ref())
    }

    #[inline]
    fn source(&self, file_id: FileId) -> Option<&str> {
        Some(&self.get(file_id)?.source)
    }

    #[inline]
    fn line_index(&self, file_id: FileId, byte_index: usize) -> Option<usize> {
        match self.get(file_id)?.line_starts.binary_search(&byte_index) {
            Ok(line) => Some(line),
            Err(next_line) => Some(next_line - 1),
        }
    }

    #[inline]
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
