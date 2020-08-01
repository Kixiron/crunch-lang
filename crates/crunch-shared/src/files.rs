use crate::{
    databases::SourceDatabase,
    error::{Location, Span},
    utils::Upcast,
};
use alloc::string::String;
use codespan_reporting::files;
use core::{fmt, ops::Range};
use serde::{Deserialize, Serialize};

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Deserialize, Serialize)]
#[repr(transparent)]
pub struct FileId(pub u32);

impl FileId {
    #[inline]
    pub fn new(id: u32) -> Self {
        Self(id)
    }
}

#[derive(Copy, Clone)]
pub struct FileCache<'a> {
    source: &'a dyn SourceDatabase,
}

impl<'a> FileCache<'a> {
    pub fn new(source: &'a dyn SourceDatabase) -> Self {
        Self { source }
    }

    pub fn upcast<T>(source: &'a T) -> Self
    where
        T: Upcast<dyn SourceDatabase> + ?Sized,
    {
        Self::new(source.upcast())
    }
}

impl<'a> files::Files<'a> for FileCache<'a> {
    type FileId = FileId;
    type Name = String;
    type Source = String;

    fn name(&self, file: FileId) -> Option<String> {
        Some(self.source.file_name(file).as_ref().clone())
    }

    fn source(&self, file: FileId) -> Option<String> {
        Some(self.source.source_text(file).as_ref().clone())
    }

    fn line_index(&self, file: FileId, byte_index: usize) -> Option<usize> {
        self.source.line_index(file, byte_index)
    }

    fn line_range(&self, file: FileId, line_index: usize) -> Option<Range<usize>> {
        self.source.line_range(file, line_index)
    }
}

impl fmt::Debug for FileCache<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("FileCache").finish()
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
