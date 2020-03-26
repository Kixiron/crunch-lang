use codespan_reporting::files;
use std::ops::Range;

#[derive(Debug, Clone)]
struct File {
    name: String,
    source: String,
    line_starts: Vec<usize>,
}

impl File {
    fn line_start(&self, line_index: usize) -> Option<usize> {
        use std::cmp::Ordering;

        match line_index.cmp(&self.line_starts.len()) {
            Ordering::Less => self.line_starts.get(line_index).cloned(),
            Ordering::Equal => Some(self.source.len()),
            Ordering::Greater => None,
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct FileId(u32);

impl FileId {
    pub fn new(id: u32) -> Self {
        Self(id)
    }
}

#[derive(Debug, Clone)]
pub struct Files {
    files: Vec<File>,
}

impl Files {
    pub fn new() -> Files {
        Files { files: Vec::new() }
    }

    pub fn add(&mut self, name: impl Into<String>, source: impl Into<String>) -> Option<FileId> {
        use std::convert::TryFrom;

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

    fn get(&self, file_id: FileId) -> Option<&File> {
        self.files.get(file_id.0 as usize)
    }
}

impl<'files> files::Files<'files> for Files {
    type FileId = FileId;
    type Name = &'files str;
    type Source = &'files str;

    fn name(&self, file_id: FileId) -> Option<&str> {
        Some(self.get(file_id)?.name.as_ref())
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
