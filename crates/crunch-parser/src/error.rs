use crate::{files::FileId, token::TokenType};

use alloc::collections::VecDeque;
use thiserror::Error;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Location {
    pub span: Option<(usize, usize)>,
    pub file: FileId,
}

impl Location {
    pub fn new(range: impl Into<(usize, usize)>, file: FileId) -> Self {
        Self {
            span: Some(range.into()),
            file,
        }
    }

    pub fn file(file: FileId) -> Self {
        Self { span: None, file }
    }

    pub fn range(&self) -> Option<core::ops::Range<usize>> {
        self.span.map(|(s, e)| s..e)
    }
}

pub type ParseResult<T> = Result<T, Locatable<Error>>;

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Locatable<T> {
    pub data: T,
    pub location: Location,
}

impl<T> Locatable<T> {
    pub fn new(data: T, location: Location) -> Self {
        Self { data, location }
    }

    pub fn file(data: T, file: FileId) -> Self {
        Self {
            data,
            location: Location::file(file),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ErrorHandler {
    errors: VecDeque<Locatable<Error>>,
    warnings: VecDeque<CompileWarning>,
}

impl ErrorHandler {
    pub fn new() -> Self {
        Self {
            errors: VecDeque::new(),
            warnings: VecDeque::new(),
        }
    }

    pub fn push_err(&mut self, err: Locatable<Error>) {
        self.errors.push_back(err);
    }

    pub fn push_warning(&mut self, warn: CompileWarning) {
        self.warnings.push_back(warn);
    }

    pub fn emit(mut self, files: &crate::files::Files) {
        use codespan_reporting::{
            diagnostic::{Diagnostic, Label},
            term::{
                self,
                termcolor::{ColorChoice, StandardStream},
                Config,
            },
        };

        let writer = StandardStream::stderr(ColorChoice::Auto);

        let config = Config::default();

        let mut diag = Diagnostic::error();
        while let Some(err) = self.errors.pop_front() {
            diag.message = format!("{}", err.data);

            if let Some(range) = err.location.range() {
                diag.labels = vec![Label::primary(err.location.file, range)];
            } else {
                diag.labels = Vec::new();
            }

            term::emit(&mut writer.lock(), &config, files, &diag).unwrap();
        }
    }
}

#[derive(Clone, Debug, Error, PartialEq)]
pub enum Error {
    #[error("Invalid Syntax: {0}")]
    Syntax(#[from] SyntaxError),

    #[error("Unexpected end of file")]
    EndOfFile,
}

#[derive(Clone, Debug, Error, PartialEq)]
pub enum SyntaxError {
    #[error("{0}")]
    Generic(String),

    #[error("Unrecognized escape sequence: \\{0}")]
    UnrecognizedEscapeSeq(char),

    #[error("String escapes are expected to begin with '{{' and end with '}}'")]
    MissingEscapeBraces,

    #[error("String escapes may only have the characters {0}")]
    InvalidEscapeCharacters(&'static str),

    #[error("Ran out of string escape specifiers")]
    MissingEscapeSpecifier,

    #[error("Invalid escape sequence: {0}")]
    InvalidEscapeSeq(String),

    #[error("Invalid {0} literal")]
    InvalidLiteral(&'static str),

    #[error("Recursion limit reached: {0} > {1}")]
    RecursionLimit(usize, usize),

    #[error("Attributes are not allowed on an {0} declaration")]
    NoAttributesAllowed(&'static str),

    #[error("Invalid top-level token: {0}")]
    InvalidTopLevel(TokenType),

    #[error("You must give a file to import from in import declarations")]
    MissingImport,
}

#[derive(Clone, Debug, Error, PartialEq)]
pub enum CompileWarning {}
