use crate::{files::FileId, token::TokenType};

use alloc::{collections::VecDeque, format, string::String, vec, vec::Vec};
use derive_more::Display;

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

    pub fn emit(&mut self, files: &crate::files::Files) {
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

impl From<Locatable<Error>> for ErrorHandler {
    fn from(err: Locatable<Error>) -> Self {
        let mut handler = ErrorHandler::new();
        handler.push_err(err);
        handler
    }
}

impl Default for ErrorHandler {
    fn default() -> Self {
        Self::new()
    }
}

// Waiting on thiserror/#64 for no_std with error derives
#[derive(Clone, Debug, Display, PartialEq)]
pub enum Error {
    #[display(fmt = "Invalid Syntax: {}", _0)]
    Syntax(SyntaxError),

    #[display(fmt = "Type Error: {}", _0)]
    Type(TypeError),

    #[display("Unexpected end of file")]
    EndOfFile,
}

#[derive(Clone, Debug, Display, PartialEq)]
pub enum SyntaxError {
    #[display(fmt = "{}", _0)]
    Generic(String),

    #[display(fmt = "Unrecognized escape sequence: \\{}", _0)]
    UnrecognizedEscapeSeq(char),

    #[display("String escapes are expected to begin with '{{' and end with '}}'")]
    MissingEscapeBraces,

    #[display(fmt = "String escapes may only have the characters {}", _0)]
    InvalidEscapeCharacters(&'static str),

    #[display("Ran out of string escape specifiers")]
    MissingEscapeSpecifier,

    #[display(fmt = "Invalid escape sequence: {}", _0)]
    InvalidEscapeSeq(String),

    #[display(fmt = "Invalid {} literal", _0)]
    InvalidLiteral(&'static str),

    #[display(fmt = "Recursion limit reached: {} > {}", _0, _1)]
    RecursionLimit(usize, usize),

    #[display(fmt = "Attributes are not allowed on an {} declaration", _0)]
    NoAttributesAllowed(&'static str),

    #[display(fmt = "Invalid top-level token: {}", _0)]
    InvalidTopLevel(TokenType),

    #[display("You must give a file to import from in import declarations")]
    MissingImport,

    #[display("File imports must use a string literal")]
    ImportStringLiteral,

    #[display("File imports must use a string literal, not a byte string literal")]
    ImportByteStringLiteral,
}

#[derive(Clone, Debug, Display, PartialEq)]
pub enum TypeError {
    #[display(fmt = "{} was previously defined at {:?}", _0, _1)]
    Redefinition(String, Location),
}

#[derive(Clone, Debug, Display, PartialEq)]
pub enum CompileWarning {}
