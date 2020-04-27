use crate::{
    files::FileId,
    token::{Token, TokenType},
};

use alloc::{collections::VecDeque, format, string::String, vec};
use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    term::{
        self,
        termcolor::{ColorChoice, StandardStream},
        Config,
    },
};
use core::{fmt, ops::Range};
use derive_more::Display;

pub type ParseResult<T> = Result<T, Locatable<Error>>;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Location {
    Concrete { span: Span, file: FileId },
    Implicit { span: Span, file: FileId },
}

impl Location {
    pub fn concrete<S, F>(span: S, file: F) -> Self
    where
        S: Into<Span>,
        F: Into<FileId>,
    {
        Self::Concrete {
            span: span.into(),
            file: file.into(),
        }
    }

    pub fn implicit<S, F>(span: S, file: F) -> Self
    where
        S: Into<Span>,
        F: Into<FileId>,
    {
        Self::Concrete {
            span: span.into(),
            file: file.into(),
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Self::Concrete { span, .. } => *span,
            Self::Implicit { span, .. } => *span,
        }
    }

    pub fn file(&self) -> FileId {
        match self {
            Self::Concrete { file, .. } => *file,
            Self::Implicit { file, .. } => *file,
        }
    }

    pub fn range(&self) -> Range<usize> {
        self.span().into()
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct Span {
    start: usize,
    end: usize,
}

impl Span {
    pub const fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub const fn start(&self) -> usize {
        self.start
    }

    pub const fn end(&self) -> usize {
        self.end
    }

    pub const fn merge(start: Self, end: Self) -> Span {
        Self::new(start.start, end.end)
    }

    pub const fn width(&self) -> usize {
        self.end - self.start
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}

impl<'a> From<&Token<'a>> for Span {
    fn from(token: &Token<'a>) -> Self {
        token.span()
    }
}

impl<'a> From<Token<'a>> for Span {
    fn from(token: Token<'a>) -> Self {
        token.span()
    }
}

impl From<Range<usize>> for Span {
    fn from(range: Range<usize>) -> Self {
        Self {
            start: range.start,
            end: range.end,
        }
    }
}

impl Into<Range<usize>> for Span {
    fn into(self) -> Range<usize> {
        self.start..self.end
    }
}

impl From<(usize, usize)> for Span {
    fn from(range: (usize, usize)) -> Self {
        Self {
            start: range.0,
            end: range.1,
        }
    }
}

impl Into<(usize, usize)> for Span {
    fn into(self) -> (usize, usize) {
        (self.start, self.end)
    }
}

impl From<[usize; 2]> for Span {
    fn from(range: [usize; 2]) -> Self {
        Self {
            start: range[0],
            end: range[1],
        }
    }
}

impl Into<[usize; 2]> for Span {
    fn into(self) -> [usize; 2] {
        [self.start, self.end]
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Locatable<T> {
    pub data: T,
    pub loc: Location,
}

impl<T> Locatable<T> {
    pub fn new(data: T, loc: Location) -> Self {
        Self { data, loc }
    }

    pub fn data(&self) -> &T {
        &self.data
    }

    pub fn into_data(self) -> T {
        self.data
    }

    pub fn loc(&self) -> Location {
        self.loc
    }

    pub fn span(&self) -> Span {
        self.loc.span()
    }

    pub fn file(&self) -> FileId {
        self.loc.file()
    }

    pub fn range(&self) -> Range<usize> {
        self.span().into()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ErrorHandler {
    errors: VecDeque<Locatable<Error>>,
    warnings: VecDeque<Locatable<Warning>>,
    fatal: bool,
}

impl ErrorHandler {
    pub fn new() -> Self {
        Self {
            errors: VecDeque::new(),
            warnings: VecDeque::new(),
            fatal: false,
        }
    }

    pub fn push_err(&mut self, err: Locatable<Error>) {
        self.fatal = true;
        self.errors.push_back(err);
    }

    pub fn push_warning(&mut self, warn: Locatable<Warning>) {
        self.warnings.push_back(warn);
    }

    pub fn is_fatal(&self) -> bool {
        self.fatal
    }

    /// Drain all errors and warnings from the current handler, emitting them
    pub fn emit(&mut self, files: &crate::files::Files) {
        let writer = StandardStream::stderr(ColorChoice::Auto);

        let config = Config::default();

        let mut diag = Diagnostic::warning();
        while let Some(err) = self.warnings.pop_front() {
            diag.message = format!("{}", err.data);

            diag.labels = vec![Label::primary(err.file(), err.range())];
            diag.labels.extend(err.data().extra_labels(err.file()));

            term::emit(&mut writer.lock(), &config, files, &diag).unwrap();
        }

        let mut diag = Diagnostic::error();
        while let Some(err) = self.errors.pop_front() {
            diag.message = format!("{}", err.data);

            diag.labels = vec![Label::primary(err.file(), err.range())];
            diag.labels.extend(err.data().extra_labels(err.file()));

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

#[derive(Clone, Debug, Display, PartialEq)]
pub enum Error {
    #[display(fmt = "Invalid Syntax: {}", _0)]
    Syntax(SyntaxError),

    #[display(fmt = "Semantic Error: {}", _0)]
    Semantic(SemanticError),

    #[display(fmt = "Type Error: {}", _0)]
    Type(TypeError),

    #[display(fmt = "Unexpected end of file")]
    EndOfFile,
}

impl Error {
    fn extra_labels(&self, file: FileId) -> Vec<Label<FileId>> {
        match self {
            Self::Syntax(err) => err.extra_labels(file),
            Self::Semantic(err) => err.extra_labels(file),
            Self::Type(err) => err.extra_labels(file),
            Self::EndOfFile => Vec::new(),
        }
    }
}

#[derive(Clone, Debug, Display, PartialEq)]
pub enum SyntaxError {
    #[display(fmt = "{}", _0)]
    Generic(String),

    #[display(fmt = "Unrecognized escape sequence: \\{}", _0)]
    UnrecognizedEscapeSeq(char),

    #[display(fmt = "String escapes are expected to begin with '{{' and end with '}}'")]
    MissingEscapeBraces,

    #[display(fmt = "String escapes may only have the characters {}", _0)]
    InvalidEscapeCharacters(&'static str),

    #[display(fmt = "Ran out of string escape specifiers")]
    MissingEscapeSpecifier,

    #[display(fmt = "Invalid escape sequence: {}", _0)]
    InvalidEscapeSeq(String),

    #[display(fmt = "Invalid {} literal", _0)]
    InvalidLiteral(&'static str),

    #[display(fmt = "{} literal overflowed: {}", _0, _1)]
    LiteralOverflow(&'static str, String),

    #[display(fmt = "{} literal underflowed: {}", _0, _1)]
    LiteralUnderflow(&'static str, String),

    #[display(fmt = "Rune literals may only contain one rune")]
    TooManyRunes,

    #[display(fmt = "Recursion limit reached: {} > {}", _0, _1)]
    RecursionLimit(usize, usize),

    #[display(fmt = "Attributes are not allowed on an {} declaration", _0)]
    NoAttributesAllowed(&'static str),

    #[display(fmt = "Invalid top-level token: {}", _0)]
    InvalidTopLevel(TokenType),

    #[display(fmt = "You must give a file to import from in import declarations")]
    MissingImport,

    #[display(fmt = "File imports must use a string literal")]
    ImportStringLiteral,

    #[display("File imports must use a string literal, not a byte string literal")]
    ImportByteStringLiteral,
}

impl SyntaxError {
    fn extra_labels(&self, _file: FileId) -> Vec<Label<FileId>> {
        match self {
            _ => Vec::new(),
        }
    }
}

#[derive(Clone, Debug, Display, PartialEq)]
pub enum SemanticError {
    #[display(fmt = "{} was previously defined", name)]
    Redefinition {
        name: String,
        first: Location,
        second: Location,
    },

    #[display(
        fmt = "Function bodies cannot be empty, use the `empty` keyword to create an empty function"
    )]
    EmptyFuncBody,

    #[display(
        fmt = "Type bodies cannot be empty, use the `empty` keyword to create an empty type"
    )]
    EmptyTypeBody,

    #[display(fmt = "Attributes should be ordered by visibility and then misc")]
    UnorderedAttrs,

    #[display(fmt = "The attribute `{}` was given multiple times", attr)]
    DuplicatedAttributes {
        attr: String,
        first: Location,
        second: Location,
    },

    #[display(fmt = "The attributes `{}` and `{}` conflict", attr1, attr2)]
    ConflictingAttributes {
        attr1: String,
        attr2: String,
        first: Location,
        second: Location,
    },
}

impl SemanticError {
    fn extra_labels(&self, file: FileId) -> Vec<Label<FileId>> {
        match self {
            Self::Redefinition { first, second, .. } => vec![
                Label::primary(file, first.range()).with_message("Defined here"),
                Label::secondary(file, second.range()).with_message("Redefined here"),
            ],
            Self::DuplicatedAttributes { first, second, .. } => vec![
                Label::primary(file, first.range()).with_message("Fist given here"),
                Label::secondary(file, second.range()).with_message("Given here"),
            ],
            Self::ConflictingAttributes { first, second, .. } => vec![
                Label::primary(file, first.range()),
                Label::secondary(file, second.range()),
            ],
            _ => Vec::new(),
        }
    }
}

#[derive(Clone, Debug, Display, PartialEq)]
pub enum TypeError {}

impl TypeError {
    fn extra_labels(&self, _file: FileId) -> Vec<Label<FileId>> {
        match self {
            _ => Vec::new(),
        }
    }
}

#[derive(Clone, Debug, Display, PartialEq)]
pub enum Warning {
    #[display(fmt = "The generic '{}' was not used", _0)]
    UnusedGeneric(String),
}

impl Warning {
    fn extra_labels(&self, _file: FileId) -> Vec<Label<FileId>> {
        match self {
            _ => Vec::new(),
        }
    }
}
