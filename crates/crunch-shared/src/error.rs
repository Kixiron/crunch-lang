use crate::files::FileId;

use alloc::{
    collections::VecDeque,
    string::{String, ToString},
    vec,
    vec::Vec,
};
use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    term::{
        self,
        termcolor::{ColorChoice, StandardStream},
        Config,
    },
};
use core::{
    fmt,
    hash::{Hash, Hasher},
    ops::{self, Range},
};
use derive_more::Display;
use serde::{Deserialize, Serialize};

pub type ParseResult<T> = Result<T, Locatable<Error>>;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Location {
    Concrete { span: Span, file: FileId },
    Implicit { span: Span, file: FileId },
}

impl Location {
    #[inline]
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

    #[inline]
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

    #[inline]
    pub fn span(&self) -> Span {
        match self {
            Self::Concrete { span, .. } => *span,
            Self::Implicit { span, .. } => *span,
        }
    }

    #[inline]
    pub fn file(&self) -> FileId {
        match self {
            Self::Concrete { file, .. } => *file,
            Self::Implicit { file, .. } => *file,
        }
    }

    #[inline]
    pub fn range(&self) -> Range<usize> {
        self.span().into()
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Span {
    start: usize,
    end: usize,
}

impl Span {
    #[inline]
    pub const fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    #[inline]
    pub const fn start(&self) -> usize {
        self.start
    }

    #[inline]
    pub const fn end(&self) -> usize {
        self.end
    }

    #[inline]
    pub const fn merge(start: Self, end: Self) -> Span {
        Self::new(start.start, end.end)
    }

    #[inline]
    pub const fn width(&self) -> usize {
        self.end - self.start
    }
}

impl fmt::Debug for Span {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}

impl From<Range<usize>> for Span {
    #[inline]
    fn from(range: Range<usize>) -> Self {
        Self {
            start: range.start,
            end: range.end,
        }
    }
}

impl Into<Range<usize>> for Span {
    #[inline]
    fn into(self) -> Range<usize> {
        self.start..self.end
    }
}

impl From<(usize, usize)> for Span {
    #[inline]
    fn from(range: (usize, usize)) -> Self {
        Self {
            start: range.0,
            end: range.1,
        }
    }
}

impl Into<(usize, usize)> for Span {
    #[inline]
    fn into(self) -> (usize, usize) {
        (self.start, self.end)
    }
}

impl From<[usize; 2]> for Span {
    #[inline]
    fn from(range: [usize; 2]) -> Self {
        Self {
            start: range[0],
            end: range[1],
        }
    }
}

impl Into<[usize; 2]> for Span {
    #[inline]
    fn into(self) -> [usize; 2] {
        [self.start, self.end]
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Locatable<T> {
    pub data: T,
    pub loc: Location,
}

impl<T> Locatable<T> {
    #[inline]
    pub fn new(data: T, loc: Location) -> Self {
        Self { data, loc }
    }

    #[inline]
    pub fn data(&self) -> &T {
        &self.data
    }

    #[inline]
    pub fn into_data(self) -> T {
        self.data
    }

    #[inline]
    pub fn loc(&self) -> Location {
        self.loc
    }

    #[inline]
    pub fn span(&self) -> Span {
        self.loc.span()
    }

    #[inline]
    pub fn file(&self) -> FileId {
        self.loc.file()
    }

    #[inline]
    pub fn range(&self) -> Range<usize> {
        self.span().into()
    }
}

impl<T> AsRef<T> for Locatable<T> {
    #[inline]
    fn as_ref(&self) -> &T {
        &self.data
    }
}

impl<T> AsMut<T> for Locatable<T> {
    #[inline]
    fn as_mut(&mut self) -> &mut T {
        &mut self.data
    }
}

impl<T> ops::Deref for Locatable<T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

impl<T> ops::DerefMut for Locatable<T> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.data
    }
}

impl<T> Hash for Locatable<T>
where
    T: Hash,
{
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.data.hash(state)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ErrorHandler {
    errors: VecDeque<Locatable<Error>>,
    warnings: VecDeque<Locatable<Warning>>,
    fatal: bool,
}

impl ErrorHandler {
    #[inline]
    pub fn new() -> Self {
        Self {
            errors: VecDeque::new(),
            warnings: VecDeque::new(),
            fatal: false,
        }
    }

    #[inline]
    pub fn push_err(&mut self, err: Locatable<Error>) {
        self.fatal = true;
        self.errors.push_back(err);
    }

    #[inline]
    pub fn push_warning(&mut self, warn: Locatable<Warning>) {
        self.warnings.push_back(warn);
    }

    #[inline]
    pub fn is_fatal(&self) -> bool {
        self.fatal
    }

    /// Drain all errors and warnings from the current handler, emitting them
    #[inline]
    pub fn emit<'a, F>(&mut self, files: &'a F)
    where
        F: codespan_reporting::files::Files<'a, FileId = FileId>,
    {
        let writer = StandardStream::stderr(ColorChoice::Auto);
        let config = Config::default();
        let mut diag = Vec::with_capacity(5);

        while let Some(err) = self.warnings.pop_front() {
            err.emit(err.file(), err.span(), &mut diag);

            for diag in diag.drain(..) {
                term::emit(&mut writer.lock(), &config, files, &diag).unwrap();
            }
        }

        while let Some(err) = self.errors.pop_front() {
            err.emit(err.file(), err.span(), &mut diag);

            for diag in diag.drain(..) {
                term::emit(&mut writer.lock(), &config, files, &diag).unwrap();
            }
        }
    }
}

impl From<Locatable<Error>> for ErrorHandler {
    #[inline]
    fn from(err: Locatable<Error>) -> Self {
        let mut handler = ErrorHandler::new();
        handler.push_err(err);
        handler
    }
}

impl Default for ErrorHandler {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone, Debug, Display, PartialEq, Eq, Serialize, Deserialize)]
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
    fn emit(&self, file: FileId, span: Span, diag: &mut Vec<Diagnostic<FileId>>) {
        match self {
            Self::Syntax(err) => err.emit(file, span, diag),
            Self::Semantic(err) => err.emit(file, span, diag),
            Self::Type(err) => err.emit(file, span, diag),
            Self::EndOfFile => diag.push(
                Diagnostic::error()
                    .with_message(self.to_string())
                    .with_labels(vec![Label::primary(file, span)]),
            ),
        }
    }
}

#[derive(Clone, Debug, Display, PartialEq, Eq, Serialize, Deserialize)]
pub enum SyntaxError {
    #[display(fmt = "{}", _0)]
    Generic(String),

    #[display(fmt = "Unrecognized escape sequence: \\{}", _0)]
    UnrecognizedEscapeSeq(char),

    #[display(fmt = "String escapes are expected to begin with '{{' and end with '}}'")]
    MissingEscapeBraces,

    #[display(fmt = "String escapes may only have the characters {}", _0)]
    InvalidEscapeCharacters(String),

    #[display(fmt = "Ran out of string escape specifiers")]
    MissingEscapeSpecifier,

    #[display(fmt = "Invalid escape sequence: {}", _0)]
    InvalidEscapeSeq(String),

    #[display(fmt = "Invalid {} literal", _0)]
    InvalidLiteral(String),

    #[display(fmt = "{} literal overflowed: {}", _0, _1)]
    LiteralOverflow(String, String),

    #[display(fmt = "{} literal underflowed: {}", _0, _1)]
    LiteralUnderflow(String, String),

    #[display(fmt = "Rune literals may only contain one rune")]
    TooManyRunes,

    #[display(fmt = "Recursion limit reached: {} > {}", _0, _1)]
    RecursionLimit(usize, usize),

    #[display(fmt = "Attributes are not allowed on an {} declaration", _0)]
    NoAttributesAllowed(String),

    #[display(fmt = "Decorators are not allowed on an {} declaration", _0)]
    NoDecoratorsAllowed(String),

    #[display(fmt = "Invalid top-level token: {}", _0)]
    InvalidTopLevel(String),

    #[display(fmt = "You must give a file to import from in import declarations")]
    MissingImport,

    #[display(fmt = "File imports must use a string literal")]
    ImportStringLiteral,

    #[display(fmt = "File imports must use a string literal, not a byte string literal")]
    ImportByteStringLiteral,

    #[display(fmt = "Array lengths cannot be negative")]
    NegativeArrayLen,
}

impl SyntaxError {
    #[inline]
    fn emit(&self, file: FileId, span: Span, diag: &mut Vec<Diagnostic<FileId>>) {
        diag.push(
            Diagnostic::error()
                .with_message(self.to_string())
                .with_labels(vec![Label::primary(file, span)]),
        )
    }
}

#[derive(Clone, Debug, Display, PartialEq, Eq, Serialize, Deserialize)]
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

    #[display(fmt = "A constant cannot be declared as mutable")]
    MutableConstant,
}

impl SemanticError {
    #[inline]
    fn emit(&self, file: FileId, span: Span, diag: &mut Vec<Diagnostic<FileId>>) {
        match self {
            Self::Redefinition { first, second, .. } => {
                diag.push(
                    Diagnostic::error()
                        .with_message(self.to_string())
                        .with_labels(vec![
                            Label::primary(file, first.range()).with_message("Defined here"),
                            Label::secondary(file, second.range()).with_message("Redefined here"),
                        ]),
                );
            }

            Self::DuplicatedAttributes { first, second, .. } => {
                diag.push(
                    Diagnostic::error()
                        .with_message(self.to_string())
                        .with_labels(vec![
                            Label::primary(file, first.range()).with_message("Fist given here"),
                            Label::secondary(file, second.range()).with_message("Given here"),
                        ]),
                );
            }

            Self::ConflictingAttributes { first, second, .. } => {
                diag.push(
                    Diagnostic::error()
                        .with_message(self.to_string())
                        .with_labels(vec![
                            Label::primary(file, first.range()),
                            Label::secondary(file, second.range()),
                        ]),
                );
            }

            _ => diag.push(
                Diagnostic::error()
                    .with_message(self.to_string())
                    .with_labels(vec![Label::primary(file, span)]),
            ),
        }
    }
}

#[derive(Clone, Debug, Display, PartialEq, Eq, Serialize, Deserialize)]
#[allow(missing_copy_implementations)]
pub enum TypeError {}

impl TypeError {
    #[inline]
    fn emit(&self, file: FileId, span: Span, diag: &mut Vec<Diagnostic<FileId>>) {
        diag.push(
            Diagnostic::error()
                .with_message(self.to_string())
                .with_labels(vec![Label::primary(file, span)]),
        )
    }
}

#[derive(Clone, Debug, Display, PartialEq, Eq, Serialize, Deserialize)]
pub enum Warning {
    #[display(fmt = "The generic '{}' was not used", _0)]
    UnusedGeneric(String),
}

impl Warning {
    #[inline]
    fn emit(&self, file: FileId, span: Span, diag: &mut Vec<Diagnostic<FileId>>) {
        diag.push(
            Diagnostic::error()
                .with_message(self.to_string())
                .with_labels(vec![Label::primary(file, span)]),
        )
    }
}
