use crate::files::FileId;
use alloc::{
    collections::VecDeque,
    format,
    string::{String, ToString},
    vec,
    vec::Vec,
};
use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::Files as CodeFiles,
    term::{self, termcolor::StandardStream, Config},
};
use core::{
    fmt,
    hash::Hash,
    mem,
    ops::{Deref, DerefMut, Range},
};
use derive_more::Display;
use serde::{Deserialize, Serialize};

pub type ParseResult<T> = Result<T, Locatable<Error>>;
pub type TypeResult<T> = Result<T, Locatable<Error>>;
pub type MirResult<T> = Result<T, Locatable<MirError>>;

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Location {
    span: Span,
    file: FileId,
}

impl Location {
    pub fn new<S, F>(span: S, file: F) -> Self
    where
        S: Into<Span>,
        F: Into<FileId>,
    {
        Self {
            span: span.into(),
            file: file.into(),
        }
    }

    pub fn merge(self, other: Self) -> Self {
        debug_assert_eq!(self.file(), other.file());

        Self {
            span: Span::merge(self.span(), other.span()),
            file: self.file(),
        }
    }

    pub const fn span(&self) -> Span {
        self.span
    }

    pub const fn file(&self) -> FileId {
        self.file
    }

    pub const fn range(&self) -> Range<usize> {
        self.span().range()
    }

    pub fn map_span<F>(self, map: F) -> Self
    where
        F: FnOnce(Span) -> Span,
    {
        Self {
            span: map(self.span),
            file: self.file,
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Span {
    start: usize,
    end: usize,
}

impl Span {
    pub const fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub const fn double(span: usize) -> Self {
        Self {
            start: span,
            end: span,
        }
    }

    pub const fn start(&self) -> usize {
        self.start
    }

    pub const fn end(&self) -> usize {
        self.end
    }

    pub const fn range(&self) -> Range<usize> {
        self.start..self.end
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

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Locatable<T> {
    data: T,
    loc: Option<Location>,
}

impl<T> Locatable<T> {
    pub fn new(data: T, loc: Location) -> Self {
        Self {
            data,
            loc: Some(loc),
        }
    }

    pub fn none(data: T) -> Self {
        Self { data, loc: None }
    }

    pub fn data(&self) -> &T {
        &self.data
    }

    pub fn into_data(self) -> T {
        self.data
    }

    pub fn location(&self) -> Location {
        self.loc.unwrap()
    }

    pub fn span(&self) -> Span {
        self.loc.unwrap().span()
    }

    pub fn file(&self) -> FileId {
        self.loc.unwrap().file()
    }

    pub fn range(&self) -> Range<usize> {
        self.span().into()
    }

    pub fn map<F, U>(self, map: F) -> Locatable<U>
    where
        F: FnOnce(T) -> U,
    {
        Locatable {
            data: map(self.data),
            loc: self.loc,
        }
    }

    pub fn as_ref(&self) -> Locatable<&T> {
        Locatable {
            data: &self.data,
            loc: self.loc,
        }
    }

    pub fn as_mut(&mut self) -> Locatable<&mut T> {
        Locatable {
            data: &mut self.data,
            loc: self.loc,
        }
    }
}

impl<T: Deref> Locatable<T> {
    pub fn as_deref(&self) -> Locatable<&T::Target> {
        self.as_ref().map(|t| t.deref())
    }
}

impl<T: DerefMut> Locatable<T> {
    pub fn as_deref_mut(&mut self) -> Locatable<&mut T::Target> {
        self.as_mut().map(|t| t.deref_mut())
    }
}

impl<T> Deref for Locatable<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

impl<T> DerefMut for Locatable<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.data
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Deserialize, Serialize)]
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

    pub fn err_len(&self) -> usize {
        self.errors.len()
    }

    pub fn warn_len(&self) -> usize {
        self.warnings.len()
    }

    /// Drain all errors and warnings from the current handler, emitting them
    pub fn emit<'a, F>(&mut self, files: &'a F, writer: &StandardStream, config: &Config)
    where
        F: CodeFiles<'a, FileId = FileId>,
    {
        let mut diag = Vec::with_capacity(5);

        while let Some(err) = self.warnings.pop_front() {
            err.emit(err.file(), err.span(), &mut diag);

            for diag in diag.drain(..) {
                term::emit(&mut writer.lock(), &config, files, &diag).unwrap();
            }
        }

        while let Some(err) = self.errors.pop_front() {
            err.emit(files, err.file(), err.span(), &mut diag);

            for diag in diag.drain(..) {
                term::emit(&mut writer.lock(), &config, files, &diag).unwrap();
            }
        }
    }

    pub fn extend(&mut self, other: Self) {
        self.fatal = self.fatal || other.fatal;
        self.errors.extend(other.errors);
        self.warnings.extend(other.warnings);
    }

    pub fn take(&mut self) -> Self {
        let taken = Self {
            fatal: self.fatal,
            errors: mem::take(&mut self.errors),
            warnings: mem::take(&mut self.warnings),
        };
        self.fatal = false;

        taken
    }
}

impl From<Locatable<Error>> for ErrorHandler {
    fn from(err: Locatable<Error>) -> Self {
        let mut handler = ErrorHandler::new();
        handler.push_err(err);
        handler
    }
}

#[derive(Clone, Debug, Display, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum Error {
    #[display(fmt = "Invalid Syntax: {}", _0)]
    Syntax(SyntaxError),

    #[display(fmt = "Semantic Error: {}", _0)]
    Semantic(SemanticError),

    #[display(fmt = "Type Error: {}", _0)]
    Type(TypeError),

    #[display(fmt = "MIR Error: {}", _0)]
    Mir(MirError),

    #[display(fmt = "Unexpected end of file")]
    EndOfFile,
}

impl Error {
    fn emit<'a, F>(
        &self,
        files: &'a F,
        file: FileId,
        span: Span,
        diag: &mut Vec<Diagnostic<FileId>>,
    ) where
        F: CodeFiles<'a, FileId = FileId>,
    {
        match self {
            Self::Syntax(err) => err.emit(files, file, span, diag),
            Self::Semantic(err) => err.emit(files, file, span, diag),
            Self::Type(err) => err.emit(file, span, diag),
            Self::Mir(err) => err.emit(file, span, diag),
            Self::EndOfFile => diag.push(
                Diagnostic::error()
                    .with_message(self.to_string())
                    .with_labels(vec![Label::primary(file, span)]),
            ),
        }
    }
}

#[derive(Clone, Debug, Display, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
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

    #[display(fmt = "Too many errors occurred (limit: {})", _0)]
    TooManyErrors(usize),

    #[display(fmt = "{} are not allowed visibility specifiers", _0)]
    NoVisibilityAllowed(String),

    #[display(fmt = "Unrecognized calling convention: {:?}", _0)]
    UnrecognizedCallConv(String),
}

impl SyntaxError {
    fn emit<'a, F>(
        &self,
        _files: &'a F,
        file: FileId,
        span: Span,
        diag: &mut Vec<Diagnostic<FileId>>,
    ) where
        F: CodeFiles<'a, FileId = FileId>,
    {
        diag.push(
            Diagnostic::error()
                .with_message(self.to_string())
                .with_labels(vec![Label::primary(file, span)]),
        )
    }
}

impl Into<Error> for SyntaxError {
    fn into(self) -> Error {
        Error::Syntax(self)
    }
}

#[derive(Clone, Debug, Display, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
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
    fn emit<'a, F>(
        &self,
        _files: &'a F,
        file: FileId,
        span: Span,
        diag: &mut Vec<Diagnostic<FileId>>,
    ) where
        F: CodeFiles<'a, FileId = FileId>,
    {
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

impl Into<Error> for SemanticError {
    fn into(self) -> Error {
        Error::Semantic(self)
    }
}

#[derive(Clone, Debug, Display, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[allow(missing_copy_implementations)]
pub enum TypeError {
    #[display(fmt = "The variable '{}' was not found in this scope", _0)]
    VarNotInScope(String),

    #[display(fmt = "<Internal error, incorrectly rendered an error>")]
    TypeConflict {
        call_type: String,
        def_type: String,
        def_site: Location,
    },

    #[display(fmt = "Failed to infer the type of '{}'", _0)]
    FailedInfer(String),

    #[display(fmt = "{} are not optional", _0)]
    MissingType(String),

    #[display(fmt = "{}", _0)]
    IncorrectType(String),

    #[display(fmt = "The function '{}' was not found in this scope", _0)]
    FuncNotInScope(String),

    #[display(fmt = "<Internal error, incorrectly rendered an error>")]
    NotEnoughArgs {
        expected: usize,
        received: usize,
        def_site: Location,
    },
}

impl TypeError {
    fn emit(&self, file: FileId, span: Span, diag: &mut Vec<Diagnostic<FileId>>) {
        match self {
            Self::TypeConflict {
                call_type,
                def_type,
                def_site,
            } => {
                diag.push(
                    Diagnostic::error()
                        .with_message("mismatched types")
                        .with_labels(
                            [Label::primary(file, span)
                                .with_message(format!("Expected {}, got {}", def_type, call_type))]
                            .into(),
                        ),
                );
                diag.push(
                    Diagnostic::note()
                        .with_message("defined here")
                        .with_labels([Label::secondary(def_site.file(), def_site.range())].into()),
                )
            }

            Self::NotEnoughArgs {
                expected,
                received,
                def_site,
            } => {
                diag.push(
                    Diagnostic::error()
                        .with_message(format!(
                            "expected {} argument{}, got {}",
                            expected,
                            if *expected == 1 { "" } else { "s" },
                            received,
                        ))
                        .with_labels(vec![Label::primary(file, span)]),
                );
                diag.push(
                    Diagnostic::note()
                        .with_message("defined here")
                        .with_labels(vec![Label::primary(def_site.file(), def_site.range())]),
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

impl Into<Error> for TypeError {
    fn into(self) -> Error {
        Error::Type(self)
    }
}

#[derive(Clone, Debug, Display, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum MirError {
    #[display(fmt = "Out of scope variables were used: {}", _0)]
    OutOfScopeVariables(String),

    #[display(fmt = "The basic block {} is missing a terminator", _0)]
    MissingTerminator(String),

    #[display(fmt = "BasicBlock {} asks for the argument {} multiple times", _0, _1)]
    DuplicatedBBArg(u64, u64),
}

impl MirError {
    fn emit(&self, file: FileId, span: Span, diag: &mut Vec<Diagnostic<FileId>>) {
        diag.push(
            Diagnostic::error()
                .with_message(self.to_string())
                .with_labels(vec![Label::primary(file, span)]),
        )
    }
}

impl Into<Error> for MirError {
    fn into(self) -> Error {
        Error::Mir(self)
    }
}

#[derive(Clone, Debug, Display, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum Warning {
    #[display(fmt = "The generic '{}' was not used", _0)]
    UnusedGeneric(String),

    #[display(fmt = "Literals should not have more than one consecutive underscore")]
    TooManyUnderscores,
}

impl Warning {
    fn emit(&self, file: FileId, span: Span, diag: &mut Vec<Diagnostic<FileId>>) {
        diag.push(
            Diagnostic::warning()
                .with_message(self.to_string())
                .with_labels(vec![Label::primary(file, span)]),
        )
    }
}
