use crate::llvm::utils::LLVMString;
use std::{
    borrow::Cow,
    error,
    ffi::NulError,
    fmt::{Display, Formatter, Result as FmtResult},
    str::Utf8Error,
};

/// A convenience type for `Result<T, `[`Error`]`>`
///
/// [`Error`]: crate::utils::Error
pub type Result<T> = std::result::Result<T, Error>;

/// An error that was encountered either from LLVM or from the program's behavior
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Error {
    /// The error message
    message: ErrorString,
    /// The type of error that occurred
    kind: ErrorKind,
}

impl Error {
    /// Create a new `Error`
    #[inline]
    pub(crate) fn new<M>(message: M, kind: ErrorKind) -> Self
    where
        M: Into<ErrorString>,
    {
        Self {
            message: message.into(),
            kind,
        }
    }

    /// Fetches the error message
    ///
    /// This method will always allocate, so [`Error::message_ref`] can often be a better option
    ///
    /// [`Error::message_ref`]: crate::utils::Error#message_ref
    #[inline]
    pub fn message(&self) -> String {
        self.message.to_string()
    }

    /// Fetches the error message as a [`Cow`]
    ///
    /// [`Cow`]: https://doc.rust-lang.org/std/borrow/enum.Cow.html
    #[inline]
    pub fn message_ref(&self) -> Cow<'_, str> {
        match self.message {
            ErrorString::String(ref string) => Cow::Borrowed(string),
            ErrorString::LLVMString(ref llvm_string) => llvm_string.to_string_lossy(),
        }
    }

    /// Fetches the type of error that occurred
    #[inline]
    pub const fn kind(&self) -> ErrorKind {
        self.kind
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(
            f,
            "an error was encountered: {:?}, {}",
            self.kind, self.message,
        )
    }
}

impl error::Error for Error {}

impl From<Utf8Error> for Error {
    fn from(err: Utf8Error) -> Self {
        Self::new(
            format!("a string was not valid utf-8: {}", err),
            ErrorKind::InvalidStr,
        )
    }
}

impl From<NulError> for Error {
    fn from(err: NulError) -> Self {
        Self::new(err.to_string(), ErrorKind::InvalidStr)
    }
}

/// The kind of error that was encountered
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum ErrorKind {
    /// LLVM returned a null pointer where one was not expected
    NullPtr,
    /// An invalid string was given or received
    InvalidStr,
    /// LLVM inexplicitly did the wrong thing
    LLVMError,
    /// An incorrect type was supplied
    MismatchedTypes,
    /// Invalid IR was parsed or created
    InvalidIR,
    /// A nonexistent file was attempted to be used
    FileNotFound,
    /// A module was invalid
    InvalidModule,
    /// A function was invalid
    InvalidFunction,
    /// Too many function arguments were provided
    TooManyArgs,
    /// Failed to emit something
    FailedEmission,
    /// The given target triple was invalid
    InvalidTriple,
    /// Failed to initialize a target
    FailedTargetInit,
}

/// A string that can either be a normal [`String`] or an [`LLVMString`]
///
/// [`String`]: https://doc.rust-lang.org/std/string/struct.String.html
/// [`LLVMString`]: crate::utils::utils::LLVMString
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub(crate) enum ErrorString {
    /// A normal [`String`]
    ///
    /// [`String`]: https://doc.rust-lang.org/std/string/struct.String.html
    String(String),
    /// A [`LLVMString`]
    ///
    /// [`LLVMString`]: crate::utils::utils::LLVMString
    LLVMString(LLVMString),
}

impl Display for ErrorString {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match self {
            Self::String(ref string) => f.write_str(string),
            Self::LLVMString(ref llvm_string) => f.write_str(&llvm_string.to_string_lossy()),
        }
    }
}

impl<T> From<T> for ErrorString
where
    T: Into<String>,
{
    fn from(string: T) -> Self {
        Self::String(string.into())
    }
}

impl From<LLVMString> for ErrorString {
    fn from(string: LLVMString) -> Self {
        Self::LLVMString(string)
    }
}
