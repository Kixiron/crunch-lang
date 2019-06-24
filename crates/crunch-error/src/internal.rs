use failure::{Backtrace, Context, Fail};
use std::fmt;

pub type CResult<T> = Result<T, Error>;

#[derive(Debug)]
pub struct Error {
    code: InternalCode,
    inner: Context<InternalError>,
}

impl Fail for Error {
    fn cause(&self) -> Option<&Fail> {
        self.inner.cause()
    }

    fn backtrace(&self) -> Option<&Backtrace> {
        self.inner.backtrace()
    }
}

impl From<std::io::Error> for Error {
    fn from(error: std::io::Error) -> Self {
        Self {
            code: InternalCode::E001,
            inner: Context::new(InternalError::Io(error)),
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.inner, f)
    }
}

#[derive(Fail, Debug)]
pub enum InternalError {
    #[fail(display = "IO Error: {:?}", _0)]
    Io(#[fail(cause)] std::io::Error),
}

impl PartialEq for InternalError {
    fn eq(&self, other: &Self) -> bool {
        std::mem::discriminant(self) == std::mem::discriminant(other)
    }

    fn ne(&self, other: &Self) -> bool {
        std::mem::discriminant(self) != std::mem::discriminant(other)
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum InternalCode {
    E001,
}
