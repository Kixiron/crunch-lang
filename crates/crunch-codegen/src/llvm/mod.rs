mod context;
mod function;
mod module;
mod utils;

use context::Context;
use module::{Builder, Module};
use utils::{Type, Typeable};

// TODO: Internal string caching, this must be done

type Result<T> = std::result::Result<T, Error>;

/// Turns a raw pointer into a `Result<NonNull<T, Error>`
#[doc(hidden)]
#[macro_export]
macro_rules! null {
    ($expr:expr, $msg:expr $(,)?) => {
        ::std::ptr::NonNull::new($expr)
            .ok_or_else(|| $crate::llvm::Error::new($msg, $crate::llvm::ErrorKind::ReceivedNullPtr))
    };
}

#[derive(Debug)]
pub struct LLVM {
    ctx: Context,
}

// Public interface
impl LLVM {
    #[inline]
    pub fn new() -> Result<Self> {
        let ctx = Context::new()?;

        Ok(Self { ctx })
    }

    #[inline]
    pub fn create_builder<'a>(&'a self) -> Result<Builder<'a>> {
        Builder::new(&self.ctx)
    }

    #[inline]
    pub fn create_module<'a, S>(&'a self, name: S) -> Result<Module<'a>>
    where
        S: AsRef<str>,
    {
        Module::new(&self.ctx, name.as_ref())
    }

    #[inline]
    pub fn get_type<'a, T: Typeable>(&'a self) -> Result<Type<'a>> {
        unsafe { <T as Typeable>::create_type(self) }
    }
}

// Private interface
impl LLVM {
    pub(crate) fn context<'a>(&'a self) -> &'a Context {
        &self.ctx
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Error {
    message: String,
    kind: ErrorKind,
}

impl Error {
    #[inline]
    fn new(message: impl Into<String>, kind: ErrorKind) -> Self {
        Self {
            message: message.into(),
            kind,
        }
    }

    #[inline]
    pub fn message(&self) -> &str {
        &self.message
    }

    #[inline]
    pub const fn kind(&self) -> ErrorKind {
        self.kind
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "an error was encountered with LLVM: {:?}, {}",
            self.kind, self.message,
        )
    }
}

impl std::error::Error for Error {}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ErrorKind {
    ReceivedNullPtr,
    InteriorNullBytes,
    MissingType,
    NamelessFunction,
    TooManyArgs,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_llvm() {
        let _llvm = LLVM::new().unwrap();
    }
}
