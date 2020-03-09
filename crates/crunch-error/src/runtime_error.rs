use alloc::string::String;
use core::fmt;

pub type RuntimeResult<T> = core::result::Result<T, RuntimeError>;

/// A Crunch Runtime Error
// TODO: Make this more detailed
#[derive(Debug, Clone, Eq)]
pub struct RuntimeError {
    ty: RuntimeErrorTy,
    message: String,
}

impl RuntimeError {
    pub fn new(ty: RuntimeErrorTy, message: impl Into<String>) -> Self {
        Self {
            ty,
            message: message.into(),
        }
    }

    /// The error message
    pub fn message(&self) -> &str {
        &self.message
    }

    /// The type of error
    pub fn ty(&self) -> RuntimeErrorTy {
        self.ty
    }
}

impl PartialEq for RuntimeError {
    fn eq(&self, other: &Self) -> bool {
        self.ty == other.ty
    }
}

// TODO: Make pretty
impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[Crunch Runtime Error: {:?}] {}", self.ty, self.message)
    }
}

/// The type of [`RuntimeError`] that occurred
///
/// [`RuntimeError`]: crate::RuntimeError
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RuntimeErrorTy {
    GcError,
    DivideByZero,
    IncompatibleTypes,
    NullVar,
    IllegalInstruction,
    InvalidJump,
    MissingValue,
    MissingString,
    InvalidString,
    BytecodeError,
    MissingFile,
    InvalidInt,
    StdoutError,
    IntegerOverflow,
    JitError,
    EmptyStack,
    ReadOnlyRegister,
}
