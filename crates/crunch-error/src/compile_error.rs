use alloc::string::String;
use core::fmt;

pub type CompileResult<T> = core::result::Result<T, CompileError>;

#[derive(Debug, Clone, Eq)]
pub struct CompileError {
    ty: CompileErrorTy,
    message: String,
}

impl CompileError {
    pub fn new(ty: CompileErrorTy, message: impl Into<String>) -> Self {
        Self {
            ty,
            message: message.into(),
        }
    }

    pub fn message(&self) -> &str {
        &self.message
    }

    pub fn ty(&self) -> CompileErrorTy {
        self.ty
    }
}

impl PartialEq for CompileError {
    fn eq(&self, other: &Self) -> bool {
        self.ty == other.ty
    }
}

// TODO: Make pretty
impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "[Crunch Compilation Error: {:?}] {}",
            self.ty, self.message
        )
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum CompileErrorTy {
    MissingMain,
    MissingSymbol,
    CompilationError,
    OverflowedRegisters,
    FileError,
    InvalidBytecode,
}
