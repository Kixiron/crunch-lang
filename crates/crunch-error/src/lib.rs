#![no_std]

extern crate alloc;

pub extern crate codespan;
pub extern crate codespan_reporting;
pub extern crate log;

pub mod compile_error;
pub mod runtime_error;

pub mod parse_prelude {
    pub extern crate codespan;
    pub extern crate codespan_reporting;

    pub use codespan_reporting::diagnostic::{Diagnostic, Label, Severity};
    pub use log::{error, info, trace, warn};

    pub type ParseResult<T> = core::result::Result<T, Diagnostic>;

    // TODO: Air-gapped way to make compile errors not reliant on codespan
}

pub mod compile_prelude {
    pub use crate::compile_error::{CompileError, CompileErrorTy, CompileResult};
    pub use log::{error, info, trace, warn};
}

pub mod runtime_prelude {
    pub use crate::runtime_error::{RuntimeError, RuntimeErrorTy, RuntimeResult};
    pub use log::{error, info, trace, warn};
}
