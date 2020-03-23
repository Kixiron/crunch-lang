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

    pub use codespan_reporting::{
        diagnostic::{Diagnostic, Label, Severity},
        files::SimpleFiles,
    };
    pub use log::{error, info, trace, warn};

    pub type FileId = usize;
    pub type ParserDiagnostic = Diagnostic<FileId>;
    pub type ParseResult<T> = core::result::Result<
        (T, alloc::vec::Vec<ParserDiagnostic>),
        alloc::vec::Vec<ParserDiagnostic>,
    >;

    // TODO: Air-gapped way to make compile errors not reliant on codespan
    // TODO: Custom `Files` implementation for codespan
}

pub mod compile_prelude {
    pub use crate::compile_error::{CompileError, CompileErrorTy, CompileResult};
    pub use log::{error, info, trace, warn};
}

pub mod runtime_prelude {
    pub use crate::runtime_error::{RuntimeError, RuntimeErrorTy, RuntimeResult};
    pub use log::{error, info, trace, warn};
}
