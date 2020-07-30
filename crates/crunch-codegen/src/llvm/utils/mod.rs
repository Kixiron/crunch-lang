//! Various LLVM utilities

use crate::llvm::{Error, ErrorKind, Result};
use std::ptr::NonNull;

mod address_space;
mod calling_convention;
mod dll_storage_class;
mod int_operand;
mod integer_opts;
mod llvm_string;
mod memory_buffer;
mod thread_local_mode;

pub use address_space::AddressSpace;
pub use calling_convention::CallingConvention;
pub use dll_storage_class::DLLStorageClass;
pub use int_operand::IntOperand;
pub use integer_opts::{DivideKind, Wrapping};
pub use llvm_string::LLVMString;
pub use memory_buffer::MemoryBuffer;
pub(crate) use sealed::Sealed;
pub use thread_local_mode::ThreadLocalMode;

/// Empty string, to be used where LLVM expects an instruction name, indicating
/// that the instruction is to be left unnamed (i.e. numbered, in textual IR).
// TODO: Use CStr::from_bytes_with_nul_unchecked once it's const-stable
pub(crate) const EMPTY_CSTR: *const i8 = b"\0".as_ptr() as *const i8;

mod sealed {
    pub trait Sealed {}
}

/// Converts a raw pointer into a [`Result`]`<`[`NonNull<T>`]`, `[`Error`]`>`, using `message`
/// as the error message
///
/// [`Result`]: https://doc.rust-lang.org/std/result/enum.Result.html
/// [`NonNull<T>`]: https://doc.rust-lang.org/std/ptr/struct.NonNull.html
/// [`Error`]: crate::llvm::Error
pub(crate) fn to_non_nul<T, S>(raw: *mut T, message: S) -> Result<NonNull<T>>
where
    S: Into<String>,
{
    NonNull::new(raw).ok_or_else(|| Error::new(message, ErrorKind::NullPtr))
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ptr;

    #[test]
    fn to_non_nul_catches_nulls() {
        let ptr = to_non_nul::<u8, _>(ptr::null_mut(), "This is my error message");

        assert!(ptr.is_err());
        assert_eq!(
            ptr.unwrap_err(),
            Error::new("This is my error message", ErrorKind::NullPtr),
        );
    }

    #[test]
    fn to_non_nul_allows_non_nulls() {
        let mut you_ate = 8u8;
        let ptr = to_non_nul::<u8, _>(&mut you_ate, "This is my error message");

        assert!(ptr.is_ok());
        unsafe {
            assert_eq!(*ptr.unwrap().as_ref(), you_ate);
        }
    }
}
