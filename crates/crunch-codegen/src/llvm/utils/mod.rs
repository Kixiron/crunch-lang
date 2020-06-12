//! Various LLVM utilities

use crate::llvm::{Error, ErrorKind, Result};
use std::ptr::NonNull;

mod llvm_string;
mod memory_buffer;

pub use llvm_string::LLVMString;
pub use memory_buffer::MemoryBuffer;

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
