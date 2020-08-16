use crate::llvm::{
    utils::{to_non_nul, LLVMString},
    Error, ErrorKind, Result,
};
use llvm_sys::{
    core::{
        LLVMCreateMemoryBufferWithContentsOfFile, LLVMCreateMemoryBufferWithMemoryRangeCopy,
        LLVMDisposeMemoryBuffer, LLVMGetBufferSize, LLVMGetBufferStart,
    },
    LLVMMemoryBuffer,
};
use std::{
    ffi::CString,
    fmt::{Debug, Formatter, Pointer, Result as FmtResult},
    mem::MaybeUninit,
    path::Path,
    ptr::NonNull,
};

// TODO: Get the buffer's name back from LLVM?

/// An [`LLVMMemoryBuffer`] deallocated using `LLVMDisposeMemoryBuffer`
///
/// [`LLVMMemoryBuffer`]: https://llvm.org/doxygen/classllvm_1_1MemoryBuffer.html
#[repr(transparent)]
pub struct MemoryBuffer {
    /// A pointer to the underlying buffer
    buffer: NonNull<LLVMMemoryBuffer>,
}

// Public interface
impl MemoryBuffer {
    /// Gets the size of the `MemoryBuffer` in bytes
    pub fn len(&self) -> usize {
        // Safety: The buffer pointer is valid and allocated by LLVM
        unsafe { LLVMGetBufferSize(self.as_mut_ptr()) as usize }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Creates a byte slice pointing to the underlying memory  
    pub fn as_slice<'a>(&'a self) -> Result<&'a [u8]> {
        let (ptr, len) = (self.buffer_start()?, self.len());

        // Safety: The pointer and size are valid, as LLVM allocated the memory and supplied the size & ptr
        Ok(unsafe { std::slice::from_raw_parts(ptr.as_ptr(), len) })
    }

    /// Creates a memory buffer from a byte slice with the given name
    pub fn from_slice<S, N>(slice: S, buf_name: N) -> Result<Self>
    where
        S: AsRef<[u8]>,
        N: AsRef<str>,
    {
        let (slice, buf_name) = (slice.as_ref(), buf_name.as_ref());

        // Safety: We're providing LLVM with valid memory and correct lengths
        let raw = unsafe {
            LLVMCreateMemoryBufferWithMemoryRangeCopy(
                slice.as_ptr() as *const i8,
                slice.len(),
                CString::new(buf_name)?.as_ptr(),
            )
        };

        // Safety: The buffer was created by LLVM and ownership is unique to the `MemoryBuffer` instance
        unsafe { Self::from_raw(raw) }
    }

    /// Create a `MemoryBuffer` from the given path
    pub fn from_file<P>(path: P) -> Result<Self>
    where
        P: AsRef<Path>,
    {
        let path = {
            let path = path.as_ref();
            // This check allows for us to have much nicer errors
            if !path.exists() {
                return Err(Error::new(
                    format!("The file {:?} does not exist", path),
                    ErrorKind::FileNotFound,
                ));
            }

            let path = path.to_str().ok_or_else(|| {
                Error::new(
                    format!("The path {:?} has interior null bytes", path),
                    ErrorKind::InvalidStr,
                )
            })?;

            // TODO: Not ideal because of the allocation, but works for now
            CString::new(path)?
        };

        let (mut memory_buf, mut err_message) = (MaybeUninit::zeroed(), MaybeUninit::zeroed());

        let failed = unsafe {
            LLVMCreateMemoryBufferWithContentsOfFile(
                path.as_ptr(),
                memory_buf.as_mut_ptr(),
                err_message.as_mut_ptr(),
            ) == 1
        };

        if failed {
            // Safety: Buffer creation wasn't successful, so the error is initialized
            let err_message = unsafe { err_message.assume_init() };
            // If the pointer is null it will be handled by `LLVMString::from_raw`, but it's not desired or expected behavior
            debug_assert!(!err_message.is_null());

            // Safety: The LLVMString has unique ownership of the message
            Err(Error::new(
                unsafe { LLVMString::from_raw(err_message)? },
                ErrorKind::LLVMError,
            ))
        } else {
            // Safety: Buffer creation was successful, so module is initialized
            let memory_buf = unsafe { memory_buf.assume_init() };
            // If the pointer is null it will be handled by `MemoryBuffer::from_raw`, but it's not desired or expected behavior
            debug_assert!(!memory_buf.is_null());

            // Safety: This buffer has unique ownership of the buffer
            unsafe { MemoryBuffer::from_raw(memory_buf) }
        }
    }
}

// Private interface
impl MemoryBuffer {
    /// Create a `MemoryBuffer` from a pointer to an [`LLVMMemoryBuffer`]. The pointer will
    /// be checked for null
    ///
    /// # Safety
    ///
    /// This instance of `MemoryBuffer` must be the *only* one created for any single buffer,
    /// since disposing of the underlying buffer (via `Drop`) multiple times is UB. The
    /// underlying pointer also *must* have been either allocated by LLVM itself or created
    /// using using LLVM
    ///
    /// [`LLVMMemoryBuffer`]: https://llvm.org/doxygen/classllvm_1_1MemoryBuffer.html
    pub(crate) unsafe fn from_raw(raw: *mut LLVMMemoryBuffer) -> Result<Self> {
        let buffer = to_non_nul(raw, "Failed to create MemoryBuffer")?;

        Ok(Self { buffer })
    }

    /// Gets a raw pointer to the underlying [`LLVMMemoryBuffer`]
    ///
    /// [`LLVMMemoryBuffer`]: https://llvm.org/doxygen/classllvm_1_1MemoryBuffer.html
    pub(crate) fn as_mut_ptr(&self) -> *mut LLVMMemoryBuffer {
        self.buffer.as_ptr()
    }

    /// Gets a pointer to the start of the `MemoryBuffer`, returning an error if the pointer is null
    pub(crate) fn buffer_start(&self) -> Result<NonNull<u8>> {
        // Safety: The buffer was allocated by LLVM and the pointer is non-null
        to_non_nul(
            unsafe { LLVMGetBufferStart(self.as_mut_ptr()) as *mut u8 },
            "Failed to get MemoryBuffer starting point",
        )
    }
}

impl Drop for MemoryBuffer {
    fn drop(&mut self) {
        // Safety: The underlying `LLVMMemoryBuffer` will only be freed once and the pointer to it is non-null
        unsafe { LLVMDisposeMemoryBuffer(self.as_mut_ptr()) }
    }
}

impl Debug for MemoryBuffer {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "{:?}", self.as_slice())
    }
}

impl Pointer for MemoryBuffer {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "{:p}", self.as_mut_ptr())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::{io::Write, ptr};

    #[test]
    fn from_file() {
        let mut file = tempfile::NamedTempFile::new().unwrap();
        write!(file.as_file_mut(), "Hello World!").unwrap();

        let buf = MemoryBuffer::from_file(file.path()).unwrap();

        assert_eq!("Hello World!".len(), buf.len());
        assert_eq!(buf.len(), buf.as_slice().unwrap().len());
        assert_eq!(buf.as_slice().unwrap(), b"Hello World!");
    }

    #[test]
    fn from_slice() {
        let slice = vec![0xFE; 1000];
        let buf = MemoryBuffer::from_slice(slice.as_slice(), "the_buffer").unwrap();

        assert_eq!(slice.len(), buf.len());
        assert_eq!(buf.len(), buf.as_slice().unwrap().len());
        assert_eq!(buf.as_slice().unwrap(), slice.as_slice());

        // Make sure that things still work after the original source isn't around
        drop(slice);

        assert_eq!(1000, buf.len());
        assert_eq!(buf.len(), buf.as_slice().unwrap().len());
        assert_eq!(buf.as_slice().unwrap(), [0xFE; 1000].as_ref());
    }

    #[test]
    fn from_raw_catches_nulls() {
        let buf = unsafe { MemoryBuffer::from_raw(ptr::null_mut()) };
        assert!(buf.is_err());
        assert_eq!(buf.unwrap_err().kind(), ErrorKind::NullPtr);
    }

    #[test]
    fn as_mut_ptr_is_correct() {
        let buf = MemoryBuffer::from_slice(&[0x00], "something").unwrap();
        assert_eq!(buf.as_mut_ptr(), buf.buffer.as_ptr());
    }
}
