use crate::llvm::{utils::to_non_nul, Result};
use llvm_sys::core::{LLVMCreateMessage, LLVMDisposeMessage};
use std::{
    borrow::Cow,
    cmp::Ordering,
    ffi::CStr,
    fmt,
    hash::{Hash, Hasher},
    ptr::NonNull,
};

/// A string owned by LLVM, also known as an LLVM Message.
///
/// Uses LLVM to allocate and deallocate the string it contains by using `LLVMCreateMessage`
/// and `LLVMDisposeMessage`
///
// TODO: Could this be `Send`?
#[repr(transparent)]
pub struct LLVMString {
    /// A pointer to the string
    string: NonNull<i8>,
}

// Public interface
impl LLVMString {
    /// Yields a [`&str`] if the underlying message contains valid UTF-8 and an [`Error`] if it does not
    ///
    /// See [`CStr::to_str`] for more information
    ///
    /// [`&str`]: https://doc.rust-lang.org/std/primitive.str.html
    /// [`CStr::to_str`]: https://doc.rust-lang.org/std/ffi/struct.CStr.html#method.to_str
    /// [`Error`]: crate::llvm::Error
    pub fn to_str(&self) -> Result<&str> {
        Ok(self.as_cstr().to_str()?)
    }

    /// Lossily converts a `LLVMString` into a [`Cow`]`<`[`str`]`>`
    ///
    /// See [`CStr::to_string_lossy`] for more information
    ///
    /// [`Cow`]: https://doc.rust-lang.org/std/borrow/enum.Cow.html
    /// [`str`]: https://doc.rust-lang.org/std/primitive.str.html
    /// [`CStr::to_string_lossy`]: https://doc.rust-lang.org/std/ffi/struct.CStr.html#method.to_string_lossy
    pub fn to_string_lossy(&self) -> Cow<'_, str> {
        self.as_cstr().to_string_lossy()
    }

    /// Returns the contents of the string as bytes, not including the null byte
    pub fn as_bytes(&self) -> &[u8] {
        self.as_cstr().to_bytes()
    }

    /// Returns the contents of the string as bytes, including the null byte
    pub fn as_bytes_with_nul(&self) -> &[u8] {
        self.as_cstr().to_bytes_with_nul()
    }

    /// Converts the `LLVMString` into an [`&CStr`]
    ///
    /// [`&CStr`]: https://doc.rust-lang.org/std/ffi/struct.CStr.html
    pub fn as_cstr(&self) -> &CStr {
        // Safety: The pointer is non-null and LLVM gives out valid c-strings
        unsafe { CStr::from_ptr(self.string.as_ptr()) }
    }

    /// Returns the length of the string in bytes
    pub fn len(&self) -> usize {
        self.as_cstr().to_bytes().len()
    }

    /// Returns `true` if the string has a length of zero
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

// Private interface
impl LLVMString {
    /// Creates a new `LLVMString` using `LLVMCreateMessage`
    pub(crate) fn new(string: &CStr) -> Result<Self> {
        let string = to_non_nul(
            unsafe { LLVMCreateMessage(string.as_ptr() as *mut _) },
            format!(
                "Failed to create a new LLVMString from the string {:?}",
                string.to_string_lossy(),
            ),
        )?;

        Ok(Self { string })
    }

    /// Creates a new `LLVMString` from a raw pointer
    ///
    /// # Safety
    ///
    /// This instance of `LLVMString` must be the *only* one created for any single string,
    /// since disposing of the underlying string (via `Drop`) multiple times is UB. The
    /// underlying pointer also *must* have been either allocated by LLVM itself or created
    /// using `LLVMCreateMessage`
    ///
    pub(crate) unsafe fn from_raw(string: *mut i8) -> Result<Self> {
        let string = to_non_nul(string, "Received a null string from LLVM")?;

        Ok(Self { string })
    }
}

impl fmt::Display for LLVMString {
    /// Uses [`LLVMString::to_string_lossy`] for the underlying formatting
    ///
    /// [`LLVMString::to_string_lossy`]: crate::llvm::utils::LLVMString::to_string_lossy
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.to_string_lossy(), f)
    }
}

impl fmt::Debug for LLVMString {
    /// Uses [`LLVMString::to_string_lossy`] for the underlying formatting
    ///
    /// [`LLVMString::to_string_lossy`]: crate::llvm::utils::LLVMString::to_string_lossy
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.to_string_lossy(), f)
    }
}

impl PartialEq for LLVMString {
    /// Compares two `LLVMString`s using [`LLVMString::to_string_lossy`]
    ///
    /// [`LLVMString::to_string_lossy`]: crate::llvm::utils::LLVMString::to_string_lossy
    fn eq(&self, other: &Self) -> bool {
        self.to_string_lossy() == other.to_string_lossy()
    }
}

impl Eq for LLVMString {}

impl PartialOrd for LLVMString {
    /// Orders two `LLVMString`s using [`LLVMString::to_string_lossy`]
    ///
    /// [`LLVMString::to_string_lossy`]: crate::llvm::utils::LLVMString::to_string_lossy
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.to_string_lossy().cmp(&other.to_string_lossy()))
    }
}

impl Ord for LLVMString {
    /// Orders two `LLVMString`s using [`LLVMString::to_string_lossy`]
    ///
    /// [`LLVMString::to_string_lossy`]: crate::llvm::utils::LLVMString::to_string_lossy
    fn cmp(&self, other: &Self) -> Ordering {
        self.to_string_lossy().cmp(&other.to_string_lossy())
    }
}

impl Clone for LLVMString {
    /// Uses LLVM to allocate a new `LLVMString`
    fn clone(&self) -> Self {
        Self::new(self.as_cstr()).expect("Failed to clone LLVMString")
    }
}

impl Hash for LLVMString {
    /// Uses [`LLVMString::to_string_lossy`] for hashing
    ///
    /// [`LLVMString::to_string_lossy`]: crate::llvm::utils::LLVMString::to_string_lossy
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.to_string_lossy().hash(state)
    }
}

impl Drop for LLVMString {
    /// Disposes of the `LLVMString` using LLVM's `DisposeMessage` function
    // TODO: Link for the correct function
    fn drop(&mut self) {
        // Safety: This is the only instance of the message, since cloning creates an entirely new one.
        //         Additionally, the pointer is non-null and was allocated by LLVM either internally or
        //         by using `LLVMCreateMessage`.
        unsafe { LLVMDisposeMessage(self.string.as_ptr()) };
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::llvm::ErrorKind;
    use std::ptr;

    #[test]
    fn from_raw_catches_nulls() {
        let string = unsafe { LLVMString::from_raw(ptr::null_mut()) };
        assert!(string.is_err());
        assert_eq!(string.unwrap_err().kind(), ErrorKind::NullPtr);
    }

    #[test]
    fn to_str() {
        let string =
            LLVMString::new(CStr::from_bytes_with_nul(b"some random string\0").unwrap()).unwrap();
        assert_eq!(string.to_str(), Ok("some random string"));

        let invalid_string = LLVMString::new(
            CStr::from_bytes_with_nul(b"oh no! \xF0\xA4\xAD\xA2\xF0\x90\x80invalid utf8!\0")
                .unwrap(),
        )
        .unwrap();
        assert!(invalid_string.to_str().is_err());
        assert_eq!(
            invalid_string.to_str().unwrap_err().kind(),
            ErrorKind::InvalidStr
        );

        let empty_string = LLVMString::new(CStr::from_bytes_with_nul(b"\0").unwrap()).unwrap();
        assert_eq!(empty_string.to_str(), Ok(""));
    }

    #[test]
    fn to_string_lossy() {
        let string =
            LLVMString::new(CStr::from_bytes_with_nul(b"some random string\0").unwrap()).unwrap();
        assert_eq!(string.to_string_lossy(), "some random string");

        let invalid_string = LLVMString::new(
            CStr::from_bytes_with_nul(b"oh no! \xF0\xA4\xAD\xA2\xF0\x90\x80invalid utf8!\0")
                .unwrap(),
        )
        .unwrap();
        assert_eq!(
            invalid_string.to_string_lossy(),
            String::from_utf8_lossy(b"oh no! \xF0\xA4\xAD\xA2\xF0\x90\x80invalid utf8!"),
        );

        let empty_string = LLVMString::new(CStr::from_bytes_with_nul(b"\0").unwrap()).unwrap();
        assert_eq!(empty_string.to_string_lossy(), "");
    }

    #[test]
    fn as_cstr() {
        let string =
            LLVMString::new(CStr::from_bytes_with_nul(b"this don't matter\0").unwrap()).unwrap();
        assert_eq!(
            string.as_cstr(),
            CStr::from_bytes_with_nul(b"this don't matter\0").unwrap()
        );

        let string = LLVMString::new(CStr::from_bytes_with_nul(b"\0").unwrap()).unwrap();
        assert_eq!(string.as_cstr(), CStr::from_bytes_with_nul(b"\0").unwrap());
    }

    #[test]
    fn as_bytes() {
        let string =
            LLVMString::new(CStr::from_bytes_with_nul(b"this don't matter\0").unwrap()).unwrap();
        assert_eq!(string.as_bytes(), b"this don't matter");

        let string = LLVMString::new(CStr::from_bytes_with_nul(b"\0").unwrap()).unwrap();
        assert_eq!(string.as_bytes(), b"");
    }

    #[test]
    fn as_bytes_with_nul() {
        let string =
            LLVMString::new(CStr::from_bytes_with_nul(b"this don't matter\0").unwrap()).unwrap();
        assert_eq!(string.as_bytes_with_nul(), b"this don't matter\0");

        let string = LLVMString::new(CStr::from_bytes_with_nul(b"\0").unwrap()).unwrap();
        assert_eq!(string.as_bytes_with_nul(), b"\0");
    }

    #[test]
    fn string_is_empty() {
        let string = LLVMString::new(CStr::from_bytes_with_nul(b"\0").unwrap()).unwrap();
        assert!(string.is_empty());

        let string =
            LLVMString::new(CStr::from_bytes_with_nul(b"this don't matter\0").unwrap()).unwrap();
        assert!(!string.is_empty());
    }

    #[test]
    fn length() {
        let string =
            LLVMString::new(CStr::from_bytes_with_nul(b"this don't matter\0").unwrap()).unwrap();
        assert_eq!(string.len(), "this don't matter".len());

        let string = LLVMString::new(CStr::from_bytes_with_nul(b"\0").unwrap()).unwrap();
        assert_eq!(string.len(), 0);
    }

    // TODO: Test trait impls
}
