mod context;
mod error;
pub mod module;
pub mod target_machine;
pub mod types;
pub mod utils;
pub mod values;

pub use context::Context;
pub use error::{Error, ErrorKind, Result};

/*
pub mod builder;
pub mod context;
pub mod function;
pub mod module;
pub mod value;

use builder::Builder;
use context::Context;
use module::Module;
use value::Value;

use std::ffi::CStr;

// TODO: Internal string caching, this must be done

pub type Result<T> = std::result::Result<T, Error>;

/// Turns a raw pointer into a `Result<NonNull<T, Error>`
#[doc(hidden)]
#[macro_export]
macro_rules! null {
    ($expr:expr, $msg:expr $(,)?) => {
        ::std::ptr::NonNull::new($expr)
            .ok_or_else(|| $crate::llvm::Error::new($msg, $crate::llvm::ErrorKind::NullPtr))
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
        Self::set_fatal_error_handler();

        Ok(Self { ctx })
    }

    #[inline]
    pub fn without_handler() -> Result<Self> {
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
        unsafe { <T as Typeable>::create_type(&self.ctx) }
    }

    #[inline]
    pub fn constant<'a, T: Constant + Typeable>(&'a self, constant: T) -> Result<Value<'a>> {
        constant.create(&self.ctx)
    }

    // TODO: This can be partially replaced by const generics
    #[inline]
    pub fn array_type<'a, T: Typeable>(&'a self, len: u32) -> Result<Type<'a>> {
        use llvm_sys::core::LLVMArrayType;

        unsafe {
            let elm = <T as Typeable>::create_type(&self.ctx)?;

            let array = Type::from_raw(null!(
                LLVMArrayType(elm.as_mut_ptr(), len),
                "Failed to create array type",
            )?);

            debug_assert!(array.is_array());
            debug_assert_eq!(array.array_len(), Some(len));

            Ok(array)
        }
    }
}

// Private interface
impl LLVM {
    pub(crate) fn set_fatal_error_handler() {
        extern "C" fn error_handler(reason: *const i8) {
            println!(
                "LLVM encountered a fatal error: {}",
                unsafe { CStr::from_ptr(reason) }.to_string_lossy(),
            );
        }

        // Unset the current error handler before inserting our own
        unsafe {
            llvm_sys::error_handling::LLVMResetFatalErrorHandler();
            llvm_sys::error_handling::LLVMInstallFatalErrorHandler(Some(error_handler))
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_llvm() {
        let _llvm = LLVM::new().unwrap();
    }

    #[test]
    fn create_array_ty() {
        let llvm = LLVM::new().unwrap();
        let array = llvm.array_type::<i32>(10).unwrap();

        assert!(array.is_array());
        assert_eq!(array.array_len(), Some(10));
    }

    #[test]
    fn create_i32() {
        let llvm = LLVM::new().unwrap();
        let int32 = llvm.constant(100i32).unwrap();

        assert!(int32.is_constant());
        assert!(int32.is_const_int());
        assert_eq!(Some(100), int32.const_int_val());
    }
}
*/
