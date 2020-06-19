use crate::llvm::{
    module::Module,
    utils::{to_non_nul, LLVMString, MemoryBuffer},
    Error, ErrorKind, Result,
};
use llvm_sys::{
    core::{LLVMContextCreate, LLVMContextDispose, LLVMModuleCreateWithNameInContext},
    ir_reader::LLVMParseIRInContext,
    LLVMContext,
};
use std::{
    ffi::CString,
    mem::{ManuallyDrop, MaybeUninit},
    ptr::NonNull,
};

// TODO: Diagnostic handler
//       https://llvm.org/docs/doxygen/group__LLVMCCoreContext.html#gad027fad059f2fc3476d5a8464e39c9ef
//       https://llvm.org/docs/doxygen/group__LLVMCCoreContext.html#gacbfc704565962bf71eaaa549a9be570f

/// A container for all LLVM entities
///
/// `Context` is not thread safe and cannot be shared across threads, but [multiple `Context`s can
/// exist and execute simultaneously]
///
/// [multiple `Context`s can exist and execute simultaneously]: https://llvm.org/docs/doxygen/group__LLVMCCoreContext.html#details
#[derive(Debug, PartialEq, Eq)]
#[repr(transparent)]
pub struct Context {
    /// The pointer to the context
    ctx: NonNull<LLVMContext>,
}

// Public interface
impl Context {
    /// Creates a new `Context` using [`LLVMContextCreate`]
    ///
    /// [`LLVMContextCreate`]: https://llvm.org/docs/doxygen/group__LLVMCCoreContext.html#gaac4f39a2d0b9735e64ac7681ab543b4c
    #[inline]
    pub fn new() -> Result<Self> {
        // Safety: The new context is checked for null and LLVM provides valid pointers
        let ctx = to_non_nul(
            unsafe { LLVMContextCreate() },
            "Failed to create LLVM Context",
        )?;

        Ok(Self { ctx })
    }

    pub fn module<'ctx, N>(&'ctx self, name: N) -> Result<Module<'ctx>>
    where
        N: AsRef<str>,
    {
        let name = CString::new(name.as_ref())?;

        unsafe {
            let module = LLVMModuleCreateWithNameInContext(name.as_ptr(), self.as_mut_ptr());

            Module::from_raw(self, module)
        }
    }

    // FIXME: `LLVMParseIRInContext` takes ownership of the buffer, but there's not really a better way to express that other than with the `ManuallyDrop`
    #[inline]
    pub fn module_from_ir<'ctx>(&'ctx self, memory_buf: MemoryBuffer) -> Result<Module<'ctx>> {
        let (mut module, mut err_message) = (MaybeUninit::zeroed(), MaybeUninit::zeroed());
        let memory_buf = ManuallyDrop::new(memory_buf);

        let succeeded = unsafe {
            LLVMParseIRInContext(
                self.as_mut_ptr(),
                memory_buf.as_mut_ptr(),
                module.as_mut_ptr(),
                err_message.as_mut_ptr(),
            ) == 0
        };

        if succeeded {
            // Safety: The parse was successful, so module is initialized
            let module = unsafe { module.assume_init() };
            // If the pointer is null it will be handled by `Module::from_raw`, but it's not desired or expected behavior
            debug_assert!(!module.is_null());

            // Safety: The parse succeeded, and so the module is valid. This instance of `Module` will also have unique ownership
            //         over the buffer
            Ok(unsafe { Module::from_raw(self, module)? })
        } else {
            // Safety: The parse wasn't successful, so the error is initialized
            let err_message = unsafe { err_message.assume_init() };
            // If the pointer is null it will be handled by `LLVMString::from_raw`, but it's not desired or expected behavior
            debug_assert!(!err_message.is_null());

            // Safety: The memory was allocated by LLVM and is therefore able to be put into an `LLVMString`,
            //         and there was an error so the pointer was initalized
            let err_message = unsafe { LLVMString::from_raw(err_message)? };

            Err(Error::new(err_message, ErrorKind::InvalidIR))
        }
    }
}

// Private interface
impl Context {
    /// Retrieves a raw pointer to the underlying `LLVMContext`
    #[inline]
    pub(crate) const fn as_mut_ptr(&self) -> *mut LLVMContext {
        self.ctx.as_ptr() as *mut LLVMContext
    }
}

/// Creates a default `Context`, panicking if an error occurs
impl Default for Context {
    #[inline]
    fn default() -> Self {
        Self::new().expect("Failed to create context")
    }
}

/// Destroys the `Context` using [`LLVMContextDispose`]
///
/// [`LLVMContextDispose`]: https://llvm.org/docs/doxygen/group__LLVMCCoreContext.html#ga9cf8b0fb4a546d4cdb6f64b8055f5f57
impl Drop for Context {
    #[inline]
    fn drop(&mut self) {
        // Safety: Context instances are unique and all produced items have been dropped
        unsafe { LLVMContextDispose(self.as_mut_ptr()) }
    }
}
