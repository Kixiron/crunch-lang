use crate::{llvm::Result, null};
use llvm_sys::{
    core::{LLVMContextCreate, LLVMContextDispose},
    LLVMContext,
};
use std::ptr::NonNull;

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct Context {
    ctx: NonNull<LLVMContext>,
}

impl Context {
    #[inline]
    pub(crate) fn new() -> Result<Self> {
        // Safety: The new context is checked for null
        let ctx = null!(
            unsafe { LLVMContextCreate() },
            "Failed to create LLVM Context"
        )?;

        Ok(Self { ctx })
    }

    #[inline]
    pub(crate) const fn as_mut_ptr(&self) -> *mut LLVMContext {
        self.ctx.as_ptr() as *mut LLVMContext
    }
}

impl Drop for Context {
    #[inline]
    fn drop(&mut self) {
        // Safety: Context instances are unique and all produced items have been dropped
        unsafe { LLVMContextDispose(self.as_mut_ptr()) }
    }
}
