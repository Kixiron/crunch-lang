use crate::llvm::{utils::to_non_nul, values::Val, Context, Result};
use llvm_sys::LLVMValue;
use std::ptr::NonNull;

pub trait SealedAnyValue<'ctx>: Sized {
    fn as_val(&self) -> Val<'ctx>;
    fn from_val(value: Val<'ctx>) -> Self;

    unsafe fn from_raw(raw: *mut LLVMValue) -> Result<Self> {
        let non_nul = to_non_nul(
            raw,
            "Received a null pointer from LLVM while trying to create a Value",
        )?;

        Ok(Self::from_non_nul(non_nul))
    }

    unsafe fn from_non_nul(value: NonNull<LLVMValue>) -> Self {
        Self::from_val(Val::from_non_nul(value))
    }

    fn as_ptr(&self) -> *const LLVMValue {
        self.as_val().as_ptr()
    }

    fn as_mut_ptr(&self) -> *mut LLVMValue {
        self.as_val().as_mut_ptr()
    }

    fn as_non_nul(&self) -> NonNull<LLVMValue> {
        self.as_val().as_non_nul()
    }
}
