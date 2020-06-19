use crate::llvm::{types::Type, utils::to_non_nul, Result};
use llvm_sys::LLVMType;
use std::ptr::NonNull;

pub trait SealedAnyType<'ctx>: Sized {
    fn as_ty(&self) -> Type<'ctx>;
    fn from_ty(value: Type<'ctx>) -> Self;

    unsafe fn from_raw(raw: *mut LLVMType) -> Result<Self> {
        let non_nul = to_non_nul(
            raw,
            "Received a null pointer from LLVM while trying to create a Type",
        )?;

        Ok(Self::from_non_nul(non_nul))
    }

    unsafe fn from_non_nul(ty: NonNull<LLVMType>) -> Self {
        Self::from_ty(Type::from_non_nul(ty))
    }

    fn as_ptr(&self) -> *const LLVMType {
        self.as_ty().as_ptr()
    }

    fn as_mut_ptr(&self) -> *mut LLVMType {
        self.as_ty().as_mut_ptr()
    }

    fn as_non_nul(&self) -> NonNull<LLVMType> {
        self.as_ty().as_non_nul()
    }
}
