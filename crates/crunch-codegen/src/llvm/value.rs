use crate::{
    llvm::{context::Context, types::Type, Result},
    null,
};
use llvm_sys::{
    core::{
        LLVMConstIntGetSExtValue, LLVMIsAAllocaInst, LLVMIsABinaryOperator, LLVMIsABranchInst,
        LLVMIsACallInst, LLVMIsACastInst, LLVMIsACmpInst, LLVMIsAConstant, LLVMIsAConstantInt,
        LLVMIsAFenceInst, LLVMIsAFunction, LLVMIsAInlineAsm, LLVMIsALoadInst, LLVMIsAReturnInst,
        LLVMIsAStoreInst, LLVMIsATruncInst, LLVMIsAUnreachableInst, LLVMIsAVAArgInst, LLVMIsNull,
        LLVMIsUndef, LLVMPrintValueToString, LLVMSetValueName2, LLVMTypeOf, LLVMValueIsBasicBlock,
    },
    LLVMValue,
};
use std::{
    ffi::{CStr, CString},
    fmt,
    marker::PhantomData,
    ptr::NonNull,
};

#[derive(Copy, Clone, PartialEq, Eq)]
#[repr(transparent)]
pub struct Value<'a>(NonNull<LLVMValue>, PhantomData<&'a Context>);

// Public interface
impl<'a> Value<'a> {
    #[inline]
    pub fn get_type(self) -> Result<Type<'a>> {
        let ty = null!(
            unsafe { LLVMTypeOf(self.as_mut_ptr()) },
            "Failed to get type of value",
        )?;

        Ok(Type::from_raw(ty))
    }

    #[inline]
    pub fn const_int_val(self) -> Option<i64> {
        if self.is_const_int() {
            Some(unsafe { LLVMConstIntGetSExtValue(self.as_mut_ptr()) })
        } else {
            None
        }
    }

    #[inline]
    pub fn set_name(self, name: &str) {
        let cname = CString::new(name).expect("Rust strings cannot have null bytes");

        unsafe { LLVMSetValueName2(self.as_mut_ptr(), cname.as_ptr(), name.len()) };
    }

    #[inline]
    pub fn with_name(self, name: &str) -> Self {
        self.set_name(name);
        self
    }
}

// Private interface
impl<'a> Value<'a> {
    #[inline]
    pub(crate) fn from_raw(raw: *mut LLVMValue) -> Result<Self> {
        let block = to_non_nul(
            raw,
            "Received a null pointer from LLVM while trying to create a Value",
        )?;

        Ok(Self::from_non_nul(block))
    }

    #[inline]
    pub(crate) const fn from_non_nul(value: NonNull<LLVMValue>) -> Self {
        Self(value, PhantomData)
    }

    #[inline]
    pub(crate) const fn as_mut_ptr(self) -> *mut LLVMValue {
        self.0.as_ptr() as *mut LLVMValue
    }
}

impl fmt::Debug for Value<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let value = unsafe { CStr::from_ptr(LLVMPrintValueToString(self.as_mut_ptr())) };

        write!(f, "{}", value.to_string_lossy())
    }
}

impl fmt::Pointer for Value<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:p}", self.0)
    }
}

#[cfg(test)]
mod tests {
    use crate::llvm::LLVM;

    #[test]
    fn create_value() {
        let llvm = LLVM::new().unwrap();
        let module = llvm.create_module("module").unwrap();
        let sig = module
            .function_ty(&[], llvm.get_type::<()>().unwrap())
            .unwrap();
        let _value = module.build_function("function", &sig, |_| Ok(())).unwrap();
    }
}
