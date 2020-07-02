use crate::llvm::{
    module::Linkage,
    types::{FunctionSig, SealedAnyType, TypeKind},
    utils::CallingConvention,
    values::{AnyValue, BasicBlock, BlockAddress, SealedAnyValue, Val, Value},
    Error, ErrorKind, Result,
};
use llvm_sys::{
    core::{
        LLVMBlockAddress, LLVMCountBasicBlocks, LLVMCountParams, LLVMDeleteFunction,
        LLVMGetFunctionCallConv, LLVMGetLinkage, LLVMGetParams, LLVMSetFunctionCallConv,
        LLVMSetLinkage,
    },
    LLVMValue,
};
use std::{convert::TryFrom, mem::MaybeUninit};

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct FunctionValue<'ctx>(Val<'ctx>);

impl<'ctx> FunctionValue<'ctx> {
    pub fn args(self) -> Result<Vec<Value<'ctx>>> {
        unsafe {
            let mut args: Vec<MaybeUninit<*mut LLVMValue>> =
                vec![MaybeUninit::zeroed(); self.num_args() as usize];
            LLVMGetParams(self.as_mut_ptr(), args.as_mut_ptr() as *mut *mut LLVMValue);

            args.into_iter()
                .map(|arg| Value::from_raw(arg.assume_init()))
                .collect()
        }
    }

    pub fn signature(self) -> Result<FunctionSig<'ctx>> {
        let mut function = self.as_type()?;
        while function.kind() == TypeKind::Pointer {
            function = function.element_type()?;
        }

        Ok(FunctionSig::from_ty(function))
    }

    pub fn num_args(self) -> u32 {
        unsafe { LLVMCountParams(self.as_mut_ptr()) }
    }

    pub fn linkage(self) -> Linkage {
        unsafe { Linkage::from(LLVMGetLinkage(self.as_mut_ptr())) }
    }

    pub fn set_linkage(self, linkage: Linkage) {
        unsafe { LLVMSetLinkage(self.as_mut_ptr(), linkage.into()) };
    }

    pub fn with_linkage(self, linkage: Linkage) -> Self {
        unsafe { LLVMSetLinkage(self.as_mut_ptr(), linkage.into()) };
        self
    }

    pub fn num_blocks(self) -> u32 {
        unsafe { LLVMCountBasicBlocks(self.as_mut_ptr()) }
    }

    // TODO: Getting the address of a block requires the source function, can we
    //       do a sneaky and make this a method on `BasicBlock`?
    pub fn block_address(self, block: BasicBlock<'ctx>) -> Result<BlockAddress<'ctx>> {
        unsafe {
            let address = LLVMBlockAddress(self.as_mut_ptr(), block.as_mut_ptr());

            BlockAddress::from_raw(address)
        }
    }

    pub fn calling_convention(self) -> Result<CallingConvention> {
        CallingConvention::try_from(unsafe { LLVMGetFunctionCallConv(self.as_mut_ptr()) })
    }

    pub fn set_calling_convention(self, calling_convention: CallingConvention) {
        unsafe { LLVMSetFunctionCallConv(self.as_mut_ptr(), calling_convention as u8 as u32) }
    }

    pub fn with_calling_convention(self, calling_convention: CallingConvention) -> Self {
        unsafe { LLVMSetFunctionCallConv(self.as_mut_ptr(), calling_convention as u8 as u32) };
        self
    }

    pub fn delete(self) {
        unsafe { LLVMDeleteFunction(self.as_mut_ptr()) }
    }
}

impl<'ctx> AnyValue<'ctx> for FunctionValue<'ctx> {}

impl<'ctx> SealedAnyValue<'ctx> for FunctionValue<'ctx> {
    fn as_val(&self) -> Val<'ctx> {
        self.0
    }

    fn from_val(value: Val<'ctx>) -> Self {
        Self(value)
    }
}

impl<'ctx> Into<Value<'ctx>> for FunctionValue<'ctx> {
    fn into(self) -> Value<'ctx> {
        Value::Function(self)
    }
}

impl<'ctx> TryFrom<Value<'ctx>> for FunctionValue<'ctx> {
    type Error = Error;

    fn try_from(val: Value<'ctx>) -> Result<Self> {
        if let Value::Function(func) = val {
            Ok(func)
        } else {
            Err(Error::new(
                format!(
                    "A value of type {:?} cannot be made into an Function",
                    val.kind()
                ),
                ErrorKind::MismatchedTypes,
            ))
        }
    }
}
