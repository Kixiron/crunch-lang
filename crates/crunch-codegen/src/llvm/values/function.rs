use crate::llvm::{
    module::Linkage,
    values::{AnyValue, BasicBlock, BlockAddress, SealedAnyValue, Val, Value},
    Error, ErrorKind, Result,
};
use llvm_sys::core::{
    LLVMBlockAddress, LLVMCountBasicBlocks, LLVMDeleteFunction, LLVMGetLinkage, LLVMSetLinkage,
};
use std::convert::TryFrom;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct Function<'ctx>(Val<'ctx>);

impl<'ctx> Function<'ctx> {
    pub fn linkage(self) -> Linkage {
        unsafe { Linkage::from(LLVMGetLinkage(self.as_mut_ptr())) }
    }

    pub fn set_linkage(self, linkage: Linkage) {
        unsafe { LLVMSetLinkage(self.as_mut_ptr(), linkage.into()) };
    }

    pub fn block_len(self) -> usize {
        unsafe { LLVMCountBasicBlocks(self.as_mut_ptr()) as usize }
    }

    // TODO: Getting the address of a block requires the source function, can we
    //       do a sneaky and make this a method on `BasicBlock`?
    pub fn block_address(&self, block: BasicBlock<'ctx>) -> Result<BlockAddress<'ctx>> {
        unsafe {
            let address = LLVMBlockAddress(self.as_mut_ptr(), block.as_mut_ptr());

            BlockAddress::from_raw(address)
        }
    }

    pub fn delete(self) {
        unsafe { LLVMDeleteFunction(self.as_mut_ptr()) }
    }
}

impl<'ctx> AnyValue<'ctx> for Function<'ctx> {}

impl<'ctx> SealedAnyValue<'ctx> for Function<'ctx> {
    fn as_val(&self) -> Val<'ctx> {
        self.0
    }

    fn from_val(value: Val<'ctx>) -> Self {
        Self(value)
    }
}

impl<'ctx> Into<Value<'ctx>> for Function<'ctx> {
    fn into(self) -> Value<'ctx> {
        Value::Function(self)
    }
}

impl<'ctx> TryFrom<Value<'ctx>> for Function<'ctx> {
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
