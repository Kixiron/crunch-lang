use crate::llvm::{
    utils::to_non_nul,
    values::{BasicBlock, Instruction, SealedAnyValue},
    Context, Result,
};
use llvm_sys::{
    core::{
        LLVMCreateBuilderInContext, LLVMDisposeBuilder, LLVMPositionBuilder,
        LLVMPositionBuilderAtEnd, LLVMPositionBuilderBefore,
    },
    LLVMBuilder,
};
use std::{marker::PhantomData, ptr::NonNull};

#[derive(Debug)]
pub struct Builder<'ctx> {
    builder: NonNull<LLVMBuilder>,
    __ctx: PhantomData<&'ctx Context>,
}

impl<'ctx> Builder<'ctx> {
    #[inline]
    pub(crate) fn new(ctx: &'ctx Context) -> Result<Self> {
        // Safety: The new builder is checked for null
        let builder = to_non_nul(
            unsafe { LLVMCreateBuilderInContext(ctx.as_mut_ptr()) },
            "Failed to create LLVM Builder",
        )?;

        Ok(Self {
            builder,
            __ctx: PhantomData,
        })
    }

    #[inline]
    pub(crate) fn move_to(&self, block: &BasicBlock<'ctx>, instruction: Instruction<'ctx>) {
        unsafe {
            LLVMPositionBuilder(
                self.as_mut_ptr(),
                BasicBlock::as_mut_ptr(*block),
                instruction.as_mut_ptr(),
            )
        };
    }

    #[inline]
    pub(crate) fn move_to_end(&self, block: &BasicBlock<'ctx>) {
        unsafe { LLVMPositionBuilderAtEnd(self.as_mut_ptr(), BasicBlock::as_mut_ptr(*block)) };
    }

    #[inline]
    pub(crate) fn move_before(&self, instruction: Instruction<'ctx>) {
        unsafe { LLVMPositionBuilderBefore(self.as_mut_ptr(), instruction.as_mut_ptr()) };
    }

    #[inline]
    pub(crate) const fn as_mut_ptr(&self) -> *mut LLVMBuilder {
        self.builder.as_ptr() as *mut LLVMBuilder
    }
}

impl<'ctx> Drop for Builder<'ctx> {
    #[inline]
    fn drop(&mut self) {
        // Safety: Builder instances are unique and the parent LLVMContext must not be dropped
        unsafe { LLVMDisposeBuilder(self.as_mut_ptr()) }
    }
}
