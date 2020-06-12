use crate::llvm::{
    context::Context,
    utils::to_non_nul,
    values::{sealed::SealedAnyValue, AnyValue, Val, Value},
    Error, ErrorKind, Result,
};
use llvm_sys::{
    core::{
        LLVMBasicBlockAsValue, LLVMDeleteBasicBlock, LLVMGetBasicBlockName,
        LLVMGetBasicBlockParent, LLVMMoveBasicBlockAfter, LLVMMoveBasicBlockBefore,
        LLVMValueAsBasicBlock,
    },
    LLVMBasicBlock,
};
use std::{
    borrow::Cow, convert::TryFrom, ffi::CStr, hash::Hash, marker::PhantomData, ptr::NonNull,
};

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct BasicBlock<'ctx> {
    block: NonNull<LLVMBasicBlock>,
    __ctx: PhantomData<&'ctx Context>,
}

impl<'ctx> BasicBlock<'ctx> {
    #[inline]
    pub fn name<'b>(&'b self) -> Option<Cow<'b, str>> {
        unsafe {
            let name = NonNull::new(LLVMGetBasicBlockName((*self).as_mut_ptr()) as *mut i8);

            name.map(|name| CStr::from_ptr(name.as_ptr()).to_string_lossy())
        }
    }

    pub fn parent(self) -> Option<Self> {
        unsafe {
            Val::from_raw(LLVMGetBasicBlockParent(self.as_mut_ptr()))
                .and_then(|val| Self::from_val(val))
                .ok()
        }
    }

    pub fn move_before(self, other: impl AsRef<Self>) {
        unsafe {
            LLVMMoveBasicBlockBefore(self.as_mut_ptr(), BasicBlock::as_mut_ptr(*other.as_ref()))
        }
    }

    pub fn move_after(self, other: impl AsRef<Self>) {
        unsafe {
            LLVMMoveBasicBlockAfter(self.as_mut_ptr(), BasicBlock::as_mut_ptr(*other.as_ref()))
        }
    }

    pub fn delete(self) {
        unsafe { LLVMDeleteBasicBlock(self.as_mut_ptr()) };
    }
}

impl<'ctx> BasicBlock<'ctx> {
    pub(crate) fn from_val(value: Val<'ctx>) -> Result<Self> {
        let block = unsafe {
            to_non_nul(
                LLVMValueAsBasicBlock(value.as_mut_ptr()),
                "Failed to turn Val into BasicBlock",
            )
        }?;

        // Safety: The pointer is actually to an `LLVMBasicBlock`
        Ok(unsafe { BasicBlock::from_non_nul(block) })
    }

    pub(crate) unsafe fn from_raw(raw: *mut LLVMBasicBlock) -> Result<Self> {
        let block = to_non_nul(
            raw,
            "Received a null pointer from LLVM while trying to create a Value",
        )?;

        Ok(Self::from_non_nul(block))
    }

    pub(crate) const unsafe fn from_non_nul(block: NonNull<LLVMBasicBlock>) -> Self {
        Self {
            block,
            __ctx: PhantomData,
        }
    }

    pub(crate) const fn as_mut_ptr(self) -> *mut LLVMBasicBlock {
        self.block.as_ptr()
    }
}

impl<'ctx> AnyValue<'ctx> for BasicBlock<'ctx> {
    fn get_name_raw<'a>(&'a self) -> Option<&'a [u8]> {
        let string = unsafe {
            let ptr =
                NonNull::new(LLVMGetBasicBlockName(BasicBlock::as_mut_ptr(*self)) as *mut i8)?;

            CStr::from_ptr(ptr.as_ptr())
        };

        Some(string.to_bytes())
    }

    // TODO: Setting the name of a block via its value may not be possible, test it
}

impl<'ctx> SealedAnyValue<'ctx> for BasicBlock<'ctx> {
    /// This method will panic if LLVM returns a null pointer
    fn as_val(&self) -> Val<'ctx> {
        unsafe {
            Val::from_raw(LLVMBasicBlockAsValue(BasicBlock::as_mut_ptr(*self)))
                .expect("Failed to convert a BasicBlock into a Val")
        }
    }

    /// This method will panic if the value it's called on is not a `BasicBlock`
    fn from_val(value: Val<'ctx>) -> Self {
        BasicBlock::from_val(value).unwrap_or_else(|_| {
            panic!(
                "Cannot turn a Val of kind {:?} into a BasicBlock",
                value.kind()
            )
        })
    }
}

impl<'ctx> Into<Value<'ctx>> for BasicBlock<'ctx> {
    fn into(self) -> Value<'ctx> {
        Value::BasicBlock(self)
    }
}

impl<'ctx> TryFrom<Value<'ctx>> for BasicBlock<'ctx> {
    type Error = Error;

    fn try_from(val: Value<'ctx>) -> Result<Self> {
        if let Value::BasicBlock(block) = val {
            Ok(block)
        } else {
            Err(Error::new(
                format!(
                    "A value of type {:?} cannot be made into an BasicBlock",
                    val.kind()
                ),
                ErrorKind::MismatchedTypes,
            ))
        }
    }
}
