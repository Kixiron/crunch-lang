/*use crate::{
    llvm::{
        context::Context,
        function::{Function, FunctionArg},
        types::TypeKind,
        value::Value,
        Error, ErrorKind, Result,
    },
    null,
};
use llvm_sys::{
    core::{
        LLVMAppendBasicBlockInContext, LLVMBlockAddress, LLVMBuildAdd, LLVMBuildBr, LLVMBuildGEP,
        LLVMBuildRet, LLVMBuildRetVoid, LLVMBuildSub, LLVMBuildUnreachable, LLVMCountBasicBlocks,
        LLVMCreateBuilderInContext, LLVMDeleteBasicBlock, LLVMDisposeBuilder,
        LLVMGetBasicBlockName, LLVMPositionBuilderAtEnd,
    },
    LLVMBasicBlock, LLVMBuilder, LLVMValue,
};
use std::{
    borrow::Cow,
    cell::Cell,
    ffi::{CStr, CString},
    marker::PhantomData,
    ptr::NonNull,
};

#[derive(Debug)]
pub struct Builder<'a> {
    builder: NonNull<LLVMBuilder>,
    current_block: Cell<Option<NonNull<LLVMBasicBlock>>>,
    __ctx: PhantomData<&'a Context>,
}

impl<'a> Builder<'a> {
    #[inline]
    pub(crate) fn new(ctx: &'a Context) -> Result<Self> {
        // Safety: The new builder is checked for null
        let builder = null!(
            unsafe { LLVMCreateBuilderInContext(ctx.as_mut_ptr()) },
            "Failed to create LLVM Builder",
        )?;

        Ok(Self {
            builder,
            current_block: Cell::new(None),
            __ctx: PhantomData,
        })
    }

    #[inline]
    pub(crate) fn move_to_end(&self, block: &BasicBlock<'a>) {
        if !matches!(self.current_block.get(), Some(current) if current == block.as_non_null()) {
            unsafe { LLVMPositionBuilderAtEnd(self.as_mut_ptr(), block.as_mut_ptr()) };
            self.current_block.set(Some(block.as_non_null()));
        }

        debug_assert_eq!(self.current_block.get(), Some(block.as_non_null()));
    }

    #[inline]
    pub(crate) const fn as_mut_ptr(&self) -> *mut LLVMBuilder {
        self.builder.as_ptr() as *mut LLVMBuilder
    }
}

impl<'a> Drop for Builder<'a> {
    #[inline]
    fn drop(&mut self) {
        // Safety: Builder instances are unique and the parent LLVMContext must not be dropped
        unsafe { LLVMDisposeBuilder(self.as_mut_ptr()) }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[repr(transparent)]
pub struct BlockAddress<'a>(Value<'a>);

impl<'a> BlockAddress<'a> {
    #[inline]
    pub(crate) const fn from_raw(address: NonNull<LLVMValue>) -> Self {
        Self(Value::from_raw(address))
    }
}

#[derive(Debug, Copy, Clone)]
pub struct BasicBlock<'a> {
    block: NonNull<LLVMBasicBlock>,
    builder: &'a FunctionBuilder<'a>,
}

// Public interface
impl<'a> BasicBlock<'a> {
    #[inline]
    pub fn name<'b>(&'b self) -> Option<Cow<'b, str>> {
        let name = NonNull::new(unsafe { LLVMGetBasicBlockName(self.as_mut_ptr()) as *mut i8 });
        name.map(|name| unsafe { CStr::from_ptr(name.as_ptr()).to_string_lossy() })
    }

    #[inline]
    pub fn delete(self) {
        unsafe { LLVMDeleteBasicBlock(self.as_mut_ptr()) };
    }

    #[inline]
    pub fn add(
        &self,
        lhs: impl Into<Value<'a>>,
        rhs: impl Into<Value<'a>>,
        name: &str,
    ) -> Result<Value<'a>> {
        let cname = CString::new(name).expect("Rust strings cannot have null bytes");
        self.builder.move_to_end(self);

        let add = Value::from_raw(null!(
            unsafe {
                LLVMBuildAdd(
                    self.builder.as_mut_ptr(),
                    lhs.into().as_mut_ptr(),
                    rhs.into().as_mut_ptr(),
                    cname.as_ptr(),
                )
            },
            "Failed to create add instruction",
        )?);

        Ok(add)
    }

    pub fn sub(
        &self,
        lhs: impl Into<Value<'a>>,
        rhs: impl Into<Value<'a>>,
        name: &str,
    ) -> Result<Value<'a>> {
        let cname = CString::new(name).expect("Rust strings cannot have null bytes");
        self.builder.move_to_end(self);

        let sub = Value::from_raw(null!(
            unsafe {
                LLVMBuildSub(
                    self.builder.as_mut_ptr(),
                    lhs.into().as_mut_ptr(),
                    rhs.into().as_mut_ptr(),
                    cname.as_ptr(),
                )
            },
            "Failed to create sub instruction",
        )?);

        Ok(sub)
    }

    #[inline]
    pub fn ret(&self, value: impl Into<Value<'a>>) -> Result<Value<'a>> {
        self.builder.move_to_end(self);
        let value = value.into();

        let ret = self.builder.function.return_type().kind();
        if ret != value.get_type()?.kind() {
            return Err(Error::new(
                format!(
                    "You cannot return void from a function that returns {:?}",
                    ret
                ),
                ErrorKind::InvalidRetType,
            ));
        }

        let ret = Value::from_raw(null!(
            unsafe { LLVMBuildRet(self.builder.as_mut_ptr(), value.as_mut_ptr()) },
            "Failed to create return instruction",
        )?);

        debug_assert!(ret.is_return());

        Ok(ret)
    }

    #[inline]
    pub fn ret_void(&self) -> Result<Value<'a>> {
        self.builder.move_to_end(self);

        let ret = self.builder.function.return_type().kind();
        if ret != TypeKind::Void {
            return Err(Error::new(
                format!(
                    "You cannot return void from a function that returns {:?}",
                    ret
                ),
                ErrorKind::InvalidRetType,
            ));
        }

        let ret = Value::from_raw(null!(
            unsafe { LLVMBuildRetVoid(self.builder.as_mut_ptr()) },
            "Failed to create return void instruction",
        )?);

        debug_assert!(ret.is_return());

        Ok(ret)
    }

    #[inline]
    pub fn branch(&self, block: BasicBlock<'a>) -> Result<Value<'a>> {
        self.builder.move_to_end(self);

        let branch = Value::from_raw(null!(
            unsafe { LLVMBuildBr(self.builder.as_mut_ptr(), block.as_mut_ptr()) },
            "Failed to create branch instruction",
        )?);

        debug_assert!(branch.is_branch());

        Ok(branch)
    }

    #[inline]
    pub fn unreachable(&self) -> Result<Value<'a>> {
        self.builder.move_to_end(self);

        let unreachable = Value::from_raw(null!(
            unsafe { LLVMBuildUnreachable(self.builder.as_mut_ptr()) },
            "Failed to create unreachable instruction"
        )?);

        debug_assert!(unreachable.is_unreachable());

        Ok(unreachable)
    }
}

// Private interface
impl<'a> BasicBlock<'a> {
    #[inline]
    pub(crate) const fn from_raw(
        block: NonNull<LLVMBasicBlock>,
        builder: &'a FunctionBuilder<'a>,
    ) -> Self {
        Self { block, builder }
    }

    #[inline]
    pub(crate) const fn as_non_null(&self) -> NonNull<LLVMBasicBlock> {
        self.block
    }

    #[inline]
    pub(crate) const fn as_mut_ptr(&self) -> *mut LLVMBasicBlock {
        self.block.as_ptr() as *mut LLVMBasicBlock
    }
}

impl<'a> PartialEq for BasicBlock<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.block == other.block
    }
}

impl<'a> Eq for BasicBlock<'a> {}

#[derive(Debug)]
pub struct FunctionBuilder<'a> {
    function: Function<'a>,
    builder: Builder<'a>,
    ctx: &'a Context,
}

// Public interface
impl<'a> FunctionBuilder<'a> {
    #[inline]
    pub fn append_block<'b>(&'b self, name: &str) -> Result<BasicBlock<'b>> {
        let name = CString::new(name).expect("Rust strings cannot have null bytes");
        let block = null!(
            unsafe {
                LLVMAppendBasicBlockInContext(
                    self.ctx.as_mut_ptr(),
                    self.function.as_value().as_mut_ptr(),
                    name.as_ptr(),
                )
            },
            "Failed to create LLVM Basic Block",
        )?;

        Ok(BasicBlock::from_raw(block, self))
    }

    #[inline]
    pub fn args(&self) -> &[FunctionArg<'a>] {
        &self.function.args()
    }

    #[inline]
    pub fn block_address(&self, block: &BasicBlock<'a>) -> Result<BlockAddress<'a>> {
        let address = null!(
            unsafe { LLVMBlockAddress(self.function.as_value().as_mut_ptr(), block.as_mut_ptr()) },
            "Failed to get BasicBlock address",
        )?;

        Ok(BlockAddress::from_raw(address))
    }

    #[inline]
    pub fn len(&self) -> usize {
        unsafe { LLVMCountBasicBlocks(self.function.as_value().as_mut_ptr()) as usize }
    }
}

// Private interface
impl<'a> FunctionBuilder<'a> {
    #[inline]
    pub(crate) fn new(ctx: &'a Context, function: Function<'a>) -> Result<Self> {
        let builder = Builder::new(ctx)?;

        Ok(Self {
            function,
            builder,
            ctx,
        })
    }

    #[inline]
    pub(crate) const fn as_mut_ptr(&self) -> *mut LLVMBuilder {
        self.builder.as_mut_ptr()
    }

    #[inline]
    pub(crate) fn finish(self) -> Result<Function<'a>> {
        self.function.verify()?;

        Ok(self.function)
    }

    #[inline]
    pub(crate) fn move_to_end(&self, block: &BasicBlock<'a>) {
        self.builder.move_to_end(block);
    }
}

impl<'a> std::ops::Index<usize> for FunctionBuilder<'a> {
    type Output = FunctionArg<'a>;

    fn index(&self, index: usize) -> &Self::Output {
        &self.args()[index]
    }
}

#[cfg(test)]
mod tests {
    use crate::llvm::LLVM;

    #[test]
    fn create_builder() {
        let llvm = LLVM::new().unwrap();
        let _builder = llvm.create_builder().unwrap();
    }

    #[test]
    fn build_function() {
        let llvm = LLVM::new().unwrap();
        let module = llvm.create_module("entry").unwrap();

        let aiee_32 = llvm.get_type::<i32>().unwrap();
        let sig = module.function_ty(&[aiee_32, aiee_32], aiee_32).unwrap();

        let _function = module
            .build_function("function", &sig, |builder| {
                let entry = builder.append_block("entry")?;
                let add = entry.add(builder[0], builder[1], "sum.1")?;
                entry.ret(add)?;

                let jump_to = builder.append_block("jump_to")?;
                let jump_from = builder.append_block("jump_from")?;
                jump_to.branch(jump_to)?;
                jump_from.branch(jump_from)?;

                Ok(())
            })
            .unwrap();
    }

    #[test]
    fn builder_len() {
        let llvm = LLVM::new().unwrap();
        let module = llvm.create_module("entry").unwrap();

        let aiee_32 = llvm.get_type::<i32>().unwrap();
        let void = llvm.get_type::<()>().unwrap();
        let sig = module.function_ty(&[aiee_32, aiee_32], void).unwrap();

        let _function = module
            .build_function("function", &sig, |builder| {
                assert_eq!(builder.len(), 0);
                let entry = builder.append_block("entry")?;
                assert_eq!(builder.len(), 1);
                entry.ret_void()?;

                let block = builder.append_block("another_block")?;
                assert_eq!(builder.len(), 2);
                block.ret_void()?;

                Ok(())
            })
            .unwrap();
    }

    #[test]
    fn add_numbers() {
        let llvm = LLVM::new().unwrap();
        let module = llvm.create_module("entry").unwrap();

        let int32 = llvm.get_type::<i32>().unwrap();
        let sig = module.function_ty(&[], int32).unwrap();

        let _function = module
            .build_function("addition", &sig, |builder| {
                let entry = builder.append_block("entry")?;

                let (one, two) = (llvm.constant(10i32)?, llvm.constant(20i32)?);
                let add = entry.add(one, two, "one_plus_two")?;
                entry.ret(add)?;

                Ok(())
            })
            .unwrap();
    }

    #[test]
    fn bad_ret_ty() {
        let llvm = LLVM::new().unwrap();
        let module = llvm.create_module("entry").unwrap();

        let int32 = llvm.get_type::<i32>().unwrap();
        let sig = module.function_ty(&[], int32).unwrap();

        let _function = module
            .build_function("addition", &sig, |builder| {
                let entry = builder.append_block("entry")?;

                // TODO: Add other type tests
                assert!(entry.ret_void().is_err());

                entry.ret(llvm.constant(0i32)?)?;

                Ok(())
            })
            .unwrap();
    }
}
*/
