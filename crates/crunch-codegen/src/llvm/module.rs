use crate::{
    llvm::{
        context::Context,
        function::{Function, FunctionBuilder, Signature},
        utils::{Type, Value},
        Result,
    },
    null,
};
use llvm_sys::{
    core::{
        LLVMAppendBasicBlockInContext, LLVMBuildAdd, LLVMBuildBr, LLVMBuildRet, LLVMBuildRetVoid,
        LLVMBuildUnreachable, LLVMCloneModule, LLVMCreateBuilderInContext, LLVMDeleteBasicBlock,
        LLVMDisposeBuilder, LLVMDisposeModule, LLVMGetBasicBlockName,
        LLVMModuleCreateWithNameInContext, LLVMPositionBuilderAtEnd, LLVMPrintModuleToString,
    },
    LLVMBasicBlock, LLVMBuilder, LLVMModule, LLVMValue,
};
use std::{
    borrow::Cow,
    ffi::{CStr, CString},
    fmt,
    marker::PhantomData,
    ptr::NonNull,
};

pub struct Module<'a> {
    module: NonNull<LLVMModule>,
    ctx: &'a Context,
}

/// Public methods
impl<'a> Module<'a> {
    #[inline]
    pub fn add_function_type(
        &self,
        return_type: Type<'a>,
        func_args: &[Type<'a>],
        variadic: bool,
    ) -> Result<Signature<'a>> {
        Signature::new(return_type, func_args, variadic)
    }

    #[inline]
    pub fn build_function<S, F>(
        &self,
        name: S,
        signature: &Signature<'a>,
        build_function: F,
    ) -> Result<Function<'a>>
    where
        S: Into<String>,
        F: FnOnce(&mut FunctionBuilder<'a>) -> Result<()>,
    {
        let name = CString::new(name.into()).expect("Rust strings cannot have null bytes");
        let function = Function::new(self, &name, signature)?;

        let mut builder = FunctionBuilder::new(self.ctx, function)?;
        build_function(&mut builder)?;

        builder.finish()
    }
}

/// Private methods
impl<'a> Module<'a> {
    #[inline]
    pub(crate) fn new(ctx: &'a Context, name: &str) -> Result<Self> {
        let name = CString::new(name).expect("Rust strings cannot have null bytes");

        // Safety: The new module is checked for null
        let module = null!(
            unsafe { LLVMModuleCreateWithNameInContext(name.as_ptr(), ctx.as_mut_ptr()) },
            "Failed to create LLVM Module",
        )?;

        Ok(Self { module, ctx })
    }

    #[inline]
    pub(crate) const unsafe fn from_raw(ctx: &'a Context, module: NonNull<LLVMModule>) -> Self {
        Self { module, ctx }
    }

    #[inline]
    pub(crate) const fn as_mut_ptr(&self) -> *mut LLVMModule {
        self.module.as_ptr() as *mut LLVMModule
    }
}

impl<'a> Clone for Module<'a> {
    #[inline]
    fn clone(&self) -> Self {
        // Safety: The new module is checked for null
        let module = unsafe {
            null!(
                LLVMCloneModule(self.as_mut_ptr()),
                "Failed to create LLVM Module",
            )
            .expect("Failed to clone Module")
        };
        debug_assert_ne!(self.module, module);

        // Safety: The new module is unique and will dispose of itself
        unsafe { Module::from_raw(self.ctx, module) }
    }
}

impl<'a> Drop for Module<'a> {
    #[inline]
    fn drop(&mut self) {
        // Safety: Module instances are unique and the parent LLVMContext must not be dropped
        unsafe { LLVMDisposeModule(self.as_mut_ptr()) }
    }
}

impl fmt::Debug for Module<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let string = unsafe {
            CStr::from_ptr(
                null!(
                    LLVMPrintModuleToString(self.as_mut_ptr()),
                    "Failed to print module to string",
                )
                .expect("Failed to print module to string")
                .as_ptr(),
            )
        };

        write!(f, "{}", CStr::to_string_lossy(string))
    }
}

#[derive(Debug)]
#[repr(transparent)]
pub struct Builder<'a> {
    builder: NonNull<LLVMBuilder>,
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
            __ctx: PhantomData,
        })
    }

    #[inline]
    pub(crate) fn new_block<'b>(
        &'b self,
        ctx: &'a Context,
        function: &Function<'a>,
        name: &CStr,
    ) -> Result<BasicBlock<'b>> {
        let block = null!(
            unsafe {
                LLVMAppendBasicBlockInContext(
                    ctx.as_mut_ptr(),
                    function.as_value().as_mut_ptr(),
                    name.as_ptr(),
                )
            },
            "Failed to create LLVM Basic Block",
        )?;

        Ok(BasicBlock::from_raw(block, self))
    }

    #[inline]
    pub(crate) fn move_to_end(&self, block: &BasicBlock<'a>) {
        unsafe { LLVMPositionBuilderAtEnd(self.as_mut_ptr(), block.as_mut_ptr()) }
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
    builder: &'a Builder<'a>,
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
    pub fn add(&self, lhs: Value<'a>, rhs: Value<'a>, name: &str) -> Result<Value<'a>> {
        let cname = CString::new(name).expect("Rust strings cannot have null bytes");
        self.builder.move_to_end(self);

        let add = null!(
            unsafe {
                LLVMBuildAdd(
                    self.builder.as_mut_ptr(),
                    lhs.as_mut_ptr(),
                    rhs.as_mut_ptr(),
                    cname.as_ptr(),
                )
            },
            "Failed to create add instruction",
        )?;

        Ok(Value::from_raw(add))
    }

    #[inline]
    pub fn ret(&self, value: Value<'a>) -> Result<Value<'a>> {
        self.builder.move_to_end(self);

        let ret = null!(
            unsafe { LLVMBuildRet(self.builder.as_mut_ptr(), value.as_mut_ptr()) },
            "Failed to create return instruction",
        )?;

        Ok(Value::from_raw(ret))
    }

    #[inline]
    pub fn ret_void(&self) -> Result<Value<'a>> {
        self.builder.move_to_end(self);

        let ret = null!(
            unsafe { LLVMBuildRetVoid(self.builder.as_mut_ptr()) },
            "Failed to create return void instruction",
        )?;

        Ok(Value::from_raw(ret))
    }

    #[inline]
    pub fn branch(&self, block: BasicBlock<'a>) -> Result<Value<'a>> {
        self.builder.move_to_end(self);

        let branch = null!(
            unsafe { LLVMBuildBr(self.builder.as_mut_ptr(), block.as_mut_ptr()) },
            "Failed to create branch instruction",
        )?;

        Ok(Value::from_raw(branch))
    }

    #[inline]
    pub fn unreachable(&self) -> Result<Value<'a>> {
        self.builder.move_to_end(self);

        let unreachable = null!(
            unsafe { LLVMBuildUnreachable(self.builder.as_mut_ptr()) },
            "Failed to create unreachable instruction"
        )?;

        Ok(Value::from_raw(unreachable))
    }
}

// Private interface
impl<'a> BasicBlock<'a> {
    #[inline]
    pub(crate) const fn from_raw(block: NonNull<LLVMBasicBlock>, builder: &'a Builder<'a>) -> Self {
        Self { block, builder }
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

#[cfg(test)]
mod tests {
    use crate::llvm::LLVM;

    #[test]
    fn create_module() {
        let llvm = LLVM::new().unwrap();
        let _module = llvm.create_module("entry").unwrap();
    }

    #[test]
    fn clone_module() {
        let llvm = LLVM::new().unwrap();
        let _module = llvm.create_module("entry").unwrap();
        let _builder = llvm.create_builder().unwrap();
    }

    #[test]
    fn clone_and_use_module() {
        let llvm = LLVM::new().unwrap();

        // Create the module and make a function for it
        let module_one = llvm.create_module("entry").unwrap();
        let aiee_thirty_two = llvm.get_type::<i32>().unwrap();
        let sig = module_one
            .add_function_type(aiee_thirty_two, &[aiee_thirty_two, aiee_thirty_two], false)
            .unwrap();
        let _function = module_one
            .build_function("function_one", &sig, |_| Ok(()))
            .unwrap();

        // Clone the module
        let module_two = module_one.clone();

        // Create a function for the old and newly cloned modules
        let sig_one = module_one
            .add_function_type(llvm.get_type::<()>().unwrap(), &[], false)
            .unwrap();
        let _function_one = module_one
            .build_function("function_two", &sig_one, |_| Ok(()))
            .unwrap();

        let sig_two = module_two
            .add_function_type(llvm.get_type::<()>().unwrap(), &[], false)
            .unwrap();
        let _function_two = module_two
            .build_function("function_two", &sig_two, |_| Ok(()))
            .unwrap();
    }

    #[test]
    fn create_builder() {
        let llvm = LLVM::new().unwrap();
        let _builder = llvm.create_builder().unwrap();
    }

    #[test]
    fn create_module_and_builder() {
        let llvm = LLVM::new().unwrap();
        let _module = llvm.create_module("entry").unwrap();
        let _builder = llvm.create_builder().unwrap();
    }
}
