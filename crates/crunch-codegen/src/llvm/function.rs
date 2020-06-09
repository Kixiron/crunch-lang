use crate::{
    llvm::{
        context::Context,
        module::{BasicBlock, BlockAddress, Builder, Module},
        utils::{Type, Value},
        Error, ErrorKind, Result,
    },
    null,
};
use llvm_sys::{
    core::{
        LLVMAddFunction, LLVMBlockAddress, LLVMCountBasicBlocks, LLVMCountParams, LLVMFunctionType,
        LLVMGetParamTypes, LLVMGetParams,
    },
    LLVMType, LLVMValue,
};
use std::{
    convert::TryInto,
    ffi::{CStr, CString},
    marker::PhantomData,
};

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

        let block = self.builder.new_block(self.ctx, &self.function, &name)?;

        Ok(block)
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
    pub(crate) fn finish(self) -> Result<Function<'a>> {
        Ok(self.function)
    }
}

impl<'a> std::ops::Index<usize> for FunctionBuilder<'a> {
    type Output = FunctionArg<'a>;

    fn index(&self, index: usize) -> &Self::Output {
        &self.args()[index]
    }
}

#[derive(Debug)]
pub struct Function<'a> {
    function: Value<'a>,
    signature: Type<'a>,
    args: Vec<FunctionArg<'a>>,
    __module: PhantomData<&'a Module<'a>>,
}

// Public interface
impl<'a> Function<'a> {
    #[inline]
    pub fn args(&self) -> &[FunctionArg<'a>] {
        &self.args
    }
}

// Private interface
impl<'a> Function<'a> {
    #[inline]
    pub(crate) fn new(module: &Module<'a>, name: &CStr, signature: &Signature<'a>) -> Result<Self> {
        use std::mem::MaybeUninit;

        let function = null!(
            unsafe {
                LLVMAddFunction(
                    module.as_mut_ptr(),
                    name.as_ptr(),
                    signature.as_type().as_mut_ptr(),
                )
            },
            "Failed to add function to module"
        )?;
        let function = Value::from_raw(function);

        // Get the number of function parameters
        let num_args = unsafe { LLVMCountParams(function.as_mut_ptr()) } as usize;

        // Get the parameter values
        let mut params: Vec<MaybeUninit<*mut LLVMValue>> = vec![MaybeUninit::zeroed(); num_args];
        unsafe {
            LLVMGetParams(
                function.as_mut_ptr(),
                params.as_mut_ptr() as *mut *mut LLVMValue,
            );
        }

        // Get the parameter types
        let mut param_types: Vec<MaybeUninit<*mut LLVMType>> =
            vec![MaybeUninit::zeroed(); num_args];
        unsafe {
            LLVMGetParamTypes(
                signature.as_type().as_mut_ptr(),
                param_types.as_mut_ptr() as *mut *mut LLVMType,
            );
        }

        // Zip up the types and values into a single vector
        let args = params
            .into_iter()
            .zip(param_types.into_iter())
            .map(|(param, ty)| {
                let param = null!(
                    unsafe { param.assume_init() },
                    "LLVM returned a null function parameter"
                )?;
                let ty = null!(
                    unsafe { ty.assume_init() },
                    "LLVM returned a null function parameter type"
                )?;

                Ok(FunctionArg {
                    value: Value::from_raw(param),
                    ty: Type::from_raw(ty),
                })
            })
            .collect::<Result<Vec<FunctionArg<'a>>>>()?;

        // Make sure the information LLVM gave us was sane
        #[cfg(debug_assertions)]
        for (i, FunctionArg { value, ty }) in args.iter().enumerate() {
            use llvm_sys::{core::LLVMGetValueKind, LLVMValueKind};

            debug_assert_eq!(*ty, signature.args()[i]);
            debug_assert_eq!(
                unsafe { LLVMGetValueKind(value.as_mut_ptr()) },
                LLVMValueKind::LLVMArgumentValueKind,
            );
        }

        Ok(Self {
            function,
            signature: signature.as_type(),
            args,
            __module: PhantomData,
        })
    }

    #[inline]
    pub(crate) const fn as_value(&self) -> Value<'a> {
        self.function
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct FunctionArg<'a> {
    value: Value<'a>,
    ty: Type<'a>,
}

impl<'a> FunctionArg<'a> {
    #[inline]
    pub const fn value(&self) -> Value<'a> {
        self.value
    }

    #[inline]
    pub const fn ty(&self) -> Type<'a> {
        self.ty
    }
}

impl<'a> Into<Value<'a>> for FunctionArg<'a> {
    fn into(self) -> Value<'a> {
        self.value
    }
}

impl<'a> Into<Type<'a>> for FunctionArg<'a> {
    fn into(self) -> Type<'a> {
        self.ty
    }
}

// TODO: Can PartialEq/Eq just go off of `signature`?
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Signature<'a> {
    signature: Type<'a>,
    return_type: Type<'a>,
    args: Vec<Type<'a>>,
    variadic: bool,
    __module: PhantomData<&'a Module<'a>>,
}

/// Public interface
impl<'a> Signature<'a> {
    #[inline]
    pub fn args(&self) -> &[Type<'a>] {
        &self.args
    }

    #[inline]
    pub const fn as_type(&self) -> Type<'a> {
        self.signature
    }
}

/// Private interface
impl<'a> Signature<'a> {
    #[inline]
    pub(crate) fn new<A>(return_type: Type<'a>, args: A, variadic: bool) -> Result<Self>
    where
        A: Into<Vec<Type<'a>>>,
    {
        let args = args.into();

        if <usize as TryInto<u32>>::try_into(args.len()).is_err() {
            return Err(Error::new(
                "Functions cannot have more than u32::MAX arguments",
                ErrorKind::TooManyArgs,
            ));
        }

        let signature = null!(
            unsafe {
                LLVMFunctionType(
                    return_type.as_mut_ptr(),
                    args.as_ptr() as *mut *mut LLVMType,
                    args.len() as u32,
                    variadic as _,
                )
            },
            "Failed to create function signature type",
        )?;
        let signature = Type::from_raw(signature);

        Ok(Self {
            signature,
            return_type,
            args,
            variadic,
            __module: PhantomData,
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::llvm::LLVM;

    #[test]
    fn build_function_sig() {
        let llvm = LLVM::new().unwrap();
        let module = llvm.create_module("module").unwrap();
        let _sig = module
            .add_function_type(
                llvm.get_type::<()>().unwrap(),
                &[
                    llvm.get_type::<i32>().unwrap(),
                    llvm.get_type::<i64>().unwrap(),
                ],
                false,
            )
            .unwrap();
    }

    #[test]
    fn add_function_to_module() {
        let llvm = LLVM::new().unwrap();
        let module = llvm.create_module("module").unwrap();
        let sig = module
            .add_function_type(llvm.get_type::<()>().unwrap(), &[], false)
            .unwrap();
        let _function = module.build_function("function", &sig, |_| Ok(())).unwrap();
    }

    #[test]
    fn build_function() {
        let llvm = LLVM::new().unwrap();
        let module = llvm.create_module("entry").unwrap();

        let aiee_32 = llvm.get_type::<i32>().unwrap();
        let sig = module
            .add_function_type(aiee_32, &[aiee_32, aiee_32], false)
            .unwrap();

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
        let sig = module
            .add_function_type(aiee_32, &[aiee_32, aiee_32], false)
            .unwrap();

        let _function = module
            .build_function("function", &sig, |builder| {
                assert_eq!(builder.len(), 0);
                builder.append_block("entry")?;
                assert_eq!(builder.len(), 1);
                builder.append_block("another_block")?;
                assert_eq!(builder.len(), 2);

                Ok(())
            })
            .unwrap();
    }
}
