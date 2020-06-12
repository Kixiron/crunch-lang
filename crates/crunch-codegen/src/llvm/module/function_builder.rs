use crate::llvm::{
    module::{Builder, BuildingBlock, Function, Module},
    types::{FunctionSig, SealedAnyType, Type},
    utils::LLVMString,
    values::{BasicBlock, SealedAnyValue, Value, ValueKind},
    Error, ErrorKind, Result,
};
use llvm_sys::{
    analysis::{LLVMVerifierFailureAction, LLVMVerifyFunction, LLVMVerifyModule},
    core::{LLVMAppendBasicBlockInContext, LLVMCountParams, LLVMGetParamTypes, LLVMGetParams},
    LLVMType, LLVMValue,
};
use std::{ffi::CString, mem::MaybeUninit, ops::Deref};

pub struct FunctionBuilder<'ctx> {
    function: Function<'ctx>,
    builder: Builder<'ctx>,
    module: &'ctx Module<'ctx>,
    args: Vec<FunctionArg<'ctx>>,
}

impl<'ctx> FunctionBuilder<'ctx> {
    pub fn append_block<'block, N>(&'block self, name: N) -> Result<BuildingBlock<'block>>
    where
        N: AsRef<str>,
    {
        let name = CString::new(name.as_ref())?;

        let block = unsafe {
            BasicBlock::from_raw(LLVMAppendBasicBlockInContext(
                self.module.ctx.as_mut_ptr(),
                self.function.as_mut_ptr(),
                name.as_ptr(),
            ))?
        };

        Ok(BuildingBlock::new(block, &self.builder))
    }

    pub fn args(&self) -> &[FunctionArg<'ctx>] {
        &self.args
    }

    pub fn num_args(&self) -> usize {
        self.args.len()
    }
}

impl<'ctx> FunctionBuilder<'ctx> {
    pub(crate) fn new(
        function: Function<'ctx>,
        module: &'ctx Module<'ctx>,
        signature: &FunctionSig<'ctx>,
    ) -> Result<Self> {
        let builder = Builder::new(module.ctx)?;

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
                signature.as_mut_ptr(),
                param_types.as_mut_ptr() as *mut *mut LLVMType,
            );
        }

        // Zip up the types and values into a single vector
        let args = params
            .into_iter()
            .zip(param_types.into_iter())
            .map(|(param, ty)| unsafe {
                let value = Value::from_raw(param.assume_init())?;
                let ty = Type::from_raw(ty.assume_init())?;

                Ok(FunctionArg { value, ty })
            })
            .collect::<Result<Vec<FunctionArg<'ctx>>>>()?;

        // Make sure the information LLVM gave us was sane
        #[cfg(debug_assertions)]
        for (i, FunctionArg { value, ty }) in args.iter().enumerate() {
            use llvm_sys::core::LLVMGetValueKind;

            // TODO: Test signature types?
            debug_assert_eq!(
                unsafe { ValueKind::from(LLVMGetValueKind(value.as_mut_ptr())) },
                ValueKind::Argument,
            );
        }

        Ok(Self {
            function,
            builder,
            module,
            args,
        })
    }

    #[inline]
    pub(crate) fn finish(self) -> Result<Function<'ctx>> {
        let failed_verification = unsafe {
            LLVMVerifyFunction(
                self.as_mut_ptr(),
                LLVMVerifierFailureAction::LLVMReturnStatusAction,
            )
        } != 0;

        if failed_verification {
            // This pointer has to be destroyed using `LLVMDisposeMessage`, which is what the later `LLVMString` guarantees
            let mut llvm_message = MaybeUninit::zeroed();
            // FIXME: This is pretty hacky, does the diagnostic handler help with this?
            unsafe {
                // This also returns a status code on the module, but we just want info on the current function
                LLVMVerifyModule(
                    self.module.as_mut_ptr(),
                    LLVMVerifierFailureAction::LLVMPrintMessageAction,
                    llvm_message.as_mut_ptr(),
                )
            };

            let llvm_string = unsafe { LLVMString::from_raw(llvm_message.assume_init()) };
            if let Ok(message) = llvm_string {
                if !message.is_empty() {
                    return Err(Error::new(
                        format!(
                            "LLVM failed to verify function: {}",
                            message.to_string_lossy()
                        ),
                        ErrorKind::InvalidFunction,
                    ));
                }
            }

            Err(Error::new(
                "<LLVM gave no error message>",
                ErrorKind::InvalidFunction,
            ))
        } else {
            Ok(self.function)
        }
    }

    #[inline]
    pub(crate) fn move_to_end(&self, block: &BasicBlock<'ctx>) {
        self.builder.move_to_end(block);
    }
}

impl<'ctx> AsRef<Function<'ctx>> for FunctionBuilder<'ctx> {
    fn as_ref(&self) -> &Function<'ctx> {
        &self.function
    }
}

impl<'ctx> Deref for FunctionBuilder<'ctx> {
    type Target = Function<'ctx>;

    fn deref(&self) -> &Self::Target {
        &self.function
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FunctionArg<'ctx> {
    value: Value<'ctx>,
    ty: Type<'ctx>,
}

impl<'ctx> FunctionArg<'ctx> {
    #[inline]
    pub const fn value(&self) -> Value<'ctx> {
        self.value
    }

    #[inline]
    pub const fn ty(&self) -> Type<'ctx> {
        self.ty
    }
}

impl<'ctx> Into<Value<'ctx>> for FunctionArg<'ctx> {
    fn into(self) -> Value<'ctx> {
        self.value
    }
}

impl<'ctx> Into<Type<'ctx>> for FunctionArg<'ctx> {
    fn into(self) -> Type<'ctx> {
        self.ty
    }
}
