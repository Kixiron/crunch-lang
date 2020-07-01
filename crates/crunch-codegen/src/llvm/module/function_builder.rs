use crate::llvm::{
    module::{Builder, BuildingBlock, Module},
    types::{FunctionSig, SealedAnyType, Type},
    utils::{LLVMString, UNNAMED_CSTR},
    values::{BasicBlock, FunctionValue, SealedAnyValue, Value, ValueKind},
    Error, ErrorKind, Result,
};
use llvm_sys::{
    analysis::{LLVMVerifierFailureAction, LLVMVerifyFunction, LLVMVerifyModule},
    core::{LLVMAppendBasicBlockInContext, LLVMCountParams, LLVMGetParamTypes, LLVMGetParams},
    LLVMType, LLVMValue,
};
use std::{mem::MaybeUninit, ops::Deref};

pub struct FunctionBuilder<'ctx> {
    function: FunctionValue<'ctx>,
    module: &'ctx Module<'ctx>,
    args: Vec<FunctionArg<'ctx>>,
    finished: bool,
}

impl<'ctx> FunctionBuilder<'ctx> {
    pub fn append_block(&self) -> Result<BuildingBlock<'ctx>> {
        let builder = Builder::new(self.module.context())?;
        let block = unsafe {
            BasicBlock::from_raw(LLVMAppendBasicBlockInContext(
                self.module.ctx.as_mut_ptr(),
                self.function.as_mut_ptr(),
                UNNAMED_CSTR,
            ))?
        };
        builder.move_to_end(&block);

        Ok(BuildingBlock::new(block, builder))
    }

    pub fn args(&self) -> &[FunctionArg<'ctx>] {
        &self.args
    }

    pub fn num_args(&self) -> usize {
        self.args.len()
    }

    pub fn move_to_end(&self, block: BasicBlock<'ctx>) -> Result<BuildingBlock<'ctx>> {
        let builder = Builder::new(self.module.context())?;
        builder.move_to_end(&block);

        Ok(BuildingBlock::new(block, builder))
    }
}

impl<'ctx> FunctionBuilder<'ctx> {
    pub(crate) fn new(
        function: FunctionValue<'ctx>,
        module: &'ctx Module<'ctx>,
        signature: &FunctionSig<'ctx>,
    ) -> Result<Self> {
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
        for FunctionArg { value, ty: _ } in args.iter() {
            use llvm_sys::core::LLVMGetValueKind;

            // TODO: Test signature types?
            debug_assert_eq!(
                unsafe { ValueKind::from(LLVMGetValueKind(value.as_mut_ptr())) },
                ValueKind::Argument,
            );
        }

        Ok(Self {
            function,
            module,
            args,
            finished: false,
        })
    }

    #[inline]
    pub fn finish(mut self) -> Result<FunctionValue<'ctx>> {
        self.finish_inner()
    }

    fn finish_inner(&mut self) -> Result<FunctionValue<'ctx>> {
        self.finished = true;

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
}

impl<'ctx> AsRef<FunctionValue<'ctx>> for FunctionBuilder<'ctx> {
    fn as_ref(&self) -> &FunctionValue<'ctx> {
        &self.function
    }
}

impl<'ctx> Deref for FunctionBuilder<'ctx> {
    type Target = FunctionValue<'ctx>;

    fn deref(&self) -> &Self::Target {
        &self.function
    }
}

impl<'ctx> Drop for FunctionBuilder<'ctx> {
    fn drop(&mut self) {
        // Don't attempt to finish if the thread is panicking, since the code is likely invalid at
        // that stage and will only incur a double panic
        if !self.finished && !std::thread::panicking() {
            self.finish_inner().expect("Failed to build function");
        }
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
