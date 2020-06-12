/*
use crate::{
    llvm::{
        context::Context, module::Module, utils::LLVMString, value::Value, Error,
        ErrorKind, Result,
    },
    null,
};
use llvm_sys::{
    analysis::{LLVMVerifierFailureAction, LLVMVerifyFunction, LLVMVerifyModule},
    core::{
        LLVMAddFunction, LLVMCountParams, LLVMCreateMessage, LLVMDisposeMessage, LLVMFunctionType,
        LLVMGetParamTypes, LLVMGetParams,
    },
    LLVMType, LLVMValue,
};
use std::{borrow::Cow, ffi::CStr, fmt, marker::PhantomData, mem::MaybeUninit, ptr::NonNull};

#[derive(Debug)]
pub struct Function<'a> {
    function: Value<'a>,
    signature: Type<'a>,
    args: Vec<FunctionArg<'a>>,
    return_type: Type<'a>,
    module: &'a Module<'a>,
}

// Public interface
impl<'a> Function<'a> {
    #[inline]
    pub fn args(&self) -> &[FunctionArg<'a>] {
        &self.args
    }

    #[inline]
    pub const fn return_type(&self) -> Type<'a> {
        self.return_type
    }
}

// Private interface
impl<'a> Function<'a> {
    #[inline]
    pub(crate) fn new(
        module: &'a Module<'a>,
        name: &CStr,
        signature: &Signature<'a>,
    ) -> Result<Self> {
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
            return_type: signature.return_type,
            args,
            module,
        })
    }

    #[inline]
    pub(crate) const fn as_value(&self) -> Value<'a> {
        self.function
    }

    #[inline]
    pub(crate) fn verify(&self) -> Result<()> {
        let failed_verification = unsafe {
            LLVMVerifyFunction(
                self.as_value().as_mut_ptr(),
                LLVMVerifierFailureAction::LLVMReturnStatusAction,
            )
        } != 0;

        if failed_verification {
            // This pointer has to be destroyed using `LLVMDisposeMessage`, which is what the later `LLVMString` guarantees
            let mut llvm_message: MaybeUninit<*mut i8> = MaybeUninit::zeroed();
            unsafe {
                // This also returns a status code on the module, but we just want info on the current function
                LLVMVerifyModule(
                    self.module.as_mut_ptr(),
                    LLVMVerifierFailureAction::LLVMPrintMessageAction,
                    llvm_message.as_mut_ptr(),
                )
            };

            let llvm_string = LLVMString::from_ptr(unsafe { llvm_message.assume_init() });
            let message = if let Ok(ref message) = llvm_string {
                if message.is_empty() {
                    "<LLVM gave no error message>".into()
                } else {
                    message.to_string_lossy()
                }
            } else {
                "<LLVM gave no error message>".into()
            };

            Err(Error::new(
                format!("LLVM failed to verify function: {}", message),
                ErrorKind::InvalidFunc,
            ))
        } else {
            Ok(())
        }
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
    pub const fn ret(&self) -> Type<'a> {
        self.return_type
    }

    #[inline]
    pub fn args(&self) -> &[Type<'a>] {
        &self.args
    }

    #[inline]
    pub fn num_args(&self) -> usize {
        self.args.len()
    }

    #[inline]
    pub const fn is_variadic(&self) -> bool {
        self.variadic
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

        // LLVM takes a u32 as the length of function arguments, so return an error if there are too many
        if args.len() > u32::max_value() as usize {
            return Err(Error::new(
                "Functions cannot have more than u32::MAX arguments",
                ErrorKind::TooManyArgs,
            ));
        }

        // Make sure all function arguments are valid
        if args.iter().any(|arg| arg.is_void()) {
            return Err(Error::new(
                "Functions cannot take `void` as an argument",
                ErrorKind::InvalidFuncArg,
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
            .function_ty(
                &[
                    llvm.get_type::<i32>().unwrap(),
                    llvm.get_type::<i64>().unwrap(),
                ],
                llvm.get_type::<()>().unwrap(),
            )
            .unwrap();
    }

    #[test]
    fn add_function_to_module() {
        let llvm = LLVM::new().unwrap();
        let module = llvm.create_module("module").unwrap();
        let sig = module
            .function_ty(&[], llvm.get_type::<()>().unwrap())
            .unwrap();

        let function = module.build_function("function", &sig, |_| Ok(())).unwrap();
        assert!(function.as_value().is_function());
    }

    #[test]
    fn void_func_arg() {
        let llvm = LLVM::new().unwrap();
        let module = llvm.create_module("module").unwrap();
        let void = llvm.get_type::<()>().unwrap();

        let sig = module.function_ty(&[void], void);

        assert!(sig.is_err());
    }
}
*/
