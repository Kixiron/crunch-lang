mod builder;
mod building_block;
mod function_builder;
mod linkage;

use builder::Builder;
pub use building_block::BuildingBlock;
pub use function_builder::FunctionBuilder;
pub use linkage::Linkage;

use crate::llvm::{
    types::{FunctionSig, SealedAnyType, Ty, Type},
    utils::{to_non_nul, LLVMString},
    values::{Function, SealedAnyValue},
    Context, Error, ErrorKind, Result,
};
use llvm_sys::{
    analysis::{LLVMVerifierFailureAction, LLVMVerifyModule},
    core::{
        LLVMAddFunction, LLVMCloneModule, LLVMDisposeModule, LLVMFunctionType,
        LLVMPrintModuleToString,
    },
    LLVMModule, LLVMType,
};
use std::{
    ffi::{CStr, CString},
    fmt::{Debug, Formatter, Result as FmtResult},
    mem::MaybeUninit,
    ptr::NonNull,
};

pub struct Module<'ctx> {
    module: NonNull<LLVMModule>,
    ctx: &'ctx Context,
}

// Public interface
impl<'ctx> Module<'ctx> {
    #[inline]
    pub fn build_function<N, B>(
        &'ctx self,
        name: N,
        signature: FunctionSig<'ctx>,
        build_function: B,
    ) -> Result<Function<'ctx>>
    where
        N: AsRef<str>,
        B: FnOnce(&mut FunctionBuilder<'ctx>) -> Result<()>,
    {
        let name = CString::new(name.as_ref())?;

        let function = unsafe {
            Function::from_raw(LLVMAddFunction(
                self.as_mut_ptr(),
                name.as_ptr(),
                signature.as_mut_ptr(),
            ))?
        };

        let mut builder = FunctionBuilder::new(function, self, &signature)?;
        build_function(&mut builder)?;
        builder.finish()
    }

    #[inline]
    pub fn verify(&self) -> Result<()> {
        let mut err_message = MaybeUninit::zeroed();

        let failed_verify = unsafe {
            LLVMVerifyModule(
                self.as_mut_ptr(),
                LLVMVerifierFailureAction::LLVMReturnStatusAction, // Makes the function return 1 on error
                err_message.as_mut_ptr(),
            ) == 1
        };

        if failed_verify {
            // Safety: An error occurred, so the error should be initialized
            let err_message = unsafe { err_message.assume_init() };
            // Null pointers will be caught by `LLVMString::from_raw`, but we want to know if it happens
            // during debugging, since it shouldn't ever happen
            debug_assert!(!err_message.is_null());

            // Safety: The string was allocated by LLVM and is uniquely owned
            Err(Error::new(
                unsafe { LLVMString::from_raw(err_message)? },
                ErrorKind::InvalidModule,
            ))
        } else {
            Ok(())
        }
    }

    #[inline]
    pub fn function_ty<T, A>(
        &self,
        // TODO: Make these both require concrete values
        return_type: T,
        args: &[A],
        variadic: bool,
    ) -> Result<FunctionSig<'ctx>>
    where
        T: Into<Type<'ctx>>,
        A: Into<Type<'ctx>> + Copy,
    {
        // TODO: This isn't ideal
        let mut args: Vec<*mut LLVMType> = args
            .iter()
            .copied()
            .map(|arg| arg.into().as_ty().as_mut_ptr())
            .collect();

        // LLVM takes a u32 as the length of function arguments, so return an error if there are too many
        if args.len() > u32::max_value() as usize {
            return Err(Error::new(
                "Functions cannot have more than u32::MAX arguments",
                ErrorKind::TooManyArgs,
            ));
        }

        // TODO: Make sure all function arguments are valid (non-void, etc.)

        unsafe {
            FunctionSig::from_raw(LLVMFunctionType(
                return_type.into().as_mut_ptr(),
                args.as_mut_ptr(),
                args.len() as u32,
                variadic as _,
            ))
        }
    }
}

// Private interface
impl<'ctx> Module<'ctx> {
    /// Creates a `Module` from a raw pointer, returning an error if the pointer is null
    ///
    /// # Safety
    ///
    /// The new `module` must have unique ownership over the given `LLVMModule`, otherwise UB
    /// will occur since `LLVMDisposeModule` will be called twice via the `Drop` implementation
    ///
    #[inline]
    pub(crate) unsafe fn from_raw(ctx: &'ctx Context, raw: *mut LLVMModule) -> Result<Self> {
        let module = to_non_nul(raw, "Failed to create Module")?;

        Ok(Self { module, ctx })
    }

    /// Fetches a raw pointer to the underlying `LLVMModule`
    #[inline]
    pub(crate) const fn as_mut_ptr(&self) -> *mut LLVMModule {
        self.module.as_ptr()
    }
}

/// Clones a `Module`, panicking if an error occurs
impl<'ctx> Clone for Module<'ctx> {
    #[inline]
    fn clone(&self) -> Self {
        // Safety: The newly created module has unique ownership over the underlying module
        unsafe {
            let module = LLVMCloneModule(self.as_mut_ptr());

            Self::from_raw(self.ctx, module).expect("Failed to clone Module")
        }
    }
}

impl Debug for Module<'_> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        let string = unsafe {
            CStr::from_ptr(
                to_non_nul(
                    LLVMPrintModuleToString(self.as_mut_ptr()),
                    "Failed to print Module to string",
                )
                .expect("Failed to print LLVM Module to string")
                .as_ptr()
                .cast(),
            )
        };

        f.write_str(&string.to_string_lossy())
    }
}

impl<'ctx> Drop for Module<'ctx> {
    #[inline]
    fn drop(&mut self) {
        // Safety: The current module is uniquely owned, and therefore will only be disposed once
        unsafe { LLVMDisposeModule(self.as_mut_ptr()) };
    }
}

/*
use crate::{
    llvm::{
        builder::FunctionBuilder,
        context::Context,
        function::{Function, Signature},
        types::Type,
        Result,
    },
    null,
};
use llvm_sys::{
    core::{
        LLVMCloneModule, LLVMDisposeModule, LLVMModuleCreateWithNameInContext,
        LLVMPrintModuleToString,
    },
    LLVMModule,
};
use std::{
    ffi::{CStr, CString},
    fmt,
    ptr::NonNull,
};

pub struct Module<'a> {
    module: NonNull<LLVMModule>,
    ctx: &'a Context,
}

/// Public methods
impl<'a> Module<'a> {
    #[inline]
    pub fn function_ty(
        &self,
        func_args: &[Type<'a>],
        return_type: Type<'a>,
    ) -> Result<Signature<'a>> {
        Signature::new(return_type, func_args, false)
    }

    #[inline]
    pub fn function_ty_variadic(
        &self,
        func_args: &[Type<'a>],
        return_type: Type<'a>,
    ) -> Result<Signature<'a>> {
        Signature::new(return_type, func_args, true)
    }

    #[inline]
    pub fn build_function<S, F>(
        &'a self,
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
        let void = llvm.get_type::<()>().unwrap();

        let sig = module_one
            .function_ty(&[aiee_thirty_two, aiee_thirty_two], aiee_thirty_two)
            .unwrap();
        let _function = module_one
            .build_function("function_one", &sig, |_| Ok(()))
            .unwrap();

        // Clone the module
        let module_two = module_one.clone();

        // Create a function for the old and newly cloned modules
        let sig_one = module_one.function_ty(&[], void).unwrap();
        let _function_one = module_one
            .build_function("function_two", &sig_one, |_| Ok(()))
            .unwrap();

        let sig_two = module_two.function_ty(&[], void).unwrap();
        let _function_two = module_two
            .build_function("function_two", &sig_two, |_| Ok(()))
            .unwrap();
    }

    #[test]
    fn create_func() {
        let llvm = LLVM::new().unwrap();
        let module = llvm.create_module("entry").unwrap();
        let int32 = llvm.get_type::<i32>().unwrap();

        let sig = module.function_ty(&[int32, int32], int32).unwrap();

        assert_eq!(sig.args(), &[int32, int32]);
        assert_eq!(sig.args().len(), 2);
        assert_eq!(sig.num_args(), 2);
        assert_eq!(sig.ret(), int32);
        assert!(!sig.is_variadic());
    }

    #[test]
    fn argless_func() {
        let llvm = LLVM::new().unwrap();
        let module = llvm.create_module("entry").unwrap();
        let int32 = llvm.get_type::<i32>().unwrap();

        let sig = module.function_ty(&[], int32).unwrap();

        assert_eq!(sig.args(), &[]);
        assert_eq!(sig.args().len(), 0);
        assert_eq!(sig.num_args(), 0);
        assert_eq!(sig.ret(), int32);
        assert!(!sig.is_variadic());
    }
}
*/
