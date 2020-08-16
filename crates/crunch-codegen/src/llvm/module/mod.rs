mod builder;
mod building_block;
mod function_builder;
mod linkage;

use builder::Builder;
pub use building_block::BuildingBlock;
pub use function_builder::FunctionBuilder;
pub use linkage::Linkage;

use crate::llvm::{
    types::{AnyType, FunctionSig, SealedAnyType, Type},
    utils::{to_non_nul, AddressSpace, LLVMString},
    values::{AnyValue, FunctionValue, Global, Pointable, PointerValue, SealedAnyValue},
    Context, Error, ErrorKind, Result,
};
use llvm_sys::{
    analysis::{LLVMVerifierFailureAction, LLVMVerifyModule},
    bit_writer::LLVMWriteBitcodeToFile,
    core::{
        LLVMAddFunction, LLVMAddGlobal, LLVMAddGlobalInAddressSpace, LLVMCloneModule,
        LLVMDisposeModule, LLVMFunctionType, LLVMGetNamedFunction, LLVMPrintModuleToFile,
        LLVMPrintModuleToString, LLVMStructType,
    },
    LLVMModule, LLVMType,
};
use std::{
    ffi::{CStr, CString},
    fmt::{Debug, Formatter, Result as FmtResult},
    mem::MaybeUninit,
    path::Path,
    ptr::NonNull,
};

#[derive(PartialEq, Eq)]
pub struct Module<'ctx> {
    module: NonNull<LLVMModule>,
    ctx: &'ctx Context,
}

// Public interface
impl<'ctx> Module<'ctx> {
    #[inline]
    pub fn build_function<N>(
        &'ctx self,
        name: N,
        signature: FunctionSig<'ctx>,
    ) -> Result<FunctionBuilder<'ctx>>
    where
        N: AsRef<str>,
    {
        let name = CString::new(name.as_ref())?;

        let function = unsafe {
            FunctionValue::from_raw(LLVMAddFunction(
                self.as_mut_ptr(),
                name.as_ptr(),
                signature.as_mut_ptr(),
            ))?
        };

        FunctionBuilder::new(function, self, &signature)
    }

    #[inline]
    pub fn create_function<N>(
        &'ctx self,
        name: N,
        signature: FunctionSig<'ctx>,
    ) -> Result<FunctionValue<'ctx>>
    where
        N: AsRef<str>,
    {
        let name = CString::new(name.as_ref())?;

        let function = unsafe {
            FunctionValue::from_raw(LLVMAddFunction(
                self.as_mut_ptr(),
                name.as_ptr(),
                signature.as_mut_ptr(),
            ))?
        };

        Ok(function)
    }

    pub fn resume_function(
        &'ctx self,
        function: FunctionValue<'ctx>,
    ) -> Result<FunctionBuilder<'ctx>> {
        FunctionBuilder::new(function, self, &function.signature()?)
    }

    pub fn create_struct(&self, elements: &[Type<'ctx>], packed: bool) -> Result<Type<'ctx>> {
        unsafe {
            Type::from_raw(LLVMStructType(
                elements.as_ptr() as *mut *mut LLVMType,
                elements.len() as u32,
                packed as i32,
            ))
        }
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
    pub fn function_ty(
        &self,
        // TODO: Make these both require concrete values
        return_type: Type<'ctx>,
        args: &[Type<'ctx>],
        variadic: bool,
    ) -> Result<FunctionSig<'ctx>> {
        // TODO: This isn't ideal
        let mut args: Vec<*mut LLVMType> =
            args.iter().copied().map(|arg| arg.as_mut_ptr()).collect();

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
                return_type.as_mut_ptr(),
                args.as_mut_ptr(),
                args.len() as u32,
                variadic as _,
            ))
        }
    }

    /// Gets a function by its name
    #[inline]
    pub fn get_function<N>(&self, name: N) -> Result<FunctionValue<'ctx>>
    where
        N: AsRef<str>,
    {
        let name = CString::new(name.as_ref())?;
        unsafe { FunctionValue::from_raw(LLVMGetNamedFunction(self.as_mut_ptr(), name.as_ptr())) }
    }

    /// Gets a function by its name or creates a new one if it doesn't exist
    // TODO: Return the signature somehow?
    #[inline]
    pub fn get_or_create_function<N, F>(&self, name: N, create: F) -> Result<FunctionValue<'ctx>>
    where
        N: AsRef<str>,
        F: FnOnce(&Module<'ctx>) -> Result<FunctionSig<'ctx>>,
    {
        let name = CString::new(name.as_ref())?;
        let func = unsafe {
            FunctionValue::from_raw(LLVMGetNamedFunction(self.as_mut_ptr(), name.as_ptr()))
        };

        match func {
            Ok(func) => Ok(func),
            Err(_) => {
                let sig = create(self)?;
                unsafe {
                    FunctionValue::from_raw(LLVMAddFunction(
                        self.as_mut_ptr(),
                        name.as_ptr(),
                        sig.as_mut_ptr(),
                    ))
                }
            }
        }
    }

    pub fn add_global<T, V, S>(
        &self,
        ty: T,
        address_space: Option<AddressSpace>,
        name: S,
    ) -> Result<PointerValue<'ctx, Global<V>>>
    where
        T: Pointable + AnyType<'ctx>,
        V: AnyValue<'ctx>,
        S: AsRef<str>,
    {
        let name = CString::new(name.as_ref())?;

        unsafe {
            let value = match address_space {
                Some(address_space) => LLVMAddGlobalInAddressSpace(
                    self.as_mut_ptr(),
                    ty.as_mut_ptr(),
                    name.as_ptr(),
                    address_space as u32,
                ),
                None => LLVMAddGlobal(self.as_mut_ptr(), ty.as_mut_ptr(), name.as_ptr()),
            };

            PointerValue::from_raw(value)
        }
    }

    #[inline]
    pub const fn context(&self) -> &'ctx Context {
        self.ctx
    }

    pub fn emit_ir_to_file(&self, path: impl AsRef<Path>) -> Result<()> {
        let path = CString::new(unsafe { &*(path.as_ref() as *const Path as *const [u8]) })?;
        let mut err_message = MaybeUninit::zeroed();

        let failed = unsafe {
            LLVMPrintModuleToFile(
                self.as_mut_ptr(),
                path.as_ptr() as *mut i8,
                err_message.as_mut_ptr(),
            ) == 1
        };

        if failed {
            let err_message = unsafe { LLVMString::from_raw(err_message.assume_init())? };

            Err(Error::new(err_message, ErrorKind::FailedEmission))
        } else {
            Ok(())
        }
    }

    pub fn emit_bitcode_to_file(&self, path: impl AsRef<Path>) -> Result<()> {
        let path = CString::new(unsafe { &*(path.as_ref() as *const Path as *const [u8]) })?;

        let failed =
            unsafe { LLVMWriteBitcodeToFile(self.as_mut_ptr(), path.as_ptr() as *mut i8) == 0 };

        if failed {
            Err(Error::new(
                "Failed to emit bitcode to file",
                ErrorKind::FailedEmission,
            ))
        } else {
            Ok(())
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
