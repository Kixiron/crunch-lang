use crate::llvm::{
    types::{ArrayType, FloatType, PointerType, StructType, VectorType},
    utils::{DLLStorageClass, Sealed, ThreadLocalMode},
    values::{AnyValue, SealedAnyValue, Val},
};
use llvm_sys::core::{
    LLVMGetDLLStorageClass, LLVMGetInitializer, LLVMGetThreadLocalMode, LLVMIsThreadLocal,
    LLVMSetDLLStorageClass, LLVMSetInitializer, LLVMSetThreadLocal, LLVMSetThreadLocalMode,
};
use std::{
    fmt::{Debug, Formatter, Result as FmtResult},
    marker::PhantomData,
};

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct PointerValue<'ctx, T>(Val<'ctx>, PhantomData<T>);

impl<'ctx, T: Copy> PointerValue<'ctx, Global<T>> {
    pub fn dll_storage_class(self) -> DLLStorageClass {
        DLLStorageClass::from(unsafe { LLVMGetDLLStorageClass(self.0.as_mut_ptr()) })
    }

    pub fn set_dll_storage_class(self, storage_class: DLLStorageClass) {
        unsafe { LLVMSetDLLStorageClass(self.0.as_mut_ptr(), storage_class.into()) }
    }

    pub fn with_dll_storage_class(self, storage_class: DLLStorageClass) -> Self {
        self.set_dll_storage_class(storage_class);
        self
    }

    pub fn thread_local(self) -> bool {
        unsafe { LLVMIsThreadLocal(self.0.as_mut_ptr()) == 1 }
    }

    pub fn set_thread_local(self, is_thread_local: bool) {
        unsafe { LLVMSetThreadLocal(self.0.as_mut_ptr(), is_thread_local as i32) }
    }

    pub fn with_thread_local(self, is_thread_local: bool) -> Self {
        self.set_thread_local(is_thread_local);
        self
    }

    pub fn thread_local_mode(self) -> Option<ThreadLocalMode> {
        let thread_local_mode = unsafe { LLVMGetThreadLocalMode(self.0.as_mut_ptr()) };

        ThreadLocalMode::from_llvm_mode(thread_local_mode)
    }

    pub fn set_thread_local_mode(self, thread_local_mode: Option<ThreadLocalMode>) {
        self.set_thread_local(true);

        unsafe {
            LLVMSetThreadLocalMode(
                self.0.as_mut_ptr(),
                ThreadLocalMode::into_llvm_mode(thread_local_mode),
            );
        }
    }

    pub fn with_thread_local_mode(self, thread_local_mode: Option<ThreadLocalMode>) -> Self {
        self.set_thread_local_mode(thread_local_mode);
        self
    }
}

impl<'ctx, T: AnyValue<'ctx>> PointerValue<'ctx, Global<T>> {
    pub fn get_initializer(self) -> Option<T> {
        unsafe { T::from_raw(LLVMGetInitializer(self.0.as_mut_ptr())).ok() }
    }

    pub fn set_initializer(self, value: T) {
        unsafe { LLVMSetInitializer(self.0.as_mut_ptr(), value.as_mut_ptr()) }
    }

    pub fn with_initializer(self, value: T) -> Self {
        self.set_initializer(value);

        self
    }
}

impl<'ctx, T> AnyValue<'ctx> for PointerValue<'ctx, T> {}

impl<'ctx, T> SealedAnyValue<'ctx> for PointerValue<'ctx, T> {
    fn as_val(&self) -> Val<'ctx> {
        self.0
    }

    fn from_val(value: Val<'ctx>) -> Self {
        Self(value, PhantomData)
    }
}

impl<'ctx, T> Sealed for PointerValue<'ctx, T> {}

impl<T> Debug for PointerValue<'_, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        Debug::fmt(&self.0, f)
    }
}

impl<'ctx, T> Clone for PointerValue<'ctx, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'ctx, T> Copy for PointerValue<'ctx, T> {}

pub trait Pointable: Sealed {}

impl<'ctx> Pointable for FloatType<'ctx> {}
impl<'ctx> Pointable for PointerType<'ctx> {}
impl<'ctx> Pointable for StructType<'ctx> {}
impl<'ctx, T: Pointable> Pointable for VectorType<'ctx, T> {}
impl<'ctx> Pointable for ArrayType<'ctx> {}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Global<T>(PhantomData<T>);

impl<'ctx, T: Sealed> Sealed for Global<T> {}
