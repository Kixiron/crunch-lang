use llvm_sys::core::LLVMIsConstant;

/// Non-terminating instructions.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Instruction<'ctx, T> {
    Add(Add<'ctx, T>),
    Sub(Sub<'ctx, T>),
    Mul(Mul<'ctx, T>),
    UDiv(UDiv<'ctx, T>),
    SDiv(SDiv<'ctx, T>),
    URem(URem<'ctx, T>),
    SRem(SRem<'ctx, T>),
}

impl<'ctx, T> Instruction<'ctx, T> {
    pub fn is_binary_op(&self) -> bool {
        match self {
            Self::Add(..)
            | Self::Sub(..)
            | Self::Mul(..)
            | Self::UDiv(..)
            | Self::SDiv(..)
            | Self::URem(..)
            | Self::SRem(..) => true,
        }
    }
}

pub(crate) trait FromRaw<'ctx, T>: Sized {
    unsafe fn from_raw(raw: *mut T) -> Result<Self>;
}

macro_rules! is_a {
    ($($name:ident -> $func:ident),* $(,)?) => {
        $(
            fn $name(&self) -> bool {
                unsafe { $func(self.llvm_ptr().as_ptr()) == 0 }
            }
        )*
    };
}

pub trait Valued<'ctx> {
    is_a! {
        is_const -> LLVMIsConstant,
    }

    /// Fetches an object's LLVM pointer
    #[doc(hidden)]
    unsafe fn llvm_ptr(&self) -> ValPtr<'ctx>;
}

macro_rules! impl_valued {
    (typed($($ident:ident),*): $struct:ident -> $field:ident) => {
        impl<'ctx, $($ident),*> Valued<'ctx> for $struct<'ctx, $($ident),*> {
            unsafe fn llvm_ptr(&self) -> ValPtr<'ctx> {
                self.$field
            }
        }
    };

    (typed($($ident:ident),*): $struct:ident) => {
        impl_valued!(typed($($ident),*): $struct->handle);
    };
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Add<'ctx, T> {
    handle: ValPtr<'ctx>,
    ty: PhantomData<T>,
}

impl<'ctx, T> Add<'ctx, T> {
    pub(crate) unsafe fn from_raw(raw: *mut LLVMValue) -> Result<Self> {
        Ok(Self {
            handle: ValPtr::new(raw)?,
            ty: PhantomData,
        })
    }
}

impl<'ctx, T> Into<IntValue<'ctx, T>> for Add<'ctx, T> {
    fn into(self) -> IntValue<'ctx, T> {
        IntValue {
            handle: self.handle,
            ty: self.ty,
        }
    }
}

impl_valued!(typed(T): Add);

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Sub<'ctx, T> {
    handle: ValPtr<'ctx>,
    ty: PhantomData<T>,
}

impl<'ctx, T> Sub<'ctx, T> {
    pub(crate) unsafe fn from_raw(raw: *mut LLVMValue) -> Result<Self> {
        Ok(Self {
            handle: ValPtr::new(raw)?,
            ty: PhantomData,
        })
    }
}

impl_valued!(typed(T): Sub);

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Mul<'ctx, T> {
    handle: ValPtr<'ctx>,
    ty: PhantomData<T>,
}

impl<'ctx, T> Mul<'ctx, T> {
    pub(crate) unsafe fn from_raw(raw: *mut LLVMValue) -> Result<Self> {
        Ok(Self {
            handle: ValPtr::new(raw)?,
            ty: PhantomData,
        })
    }
}

impl_valued!(typed(T): Mul);

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct UDiv<'ctx, T> {
    handle: ValPtr<'ctx>,
    ty: PhantomData<T>,
}

impl<'ctx, T> UDiv<'ctx, T> {
    pub(crate) unsafe fn from_raw(raw: *mut LLVMValue) -> Result<Self> {
        Ok(Self {
            handle: ValPtr::new(raw)?,
            ty: PhantomData,
        })
    }
}

impl_valued!(typed(T): UDiv);

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct SDiv<'ctx, T> {
    handle: ValPtr<'ctx>,
    ty: PhantomData<T>,
}

impl<'ctx, T> SDiv<'ctx, T> {
    pub(crate) unsafe fn from_raw(raw: *mut LLVMValue) -> Result<Self> {
        Ok(Self {
            handle: ValPtr::new(raw)?,
            ty: PhantomData,
        })
    }
}

impl_valued!(typed(T): SDiv);

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct URem<'ctx, T> {
    handle: ValPtr<'ctx>,
    ty: PhantomData<T>,
}

impl<'ctx, T> URem<'ctx, T> {
    pub(crate) unsafe fn from_raw(raw: *mut LLVMValue) -> Result<Self> {
        Ok(Self {
            handle: ValPtr::new(raw)?,
            ty: PhantomData,
        })
    }
}

impl_valued!(typed(T): URem);

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct SRem<'ctx, T> {
    handle: ValPtr<'ctx>,
    ty: PhantomData<T>,
}

impl<'ctx, T> SRem<'ctx, T> {
    pub(crate) unsafe fn from_raw(raw: *mut LLVMValue) -> Result<Self> {
        Ok(Self {
            handle: ValPtr::new(raw)?,
            ty: PhantomData,
        })
    }
}

impl_valued!(typed(T): SRem);

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct IntValue<'ctx, T> {
    handle: ValPtr<'ctx>,
    ty: PhantomData<T>,
}

impl<'ctx, T> IntValue<'ctx, T> {
    pub(crate) unsafe fn from_raw(raw: *mut LLVMValue) -> Result<Self> {
        Ok(Self {
            handle: ValPtr::new(raw)?,
            ty: PhantomData,
        })
    }
}

impl_valued!(typed(T): IntValue);

use crate::llvm::{utils::AddressSpace, Context, Error, ErrorKind, Result};
use llvm_sys::{LLVMType, LLVMValue};
use std::{marker::PhantomData, ptr::NonNull};

/// A lifetime-constrained pointer to an LLVM value
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[repr(transparent)]
pub struct ValPtr<'ctx> {
    ptr: NonNull<LLVMValue>,
    __ctx: PhantomData<&'ctx Context>,
}

impl<'ctx> ValPtr<'ctx> {
    // TODO: `#[track_caller]`
    pub(crate) fn new(ptr: *mut LLVMValue) -> Result<Self> {
        Ok(Self {
            ptr: NonNull::new(ptr).ok_or_else(|| {
                Error::new(
                    "Attempted to make a ValPtr from a null pointer",
                    ErrorKind::NullPtr,
                )
            })?,
            __ctx: PhantomData,
        })
    }

    pub(crate) fn as_ptr(self) -> *mut LLVMValue {
        self.ptr.as_ptr()
    }
}

/// A lifetime-constrained pointer to an LLVM type
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[repr(transparent)]
pub struct TypePtr<'ctx> {
    ptr: NonNull<LLVMType>,
    __ctx: PhantomData<&'ctx Context>,
}

impl<'ctx> TypePtr<'ctx> {
    // TODO: `#[track_caller]`
    pub(crate) fn new(ptr: *mut LLVMType) -> Result<Self> {
        Ok(Self {
            ptr: NonNull::new(ptr).ok_or_else(|| {
                Error::new(
                    "Attempted to make a TypePtr from a null pointer",
                    ErrorKind::NullPtr,
                )
            })?,
            __ctx: PhantomData,
        })
    }

    pub(crate) fn as_ptr(self) -> *mut LLVMType {
        self.ptr.as_ptr()
    }
}

pub trait Typed<'ctx> {
    /// Fetches an object's LLVM pointer
    #[doc(hidden)]
    unsafe fn llvm_ptr(&self) -> TypePtr<'ctx>;
}

macro_rules! impl_typed {
    ($struct:ident -> $field:ident) => {
        impl<'ctx> Typed<'ctx> for $struct<'ctx> {
            unsafe fn llvm_ptr(&self) -> TypePtr<'ctx> {
                self.$field
            }
        }
    };

    ($struct:ident) => {
        impl_typed!($struct->handle);
    };

    (typed($($ident:ident),*): $struct:ident -> $field:ident) => {
        impl<'ctx, $($ident),*> Typed<'ctx> for $struct<'ctx, $($ident),*> {
            unsafe fn llvm_ptr(&self) -> TypePtr<'ctx> {
                self.$field
            }
        }
    };

    (typed($($ident:ident),*): $struct:ident) => {
        impl_typed!(typed($($ident),*): $struct->handle);
    };
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[repr(transparent)]
pub struct Null<'ctx, T> {
    handle: TypePtr<'ctx>,
    ptr: PhantomData<T>,
}

impl_typed!(typed(T): Null);

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct VoidType<'ctx> {
    handle: TypePtr<'ctx>,
}

impl_typed!(VoidType);

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct IntegerType<'ctx, T> {
    bits: u32,
    handle: TypePtr<'ctx>,
    ty: PhantomData<T>,
}

impl_typed!(typed(T): IntegerType);

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct PointerType<'ctx, T> {
    addr_space: AddressSpace,
    handle: TypePtr<'ctx>,
    ty: PhantomData<T>,
}

impl_typed!(typed(T): PointerType);

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct FunctionType<'ctx, P, R> {
    params: PhantomData<P>,
    result: PhantomData<R>,
    is_var_arg: bool,
    handle: TypePtr<'ctx>,
}

impl_typed!(typed(P, R): FunctionType);

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct ArrayType<'ctx, E> {
    element_type: PhantomData<E>,
    num_elements: usize,
    handle: TypePtr<'ctx>,
}

impl_typed!(typed(E): ArrayType);
