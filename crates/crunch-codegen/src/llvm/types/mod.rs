mod integer;
mod sealed_any_type;
mod ty;
mod type_kind;

pub use integer::{IntType, IntWidth, Unknown, I1};
pub(crate) use sealed_any_type::SealedAnyType;
pub(crate) use ty::Type;
pub use type_kind::TypeKind;

use crate::llvm::{
    utils::{AddressSpace, Sealed},
    values::{AnyValue, IntValue, SealedAnyValue, Value},
    Context, Error, ErrorKind, Result,
};
use llvm_sys::{
    core::{
        LLVMArrayType, LLVMCountParamTypes, LLVMGetElementType, LLVMGetParamTypes,
        LLVMGetVectorSize, LLVMPointerType, LLVMSizeOf, LLVMVoidTypeInContext,
    },
    LLVMType,
};
use std::{
    fmt::{Debug, Formatter, Result as FmtResult},
    marker::PhantomData,
    mem::MaybeUninit,
};

pub trait AnyType<'ctx>: SealedAnyType<'ctx> + Sized {
    fn kind(self) -> TypeKind {
        self.as_ty().kind()
    }

    fn size_of(self) -> Result<IntValue<'ctx>> {
        let size: Value<'ctx> = unsafe { Value::from_raw(LLVMSizeOf(self.as_ty().as_mut_ptr()))? };

        size.downcast::<IntValue<'ctx>>().ok_or_else(|| {
            Error::new(
                "LLVM returned a non-integer for the size of a type",
                ErrorKind::LLVMError,
            )
        })
    }

    fn make_pointer(self, address_space: AddressSpace) -> Result<PointerType<'ctx>> {
        unsafe {
            PointerType::from_raw(LLVMPointerType(
                self.as_mut_ptr(),
                address_space as u8 as u32,
            ))
        }
    }

    fn make_array(self, len: u32) -> Result<ArrayType<'ctx>> {
        unsafe { ArrayType::from_raw(LLVMArrayType(self.as_mut_ptr(), len)) }
    }

    fn element_type(self) -> Result<Type<'ctx>> {
        unsafe { Type::from_raw(LLVMGetElementType(self.as_mut_ptr())) }
    }
}

macro_rules! impl_any_ty {
    ($($ty:ident),* $(,)?) => {
        $(
            impl<'ctx> AnyType<'ctx> for $ty<'ctx> {}

            impl<'ctx> SealedAnyType<'ctx> for $ty<'ctx> {
                fn as_ty(&self) -> Type<'ctx> {
                    self.0
                }

                fn from_ty(ty: Type<'ctx>) -> Self {
                    Self(ty)
                }
            }

            impl<'ctx> Into<Type<'ctx>> for $ty<'ctx> {
                fn into(self) -> Type<'ctx> {
                    self.0
                }
            }
        )*
    };
}

impl_any_ty! {
    FunctionSig, VoidType,
    ArrayType, StructType,
    FloatType, PointerType,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct ArrayType<'ctx>(Type<'ctx>);

impl<'ctx> Sealed for ArrayType<'ctx> {}

impl Debug for ArrayType<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        Debug::fmt(&self.0, f)
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct FloatType<'ctx>(Type<'ctx>);

impl<'ctx> Sealed for FloatType<'ctx> {}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct FunctionSig<'ctx>(Type<'ctx>);

impl<'ctx> FunctionSig<'ctx> {
    pub fn args(self) -> Result<Vec<Type<'ctx>>> {
        unsafe {
            let mut args: Vec<MaybeUninit<*mut LLVMType>> =
                vec![MaybeUninit::zeroed(); self.num_args() as usize];
            LLVMGetParamTypes(self.as_mut_ptr(), args.as_mut_ptr() as *mut *mut LLVMType);

            args.into_iter()
                .map(|arg| Type::from_raw(arg.assume_init()))
                .collect()
        }
    }

    pub fn num_args(self) -> u32 {
        unsafe { LLVMCountParamTypes(self.as_mut_ptr()) }
    }

    pub(crate) const unsafe fn from(ty: Type<'ctx>) -> Self {
        Self(ty)
    }
}

impl<'ctx> Sealed for FunctionSig<'ctx> {}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct PointerType<'ctx>(pub(crate) Type<'ctx>);

impl<'ctx> Sealed for PointerType<'ctx> {}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct StructType<'ctx>(Type<'ctx>);

impl<'ctx> Sealed for StructType<'ctx> {}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct VectorType<'ctx, E>(Type<'ctx>, PhantomData<E>);

impl<'ctx, E> VectorType<'ctx, E> {
    pub fn len(self) -> u32 {
        unsafe { LLVMGetVectorSize(self.as_mut_ptr()) }
    }
}

impl<'ctx, E> AnyType<'ctx> for VectorType<'ctx, E> {}

impl<'ctx, E> SealedAnyType<'ctx> for VectorType<'ctx, E> {
    fn as_ty(&self) -> Type<'ctx> {
        self.0
    }

    fn from_ty(ty: Type<'ctx>) -> Self {
        Self(ty, PhantomData)
    }
}

impl<'ctx, T: Sealed> Sealed for VectorType<'ctx, T> {}

impl<'ctx, E> Into<Type<'ctx>> for VectorType<'ctx, E> {
    fn into(self) -> Type<'ctx> {
        self.0
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct VoidType<'ctx>(Type<'ctx>);

impl<'ctx> VoidType<'ctx> {
    pub fn new(ctx: &'ctx Context) -> Result<Self> {
        unsafe { Self::from_raw(LLVMVoidTypeInContext(ctx.as_mut_ptr())) }
    }
}
