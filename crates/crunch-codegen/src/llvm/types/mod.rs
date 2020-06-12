mod sealed;
mod ty;

pub(crate) use sealed::SealedAnyType;
pub(crate) use ty::Ty;
pub use ty::TypeKind;

use crate::llvm::{
    values::{AnyValue, Int, SealedAnyValue, Value},
    Context, Error, ErrorKind, Result,
};
use llvm_sys::core::{LLVMConstInt, LLVMInt32TypeInContext, LLVMSizeOf, LLVMVoidTypeInContext};
use std::fmt::Debug;

pub trait AnyType<'ctx>: SealedAnyType<'ctx> + Debug + Sized {
    fn kind(self) -> TypeKind {
        self.as_ty().kind()
    }

    fn size_of(self) -> Result<Int<'ctx>> {
        let size: Value<'ctx> = unsafe { Value::from_raw(LLVMSizeOf(self.as_ty().as_mut_ptr()))? };

        size.downcast::<Int<'ctx>>().ok_or_else(|| {
            Error::new(
                "LLVM returned a non-integer for the size of a type",
                ErrorKind::LLVMError,
            )
        })
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum Type<'ctx> {
    Array(Array<'ctx>),
    Float(Float<'ctx>),
    FunctionSig(FunctionSig<'ctx>),
    Int(IntTy<'ctx>),
    Pointer(Pointer<'ctx>),
    Struct(Struct<'ctx>),
    Vector(Vector<'ctx>),
    Void(Void<'ctx>),
}

impl<'ctx> AnyType<'ctx> for Type<'ctx> {}

impl<'ctx> SealedAnyType<'ctx> for Type<'ctx> {
    fn from_ty(ty: Ty<'ctx>) -> Self {
        match ty.kind() {
            TypeKind::Array => Self::Array(Array::from_ty(ty)),
            TypeKind::Float => Self::Float(Float::from_ty(ty)),
            TypeKind::Function => Self::FunctionSig(FunctionSig::from_ty(ty)),
            TypeKind::Integer => Self::Int(IntTy::from_ty(ty)),
            TypeKind::Pointer => Self::Pointer(Pointer::from_ty(ty)),
            TypeKind::Struct => Self::Struct(Struct::from_ty(ty)),
            TypeKind::Vector => Self::Vector(Vector::from_ty(ty)),
            TypeKind::Void => Self::Void(Void::from_ty(ty)),

            unhandled => unimplemented!("{:?} has not been implemented as a Type yet", unhandled),
        }
    }

    fn as_ty(&self) -> Ty<'ctx> {
        match self {
            Self::Array(array) => array.as_ty(),
            Self::Float(float) => float.as_ty(),
            Self::FunctionSig(function) => function.as_ty(),
            Self::Int(int) => int.as_ty(),
            Self::Pointer(pointer) => pointer.as_ty(),
            Self::Struct(structure) => structure.as_ty(),
            Self::Vector(vector) => vector.as_ty(),
            Self::Void(void) => void.as_ty(),
        }
    }
}

macro_rules! impl_any_ty {
    ($($ty:ident),* $(,)?) => {
        $(
            impl<'ctx> AnyType<'ctx> for $ty<'ctx> {}

            impl<'ctx> SealedAnyType<'ctx> for $ty<'ctx> {
                fn as_ty(&self) -> Ty<'ctx> {
                    self.0
                }

                fn from_ty(ty: Ty<'ctx>) -> Self {
                    Self(ty)
                }
            }
        )*
    };
}

impl_any_ty! {
    FunctionSig, Void, IntTy,
    Array, Struct, Vector,
    Float, Pointer,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct Array<'ctx>(Ty<'ctx>);

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct Float<'ctx>(Ty<'ctx>);

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct FunctionSig<'ctx>(Ty<'ctx>);

impl<'ctx> Into<Type<'ctx>> for FunctionSig<'ctx> {
    fn into(self) -> Type<'ctx> {
        Type::FunctionSig(self)
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct IntTy<'ctx>(Ty<'ctx>);

impl<'ctx> IntTy<'ctx> {
    pub fn i32(ctx: &'ctx Context) -> Result<Self> {
        unsafe { Self::from_raw(LLVMInt32TypeInContext(ctx.as_mut_ptr())) }
    }

    pub fn constant(self, value: u64, sign_extend: bool) -> Result<Int<'ctx>> {
        unsafe {
            let value = LLVMConstInt(self.as_mut_ptr(), value, sign_extend as i32);

            Int::from_raw(value)
        }
    }
}

impl<'ctx> Into<Type<'ctx>> for IntTy<'ctx> {
    fn into(self) -> Type<'ctx> {
        Type::Int(self)
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct Pointer<'ctx>(Ty<'ctx>);

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct Struct<'ctx>(Ty<'ctx>);

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct Vector<'ctx>(Ty<'ctx>);

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct Void<'ctx>(Ty<'ctx>);

impl<'ctx> Void<'ctx> {
    pub fn new(ctx: &'ctx Context) -> Result<Self> {
        unsafe { Self::from_raw(LLVMVoidTypeInContext(ctx.as_mut_ptr())) }
    }
}

impl<'ctx> Into<Type<'ctx>> for Void<'ctx> {
    fn into(self) -> Type<'ctx> {
        Type::Void(self)
    }
}
