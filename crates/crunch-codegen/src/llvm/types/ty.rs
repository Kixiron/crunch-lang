use crate::llvm::{
    context::Context,
    types::{Array, Float, FunctionSig, IntTy, Pointer, SealedAnyType, Struct, Type, Vector, Void},
    utils::to_non_nul,
    Result,
};
use llvm_sys::{
    core::{LLVMGetTypeKind, LLVMPrintTypeToString},
    LLVMType, LLVMTypeKind,
};
use std::{
    ffi::CStr,
    fmt::{
        Debug, Display, Formatter, LowerHex, Octal, Pointer as FmtPointer, Result as FmtResult,
        UpperHex,
    },
    hash::Hash,
    marker::PhantomData,
    ptr::NonNull,
};

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct Ty<'ctx> {
    ty: NonNull<LLVMType>,
    __ctx: PhantomData<&'ctx Context>,
}

impl<'ctx> Ty<'ctx> {
    #[inline]
    pub(crate) unsafe fn from_raw(raw: *mut LLVMType) -> Result<Self> {
        let ty = to_non_nul(
            raw,
            "Received a null pointer from LLVM while trying to create a Ty",
        )?;

        Ok(Self::from_non_nul(ty))
    }

    pub(crate) const unsafe fn from_non_nul(ty: NonNull<LLVMType>) -> Self {
        Self {
            ty,
            __ctx: PhantomData,
        }
    }

    pub(crate) const fn as_ptr(self) -> *const LLVMType {
        self.ty.as_ptr()
    }

    pub(crate) const fn as_mut_ptr(self) -> *mut LLVMType {
        self.ty.as_ptr()
    }

    pub(crate) const fn as_non_nul(self) -> NonNull<LLVMType> {
        self.ty
    }

    pub(crate) fn kind(self) -> TypeKind {
        let kind = unsafe { LLVMGetTypeKind(self.as_mut_ptr()) };

        TypeKind::from(kind)
    }
}

impl Display for Ty<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        let string = unsafe {
            let value = to_non_nul(
                LLVMPrintTypeToString(self.as_mut_ptr()),
                "Received a null pointer from LLVM",
            )
            .expect("Failed to print a LLVM type to a string");

            CStr::from_ptr(value.as_ptr()).to_string_lossy()
        };

        f.write_str(&string)
    }
}

impl Debug for Ty<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        let string = unsafe {
            let value = to_non_nul(
                LLVMPrintTypeToString(self.as_mut_ptr()),
                "Received a null pointer from LLVM",
            )
            .expect("Failed to print a LLVM type to a string");

            CStr::from_ptr(value.as_ptr()).to_string_lossy()
        };

        f.write_str(&string)
    }
}

impl FmtPointer for Ty<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "{:p}", self.as_mut_ptr())
    }
}

impl UpperHex for Ty<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "{:X}", self.as_mut_ptr() as usize)
    }
}

impl LowerHex for Ty<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "{:x}", self.as_mut_ptr() as usize)
    }
}

impl Octal for Ty<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "{:o}", self.as_mut_ptr() as usize)
    }
}

macro_rules! passthrough_fmt {
    ($($ty:ident),* $(,)?) => {
        $(
            passthrough_fmt! {
                @inner $ty [Display, Debug, FmtPointer, UpperHex, LowerHex, Octal]
            }
        )*
    };

    (@inner $ty:ident [$($fmt:ident),* $(,)?]) => {
        $(
            impl $fmt for $ty<'_> {
                fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
                    $fmt::fmt(&self.as_ty(), f)
                }
            }
        )*
    };
}

// TODO: Test all of these
passthrough_fmt! {
    Array, Float, IntTy,
    FunctionSig, Pointer,
    Struct, Vector,
    Void, Type,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
#[allow(non_camel_case_types)]
pub enum TypeKind {
    Void,
    Half,
    Float,
    Double,
    X86_FP80,
    FP128,
    PC_FP128,
    Label,
    Integer,
    Function,
    Struct,
    Array,
    Pointer,
    Vector,
    Metadata,
    X86_MMX,
    Token,
}

#[rustfmt::skip]
impl From<LLVMTypeKind> for TypeKind {
    fn from(kind: LLVMTypeKind) -> Self {
        match kind {
            LLVMTypeKind::LLVMVoidTypeKind      => Self::Void,
            LLVMTypeKind::LLVMHalfTypeKind      => Self::Half,
            LLVMTypeKind::LLVMFloatTypeKind     => Self::Float,
            LLVMTypeKind::LLVMDoubleTypeKind    => Self::Double,
            LLVMTypeKind::LLVMX86_FP80TypeKind  => Self::X86_FP80,
            LLVMTypeKind::LLVMFP128TypeKind     => Self::FP128,
            LLVMTypeKind::LLVMPPC_FP128TypeKind => Self::PC_FP128,
            LLVMTypeKind::LLVMLabelTypeKind     => Self::Label,
            LLVMTypeKind::LLVMIntegerTypeKind   => Self::Integer,
            LLVMTypeKind::LLVMFunctionTypeKind  => Self::Function,
            LLVMTypeKind::LLVMStructTypeKind    => Self::Struct,
            LLVMTypeKind::LLVMArrayTypeKind     => Self::Array,
            LLVMTypeKind::LLVMPointerTypeKind   => Self::Pointer,
            LLVMTypeKind::LLVMVectorTypeKind    => Self::Vector,
            LLVMTypeKind::LLVMMetadataTypeKind  => Self::Metadata,
            LLVMTypeKind::LLVMX86_MMXTypeKind   => Self::X86_MMX,
            LLVMTypeKind::LLVMTokenTypeKind     => Self::Token,
        }
    }
}
