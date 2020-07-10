use crate::llvm::{
    context::Context,
    types::{ArrayType, PointerType, SealedAnyType, TypeKind},
    utils::{to_non_nul, AddressSpace},
    Result,
};
use llvm_sys::{
    core::{
        LLVMArrayType, LLVMGetElementType, LLVMGetTypeKind, LLVMPointerType, LLVMPrintTypeToString,
    },
    LLVMType,
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
pub struct Type<'ctx> {
    ty: NonNull<LLVMType>,
    __ctx: PhantomData<&'ctx Context>,
}

impl<'ctx> Type<'ctx> {
    pub(crate) fn make_pointer(self, address_space: AddressSpace) -> Result<PointerType<'ctx>> {
        unsafe {
            PointerType::from_raw(LLVMPointerType(
                self.as_mut_ptr(),
                address_space as u8 as u32,
            ))
        }
    }

    pub(crate) fn make_array(self, len: u32) -> Result<ArrayType<'ctx>> {
        unsafe { ArrayType::from_raw(LLVMArrayType(self.as_mut_ptr(), len)) }
    }

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

    pub(crate) fn element_type(self) -> Result<Type<'ctx>> {
        unsafe { Type::from_raw(LLVMGetElementType(self.as_mut_ptr())) }
    }
}

impl Display for Type<'_> {
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

impl Debug for Type<'_> {
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

impl FmtPointer for Type<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "{:p}", self.as_mut_ptr())
    }
}

impl UpperHex for Type<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "{:X}", self.as_mut_ptr() as usize)
    }
}

impl LowerHex for Type<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "{:x}", self.as_mut_ptr() as usize)
    }
}

impl Octal for Type<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "{:o}", self.as_mut_ptr() as usize)
    }
}
