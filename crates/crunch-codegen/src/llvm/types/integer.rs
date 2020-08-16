use crate::llvm::{
    types::{AnyType, SealedAnyType, Type},
    utils::{to_non_nul, Sealed},
    values::{IntValue, Pointable, SealedAnyValue},
    Context, Result,
};
use llvm_sys::{
    core::{
        LLVMConstInt, LLVMGetIntTypeWidth, LLVMInt128TypeInContext, LLVMInt16TypeInContext,
        LLVMInt1TypeInContext, LLVMInt32TypeInContext, LLVMInt64TypeInContext,
        LLVMInt8TypeInContext, LLVMIntTypeInContext,
    },
    LLVMType,
};
use std::{
    fmt::{
        Debug, Display, Formatter, LowerHex, Octal, Pointer as FmtPointer, Result as FmtResult,
        UpperHex,
    },
    marker::PhantomData,
    ptr::NonNull,
};

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct IntType<'ctx, W: IntWidth>(Type<'ctx>, PhantomData<W>);

impl<'ctx, W: IntWidth> IntType<'ctx, W> {
    #[allow(clippy::new_ret_no_self)]
    pub fn new(ctx: &'ctx Context, bits: u32) -> Result<IntType<'ctx, Unknown>> {
        unsafe { IntType::from_raw(LLVMIntTypeInContext(ctx.as_mut_ptr(), bits)) }
    }

    pub fn width(self) -> u32 {
        unsafe { LLVMGetIntTypeWidth(self.as_mut_ptr()) }
    }

    // pub fn into_array(self, len: u32) -> Result<Vector<'ctx, Self>> {}

    pub fn constant(self, value: u64, sign_extend: bool) -> Result<IntValue<'ctx>> {
        unsafe {
            let value = LLVMConstInt(self.as_mut_ptr(), value, sign_extend as i32);

            IntValue::from_raw(value)
        }
    }

    pub fn erase(self) -> IntType<'ctx, Unknown> {
        IntType(self.0, PhantomData)
    }
}

impl<'ctx, W: IntWidth> IntType<'ctx, W> {
    pub(crate) fn as_ty(&self) -> Type<'ctx> {
        self.0
    }

    pub(crate) unsafe fn from_raw(raw: *mut LLVMType) -> Result<Self> {
        let non_nul = to_non_nul(
            raw,
            "Received a null pointer from LLVM while trying to create a Type",
        )?;

        Ok(Self::from_non_nul(non_nul))
    }

    pub(crate) unsafe fn from_non_nul(ty: NonNull<LLVMType>) -> Self {
        Self(Type::from_non_nul(ty), PhantomData)
    }

    pub(crate) fn as_mut_ptr(&self) -> *mut LLVMType {
        self.as_ty().as_mut_ptr()
    }
}

macro_rules! create_ints {
    ($($name:ident, $func:ident => $ty:ty),* $(,)?) => {
        $(
            impl<'ctx> IntType<'ctx, $ty> {
                pub fn $name(ctx: &'ctx Context) -> Result<IntType<'ctx, $ty>> {
                    unsafe { IntType::from_raw($func(ctx.as_mut_ptr())) }
                }
            }
        )*
    };
}

create_ints! {
    i1,   LLVMInt1TypeInContext   => I1,
    i8,   LLVMInt8TypeInContext   => i8,
    u8,   LLVMInt8TypeInContext   => u8,
    i16,  LLVMInt16TypeInContext  => i16,
    u16,  LLVMInt16TypeInContext  => u16,
    i32,  LLVMInt32TypeInContext  => i32,
    u32,  LLVMInt32TypeInContext  => u32,
    i64,  LLVMInt64TypeInContext  => i64,
    u64,  LLVMInt64TypeInContext  => u64,
    i128, LLVMInt128TypeInContext => i128,
    u128, LLVMInt128TypeInContext => u128,
}

impl<'ctx, W: IntWidth> AnyType<'ctx> for IntType<'ctx, W> {}

impl<'ctx, W: IntWidth> SealedAnyType<'ctx> for IntType<'ctx, W> {
    fn as_ty(&self) -> Type<'ctx> {
        self.0
    }

    fn from_ty(value: Type<'ctx>) -> Self {
        Self(value, PhantomData)
    }
}

impl<'ctx, T: IntWidth> Sealed for IntType<'ctx, T> {}
impl<'ctx, T: IntWidth> Pointable for IntType<'ctx, T> {}

impl<'ctx, W: IntWidth> Into<Type<'ctx>> for IntType<'ctx, W> {
    fn into(self) -> Type<'ctx> {
        self.0
    }
}

pub trait IntWidth: Sealed {}

pub struct I1(());

impl IntWidth for I1 {}
impl Sealed for I1 {}

pub struct Unknown(());

impl IntWidth for Unknown {}
impl Sealed for Unknown {}

macro_rules! int_width {
    ($($width:ty),* $(,)?) => {
        $(
            impl IntWidth for $width {}
            impl Sealed for $width {}
        )*
    };
}

int_width! {
    i8,   u8,
    i16,  u16,
    i32,  u32,
    i64,  u64,
    i128, u128,
}

macro_rules! fmt {
    () => {
        fmt! {
            @inner [Display, Debug, FmtPointer, UpperHex, LowerHex, Octal]
        }
    };

    (@inner [$($fmt:ident),* $(,)?]) => {
        $(
            impl<W: IntWidth> $fmt for IntType<'_, W> {
                fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
                    $fmt::fmt(&self.as_ty(), f)
                }
            }
        )*
    };
}

fmt!();
