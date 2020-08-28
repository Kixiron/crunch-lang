use crate::llvm::{
    context::Context,
    types::Type,
    utils::to_non_nul,
    values::{
        AnyValue, Argument, ArrayValue, BasicBlock, BlockAddress, FloatValue, FunctionValue,
        InlineAsm, InstructionValue, IntValue, Metadata, NullPtr, SealedAnyValue, StructValue,
        Undef, Value, ValueKind, VectorValue,
    },
    Result,
};
use llvm_sys::{
    core::{
        LLVMGetNextUse, LLVMGetUsedValue, LLVMGetUser, LLVMGetValueKind, LLVMPrintValueToString,
        LLVMTypeOf,
    },
    LLVMUse, LLVMValue,
};
use std::{
    cmp::Ordering,
    ffi::CStr,
    fmt::{Debug, Display, Formatter, LowerHex, Octal, Pointer, Result as FmtResult, UpperHex},
    hash::{Hash, Hasher},
    marker::PhantomData,
    ptr::NonNull,
};

#[derive(Copy, Clone)]
#[repr(transparent)]
pub struct Val<'ctx> {
    value: NonNull<LLVMValue>,
    __ctx: PhantomData<&'ctx Context>,
}

impl<'ctx> Val<'ctx> {
    pub(crate) unsafe fn from_raw(raw: *mut LLVMValue) -> Result<Self> {
        let block = to_non_nul(
            raw,
            "Received a null pointer from LLVM while trying to create a Val",
        )?;

        Ok(Self::from_non_nul(block))
    }

    pub(crate) const unsafe fn from_non_nul(value: NonNull<LLVMValue>) -> Self {
        Self {
            value,
            __ctx: PhantomData,
        }
    }

    pub(crate) const fn as_ptr(self) -> *const LLVMValue {
        self.value.as_ptr()
    }

    pub(crate) const fn as_mut_ptr(self) -> *mut LLVMValue {
        self.value.as_ptr()
    }

    pub(crate) const fn as_non_nul(self) -> NonNull<LLVMValue> {
        self.value
    }

    pub(crate) fn kind(self) -> ValueKind {
        let kind = unsafe { LLVMGetValueKind(self.as_mut_ptr()) };

        ValueKind::from(kind)
    }

    pub(crate) fn as_type(self) -> Result<Type<'ctx>> {
        unsafe { Type::from_raw(LLVMTypeOf(self.as_mut_ptr())) }
    }
}

impl<'ctx> PartialEq for Val<'ctx> {
    fn eq(&self, other: &Self) -> bool {
        self.value.eq(&other.value)
    }
}

impl<'ctx> Eq for Val<'ctx> {}

impl<'ctx> PartialOrd for Val<'ctx> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.value.partial_cmp(&other.value)
    }
}

impl<'ctx> Ord for Val<'ctx> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.value.cmp(&other.value)
    }
}

impl<'ctx> Hash for Val<'ctx> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.value.hash(state)
    }
}

impl Display for Val<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        let string = unsafe {
            let value = to_non_nul(
                LLVMPrintValueToString(self.as_mut_ptr()),
                "Received a null pointer from LLVM",
            )
            .expect("Failed to print a LLVM value to a string");

            CStr::from_ptr(value.as_ptr()).to_string_lossy()
        };

        f.write_str(&string)
    }
}

impl Debug for Val<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        let string = unsafe {
            let value = to_non_nul(
                LLVMPrintValueToString(self.as_mut_ptr()),
                "Received a null pointer from LLVM",
            )
            .expect("Failed to print a LLVM value to a string");

            CStr::from_ptr(value.as_ptr()).to_string_lossy()
        };

        f.write_str(&string)
    }
}

impl Pointer for Val<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "{:p}", self.as_mut_ptr())
    }
}

impl UpperHex for Val<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "{:X}", self.as_mut_ptr() as usize)
    }
}

impl LowerHex for Val<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "{:x}", self.as_mut_ptr() as usize)
    }
}

impl Octal for Val<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "{:o}", self.as_mut_ptr() as usize)
    }
}

macro_rules! passthrough_fmt {
    ($($ty:ident),* $(,)?) => {
        $(
            passthrough_fmt! {
                @inner $ty [Display, Debug, Pointer, UpperHex, LowerHex, Octal]
            }
        )*
    };

    (@inner $ty:ident [$($fmt:ident),* $(,)?]) => {
        $(
            impl $fmt for $ty<'_> {
                fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
                    $fmt::fmt(&self.as_val(), f)
                }
            }
        )*
    };
}

// TODO: Test all of these
passthrough_fmt! {
    IntValue, Value, BasicBlock,
    FunctionValue, BlockAddress,
    ArrayValue, StructValue, VectorValue,
    FloatValue, NullPtr, Undef,
    Argument, InlineAsm,
    Metadata, InstructionValue,
}

#[derive(Copy, Clone)]
pub struct ValueUsage<'ctx> {
    usage: NonNull<LLVMUse>,
    __ctx: PhantomData<&'ctx Context>,
}

impl<'ctx> ValueUsage<'ctx> {
    pub fn get_user(self) -> Result<Value<'ctx>> {
        let user = unsafe {
            to_non_nul(
                LLVMGetUser(self.as_mut_ptr()),
                "Failed to get a ValueUsage's user",
            )?
        };

        Ok(unsafe { Value::from_non_nul(user) })
    }

    pub fn get_value(self) -> Result<Value<'ctx>> {
        let used = unsafe {
            to_non_nul(
                LLVMGetUsedValue(self.as_mut_ptr()),
                "Failed to get a ValueUsage's value",
            )?
        };

        Ok(unsafe { Value::from_non_nul(used) })
    }

    pub fn replace_all_uses(self, new: Value<'ctx>) -> Result<()> {
        self.get_value()?.replace_all_uses(new);

        Ok(())
    }
}

impl<'ctx> ValueUsage<'ctx> {
    pub(crate) const unsafe fn from_non_nul(usage: NonNull<LLVMUse>) -> Self {
        Self {
            usage,
            __ctx: PhantomData,
        }
    }

    pub(crate) const fn as_mut_ptr(self) -> *mut LLVMUse {
        self.usage.as_ptr()
    }
}

impl<'ctx> PartialEq for ValueUsage<'ctx> {
    fn eq(&self, other: &Self) -> bool {
        self.usage.eq(&other.usage)
    }
}

impl<'ctx> Eq for ValueUsage<'ctx> {}

impl<'ctx> PartialOrd for ValueUsage<'ctx> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.usage.partial_cmp(&other.usage)
    }
}

impl<'ctx> Ord for ValueUsage<'ctx> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.usage.cmp(&other.usage)
    }
}

impl<'ctx> Hash for ValueUsage<'ctx> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.usage.hash(state)
    }
}

impl<'ctx> Iterator for ValueUsage<'ctx> {
    type Item = Self;

    fn next(&mut self) -> Option<Self::Item> {
        unsafe {
            let usage = LLVMGetNextUse(self.as_mut_ptr());

            NonNull::new(usage).map(|usage| Self::from_non_nul(usage))
        }
    }
}
