#![allow(dead_code)]

use crate::llvm::{
    utils::{to_non_nul, AddressSpace},
    Context, Error, ErrorKind, Result,
};
use core::{fmt, marker::PhantomData, ptr::NonNull};
use llvm_sys::{
    core::{
        LLVMConstInt, LLVMIntTypeInContext, LLVMIsConstant, LLVMPrintTypeToString,
        LLVMPrintValueToString, LLVMTypeOf,
    },
    LLVMType, LLVMValue,
};
use std::ffi::CStr;
use typenum::{consts::P1, Integer, NonZero, Unsigned};

/// Non-terminating instructions.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Instruction<'ctx, T: ?Sized> {
    Add(Add<'ctx, T>),
    Sub(Sub<'ctx, T>),
    Mul(Mul<'ctx, T>),
    UDiv(UDiv<'ctx, T>),
    SDiv(SDiv<'ctx, T>),
    URem(URem<'ctx, T>),
    SRem(SRem<'ctx, T>),
    Ret(Ret<'ctx, T>),
    Unreachable(Unreachable<'ctx>),
    ICmp(ICmp<'ctx>),
}

impl<'ctx, T> Instruction<'ctx, T> {
    pub fn is_binary_op(&self) -> bool {
        matches!(
            self,
            Self::Add(..)
                | Self::Sub(..)
                | Self::Mul(..)
                | Self::UDiv(..)
                | Self::SDiv(..)
                | Self::URem(..)
                | Self::SRem(..)
        )
    }

    pub fn is_ret(&self) -> bool {
        matches!(self, Self::Ret(..))
    }

    pub fn is_unreachable(&self) -> bool {
        matches!(self, Self::Unreachable(..))
    }

    pub fn erase(self) -> Instruction<'ctx, Erased> {
        unsafe { std::mem::transmute::<Instruction<'ctx, T>, Instruction<'ctx, Erased>>(self) }
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
    fn llvm_ptr(&self) -> ValPtr<'ctx>;

    fn get_type(&self) -> Result<TypePtr<'ctx>> {
        unsafe { TypePtr::new(LLVMTypeOf(self.llvm_ptr().as_ptr())) }
    }
}

macro_rules! impl_valued {
    (typed($($ident:ident),*): $struct:ident -> $field:ident) => {
        impl<'ctx, $($ident),*> Valued<'ctx> for $struct<'ctx, $($ident),*> {
            fn llvm_ptr(&self) -> ValPtr<'ctx> {
                self.$field
            }
        }
    };

    (typed($($ident:ident),*): $struct:ident) => {
        impl_valued!(typed($($ident),*): $struct->handle);
    };
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Add<'ctx, T: ?Sized> {
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

impl<'ctx, T> Into<Instruction<'ctx, T>> for Add<'ctx, T> {
    fn into(self) -> Instruction<'ctx, T> {
        Instruction::Add(self)
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
pub struct Sub<'ctx, T: ?Sized> {
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

impl<'ctx, T> Into<Instruction<'ctx, T>> for Sub<'ctx, T> {
    fn into(self) -> Instruction<'ctx, T> {
        Instruction::Sub(self)
    }
}

impl<'ctx, T> Into<IntValue<'ctx, T>> for Sub<'ctx, T> {
    fn into(self) -> IntValue<'ctx, T> {
        IntValue {
            handle: self.handle,
            ty: self.ty,
        }
    }
}

impl_valued!(typed(T): Sub);

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Mul<'ctx, T: ?Sized> {
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

impl<'ctx, T> Into<Instruction<'ctx, T>> for Mul<'ctx, T> {
    fn into(self) -> Instruction<'ctx, T> {
        Instruction::Mul(self)
    }
}

impl<'ctx, T> Into<IntValue<'ctx, T>> for Mul<'ctx, T> {
    fn into(self) -> IntValue<'ctx, T> {
        IntValue {
            handle: self.handle,
            ty: self.ty,
        }
    }
}

impl_valued!(typed(T): Mul);

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct UDiv<'ctx, T: ?Sized> {
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

impl<'ctx, T> Into<Instruction<'ctx, T>> for UDiv<'ctx, T> {
    fn into(self) -> Instruction<'ctx, T> {
        Instruction::UDiv(self)
    }
}

impl<'ctx, T> Into<IntValue<'ctx, T>> for UDiv<'ctx, T> {
    fn into(self) -> IntValue<'ctx, T> {
        IntValue {
            handle: self.handle,
            ty: self.ty,
        }
    }
}

impl_valued!(typed(T): UDiv);

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct SDiv<'ctx, T: ?Sized> {
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

impl<'ctx, T> Into<Instruction<'ctx, T>> for SDiv<'ctx, T> {
    fn into(self) -> Instruction<'ctx, T> {
        Instruction::SDiv(self)
    }
}

impl<'ctx, T> Into<IntValue<'ctx, T>> for SDiv<'ctx, T> {
    fn into(self) -> IntValue<'ctx, T> {
        IntValue {
            handle: self.handle,
            ty: self.ty,
        }
    }
}

impl_valued!(typed(T): SDiv);

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct URem<'ctx, T: ?Sized> {
    handle: ValPtr<'ctx>,
    ty: PhantomData<T>,
}

impl<'ctx, T> Into<Instruction<'ctx, T>> for URem<'ctx, T> {
    fn into(self) -> Instruction<'ctx, T> {
        Instruction::URem(self)
    }
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
pub struct SRem<'ctx, T: ?Sized> {
    handle: ValPtr<'ctx>,
    ty: PhantomData<T>,
}

impl<'ctx, T> Into<Instruction<'ctx, T>> for SRem<'ctx, T> {
    fn into(self) -> Instruction<'ctx, T> {
        Instruction::SRem(self)
    }
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
pub struct Ret<'ctx, T: ?Sized> {
    handle: ValPtr<'ctx>,
    ty: PhantomData<T>,
}

impl<'ctx, T> Ret<'ctx, T> {
    pub(crate) unsafe fn from_raw(raw: *mut LLVMValue) -> Result<Self> {
        Ok(Self {
            handle: ValPtr::new(raw)?,
            ty: PhantomData,
        })
    }
}

impl<'ctx, T> Into<Instruction<'ctx, T>> for Ret<'ctx, T> {
    fn into(self) -> Instruction<'ctx, T> {
        Instruction::Ret(self)
    }
}

impl_valued!(typed(T): Ret);

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct ICmp<'ctx> {
    handle: ValPtr<'ctx>,
}

impl<'ctx> ICmp<'ctx> {
    pub(crate) unsafe fn from_raw(raw: *mut LLVMValue) -> Result<Self> {
        Ok(Self {
            handle: ValPtr::new(raw)?,
        })
    }
}

impl<'ctx> Into<Instruction<'ctx, ()>> for ICmp<'ctx> {
    fn into(self) -> Instruction<'ctx, ()> {
        Instruction::ICmp(self)
    }
}

impl<'ctx> Into<BoolValue<'ctx>> for ICmp<'ctx> {
    fn into(self) -> BoolValue<'ctx> {
        BoolValue {
            handle: self.handle,
        }
    }
}

impl<'ctx> Valued<'ctx> for ICmp<'ctx> {
    fn llvm_ptr(&self) -> ValPtr<'ctx> {
        self.handle
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Unreachable<'ctx> {
    handle: ValPtr<'ctx>,
}

impl<'ctx> Unreachable<'ctx> {
    pub(crate) unsafe fn from_raw(raw: *mut LLVMValue) -> Result<Self> {
        Ok(Self {
            handle: ValPtr::new(raw)?,
        })
    }
}

impl<'ctx> Into<Instruction<'ctx, ()>> for Unreachable<'ctx> {
    fn into(self) -> Instruction<'ctx, ()> {
        Instruction::Unreachable(self)
    }
}

impl<'ctx> Valued<'ctx> for Unreachable<'ctx> {
    fn llvm_ptr(&self) -> ValPtr<'ctx> {
        self.handle
    }
}

pub enum Value<'ctx, T: ?Sized> {
    Int(IntValue<'ctx, T>),
    Bool(BoolValue<'ctx>),
}

impl<'ctx, T> Value<'ctx, T> {
    pub fn erase(self) -> Value<'ctx, Erased> {
        unsafe { std::mem::transmute::<Value<'ctx, T>, Value<'ctx, Erased>>(self) }
    }
}

impl<'ctx, T: ?Sized> Valued<'ctx> for Value<'ctx, T> {
    fn llvm_ptr(&self) -> ValPtr<'ctx> {
        match self {
            Self::Int(int) => int.llvm_ptr(),
            Self::Bool(boolean) => boolean.llvm_ptr(),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct IntValue<'ctx, T: ?Sized> {
    handle: ValPtr<'ctx>,
    ty: PhantomData<T>,
}

impl<'ctx, T> IntValue<'ctx, T> {
    // FIXME: Verify the size of the provided integer somehow?
    pub fn new(value: u64, ty: IntegerType<'ctx, T>, sign_extended: bool) -> Result<Self> {
        unsafe {
            Self::from_raw(LLVMConstInt(
                ty.llvm_ptr().as_ptr(),
                value,
                sign_extended as i32,
            ))
        }
    }

    pub fn erase(self) -> IntValue<'ctx, Erased> {
        IntValue {
            handle: self.handle,
            ty: PhantomData,
        }
    }

    pub(crate) unsafe fn from_raw(raw: *mut LLVMValue) -> Result<Self> {
        Ok(Self {
            handle: ValPtr::new(raw)?,
            ty: PhantomData,
        })
    }
}

impl<'ctx> IntValue<'ctx, P1> {
    pub const fn as_bool(self) -> BoolValue<'ctx> {
        BoolValue {
            handle: self.handle,
        }
    }
}

impl<'ctx, T> Into<Value<'ctx, T>> for IntValue<'ctx, T> {
    fn into(self) -> Value<'ctx, T> {
        Value::Int(self)
    }
}

impl<'ctx, T: ?Sized> Valued<'ctx> for IntValue<'ctx, T> {
    fn llvm_ptr(&self) -> ValPtr<'ctx> {
        self.handle
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct BoolValue<'ctx> {
    handle: ValPtr<'ctx>,
}

impl<'ctx> BoolValue<'ctx> {
    pub const fn as_i1(self) -> IntValue<'ctx, P1> {
        IntValue {
            handle: self.handle,
            ty: PhantomData,
        }
    }

    pub(crate) unsafe fn from_raw(raw: *mut LLVMValue) -> Result<Self> {
        Ok(Self {
            handle: ValPtr::new(raw)?,
        })
    }
}

impl<'ctx> Into<Value<'ctx, bool>> for BoolValue<'ctx> {
    fn into(self) -> Value<'ctx, bool> {
        Value::Bool(self)
    }
}

impl_valued!(typed(): BoolValue);

/// A lifetime-constrained pointer to an LLVM value
#[derive(Copy, Clone, PartialEq, Eq)]
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

impl fmt::Debug for ValPtr<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let string = unsafe {
            let value = to_non_nul(
                LLVMPrintValueToString(self.ptr.as_ptr()),
                "Received a null pointer from LLVM",
            )
            .expect("Failed to print a ValPtr to a string");

            CStr::from_ptr(value.as_ptr()).to_string_lossy()
        };

        f.debug_struct("ValPtr")
            .field("llvm_value", &string)
            .field("address", &self.ptr)
            .finish()
    }
}

/// A lifetime-constrained pointer to an LLVM type
#[derive(Copy, Clone, PartialEq, Eq)]
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

impl fmt::Debug for TypePtr<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let string = unsafe {
            let value = to_non_nul(
                LLVMPrintTypeToString(self.ptr.as_ptr()),
                "Received a null pointer from LLVM",
            )
            .expect("Failed to print a TypePtr to a string");

            CStr::from_ptr(value.as_ptr()).to_string_lossy()
        };

        f.debug_struct("TypePtr")
            .field("llvm_type", &string)
            .field("address", &self.ptr)
            .finish()
    }
}

pub trait Typed<'ctx> {
    /// Fetches an object's LLVM pointer
    #[doc(hidden)]
    fn llvm_ptr(&self) -> TypePtr<'ctx>;
}

macro_rules! impl_typed {
    ($struct:ident -> $field:ident) => {
        impl<'ctx> Typed<'ctx> for $struct<'ctx> {
            fn llvm_ptr(&self) -> TypePtr<'ctx> {
                self.$field
            }
        }
    };

    ($struct:ident) => {
        impl_typed!($struct->handle);
    };

    (typed($($ident:ident),*): $struct:ident -> $field:ident) => {
        impl<'ctx, $($ident),*> Typed<'ctx> for $struct<'ctx, $($ident),*> {
            fn llvm_ptr(&self) -> TypePtr<'ctx> {
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
pub struct Null<'ctx, T: ?Sized> {
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
pub struct IntegerType<'ctx, T: ?Sized> {
    handle: TypePtr<'ctx>,
    ty: PhantomData<T>,
}

impl<'ctx, T> IntegerType<'ctx, T> {
    pub(crate) fn from_raw(ptr: *mut LLVMType) -> Result<Self> {
        Ok(Self {
            handle: TypePtr::new(ptr)?,
            ty: PhantomData,
        })
    }
}

impl<'ctx, Int: Unsigned + NonZero> IntegerType<'ctx, Int> {
    pub fn new_unsigned(context: &'ctx Context) -> Result<Self> {
        unsafe { Self::from_raw(LLVMIntTypeInContext(context.as_mut_ptr(), Int::to_u32())) }
    }
}

impl<'ctx, Int: Integer + NonZero> IntegerType<'ctx, Int> {
    pub fn new_signed(context: &'ctx Context) -> Result<Self> {
        unsafe {
            Self::from_raw(LLVMIntTypeInContext(
                context.as_mut_ptr(),
                Int::to_i32() as u32,
            ))
        }
    }
}

impl_typed!(typed(T): IntegerType);

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct PointerType<'ctx, T: ?Sized> {
    addr_space: AddressSpace,
    handle: TypePtr<'ctx>,
    ty: PhantomData<T>,
}

impl_typed!(typed(T): PointerType);

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct FunctionType<'ctx, P: ?Sized, R: ?Sized> {
    params: PhantomData<P>,
    result: PhantomData<R>,
    is_var_arg: bool,
    handle: TypePtr<'ctx>,
}

impl_typed!(typed(P, R): FunctionType);

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct ArrayType<'ctx, E: ?Sized> {
    element_type: PhantomData<E>,
    num_elements: usize,
    handle: TypePtr<'ctx>,
}

impl_typed!(typed(E): ArrayType);

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Erased(());

#[cfg(test)]
mod tests {
    use super::*;
    use typenum::consts::{P32, U32};

    fn wrapper<F: FnOnce() -> Result<()>>(func: F) -> Result<()> {
        func()
    }

    #[test]
    fn make_integer_types() {
        wrapper(|| {
            let context = Context::new()?;

            let _i32 = IntegerType::<P32>::new_signed(&context)?;
            let _u32 = IntegerType::<U32>::new_unsigned(&context)?;

            Ok(())
        })
        .unwrap();
    }

    #[test]
    fn make_integer_values() {
        wrapper(|| {
            let context = Context::new()?;

            let _i32 = IntegerType::<P32>::new_signed(&context)?;
            let _u32 = IntegerType::<U32>::new_unsigned(&context)?;

            let _i32 = IntValue::new(100, _i32, true)?;
            let _u32 = IntValue::new(100, _u32, false)?;

            Ok(())
        })
        .unwrap();
    }
}
