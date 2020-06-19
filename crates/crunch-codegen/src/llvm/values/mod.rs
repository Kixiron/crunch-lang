mod any_value;
mod basic_block;
mod function;
mod integer;
mod pointer;
mod sealed;
mod value;
mod value_kind;

pub use any_value::AnyValue;
pub use basic_block::BasicBlock;
pub use function::FunctionValue;
pub use integer::{IntMath, IntValue};
pub use pointer::{Global, Pointable, PointerValue};
pub(crate) use sealed::SealedAnyValue;
pub(crate) use value::Val;
pub use value::ValueUsage;
pub use value_kind::ValueKind;

use crate::llvm::{utils::Sealed, Context, Error, ErrorKind, Result};
use llvm_sys::core::LLVMConstStringInContext;
use std::{
    convert::TryFrom,
    fmt::{Debug, Formatter, Result as FmtResult},
};

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum Value<'ctx> {
    Argument(Argument<'ctx>),
    BasicBlock(BasicBlock<'ctx>),
    Function(FunctionValue<'ctx>),
    BlockAddress(BlockAddress<'ctx>),
    ConstArray(ArrayValue<'ctx>),
    ConstStruct(StructValue<'ctx>),
    ConstVector(VectorValue<'ctx>),
    Undef(Undef<'ctx>),
    ConstInt(IntValue<'ctx>),
    ConstFloat(FloatValue<'ctx>),
    ConstPtrNull(NullPtr<'ctx>),
    Metadata(Metadata<'ctx>),
    InlineAsm(InlineAsm<'ctx>),
    Instruction(Instruction<'ctx>),
    GlobalVariable(Val<'ctx>),
    ConstExpr(Val<'ctx>),
    ConstDataArray(Val<'ctx>),
}

impl<'ctx> Value<'ctx> {
    pub(crate) fn from_val(val: Val<'ctx>) -> Self {
        match val.kind() {
            ValueKind::Argument => Self::Argument(Argument::from_val(val)),
            ValueKind::BasicBlock => {
                Self::BasicBlock(<BasicBlock as SealedAnyValue>::from_val(val))
            }
            ValueKind::Function => Self::Function(FunctionValue::from_val(val)),
            ValueKind::BlockAddress => Self::BlockAddress(BlockAddress::from_val(val)),
            ValueKind::Undef => Self::Undef(Undef::from_val(val)),
            ValueKind::MetadataAsValue => Self::Metadata(Metadata::from_val(val)),
            ValueKind::InlineAsm => Self::InlineAsm(InlineAsm::from_val(val)),
            ValueKind::Instruction => Self::Instruction(Instruction::from_val(val)),
            ValueKind::ConstInt => Self::ConstInt(IntValue::from_val(val)),

            ValueKind::ConstArray => Self::ConstArray(todo!()),
            ValueKind::ConstStruct => Self::ConstStruct(todo!()),
            ValueKind::ConstVector => Self::ConstVector(todo!()),
            ValueKind::ConstFloat => Self::ConstFloat(todo!()),
            ValueKind::ConstPointerNull => Self::ConstPtrNull(todo!()),
            ValueKind::GlobalVariable => Self::GlobalVariable(val),
            ValueKind::ConstExpr => Self::ConstExpr(val),
            ValueKind::ConstDataArray => Self::ConstDataArray(val),

            unhandled => unimplemented!("{:?} has not been implemented as a Value yet", unhandled),
        }
    }

    pub(crate) fn as_val(self) -> Val<'ctx> {
        match self {
            Self::Argument(arg) => arg.as_val(),
            Self::BasicBlock(block) => block.as_val(),
            Self::Function(func) => func.as_val(),
            Self::BlockAddress(addr) => addr.as_val(),
            Self::ConstArray(array) => array.as_val(),
            Self::ConstStruct(structure) => structure.as_val(),
            Self::ConstVector(vector) => vector.as_val(),
            Self::Undef(undef) => undef.as_val(),
            Self::ConstInt(int) => int.as_val(),
            Self::ConstFloat(float) => float.as_val(),
            Self::ConstPtrNull(ptr) => ptr.as_val(),
            Self::Metadata(meta) => meta.as_val(),
            Self::InlineAsm(asm) => asm.as_val(),
            Self::Instruction(inst) => inst.as_val(),
            Self::GlobalVariable(val) | Self::ConstExpr(val) | Self::ConstDataArray(val) => val,
        }
    }
}

impl<'ctx> AnyValue<'ctx> for Value<'ctx> {
    fn as_value(&self) -> Value<'ctx> {
        *self
    }
}

impl<'ctx> SealedAnyValue<'ctx> for Value<'ctx> {
    fn as_val(&self) -> Val<'ctx> {
        Value::as_val(*self)
    }

    fn from_val(value: Val<'ctx>) -> Self {
        Self::from_val(value)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum FunctionOrPointer<'ctx> {
    Function(Val<'ctx>),
    Pointer(Val<'ctx>),
}

impl<'ctx> From<FunctionValue<'ctx>> for FunctionOrPointer<'ctx> {
    fn from(func: FunctionValue<'ctx>) -> Self {
        Self::Function(func.as_val())
    }
}

impl<'ctx> From<PointerValue<'ctx, FunctionValue<'ctx>>> for FunctionOrPointer<'ctx> {
    fn from(ptr: PointerValue<'ctx, FunctionValue<'ctx>>) -> Self {
        Self::Pointer(ptr.as_val())
    }
}

impl<'ctx> From<PointerValue<'ctx, Global<FunctionValue<'ctx>>>> for FunctionOrPointer<'ctx> {
    fn from(ptr: PointerValue<'ctx, Global<FunctionValue<'ctx>>>) -> Self {
        Self::Pointer(ptr.as_val())
    }
}

macro_rules! impl_any_val {
    ($($ty:ident),* $(,)?) => {
        $(
            impl<'ctx> AnyValue<'ctx> for $ty<'ctx> {}

            impl<'ctx> SealedAnyValue<'ctx> for $ty<'ctx> {
                fn as_val(&self) -> Val<'ctx> {
                    self.0
                }

                fn from_val(value: Val<'ctx>) -> Self {
                    Self(value)
                }
            }

            impl<'ctx> Sealed for $ty<'ctx> {}
        )*
    };
}

impl_any_val! {
    BlockAddress, InlineAsm, FloatValue,
    ArrayValue, StructValue, VectorValue,
    NullPtr, Undef, Argument, Metadata,
    Instruction, CallSiteValue,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct CallSiteValue<'ctx>(Val<'ctx>);

impl Debug for CallSiteValue<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        Debug::fmt(&self.0, f)
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct BlockAddress<'ctx>(Val<'ctx>);

impl<'ctx> Into<Value<'ctx>> for BlockAddress<'ctx> {
    fn into(self) -> Value<'ctx> {
        Value::BlockAddress(self)
    }
}

impl<'ctx> TryFrom<Value<'ctx>> for BlockAddress<'ctx> {
    type Error = Error;

    fn try_from(val: Value<'ctx>) -> Result<Self> {
        if let Value::BlockAddress(addr) = val {
            Ok(addr)
        } else {
            Err(Error::new(
                format!(
                    "A value of type {:?} cannot be made into an BlockAddress",
                    val.kind()
                ),
                ErrorKind::MismatchedTypes,
            ))
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct ArrayValue<'ctx>(Val<'ctx>);

impl<'ctx> ArrayValue<'ctx> {
    pub fn const_string(
        ctx: &'ctx Context,
        string: &[u8],
        null_terminated: bool,
    ) -> Result<ArrayValue<'ctx>> {
        unsafe {
            Self::from_raw(LLVMConstStringInContext(
                ctx.as_mut_ptr(),
                string.as_ptr() as *const i8,
                string.len() as u32,
                // The variable's name is `DontNullTerminate`, which is the most ass-backwards naming scheme I've ever seen
                !null_terminated as i32,
            ))
        }
    }
}

impl<'ctx> Into<Value<'ctx>> for ArrayValue<'ctx> {
    fn into(self) -> Value<'ctx> {
        Value::ConstArray(self)
    }
}

impl<'ctx> TryFrom<Value<'ctx>> for ArrayValue<'ctx> {
    type Error = Error;

    fn try_from(val: Value<'ctx>) -> Result<Self> {
        if let Value::ConstArray(array) = val {
            Ok(array)
        } else {
            Err(Error::new(
                format!(
                    "A value of type {:?} cannot be made into an Array",
                    val.kind()
                ),
                ErrorKind::MismatchedTypes,
            ))
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct StructValue<'ctx>(Val<'ctx>);

impl<'ctx> Into<Value<'ctx>> for StructValue<'ctx> {
    fn into(self) -> Value<'ctx> {
        Value::ConstStruct(self)
    }
}

impl<'ctx> TryFrom<Value<'ctx>> for StructValue<'ctx> {
    type Error = Error;

    fn try_from(val: Value<'ctx>) -> Result<Self> {
        if let Value::ConstStruct(structure) = val {
            Ok(structure)
        } else {
            Err(Error::new(
                format!(
                    "A value of type {:?} cannot be made into an Struct",
                    val.kind()
                ),
                ErrorKind::MismatchedTypes,
            ))
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct VectorValue<'ctx>(Val<'ctx>);

impl<'ctx> Into<Value<'ctx>> for VectorValue<'ctx> {
    fn into(self) -> Value<'ctx> {
        Value::ConstVector(self)
    }
}

impl<'ctx> TryFrom<Value<'ctx>> for VectorValue<'ctx> {
    type Error = Error;

    fn try_from(val: Value<'ctx>) -> Result<Self> {
        if let Value::ConstVector(vector) = val {
            Ok(vector)
        } else {
            Err(Error::new(
                format!(
                    "A value of type {:?} cannot be made into an Vector",
                    val.kind()
                ),
                ErrorKind::MismatchedTypes,
            ))
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct FloatValue<'ctx>(Val<'ctx>);

impl<'ctx> Into<Value<'ctx>> for FloatValue<'ctx> {
    fn into(self) -> Value<'ctx> {
        Value::ConstFloat(self)
    }
}

impl<'ctx> TryFrom<Value<'ctx>> for FloatValue<'ctx> {
    type Error = Error;

    fn try_from(val: Value<'ctx>) -> Result<Self> {
        if let Value::ConstFloat(float) = val {
            Ok(float)
        } else {
            Err(Error::new(
                format!(
                    "A value of type {:?} cannot be made into an Float",
                    val.kind()
                ),
                ErrorKind::MismatchedTypes,
            ))
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct NullPtr<'ctx>(Val<'ctx>);

impl<'ctx> Into<Value<'ctx>> for NullPtr<'ctx> {
    fn into(self) -> Value<'ctx> {
        Value::ConstPtrNull(self)
    }
}

impl<'ctx> TryFrom<Value<'ctx>> for NullPtr<'ctx> {
    type Error = Error;

    fn try_from(val: Value<'ctx>) -> Result<Self> {
        if let Value::ConstPtrNull(ptr) = val {
            Ok(ptr)
        } else {
            Err(Error::new(
                format!(
                    "A value of type {:?} cannot be made into an NullPtr",
                    val.kind()
                ),
                ErrorKind::MismatchedTypes,
            ))
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct Undef<'ctx>(Val<'ctx>);

impl<'ctx> Into<Value<'ctx>> for Undef<'ctx> {
    fn into(self) -> Value<'ctx> {
        Value::Undef(self)
    }
}

impl<'ctx> TryFrom<Value<'ctx>> for Undef<'ctx> {
    type Error = Error;

    fn try_from(val: Value<'ctx>) -> Result<Self> {
        if let Value::Undef(undef) = val {
            Ok(undef)
        } else {
            Err(Error::new(
                format!(
                    "A value of type {:?} cannot be made into an Undef",
                    val.kind()
                ),
                ErrorKind::MismatchedTypes,
            ))
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct Argument<'ctx>(Val<'ctx>);

impl<'ctx> Into<Value<'ctx>> for Argument<'ctx> {
    fn into(self) -> Value<'ctx> {
        Value::Argument(self)
    }
}

impl<'ctx> TryFrom<Value<'ctx>> for Argument<'ctx> {
    type Error = Error;

    fn try_from(val: Value<'ctx>) -> Result<Self> {
        if let Value::Argument(arg) = val {
            Ok(arg)
        } else {
            Err(Error::new(
                format!(
                    "A value of type {:?} cannot be made into an Argument",
                    val.kind()
                ),
                ErrorKind::MismatchedTypes,
            ))
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct InlineAsm<'ctx>(Val<'ctx>);

impl<'ctx> Into<Value<'ctx>> for InlineAsm<'ctx> {
    fn into(self) -> Value<'ctx> {
        Value::InlineAsm(self)
    }
}

impl<'ctx> TryFrom<Value<'ctx>> for InlineAsm<'ctx> {
    type Error = Error;

    fn try_from(val: Value<'ctx>) -> Result<Self> {
        if let Value::InlineAsm(asm) = val {
            Ok(asm)
        } else {
            Err(Error::new(
                format!(
                    "A value of type {:?} cannot be made into an InlineAsm",
                    val.kind()
                ),
                ErrorKind::MismatchedTypes,
            ))
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct Metadata<'ctx>(Val<'ctx>);

impl<'ctx> Into<Value<'ctx>> for Metadata<'ctx> {
    fn into(self) -> Value<'ctx> {
        Value::Metadata(self)
    }
}

impl<'ctx> TryFrom<Value<'ctx>> for Metadata<'ctx> {
    type Error = Error;

    fn try_from(val: Value<'ctx>) -> Result<Self> {
        if let Value::Metadata(meta) = val {
            Ok(meta)
        } else {
            Err(Error::new(
                format!(
                    "A value of type {:?} cannot be made into an Metadata",
                    val.kind()
                ),
                ErrorKind::MismatchedTypes,
            ))
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct Instruction<'ctx>(Val<'ctx>);

impl<'ctx> Into<Value<'ctx>> for Instruction<'ctx> {
    fn into(self) -> Value<'ctx> {
        Value::Instruction(self)
    }
}

impl<'ctx> TryFrom<Value<'ctx>> for Instruction<'ctx> {
    type Error = Error;

    fn try_from(val: Value<'ctx>) -> Result<Self> {
        if let Value::Instruction(inst) = val {
            Ok(inst)
        } else {
            Err(Error::new(
                format!(
                    "A value of type {:?} cannot be made into an Instruction",
                    val.kind()
                ),
                ErrorKind::MismatchedTypes,
            ))
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum InstructionKind {
    AtomicCmpXchg,
    AtomicRMW,
    BinaryOperator,
    Branch,
    Call,
    CatchReturn,
    CatchSwitch,
    CleanupReturn,
    Cmp,
    ExtractElem,
    Fence,
    FuncletPad,
    GetElemPtr,
    IndirectBranch,
    InsertElem,
    InsertValue,
    LandingPad,
    PhiNode,
    Resume,
    Return,
    Select,
    ShuffleVector,
    Store,
    Switch,
    Unary,
    Unreachable,
}

/*
use crate::{
    llvm::{context::Context, utils::to_non_nul, value::Value, Error, ErrorKind, Result, LLVM},
    null,
};
use llvm_sys::{
    core::{
        LLVMConstInt, LLVMGetArrayLength, LLVMGetTypeKind, LLVMGetUndef, LLVMInt16TypeInContext,
        LLVMInt32TypeInContext, LLVMInt64TypeInContext, LLVMInt8TypeInContext, LLVMPointerType,
        LLVMPrintValueToString, LLVMSizeOf, LLVMTypeIsSized, LLVMVoidTypeInContext,
    },
    LLVMType, LLVMTypeKind, LLVMValue as RawLLVMValue,
};
use std::{
    ffi::CStr,
    fmt::{self, Debug, Formatter, Result as FmtResult},
    marker::PhantomData,
    ptr::NonNull,
};

mod integer;
use llvm_value::LLVMValue;


macro_rules! is_type {
    ($($name:ident => $kind:ident),* $(,)?) => {
        $(
            #[inline]
            pub fn $name(self) -> bool {
                Type::kind(self) == TypeKind::$kind
            }
        )*
    };
}

#[derive(Copy, Clone, PartialEq, Eq)]
#[repr(transparent)]
pub struct Type<'a>(NonNull<LLVMType>, PhantomData<&'a Context>);

// Public interface
impl<'a> Type<'a> {
    #[inline]
    pub fn into_undef(self) -> Result<Value<'a>> {
        let undef = Value::from_raw(null!(
            unsafe { LLVMGetUndef(self.as_mut_ptr()) },
            "Failed to turn type into undef value",
        )?);
        debug_assert!(undef.is_undef());

        Ok(undef)
    }

    #[inline]
    pub fn array_len(self) -> Option<u32> {
        if self.is_array() {
            unsafe { Some(LLVMGetArrayLength(self.as_mut_ptr())) }
        } else {
            None
        }
    }

    #[inline]
    pub fn kind(self) -> TypeKind {
        TypeKind::from(unsafe { LLVMGetTypeKind(self.as_mut_ptr()) })
    }

    #[inline]
    pub fn can_be_ptr(self) -> bool {
        !matches!(self.kind(), TypeKind::Void)
    }

    #[inline]
    pub fn is_sized(self) -> bool {
        unsafe { LLVMTypeIsSized(self.as_mut_ptr()) != 0 }
    }

    #[inline]
    pub fn size_of(self) -> Result<Value<'a>> {
        let size = null!(
            unsafe { LLVMSizeOf(self.as_mut_ptr()) },
            "Failed to get size of type",
        )?;

        Ok(Value::from_raw(size))
    }

    is_type! {
        is_void      => Void,
        is_half      => Half,
        is_float     => Float,
        is_double    => Double,
        is_x86_fp80  => X86_FP80,
        is_fp128     => FP128,
        is_ppc_fp128 => PPC_FP128,
        is_label     => Label,
        is_int       => Integer,
        is_func      => Function,
        is_struct    => Struct,
        is_array     => Array,
        is_ptr       => Pointer,
        is_vector    => Vector,
        is_meta      => Metadata,
        is_x86_mmx   => X86_MMX,
        is_token     => Token,
    }
}

// Private interface
impl<'a> Type<'a> {
    #[inline]
    pub(crate) const fn from_raw(raw_type: NonNull<LLVMType>) -> Self {
        Self(raw_type, PhantomData)
    }

    #[inline]
    pub(crate) const fn as_mut_ptr(self) -> *mut LLVMType {
        self.0.as_ptr() as *mut LLVMType
    }
}

impl fmt::Debug for Type<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.kind())
    }
}

impl fmt::Pointer for Type<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:p}", self.0)
    }
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
    PPC_FP128,
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
            LLVMTypeKind::LLVMPPC_FP128TypeKind => Self::PPC_FP128,
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

pub trait Typeable: sealed::Sealed {
    unsafe fn create_type<'a>(ctx: &'a Context) -> Result<Type<'a>>;
}

mod sealed {
    pub trait Sealed {}
}

macro_rules! typeable {
    ($($ty:ty => $func:path : $expected:ident),* $(,)?) => {
        $(
            impl Typeable for $ty {
                #[inline]
                unsafe fn create_type<'a>(ctx: &'a Context) -> Result<Type<'a>> {
                    let ty = Type::from_raw($crate::null!(
                        $func(ctx.as_mut_ptr()),
                        concat!("Failed to create `", stringify!($ty), "` type"),
                    )?);

                    // Make sure the type we got was the one we wanted
                    debug_assert_eq!(Type::kind(ty), TypeKind::$expected);

                    Ok(ty)
                }
            }

            impl sealed::Sealed for $ty {}
        )*

        #[cfg(test)]
        mod type_tests {
            use $crate::llvm::LLVM;
            use super::*;

            #[test]
            fn create_types() {
                let llvm = LLVM::new().unwrap();

                $(
                    let ty = llvm.get_type::<$ty>().unwrap();
                    assert_eq!(ty.kind(), TypeKind::$expected);

                    // Assert that pointer-able types make pointers and others make errors
                    if ty.can_be_ptr() {
                        let ptr = llvm.get_type::<*mut $ty>().unwrap();
                        assert_eq!(ptr.kind(), TypeKind::Pointer);

                        let ptr = llvm.get_type::<*const $ty>().unwrap();
                        assert_eq!(ptr.kind(), TypeKind::Pointer);
                    } else {
                        let ptr = llvm.get_type::<*mut $ty>();
                        assert!(ptr.is_err());

                        let ptr = llvm.get_type::<*const $ty>();
                        assert!(ptr.is_err());
                    }
                )*
            }
        }
    };
}

typeable! {
    ()  => LLVMVoidTypeInContext  : Void,
    u8  => LLVMInt8TypeInContext  : Integer,
    i8  => LLVMInt8TypeInContext  : Integer,
    u16 => LLVMInt16TypeInContext : Integer,
    i16 => LLVMInt16TypeInContext : Integer,
    u32 => LLVMInt32TypeInContext : Integer,
    i32 => LLVMInt32TypeInContext : Integer,
    u64 => LLVMInt64TypeInContext : Integer,
    i64 => LLVMInt64TypeInContext : Integer,
}

impl<T> Typeable for *const T
where
    T: Typeable,
{
    #[inline]
    unsafe fn create_type<'a>(ctx: &'a Context) -> Result<Type<'a>> {
        let ty = <T as Typeable>::create_type(ctx)?;

        if !ty.can_be_ptr() {
            return Err(Error::new(
                format!(
                    "You cannot create pointers to types of kind {:?}",
                    ty.kind(),
                ),
                ErrorKind::MistypedPtr,
            ));
        }

        let ptr = Type::from_raw(null!(
            LLVMPointerType(ty.as_mut_ptr(), 0),
            concat!("Failed to create `*const ", stringify!($ty), "` type"),
        )?);

        debug_assert_eq!(Type::kind(ptr), TypeKind::Pointer);

        Ok(ptr)
    }
}

impl<T> sealed::Sealed for *const T where T: Typeable {}

impl<T> Typeable for *mut T
where
    T: Typeable,
{
    #[inline]
    unsafe fn create_type<'a>(ctx: &'a Context) -> Result<Type<'a>> {
        let ty = <T as Typeable>::create_type(ctx)?;

        if !ty.can_be_ptr() {
            return Err(Error::new(
                format!(
                    "You cannot create pointers to types of kind {:?}",
                    ty.kind(),
                ),
                ErrorKind::MistypedPtr,
            ));
        }

        let ptr = Type::from_raw(null!(
            LLVMPointerType(ty.as_mut_ptr(), 0),
            concat!("Failed to create `*mut ", stringify!($ty), "` type"),
        )?);

        debug_assert_eq!(Type::kind(ptr), TypeKind::Pointer);

        Ok(ptr)
    }
}

impl<T> sealed::Sealed for *mut T where T: Typeable {}

pub trait Constant: sealed::Sealed {
    fn create<'a>(&self, ctx: &'a Context) -> Result<Value<'a>>;
}

impl Constant for i32 {
    fn create<'a>(&self, ctx: &'a Context) -> Result<Value<'a>> {
        let ty = unsafe { <Self as Typeable>::create_type(ctx)? };
        debug_assert!(ty.is_int());

        let int = Value::from_raw(null!(
            unsafe { LLVMConstInt(ty.as_mut_ptr(), *self as u64, true as i32) },
            "Failed to create LLVM const integer",
        )?);

        debug_assert_eq!(int.get_type(), Ok(ty));
        debug_assert_eq!(int.const_int_val(), Some(*self as i64));

        Ok(int)
    }
}

impl Constant for u8 {
    fn create<'a>(&self, ctx: &'a Context) -> Result<Value<'a>> {
        let ty = unsafe { <Self as Typeable>::create_type(ctx)? };
        debug_assert!(ty.is_int());

        let int = Value::from_raw(null!(
            unsafe { LLVMConstInt(ty.as_mut_ptr(), *self as u64, true as i32) },
            "Failed to create LLVM const integer",
        )?);

        debug_assert_eq!(int.get_type(), Ok(ty));
        debug_assert_eq!(int.const_int_val(), Some(*self as i64));

        Ok(int)
    }
}

#[cfg(test)]
mod tests {
    use crate::llvm::LLVM;

    #[test]
    fn undef_is_undef() {
        let llvm = LLVM::new().unwrap();

        let undef = llvm.get_type::<i32>().unwrap().into_undef().unwrap();
        assert!(undef.is_undef());
    }

    #[test]
    fn use_undef() {
        let llvm = LLVM::new().unwrap();
        let module = llvm.create_module("ub_for_me").unwrap();

        let hei_32 = llvm.get_type::<i32>().unwrap();
        let sig = module.function_ty(&[hei_32, hei_32], hei_32).unwrap();

        module
            .build_function("add_undef", &sig, |builder| {
                let undef = hei_32.into_undef()?;

                let block = builder.append_block("ub_goes_here")?;
                let add = block.add(
                    builder.args()[0].value(),
                    builder.args()[1].value(),
                    "ub_goes_here.0",
                )?;
                let add = block.add(add, undef, "ub_goes_here.1")?;
                block.ret(add)?;

                Ok(())
            })
            .unwrap();

        println!("{:?}", module);
    }
}
*/
