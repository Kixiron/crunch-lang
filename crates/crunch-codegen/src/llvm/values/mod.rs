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

use crate::llvm::{types::Type, utils::Sealed, Context, Error, ErrorKind, Result};
use llvm_sys::{
    core::{LLVMConstArray, LLVMConstStringInContext},
    LLVMValue,
};
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
    Instruction(InstructionValue<'ctx>),
    GlobalVariable(Val<'ctx>),
    ConstExpr(Val<'ctx>),
    ConstDataArray(Val<'ctx>),
    CallSite(CallSiteValue<'ctx>),
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
            ValueKind::Instruction => Self::Instruction(InstructionValue::from_val(val)),
            ValueKind::ConstInt => Self::ConstInt(IntValue::from_val(val)),

            // ValueKind::ConstArray => Self::ConstArray(todo!()),
            // ValueKind::ConstStruct => Self::ConstStruct(todo!()),
            // ValueKind::ConstVector => Self::ConstVector(todo!()),
            // ValueKind::ConstFloat => Self::ConstFloat(todo!()),
            // ValueKind::ConstPointerNull => Self::ConstPtrNull(todo!()),
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
            Self::CallSite(call) => call.as_val(),
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
    InstructionValue, CallSiteValue,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct CallSiteValue<'ctx>(Val<'ctx>);

impl Debug for CallSiteValue<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        Debug::fmt(&self.0, f)
    }
}

impl<'ctx> Into<Value<'ctx>> for CallSiteValue<'ctx> {
    fn into(self) -> Value<'ctx> {
        Value::CallSite(self)
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
    pub fn const_array(
        element_type: Type<'ctx>,
        elements: &[Value<'ctx>],
    ) -> Result<ArrayValue<'ctx>> {
        let mut elements: Vec<*mut LLVMValue> =
            elements.iter().map(|e| e.as_val().as_mut_ptr()).collect();

        unsafe {
            Self::from_raw(LLVMConstArray(
                element_type.as_mut_ptr(),
                elements.as_mut_ptr(),
                elements.len() as u32,
            ))
        }
    }

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
pub struct InstructionValue<'ctx>(Val<'ctx>);

impl<'ctx> Into<Value<'ctx>> for InstructionValue<'ctx> {
    fn into(self) -> Value<'ctx> {
        Value::Instruction(self)
    }
}

impl<'ctx> TryFrom<Value<'ctx>> for InstructionValue<'ctx> {
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
