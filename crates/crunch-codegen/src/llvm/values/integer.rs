use crate::llvm::{
    values::{sealed::SealedAnyValue, AnyValue, Val, Value},
    Context, Error, ErrorKind, Result,
};
use llvm_sys::core::LLVMInt32TypeInContext;
use std::convert::TryFrom;

pub trait IntMath<'ctx>: AnyValue<'ctx> {}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct Int<'ctx>(Val<'ctx>);

impl<'ctx> AnyValue<'ctx> for Int<'ctx> {}

impl<'ctx> SealedAnyValue<'ctx> for Int<'ctx> {
    fn as_val(&self) -> Val<'ctx> {
        self.0
    }

    fn from_val(value: Val<'ctx>) -> Self {
        Self(value)
    }
}

impl<'ctx> Into<Value<'ctx>> for Int<'ctx> {
    fn into(self) -> Value<'ctx> {
        Value::ConstInt(self)
    }
}

impl<'ctx> TryFrom<Value<'ctx>> for Int<'ctx> {
    type Error = Error;

    fn try_from(val: Value<'ctx>) -> Result<Self> {
        if let Value::ConstInt(int) = val {
            Ok(int)
        } else {
            Err(Error::new(
                format!(
                    "A value of type {:?} cannot be made into an Int",
                    val.kind()
                ),
                ErrorKind::MismatchedTypes,
            ))
        }
    }
}
