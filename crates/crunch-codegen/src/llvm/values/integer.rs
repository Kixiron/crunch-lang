use crate::llvm::{
    utils::Sealed,
    values::{sealed::SealedAnyValue, AnyValue, Val, Value},
    Error, ErrorKind, Result,
};
use std::convert::TryFrom;

pub trait IntMath<'ctx>: AnyValue<'ctx> {}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct IntValue<'ctx>(Val<'ctx>);

impl<'ctx> AnyValue<'ctx> for IntValue<'ctx> {}

impl<'ctx> SealedAnyValue<'ctx> for IntValue<'ctx> {
    fn as_val(&self) -> Val<'ctx> {
        self.0
    }

    fn from_val(value: Val<'ctx>) -> Self {
        Self(value)
    }
}

impl<'ctx> Into<Value<'ctx>> for IntValue<'ctx> {
    fn into(self) -> Value<'ctx> {
        Value::ConstInt(self)
    }
}

impl<'ctx> Sealed for IntValue<'ctx> {}

impl<'ctx> TryFrom<Value<'ctx>> for IntValue<'ctx> {
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
