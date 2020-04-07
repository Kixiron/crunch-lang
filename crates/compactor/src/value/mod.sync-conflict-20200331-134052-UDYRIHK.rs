mod compare;

pub use compare::Compare;

use crate::{return_frame::ReturnFrame, CRUNCH_ALLOCATOR};

use ballast::Rooted;
use crunch_error::runtime_prelude::*;
use num_bigint::BigInt;

#[cfg(feature = "dll-ffi")]
use alloc::sync::Arc;
use alloc::{
    boxed::Box,
    format,
    string::{String, ToString},
    vec,
    vec::Vec,
};
use core::{fmt, mem};

// TODO: Test all implemented operations
// TODO: Implement all inter-int operations
// Eg. IByte and I32 can be added together

// Moving to a tagged enum is waiting on tagged_box/#2, specifically Unit Variants
// https://github.com/Kixiron/tagged-box/issues/2
#[derive(Debug)]
#[repr(u8)]
pub enum Value {
    // Integers
    Byte(u8),
    IByte(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    I128(i128),
    GcInt(Rooted<BigInt>),

    // Floats
    F32(f32),
    F64(f64),

    // Strings
    Char(char),
    Str(&'static str),
    GcString(Rooted<String>),

    Bool(bool),
    #[cfg(feature = "dll-ffi")]
    Library(Arc<dlopen::raw::Library>),
    Null,

    Array(Vec<Value>),

    Generator(Box<ReturnFrame>),

    None,
}

impl Clone for Value {
    fn clone(&self) -> Self {
        match self {
            Self::Byte(i) => Self::Byte(*i),
            Self::IByte(i) => Self::IByte(*i),
            Self::I16(i) => Self::I16(*i),
            Self::I32(i) => Self::I32(*i),
            Self::I64(i) => Self::I64(*i),
            Self::I128(i) => Self::I128(*i),
            Self::GcInt(i) => {
                let (sign, digits) = (&**i).to_u32_digits();
                Self::GcInt(unsafe {
                    CRUNCH_ALLOCATOR.with(|alloc| {
                        alloc
                            .get()
                            .as_mut()
                            .unwrap()
                            .alloc(BigInt::new(sign, digits))
                    })
                })
            }

            Self::F32(f) => Self::F32(*f),
            Self::F64(f) => Self::F64(*f),

            Self::Char(c) => Self::Char(*c),
            Self::Str(s) => Self::Str(s),
            Self::GcString(s) => Self::GcString(unsafe {
                CRUNCH_ALLOCATOR.with(|alloc| alloc.get().as_mut().unwrap().alloc((&**s).clone()))
            }),

            Self::Bool(b) => Self::Bool(*b),

            #[cfg(feature = "dll-ffi")]
            Self::Library(lib) => Self::Library(Arc::clone(lib)),

            Self::Array(a) => Self::Array(a.clone()),

            Self::Generator(g) => Self::Generator(g.clone()),

            Self::Null => Self::Null,
            Self::None => Self::None,
        }
    }
}

impl Value {
    #[inline]
    #[must_use]
    pub fn take(&mut self) -> Self {
        let mut temp = Value::None;
        mem::swap(self, &mut temp);

        temp
    }

    pub fn to_usize(&self) -> Option<usize> {
        Some(match self {
            Self::Byte(int) => *int as usize,
            Self::IByte(int) => *int as usize,
            Self::I16(int) => *int as usize,
            Self::I32(int) => *int as usize,
            Self::I64(int) => *int as usize,
            Self::I128(int) => *int as usize,
            Self::F32(int) => *int as usize,
            Self::F64(int) => *int as usize,

            // TODO: Handle Bigints
            _ => return None,
        })
    }

    #[must_use]
    pub fn name(&self) -> &'static str {
        match self {
            Self::Byte(_) => "byte",
            Self::IByte(_) => "ibyte",
            Self::I16(_) => "int16",
            Self::I32(_) => "int",
            Self::I64(_) => "int64",
            Self::I128(_) => "int128",
            Self::F32(_) => "float",
            Self::F64(_) => "float64",
            Self::Bool(_) => "bool",
            Self::Char(_) => "char",
            Self::GcString(_) | Self::Str(_) => "str",
            Self::GcInt(_) => "bigint",
            #[cfg(feature = "dll-ffi")]
            Self::Library(_) => "lib",
            Self::Generator(_) => "generator",
            Self::Array(_) => "array",
            Self::Null => "null",
            Self::None => "NoneType",
        }
    }

    pub fn is_none(&self) -> bool {
        self == &Self::None
    }

    pub fn is_null(&self) -> bool {
        self == &Self::Null
    }

    // TODO: Add similar-type eq
    pub fn compare(&self, other: &Self) -> RuntimeResult<Compare> {
        Ok(match (self, other) {
            (Self::Byte(left), Self::Byte(right)) => Compare::ordering(left, right),
            (Self::IByte(left), Self::IByte(right)) => Compare::ordering(left, right),
            (Self::I16(left), Self::I16(right)) => Compare::ordering(left, right),
            (Self::I32(left), Self::I32(right)) => Compare::ordering(left, right),
            (Self::I64(left), Self::I64(right)) => Compare::ordering(left, right),
            (Self::I128(left), Self::I128(right)) => Compare::ordering(left, right),
            (Self::GcInt(left), Self::GcInt(right)) => Compare::ordering(&**left, &**right),

            (Self::F32(left), Self::F32(right)) => Compare::ordering(left, right),
            (Self::F64(left), Self::F64(right)) => Compare::ordering(left, right),

            (Self::Bool(left), Self::Bool(right)) => Compare::just_equality(left, right),

            (Value::Null, Value::Null) => Compare::Equal,
            (Value::Null, _) | (_, Value::Null) => Compare::Unequal,

            (left, right) if left == &Self::None || right == &Self::None => {
                return Err(RuntimeError::new(
                    RuntimeErrorTy::NullVar,
                    format!(
                        "Values of types '{}' and '{}' cannot be equal",
                        left.name(),
                        right.name()
                    ),
                ));
            }
            (_, _) => Compare::Incomparable,
        })
    }

    #[cfg(test)]
    pub(crate) fn is_equal(&self, other: &Self) -> RuntimeResult<bool> {
        Ok(match self.compare(other)? {
            Compare::Equal => true,
            _ => false,
        })
    }

    pub fn to_string(&self) -> String {
        match self {
            Self::Byte(int) => int.to_string(),
            Self::IByte(int) => int.to_string(),
            Self::I16(int) => int.to_string(),
            Self::I32(int) => int.to_string(),
            Self::I64(int) => int.to_string(),
            Self::I128(int) => int.to_string(),
            Self::F32(int) => int.to_string(),
            Self::F64(int) => int.to_string(),
            Self::Bool(int) => int.to_string(),
            #[cfg(feature = "dll-ffi")]
            Self::Library(lib) => format!("{:?}", lib),
            Self::Char(c) => c.to_string(),
            Self::GcString(string) => (&**string).to_string(),
            Self::Str(string) => (*string).to_string(),
            Self::GcInt(int) => (&**int).to_string(),
            Self::Generator(gen) => format!("{:p}", gen.function_index as *const u8),
            Self::Array(arr) => format!("{:?}", arr),
            Self::Null => "null".to_string(),
            Self::None => "NoneType".to_string(),
        }
    }

    pub fn from_bytes(
        _bytes: &[u8],
        _strings: &mut alloc::collections::VecDeque<String>,
    ) -> RuntimeResult<Self> {
        unimplemented!()
    }

    #[must_use]
    #[allow(clippy::unused_self)]
    pub fn as_bytes(&self) -> (Vec<u8>, Option<String>) {
        unimplemented!()
    }
}

/// Numerical operations
// TODO: Remainder, modulo
impl Value {
    pub fn add_upflowing(&self, other: &Self) -> RuntimeResult<Self> {
        Ok(match (&self, &other) {
            (Self::Byte(left), Self::Byte(right)) => {
                if let Some(result) = left.checked_add(*right) {
                    Self::Byte(result)
                } else {
                    Self::I16(*left as i16).add_upflowing(&Self::I16(*right as i16))?
                }
            }
            (Self::IByte(left), Self::IByte(right)) => {
                if let Some(result) = left.checked_add(*right) {
                    Self::IByte(result)
                } else {
                    Self::I16(*left as i16).add_upflowing(&Self::I16(*right as i16))?
                }
            }
            (Self::I16(left), Self::I16(right)) => {
                if let Some(result) = left.checked_add(*right) {
                    Self::I16(result)
                } else {
                    Self::I32(*left as i32).add_upflowing(&Self::I32(*right as i32))?
                }
            }
            (Self::I32(left), Self::I32(right)) => {
                if let Some(result) = left.checked_add(*right) {
                    Self::I32(result)
                } else {
                    Self::I64(*left as i64).add_upflowing(&Self::I64(*right as i64))?
                }
            }
            (Self::I64(left), Self::I64(right)) => {
                if let Some(result) = left.checked_add(*right) {
                    Self::I64(result)
                } else {
                    Self::I128(*left as i128).add_upflowing(&Self::I128(*right as i128))?
                }
            }
            (Self::I128(left), Self::I128(right)) => {
                if let Some(result) = left.checked_add(*right) {
                    Self::I128(result)
                } else {
                    let mut int = BigInt::from(*left);
                    int += *right;

                    Self::GcInt(unsafe {
                        CRUNCH_ALLOCATOR.with(|alloc| alloc.get().as_mut().unwrap().alloc(int))
                    })
                }
            }
            (Self::GcInt(left), Self::GcInt(right)) => {
                let (sign, digits) = (&**left).to_u32_digits();
                let mut int = BigInt::new(sign, digits);
                int += &**right;

                Self::GcInt(unsafe {
                    CRUNCH_ALLOCATOR.with(|alloc| alloc.get().as_mut().unwrap().alloc(int))
                })
            }

            (Self::F32(left), Self::F32(right)) => Self::F32(left + right),
            (Self::F64(left), Self::F64(right)) => Self::F64(left + right),

            (Self::Str(left), Self::Str(right)) => {
                let mut string = String::with_capacity(left.len() + right.len());
                string.push_str(left);
                string.push_str(right);

                Self::GcString(unsafe {
                    CRUNCH_ALLOCATOR.with(|alloc| alloc.get().as_mut().unwrap().alloc(string))
                })
            }

            (Self::GcString(left), Self::GcString(right)) => {
                let mut string = String::with_capacity((&**left).len() + (&**right).len());
                string.push_str(&**left);
                string.push_str(&**right);

                Self::GcString(unsafe {
                    CRUNCH_ALLOCATOR.with(|alloc| alloc.get().as_mut().unwrap().alloc(string))
                })
            }

            (left, right) if *left == &Self::None || *right == &Self::None => {
                error!(
                    "Values of types {} and {} cannot be added",
                    left.name(),
                    right.name()
                );
                return Err(RuntimeError::new(
                    RuntimeErrorTy::NullVar,
                    format!(
                        "Values of types '{}' and '{}' cannot be added",
                        left.name(),
                        right.name()
                    ),
                ));
            }
            (left, right) => {
                error!(
                    "Values of types {} and {} cannot be added",
                    left.name(),
                    right.name()
                );
                return Err(RuntimeError::new(
                    RuntimeErrorTy::IncompatibleTypes,
                    format!(
                        "Values of types '{}' and '{}' cannot be added",
                        left.name(),
                        right.name()
                    ),
                ));
            }
        })
    }

    pub fn sub_upflowing(&self, other: &Self) -> RuntimeResult<Self> {
        Ok(match (&self, &other) {
            (Self::Byte(left), Self::Byte(right)) => {
                if let Some(result) = left.checked_sub(*right) {
                    Self::Byte(result)
                } else {
                    Self::I16(*left as i16).sub_upflowing(&Self::I16(*right as i16))?
                }
            }
            (Self::IByte(left), Self::IByte(right)) => {
                if let Some(result) = left.checked_sub(*right) {
                    Self::IByte(result)
                } else {
                    Self::I16(*left as i16).sub_upflowing(&Self::I16(*right as i16))?
                }
            }
            (Self::I16(left), Self::I16(right)) => {
                if let Some(result) = left.checked_sub(*right) {
                    Self::I16(result)
                } else {
                    Self::I32(*left as i32).sub_upflowing(&Self::I32(*right as i32))?
                }
            }
            (Self::I32(left), Self::I32(right)) => {
                if let Some(result) = left.checked_sub(*right) {
                    Self::I32(result)
                } else {
                    Self::I64(*left as i64).sub_upflowing(&Self::I64(*right as i64))?
                }
            }
            (Self::I64(left), Self::I64(right)) => {
                if let Some(result) = left.checked_sub(*right) {
                    Self::I64(result)
                } else {
                    Self::I128(*left as i128).sub_upflowing(&Self::I128(*right as i128))?
                }
            }
            (Self::I128(left), Self::I128(right)) => {
                if let Some(result) = left.checked_sub(*right) {
                    Self::I128(result)
                } else {
                    let mut int = BigInt::from(*left);
                    int -= *right;

                    Self::GcInt(unsafe {
                        CRUNCH_ALLOCATOR.with(|alloc| alloc.get().as_mut().unwrap().alloc(int))
                    })
                }
            }
            (Self::GcInt(left), Self::GcInt(right)) => {
                let (sign, digits) = (&**left).to_u32_digits();
                let mut int = BigInt::new(sign, digits);
                int -= &**right;

                Self::GcInt(unsafe {
                    CRUNCH_ALLOCATOR.with(|alloc| alloc.get().as_mut().unwrap().alloc(int))
                })
            }

            (Self::F32(left), Self::F32(right)) => Self::F32(left - right),
            (Self::F64(left), Self::F64(right)) => Self::F64(left - right),

            (left, right) if *left == &Self::None || *right == &Self::None => {
                error!(
                    "Values of types {} and {} cannot be subtracted",
                    left.name(),
                    right.name()
                );
                return Err(RuntimeError::new(
                    RuntimeErrorTy::NullVar,
                    format!(
                        "Values of types '{}' and '{}' cannot be subtracted",
                        left.name(),
                        right.name()
                    ),
                ));
            }
            (left, right) => {
                error!(
                    "Values of types {} and {} cannot be subtracted",
                    left.name(),
                    right.name()
                );
                return Err(RuntimeError::new(
                    RuntimeErrorTy::IncompatibleTypes,
                    format!(
                        "Values of types '{}' and '{}' cannot be subtracted",
                        left.name(),
                        right.name()
                    ),
                ));
            }
        })
    }

    // TODO: Make this take into account division by zero for all variants
    pub fn div_upflowing(&self, other: &Self) -> RuntimeResult<Self> {
        Ok(match (&self, &other) {
            (Self::Byte(left), Self::Byte(right)) => {
                Self::Byte(left.checked_div(*right).unwrap_or(0))
            }

            (Self::IByte(left), Self::IByte(right)) => {
                if let Some(result) = left.checked_div(*right) {
                    Self::IByte(result)
                } else {
                    Self::I16(*left as i16).div_upflowing(&Self::I16(*right as i16))?
                }
            }
            (Self::I16(left), Self::I16(right)) => {
                if let Some(result) = left.checked_div(*right) {
                    Self::I16(result)
                } else {
                    Self::I32(*left as i32).div_upflowing(&Self::I32(*right as i32))?
                }
            }
            (Self::I32(left), Self::I32(right)) => {
                if let Some(result) = left.checked_div(*right) {
                    Self::I32(result)
                } else {
                    Self::I64(*left as i64).div_upflowing(&Self::I64(*right as i64))?
                }
            }
            (Self::I32(left), Self::I64(right)) => {
                if let Some(result) = (*left as i64).checked_div(*right) {
                    Self::I64(result)
                } else {
                    Self::I64(*left as i64).div_upflowing(&Self::I64(*right as i64))?
                }
            }
            (Self::I64(left), Self::I64(right)) => {
                if let Some(result) = left.checked_div(*right) {
                    Self::I64(result)
                } else {
                    Self::I128(*left as i128).div_upflowing(&Self::I128(*right as i128))?
                }
            }
            (Self::I128(left), Self::I128(right)) => {
                if let Some(result) = left.checked_div(*right) {
                    Self::I128(result)
                } else {
                    let mut int = BigInt::from(*left);
                    int /= *right;

                    Self::GcInt(unsafe {
                        CRUNCH_ALLOCATOR.with(|alloc| alloc.get().as_mut().unwrap().alloc(int))
                    })
                }
            }
            (Self::GcInt(left), Self::GcInt(right)) => {
                let (sign, digits) = (&**left).to_u32_digits();
                let mut int = BigInt::new(sign, digits);
                int /= &**right;

                Self::GcInt(unsafe {
                    CRUNCH_ALLOCATOR.with(|alloc| alloc.get().as_mut().unwrap().alloc(int))
                })
            }

            (Self::F32(left), Self::F32(right)) if *left != 0.0 && *right != 0.0 => {
                Self::F32(left / right)
            }
            (Self::F32(_left), Self::F32(_right)) => Self::F32(0.0),

            (Self::F64(left), Self::F64(right)) if *left != 0.0 && *right != 0.0 => {
                Self::F64(left / right)
            }
            (Self::F64(_left), Self::F64(_right)) => Self::F64(0.0),

            (left, right) if *left == &Self::None || *right == &Self::None => {
                return Err(RuntimeError::new(
                    RuntimeErrorTy::NullVar,
                    format!(
                        "Values of types '{}' and '{}' cannot be divided",
                        left.name(),
                        right.name()
                    ),
                ));
            }
            (left, right) => {
                return Err(RuntimeError::new(
                    RuntimeErrorTy::IncompatibleTypes,
                    format!(
                        "Values of types '{}' and '{}' cannot be divided",
                        left.name(),
                        right.name()
                    ),
                ));
            }
        })
    }

    pub fn mult_upflowing(&self, other: &Self) -> RuntimeResult<Self> {
        Ok(match (&self, &other) {
            (Self::Byte(left), Self::Byte(right)) => {
                Self::Byte(left.checked_mul(*right).unwrap_or(0))
            }

            (Self::IByte(left), Self::IByte(right)) => {
                if let Some(result) = left.checked_mul(*right) {
                    Self::IByte(result)
                } else {
                    Self::I16(*left as i16).mult_upflowing(&Self::I16(*right as i16))?
                }
            }
            (Self::I16(left), Self::I16(right)) => {
                if let Some(result) = left.checked_mul(*right) {
                    Self::I16(result)
                } else {
                    Self::I32(*left as i32).mult_upflowing(&Self::I32(*right as i32))?
                }
            }
            (Self::I32(left), Self::I32(right)) => {
                if let Some(result) = left.checked_mul(*right) {
                    Self::I32(result)
                } else {
                    Self::I64(*left as i64).mult_upflowing(&Self::I64(*right as i64))?
                }
            }
            (Self::I32(left), Self::I64(right)) => {
                if let Some(result) = (*left as i64).checked_mul(*right) {
                    Self::I64(result)
                } else {
                    Self::I64(*left as i64).mult_upflowing(&Self::I64(*right as i64))?
                }
            }
            (Self::I64(left), Self::I64(right)) => {
                if let Some(result) = left.checked_mul(*right) {
                    Self::I64(result)
                } else {
                    Self::I128(*left as i128).mult_upflowing(&Self::I128(*right as i128))?
                }
            }
            (Self::I128(left), Self::I128(right)) => {
                if let Some(result) = left.checked_mul(*right) {
                    Self::I128(result)
                } else {
                    let mut int = BigInt::from(*left);
                    int *= *right;

                    Self::GcInt(unsafe {
                        CRUNCH_ALLOCATOR.with(|alloc| alloc.get().as_mut().unwrap().alloc(int))
                    })
                }
            }
            (Self::GcInt(left), Self::GcInt(right)) => {
                let (sign, digits) = (*left).to_u32_digits();
                let mut int = BigInt::new(sign, digits);
                int *= &**right;

                Self::GcInt(unsafe {
                    CRUNCH_ALLOCATOR.with(|alloc| alloc.get().as_mut().unwrap().alloc(int))
                })
            }

            (Self::F32(left), Self::F32(right)) if *left != 0.0 && *right != 0.0 => {
                Self::F32(left * right)
            }
            (Self::F32(_left), Self::F32(_right)) => Self::F32(0.0),

            (Self::F64(left), Self::F64(right)) if *left != 0.0 && *right != 0.0 => {
                Self::F64(left * right)
            }
            (Self::F64(_left), Self::F64(_right)) => Self::F64(0.0),

            (left, right) if *left == &Self::None || *right == &Self::None => {
                return Err(RuntimeError::new(
                    RuntimeErrorTy::NullVar,
                    format!(
                        "Values of types '{}' and '{}' cannot be multiplied",
                        left.name(),
                        right.name()
                    ),
                ));
            }
            (left, right) => {
                return Err(RuntimeError::new(
                    RuntimeErrorTy::IncompatibleTypes,
                    format!(
                        "Values of types '{}' and '{}' cannot be multiplied",
                        left.name(),
                        right.name()
                    ),
                ));
            }
        })
    }
}

/// Bitwise operations
impl Value {
    pub fn bit_or(&self, other: &Self) -> RuntimeResult<Self> {
        Ok(match (self, other) {
            (Self::Byte(left), Self::Byte(right)) => Self::Byte(left | right),
            (Self::IByte(left), Self::IByte(right)) => Self::IByte(left | right),
            (Self::I16(left), Self::I16(right)) => Self::I16(left | right),
            (Self::I32(left), Self::I32(right)) => Self::I32(left | right),
            (Self::I64(left), Self::I64(right)) => Self::I64(left | right),
            (Self::I32(left), Self::I64(right)) => Self::I64((*left as i64) | right),
            (Self::I128(left), Self::I128(right)) => Self::I128(left | right),
            (Self::GcInt(left), Self::GcInt(right)) => {
                let (sign, digits) = (*left).to_u32_digits();
                let mut int = BigInt::new(sign, digits);
                int |= &**right;

                Self::GcInt(unsafe {
                    CRUNCH_ALLOCATOR.with(|alloc| alloc.get().as_mut().unwrap().alloc(int))
                })
            }

            (left, right) if *left == Self::None || *right == Self::None => {
                return Err(RuntimeError::new(
                    RuntimeErrorTy::NullVar,
                    format!(
                        "You cannot bitwise or values of types '{}' and '{}'",
                        left.name(),
                        right.name()
                    ),
                ));
            }
            (left, right) => {
                return Err(RuntimeError::new(
                    RuntimeErrorTy::IncompatibleTypes,
                    format!(
                        "You cannot bitwise or values of types '{}' and '{}'",
                        left.name(),
                        right.name()
                    ),
                ));
            }
        })
    }

    pub fn bit_xor(&self, other: &Self) -> RuntimeResult<Self> {
        Ok(match (self, other) {
            (Self::Byte(left), Self::Byte(right)) => Self::Byte(left ^ right),
            (Self::IByte(left), Self::IByte(right)) => Self::IByte(left ^ right),
            (Self::I16(left), Self::I16(right)) => Self::I16(left ^ right),
            (Self::I32(left), Self::I32(right)) => Self::I32(left ^ right),
            (Self::I64(left), Self::I64(right)) => Self::I64(left ^ right),
            (Self::I32(left), Self::I64(right)) => Self::I64((*left as i64) ^ right),
            (Self::I128(left), Self::I128(right)) => Self::I128(left ^ right),
            (Self::GcInt(left), Self::GcInt(right)) => {
                let (sign, digits) = (*left).to_u32_digits();
                let mut int = BigInt::new(sign, digits);
                int ^= &**right;

                Self::GcInt(unsafe {
                    CRUNCH_ALLOCATOR.with(|alloc| alloc.get().as_mut().unwrap().alloc(int))
                })
            }

            (left, right) if *left == Self::None || *right == Self::None => {
                return Err(RuntimeError::new(
                    RuntimeErrorTy::NullVar,
                    format!(
                        "You cannot bitwise xor values of types '{}' and '{}'",
                        left.name(),
                        right.name()
                    ),
                ));
            }
            (left, right) => {
                return Err(RuntimeError::new(
                    RuntimeErrorTy::IncompatibleTypes,
                    format!(
                        "You cannot bitwise xor values of types '{}' and '{}'",
                        left.name(),
                        right.name()
                    ),
                ));
            }
        })
    }

    pub fn bit_and(&self, other: &Self) -> RuntimeResult<Self> {
        Ok(match (self, other) {
            (Self::Byte(left), Self::Byte(right)) => Self::Byte(left & right),
            (Self::IByte(left), Self::IByte(right)) => Self::IByte(left & right),
            (Self::I16(left), Self::I16(right)) => Self::I16(left & right),
            (Self::I32(left), Self::I32(right)) => Self::I32(left & right),
            (Self::I64(left), Self::I64(right)) => Self::I64(left & right),
            (Self::I32(left), Self::I64(right)) => Self::I64((*left as i64) & right),
            (Self::I128(left), Self::I128(right)) => Self::I128(left & right),
            (Self::GcInt(left), Self::GcInt(right)) => {
                let (sign, digits) = (*left).to_u32_digits();
                let mut int = BigInt::new(sign, digits);
                int &= &**right;

                Self::GcInt(unsafe {
                    CRUNCH_ALLOCATOR.with(|alloc| alloc.get().as_mut().unwrap().alloc(int))
                })
            }

            (left, right) if *left == Self::None || *right == Self::None => {
                return Err(RuntimeError::new(
                    RuntimeErrorTy::NullVar,
                    format!(
                        "You cannot bitwise and values of types '{}' and '{}'",
                        left.name(),
                        right.name()
                    ),
                ));
            }
            (left, right) => {
                return Err(RuntimeError::new(
                    RuntimeErrorTy::IncompatibleTypes,
                    format!(
                        "You cannot bitwise and values of types '{}' and '{}'",
                        left.name(),
                        right.name()
                    ),
                ));
            }
        })
    }

    pub fn bit_not(&self) -> RuntimeResult<Self> {
        Ok(match self {
            Self::Byte(int) => Self::Byte(!int),
            Self::IByte(int) => Self::IByte(!int),
            Self::I16(int) => Self::I16(!int),
            Self::I32(int) => Self::I32(!int),
            Self::I64(int) => Self::I64(!int),
            Self::I128(int) => Self::I128(!int),
            Self::GcInt(int) => Self::GcInt(unsafe {
                CRUNCH_ALLOCATOR.with(|alloc| alloc.get().as_mut().unwrap().alloc(!(&**int)))
            }),

            Self::F32(_int) => unimplemented!("No idea how floats work"),
            Self::F64(_int) => unimplemented!("No idea how floats work"),

            val => {
                return Err(RuntimeError::new(
                    RuntimeErrorTy::NullVar,
                    format!("Cannot apply bitwise not to the type {}", val.name()),
                ));
            }
        })
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        mem::discriminant(self) == mem::discriminant(other)
    }
}

impl Eq for Value {}

impl fmt::Display for Value {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.write_str(self.name())
    }
}

macro_rules! impl_from {
    ($( $from:ty => $variant:ident ),*) => {
        $(
            impl From<$from> for Value {
                fn from(val: $from) -> Self {
                    Self::$variant(val)
                }
            }
        )*
    };
}

impl_from! {
    u8 => Byte,
    i8 => IByte,
    i16 => I16,
    i32 => I32,
    i64 => I64,
    i128 => I128,
    f32 => F32,
    f64 => F64,
    bool => Bool,
    char => Char,
    &'static str => Str,
    Vec<Self> => Array
}

macro_rules! bytes {
    ($discrim:tt, $int:expr, $ty:ty) => {{
        let mut vec = Vec::with_capacity(core::mem::size_of::<$ty>() + 1);
        vec.push($discrim);
        vec.extend_from_slice(&$int.to_le_bytes());
        vec
    }};
}

impl Into<Vec<u8>> for Value {
    fn into(self) -> Vec<u8> {
        match self {
            Self::None => vec![0x00],
            Self::Null => vec![0x01],

            Self::Byte(int) => vec![0x02, int],
            Self::IByte(int) => vec![0x03, int as u8],
            Self::I16(int) => bytes!(0x04, int, i16),
            Self::I32(int) => bytes!(0x05, int, i32),
            Self::I64(int) => bytes!(0x06, int, i64),
            Self::I128(int) => bytes!(0x07, int, i128),

            Self::F32(int) => bytes!(0x08, int, f32),
            Self::F64(int) => bytes!(0x09, int, f64),

            Self::Bool(boolean) => vec![0x0A, boolean as u8],

            Self::Char(character) => bytes!(0x0B, character as u32, u32),
            Self::Str(string) => {
                let bytes = string.as_bytes();

                let mut vec = Vec::with_capacity(bytes.len() + 1);
                vec.push(0x0C);
                vec.extend_from_slice(&bytes);
                vec
            }
            Self::GcString(string) => {
                let bytes = (*string).as_bytes();

                let mut vec = Vec::with_capacity(bytes.len() + 1);
                vec.push(0x0C);
                vec.extend_from_slice(&bytes);
                vec
            }

            _ => unimplemented!(),
        }
    }
}

impl From<&[u8]> for Value {
    fn from(bytes: &[u8]) -> Self {
        use core::convert::TryInto;

        match bytes[0] {
            0x00 => Self::None,
            0x01 => Self::Null,

            0x02 => Self::Byte(bytes[1]),
            0x03 => Self::IByte(bytes[1] as i8),
            0x04 => Self::I16(i16::from_le_bytes(bytes[1..].try_into().unwrap())),
            0x05 => Self::I32(i32::from_le_bytes(bytes[1..].try_into().unwrap())),
            0x06 => Self::I64(i64::from_le_bytes(bytes[1..].try_into().unwrap())),
            0x07 => Self::I128(i128::from_le_bytes(bytes[1..].try_into().unwrap())),

            0x08 => Self::F32(f32::from_le_bytes(bytes[1..].try_into().unwrap())),
            0x09 => Self::F64(f64::from_le_bytes(bytes[1..].try_into().unwrap())),

            0x0A => Self::Bool(bytes[1] > 0),

            0x0B => Self::Char(
                core::char::from_u32(u32::from_le_bytes(bytes[1..].try_into().unwrap())).unwrap(),
            ),
            0x0C => Self::Str(Box::leak(
                alloc::string::String::from_utf8(bytes[1..].to_vec())
                    .unwrap()
                    .into_boxed_str(),
            )),

            _ => unimplemented!(),
        }
    }
}
