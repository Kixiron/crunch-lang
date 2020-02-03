use super::{AllocId, Collectable, Gc, Heap, Result, ReturnFrame, RuntimeError, RuntimeErrorTy};
use num_bigint::{BigInt, BigUint};
use std::fmt;

// TODO: Test all implemented operations
// TODO: Implement all inter-int operations
// Eg. IByte and I32 can be added together

#[derive(Debug, Clone)]
#[repr(u8)]
pub enum Value {
    // Unsigned integers
    Byte(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    U128(u128),
    GcUint(Heap<BigUint>),

    // Signed integers
    IByte(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    I128(i128),
    GcInt(Heap<BigInt>),

    // Floats
    F32(f32),
    F64(f64),

    // Strings
    Char(char),
    Str(&'static str),
    GcString(Heap<&'static str>),

    Bool(bool),
    Pointer(AllocId),
    Library(std::sync::Arc<dlopen::raw::Library>),
    Null,

    Generator(Box<ReturnFrame>),

    None,
}

#[derive(Debug, Clone, Copy)]
/// Gauges the relationship between two Values,
/// Always reflects Equivalency, sometimes also reflects Greater/Less than.
pub enum Compare {
    Equal,
    Less,
    Greater,
    Unequal,
    /// Sometimes, things just don't line up.
    /// (trying to compare a Bool to a BigInt, are ya?)
    Incomparable,
}

impl Compare {
    /// Generates a Comparison which respects the nuances of greater than and equal to.
    /// Usually only applicable for numbers.
    ///
    /// Comparison to i.e. `std::f32::NAN` simply yields `Unequal`, as is the case with Rust.
    fn ordering<O: std::cmp::PartialOrd>(a: &O, b: &O) -> Self {
        use std::cmp::Ordering;

        a.partial_cmp(b)
            .map(|c| match c {
                Ordering::Less => Compare::Less,
                Ordering::Greater => Compare::Greater,
                Ordering::Equal => Compare::Equal,
            })
            .unwrap_or(Compare::Unequal)
    }

    /// A Bool, for example, can't be "less than" another Bool, but it can be different.
    /// Types that impl PartialOrd probably shouldn't be compared this way, prefer `Compare::full`.
    fn just_equality<E: Eq>(a: &E, b: &E) -> Self {
        match a == b {
            true => Compare::Equal,
            false => Compare::Unequal,
        }
    }
}

impl Value {
    #[must_use]
    pub fn name(&self) -> &'static str {
        match self {
            Self::Byte(_) => "byte",
            Self::U16(_) => "uint16",
            Self::U32(_) => "uint",
            Self::U64(_) => "uint64",
            Self::U128(_) => "uint128",
            Self::IByte(_) => "ibyte",
            Self::I16(_) => "int16",
            Self::I32(_) => "int",
            Self::I64(_) => "int64",
            Self::I128(_) => "int128",
            Self::F32(_) => "float",
            Self::F64(_) => "float64",
            Self::Bool(_) => "bool",
            Self::Pointer(_) => "ptr",
            Self::Char(_) => "char",
            Self::GcString(_) | Self::Str(_) => "str",
            Self::GcInt(_) => "bigint",
            Self::GcUint(_) => "biguint",
            Self::Library(_) => "lib",
            Self::Generator(_) => "gen",
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
    pub fn compare(&self, other: &Self, gc: &Gc) -> Result<Compare> {
        Ok(match (self, other) {
            (Self::Byte(left), Self::Byte(right)) => Compare::ordering(left, right),
            (Self::U16(left), Self::U16(right)) => Compare::ordering(left, right),
            (Self::U32(left), Self::U32(right)) => Compare::ordering(left, right),
            (Self::U64(left), Self::U64(right)) => Compare::ordering(left, right),
            (Self::U128(left), Self::U128(right)) => Compare::ordering(left, right),
            (Self::GcUint(left), Self::GcUint(right)) => {
                Compare::ordering(&left.fetch(gc)?, &right.fetch(gc)?)
            }

            (Self::IByte(left), Self::IByte(right)) => Compare::ordering(left, right),
            (Self::I16(left), Self::I16(right)) => Compare::ordering(left, right),
            (Self::I32(left), Self::I32(right)) => Compare::ordering(left, right),
            (Self::I64(left), Self::I64(right)) => Compare::ordering(left, right),
            (Self::I128(left), Self::I128(right)) => Compare::ordering(left, right),
            (Self::GcInt(left), Self::GcInt(right)) => {
                Compare::ordering(&left.fetch(gc)?, &right.fetch(gc)?)
            }

            (Self::F32(left), Self::F32(right)) => Compare::ordering(left, right),
            (Self::F64(left), Self::F64(right)) => Compare::ordering(left, right),

            (Self::Pointer(left), Self::Pointer(right)) => Compare::just_equality(left, right),

            (Self::Bool(left), Self::Bool(right)) => Compare::just_equality(left, right),

            (Value::Null, _) | (_, Value::Null) => Compare::Unequal,

            (left, right) if left == &Self::None || right == &Self::None => {
                return Err(RuntimeError {
                    ty: RuntimeErrorTy::NullVar,
                    message: format!(
                        "Values of types '{}' and '{}' cannot be equal",
                        left.name(),
                        right.name()
                    ),
                });
            }
            (_, _) => Compare::Incomparable,
        })
    }

    #[cfg(test)]
    pub(crate) fn is_equal(&self, other: &Self, gc: &Gc) -> Result<bool> {
        Ok(match self.compare(other, gc)? {
            Compare::Equal => true,
            _ => false,
        })
    }

    pub fn to_string(&self, gc: &Gc) -> Result<String> {
        Ok(match self {
            Self::Byte(int) => int.to_string(),
            Self::U16(int) => int.to_string(),
            Self::U32(int) => int.to_string(),
            Self::U64(int) => int.to_string(),
            Self::U128(int) => int.to_string(),
            Self::IByte(int) => int.to_string(),
            Self::I16(int) => int.to_string(),
            Self::I32(int) => int.to_string(),
            Self::I64(int) => int.to_string(),
            Self::I128(int) => int.to_string(),
            Self::F32(int) => int.to_string(),
            Self::F64(int) => int.to_string(),
            Self::Bool(int) => int.to_string(),
            Self::Pointer(int) => format!("{:p}", int as *const _),
            Self::Library(lib) => format!("{:?}", lib),
            Self::Char(c) => c.to_string(),
            Self::GcString(string) => string.fetch(gc)?,
            Self::Str(string) => (*string).to_string(),
            Self::GcInt(int) => int.fetch(gc)?.to_string(),
            Self::GcUint(int) => int.fetch(gc)?.to_string(),
            Self::Generator(gen) => format!("{:p}", gen.function_index as *const u8),
            Self::Null => "null".to_string(),
            Self::None => "NoneType".to_string(),
        })
    }

    pub fn drop(&mut self, gc: &Gc) -> Result<()> {
        match self {
            Self::GcInt(int) => int.drop(gc)?,
            Self::GcUint(uint) => uint.drop(gc)?,
            Self::GcString(string) => string.drop(gc)?,
            _ => {}
        }

        *self = Self::None;

        Ok(())
    }

    pub fn from_bytes(
        _bytes: &[u8],
        _strings: &mut std::collections::VecDeque<String>,
    ) -> Result<Self> {
        unimplemented!()
    }

    #[must_use]
    #[allow(clippy::unused_self)]
    pub fn as_bytes(&self) -> (Vec<u8>, Option<String>) {
        unimplemented!()
    }

    pub fn add_upflowing(self, other: Self, gc: &mut Gc) -> Result<Self> {
        Ok(match (&self, &other) {
            (Self::Byte(left), Self::Byte(right)) => {
                if let Some(result) = left.checked_add(*right) {
                    Self::Byte(result)
                } else {
                    Self::U16(*left as u16).add_upflowing(Self::U16(*right as u16), gc)?
                }
            }
            (Self::U16(left), Self::U16(right)) => {
                if let Some(result) = left.checked_add(*right) {
                    Self::U16(result)
                } else {
                    Self::U32(*left as u32).add_upflowing(Self::U32(*right as u32), gc)?
                }
            }
            (Self::U32(left), Self::U32(right)) => {
                if let Some(result) = left.checked_add(*right) {
                    Self::U32(result)
                } else {
                    Self::U64(*left as u64).add_upflowing(Self::U64(*right as u64), gc)?
                }
            }
            (Self::U64(left), Self::U64(right)) => {
                if let Some(result) = left.checked_add(*right) {
                    Self::U64(result)
                } else {
                    Self::U128(*left as u128).add_upflowing(Self::U128(*right as u128), gc)?
                }
            }
            // TODO: Optimize bigint operations
            (Self::U128(left), Self::U128(right)) => {
                if let Some(result) = left.checked_add(*right) {
                    Self::U128(result)
                } else {
                    let mut int = BigUint::from_bytes_le(&left.to_le_bytes());
                    int += *right;

                    let heap = int.alloc(gc)?;

                    Self::GcUint(heap)
                }
            }
            (Self::GcUint(left), Self::GcUint(right)) => {
                let left = left.fetch(gc)?;
                let right = right.fetch(gc)?;

                let heap = (left + right).alloc(gc)?;

                Self::GcUint(heap)
            }

            (Self::IByte(left), Self::IByte(right)) => {
                if let Some(result) = left.checked_add(*right) {
                    Self::IByte(result)
                } else {
                    Self::I16(*left as i16).add_upflowing(Self::I16(*right as i16), gc)?
                }
            }
            (Self::I16(left), Self::I16(right)) => {
                if let Some(result) = left.checked_add(*right) {
                    Self::I16(result)
                } else {
                    Self::I32(*left as i32).add_upflowing(Self::I32(*right as i32), gc)?
                }
            }
            (Self::I32(left), Self::I32(right)) => {
                if let Some(result) = left.checked_add(*right) {
                    Self::I32(result)
                } else {
                    Self::I64(*left as i64).add_upflowing(Self::I64(*right as i64), gc)?
                }
            }
            (Self::I64(left), Self::I64(right)) => {
                if let Some(result) = left.checked_add(*right) {
                    Self::I64(result)
                } else {
                    Self::I128(*left as i128).add_upflowing(Self::I128(*right as i128), gc)?
                }
            }
            (Self::I128(left), Self::I128(right)) => {
                if let Some(result) = left.checked_add(*right) {
                    Self::I128(result)
                } else {
                    let mut int = BigInt::from_signed_bytes_le(&left.to_le_bytes());
                    int += *right;

                    let heap = int.alloc(gc)?;

                    Self::GcInt(heap)
                }
            }
            (Self::GcInt(left), Self::GcInt(right)) => {
                let left = left.fetch(gc)?;
                let right = right.fetch(gc)?;

                let heap = (left + right).alloc(gc)?;

                Self::GcInt(heap)
            }

            (Self::F32(left), Self::F32(right)) => Self::F32(left + right),
            (Self::F64(left), Self::F64(right)) => Self::F64(left + right),

            (Self::Str(left), Self::Str(right)) => {
                let unallocated = (*left).to_string() + right;
                let new = <&str>::alloc(
                    unsafe { std::mem::transmute::<&str, &'static str>(&unallocated) },
                    gc,
                )?;

                Self::GcString(new)
            }

            (Self::GcString(left), Self::GcString(right)) => {
                let unallocated = left.fetch(gc)? + &right.fetch(gc)?;
                let new = <&str>::alloc(
                    unsafe { std::mem::transmute::<&str, &'static str>(&unallocated) },
                    gc,
                )?;
                Self::GcString(new)
            }

            (left, right) if left == &Self::None || right == &Self::None => {
                error!(
                    "Values of types {} and {} cannot be added",
                    left.name(),
                    right.name()
                );
                return Err(RuntimeError {
                    ty: RuntimeErrorTy::NullVar,
                    message: format!(
                        "Values of types '{}' and '{}' cannot be added",
                        left.name(),
                        right.name()
                    ),
                });
            }
            (left, right) => {
                error!(
                    "Values of types {} and {} cannot be added",
                    left.name(),
                    right.name()
                );
                return Err(RuntimeError {
                    ty: RuntimeErrorTy::IncompatibleTypes,
                    message: format!(
                        "Values of types '{}' and '{}' cannot be added",
                        left.name(),
                        right.name()
                    ),
                });
            }
        })
    }

    pub fn bit_not(self, _gc: &mut Gc) -> Result<Self> {
        Ok(match self {
            Self::Byte(int) => Self::Byte(!int),
            Self::U16(int) => Self::U16(!int),
            Self::U32(int) => Self::U32(!int),
            Self::U64(int) => Self::U64(!int),
            Self::U128(int) => Self::U128(!int),
            // Self::GcUint(int) => Self::GcUint(int.bit_not(gc)?),
            Self::IByte(int) => Self::IByte(!int),
            Self::I16(int) => Self::I16(!int),
            Self::I32(int) => Self::I32(!int),
            Self::I64(int) => Self::I64(!int),
            Self::I128(int) => Self::I128(!int),
            // Self::GcInt(int) => Self::GcInt(int.bit_not(gc)?),
            Self::F32(_int) => unimplemented!("No idea how floats work"),
            Self::F64(_int) => unimplemented!("No idea how floats work"),

            val => {
                return Err(RuntimeError {
                    ty: RuntimeErrorTy::NullVar,
                    message: format!("Cannot apply the bitwise not to the type {}", val.name()),
                });
            }
        })
    }

    pub fn div_upflowing(self, other: Self, gc: &mut Gc) -> Result<Self> {
        Ok(match (&self, &other) {
            (Self::Byte(left), Self::Byte(right)) => {
                if let Some(result) = left.checked_div(*right) {
                    Self::Byte(result)
                } else {
                    Self::U16(*left as u16).div_upflowing(Self::U16(*right as u16), gc)?
                }
            }
            (Self::U16(left), Self::U16(right)) => {
                if let Some(result) = left.checked_div(*right) {
                    Self::U16(result)
                } else {
                    Self::U32(*left as u32).div_upflowing(Self::U32(*right as u32), gc)?
                }
            }
            (Self::U32(left), Self::U32(right)) => {
                if let Some(result) = left.checked_div(*right) {
                    Self::U32(result)
                } else {
                    Self::U64(*left as u64).div_upflowing(Self::U64(*right as u64), gc)?
                }
            }
            (Self::U64(left), Self::U64(right)) => {
                if let Some(result) = left.checked_div(*right) {
                    Self::U64(result)
                } else {
                    Self::U128(*left as u128).div_upflowing(Self::U128(*right as u128), gc)?
                }
            }
            (Self::U128(left), Self::U128(right)) => {
                if let Some(result) = left.checked_add(*right) {
                    Self::U128(result)
                } else {
                    let int = BigUint::from_bytes_le(&left.to_le_bytes());
                    let heap = (int / *right).alloc(gc)?;

                    Self::GcUint(heap)
                }
            }
            (Self::GcUint(left), Self::GcUint(right)) => {
                let left = left.fetch(gc)?;
                let right = right.fetch(gc)?;

                let heap = (left / right).alloc(gc)?;

                Self::GcUint(heap)
            }

            (Self::IByte(left), Self::IByte(right)) => {
                if let Some(result) = left.checked_div(*right) {
                    Self::IByte(result)
                } else {
                    Self::I16(*left as i16).div_upflowing(Self::I16(*right as i16), gc)?
                }
            }
            (Self::I16(left), Self::I16(right)) => {
                if let Some(result) = left.checked_div(*right) {
                    Self::I16(result)
                } else {
                    Self::I32(*left as i32).div_upflowing(Self::I32(*right as i32), gc)?
                }
            }
            (Self::I32(left), Self::I32(right)) => {
                if let Some(result) = left.checked_div(*right) {
                    Self::I32(result)
                } else {
                    Self::I64(*left as i64).div_upflowing(Self::I64(*right as i64), gc)?
                }
            }
            (Self::I32(left), Self::I64(right)) => {
                if let Some(result) = (*left as i64).checked_div(*right) {
                    Self::I64(result)
                } else {
                    Self::I64(*left as i64).div_upflowing(Self::I64(*right as i64), gc)?
                }
            }
            (Self::I64(left), Self::I64(right)) => {
                if let Some(result) = left.checked_div(*right) {
                    Self::I64(result)
                } else {
                    Self::I128(*left as i128).div_upflowing(Self::I128(*right as i128), gc)?
                }
            }
            (Self::I128(left), Self::I128(right)) => {
                if let Some(result) = left.checked_div(*right) {
                    Self::I128(result)
                } else {
                    let int = BigInt::from_signed_bytes_le(&left.to_le_bytes());
                    let heap = (int / *right).alloc(gc)?;

                    Self::GcInt(heap)
                }
            }
            (Self::GcInt(left), Self::GcInt(right)) => {
                let left = left.fetch(gc)?;
                let right = right.fetch(gc)?;

                let heap = (left / right).alloc(gc)?;

                Self::GcInt(heap)
            }

            (Self::F32(left), Self::F32(right)) if *left != 0.0 && *right != 0.0 => {
                Self::F32(left / right)
            }
            (Self::F32(_left), Self::F32(_right)) => Self::F32(0.0),

            (Self::F64(left), Self::F64(right)) if *left != 0.0 && *right != 0.0 => {
                Self::F64(left / right)
            }
            (Self::F64(_left), Self::F64(_right)) => Self::F64(0.0),

            (left, right) if left == &Self::None || right == &Self::None => {
                return Err(RuntimeError {
                    ty: RuntimeErrorTy::NullVar,
                    message: format!(
                        "Values of types '{}' and '{}' cannot be divided",
                        left.name(),
                        right.name()
                    ),
                });
            }
            (left, right) => {
                return Err(RuntimeError {
                    ty: RuntimeErrorTy::IncompatibleTypes,
                    message: format!(
                        "Values of types '{}' and '{}' cannot be divided",
                        left.name(),
                        right.name()
                    ),
                });
            }
        })
    }
}

macro_rules! upflowing {
    ($ty:ty, $([$name:tt, $func:tt, $func_two:tt, $for_floats:tt, $func_three:tt, $err_one:literal, $err_two:literal]),*) => {
        impl $ty {
            $(
                pub fn $name(self, other: Self, gc: &mut Gc) -> Result<Self> {
                    Ok(match (&self, &other) {
                        (Self::Byte(left), Self::Byte(right)) => {
                            if let Some(result) = left.$func(*right) {
                                Self::Byte(result)
                            } else {
                                Self::U16(*left as u16).$name(Self::U16(*right as u16), gc)?
                            }
                        }
                        (Self::U16(left), Self::U16(right)) => {
                            if let Some(result) = left.$func(*right) {
                                Self::U16(result)
                            } else {
                                Self::U32(*left as u32).$name(Self::U32(*right as u32), gc)?
                            }
                        }
                        (Self::U32(left), Self::U32(right)) => {
                            if let Some(result) = left.$func(*right) {
                                Self::U32(result)
                            } else {
                                Self::U64(*left as u64).$name(Self::U64(*right as u64), gc)?
                            }
                        }
                        (Self::U64(left), Self::U64(right)) => {
                            if let Some(result) = left.$func(*right) {
                                Self::U64(result)
                            } else {
                                Self::U128(*left as u128).$name(Self::U128(*right as u128), gc)?
                            }
                        }
                        (Self::U128(left), Self::U128(right)) => {
                            if let Some(result) = left.checked_add(*right) {
                                Self::U128(result)
                            } else {
                                let int = BigUint::from_bytes_le(&left.to_le_bytes());
                                let heap = (int $for_floats *right).alloc(gc)?;

                                Self::GcUint(heap)
                            }
                        }
                        (Self::GcUint(left), Self::GcUint(right)) => {
                            let left = left.fetch(gc)?;
                            let right = right.fetch(gc)?;

                            let heap = (left $for_floats right).alloc(gc)?;

                            Self::GcUint(heap)
                        }

                        (Self::IByte(left), Self::IByte(right)) => {
                            if let Some(result) = left.$func(*right) {
                                Self::IByte(result)
                            } else {
                                Self::I16(*left as i16).$name(Self::I16(*right as i16), gc)?
                            }
                        }
                        (Self::I16(left), Self::I16(right)) => {
                            if let Some(result) = left.$func(*right) {
                                Self::I16(result)
                            } else {
                                Self::I32(*left as i32).$name(Self::I32(*right as i32), gc)?
                            }
                        }
                        (Self::I32(left), Self::I32(right)) => {
                            if let Some(result) = left.$func(*right) {
                                Self::I32(result)
                            } else {
                                Self::I64(*left as i64).$name(Self::I64(*right as i64), gc)?
                            }
                        }
                        (Self::I32(left), Self::I64(right)) => {
                            if let Some(result) = (*left as i64).$func(*right) {
                                Self::I64(result)
                            } else {
                                Self::I64(*left as i64).$name(Self::I64(*right as i64), gc)?
                            }
                        }
                        (Self::I64(left), Self::I64(right)) => {
                            if let Some(result) = left.$func(*right) {
                                Self::I64(result)
                            } else {
                                Self::I128(*left as i128).$name(Self::I128(*right as i128), gc)?
                            }
                        }
                        (Self::I128(left), Self::I128(right)) => {
                            if let Some(result) = left.$func(*right) {
                                Self::I128(result)
                            } else {
                                let int = BigInt::from_signed_bytes_le(&left.to_le_bytes());
                                let heap = (int $for_floats *right).alloc(gc)?;

                                Self::GcInt(heap)
                            }
                        }
                        (Self::GcInt(left), Self::GcInt(right)) => {
                            let left = left.fetch(gc)?;
                            let right = right.fetch(gc)?;

                            let heap = (left $for_floats right).alloc(gc)?;

                            Self::GcInt(heap)
                        }

                        (Self::F32(left), Self::F32(right)) => Self::F32(left $for_floats right),
                        (Self::F64(left), Self::F64(right)) => Self::F64(left $for_floats right),

                        (left, right) if left == &Self::None || right == &Self::None => {
                            return Err(RuntimeError {
                                ty: RuntimeErrorTy::NullVar,
                                message: format!(
                                    $err_two,
                                    left.name(),
                                    right.name()
                                ),
                            });
                        }
                        (left, right) => {
                            return Err(RuntimeError {
                                ty: RuntimeErrorTy::IncompatibleTypes,
                                message: format!(
                                    $err_two,
                                    left.name(),
                                    right.name()
                                ),
                            });
                        }
                    })
                }
            )*
        }
    }
}

macro_rules! binary_op {
    ($ty:ty, $([$name:tt, $op:tt, $func:tt, $err:literal]),*) => {
        impl $ty {
            $(
                pub fn $name(self, other: Self, _gc: &mut Gc) -> Result<Self> {
                    Ok(match (self, other) {
                        (Self::Byte(left), Self::Byte(right)) => Self::Byte(left $op right),
                        (Self::U16(left), Self::U16(right)) => Self::U16(left $op right),
                        (Self::U32(left), Self::U32(right)) => Self::U32(left $op right),
                        (Self::U64(left), Self::U64(right)) => Self::U64(left $op right),
                        (Self::U128(left), Self::U128(right)) => Self::U128(left $op right),
                        // (Self::GcUint(left), Self::GcUint(right)) => Self::GcUint(left.$func(right, gc)?),

                        (Self::IByte(left), Self::IByte(right)) => Self::IByte(left $op right),
                        (Self::I16(left), Self::I16(right)) => Self::I16(left $op right),
                        (Self::I32(left), Self::I32(right)) => Self::I32(left $op right),
                        (Self::I64(left), Self::I64(right)) => Self::I64(left $op right),
                        (Self::I32(left), Self::I64(right)) => Self::I64((left as i64) $op right),
                        (Self::I128(left), Self::I128(right)) => Self::I128(left $op right),
                        // (Self::GcInt(left), Self::GcInt(right)) => Self::GcInt(left.$func(right, gc)?),

                        (Self::F32(_left), Self::F32(_right)) => unimplemented!("No idea how floats work"),
                        (Self::F64(_left), Self::F64(_right)) => unimplemented!("No idea how floats work"),

                        (left, right) if left == Self::None || right == Self::None => {
                            return Err(RuntimeError {
                                ty: RuntimeErrorTy::NullVar,
                                message: format!(
                                    $err,
                                    left.name(),
                                    right.name()
                                ),
                            });
                        }
                        (left, right) => {
                            return Err(RuntimeError {
                                ty: RuntimeErrorTy::IncompatibleTypes,
                                message: format!(
                                    $err,
                                    left.name(),
                                    right.name()
                                ),
                            });
                        }
                    })
                }
            )*
        }
    }
}

upflowing!(
    Value,
    [
        sub_upflowing,
        checked_sub,
        sub,
        -,
        new_subtracting,
        "The attempted subtraction is too large to fit in a '{}'",
        "Values of types '{}' and '{}' cannot be subtracted"
    ],
    [
        mult_upflowing,
        checked_mul,
        mult,
        *,
        new_multiplying,
        "The attempted multiplication is too large to fit in a '{}'",
        "Values of types '{}' and '{}' cannot be multiplied"
    ]
);

binary_op!(
    Value,
    [
        bit_or, |, bit_or,
        "Values of types '{}' and '{}' cannot be bit ord"
    ],
    [
        bit_xor, ^, bit_xor,
        "Values of types '{}' and '{}' cannot be bit xored"
    ],
    [
        bit_and, &, bit_and,
        "Values of types '{}' and '{}' cannot be bit anded"
    ]
);

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        use std::mem::discriminant;

        discriminant(self) == discriminant(other)
    }
}

impl Eq for Value {}

impl fmt::Display for Value {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.write_str(self.name())
    }
}

macro_rules! bytes {
    ($discrim:tt, $int:expr, $ty:ty) => {{
        let mut vec = Vec::with_capacity(size_of::<$ty>() + 1);
        vec.push($discrim);
        vec.extend_from_slice(&$int.to_le_bytes());
        vec
    }};
}

impl Into<Vec<u8>> for Value {
    fn into(self) -> Vec<u8> {
        use std::mem::size_of;

        match self {
            Self::None => vec![0x00],
            Self::Null => vec![0x01],

            Self::Byte(int) => vec![0x02, int],
            Self::U16(int) => bytes!(0x03, int, u16),
            Self::U32(int) => bytes!(0x04, int, u32),
            Self::U64(int) => bytes!(0x05, int, u64),
            Self::U128(int) => bytes!(0x06, int, u128),

            Self::IByte(int) => vec![0x07, int as u8],
            Self::I16(int) => bytes!(0x08, int, i16),
            Self::I32(int) => bytes!(0x09, int, i32),
            Self::I64(int) => bytes!(0x0A, int, i64),
            Self::I128(int) => bytes!(0x0B, int, i128),

            Self::F32(int) => bytes!(0x0C, int, f32),
            Self::F64(int) => bytes!(0x0D, int, f64),

            Self::Pointer(int) => bytes!(0x0E, int, u16),

            Self::Bool(boolean) => vec![0x0F, boolean as u8],

            Self::Char(character) => bytes!(0x10, character as u32, u32),
            Self::Str(string) => {
                let bytes = string.as_bytes();

                let mut vec = Vec::with_capacity(bytes.len() + 1);
                vec.push(0x11);
                vec.extend_from_slice(&bytes);
                vec
            }

            _ => unimplemented!(),
        }
    }
}

impl From<&[u8]> for Value {
    fn from(bytes: &[u8]) -> Self {
        use std::convert::TryInto;

        match bytes[0] {
            0x00 => Self::None,
            0x01 => Self::Null,

            0x02 => Self::Byte(bytes[1]),
            0x03 => Self::U16(u16::from_le_bytes(bytes[1..].try_into().unwrap())),
            0x04 => Self::U32(u32::from_le_bytes(bytes[1..].try_into().unwrap())),
            0x05 => Self::U64(u64::from_le_bytes(bytes[1..].try_into().unwrap())),
            0x06 => Self::U128(u128::from_le_bytes(bytes[1..].try_into().unwrap())),

            0x07 => Self::IByte(bytes[1] as i8),
            0x08 => Self::I16(i16::from_le_bytes(bytes[1..].try_into().unwrap())),
            0x09 => Self::I32(i32::from_le_bytes(bytes[1..].try_into().unwrap())),
            0x0A => Self::I64(i64::from_le_bytes(bytes[1..].try_into().unwrap())),
            0x0B => Self::I128(i128::from_le_bytes(bytes[1..].try_into().unwrap())),

            0x0C => Self::F32(f32::from_le_bytes(bytes[1..].try_into().unwrap())),
            0x0D => Self::F64(f64::from_le_bytes(bytes[1..].try_into().unwrap())),

            0x0E => Self::Pointer(AllocId(usize::from_le_bytes(
                bytes[1..].try_into().unwrap(),
            ))),

            0x0F => Self::Bool(bytes[1] > 0),

            0x10 => Self::Char(
                std::char::from_u32(u32::from_le_bytes(bytes[1..].try_into().unwrap())).unwrap(),
            ),
            0x11 => Self::Str(Box::leak(
                String::from_utf8(bytes[1..].to_vec())
                    .unwrap()
                    .into_boxed_str(),
            )),

            _ => unimplemented!(),
        }
    }
}
