use super::{Gc, GcBigInt, GcBigUint, GcStr, GcVec, Result, RuntimeError, RuntimeErrorTy};
use std::fmt;

// TODO: Test all implemented operations

#[derive(Debug, Clone)]
pub enum RuntimeValue {
    // Unsigned integers
    Byte(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    U128(u128),
    GcUint(GcBigUint),

    // Signed integers
    IByte(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    I128(i128),
    GcInt(GcBigInt),

    // Floats
    F32(f32),
    F64(f64),

    // Strings
    Char(char),
    Str(&'static str),
    GcString(GcStr),

    // Boolean
    Bool(bool),
    // Pointer
    Pointer(usize),
    // Vec
    GcVec(GcVec<RuntimeValue>),
    // Null
    Null,

    None,
}

impl RuntimeValue {
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
            Self::GcVec(_) => "vec",
            Self::Null => "null",
            Self::None => "NoneType",
        }
    }

    // TODO: Add similar-type eq
    pub fn is_equal(self, other: Self, gc: &Gc) -> Result<bool> {
        Ok(match (self, other) {
            (Self::Byte(left), Self::Byte(right)) => left == right,
            (Self::U16(left), Self::U16(right)) => left == right,
            (Self::U32(left), Self::U32(right)) => left == right,
            (Self::U64(left), Self::U64(right)) => left == right,
            (Self::U128(left), Self::U128(right)) => left == right,
            (Self::GcUint(left), Self::GcUint(right)) => {
                *left.to_uint(gc)? == *right.to_uint(gc)?
            }

            (Self::IByte(left), Self::IByte(right)) => left == right,
            (Self::I16(left), Self::I16(right)) => left == right,
            (Self::I32(left), Self::I32(right)) => left == right,
            (Self::I64(left), Self::I64(right)) => left == right,
            (Self::I128(left), Self::I128(right)) => left == right,
            (Self::GcInt(left), Self::GcInt(right)) => *left.to_int(gc)? == *right.to_int(gc)?,

            (Self::F32(_left), Self::F32(_right)) => unimplemented!("No idea how floats work"),
            (Self::F64(_left), Self::F64(_right)) => unimplemented!("No idea how floats work"),

            (Self::Pointer(left), Self::Pointer(right)) => left == right,

            (left, right) if left == Self::None || right == Self::None => {
                return Err(RuntimeError {
                    ty: RuntimeErrorTy::NullVar,
                    message: format!(
                        "Values of types '{}' and '{}' cannot be equal",
                        left.name(),
                        right.name()
                    ),
                });
            }
            (_, _) => false,
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
            Self::Char(c) => c.to_string(),
            Self::GcString(string) => string.to_str(gc)?.to_string(),
            Self::Str(string) => (*string).to_string(),
            Self::GcInt(int) => int.to_int(gc)?.to_string(),
            Self::GcUint(int) => int.to_uint(gc)?.to_string(),
            Self::GcVec(vec) => format!("{:?}", *vec.to_vec(gc)?),
            Self::Null => "null".to_string(),
            Self::None => "NoneType".to_string(),
        })
    }

    pub fn drop(self, gc: &mut Gc) -> Result<()> {
        match self {
            Self::GcString(string) => string.drop(gc)?,
            Self::GcInt(int) => int.drop(gc)?,
            Self::GcUint(int) => int.drop(gc)?,
            Self::GcVec(vec) => vec.drop(gc)?,
            _ => {}
        }

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
            (Self::U128(left), Self::U128(right)) => {
                if let Some(result) = left.checked_add(*right) {
                    Self::U128(result)
                } else {
                    Self::GcInt(GcBigInt::new_adding(*left, *right, gc)?)
                }
            }
            (Self::GcUint(left), Self::GcUint(right)) => Self::GcUint(left.add(*right, gc)?),

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
                    Self::GcInt(GcBigInt::new_adding(*left, *right, gc)?)
                }
            }
            (Self::GcInt(left), Self::GcInt(right)) => Self::GcInt(left.add(*right, gc)?),

            (Self::F32(_left), Self::F32(_right)) => unimplemented!("No idea how floats work"),
            (Self::F64(_left), Self::F64(_right)) => unimplemented!("No idea how floats work"),

            (Self::Pointer(left), Self::Pointer(right)) => {
                if let Some(ptr) = left.checked_add(*right) {
                    Self::Pointer(ptr)
                } else {
                    return Err(RuntimeError {
                        ty: RuntimeErrorTy::IntegerOverflow,
                        message: format!(
                            "The attempted subtract is too large to fit in a '{}'",
                            self.name()
                        ),
                    });
                }
            }

            (Self::Str(left), Self::Str(right)) => {
                Self::GcString(GcStr::new_adding(left, right, gc)?)
            }
            (Self::GcString(left), Self::GcString(right)) => Self::GcString(left.add(*right, gc)?),

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

    pub fn bit_not(self, gc: &mut Gc) -> Result<Self> {
        Ok(match self {
            Self::Byte(int) => Self::Byte(!int),
            Self::U16(int) => Self::U16(!int),
            Self::U32(int) => Self::U32(!int),
            Self::U64(int) => Self::U64(!int),
            Self::U128(int) => Self::U128(!int),
            Self::GcUint(int) => Self::GcUint(int.bit_not(gc)?),

            Self::IByte(int) => Self::IByte(!int),
            Self::I16(int) => Self::I16(!int),
            Self::I32(int) => Self::I32(!int),
            Self::I64(int) => Self::I64(!int),
            Self::I128(int) => Self::I128(!int),
            Self::GcInt(int) => Self::GcInt(int.bit_not(gc)?),

            Self::F32(_int) => unimplemented!("No idea how floats work"),
            Self::F64(_int) => unimplemented!("No idea how floats work"),

            Self::Pointer(int) => Self::Pointer(!int),

            val => {
                return Err(RuntimeError {
                    ty: RuntimeErrorTy::NullVar,
                    message: format!("Cannot apply the bitwise not to the type {}", val.name()),
                });
            }
        })
    }
}

macro_rules! upflowing {
    ($ty:ty, $([$name:tt, $func:tt, $func_two:tt, $func_three:tt, $err_one:literal, $err_two:literal]),*) => {
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
                            if let Some(result) = left.$func(*right) {
                                Self::U128(result)
                            } else {
                                Self::GcInt(GcBigInt::$func_three(*left, *right, gc)?)
                            }
                        }
                        (Self::GcUint(left), Self::GcUint(right)) => Self::GcUint(left.$func_two(*right, gc)?),

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
                                Self::GcInt(GcBigInt::$func_three(*left, *right, gc)?)
                            }
                        }
                        (Self::GcInt(left), Self::GcInt(right)) => Self::GcInt(left.$func_two(*right, gc)?),

                        (Self::F32(_left), Self::F32(_right)) => unimplemented!("No idea how floats work"),
                        (Self::F64(_left), Self::F64(_right)) => unimplemented!("No idea how floats work"),

                        (Self::Pointer(left), Self::Pointer(right)) => {
                            if let Some(ptr) = left.$func(*right) {
                                Self::Pointer(ptr)
                            } else {
                                return Err(RuntimeError {
                                    ty: RuntimeErrorTy::IntegerOverflow,
                                    message: format!(
                                        $err_one,
                                        self.name()
                                    ),
                                });
                            }
                        }

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
                pub fn $name(self, other: Self, gc: &mut Gc) -> Result<Self> {
                    Ok(match (self, other) {
                        (Self::Byte(left), Self::Byte(right)) => Self::Byte(left $op right),
                        (Self::U16(left), Self::U16(right)) => Self::U16(left $op right),
                        (Self::U32(left), Self::U32(right)) => Self::U32(left $op right),
                        (Self::U64(left), Self::U64(right)) => Self::U64(left $op right),
                        (Self::U128(left), Self::U128(right)) => Self::U128(left $op right),
                        (Self::GcUint(left), Self::GcUint(right)) => Self::GcUint(left.$func(right, gc)?),

                        (Self::IByte(left), Self::IByte(right)) => Self::IByte(left $op right),
                        (Self::I16(left), Self::I16(right)) => Self::I16(left $op right),
                        (Self::I32(left), Self::I32(right)) => Self::I32(left $op right),
                        (Self::I64(left), Self::I64(right)) => Self::I64(left $op right),
                        (Self::I128(left), Self::I128(right)) => Self::I128(left $op right),
                        (Self::GcInt(left), Self::GcInt(right)) => Self::GcInt(left.$func(right, gc)?),

                        (Self::F32(_left), Self::F32(_right)) => unimplemented!("No idea how floats work"),
                        (Self::F64(_left), Self::F64(_right)) => unimplemented!("No idea how floats work"),

                        (Self::Pointer(left), Self::Pointer(right)) => Self::Pointer(left $op right),

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
    RuntimeValue,
    [
        sub_upflowing,
        checked_sub,
        sub,
        new_subtracting,
        "The attempted subtract is too large to fit in a '{}'",
        "Values of types '{}' and '{}' cannot be subtracted"
    ],
    [
        mult_upflowing,
        checked_mul,
        mult,
        new_multiplying,
        "The attempted multiply is too large to fit in a '{}'",
        "Values of types '{}' and '{}' cannot be multiplied"
    ],
    [
        div_upflowing,
        checked_div,
        div,
        new_dividing,
        "The attempted divide is too large to fit in a '{}'",
        "Values of types '{}' and '{}' cannot be divided"
    ]
);

binary_op!(
    RuntimeValue,
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

impl PartialEq for RuntimeValue {
    fn eq(&self, other: &Self) -> bool {
        use std::mem::discriminant;

        discriminant(self) == discriminant(other)
    }
}

impl Eq for RuntimeValue {}

impl fmt::Display for RuntimeValue {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.write_str(self.name())
    }
}
