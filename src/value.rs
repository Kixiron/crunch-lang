use super::instruction::{Result, RuntimeError, RuntimeErrorTy};
use derive_more::Display;
use std::ops;

/// The length of an encoded Value
pub const VALUE_LENGTH: usize = 6;

/// A value contained in the GC
#[derive(Debug, Clone, Eq, Display)]
pub enum Value {
    #[display(fmt = "{}", _0)]
    Int(i32),
    #[display(fmt = "{}", _0)]
    Bool(bool),
    #[display(fmt = "{}", _0)]
    String(String),
    #[display(fmt = "null")]
    None,
}

impl Value {
    /// Get the type of the Value as a static str
    #[inline]
    pub fn ty(&self) -> &'static str {
        match self {
            Self::Int(_) => "int",
            Self::Bool(_) => "bool",
            Self::String(_) => "str",
            Self::None => "none",
        }
    }

    /// Convert the Value into its Bytecode representation
    // TODO: Test this
    #[inline]
    pub fn as_bytes(&self) -> ([u8; VALUE_LENGTH], Option<&[u8]>) {
        use std::mem::size_of;

        let mut bytes = [0; VALUE_LENGTH];
        let mut string_bytes = None;

        match self {
            Self::None => {
                bytes[0] = 0x00;
            }
            Self::Int(i) => {
                bytes[0] = 0x01;
                bytes[1..size_of::<i32>() + 1].copy_from_slice(&i.to_be_bytes());
            }
            Self::Bool(b) => {
                bytes[0] = 0x02;
                bytes[1] = *b as u8;
            }
            Self::String(s) => {
                bytes[0] = 0x03;
                string_bytes = Some(s.as_bytes());
            }
        }

        (bytes, string_bytes)
    }

    /// Create a Value from its Bytecode representation
    // TODO: Test this
    #[inline]
    pub fn from_bytes(
        value: [u8; VALUE_LENGTH],
        strings: &mut std::collections::VecDeque<String>,
    ) -> std::result::Result<Self, &'static str> {
        use std::{convert::TryInto, mem::size_of};

        Ok(match value[0] {
            0x00 => Self::None,
            0x01 => Self::Int(i32::from_be_bytes(
                match value[1..size_of::<i32>() + 1].try_into() {
                    Ok(val) => val,
                    Err(_) => return Err("Invalid integer"),
                },
            )),
            0x02 => Self::Bool(value[1] > 0),
            0x03 => Self::String(match strings.pop_front() {
                Some(s) => s,
                None => return Err("Not enough strings supplied"),
            }),

            _ => return Err("Invalid Value Header"),
        })
    }
}

// TODO: Test all implemented operations

impl ops::Add for Value {
    type Output = Result<Self>;

    #[inline]
    fn add(self, other: Self) -> Self::Output {
        match (self, other) {
            (Self::Int(left), Self::Int(right)) => Ok(Self::Int(right + left)),
            (Self::None, Self::None) => Ok(Self::None),
            (Self::String(left), Self::String(right)) => {
                let mut output = Vec::with_capacity(left.as_bytes().len() + right.as_bytes().len());
                output.extend_from_slice(left.as_bytes());
                output.extend_from_slice(right.as_bytes());

                Ok(Self::String(unsafe { String::from_utf8_unchecked(output) }))
            }

            (left, right) => Err(RuntimeError {
                ty: RuntimeErrorTy::IncompatibleTypes,
                message: format!(
                    "Cannot add variables of types {} and {}",
                    left.ty(),
                    right.ty()
                ),
            }),
        }
    }
}

impl ops::Sub for Value {
    type Output = Result<Self>;

    #[inline]
    fn sub(self, other: Self) -> Self::Output {
        match (self, other) {
            (Self::Int(left), Self::Int(right)) => Ok(Self::Int(right - left)),

            (left, right) => Err(RuntimeError {
                ty: RuntimeErrorTy::IncompatibleTypes,
                message: format!(
                    "Cannot subtract variables of types {} and {}",
                    left.ty(),
                    right.ty()
                ),
            }),
        }
    }
}

impl ops::Mul for Value {
    type Output = Result<Self>;

    #[inline]
    fn mul(self, other: Self) -> Self::Output {
        match (self, other) {
            (Self::Int(left), Self::Int(right)) => Ok(Self::Int(right * left)),

            (left, right) => Err(RuntimeError {
                ty: RuntimeErrorTy::IncompatibleTypes,
                message: format!(
                    "Cannot multiply variables of types {} and {}",
                    left.ty(),
                    right.ty()
                ),
            }),
        }
    }
}

impl ops::Div for Value {
    type Output = Result<Self>;

    #[inline]
    fn div(self, other: Self) -> Self::Output {
        match (self, other) {
            (Self::Int(left), Self::Int(right)) => {
                if right == 0 {
                    Err(RuntimeError {
                        ty: RuntimeErrorTy::DivideByZero,
                        message: "Cannot divide by zero".to_string(),
                    })
                } else {
                    Ok(Self::Int(left / right))
                }
            }

            (left, right) => Err(RuntimeError {
                ty: RuntimeErrorTy::IncompatibleTypes,
                message: format!(
                    "Cannot divide variables of types {} and {}",
                    left.ty(),
                    right.ty()
                ),
            }),
        }
    }
}

impl ops::BitAnd for Value {
    type Output = Result<Self>;

    #[inline]
    fn bitand(self, other: Self) -> Self::Output {
        match (self, other) {
            (Self::Int(left), Self::Int(right)) => Ok(Self::Int(right & left)),
            (Self::Bool(left), Self::Bool(right)) => Ok(Self::Bool(right & left)),

            (left, right) => Err(RuntimeError {
                ty: RuntimeErrorTy::IncompatibleTypes,
                message: format!(
                    "Cannot bitwise and variables of types {} and {}",
                    left.ty(),
                    right.ty()
                ),
            }),
        }
    }
}

impl ops::BitOr for Value {
    type Output = Result<Self>;

    #[inline]
    fn bitor(self, other: Self) -> Self::Output {
        match (self, other) {
            (Self::Int(left), Self::Int(right)) => Ok(Self::Int(right | left)),
            (Self::Bool(left), Self::Bool(right)) => Ok(Self::Bool(right | left)),

            (left, right) => Err(RuntimeError {
                ty: RuntimeErrorTy::IncompatibleTypes,
                message: format!(
                    "Cannot bitwise or variables of types {} and {}",
                    left.ty(),
                    right.ty()
                ),
            }),
        }
    }
}

impl ops::BitXor for Value {
    type Output = Result<Self>;

    #[inline]
    fn bitxor(self, other: Self) -> Self::Output {
        match (self, other) {
            (Self::Int(left), Self::Int(right)) => Ok(Self::Int(right ^ left)),
            (Self::Bool(left), Self::Bool(right)) => Ok(Self::Bool(right ^ left)),

            (left, right) => Err(RuntimeError {
                ty: RuntimeErrorTy::IncompatibleTypes,
                message: format!(
                    "Cannot bitwise xor variables of types {} and {}",
                    left.ty(),
                    right.ty()
                ),
            }),
        }
    }
}

impl ops::Not for Value {
    type Output = Result<Self>;

    #[inline]
    fn not(self) -> Self::Output {
        match self {
            Self::Int(int) => Ok(Self::Int(!int)),
            Self::Bool(boolean) => Ok(Self::Bool(!boolean)),

            val => Err(RuntimeError {
                ty: RuntimeErrorTy::IncompatibleTypes,
                message: format!("Cannot apply not to variable of type {}", val.ty()),
            }),
        }
    }
}

impl std::cmp::PartialOrd for Value {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Self::Int(left), Self::Int(right)) => Some(left.cmp(right)),
            (Self::Bool(left), Self::Bool(right)) => Some(left.cmp(right)),
            (Self::String(left), Self::String(right)) => Some(left.cmp(right)),

            (_, _) => None,
        }
    }
}

impl std::cmp::PartialEq for Value {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Int(left), Self::Int(right)) => left == right,
            (Self::Bool(left), Self::Bool(right)) => left == right,
            (Self::String(left), Self::String(right)) => left == right,
            (Self::None, Self::None) => true,

            (_, _) => false,
        }
    }
}
