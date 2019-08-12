use super::Registers;
use derive_more::{Add, AddAssign, Constructor, Display, From, Into, Mul, MulAssign};
use serde::{Deserialize, Serialize};
use shrinkwraprs::Shrinkwrap;

/// A pointer/index for a string
#[derive(
    Debug,
    Copy,
    Clone,
    PartialEq,
    Eq,
    Add,
    Mul,
    AddAssign,
    MulAssign,
    Constructor,
    Display,
    From,
    Into,
    Deserialize,
    Serialize,
    Shrinkwrap,
)]
#[display(fmt = "{}", "_0")]
#[repr(transparent)]
pub struct StringPointer(pub u8);

impl StringPointer {
    /// Gets a string
    #[inline]
    pub fn get(self, registers: &Registers) -> &str {
        registers.get_str(self.0.into())
    }

    /// Gets a string mutably
    #[inline]
    pub fn get_mut(self, registers: &mut Registers) -> &mut String {
        registers.get_str_mut(self.0.into())
    }
}

/// The instruction index for the VM
#[derive(
    Debug,
    Copy,
    Clone,
    PartialEq,
    Eq,
    Add,
    Mul,
    AddAssign,
    MulAssign,
    Constructor,
    Display,
    From,
    Into,
    Deserialize,
    Serialize,
    Shrinkwrap,
)]
#[display(fmt = "{}", "_0")]
#[repr(transparent)]
pub struct Index(pub u32);

impl Index {
    /// Gets the index
    #[inline]
    pub const fn index(self) -> usize {
        self.0 as usize
    }

    /// Adds to the index
    #[inline]
    pub fn add(&mut self, amount: Self) {
        self.0 += amount.0
    }

    /// Subtracts from the index
    #[inline]
    pub fn sub(&mut self, amount: Self) {
        self.0 -= amount.0
    }

    /// Sets the index
    #[inline]
    pub fn set(&mut self, mut index: Self) {
        std::mem::swap(self, &mut index);
    }
}

#[derive(
    Debug,
    Copy,
    Clone,
    PartialEq,
    Eq,
    Add,
    Mul,
    AddAssign,
    MulAssign,
    Constructor,
    Display,
    From,
    Into,
    Deserialize,
    Serialize,
    Shrinkwrap,
)]
#[display(fmt = "{}", "_0")]
#[repr(transparent)]
pub struct Register(pub u8);

#[derive(Deserialize, Serialize, Debug, Clone, PartialEq, Eq, Constructor)]
pub struct LoadedString {
    pub string: String,
    pub str_reg: StringPointer,
    pub reg: Register,
}

impl LoadedString {
    #[inline]
    pub fn new_boxed(string: String, str_reg: StringPointer, reg: Register) -> Box<Self> {
        Box::new(Self::new(string, str_reg, reg))
    }
}
