use super::Registers;
use derive_more::{Add, AddAssign, Constructor, Display, From, Into, Mul, MulAssign};
use serde::{Deserialize, Serialize};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Display)]
pub enum Value {
    #[display(fmt = "{}", _0)]
    Int(i32),
    #[display(fmt = "StringPointer to StrReg {}", _0)]
    Str(StringPointer),
    #[display(fmt = "{}", _0)]
    Bool(bool),
    #[display(fmt = "Empty Register")]
    None,
}

impl Value {
    #[inline]
    pub fn add(self, other: Self, registers: &mut super::Registers) -> Self {
        use Value::*;

        match (self, other) {
            (Int(left), Int(right)) => Int(left + right),
            (Str(left_reg), Str(right_reg)) => {
                let left = registers.get_str(left_reg);
                let right = registers.get_str(right_reg);

                let mut new = String::with_capacity(left.len() + right.len());
                new.push_str(left);
                new.push_str(right);

                registers.load_str(new, left_reg.into());

                Str(left_reg)
            }
            (None, None) => None,
            (_, _) => unreachable!(),
        }
    }

    #[inline]
    pub fn eq(self, other: Self, registers: &mut super::Registers) -> bool {
        use Value::*;

        match (self, other) {
            (Int(left), Int(right)) => left == right,
            (Str(ref left), Str(ref right)) => left.fetch(registers) == right.fetch(registers),
            (Bool(left), Bool(right)) => left == right,
            (None, None) => true,
            (_, _) => false,
        }
    }
}

macro_rules! sub {
    ($value:ty) => {
        #[allow(clippy::use_self)]
        impl std::ops::Sub for $value {
            type Output = Value;

            #[inline]
            fn sub(self, other: $value) -> Self::Output {
                use Value::*;

                match (self, other) {
                    (Int(left), Int(right)) => Int(left - right),
                    (None, None) => None,
                    (_, _) => unreachable!(),
                }
            }
        }
    };
}

#[allow(clippy::use_self)]
sub!(Value);
sub!(&Value);

impl std::ops::AddAssign for Value {
    #[inline]
    fn add_assign(&mut self, other: Self) {
        use Value::*;

        match (self, &other) {
            (Int(left), Int(right)) => *left += right,
            (None, None) => {}
            (_, _) => unreachable!(),
        }
    }
}

impl std::ops::SubAssign for Value {
    #[inline]
    fn sub_assign(&mut self, other: Self) {
        use Value::*;

        match (self, &other) {
            (Int(left), Int(right)) => *left -= right,
            (None, None) => {}
            (_, _) => unreachable!(),
        }
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
)]
#[display(fmt = "{}", "_0")]
pub struct StringPointer(u8);

impl StringPointer {
    #[inline]
    pub fn fetch(self, registers: &Registers) -> &str {
        registers.get_str(self.0.into())
    }
}

impl ::std::ops::Deref for StringPointer {
    type Target = u8;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl ::std::ops::DerefMut for StringPointer {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
