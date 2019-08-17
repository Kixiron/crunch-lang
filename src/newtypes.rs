use derive_more::{
    Add, AddAssign, Constructor, Display, From, Into, Mul, MulAssign, Sub, SubAssign,
};
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
    SubAssign,
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
    SubAssign,
    Sub,
)]
#[display(fmt = "{}", "_0")]
#[repr(transparent)]
pub struct Index(pub u32);

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
    SubAssign,
    Sub,
)]
#[display(fmt = "{}", "_0")]
#[repr(transparent)]
pub struct Register(pub u8);
