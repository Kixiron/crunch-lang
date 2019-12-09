use derive_more::{
    Add, AddAssign, Constructor, Display, From, Into, Mul, MulAssign, Sub, SubAssign,
};
use shrinkwraprs::Shrinkwrap;

// TODO: Are these types really needed?

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
    Shrinkwrap,
    SubAssign,
    Sub,
)]
#[display(fmt = "{}", "_0")]
#[repr(transparent)]
pub struct Index(pub u32);

#[derive(
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
    Shrinkwrap,
    SubAssign,
    Sub,
)]
#[display(fmt = "Rx{:03}", "_0")]
#[repr(transparent)]
pub struct Register(pub u8);

impl std::fmt::Debug for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Rx{:03}", self.0)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, From, Into, Shrinkwrap)]
pub struct Bytecode<'a>(&'a [u8]);

impl<'a> Bytecode<'a> {
    #[inline]
    pub fn validate(bytes: &'a [u8]) -> Result<Self, &'static str> {
        // TODO: Write this

        Ok(Self(bytes))
    }
}
