pub mod ast;
pub mod hir;
pub mod mir;

use crate::{
    error::SyntaxError,
    strings::{StrInterner, StrT},
};
#[cfg(feature = "no-std")]
use alloc::{borrow::ToOwned, boxed::Box, string::String, vec, vec::Vec};
use core::{
    fmt::{Debug, Display, Formatter, Result, Write},
    ops::{Deref, DerefMut, Not},
    str::FromStr,
};
use derive_more::Display;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Eq, Hash, Deserialize, Serialize)]
pub struct Sided<T, S> {
    pub lhs: S,
    pub op: T,
    pub rhs: S,
}

#[derive(Clone, PartialEq, Eq, Hash, Deserialize, Serialize)]
pub struct Ref<T>(Box<T>);

impl<T> Ref<T> {
    pub fn new(val: T) -> Self {
        Self(Box::new(val))
    }
}

impl<T: Deref> Ref<T> {
    pub fn as_deref(&self) -> &T::Target {
        self.0.deref()
    }
}

impl<T: DerefMut> Ref<T> {
    pub fn as_deref_mut(&mut self) -> &mut T::Target {
        self.0.deref_mut()
    }
}

impl<T> AsRef<T> for Ref<T> {
    fn as_ref(&self) -> &T {
        &*self.0
    }
}

impl<T> Deref for Ref<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}

impl<T> DerefMut for Ref<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut *self.0
    }
}

impl<T: Debug> Debug for Ref<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        Debug::fmt(&*self.0, f)
    }
}

impl<T: Display> Display for Ref<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        Display::fmt(&*self.0, f)
    }
}

#[derive(Display, Debug, Copy, Clone, PartialEq, Eq, Hash, Deserialize, Serialize)]
pub enum CallConv {
    #[display(fmt = "Crunch")]
    Crunch,
    #[display(fmt = "C")]
    C,
}

impl FromStr for CallConv {
    type Err = SyntaxError;

    fn from_str(s: &str) -> core::result::Result<Self, Self::Err> {
        // Calling conventions are (hopefully) ordered in order of most use
        let callconv = match s {
            c if c.eq_ignore_ascii_case("c") => Self::C,
            crunch if crunch.eq_ignore_ascii_case("crunch") => Self::Crunch,

            conv => return Err(SyntaxError::UnrecognizedCallConv(conv.to_owned())),
        };

        Ok(callconv)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Deserialize, Serialize)]
#[repr(transparent)]
pub struct ItemPath(Vec<StrT>);

impl ItemPath {
    pub fn new(path: impl Into<Self>) -> Self {
        path.into()
    }

    pub fn join(&self, other: impl Into<Self>) -> Self {
        let mut new = self.0.clone();
        new.extend(other.into().0.drain(..));

        Self(new)
    }

    pub fn to_string(&self, interner: &StrInterner) -> String {
        let mut string = String::with_capacity(self.len() * 2);
        let mut segments = self.0.iter();
        let last = segments.next_back();

        for seg in segments {
            string.push_str(interner.resolve(*seg).as_ref());
            string.push('.');
        }

        if let Some(seg) = last {
            string.push_str(interner.resolve(*seg).as_ref());
        }

        string
    }

    pub fn to_vec(&self) -> Vec<StrT> {
        self.0.clone()
    }

    pub fn into_vec(self) -> Vec<StrT> {
        self.0
    }
}

impl From<StrT> for ItemPath {
    fn from(seg: StrT) -> Self {
        Self(vec![seg])
    }
}

impl From<Vec<StrT>> for ItemPath {
    fn from(segs: Vec<StrT>) -> Self {
        Self(segs)
    }
}

impl Deref for ItemPath {
    type Target = [StrT];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Deserialize, Serialize)]
pub enum Signedness {
    Unsigned,
    Signed,
}

impl Display for Signedness {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let sign = match self {
            Self::Unsigned => 'u',
            Self::Signed => 'i',
        };

        f.write_char(sign)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Deserialize, Serialize)]
pub enum Sign {
    Positive,
    Negative,
}

impl Sign {
    pub fn is_negative(self) -> bool {
        self == Self::Negative
    }

    pub fn maybe_negate<T>(self, integer: T) -> T
    where
        T: Not<Output = T>,
    {
        if self.is_negative() {
            !integer
        } else {
            integer
        }
    }
}

impl Display for Sign {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "{}",
            match self {
                Self::Positive => "",
                Self::Negative => "-",
            },
        )
    }
}
