use core::hash::Hash;
use lasso::{Key, Spur};
use serde::{Deserialize, Serialize};

/// A token for an interned string
#[derive(Copy, Clone, PartialEq, Eq, Ord, PartialOrd, Hash, Deserialize, Serialize)]
#[repr(transparent)]
pub struct StrT(Spur);

impl StrT {
    #[inline]
    pub fn new(key: usize) -> Self {
        Self(Spur::try_from_usize(key).unwrap())
    }

    #[inline]
    pub fn get(self) -> Spur {
        self.0
    }
}

impl From<Spur> for StrT {
    #[inline]
    fn from(spur: Spur) -> Self {
        Self(spur)
    }
}

impl std::fmt::Debug for StrT {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Safety: We're just doing a debug print, the unsafety doesn't apply
        write!(f, "{}", unsafe { self.get().into_usize() })
    }
}
