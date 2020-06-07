pub mod ast;
pub mod hir;

use core::{
    fmt::{Debug, Display, Formatter, Result},
    ops::{Deref, DerefMut},
};
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
    #[inline]
    pub fn new(val: T) -> Self {
        Self(Box::new(val))
    }
}

impl<T> AsRef<T> for Ref<T> {
    #[inline]
    fn as_ref(&self) -> &T {
        &*self.0
    }
}

impl<T> Deref for Ref<T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}

impl<T> DerefMut for Ref<T> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut *self.0
    }
}

impl<T: Debug> Debug for Ref<T> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        Debug::fmt(&*self.0, f)
    }
}

impl<T: Display> Display for Ref<T> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        Display::fmt(&*self.0, f)
    }
}
