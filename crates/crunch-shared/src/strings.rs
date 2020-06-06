use core::{fmt, hash::Hash};
use lasso::{Key, Spur};
use serde::{Deserialize, Serialize};

pub use interner::StrInterner;

#[cfg(feature = "concurrent")]
mod interner {
    use super::StrT;
    use alloc::sync::Arc;
    use lasso::{Spur, ThreadedRodeo};

    #[derive(Debug, Clone)]
    #[repr(transparent)]
    pub struct StrInterner(Arc<ThreadedRodeo<str, Spur>>);

    impl StrInterner {
        #[inline]
        pub fn new() -> Self {
            Self(Arc::new(ThreadedRodeo::with_capacity(2048)))
        }

        #[inline]
        pub fn resolve<'a>(&'a self, sym: StrT) -> &'a str {
            self.0.resolve(&sym.get())
        }

        #[inline]
        pub fn intern(&self, string: impl AsRef<str>) -> StrT {
            StrT::from(self.0.get_or_intern(string.as_ref()))
        }
    }

    impl Default for StrInterner {
        #[inline]
        fn default() -> Self {
            Self::new()
        }
    }
}

#[cfg(not(feature = "concurrent"))]
mod interner {
    use alloc::rc::Rc;
    use lasso::{Rodeo, Spur};

    #[derive(Debug, Clone)]
    #[repr(transparent)]
    pub struct StrInterner(Rc<Rodeo<str, Spur>>);

    impl StrInterner {
        #[inline]
        pub fn new() -> Self {
            Self(Rc::new(Rodeo::with_capacity(2048)))
        }

        #[inline]
        pub fn resolve<'a>(&'a self, sym: StrT) -> &'a str {
            self.0.resolve(&sym.get())
        }

        #[inline]
        pub fn intern(&self, string: impl AsRef<str>) -> StrT {
            StrT::from(
                Rc::get_mut(&mut self.0)
                    .expect("Multiple mutable borrows of an interner")
                    .get_or_intern(string.as_ref()),
            )
        }
    }

    impl Default for StrInterner {
        #[inline]
        fn default() -> Self {
            Self::new()
        }
    }
}

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

impl fmt::Debug for StrT {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Safety: We're just doing a debug print, the unsafety doesn't apply
        write!(f, "{}", unsafe { self.get().into_usize() })
    }
}
