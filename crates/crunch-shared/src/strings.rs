use core::{fmt, hash::Hash};
use lasso::{Key, Spur};
use serde::{Deserialize, Serialize};

pub use interner::StrInterner;

#[cfg(all(feature = "concurrent", not(feature = "no-std")))]
mod interner {
    use crate::utils::Hasher;
    use alloc::sync::Arc;
    use lasso::ThreadedRodeo;
    use lasso::{Capacity, Key, Spur};

    #[derive(Debug, Clone)]
    #[repr(transparent)]
    pub struct StrInterner(Arc<ThreadedRodeo<Spur, Hasher>>);

    impl StrInterner {
        #[inline]
        pub fn new() -> Self {
            Self(Arc::new(ThreadedRodeo::with_capacity_and_hasher(
                Capacity::for_strings(1000),
                Hasher::default(),
            )))
        }

        #[inline]
        pub fn resolve<'a>(&'a self, sym: StrT) -> impl AsRef<str> + 'a {
            self.0.resolve(&sym.get())
        }

        #[inline]
        pub fn intern(&self, string: impl AsRef<str>) -> StrT {
            StrT::from(self.0.get_or_intern(string.as_ref()))
        }

        #[inline]
        pub fn intern_static(&self, string: &'static str) -> StrT {
            StrT::from(self.0.get_or_intern_static(string))
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
    use super::StrT;
    use crate::utils::Hasher;
    use alloc::rc::Rc;
    use core::{
        cell::{Ref, RefCell},
        ops::Deref,
    };
    use lasso::Rodeo;
    use lasso::{Capacity, Spur};

    #[derive(Debug, Clone)]
    #[repr(transparent)]
    pub struct StrInterner(Rc<RefCell<Rodeo<Spur, Hasher>>>);

    impl StrInterner {
        #[inline]
        pub fn new() -> Self {
            crate::debug!("Creating a new string interner");

            let rodeo =
                Rodeo::with_capacity_and_hasher(Capacity::for_strings(1000), Hasher::default());

            Self(Rc::new(RefCell::new(rodeo)))
        }

        #[inline]
        pub fn resolve<'a>(&'a self, sym: StrT) -> impl AsRef<str> + 'a {
            crate::debug!(
                "borrowing strings immutably (strong count is {}, weak count is {})",
                Rc::strong_count(&self.0),
                Rc::weak_count(&self.0),
            );

            #[repr(transparent)]
            struct RefWrap<T>(T);

            impl<'a> AsRef<str> for RefWrap<Ref<'a, str>> {
                fn as_ref(&self) -> &str {
                    self.0.deref()
                }
            }

            RefWrap(Ref::map(self.0.borrow(), |i| i.resolve(&sym.get())))
        }

        #[inline]
        #[track_caller]
        pub fn intern(&self, string: impl AsRef<str>) -> StrT {
            crate::debug!(
                "borrowed strings mutably for interning (strong count is {}, weak count is {}) @ {}",
                Rc::strong_count(&self.0),
                Rc::weak_count(&self.0),
                core::panic::Location::caller(),
            );

            let mut borrow = self.0.borrow_mut();
            StrT::from(borrow.get_or_intern(string.as_ref()))
        }

        #[inline]
        #[track_caller]
        pub fn intern_static(&self, string: &'static str) -> StrT {
            crate::debug!(
                "borrowed strings mutably for static interning (strong count is {}, weak count is {}) @ {}",
                Rc::strong_count(&self.0),
                Rc::weak_count(&self.0),
                core::panic::Location::caller(),
            );

            StrT::from(self.0.borrow_mut().get_or_intern_static(string))
        }
    }

    impl Default for StrInterner {
        #[inline]
        fn default() -> Self {
            Self::new()
        }
    }
}

// This is to make rust-analyzer shut up, concurrent & no-std is not supported
#[cfg(all(feature = "concurrent", feature = "no-std"))]
mod interner {
    use super::StrT;

    #[derive(Debug, Clone)]
    #[allow(missing_copy_implementations)]
    pub struct StrInterner;

    impl StrInterner {
        #[inline]
        pub fn new() -> Self {
            unreachable!()
        }

        #[inline]
        #[allow(unreachable_code)]
        pub fn resolve<'a>(&'a self, _sym: StrT) -> impl AsRef<str> + 'a {
            unreachable!();
            "" // This looks weird and it is, but for some reason `!` wasn't coercing to an `impl AsRef<str>`, so here we are
        }

        #[inline]
        pub fn intern(&self, _string: impl AsRef<str>) -> StrT {
            unreachable!()
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
