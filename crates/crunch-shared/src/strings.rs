use core::{fmt, hash::Hash};
use lasso::{Key, Spur};
use serde::{Deserialize, Serialize};

pub use interner::StrInterner;

// TODO: Encapsulate interners into a trait and use a `Box<dyn Interner>`

#[cfg(all(feature = "concurrent", not(feature = "no-std")))]
mod interner {
    use crate::utils::Hasher;
    use alloc::sync::Arc;
    use core::fmt::{Debug, Display};
    use lasso::{Capacity, Key, Spur, ThreadedRodeo};

    #[derive(Debug, Clone)]
    #[repr(transparent)]
    pub struct StrInterner(Arc<ThreadedRodeo<Spur, Hasher>>);

    impl StrInterner {
        pub fn new() -> Self {
            crate::trace!(target: "string_interning", "created a string interner");

            Self(Arc::new(ThreadedRodeo::with_capacity_and_hasher(
                Capacity::for_strings(1000),
                Hasher::default(),
            )))
        }

        pub fn resolve<'a>(&'a self, sym: StrT) -> impl AsRef<str> + Display + Debug + 'a {
            crate::trace!(target: "string_interning", "resolved key: {:?}", sym);

            self.0.resolve(&sym.get())
        }

        pub fn intern(&self, string: impl AsRef<str>) -> StrT {
            crate::trace!(target: "string_interning", "interned string: {:?}", string.as_ref());

            StrT::from(self.0.get_or_intern(string.as_ref()))
        }

        pub fn intern_static(&self, string: &'static str) -> StrT {
            crate::trace!(target: "string_interning", "interned static string: {:?}", string);

            StrT::from(self.0.get_or_intern_static(string))
        }
    }

    impl Default for StrInterner {
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
        cell::RefCell,
        fmt::{Debug, Display},
    };
    use lasso::{Capacity, Rodeo, Spur};

    #[derive(Debug)]
    #[repr(transparent)]
    pub struct StrInterner(Rc<RefCell<Rodeo<Spur, Hasher>>>);

    impl StrInterner {
        pub fn new() -> Self {
            crate::trace!(target: "string_interning", "created a string interner");

            let rodeo =
                Rodeo::with_capacity_and_hasher(Capacity::for_strings(1000), Hasher::default());

            Self(Rc::new(RefCell::new(rodeo)))
        }

        pub fn resolve<'a>(&'a self, sym: StrT) -> impl AsRef<str> + Display + Debug + 'a {
            crate::trace!(target: "string_interning", "resolved key: {:?}", sym);

            let borrow = self.0.borrow();
            let string: &str = borrow.resolve(&sym.get());

            // Safety: The string's actual location in memory will never change, so tying the lifetime to
            //         the interner is safe. Even if the interner has strings appended to it the only things
            //         capable of moving in memory are the tables that store pointers *to* the strings, which
            //         are still inside immovable buckets. This allows us to ignore the `RefCell` song and dance
            //         since it's not needed after we've gotten the pointer we need.
            unsafe { core::mem::transmute::<&str, &'a str>(string) }
        }

        pub fn intern(&self, string: impl AsRef<str>) -> StrT {
            crate::trace!(target: "string_interning", "interned string: {:?}", string.as_ref());

            let mut borrow = self.0.borrow_mut();
            StrT::from(borrow.get_or_intern(string.as_ref()))
        }

        pub fn intern_static(&self, string: &'static str) -> StrT {
            crate::trace!(target: "string_interning", "interned static string: {:?}", string);

            let mut borrow = self.0.borrow_mut();
            StrT::from(borrow.get_or_intern_static(string.as_ref()))
        }
    }

    impl Default for StrInterner {
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
        pub fn new() -> Self {
            unreachable!()
        }

        #[allow(unreachable_code)]
        pub fn resolve<'a>(&'a self, _sym: StrT) -> impl AsRef<str> + 'a {
            unreachable!();
            "" // This looks weird and it is, but for some reason `!` wasn't coercing to an `impl AsRef<str>`, so here we are
        }

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
    pub fn new(key: usize) -> Self {
        Self(Spur::try_from_usize(key).unwrap())
    }

    pub fn get(self) -> Spur {
        self.0
    }

    #[doc(hidden)]
    pub fn as_u32(self) -> u32 {
        self.get().into_usize() as u32
    }
}

impl From<Spur> for StrT {
    fn from(spur: Spur) -> Self {
        Self(spur)
    }
}

impl fmt::Debug for StrT {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.get().into_usize())
    }
}
