use alloc::borrow::Cow;
use core::{
    fmt,
    ops::{Deref, DerefMut},
};
use fxhash::FxBuildHasher;
use serde::{Deserialize, Serialize};

cfg_if::cfg_if! {
    if #[cfg(feature = "no-std")] {
        pub type HashMap<K, V> = hashbrown::HashMap<K, V, FxBuildHasher>;
        pub type HashSet<K> = hashbrown::HashSet<K, FxBuildHasher>;
    } else {
        pub type HashMap<K, V> = std::collections::HashMap<K, V, FxBuildHasher>;
        pub type HashSet<K> = std::collections::HashSet<K, FxBuildHasher>;
    }
}

pub type Hasher = FxBuildHasher;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Deserialize, Serialize)]
pub enum Either<L, R> {
    Left(L),
    Right(R),
}

impl<L, R> Either<L, R> {
    pub fn is_left(&self) -> bool {
        matches!(self, Self::Left(..))
    }

    pub fn is_right(&self) -> bool {
        matches!(self, Self::Right(..))
    }

    pub fn left(self) -> Option<L> {
        if let Self::Left(left) = self {
            Some(left)
        } else {
            None
        }
    }

    pub fn right(self) -> Option<R> {
        if let Self::Right(right) = self {
            Some(right)
        } else {
            None
        }
    }

    pub fn left_or(self, default: L) -> L {
        if let Self::Left(left) = self {
            left
        } else {
            default
        }
    }

    pub fn right_or(self, default: R) -> R {
        if let Self::Right(right) = self {
            right
        } else {
            default
        }
    }

    pub fn left_or_else<F>(self, default: F) -> L
    where
        F: FnOnce() -> L,
    {
        if let Self::Left(left) = self {
            left
        } else {
            default()
        }
    }

    pub fn right_or_else<F>(self, default: F) -> R
    where
        F: FnOnce() -> R,
    {
        if let Self::Right(right) = self {
            right
        } else {
            default()
        }
    }

    pub fn unwrap_left(self) -> L {
        if let Self::Left(left) = self {
            left
        } else {
            // TODO: track_caller
            panic!("'called `Either::unwrap_left()` on a `Right` value'");
        }
    }

    pub fn unwrap_right(self) -> R {
        if let Self::Right(right) = self {
            right
        } else {
            // TODO: track_caller
            panic!("'called `Either::unwrap_right()` on a `Left` value'");
        }
    }

    pub fn expect_left(self, message: impl AsRef<str>) -> L {
        if let Self::Left(left) = self {
            left
        } else {
            // TODO: track_caller
            panic!("'{}'", message.as_ref());
        }
    }

    pub fn expect_right(self, message: impl AsRef<str>) -> R {
        if let Self::Right(right) = self {
            right
        } else {
            // TODO: track_caller
            panic!("'{}'", message.as_ref());
        }
    }

    pub fn map_left<F, U>(self, map: F) -> Either<U, R>
    where
        F: FnOnce(L) -> U,
    {
        match self {
            Self::Left(left) => Either::Left(map(left)),
            Self::Right(right) => Either::Right(right),
        }
    }

    pub fn map_right<F, U>(self, map: F) -> Either<L, U>
    where
        F: FnOnce(R) -> U,
    {
        match self {
            Self::Left(left) => Either::Left(left),
            Self::Right(right) => Either::Right(map(right)),
        }
    }

    pub fn map_left_or<F, U>(self, default: U, map: F) -> U
    where
        F: FnOnce(L) -> U,
    {
        if let Self::Left(left) = self {
            map(left)
        } else {
            default
        }
    }

    pub fn map_right_or<F, U>(self, default: U, map: F) -> U
    where
        F: FnOnce(R) -> U,
    {
        if let Self::Right(right) = self {
            map(right)
        } else {
            default
        }
    }

    pub fn map_left_or_else<D, F, U>(self, default: D, map: F) -> U
    where
        F: FnOnce(L) -> U,
        D: FnOnce() -> U,
    {
        if let Self::Left(left) = self {
            map(left)
        } else {
            default()
        }
    }

    pub fn map_right_or_else<D, F, U>(self, default: D, map: F) -> U
    where
        F: FnOnce(R) -> U,
        D: FnOnce() -> U,
    {
        if let Self::Right(right) = self {
            map(right)
        } else {
            default()
        }
    }
}

#[derive(Debug)]
#[allow(missing_copy_implementations)]
pub struct Timer {
    #[cfg(not(feature = "no-std"))]
    name: Cow<'static, str>,

    #[cfg(not(feature = "no-std"))]
    start: std::time::Instant,

    #[cfg(not(feature = "no-std"))]
    finished: bool,
}

impl Timer {
    #[allow(unused_variables)]
    pub fn start(name: impl Into<Cow<'static, str>>) -> Self {
        cfg_if::cfg_if! {
            if #[cfg(feature = "no-std")] {
                Self { }
            } else {
                let name = name.into();
                crate::info!("Started {}", name);

                Self {
                    name,
                    start: std::time::Instant::now(),
                    finished: false,
                }
            }
        }
    }

    pub fn end(mut self) {
        self.end_inner();
    }

    fn end_inner(&mut self) {
        cfg_if::cfg_if! {
            if #[cfg(not(feature = "no-std"))] {
                if !self.finished {
                    let elapsed = self.start.elapsed();

                    crate::info!(
                        "Finished {} in {}sec, {}ms and {}μs",
                        self.name,
                        elapsed.as_secs(),
                        elapsed.subsec_micros() / 1000,
                        elapsed.subsec_micros() % 1000,
                    );

                    self.finished = true;
                }
            }
        }
    }
}

impl Drop for Timer {
    fn drop(&mut self) {
        self.end_inner();
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DbgWrap<T>(pub T);

impl<T> DbgWrap<T> {
    pub fn new(inner: T) -> Self {
        Self(inner)
    }

    pub fn into_inner(self) -> T {
        self.0
    }
}

impl<T> Deref for DbgWrap<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> DerefMut for DbgWrap<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<T> AsRef<T> for DbgWrap<T> {
    fn as_ref(&self) -> &T {
        &self.0
    }
}

impl<T> AsMut<T> for DbgWrap<T> {
    fn as_mut(&mut self) -> &mut T {
        &mut self.0
    }
}

impl<T> fmt::Debug for DbgWrap<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct(core::any::type_name::<T>()).finish()
    }
}
