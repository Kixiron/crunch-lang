use serde::{Deserialize, Serialize};

#[cfg(feature = "no-std")]
pub use hashbrown::{HashMap, HashSet};
#[cfg(not(feature = "no-std"))]
pub use std::collections::{HashMap, HashSet};

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

#[cfg(not(feature = "logging"))]
mod log {
    #[macro_export]
    macro_rules! error {
        (target: $target:expr, $($arg:tt)+) => {};
        ($($arg:tt)+) => {};
    }

    #[macro_export]
    macro_rules! warn {
        (target: $target:expr, $($arg:tt)+) => {};
        ($($arg:tt)+) => {};
    }

    #[macro_export]
    macro_rules! info {
        (target: $target:expr, $($arg:tt)+) => {};
        ($($arg:tt)+) => {};
    }

    #[macro_export]
    macro_rules! trace {
        (target: $target:expr, $($arg:tt)+) => {};
        ($($arg:tt)+) => {};
    }

    #[macro_export]
    macro_rules! debug {
        (target: $target:expr, $($arg:tt)+) => {};
        ($($arg:tt)+) => {};
    }
}

#[cfg(feature = "no-std")]
#[macro_export]
macro_rules! start_timer {
    ($thing:expr) => {{
        $crate::info!("Started {}", $thing);
    }};
}

#[cfg(not(feature = "no-std"))]
#[macro_export]
macro_rules! start_timer {
    ($thing:expr) => {{
        $crate::info!("Started {}", $thing);
        ::std::time::Instant::now()
    }};
}

#[cfg(feature = "no-std")]
#[macro_export]
macro_rules! end_timer {
    ($thing:expr, $time:expr) => {{
        $crate::info!("Finished {}", $thing);
        let _ = $time;
    }};
}

#[cfg(not(feature = "no-std"))]
#[macro_export]
macro_rules! end_timer {
    ($thing:expr, $time:expr) => {{
        let elapsed = $time.elapsed();
        $crate::info!(
            "Finished {} in {}sec, {}ms and {}μs",
            $thing,
            elapsed.as_secs(),
            elapsed.subsec_micros() / 1000,
            elapsed.subsec_micros() % 1000,
        );
        let _ = $time;
    }};
}
