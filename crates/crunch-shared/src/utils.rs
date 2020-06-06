#[cfg(feature = "no-std")]
pub use hashbrown::{HashMap, HashSet};
#[cfg(not(feature = "no-std"))]
pub use std::collections::{HashMap, HashSet};

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
