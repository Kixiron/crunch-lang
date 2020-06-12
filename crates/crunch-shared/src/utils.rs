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

#[cfg(feature = "no-std")]
#[macro_export]
macro_rules! start_timer {{
    ($thing:expr) => {
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
