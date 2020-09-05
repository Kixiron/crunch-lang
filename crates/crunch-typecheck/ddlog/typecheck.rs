use core::fmt::Debug;

pub fn dbg<T: Debug>(thing: T) -> T {
    dbg!(thing)
}
