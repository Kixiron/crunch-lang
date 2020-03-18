#![feature(track_caller)]

// TODO: Candidate for no_std

extern crate alloc;

mod code_builder;
mod data_location;
mod intrinsics;
mod vice;
mod vir;

pub use self::vice::{Vice, ViceOptions};
