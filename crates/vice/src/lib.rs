#![feature(track_caller)]

// TODO: Candidate for no_std

extern crate alloc;

mod code_builder;
mod data_location;
mod intrinsics;
mod vice;

pub use vice::{Vice, ViceOptions};
