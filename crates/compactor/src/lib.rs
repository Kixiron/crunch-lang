#![cfg_attr(not(feature = "std"), no_std)]

extern crate alloc;

#[cfg(feature = "runtime")]
thread_local! {
    static CRUNCH_ALLOCATOR: core::cell::UnsafeCell<ballast::BumpHeap> =
        core::cell::UnsafeCell::new(ballast::BumpHeap::new(ballast::BumpOptions::default()));
}

mod compactor;
mod compactor_options;
mod instruction;
mod return_frame;
mod value;
mod write;

pub use compactor::Compactor;
pub use compactor_options::CompactorOptions;
pub use instruction::Instruction;
pub use value::Value;
pub use write::CrunchWrite;

/// The number of available registers for the Compactor
pub const NUMBER_REGISTERS: usize = 32;
