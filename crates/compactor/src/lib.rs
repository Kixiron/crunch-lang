#![cfg_attr(not(feature = "std"), no_std)]

extern crate alloc;

extern "Rust" {
    static mut CRUNCH_ALLOCATOR: ballast::BumpHeap;
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
