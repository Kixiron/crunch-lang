#![cfg_attr(not(feature = "std"), no_std)]

extern crate alloc;

mod compactor;
mod compactor_options;
mod instruction;
mod return_frame;
mod value;
mod write;

pub use self::compactor::Compactor;
pub use compactor_options::CompactorOptions;
pub use instruction::Instruction;
pub use value::Value;
pub use write::CrunchWrite;

/// The number of available registers for the Compactor
pub const NUMBER_REGISTERS: usize = 32;
