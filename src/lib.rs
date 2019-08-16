#![deny(missing_debug_implementations)]
// #![deny(missing_docs)]
#![warn(
    clippy::cargo,
    clippy::nursery,
    clippy::pedantic,
    clippy::perf,
    clippy::complexity,
    clippy::style,
    clippy::correctness,
    rust_2018_idioms
)]

const NUMBER_REGISTERS: usize = 10;
const NUMBER_STRINGS: usize = 10;

#[cfg(feature = "bytecode")]
mod bytecode;
mod instruction;
mod newtypes;
#[cfg(feature = "parser")]
mod parser;
mod registers;
mod value;

#[cfg(feature = "bytecode")]
pub use bytecode::*;
pub use instruction::*;
pub use newtypes::{Index, LoadedString, Register, StringPointer};
#[cfg(feature = "parser")]
pub use parser::*;
pub use registers::*;
pub use value::*;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Crunch {
    instructions: Vec<Instruction>,
    registers: Registers,
}

impl Crunch {
    #[inline]
    pub fn execute(&mut self) {
        while !self.registers.environment.finished_execution {
            log::trace!(
                "Executing Instruction {:?}",
                self.instructions[*self.registers.environment.index as usize]
            );
            self.instructions[*self.registers.environment.index as usize]
                .execute(&mut self.registers);
        }
    }

    #[cfg(feature = "bytecode")]
    #[inline]
    pub fn parse(bytes: &[u8]) -> Vec<Instruction> {
        decode_instructions(bytes)
    }
}

impl From<Vec<Instruction>> for Crunch {
    #[inline]
    fn from(instructions: Vec<Instruction>) -> Self {
        Self {
            instructions,
            registers: Registers::new(),
        }
    }
}
