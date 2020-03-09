#![no_std]

extern crate alloc;

mod assembler;
mod bytecode;

pub use bytecode::{Decoder, Encoder};

use crunch_error::runtime_prelude::RuntimeResult;

// TODO: A wrapper to represent valid bytecode
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Bytecode<'a>(&'a [u8]);

impl<'a> Bytecode<'a> {
    #[inline]
    pub fn validate(bytes: &'a [u8]) -> RuntimeResult<Self> {
        // TODO: Write this

        Ok(Self(bytes))
    }
}

impl<'a> core::ops::Deref for Bytecode<'a> {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        self.0
    }
}
