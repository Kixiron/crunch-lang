use derive_more::{
    Add, AddAssign, Constructor, Display, From, Into, Mul, MulAssign, Sub, SubAssign,
};
use serde::{Deserialize, Serialize};
use shrinkwraprs::Shrinkwrap;

/// The instruction index for the VM
#[derive(
    Debug,
    Copy,
    Clone,
    PartialEq,
    Eq,
    Add,
    Mul,
    AddAssign,
    MulAssign,
    Constructor,
    Display,
    From,
    Into,
    Deserialize,
    Serialize,
    Shrinkwrap,
    SubAssign,
    Sub,
)]
#[display(fmt = "{}", "_0")]
#[repr(transparent)]
pub struct Index(pub u32);

#[derive(
    Copy,
    Clone,
    PartialEq,
    Eq,
    Add,
    Mul,
    AddAssign,
    MulAssign,
    Constructor,
    Display,
    From,
    Into,
    Deserialize,
    Serialize,
    Shrinkwrap,
    SubAssign,
    Sub,
)]
#[display(fmt = "Rx{}", "_0")]
#[repr(transparent)]
pub struct Register(pub u8);

impl std::fmt::Debug for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Rx{:03}", self.0)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, From, Into, Deserialize, Serialize, Shrinkwrap)]
pub struct Bytecode<'a>(&'a [u8]);

impl<'a> Bytecode<'a> {
    #[inline]
    pub fn validate(bytes: &'a [u8]) -> Result<Self, &'static str> {
        use crate::bytecode::{INSTRUCTION_BYTES, INSTRUCTION_LENGTH};
        use std::convert::TryInto;

        let mut index = 0;

        // Validate Values
        {
            // TODO: Validate once Value encoding is stable
        }

        // Validate Functions
        {
            let num_func = u32::from_be_bytes(match bytes[index..index + 4].try_into() {
                Ok(i) => i,
                Err(_) => return Err("Invalid Number of Functions"),
            });
            index += 4;

            for _ in 0..num_func as usize {
                let num_instructions =
                    u32::from_be_bytes(match bytes[index..index + 4].try_into() {
                        Ok(i) => i,
                        Err(_) => return Err("Invalid Number of Instructions"),
                    });
                index += 4;

                for instruction in bytes
                    [index..index + (num_instructions as usize * INSTRUCTION_LENGTH)]
                    .chunks(INSTRUCTION_LENGTH)
                {
                    if !INSTRUCTION_BYTES.iter().any(|byte| *byte == instruction[0]) {
                        return Err("Invalid Instruction Header");
                    } else if instruction.len() != INSTRUCTION_LENGTH {
                        return Err("Instruction Too Short");
                    }
                }
                index += num_instructions as usize * INSTRUCTION_LENGTH;
            }
        }

        // Validate Main
        {
            for chunk in bytes[index..].chunks(INSTRUCTION_LENGTH) {
                if !INSTRUCTION_BYTES.iter().any(|byte| *byte == chunk[0]) {
                    return Err("Invalid Instruction Header");
                } else if chunk.len() != INSTRUCTION_LENGTH {
                    return Err("Instruction Too Short");
                }
            }
        }

        Ok(Self(bytes))
    }
}
