use crate::{Value, NUMBER_REGISTERS};

use alloc::{format, vec::Vec};
use core::fmt;

/// A function's stack frame, holds enough information to continue execution where it
/// last left off
#[derive(Clone)]
pub struct ReturnFrame {
    /// The frame's registers
    pub registers: [Value; NUMBER_REGISTERS + 2],
    /// The current [`Instruction`] index of the frame
    ///
    /// [`Instruction`]: crate::Instruction
    pub index: u32,
    /// The index of the function to return to
    pub function_index: u32,
    pub yield_point: Option<u32>,
}

impl fmt::Debug for ReturnFrame {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.debug_struct("ReturnFrame")
            .field(
                "registers",
                &format!(
                    "[{}]",
                    self.registers
                        .iter()
                        .map(|r| format!("{:?}", r))
                        .collect::<Vec<_>>()
                        .join(", ")
                ),
            )
            .field("Index", &self.index)
            .field("function_index", &self.function_index)
            .field("yield_point", &self.yield_point)
            .finish()
    }
}
