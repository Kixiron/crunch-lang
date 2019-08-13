use super::{Index, LoadedString, Register, Registers, StringPointer, Value};
use log::{trace, warn};
use serde::{Deserialize, Serialize};

/// Instructions for the VM
#[derive(Debug, PartialEq, Eq, Clone, Deserialize, Serialize)]
pub enum Instruction {
    /// Loads an integer into a [`Register`]
    LoadInt(i32, Register),
    /// Loads a string as specified by [`LoadedString`]
    LoadStr(Box<LoadedString>),
    /// Loads a boolean into a [`Register`]
    LoadBool(bool, Register),
    /// Drops the contents of a [`Register`]
    Drop(Register),
    /// Drops a string by [`StringPointer`]
    DropStr(StringPointer),
    /// Adds two strings together, storing the result in `left`
    AddStr {
        /// The left register, stores the added string
        left: StringPointer,
        /// The right register, unaffected by operation
        right: StringPointer,
    },
    /// Adds ints strings together, storing the result in `left`
    AddInt {
        /// The left register, stores the added value
        left: Register,
        /// The right register, unaffected by operation
        right: Register,
    },
    /// Subtracts two ints from each other, storing the result in `left`
    SubInt {
        /// The left register, stores the subtracted value
        left: Register,
        /// The right register, unaffected by operation
        right: Register,
    },
    /// Prints the contents of a [`Register`] stdout
    Print(Register),
    /// Jumps to the nth [`Instruction`] where n is `self.0`
    Jump(i32),
    /// Jumps to the nth [`Instruction`] where n is the `index`
    CondJump {
        /// The nth [`Instruction`] to jump to
        index: i32,
        /// The register to read for a bool. If the bool if true, the jump is preformed.
        /// If the stored value is not a `Value::Bool`, then the jump will never be preformed
        reg: Register,
    },
    /// Ends execution of the program by setting [`Registers.environment.finished_execution`] to `true`
    Halt,
}

impl Instruction {
    #[inline]
    pub fn execute(&self, registers: &mut Registers) {
        use Instruction::*;

        match self {
            LoadInt(int, reg) => {
                trace!("Loading {:?} into {:?}", int, reg);

                registers.load(Value::Int(*int), *reg);
            }
            LoadStr(load) => {
                registers.load_str(load.string.clone(), load.str_reg);
                registers.load(Value::Str(load.str_reg), load.reg);
            }
            LoadBool(boolean, reg) => {
                trace!("Loading {:?} into {:?}", boolean, reg);

                registers.load(Value::Bool(*boolean), *reg);
            }
            Drop(reg) => {
                trace!("Dropping {:?}", reg);
                registers.clear(*reg);
            }
            DropStr(ptr) => ptr.get_mut(registers).clear(),
            AddStr { left, right } => {
                registers.add_strings(*left, *right);
            }
            AddInt { left, right } => {
                trace!("Adding {:?} and {:?}", left, right);

                let int = *registers.get(*right);
                (*registers.get_mut(*left)) += int;
            }
            SubInt { left, right } => {
                trace!("Subtracting {:?} and {:?}", left, right);

                let int = *registers.get(*right);
                (*registers.get_mut(*left)) -= int;
            }
            Print(reg) => {
                trace!("Printing {:?}", reg);

                match registers.get(*reg) {
                    Value::Str(ptr) => println!("{}", ptr.get(registers)),
                    val => println!("{}", val),
                }
            }
            Jump(index) => {
                trace!("Jumping by offset {}", index);

                registers.environment.index.set(Index(
                    (registers.environment.index.0 as i32 + index - 1) as u32,
                ));
            }
            CondJump { index, reg } => {
                let reg_value = registers.get(*reg);
                if reg_value == &Value::Bool(true) {
                    trace!(
                        "CondJump by offset {}, reading {:?} (Value: {})",
                        index,
                        reg,
                        reg_value
                    );
                    registers.environment.index.set(Index(
                        (registers.environment.index.0 as i32 + index - 1) as u32,
                    ));
                } else if reg_value == &Value::Bool(false) {
                    trace!(
                        "CondJump by offset {}, reading {:?} (Value: {})",
                        index,
                        reg,
                        reg_value
                    );
                } else {
                    warn!("The requested register for `CondJump` does not contain a boolean [Register: {:?} Value: {:?}]", reg, reg_value);

                    trace!(
                        "CondJump by offset {}, reading {:?} (Value: {})",
                        index,
                        reg,
                        reg_value
                    );
                }
            }
            Halt => {
                registers.cleanup();
                registers.environment.finished_execution = true;
            }
        }

        registers.environment.index.add(Index(1));
    }
}
