use super::{Index, LoadedString, Register, Registers, StringPointer, Value};
use log::{error, trace, warn};
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
        output: StringPointer,
    },
    /// Adds ints strings together, storing the result in `left`
    AddInt {
        /// The left register, stores the added value
        left: Register,
        /// The right register, unaffected by operation
        right: Register,
        output: Register,
    },
    /// Subtracts two ints from each other, storing the result in `left`
    SubInt {
        /// The left register, stores the subtracted value
        left: Register,
        /// The right register, unaffected by operation
        right: Register,
        output: Register,
    },
    MultInt {
        left: Register,
        right: Register,
        output: Register,
    },
    DivInt {
        left: Register,
        right: Register,
        output: Register,
    },
    // BitShiftLeft {
    //     reg: Register,
    //     amount: u8,
    //     output: Register,
    // },
    // BitShiftRight {
    //     reg: Register,
    //     amount: u8,
    //     output: Register,
    // },
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
    JumpLessThan {
        index: i32,
        reg: Register,
        compare: Register,
    },
    JumpGreaterThan {
        index: i32,
        reg: Register,
        compare: Register,
    },
    FuncJump(Index),
    FuncCall(Index),
    Return,
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
            AddStr {
                left,
                right,
                output,
            } => {
                registers.add_strings(*left, *right, *output);
            }
            AddInt {
                left,
                right,
                output,
            } => {
                trace!("Adding {:?} and {:?}", left, right);

                let (left, right) = (*registers.get(*left), *registers.get(*right));
                if let (Value::Int(left), Value::Int(right)) = (left, right) {
                    (*registers.get_mut(*output)) = Value::Int(left + right);
                } else {
                    error!("Un-addable types: {:?} + {:?}", left, right);
                }
            }
            SubInt {
                left,
                right,
                output,
            } => {
                trace!("Subtracting {:?} and {:?}", left, right);

                let (left, right) = (*registers.get(*left), *registers.get(*right));
                if let (Value::Int(left), Value::Int(right)) = (left, right) {
                    (*registers.get_mut(*output)) = Value::Int(left - right);
                } else {
                    error!("Un-subtractable types: {:?} - {:?}", left, right);
                }
            }
            MultInt {
                left,
                right,
                output,
            } => {
                let (left, right) = (*registers.get(*left), *registers.get(*right));
                if let (Value::Int(left), Value::Int(right)) = (left, right) {
                    (*registers.get_mut(*output)) = Value::Int(left * right);
                } else {
                    error!("Un-multipliable types: {:?} * {:?}", left, right);
                }
            }
            DivInt {
                left,
                right,
                output,
            } => {
                let (left, right) = (*registers.get(*left), *registers.get(*right));
                if let (Value::Int(left), Value::Int(right)) = (left, right) {
                    (*registers.get_mut(*output)) = Value::Int(left / right);
                } else {
                    error!("Indivisible types: {:?} / {:?}", left, right);
                }
            }
            Print(reg) => {
                trace!("Printing {:?}", reg);

                match registers.get(*reg) {
                    Value::Str(ptr) => print!("{}", ptr.get(registers)),
                    val => print!("{}", val),
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
            JumpLessThan {
                index,
                reg,
                compare,
            } => {
                let reg_value = registers.get(*reg);
                let comp_value = registers.get(*compare);

                if let (Value::Int(int), Value::Int(compare)) = (reg_value, comp_value) {
                    if int < compare {
                        registers.environment.index.set(Index(
                            (registers.environment.index.0 as i32 + index - 1) as u32,
                        ));
                    }
                } else {
                    error!(
                        "Jump Less Than registers are not both integers: {:?} < {:?}",
                        reg_value, comp_value
                    );
                }
            }
            JumpGreaterThan {
                index,
                reg,
                compare,
            } => {
                let reg_value = registers.get(*reg);
                let comp_value = registers.get(*compare);

                if let (Value::Int(int), Value::Int(compare)) = (reg_value, comp_value) {
                    if int > compare {
                        registers.environment.index.set(Index(
                            (registers.environment.index.0 as i32 + index - 1) as u32,
                        ));
                    }
                } else {
                    error!(
                        "Jump Less Than registers are not both integers: {:?} < {:?}",
                        reg_value, comp_value
                    );
                }
            }
            FuncJump(index) => {
                registers.return_stack.push(registers.environment.index);
                registers.environment.index.set(*index - 2.into());
            }
            FuncCall(index) => {
                registers.snapshot();

                registers.functions[index]
            }
            Return => {
                if let Some(location) = registers.return_stack.pop() {
                    registers.environment.index.set(location);
                } else if registers.environment.in_function {

                } else {
                    registers.cleanup();
                    registers.environment.finished_execution = true;
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
