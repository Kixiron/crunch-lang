use super::{Index, Register, Registers, StringPointer, Value};
use log::{error, trace, warn};
use serde::{Deserialize, Serialize};

/// Instructions for the VM
#[derive(Debug, PartialEq, Eq, Clone, Deserialize, Serialize)]
pub enum Instruction {
    /// Loads an integer into a [`Register`]
    LoadInt(i32, Register),
    LoadStr(&'static str, StringPointer, Register),
    /// Loads a boolean into a [`Register`]
    LoadBool(bool, Register),
    LoadHandoff(Register, Register),
    TakeHandoff(Register, Register),
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
    FuncReturn,
    /// Ends execution of the program by setting [`Registers.environment.finished_execution`] to `true`
    Halt,
}

impl Instruction {
    /// The execution of each instruction
    #[inline]
    pub fn execute(&self, mut registers: &mut Registers) {
        use Instruction::*;

        match self {
            LoadInt(int, reg) => {
                trace!("Loading {:?} into {:?}", int, reg);

                registers.load(Value::Int(*int), *reg);
                *registers.environment_mut().index_mut() += Index(1);
            }
            LoadStr(string, ptr, reg) => {
                registers.load_str(std::borrow::Cow::Borrowed(*string), *ptr);
                registers.load(Value::Str(*ptr), *reg);

                *registers.environment_mut().index_mut() += Index(1);
            }
            LoadHandoff(output_reg, handoff_reg) => {
                let mut value = Value::None;
                std::mem::swap(registers.get_mut(*output_reg), &mut value);
                registers.load_handoff(*handoff_reg, value);

                *registers.environment_mut().index_mut() += Index(1);
            }
            TakeHandoff(handoff_reg, destination_reg) => {
                registers.registers_mut()[**destination_reg as usize] =
                    registers.handoff_registers()[**handoff_reg as usize].clone();

                *registers.environment_mut().index_mut() += Index(1);
            }
            LoadBool(boolean, reg) => {
                trace!("Loading {:?} into {:?}", boolean, reg);

                registers.load(Value::Bool(*boolean), *reg);

                *registers.environment_mut().index_mut() += Index(1);
            }
            Drop(reg) => {
                trace!("Dropping {:?}", reg);
                registers.clear(*reg);

                *registers.environment_mut().index_mut() += Index(1);
            }
            DropStr(ptr) => registers.get_str_mut(*ptr).to_mut().clear(),
            AddStr {
                left,
                right,
                output,
            } => {
                registers.add_strings(*left, *right, *output);

                *registers.environment_mut().index_mut() += Index(1);
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

                *registers.environment_mut().index_mut() += Index(1);
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
                *registers.environment_mut().index_mut() += Index(1);
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

                *registers.environment_mut().index_mut() += Index(1);
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

                *registers.environment_mut().index_mut() += Index(1);
            }
            Print(reg) => {
                trace!("Printing {:?}", reg);

                match registers.get(*reg) {
                    Value::Str(ptr) => print!("{}", registers.get_str(*ptr)),
                    val => print!("{}", val),
                }

                *registers.environment_mut().index_mut() += Index(1);
            }
            Jump(index) => {
                trace!("Jumping by offset {}", index);

                let index = Index((*registers.environment().index() as i32 + index) as u32);
                *registers.environment_mut().index_mut() += index;
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

                    let index = Index((*registers.environment().index() as i32 + index) as u32);
                    *registers.environment_mut().index_mut() += index;
                } else if reg_value == &Value::Bool(false) {
                    trace!(
                        "CondJump by offset {}, reading {:?} (Value: {})",
                        index,
                        reg,
                        reg_value
                    );

                    *registers.environment_mut().index_mut() += Index(1);
                } else {
                    warn!("The requested register for `CondJump` does not contain a boolean [Register: {:?} Value: {:?}]", reg, reg_value);

                    trace!(
                        "CondJump by offset {}, reading {:?} (Value: {})",
                        index,
                        reg,
                        reg_value
                    );

                    *registers.environment_mut().index_mut() += Index(1);
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
                        let index = Index((*registers.environment().index() as i32 + index) as u32);
                        *registers.environment_mut().index_mut() += index;
                    }
                } else {
                    error!(
                        "Jump Less Than registers are not both integers: {:?} < {:?}",
                        reg_value, comp_value
                    );

                    *registers.environment_mut().index_mut() += Index(1);
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
                        let index = Index((*registers.environment().index() as i32 + index) as u32);
                        *registers.environment_mut().index_mut() += index;
                    }
                } else {
                    error!(
                        "Jump Less Than registers are not both integers: {:?} < {:?}",
                        reg_value, comp_value
                    );

                    *registers.environment_mut().index_mut() += Index(1);
                }
            }
            FuncJump(index) => {
                {
                    let index = registers.environment().index();
                    registers.return_stack_mut().push(index);
                }

                *registers.environment_mut().index_mut() = *index;
            }
            FuncCall(index) => {
                registers.snapshot();
                *registers.environment_mut().index_mut() = Index(0);

                while !registers.environment().returning() {
                    registers.functions()[**index as usize]
                        [*registers.environment().index() as usize]
                        .clone()
                        .execute(&mut registers);
                }

                *registers.environment_mut().index_mut() += Index(1);
            }
            Return => {
                if let Some(location) = registers.return_stack_mut().pop() {
                    *registers.environment_mut().index_mut() = location;
                } else {
                    registers.cleanup();
                    *registers.environment_mut().finished_execution_mut() = true;
                }

                *registers.environment_mut().index_mut() += Index(1);
            }
            FuncReturn => {
                *registers.environment_mut().returning_mut() = true;

                if let Some(context) = registers.snapshots_mut().pop() {
                    *registers.environment_mut().index_mut() = context.0;
                    *registers.registers_mut() = context.2;

                    if let Some(index) = context.1 {
                        *registers.environment_mut().returning_mut() = false;

                        while !registers.environment().returning() {
                            registers.functions()[*index as usize]
                                [*registers.environment().index() as usize]
                                .clone()
                                .execute(&mut registers);
                        }
                    } else {
                        trace!("Returning to main");
                    }
                } else {
                    error!("Returned from non-existant function");
                }
            }
            Halt => {
                registers.cleanup();
                *registers.environment_mut().finished_execution_mut() = true;
            }
        }
    }
}
