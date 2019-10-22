use super::{Index, Register, Value, Vm};

/// A type alias for results that could be a [`RuntimeError`]
pub type Result<T> = std::result::Result<T, RuntimeError>;

/// A Crunch Runtime Error
// TODO: Make this more detailed
#[derive(Debug, Clone)]
pub struct RuntimeError {
    /// The type of error
    pub ty: RuntimeErrorTy,
    /// The error message
    pub message: String,
}

impl RuntimeError {
    /// Prints the formatted error to stdout
    // TODO: Make this fancy, and more detailed
    pub fn emit(&self) {
        println!("[Crunch Runtime Error: {:?}] {}", self.ty, self.message);
    }
}

/// The type of [`RuntimeError`] that occurred
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RuntimeErrorTy {
    /// An error in the [`GC`]
    GcError,
    /// The user attempted to divide by zero
    DivideByZero,
    /// The two types are incompatible in the requested operation
    IncompatibleTypes,
    /// The program is missing a main function
    MissingMain,
    /// The requested variable is null
    NullVar,
    /// Thrown when an illegal instruction is executed
    IllegalInstruction,
}

/// Instructions for the VM
// TODO: Document all Instructions
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Instruction {
    /// Load a Value from the GC into a register
    Load(u32, Register),
    /// Cache a Value into the GC
    Cache(u32, Value),
    CompToReg(Register),
    OpToReg(Register),
    Save(u32, Register),
    DropReg(Register),
    Drop(u32),

    Add(Register, Register),
    Sub(Register, Register),
    Mult(Register, Register),
    Div(Register, Register),

    Print(Register),

    Jump(i32),
    JumpComp(i32),

    And(Register, Register),
    Or(Register, Register),
    Xor(Register, Register),
    Not(Register),

    Eq(Register, Register),
    NotEq(Register, Register),
    GreaterThan(Register, Register),
    LessThan(Register, Register),

    Collect,
    // TODO: Flesh this instruction out
    Return,
    Halt,

    Illegal,
}

impl Instruction {
    /// The execution of each instruction
    // TODO: Document this bad boy
    #[inline]
    pub fn execute(&self, mut vm: &mut Vm) -> Result<()> {
        match self {
            Instruction::Load(heap_loc, reg) => {
                trace!("Loading val at {} into {}", heap_loc, reg);

                let val: Value = match vm.gc.fetch(*heap_loc as usize) {
                    Some(val) => val,
                    None => {
                        return Err(RuntimeError {
                            ty: RuntimeErrorTy::GcError,
                            message: "Failed to retrieve error value from GC".to_string(),
                        });
                    }
                };

                vm.registers[**reg as usize] = val;

                vm.index += Index(1);
            }
            Instruction::Cache(heap_loc, val) => {
                trace!("Loading value onto heap at {}, Val: {:?}", heap_loc, val);

                if let Some((alloc_val, alloc_id)) = vm
                    .gc
                    .allocate_id(std::mem::size_of::<Value>(), *heap_loc as usize)
                {
                    unsafe {
                        if let Err(err) = vm.gc.write(alloc_id, val.to_owned(), Some(&alloc_val)) {
                            return Err(RuntimeError {
                                ty: RuntimeErrorTy::GcError,
                                message: err,
                            });
                        }
                    }

                    vm.gc.add_root(alloc_val);

                    vm.index += Index(1);

                    assert_eq!(*heap_loc as usize, *alloc_id);
                } else {
                    return Err(RuntimeError {
                        ty: RuntimeErrorTy::GcError,
                        message: "Failed to allocate value to GC".to_string(),
                    });
                }
            }
            Instruction::CompToReg(reg) => {
                trace!("Loading previous comparison into {}", reg);

                vm.registers[**reg as usize] = Value::Bool(vm.prev_comp);
                vm.index += Index(1);
            }
            Instruction::OpToReg(reg) => {
                trace!("Loading previous operation into {}", reg);

                std::mem::swap(&mut vm.registers[**reg as usize], &mut vm.prev_op);
                vm.index += Index(1);
            }
            Instruction::Save(heap_loc, reg) => {
                trace!("Saving register {} to {}", reg, heap_loc);

                unsafe {
                    if let Err(err) = vm.gc.write(
                        *heap_loc as usize,
                        vm.registers[**reg as usize].clone(),
                        None,
                    ) {
                        return Err(RuntimeError {
                            ty: RuntimeErrorTy::GcError,
                            message: err,
                        });
                    }
                }

                vm.index += Index(1);
            }
            Instruction::DropReg(reg) => {
                trace!("Clearing register {}", reg);

                vm.registers[**reg as usize] = Value::None;
                vm.index += Index(1);
            }
            Instruction::Drop(id) => {
                trace!("Dropping {:?}", id);
                if vm.gc.remove_root(*id as usize).is_err() {
                    return Err(RuntimeError {
                        ty: RuntimeErrorTy::GcError,
                        message: "Attempted to drop non-existant value".to_string(),
                    });
                }
                vm.index += Index(1);
            }

            Instruction::Add(left, right) => {
                vm.prev_op = ((*vm).get(*left).to_owned() + (*vm).get(*right).to_owned())?;
                vm.index += Index(1);
            }
            Instruction::Sub(left, right) => {
                vm.prev_op = ((*vm).get(*left).to_owned() - (*vm).get(*right).to_owned())?;
                vm.index += Index(1);
            }
            Instruction::Mult(left, right) => {
                vm.prev_op = ((*vm).get(*left).to_owned() * (*vm).get(*right).to_owned())?;
                vm.index += Index(1);
            }
            Instruction::Div(left, right) => {
                vm.prev_op = ((*vm).get(*left).to_owned() / (*vm).get(*right).to_owned())?;
                vm.index += Index(1);
            }

            Instruction::Print(reg) => {
                trace!("Printing reg {:?}", reg);

                println!("{}", vm.get(*reg));
                vm.index += Index(1);
            }

            Instruction::Jump(index) => {
                trace!("Jumping by offset {}", index);

                let index = Index((*vm.index as i32 + index + 1) as u32);
                vm.index = index;
            }
            Instruction::JumpComp(index) => {
                trace!(
                    "Comparison Jump: Prev Comp is {}, jump amount {}",
                    vm.prev_comp,
                    index
                );

                if vm.prev_comp {
                    let index = Index((*vm.index as i32 + index + 1) as u32);
                    vm.index = index;
                } else {
                    vm.index += Index(1);
                }
            }

            Instruction::And(left, right) => {
                vm.prev_op = ((*vm).get(*left).to_owned() & (*vm).get(*right).to_owned())?;
                vm.index += Index(1);
            }
            Instruction::Or(left, right) => {
                vm.prev_op = ((*vm).get(*left).to_owned() | (*vm).get(*right).to_owned())?;
                vm.index += Index(1);
            }
            Instruction::Xor(left, right) => {
                vm.prev_op = ((*vm).get(*left).to_owned() ^ (*vm).get(*right).to_owned())?;
                vm.index += Index(1);
            }
            Instruction::Not(reg) => {
                vm.prev_op = (!(*vm).get(*reg).to_owned())?;
                vm.index += Index(1);
            }

            Instruction::Eq(left, right) => {
                vm.prev_comp = (*vm).get(*left).to_owned() == (*vm).get(*right).to_owned();
                vm.index += Index(1);
            }
            Instruction::NotEq(left, right) => {
                vm.prev_comp = (*vm).get(*left).to_owned() != (*vm).get(*right).to_owned();
                vm.index += Index(1);
            }
            Instruction::GreaterThan(left, right) => {
                vm.prev_comp = (*vm).get(*left).to_owned() > (*vm).get(*right).to_owned();
                vm.index += Index(1);
            }
            Instruction::LessThan(left, right) => {
                vm.prev_comp = (*vm).get(*left).to_owned() < (*vm).get(*right).to_owned();
                vm.index += Index(1);
            }

            Instruction::Collect => {
                trace!("Forcing a GC collect");

                vm.gc.collect();
                vm.index += Index(1);
            }
            Instruction::Return => {
                vm.returning = true;

                if let Some(context) = vm.snapshots.pop() {
                    vm.index = context.0;
                    vm.registers = context.2;

                    if let Some(index) = context.1 {
                        vm.returning = false;

                        while !vm.returning {
                            vm.functions[*index as usize][*vm.index as usize]
                                .clone()
                                .execute(&mut vm)?;
                        }
                    } else {
                        trace!("Returning to main");
                    }
                } else {
                    if let Some(location) = vm.return_stack.pop() {
                        vm.index = location;
                    } else {
                        vm.cleanup();
                        vm.finished_execution = true;
                    }

                    vm.index += Index(1);
                }
            }
            Instruction::Halt => {
                vm.cleanup();
                vm.finished_execution = true;
            }

            Instruction::Illegal => {
                return Err(RuntimeError {
                    ty: RuntimeErrorTy::IllegalInstruction,
                    message: "Illegal Instruction".to_string(),
                })
            }
        }

        Ok(())
    }

    /// Turns the instruction into a string representation, for disassembly purposes
    pub fn to_str(&self) -> &'static str {
        match self {
            Self::Load(_, _) => "ld",
            Self::Cache(_, _) => "cache",
            Self::CompToReg(_) => "compr",
            Self::OpToReg(_) => "opr",
            Self::Save(_, _) => "save",
            Self::DropReg(_) => "dropr",
            Self::Drop(_) => "drop",

            Self::Add(_, _) => "add",
            Self::Sub(_, _) => "sub",
            Self::Mult(_, _) => "mul",
            Self::Div(_, _) => "div",

            Self::Print(_) => "print",

            Self::Jump(_) => "jmp",
            Self::JumpComp(_) => "jmpcmp",

            Self::And(_, _) => "and",
            Self::Or(_, _) => "or",
            Self::Xor(_, _) => "xor",
            Self::Not(_) => "not",

            Self::Eq(_, _) => "eq",
            Self::NotEq(_, _) => "neq",
            Self::GreaterThan(_, _) => "grt",
            Self::LessThan(_, _) => "let",

            Self::Collect => "coll",
            Self::Return => "ret",
            Self::Halt => "halt",

            Self::Illegal => "illegal",
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn variable_ops() {
        let mut vm = Vm::new(
            Vec::new(),
            &crate::OptionBuilder::new("./variable_tests").build(),
        );

        let cache = Instruction::Cache(0, Value::Int(10));
        cache.execute(&mut vm).unwrap();
        assert!(vm.gc.contains(0));
        assert_eq!(vm.gc.fetch(0), Some(Value::Int(10)));

        let load = Instruction::Load(0, 0.into());
        load.execute(&mut vm).unwrap();
        assert_eq!(vm.registers[0], Value::Int(10));

        let comp_to_reg = Instruction::CompToReg(1.into());
        vm.prev_comp = true;
        comp_to_reg.execute(&mut vm).unwrap();
        assert_eq!(vm.registers[1], Value::Bool(true));
        vm.prev_comp = false;
        comp_to_reg.execute(&mut vm).unwrap();
        assert_eq!(vm.registers[1], Value::Bool(false));

        let op_to_reg = Instruction::OpToReg(2.into());
        vm.prev_op = Value::Int(10);
        op_to_reg.execute(&mut vm).unwrap();
        assert_eq!(vm.registers[2], Value::Int(10));
        vm.prev_op = Value::Int(20);
        op_to_reg.execute(&mut vm).unwrap();
        assert_eq!(vm.registers[2], Value::Int(20));

        let save = Instruction::Save(0, 2.into());
        save.execute(&mut vm).unwrap();
        assert!(vm.gc.contains(0));
        assert_eq!(vm.gc.fetch(0), Some(Value::Int(20)));

        let drop_reg = Instruction::DropReg(1.into());
        drop_reg.execute(&mut vm).unwrap();
        assert_eq!(vm.registers[1], Value::None);

        let drop = Instruction::Drop(0);
        assert!(vm.gc.contains(0));
        assert_eq!(vm.gc.fetch(0), Some(Value::Int(20)));
        drop.execute(&mut vm).unwrap();
        vm.gc.collect();
        assert!(!vm.gc.contains(0));
        assert_eq!(vm.gc.fetch(0), <Option<usize>>::None);
    }

    #[test]
    fn math_ops() {
        let mut vm = Vm::new(
            Vec::new(),
            &crate::OptionBuilder::new("./variable_tests").build(),
        );

        vm.registers[0] = Value::Int(10);
        vm.registers[1] = Value::Int(10);

        let add = Instruction::Add(0.into(), 1.into());
        add.execute(&mut vm).unwrap();
        assert_eq!(vm.prev_op, Value::Int(10 + 10));

        let sub = Instruction::Sub(0.into(), 1.into());
        sub.execute(&mut vm).unwrap();
        assert_eq!(vm.prev_op, Value::Int(10 - 10));

        let mult = Instruction::Mult(0.into(), 1.into());
        mult.execute(&mut vm).unwrap();
        assert_eq!(vm.prev_op, Value::Int(10 * 10));

        let div = Instruction::Div(0.into(), 1.into());
        div.execute(&mut vm).unwrap();
        assert_eq!(vm.prev_op, Value::Int(10 / 10));
    }

    #[test]
    fn print_op() {
        // TODO: Capture STDOUT to find out if this worked
    }

    #[test]
    fn jump_ops() {
        let mut vm = Vm::new(
            Vec::new(),
            &crate::OptionBuilder::new("./variable_tests").build(),
        );

        // Each executed instruction increments the index by one, so take that into account

        let jump = Instruction::Jump(10);
        jump.execute(&mut vm).unwrap();
        assert_eq!(vm.index, 11.into());

        let jump_comp = Instruction::JumpComp(10);
        jump_comp.execute(&mut vm).unwrap();
        assert_eq!(vm.index, 12.into());
        vm.prev_comp = true;
        jump_comp.execute(&mut vm).unwrap();
        assert_eq!(vm.index, 23.into());
    }

    #[test]
    fn bitwise_ops() {
        let mut vm = Vm::new(
            Vec::new(),
            &crate::OptionBuilder::new("./variable_tests").build(),
        );

        vm.registers[0] = Value::Int(10);
        vm.registers[1] = Value::Int(10);

        let and = Instruction::And(0.into(), 1.into());
        and.execute(&mut vm).unwrap();
        assert_eq!(vm.prev_op, Value::Int(10 & 10));

        let or = Instruction::Or(0.into(), 1.into());
        or.execute(&mut vm).unwrap();
        assert_eq!(vm.prev_op, Value::Int(10 | 10));

        let xor = Instruction::Xor(0.into(), 1.into());
        xor.execute(&mut vm).unwrap();
        assert_eq!(vm.prev_op, Value::Int(10 ^ 10));

        let not = Instruction::Not(0.into());
        not.execute(&mut vm).unwrap();
        assert_eq!(vm.prev_op, Value::Int(!10));
    }

    #[test]
    fn eq_ops() {
        let mut vm = Vm::new(
            Vec::new(),
            &crate::OptionBuilder::new("./variable_tests").build(),
        );

        vm.registers[0] = Value::Int(10);
        vm.registers[1] = Value::Int(10);

        let eq = Instruction::Eq(0.into(), 1.into());
        eq.execute(&mut vm).unwrap();
        assert_eq!(vm.prev_comp, true);

        vm.registers[0] = Value::Int(20);

        let not_eq = Instruction::NotEq(0.into(), 1.into());
        not_eq.execute(&mut vm).unwrap();
        assert_eq!(vm.prev_comp, true);

        let greater_than = Instruction::GreaterThan(0.into(), 1.into());
        greater_than.execute(&mut vm).unwrap();
        assert_eq!(vm.prev_comp, true);

        vm.registers[0] = Value::Int(0);

        let less_than = Instruction::LessThan(0.into(), 1.into());
        less_than.execute(&mut vm).unwrap();
        assert_eq!(vm.prev_comp, true);
    }

    #[test]
    fn misc_ops() {
        let mut vm = Vm::new(
            Vec::new(),
            &crate::OptionBuilder::new("./variable_tests").build(),
        );

        let collect = Instruction::Collect;

        let (discard, discard_id) = vm.gc.allocate(std::mem::size_of::<Value>()).unwrap();
        unsafe {
            vm.gc
                .write(discard_id, Value::Int(10), Some(&discard))
                .unwrap();
        }
        assert!(vm.gc.contains(discard_id));
        assert!(vm.gc.fetch(discard_id) == Some(Value::Int(10)));
        collect.execute(&mut vm).unwrap();
        assert!(!vm.gc.contains(discard_id));

        // TODO: Find way to test these
        // Return,
        // Halt,
    }

    #[test]
    fn illegal_op() {
        let mut vm = Vm::new(
            Vec::new(),
            &crate::OptionBuilder::new("./variable_tests").build(),
        );

        let illegal = Instruction::Illegal;
        assert_eq!(
            illegal.execute(&mut vm).err().unwrap().ty,
            RuntimeErrorTy::IllegalInstruction
        );
    }
}
