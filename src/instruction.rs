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
    InvalidJump,
    MissingValue,
    MissingString,
    InvalidString,
    FileError,
    BytecodeError,
}

/// Instructions for the VM
// TODO: Document all Instructions
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Instruction {
    /// Load a Value from the GC into a register
    Load(u32, Register),
    /// Cache a Value into the GC
    Cache(u32, Value),
    // TODO: Have a direct load to a register
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
    // TODO: Use a `Value::ParamBuf` instead
    Syscall(
        u8,
        Register,
        Register,
        Register,
        Register,
        Register,
        Register,
    ),

    // TODO: Handle FFI with the following instructions
    // LoadLib(&'static str), // Loads a dynamic library
    // CallLib(&'static str, input: Value::ParamBuf, output: Value::ParamBuf),

    // An illegal instruction
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

                trace!("Loading {:?} into {}", &val, reg);

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
                trace!("Previous Operation Value: {:?}", &vm.prev_op);

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

                if let Err(_err) = write!(vm.stdout, "{}", vm.get(*reg).clone()) {
                    panic!("Handle this sometime");
                }

                vm.index += Index(1);
            }

            Instruction::Jump(index) => {
                trace!("Jumping by offset {}", index);

                println!("Jumping by {} on index {}", index, vm.index);

                let index = if index.is_negative() {
                    let (index, overflowed) = vm.index.overflowing_sub(index.abs() as u32);

                    if overflowed {
                        return Err(RuntimeError {
                            ty: RuntimeErrorTy::InvalidJump,
                            message: "Jump overflowed".to_string(),
                        });
                    }

                    index + 1
                } else {
                    *vm.index + *index as u32 + 1
                };

                vm.index = Index(index);
            }
            Instruction::JumpComp(index) => {
                trace!(
                    "Comparison Jump: Prev Comp is {}, jump amount is {}",
                    vm.prev_comp,
                    index
                );

                let index = if vm.prev_comp && index.is_negative() {
                    let (index, overflowed) = vm.index.overflowing_sub(index.abs() as u32);

                    if overflowed {
                        return Err(RuntimeError {
                            ty: RuntimeErrorTy::InvalidJump,
                            message: "Jump overflowed".to_string(),
                        });
                    }

                    index + 1
                } else if vm.prev_comp {
                    *vm.index + *index as u32 + 1
                } else {
                    *vm.index + 1
                };

                vm.index = Index(index);
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
                        vm.finished_execution = true;
                    }
                }

                vm.index += Index(1);
            }
            Instruction::Halt => {
                vm.finished_execution = true;
            }
            Instruction::Syscall(offset, output, param_1, param_2, param_3, param_4, param_5) => {
                let p = [param_1, param_2, param_3, param_4, param_5];
                let func = (*super::syscall::SYSCALL_TABLE)[*offset as usize];
                let func: unsafe extern "C" fn(usize, usize, usize, usize, usize) -> usize =
                    unsafe { std::mem::transmute(func) };

                let mut params = [0_usize; 5];
                for i in 0..5 {
                    match vm.registers[**p[i] as usize] {
                        Value::Pointer(p) => params[i] = p,
                        Value::Int(int) => params[i] = int as usize,
                        _ => {}
                    }
                }

                let result = unsafe { func(params[0], params[1], params[2], params[3], params[4]) };

                vm.registers[**output as usize] = Value::Pointer(result);
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
            Self::Syscall(_, _, _, _, _, _, _) => "sysc",

            Self::Illegal => "illegal",
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::stdout;

    #[test]
    fn variable_ops() {
        let mut vm = Vm::new(
            Vec::new(),
            &crate::OptionBuilder::new("./variable_ops").build(),
            Box::new(stdout()),
        );

        let values = {
            use rand::Rng;
            use std::thread;

            let mut rng = rand::thread_rng();
            let (num_ints, num_strs) = (rng.gen_range(200, 1000), rng.gen_range(300, 1500));
            let mut vec = Vec::with_capacity(num_ints + num_strs);

            let ints = thread::spawn(move || {
                let mut rng = rand::thread_rng();
                let mut vec = Vec::with_capacity(num_ints);

                for _ in 0..num_ints {
                    vec.push(Value::Int(rng.gen_range(0, i32::max_value())));
                }

                vec
            });

            let first_strings = thread::spawn(move || {
                let mut rng = rand::thread_rng();
                let mut vec = Vec::with_capacity(num_strs / 2);

                for _ in 0..num_strs / 2 {
                    let len = rng.gen_range(10, 200);
                    let mut string = String::with_capacity(len);

                    for _ in 0..len {
                        string.push(rand::random::<char>());
                    }

                    vec.push(Value::String(string));
                }

                vec
            });

            let second_strings = thread::spawn(move || {
                let mut rng = rand::thread_rng();
                let mut vec = Vec::with_capacity(num_strs / 2);

                for _ in num_strs / 2..num_strs {
                    let len = rng.gen_range(10, 200);
                    let mut string = String::with_capacity(len);

                    for _ in 0..len {
                        string.push(rand::random::<char>());
                    }

                    vec.push(Value::String(string));
                }

                vec
            });

            vec.push(Value::Bool(true));
            vec.push(Value::Bool(false));

            vec.extend_from_slice(&ints.join().unwrap());
            vec.extend_from_slice(&first_strings.join().unwrap());
            vec.extend_from_slice(&second_strings.join().unwrap());

            vec
        };

        for (id, val) in values.clone().into_iter().enumerate() {
            let cache = Instruction::Cache(id as u32, val.clone());
            cache.execute(&mut vm).unwrap();
            assert!(vm.gc.contains(id));
            assert_eq!(vm.gc.fetch(id), Some(val));
        }

        // Do a VM Reset
        vm.gc = crate::Gc::new(&crate::OptionBuilder::new("./variable_ops").build());

        for (id, val) in values.clone().into_iter().enumerate() {
            let load = Instruction::Load(id as u32, 0.into());

            let (alloc_val, alloc_id) =
                vm.gc.allocate_id(std::mem::size_of::<Value>(), id).unwrap();
            unsafe {
                vm.gc
                    .write(alloc_id, val.clone(), Some(&alloc_val))
                    .unwrap();
            }
            vm.gc.add_root(alloc_val);

            vm.registers[0] = val.clone();
            load.execute(&mut vm).unwrap();
            assert_eq!(vm.registers[0], val);
        }

        // Do a VM Reset
        vm.gc = crate::Gc::new(&crate::OptionBuilder::new("./variable_ops").build());

        let comp_to_reg = Instruction::CompToReg(0.into());
        vm.prev_comp = true;
        comp_to_reg.execute(&mut vm).unwrap();
        assert_eq!(vm.registers[0], Value::Bool(true));
        vm.prev_comp = false;
        comp_to_reg.execute(&mut vm).unwrap();
        assert_eq!(vm.registers[0], Value::Bool(false));

        let op_to_reg = Instruction::OpToReg(2.into());
        for val in values.clone() {
            vm.prev_op = val.clone();
            op_to_reg.execute(&mut vm).unwrap();
            assert_eq!(vm.registers[2], val);
        }

        for (id, val) in values.clone().into_iter().enumerate() {
            let (alloc_val, alloc_id) =
                vm.gc.allocate_id(std::mem::size_of::<Value>(), id).unwrap();
            unsafe {
                vm.gc
                    .write(alloc_id, val.clone(), Some(&alloc_val))
                    .unwrap();
            }
            vm.gc.add_root(alloc_val);

            vm.registers[0] = val.clone();
            let save = Instruction::Save(id as u32, 0.into());
            save.execute(&mut vm).unwrap();
            assert!(vm.gc.contains(id));
            assert_eq!(vm.gc.fetch(id), Some(val));
        }

        let drop_reg = Instruction::DropReg(0.into());
        drop_reg.execute(&mut vm).unwrap();
        assert_eq!(vm.registers[1], Value::None);

        // Do a VM Reset
        vm.gc = crate::Gc::new(&crate::OptionBuilder::new("./variable_ops").build());

        for (id, val) in values.clone().into_iter().enumerate() {
            let drop = Instruction::Drop(id as u32);

            let (alloc_val, alloc_id) =
                vm.gc.allocate_id(std::mem::size_of::<Value>(), id).unwrap();
            unsafe {
                vm.gc
                    .write(alloc_id, val.clone(), Some(&alloc_val))
                    .unwrap();
            }
            vm.gc.add_root(alloc_val);

            assert!(vm.gc.contains(id));
            assert_eq!(vm.gc.fetch(id), Some(val.clone()));
            drop.execute(&mut vm).unwrap();
            vm.gc.collect();
            assert!(!vm.gc.contains(id));
            assert_eq!(vm.gc.fetch(id), <Option<Value>>::None);
        }
    }

    #[test]
    fn math_ops() {
        let mut vm = Vm::new(
            Vec::new(),
            &crate::OptionBuilder::new("./math_ops").build(),
            Box::new(stdout()),
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
        use std::mem;

        let print = Instruction::Print(0.into());
        let mut vm = Vm::new(
            Vec::new(),
            &crate::OptionBuilder::new("./print_op").build(),
            Box::new(Vec::<u8>::new()),
        );

        vm.registers[0] = Value::String("Test".to_string());
        print.execute(&mut vm).unwrap();

        // Have to do some monkeying with stdout because of Vm's drop implementation
        let mut stdout: Box<dyn std::io::Write + 'static> = Box::new(Vec::<u8>::new());
        mem::swap(&mut vm.stdout, &mut stdout);
        assert_eq!(unsafe { *(Box::into_raw(stdout) as *const &str) }, "Test");

        vm.registers[0] = Value::Int(10);
        print.execute(&mut vm).unwrap();

        let mut stdout: Box<dyn std::io::Write + 'static> = Box::new(Vec::<u8>::new());
        mem::swap(&mut vm.stdout, &mut stdout);
        assert_eq!(unsafe { *(Box::into_raw(stdout) as *const &str) }, "10");

        vm.registers[0] = Value::Bool(true);
        print.execute(&mut vm).unwrap();

        let mut stdout: Box<dyn std::io::Write + 'static> = Box::new(Vec::<u8>::new());
        mem::swap(&mut vm.stdout, &mut stdout);
        assert_eq!(unsafe { *(Box::into_raw(stdout) as *const &str) }, "true");

        vm.registers[0] = Value::Bool(false);
        print.execute(&mut vm).unwrap();

        let mut stdout: Box<dyn std::io::Write + 'static> = Box::new(std::io::stdout()); //  Load stdout into vm.stdout for the printing portion
        mem::swap(&mut vm.stdout, &mut stdout);
        assert_eq!(unsafe { *(Box::into_raw(stdout) as *const &str) }, "false");

        // Test that writing to stdout works too, can only verify that it does, not that it is correct

        vm.registers[0] = Value::String("Test".to_string());
        print.execute(&mut vm).unwrap();

        vm.registers[0] = Value::Int(10);
        print.execute(&mut vm).unwrap();

        vm.registers[0] = Value::Bool(true);
        print.execute(&mut vm).unwrap();

        vm.registers[0] = Value::Bool(false);
        print.execute(&mut vm).unwrap();
    }

    #[test]
    fn jump_ops() {
        let mut vm = Vm::new(
            Vec::new(),
            &crate::OptionBuilder::new("./jump_ops").build(),
            Box::new(stdout()),
        );

        // Each executed instruction increments the index by one, so take that into account

        let jump = Instruction::Jump(10);
        jump.execute(&mut vm).unwrap();
        assert_eq!(vm.index, 11.into());
        let jump = Instruction::Jump(-10);
        jump.execute(&mut vm).unwrap();
        assert_eq!(vm.index, 2.into());

        let jump_comp = Instruction::JumpComp(10);
        jump_comp.execute(&mut vm).unwrap();
        assert_eq!(vm.index, 3.into());
        vm.prev_comp = true;
        jump_comp.execute(&mut vm).unwrap();
        assert_eq!(vm.index, 14.into());
        let jump_comp = Instruction::JumpComp(-10);
        jump_comp.execute(&mut vm).unwrap();
        assert_eq!(vm.index, 5.into());
    }

    #[test]
    fn bitwise_ops() {
        let mut vm = Vm::new(
            Vec::new(),
            &crate::OptionBuilder::new("./bitwise_ops").build(),
            Box::new(stdout()),
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
            &crate::OptionBuilder::new("./eq_ops").build(),
            Box::new(stdout()),
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
            &crate::OptionBuilder::new("./misc_ops").build(),
            Box::new(stdout()),
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
            &crate::OptionBuilder::new("./illegal_ops").build(),
            Box::new(stdout()),
        );

        let illegal = Instruction::Illegal;
        assert_eq!(
            illegal.execute(&mut vm).err().unwrap().ty,
            RuntimeErrorTy::IllegalInstruction
        );
    }
}
