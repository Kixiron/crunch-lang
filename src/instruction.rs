use super::{Index, Register, RuntimeValue, Vm};

/// A type alias for results that could be a [`RuntimeError`]
pub type Result<T> = std::result::Result<T, RuntimeError>;

/// A Crunch Runtime Error
// TODO: Make this more detailed
#[derive(Debug, Clone, Eq)]
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

impl PartialEq for RuntimeError {
    fn eq(&self, other: &Self) -> bool {
        self.ty == other.ty
    }
}

/// The type of [`RuntimeError`] that occurred
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
    CompilationError,
    MissingFile,
    InvalidInt,
    StdoutError,
    IntegerOverflow,
}

/// Instructions for the VM
// TODO: Document all Instructions
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Instruction {
    /// Load a Value directly into a register
    Load(RuntimeValue, Register),
    CompToReg(Register),
    OpToReg(Register),
    DropReg(Register),

    Add(Register, Register),
    Sub(Register, Register),
    Mult(Register, Register),
    Div(Register, Register),

    Print(Register),

    Jump(i32),
    JumpComp(i32),
    JumpPoint(u32),

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
    NoOp,
}

impl Instruction {
    /// The execution of each instruction
    // TODO: Document this bad boy
    #[inline]
    pub fn execute(&self, mut vm: &mut Vm) -> Result<()> {
        match self {
            Instruction::Load(val, reg) => {
                trace!("Loading val into {}", reg);

                vm.registers[**reg as usize] = *val;
                vm.index += Index(1);
            }
            Instruction::CompToReg(reg) => {
                trace!("Loading previous comparison into {}", reg);

                vm.registers[**reg as usize] = RuntimeValue::Bool(vm.prev_comp);
                vm.index += Index(1);
            }
            Instruction::OpToReg(reg) => {
                trace!(
                    "Loading previous operation ({:?}) into {}",
                    &vm.prev_op,
                    reg
                );

                vm.registers[**reg as usize] = RuntimeValue::None;
                std::mem::swap(&mut vm.registers[**reg as usize], &mut vm.prev_op);
                vm.index += Index(1);
            }
            Instruction::DropReg(reg) => {
                trace!("Clearing register {}", reg);

                vm.registers[**reg as usize].drop(&mut vm.gc)?;
                vm.index += Index(1);
            }

            Instruction::Add(left, right) => {
                println!("here");
                vm.prev_op = vm.registers[**left as usize]
                    .add_upflowing(vm.registers[**right as usize], &mut vm.gc)?;
                vm.index += Index(1);
            }
            Instruction::Sub(left, right) => {
                vm.prev_op = vm.registers[**left as usize]
                    .sub_upflowing(vm.registers[**right as usize], &mut vm.gc)?;
                vm.index += Index(1);
            }
            Instruction::Mult(left, right) => {
                vm.prev_op = vm.registers[**left as usize]
                    .mult_upflowing(vm.registers[**right as usize], &mut vm.gc)?;
                vm.index += Index(1);
            }
            Instruction::Div(left, right) => {
                vm.prev_op = vm.registers[**left as usize]
                    .div_upflowing(vm.registers[**right as usize], &mut vm.gc)?;
                vm.index += Index(1);
            }

            Instruction::Print(reg) => {
                trace!("Printing reg {:?}", reg);

                if let Err(err) = write!(
                    vm.stdout,
                    "{}",
                    vm.registers[**reg as usize].to_string(&vm.gc)?
                ) {
                    error!("Error printing to stdout: {:?}", err);
                    return Err(RuntimeError {
                        ty: RuntimeErrorTy::StdoutError,
                        message: "Failed to print to stdout".to_string(),
                    });
                }

                vm.index += Index(1);
            }

            Instruction::Jump(index) => {
                trace!("Jumping by offset {}", index);

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

                if vm.prev_comp {
                    vm.index = Index((*vm.index as i32 + *index + 1) as u32);
                } else {
                    vm.index += Index(1);
                }
            }

            /*
            Instruction::And(left, right) => {
                vm.prev_op = (*vm).get(*left).bit_and((*vm).get(*right), &vm.gc)?;
                vm.index += Index(1);
            }
            Instruction::Or(left, right) => {
                vm.prev_op = (*vm).get(*left).bit_or((*vm).get(*right), &vm.gc)?;
                vm.index += Index(1);
            }
            Instruction::Xor(left, right) => {
                vm.prev_op = (*vm).get(*left).bit_xor((*vm).get(*right), &vm.gc)?;
                vm.index += Index(1);
            }
            Instruction::Not(reg) => {
                vm.prev_op = (*vm).get(*reg).not(&vm.gc)?;
                vm.index += Index(1);
            }

            Instruction::Eq(left, right) => {
                vm.prev_comp = (*vm).get(*left).eq((*vm).get(*right), &vm.gc)?;
                vm.index += Index(1);
            }
            Instruction::NotEq(left, right) => {
                vm.prev_comp = !(*vm).get(*left).eq((*vm).get(*right), &vm.gc)?;
                vm.index += Index(1);
            }
            Instruction::GreaterThan(left, right) => {
                vm.prev_comp = (*vm).get(*left).cmp((*vm).get(*right), &vm.gc)?
                    == Some(std::cmp::Ordering::Greater);
                vm.index += Index(1);
            }
            Instruction::LessThan(left, right) => {
                vm.prev_comp = (*vm).get(*left).cmp((*vm).get(*right), &vm.gc)?
                    == Some(std::cmp::Ordering::Less);
                vm.index += Index(1);
            }
            */
            Instruction::Collect => {
                trace!("Forcing a GC collect");

                vm.gc.collect()?;
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
            Instruction::Syscall(
                _offset,
                _output,
                _param_1,
                _param_2,
                _param_3,
                _param_4,
                _param_5,
            ) => {
                unimplemented!("Syscalls are not stable");
            }

            Instruction::NoOp => {
                vm.index += Index(1);
            }

            Instruction::Illegal | Instruction::JumpPoint(_) => {
                return Err(RuntimeError {
                    ty: RuntimeErrorTy::IllegalInstruction,
                    message: "Illegal Instruction".to_string(),
                })
            }

            _ => {
                vm.index += Index(1);
            }
        }

        Ok(())
    }

    /// Turns the instruction into a string representation, for disassembly purposes
    pub fn to_str(&self) -> &'static str {
        match self {
            Self::Load(_, _) => "ld",
            Self::CompToReg(_) => "compr",
            Self::OpToReg(_) => "opr",
            Self::DropReg(_) => "dropr",

            Self::Add(_, _) => "add",
            Self::Sub(_, _) => "sub",
            Self::Mult(_, _) => "mul",
            Self::Div(_, _) => "div",

            Self::Print(_) => "print",

            Self::Jump(_) => "jmp",
            Self::JumpComp(_) => "jmpcmp",
            Self::JumpPoint(_) => "jmppt",

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
            Self::NoOp => "nop",
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

        // Construct random values to test with
        let values = {
            use rand::Rng;
            use std::thread;

            let mut rng = rand::thread_rng(); // Init rng
            let (num_ints, num_strs) = (rng.gen_range(200, 1000), rng.gen_range(300, 1500)); // Get the number of integers and strings to generate
            let mut vec = Vec::with_capacity(num_ints + num_strs); // Create a vec to hold all the values

            // Create the integers randomly
            let ints = thread::spawn(move || {
                let mut rng = rand::thread_rng();
                let mut vec = Vec::with_capacity(num_ints);

                for _ in 0..num_ints {
                    vec.push(RuntimeValue::I32(rng.gen_range(0, i32::max_value())));
                }

                vec
            });

            // Create half of the strings randomly
            let create_strings = move || {
                let mut rng = rand::thread_rng();
                let mut vec = Vec::with_capacity(num_strs / 2);

                for _ in 0..num_strs / 2 {
                    let len = rng.gen_range(10, 200);

                    let string: Vec<u8> = rng
                        .sample_iter(rand::distributions::Standard)
                        .take(len)
                        .collect();

                    if let Ok(string) = String::from_utf8(string) {
                        vec.push(RuntimeValue::Str(Box::leak(string.into_boxed_str())));
                    }
                }

                vec
            };

            let first_strings = thread::spawn(create_strings);
            let second_strings = thread::spawn(create_strings);

            // Add boolean values to test on
            vec.push(RuntimeValue::Bool(true));
            vec.push(RuntimeValue::Bool(false));
            // Add all the randomly generated values to the vector
            vec.extend_from_slice(&ints.join().unwrap());
            vec.extend_from_slice(&first_strings.join().unwrap());
            vec.extend_from_slice(&second_strings.join().unwrap());

            vec
        };

        // Testing the Load instruction, it should take a value and store it in a register, which in this case is
        // register 0
        for val in values.clone().into_iter() {
            // Construct and execute the Load instruction
            let load = Instruction::Load(val.clone(), 0.into());
            load.execute(&mut vm).unwrap();

            // Assert that the registers contain the correct value
            assert!(vm.registers[0].is_equal(val, &vm.gc).unwrap());
        }

        // Do a GC Reset
        vm.gc = crate::Gc::new(&crate::OptionBuilder::new("./variable_ops").build());

        {
            // Testing the CompToReg instruction, it should take the value of the comparison register and store its value
            // in the given register, register 0
            let comp_to_reg = Instruction::CompToReg(0.into());

            // Set the previous comparison to `true`
            vm.prev_comp = true;
            comp_to_reg.execute(&mut vm).unwrap();
            assert!(vm.registers[0]
                .is_equal(RuntimeValue::Bool(true), &vm.gc)
                .unwrap());

            // Set the previous comparison to `false`
            vm.prev_comp = false;
            comp_to_reg.execute(&mut vm).unwrap();
            assert!(vm.registers[0]
                .is_equal(RuntimeValue::Bool(false), &vm.gc)
                .unwrap());
        }

        // Testing the OpToReg instruction, it takes the value of the operation register and store its value
        // in the given register, register 0
        let op_to_reg = Instruction::OpToReg(0.into());
        for val in values.clone() {
            vm.prev_op = val.clone();
            op_to_reg.execute(&mut vm).unwrap();

            assert!(vm.registers[0].is_equal(val, &vm.gc).unwrap());
        }

        // Do a GC Reset
        vm.gc = crate::Gc::new(&crate::OptionBuilder::new("./variable_ops").build());

        // Testing the DropReg instruction, which should drop a value from a register
        {
            vm.registers[0] = RuntimeValue::Str("test string"); // Load the register before hand
            let drop_reg = Instruction::DropReg(0.into());
            drop_reg.execute(&mut vm).unwrap();
            assert!(vm.registers[0]
                .is_equal(RuntimeValue::Str("test string"), &vm.gc)
                .unwrap());
        }
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

        let swap_assert = |vm: &mut Vm, expected: &str| {
            // Have to do some monkeying with stdout because of Vm's drop implementation
            let mut stdout: Box<dyn std::io::Write + 'static> = Box::new(Vec::<u8>::new());
            mem::swap(&mut vm.stdout, &mut stdout);

            // Assert that the printed value and the expected are the same
            assert_eq!(unsafe { *(Box::into_raw(stdout) as *const &str) }, expected);
        };

        // Test printing register values
        {
            vm.registers[0] = RuntimeValue::Str("Test");
            print.execute(&mut vm).unwrap();
            swap_assert(&mut vm, "Test");

            vm.registers[0] = RuntimeValue::I32(10);
            print.execute(&mut vm).unwrap();
            swap_assert(&mut vm, "10");

            vm.registers[0] = RuntimeValue::Bool(true);
            print.execute(&mut vm).unwrap();
            swap_assert(&mut vm, "true");

            vm.registers[0] = RuntimeValue::Bool(false);
            print.execute(&mut vm).unwrap();
            swap_assert(&mut vm, "false");

            // Test that writing to stdout works too, can only verify that it does, not that it is correct
            vm.stdout = Box::new(std::io::stdout());

            vm.registers[0] = RuntimeValue::Str("Test");
            print.execute(&mut vm).unwrap();

            vm.registers[0] = RuntimeValue::I32(10);
            print.execute(&mut vm).unwrap();

            vm.registers[0] = RuntimeValue::Bool(true);
            print.execute(&mut vm).unwrap();

            vm.registers[0] = RuntimeValue::Bool(false);
            print.execute(&mut vm).unwrap();
        }

        // Test printing gc values
        {
            vm.stdout = Box::new(Vec::<u8>::new());

            vm.registers[0] =
                RuntimeValue::GcString(crate::GcStr::new("Test", &mut vm.gc).unwrap());
            print.execute(&mut vm).unwrap();
            swap_assert(&mut vm, "Test");

            vm.registers[0] = RuntimeValue::I32(10);
            print.execute(&mut vm).unwrap();
            swap_assert(&mut vm, "10");

            vm.registers[0] = RuntimeValue::Bool(true);
            print.execute(&mut vm).unwrap();
            swap_assert(&mut vm, "true");

            vm.registers[0] = RuntimeValue::Bool(false);
            print.execute(&mut vm).unwrap();
            swap_assert(&mut vm, "false");

            // Test that writing to stdout works too, can only verify that it does, not that it is correct
            vm.stdout = Box::new(std::io::stdout());

            vm.registers[0] =
                RuntimeValue::GcString(crate::GcStr::new("Test", &mut vm.gc).unwrap());
            print.execute(&mut vm).unwrap();

            vm.registers[0] = RuntimeValue::I32(10);
            print.execute(&mut vm).unwrap();

            vm.registers[0] = RuntimeValue::Bool(true);
            print.execute(&mut vm).unwrap();

            vm.registers[0] = RuntimeValue::Bool(false);
            print.execute(&mut vm).unwrap();
        }
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
    fn eq_ops() {
        let mut vm = Vm::new(
            Vec::new(),
            &crate::OptionBuilder::new("./eq_ops").build(),
            Box::new(stdout()),
        );

        vm.registers[0] = RuntimeValue::I32(10);
        vm.registers[1] = RuntimeValue::I32(10);

        let eq = Instruction::Eq(0.into(), 1.into());
        eq.execute(&mut vm).unwrap();
        assert_eq!(vm.prev_comp, true);

        vm.registers[0] = RuntimeValue::I32(20);

        let not_eq = Instruction::NotEq(0.into(), 1.into());
        not_eq.execute(&mut vm).unwrap();
        assert_eq!(vm.prev_comp, true);

        let greater_than = Instruction::GreaterThan(0.into(), 1.into());
        greater_than.execute(&mut vm).unwrap();
        assert_eq!(vm.prev_comp, true);

        vm.registers[0] = RuntimeValue::I32(0);

        let less_than = Instruction::LessThan(0.into(), 1.into());
        less_than.execute(&mut vm).unwrap();
        assert_eq!(vm.prev_comp, true);
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

    mod property_tests {
        use super::super::*;
        use proptest::prelude::*;
        use std::io::stdout;

        proptest! {
            #[test]
            fn collect(int in 0..i32::max_value(), string in "\\PC*") {
                let mut vm = Vm::new(
                    Vec::new(),
                    &crate::OptionBuilder::new("./misc_ops").build(),
                    Box::new(stdout()),
                );

                let collect = Instruction::Collect;

                let (discard, discard_id) = vm.gc.allocate(std::mem::size_of::<RuntimeValue>()).unwrap();
                unsafe {
                    vm.gc
                        .write(discard_id, RuntimeValue::I32(int), Some(&discard))
                        .unwrap();
                }
                vm.gc.add_root(discard);

                assert!(vm.gc.contains(discard_id));
                assert!(vm.gc.fetch(discard_id) == Ok(RuntimeValue::I32(int)));
                vm.gc.remove_root(discard_id).unwrap();

                collect.execute(&mut vm).unwrap();
                assert!(!vm.gc.contains(discard_id));

                let gc_str = crate::GcStr::new(&string, &mut vm.gc).unwrap();

                assert!(vm.gc.contains(gc_str.id));
                assert!(gc_str.to_str(&vm.gc) == Ok(&string));

                gc_str.drop(&mut vm.gc).unwrap();
                collect.execute(&mut vm).unwrap();
                assert!(!vm.gc.contains(gc_str.id));
            }

            #[test]
            fn bitwise_and(left in 0_i32..i32::max_value(), right in 0_i32..i32::max_value()) {
                let mut vm = Vm::new(
                    Vec::new(),
                    &crate::OptionBuilder::new("./bitwise_ops").build(),
                    Box::new(stdout()),
                );

                vm.registers[0] = RuntimeValue::I32(left);
                vm.registers[1] = RuntimeValue::I32(right);

                let and = Instruction::And(0.into(), 1.into());
                and.execute(&mut vm).unwrap();

                assert!(vm.prev_op.is_equal(RuntimeValue::I32(left & right), &vm.gc).unwrap());
            }

            #[test]
            fn bitwise_or(left in 0_i32..i32::max_value(), right in 0_i32..i32::max_value()) {
                let mut vm = Vm::new(
                    Vec::new(),
                    &crate::OptionBuilder::new("./bitwise_ops").build(),
                    Box::new(stdout()),
                );

                vm.registers[0] = RuntimeValue::I32(left);
                vm.registers[1] = RuntimeValue::I32(right);

                let or = Instruction::Or(0.into(), 1.into());
                or.execute(&mut vm).unwrap();

                assert!(vm.prev_op.is_equal(RuntimeValue::I32(left | right), &vm.gc).unwrap());
            }

            #[test]
            fn bitwise_xor(left in 0_i32..i32::max_value(), right in 0_i32..i32::max_value()) {
                let mut vm = Vm::new(
                    Vec::new(),
                    &crate::OptionBuilder::new("./bitwise_ops").build(),
                    Box::new(stdout()),
                );

                vm.registers[0] = RuntimeValue::I32(left);
                vm.registers[1] = RuntimeValue::I32(right);

                let xor = Instruction::Xor(0.into(), 1.into());
                xor.execute(&mut vm).unwrap();

                assert!(vm.prev_op.is_equal(RuntimeValue::I32(left ^ right), &vm.gc).unwrap());
            }

            #[test]
            fn bitwise_not(int in 0_i32..i32::max_value()) {
                let mut vm = Vm::new(
                    Vec::new(),
                    &crate::OptionBuilder::new("./bitwise_ops").build(),
                    Box::new(stdout()),
                );

                vm.registers[0] = RuntimeValue::I32(int);

                let not = Instruction::Not(0.into());
                not.execute(&mut vm).unwrap();

                assert!(vm.prev_op.is_equal(RuntimeValue::I32(!int), &vm.gc).unwrap());
            }

            #[test]
            fn add(left in 0_i32..i32::max_value(), right in 0_i32..i32::max_value()) {
                let mut vm = Vm::new(
                    Vec::new(),
                    &crate::OptionBuilder::new("./math_ops").build(),
                    Box::new(stdout()),
                );

                vm.registers[0] = RuntimeValue::I32(left);
                vm.registers[1] = RuntimeValue::I32(right);

                let add = Instruction::Add(0.into(), 1.into());
                add.execute(&mut vm).unwrap();

                assert!(vm.prev_op.is_equal(RuntimeValue::I32(left).add_upflowing(RuntimeValue::I32(right), &mut vm.gc).unwrap(), &vm.gc).unwrap());
            }

            #[test]
            fn subtract(left in 0_i32..i32::max_value(), right in 0_i32..i32::max_value()) {
                let mut vm = Vm::new(
                    Vec::new(),
                    &crate::OptionBuilder::new("./math_ops").build(),
                    Box::new(stdout()),
                );

                vm.registers[0] = RuntimeValue::I32(left);
                vm.registers[1] = RuntimeValue::I32(right);

                let sub = Instruction::Sub(0.into(), 1.into());
                sub.execute(&mut vm).unwrap();

                assert!(vm.prev_op.is_equal(RuntimeValue::I32(left).sub_upflowing(RuntimeValue::I32(right), &mut vm.gc).unwrap(), &vm.gc).unwrap());
            }

            #[test]
            fn multiply(left in 0_i32..i32::max_value(), right in 0_i32..i32::max_value()) {
                let mut vm = Vm::new(
                    Vec::new(),
                    &crate::OptionBuilder::new("./math_ops").build(),
                    Box::new(stdout()),
                );

                vm.registers[0] = RuntimeValue::I32(left);
                vm.registers[1] = RuntimeValue::I32(right);

                let mult = Instruction::Mult(0.into(), 1.into());
                mult.execute(&mut vm).unwrap();

                assert!(vm.prev_op.is_equal(RuntimeValue::I32(left).mult_upflowing(RuntimeValue::I32(right), &mut vm.gc).unwrap(), &vm.gc).unwrap());
            }

            #[test]
            fn divide(left in 0_i32..i32::max_value(), right in 0_i32..i32::max_value()) {
                let mut vm = Vm::new(
                    Vec::new(),
                    &crate::OptionBuilder::new("./math_ops").build(),
                    Box::new(stdout()),
                );

                vm.registers[0] = RuntimeValue::I32(left);
                vm.registers[1] = RuntimeValue::I32(right);

                let div = Instruction::Div(0.into(), 1.into());
                div.execute(&mut vm).unwrap();

                assert!(vm.prev_op.is_equal(RuntimeValue::I32(left).div_upflowing(RuntimeValue::I32(right), &mut vm.gc).unwrap(), &vm.gc).unwrap());
            }
        }
    }
}
