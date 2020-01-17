use super::{Register, RuntimeValue, Vm};

pub mod functions;

/// A type alias for results that could be a [`RuntimeError`]
pub type Result<T> = std::result::Result<T, RuntimeError>;

/// A Crunch Runtime Error
// TODO: Make this more detailed
#[repr(C)]
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
    MissingSymbol,
    JitError,
}

/// Instructions for the VM
// TODO: Document all Instructions
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Instruction {
    /// Load a Value directly into a register
    Load(RuntimeValue, Register),
    CompToReg(Register),
    OpToReg(Register),
    Drop(Register),
    Move(Register, Register),

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

    Func(u32),
    Yield,
    Return,

    Collect,
    Halt,

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
    pub fn execute(&self, vm: &mut Vm) -> Result<()> {
        trace!("Executing instruction {:?}", self);

        match self {
            Self::Load(val, reg) => functions::load(vm, val.clone(), **reg)?,
            Self::CompToReg(reg) => functions::comp_to_reg(vm, **reg)?,
            Self::OpToReg(reg) => functions::comp_to_reg(vm, **reg)?,
            Self::Drop(reg) => functions::drop(vm, **reg)?,
            Self::Move(target, source) => functions::mov(vm, **target, **source)?,

            Self::Add(left, right) => functions::add(vm, **left, **right)?,
            Self::Sub(left, right) => functions::sub(vm, **left, **right)?,
            Self::Mult(left, right) => functions::mult(vm, **left, **right)?,
            Self::Div(left, right) => functions::div(vm, **left, **right)?,

            Self::Print(reg) => functions::print(vm, **reg)?,

            Self::Jump(index) => functions::jump(vm, *index)?,
            Self::JumpComp(index) => {
                functions::jump_comp(vm, *index)?;
            }

            Self::And(left, right) => functions::and(vm, **left, **right)?,
            Self::Or(left, right) => functions::or(vm, **left, **right)?,
            Self::Xor(left, right) => functions::xor(vm, **left, **right)?,
            Self::Not(reg) => functions::not(vm, **reg)?,

            Self::Eq(left, right) => functions::eq(vm, **left, **right)?,
            Self::NotEq(left, right) => functions::not_eq(vm, **left, **right)?,
            Self::GreaterThan(left, right) => functions::greater_than(vm, **left, **right)?,
            Self::LessThan(left, right) => functions::less_than(vm, **left, **right)?,

            Self::Func(func) => functions::func(vm, *func)?,
            Self::Yield => functions::yield_generator(vm)?,
            Self::Return => functions::ret(vm)?,

            Self::Collect => functions::collect(vm)?,
            Self::Halt => functions::halt(vm)?,
            Self::NoOp => functions::no_op(vm)?,
            Self::JumpPoint(_) => functions::jump_point(vm)?,
            Self::Illegal => functions::illegal(vm)?,
        }

        Ok(())
    }

    /// Turns the instruction into a string representation, for disassembly purposes
    #[must_use]
    pub fn to_str(&self) -> &'static str {
        match self {
            Self::Load(_, _) => "ld",
            Self::CompToReg(_) => "cmp",
            Self::OpToReg(_) => "opr",
            Self::Drop(_) => "drop",
            Self::Move(_, _) => "mov",

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

            Self::Func(_) => "call",
            Self::Yield => "yield",
            Self::Return => "ret",

            Self::Collect => "coll",
            Self::Halt => "halt",

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
    fn function_test() {
        use crate::Crunch;

        let mut crunch = Crunch::new(crate::OptionBuilder::new("./function_test").build());
        let functions = vec![
            vec![
                Instruction::Load(RuntimeValue::Str("Calling the function!\n"), 31.into()),
                Instruction::Print(31.into()),
                Instruction::Drop(31.into()),
                Instruction::Load(RuntimeValue::Bool(false), 0.into()),
                Instruction::Func(1_u32.into()),
                Instruction::Load(RuntimeValue::Str("Was the function called? "), 31.into()),
                Instruction::Load(RuntimeValue::Str("\n"), 30.into()),
                Instruction::Print(31.into()),
                Instruction::Print(0.into()),
                Instruction::Print(30.into()),
                Instruction::Drop(31.into()),
                Instruction::Drop(30.into()),
                Instruction::Drop(0.into()),
                Instruction::Return,
            ],
            vec![
                Instruction::Load(RuntimeValue::Str("The function was called!\n"), 0.into()),
                Instruction::Print(0.into()),
                Instruction::Drop(0.into()),
                Instruction::Load(RuntimeValue::Bool(true), 0.into()),
                Instruction::Return,
            ],
        ];

        crunch.execute(functions).unwrap();
    }

    #[test]
    fn variable_ops() {
        let mut vm = Vm::new(
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
            assert!(vm.registers[0].clone().is_equal(val, &vm.gc).unwrap());
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
                .clone()
                .is_equal(RuntimeValue::Bool(true), &vm.gc)
                .unwrap());

            // Set the previous comparison to `false`
            vm.prev_comp = false;
            comp_to_reg.execute(&mut vm).unwrap();
            assert!(vm.registers[0]
                .clone()
                .is_equal(RuntimeValue::Bool(false), &vm.gc)
                .unwrap());
        }

        // Testing the OpToReg instruction, it takes the value of the operation register and store its value
        // in the given register, register 0
        let op_to_reg = Instruction::OpToReg(0.into());
        for val in values.clone() {
            vm.prev_op = val.clone();
            op_to_reg.execute(&mut vm).unwrap();

            println!(
                "{:?}/{:?} == {:?} = {:?}",
                vm.prev_op,
                vm.registers[0].clone(),
                val.clone(),
                vm.registers[0].clone().is_equal(val.clone(), &vm.gc)
            );
            assert!(vm.registers[0].clone().is_equal(val, &vm.gc).unwrap());
        }

        // Do a GC Reset
        vm.gc = crate::Gc::new(&crate::OptionBuilder::new("./variable_ops").build());

        // Testing the DropReg instruction, which should drop a value from a register
        {
            vm.registers[0] = RuntimeValue::Str("test string"); // Load the register before hand
            let drop_reg = Instruction::Drop(0.into());
            drop_reg.execute(&mut vm).unwrap();
            assert!(vm.registers[0]
                .clone()
                .is_equal(RuntimeValue::Str("test string"), &vm.gc)
                .unwrap());
        }
    }

    #[test]
    fn print_op<'a>() {
        use std::mem;

        let print = Instruction::Print(0.into());
        let mut vm = Vm::new(
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
            &crate::OptionBuilder::new("./illegal_op").build(),
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
                    &crate::OptionBuilder::new("./misc_ops").build(),
                    Box::new(stdout()),
                );

                let collect = Instruction::Collect;

                let discard = vm.gc.alloc(std::mem::size_of::<RuntimeValue>()).unwrap();
                unsafe {
                    vm.gc
                        .write(discard, &<RuntimeValue as Into<Vec<u8>>>::into(RuntimeValue::I32(int)))
                        .expect("here");
                }
                vm.gc.add_root(discard);

                assert!(vm.gc.contains(discard));
                assert!(vm.gc.fetch(discard) == Ok(&RuntimeValue::I32(int)));
                vm.gc.remove_root(discard).unwrap();

                collect.execute(&mut vm).unwrap();
                assert!(!vm.gc.contains(discard));

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
