mod property_tests;

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

    crunch.execute(&functions).unwrap();
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
        assert!(vm.registers[0].is_equal(&val, &vm.gc).unwrap());
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
            .is_equal(&RuntimeValue::Bool(true), &vm.gc)
            .unwrap());

        // Set the previous comparison to `false`
        vm.prev_comp = false;
        comp_to_reg.execute(&mut vm).unwrap();
        assert!(vm.registers[0]
            .is_equal(&RuntimeValue::Bool(false), &vm.gc)
            .unwrap());
    }

    // Testing the OpToReg instruction, it takes the value of the operation register and store its value
    // in the given register, register 0
    let op_to_reg = Instruction::OpToReg(0.into());
    for val in values.clone() {
        vm.prev_op = val.clone();
        op_to_reg.execute(&mut vm).unwrap();

        assert!(vm.registers[0].is_equal(&val, &vm.gc).unwrap());
    }

    // Do a GC Reset
    vm.gc = crate::Gc::new(&crate::OptionBuilder::new("./variable_ops").build());

    // Testing the DropReg instruction, which should drop a value from a register
    {
        vm.registers[0] = RuntimeValue::Str("test string"); // Load the register before hand
        let drop_reg = Instruction::Drop(0.into());
        drop_reg.execute(&mut vm).unwrap();
        assert!(vm.registers[0].is_none());
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

        vm.registers[0] = RuntimeValue::F32(6.9);
        print.execute(&mut vm).unwrap();
        swap_assert(&mut vm, "6.9");

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
        /*
        TODO: Rework this
        vm.stdout = Box::new(Vec::<u8>::new());

        vm.registers[0] = RuntimeValue::GcString("Test", &mut vm.gc).unwrap();
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
        */
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
fn incompatible_eq_ops() {
    let mut vm = Vm::new(
        &crate::OptionBuilder::new("./eq_ops").build(),
        Box::new(stdout()),
    );

    // test incompatible types
    vm.registers[0] = RuntimeValue::Bool(true);
    vm.registers[1] = RuntimeValue::I32(10);

    let less_than = Instruction::LessThan(0.into(), 1.into());
    assert_eq!(
        less_than.execute(&mut vm).unwrap_err().message,
        "Values of types 'bool' and 'int' cannot be 'less_than'ed".to_string(),
    );

    vm.options.fault_tolerant = true;
    less_than.execute(&mut vm).unwrap();
    assert_eq!(vm.prev_comp, false);
}

macro_rules! test_eq_ops {
    ( $( $fn_name:ident { internal: $internal:tt , hi: $hi:literal, mid: $mid:literal, lo: $lo:literal } $(,)? )* ) => { $(
        #[test]
        fn $fn_name() {
            let mut vm = Vm::new(
                &crate::OptionBuilder::new("./eq_ops").build(),
                Box::new(stdout()),
            );

            // test incompatible types
            vm.registers[0] = RuntimeValue::$internal($mid);
            vm.registers[1] = RuntimeValue::$internal($lo);

            let greater_than = Instruction::GreaterThan(0.into(), 1.into());
            greater_than.execute(&mut vm).unwrap();
            assert_eq!(vm.prev_comp, true);

            let not_eq = Instruction::NotEq(0.into(), 1.into());
            not_eq.execute(&mut vm).unwrap();
            assert_eq!(vm.prev_comp, true);

            let lete = Instruction::LessThanEq(0.into(), 1.into());
            lete.execute(&mut vm).unwrap();
            assert_eq!(vm.prev_comp, false);

            vm.registers[1] = RuntimeValue::$internal($mid);

            let grte = Instruction::GreaterThanEq(0.into(), 1.into());
            grte.execute(&mut vm).unwrap();
            assert_eq!(vm.prev_comp, true);

            let eq = Instruction::Eq(0.into(), 1.into());
            eq.execute(&mut vm).unwrap();
            assert_eq!(vm.prev_comp, true);

            vm.registers[1] = RuntimeValue::$internal($hi);

            greater_than.execute(&mut vm).unwrap();
            assert_eq!(vm.prev_comp, false);

            let less_than = Instruction::LessThan(0.into(), 1.into());
            less_than.execute(&mut vm).unwrap();
            assert_eq!(vm.prev_comp, true);

            // should return false with parameters flipped
            let less_than = Instruction::LessThan(1.into(), 0.into());
            less_than.execute(&mut vm).unwrap();
            assert_eq!(vm.prev_comp, false);
        }
    )*}
}

test_eq_ops! {
    u16_ops {
        internal: U16,
        hi: 128,
        mid: 20,
        lo: 0
    }
    u32_ops {
        internal: U32,
        hi: 20,
        mid: 10,
        lo: 0
    }
    u64_ops {
        internal: U64,
        hi: 128,
        mid: 20,
        lo: 0
    }
    u128_ops {
        internal: U128,
        hi: 20,
        mid: 10,
        lo: 0
    }

    i16_ops {
        internal: I16,
        hi: 128,
        mid: 0,
        lo: -1
    }
    i32_eq_ops {
        internal: I32,
        hi: 1,
        mid: 0,
        lo: -1
    }
    i64_ops {
        internal: I64,
        hi: 128,
        mid: 20,
        lo: 0
    }
    i128_ops {
        internal: I128,
        hi: 20,
        mid: 10,
        lo: -0
    }

    f32_eq_ops {
        internal: F32,
        hi: 10.1,
        mid: 10.0,
        lo: 9.7
    }
    f64_eq_ops {
        internal: F64,
        hi: 10.1,
        mid: 10.0,
        lo: 9.7
    }
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
