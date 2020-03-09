mod property_tests;

use super::*;
use crate::{bytecode, Crunch, Vm};
use std::io::stdout;

#[test]
fn array_test() {
    let functions = bytecode! {
        0 => {
            load vec![], 0;

            load 1i32, 1;
            load 2i32, 2;
            load 3i32, 3;
            pusharr 1, 0;
            pusharr 2, 0;
            pusharr 3, 0;

            load "\n", 3;
            load null, 1;

            print 0;
            print 3;

            poparr 2, 0;
            neq 1, 2;
            print 2;
            print 3;
            jumpcmp -4;

            load "Done!", 4;
            print 4;
            print 3;
            ret;
        }
    };

    Vm::default().execute(&functions).unwrap();
}

#[test]
fn generator_test() {
    let functions = bytecode! {
        0 => {
            load 5u8, 31;
            load 1u8, 30;
            load 0u8, 29;

            // Do this 6 times
            gen 1u32, 0;
            pop 0;
            pop 1;
            print 1;

            sub 31, 30;
            opr 31;
            neq 31, 30;
            jumpcmp -7;

            ret;
        }

        1 => {
            load 0i32, 0;
            push 0;
            yield;

            load 1i32, 0;
            push 0;
            yield;

            load 2i32, 0;
            push 0;
            yield;

            load 3i32, 0;
            push 0;
            yield;

            // Yield null forever
            load null, 0;
            push 0;
            yield;
            jump -3;
        }
    };

    Vm::default().execute(&functions).unwrap();
}

#[test]
fn eq() {
    let functions = bytecode! {
        0 => {
            load null, 0;
            load 1i32, 1;
            lesseq 0, 1;
            cmpr 2;
            print 2;
            ret;
        }
    };

    Vm::default().execute(&functions).unwrap();
}

#[test]
fn function_test() {
    let mut crunch = Crunch::new(crate::OptionBuilder::new("./function_test").build());

    let functions = bytecode! {
        0 => {
            load "Calling the function!\n", 31;
            print 31;
            load true, 0;
            func 1u32;
            load "Was the function called?", 31;
            load "\n", 30;
            print 31;
            print 0;
            print 30;
            ret;
        }
        1 => {
            load "\nThe function was called!", 0;
            print 0;
            ret;
        }
    };

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
                vec.push(Value::I32(rng.gen_range(0, i32::max_value())));
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
                    vec.push(Value::Str(Box::leak(string.into_boxed_str())));
                }
            }

            vec
        };

        let first_strings = thread::spawn(create_strings);
        let second_strings = thread::spawn(create_strings);

        // Add boolean values to test on
        vec.push(Value::Bool(true));
        vec.push(Value::Bool(false));
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
        let load = Instruction::Load(Box::new(val.clone()), 0.into());
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
            .is_equal(&Value::Bool(true), &vm.gc)
            .unwrap());

        // Set the previous comparison to `false`
        vm.prev_comp = false;
        comp_to_reg.execute(&mut vm).unwrap();
        assert!(vm.registers[0]
            .is_equal(&Value::Bool(false), &vm.gc)
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
        vm.registers[0] = Value::Str("test string"); // Load the register before hand
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
        vm.registers[0] = Value::Str("Test");
        print.execute(&mut vm).unwrap();
        swap_assert(&mut vm, "Test");

        vm.registers[0] = Value::I32(10);
        print.execute(&mut vm).unwrap();
        swap_assert(&mut vm, "10");

        vm.registers[0] = Value::F32(6.9);
        print.execute(&mut vm).unwrap();
        swap_assert(&mut vm, "6.9");

        vm.registers[0] = Value::Bool(true);
        print.execute(&mut vm).unwrap();
        swap_assert(&mut vm, "true");

        vm.registers[0] = Value::Bool(false);
        print.execute(&mut vm).unwrap();
        swap_assert(&mut vm, "false");

        // Test that writing to stdout works too, can only verify that it does, not that it is correct
        vm.stdout = Box::new(std::io::stdout());

        vm.registers[0] = Value::Str("Test");
        print.execute(&mut vm).unwrap();

        vm.registers[0] = Value::I32(10);
        print.execute(&mut vm).unwrap();

        vm.registers[0] = Value::Bool(true);
        print.execute(&mut vm).unwrap();

        vm.registers[0] = Value::Bool(false);
        print.execute(&mut vm).unwrap();
    }

    // TODO: Test printing
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
    assert_eq!(vm.index, 10.into());

    let jump = Instruction::Jump(-10);
    jump.execute(&mut vm).unwrap();
    assert_eq!(vm.index, 0.into());

    let jump_comp = Instruction::JumpComp(10);
    jump_comp.execute(&mut vm).unwrap();
    assert_eq!(vm.index, 1.into());

    vm.prev_comp = true;
    jump_comp.execute(&mut vm).unwrap();
    assert_eq!(vm.index, 11.into());

    let jump_comp = Instruction::JumpComp(-10);
    jump_comp.execute(&mut vm).unwrap();
    assert_eq!(vm.index, 1.into());
}

#[test]
fn incompatible_eq_ops() {
    let mut vm = Vm::new(
        &crate::OptionBuilder::new("./eq_ops").build(),
        Box::new(stdout()),
    );

    // test incompatible types
    vm.registers[0] = Value::Bool(true);
    vm.registers[1] = Value::I32(10);

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
            vm.registers[0] = Value::$internal($mid);
            vm.registers[1] = Value::$internal($lo);

            let greater_than = Instruction::GreaterThan(0.into(), 1.into());
            greater_than.execute(&mut vm).unwrap();
            assert_eq!(vm.prev_comp, true);

            let not_eq = Instruction::NotEq(0.into(), 1.into());
            not_eq.execute(&mut vm).unwrap();
            assert_eq!(vm.prev_comp, true);

            let lete = Instruction::LessThanEq(0.into(), 1.into());
            lete.execute(&mut vm).unwrap();
            assert_eq!(vm.prev_comp, false);

            vm.registers[1] = Value::$internal($mid);

            let grte = Instruction::GreaterThanEq(0.into(), 1.into());
            grte.execute(&mut vm).unwrap();
            assert_eq!(vm.prev_comp, true);

            let eq = Instruction::Eq(0.into(), 1.into());
            eq.execute(&mut vm).unwrap();
            assert_eq!(vm.prev_comp, true);

            vm.registers[1] = Value::$internal($hi);

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
