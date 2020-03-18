mod property_tests;

use super::*;
use crate::{bytecode, Compactor};

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

    let mut stdout = std::io::stdout();
    Compactor::with_stdout(Box::new(&mut stdout))
        .execute(&functions)
        .unwrap();
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

    let mut stdout = std::io::stdout();
    Compactor::with_stdout(Box::new(&mut stdout))
        .execute(&functions)
        .unwrap();
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

    let mut stdout = std::io::stdout();
    Compactor::with_stdout(Box::new(&mut stdout))
        .execute(&functions)
        .unwrap();
}

#[test]
fn function_test() {
    let mut stdout = std::io::stdout();
    let mut crunch = Compactor::with_stdout(Box::new(&mut stdout));

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

/* FIXME: This testing is broken by the changes to Compactor's stdout
#[test]
fn print_op() {
    let print = Instruction::Print(0.into());
    let mut stdout = Vec::<u8>::new();
    let vm = Compactor::with_stdout(Box::new(&mut stdout));

    {
        vm.registers[0] = Value::Str("Test");
        print.execute(&mut vm).unwrap();
        assert!(u&mt vm, "Test");

        vm.registers[0] = Value::I32(10);
        print.execute(&mut vm).unwrap();
        swap_assert(&mut vm, "10");

        vm.registers[0] = Value::F32(6.9);
        print.execute(&mut vm).unwrap();
        swap_assert(&mut vm, "6.9");

        vm.registers[0] = Value::Bool(true);
        print.execute(&mut vm).unwrap();
        swap_assert(&mut vm, "true");
    }

    // TODO: Test that correct values are printed
}
*/

#[test]
fn jump_ops() {
    let mut stdout = std::io::stdout();
    let mut vm = Compactor::with_stdout(Box::new(&mut stdout));

    // Each executed instruction increments the index by one, so take that into account

    let jump = Instruction::Jump(10);
    jump.execute(&mut vm).unwrap();
    assert_eq!(vm.index, 10);

    let jump = Instruction::Jump(-10);
    jump.execute(&mut vm).unwrap();
    assert_eq!(vm.index, 0);

    let jump_comp = Instruction::JumpComp(10);
    jump_comp.execute(&mut vm).unwrap();
    assert_eq!(vm.index, 1);

    vm.prev_comp = true;
    jump_comp.execute(&mut vm).unwrap();
    assert_eq!(vm.index, 11);

    let jump_comp = Instruction::JumpComp(-10);
    jump_comp.execute(&mut vm).unwrap();
    assert_eq!(vm.index, 1);
}

#[test]
fn incompatible_eq_ops() {
    let mut stdout = std::io::stdout();
    let mut vm = Compactor::with_stdout(Box::new(&mut stdout));

    // test incompatible types
    vm.registers[0] = Value::Bool(true);
    vm.registers[1] = Value::I32(10);

    let less_than = Instruction::LessThan(0.into(), 1.into());
    assert_eq!(
        less_than.execute(&mut vm).unwrap_err().message(),
        "Values of types 'bool' and 'int' cannot be 'less_than'ed".to_string(),
    );
}

macro_rules! test_eq_ops {
    ( $( $fn_name:ident { internal: $internal:tt , hi: $hi:literal, mid: $mid:literal, lo: $lo:literal } $(,)? )* ) => { $(
        #[test]
        fn $fn_name() {
            let mut stdout = std::io::stdout();
            let mut vm = Compactor::with_stdout(Box::new(&mut stdout));

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

// TODO: Add Byte and IByte
test_eq_ops! {
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
    let mut stdout = std::io::stdout();
    let mut vm = Compactor::with_stdout(Box::new(&mut stdout));

    let illegal = Instruction::Illegal;
    assert_eq!(
        illegal.execute(&mut vm).err().unwrap().ty(),
        RuntimeErrorTy::IllegalInstruction
    );
}
