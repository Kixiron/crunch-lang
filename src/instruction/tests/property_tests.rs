use super::super::*;
use proptest::prelude::*;
use std::io::stdout;

macro_rules! number_proptest {
    ( $( $mod_name:ident [ $value_variant:ident, $primitive:ty ] $( $bitwise:ident )? , )* ) => {$(
        mod $mod_name {
            use super::super::super::*;
            use proptest::prelude::*;
            use std::io::stdout;

            $(mod $bitwise {
                use super::super::super::*;
                use proptest::prelude::*;
                use std::io::stdout;

                proptest! {
                    #[test]
                    fn bitwise_and(left: $primitive, right: $primitive) {
                        let mut vm = Vm::new(
                            &crate::OptionBuilder::new("./bitwise_ops").build(),
                            Box::new(stdout()),
                        );

                        vm.registers[0] = RuntimeValue::$value_variant(left);
                        vm.registers[1] = RuntimeValue::$value_variant(right);

                        let and = Instruction::And(0.into(), 1.into());
                        and.execute(&mut vm).unwrap();

                        assert!(
                            vm
                                .prev_op
                                .is_equal(
                                    &RuntimeValue::$value_variant(left & right),
                                    &vm.gc
                                )
                                .unwrap()
                        );
                    }

                    #[test]
                    fn bitwise_or(left: $primitive, right: $primitive) {
                        let mut vm = Vm::new(
                            &crate::OptionBuilder::new("./bitwise_ops").build(),
                            Box::new(stdout()),
                        );

                        vm.registers[0] = RuntimeValue::$value_variant(left);
                        vm.registers[1] = RuntimeValue::$value_variant(right);

                        let or = Instruction::Or(0.into(), 1.into());
                        or.execute(&mut vm).unwrap();

                        assert!(
                            vm
                                .prev_op
                                .is_equal(
                                    &RuntimeValue::$value_variant(left | right),
                                    &vm.gc
                                )
                                .unwrap()
                        );
                    }

                    #[test]
                    fn bitwise_xor(left: $primitive, right: $primitive) {
                        let mut vm = Vm::new(
                            &crate::OptionBuilder::new("./bitwise_ops").build(),
                            Box::new(stdout()),
                        );

                        vm.registers[0] = RuntimeValue::$value_variant(left);
                        vm.registers[1] = RuntimeValue::$value_variant(right);

                        let xor = Instruction::Xor(0.into(), 1.into());
                        xor.execute(&mut vm).unwrap();

                        assert!(
                            vm
                                .prev_op
                                .is_equal(
                                    &RuntimeValue::$value_variant(left ^ right),
                                    &vm.gc
                                )
                                .unwrap()
                        );
                    }

                    #[test]
                    fn bitwise_not(int: $primitive) {
                        let mut vm = Vm::new(
                            &crate::OptionBuilder::new("./bitwise_ops").build(),
                            Box::new(stdout()),
                        );

                        vm.registers[0] = RuntimeValue::$value_variant(int);

                        let not = Instruction::Not(0.into());
                        not.execute(&mut vm).unwrap();

                        assert!(
                            vm
                                .prev_op
                                .is_equal(
                                    &RuntimeValue::$value_variant(!int),
                                    &vm.gc
                                )
                                .unwrap()
                        );
                    }
                }
            })?

            proptest! {

                #[test]
                fn add(left: $primitive, right: $primitive) {
                    let mut vm = Vm::new(
                        &crate::OptionBuilder::new("./math_ops").build(),
                        Box::new(stdout()),
                    );

                    vm.registers[0] = RuntimeValue::$value_variant(left);
                    vm.registers[1] = RuntimeValue::$value_variant(right);

                    let add = Instruction::Add(0.into(), 1.into());
                    add.execute(&mut vm).unwrap();

                    assert!(
                        vm
                            .prev_op
                            .is_equal(
                                &RuntimeValue::$value_variant(left)
                                    .add_upflowing(
                                        RuntimeValue::$value_variant(right),
                                        &mut vm.gc
                                    )
                                    .unwrap(),
                                &vm.gc
                            )
                            .unwrap()
                    );
                }

                #[test]
                fn subtract(left: $primitive, right: $primitive) {
                    let mut vm = Vm::new(
                        &crate::OptionBuilder::new("./math_ops").build(),
                        Box::new(stdout()),
                    );

                    vm.registers[0] = RuntimeValue::$value_variant(left);
                    vm.registers[1] = RuntimeValue::$value_variant(right);

                    let sub = Instruction::Sub(0.into(), 1.into());
                    sub.execute(&mut vm).unwrap();

                    assert!(
                        vm
                            .prev_op
                            .is_equal(
                                &RuntimeValue::$value_variant(left)
                                    .sub_upflowing(
                                        RuntimeValue::$value_variant(right),
                                        &mut vm.gc
                                    )
                                    .unwrap(),
                                &vm.gc
                            )
                            .unwrap()
                    );
                }

                #[test]
                fn multiply(left: $primitive, right: $primitive) {
                    let mut vm = Vm::new(
                        &crate::OptionBuilder::new("./math_ops").build(),
                        Box::new(stdout()),
                    );

                    vm.registers[0] = RuntimeValue::$value_variant(left);
                    vm.registers[1] = RuntimeValue::$value_variant(right);

                    let mult = Instruction::Mult(0.into(), 1.into());
                    mult.execute(&mut vm).unwrap();

                    assert!(
                        vm
                            .prev_op
                            .is_equal(
                                &RuntimeValue::$value_variant(left)
                                    .mult_upflowing(
                                        RuntimeValue::$value_variant(right),
                                        &mut vm.gc
                                    )
                                    .unwrap(),
                                &vm.gc
                            )
                            .unwrap()
                    );
                }

                #[test]
                fn divide(left: $primitive, right: $primitive) {
                    let mut vm = Vm::new(
                        &crate::OptionBuilder::new("./math_ops").build(),
                        Box::new(stdout()),
                    );

                    vm.registers[0] = RuntimeValue::$value_variant(left);
                    vm.registers[1] = RuntimeValue::$value_variant(right);

                    let div = Instruction::Div(0.into(), 1.into());
                    div.execute(&mut vm).unwrap();

                    assert!(
                        vm
                            .prev_op
                            .is_equal(
                                &RuntimeValue::$value_variant(left)
                                    .div_upflowing(
                                        RuntimeValue::$value_variant(right),
                                        &mut vm.gc
                                    )
                                    .unwrap(),
                                &vm.gc
                            )
                            .unwrap()
                    );
                }
            }
        }
    )*}
}

/*
#[test]
fn collect(int: i32, string in "\\PC*") {
    /*
    TODO: Rework this
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
    vm.gc.remove_root(discard).unwrap();

    collect.execute(&mut vm).unwrap();
    assert!(!vm.gc.contains(discard));

    let gc_str = crate::GcStr::new(&string, &mut vm.gc).unwrap();

    assert!(vm.gc.contains(gc_str.id));
    assert!(gc_str.to_str(&vm.gc) == Ok(&string));

    gc_str.drop(&mut vm.gc).unwrap();
    collect.execute(&mut vm).unwrap();
    assert!(!vm.gc.contains(gc_str.id));
    */
}*/

number_proptest! {
    u16_ops [U16, u16] bitwise,
    u32_ops [U32, u32] bitwise,
    u64_ops [U64, u64] bitwise,
    u128_ops [U128, u128] bitwise,

    i16_ops [I16, i16] bitwise,
    i32_ops [I32, i32] bitwise,
    i64_ops [I64, i64] bitwise,
    i128_ops [I128, i128] bitwise,

    f32_ops [F32, f32],
    f64_ops [F64, f64],
}
