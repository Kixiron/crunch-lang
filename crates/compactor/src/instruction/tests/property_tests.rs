/// Creates integer proptests
///
/// # Parameters
/// * `value_variant`: The variant of the `Value` enum
/// * `primitive`: The primitive type contained by the `Value` variant
/// * `bitwise`: Optional, sets what bitwise operands can be used with that `Value` variant
///
/// [`Value`]: crate.Value
macro_rules! number_proptest {
    ( $( $mod_name:ident { internal: $value_variant:ident, primitive: $primitive:ty, unsigned: $unsigned:literal $( , bitwise: $bitwise:ident )? } ),* ) => {
        $(
            mod $mod_name {
                use super::super::super::*;
                use proptest::prelude::*;

                $(
                    mod $bitwise {
                        use super::super::super::*;
                        use proptest::prelude::*;

                        proptest! {
                            #[test]
                            fn bitwise_and(left: $primitive, right: $primitive) {
                                if $unsigned {
                                    prop_assume!(left >= right)
                                }

                                let mut stdout = std::io::stdout();
                                let mut vm = Compactor::with_stdout(Box::new(&mut stdout));

                                vm.registers[0] = Value::$value_variant(left);
                                vm.registers[1] = Value::$value_variant(right);

                                let and = Instruction::And(0.into(), 1.into());
                                and.execute(&mut vm).unwrap();

                                assert!(
                                    vm
                                        .prev_op
                                        .is_equal(&Value::$value_variant(left & right))
                                        .unwrap()
                                );
                            }

                            #[test]
                            fn bitwise_or(left: $primitive, right: $primitive) {
                                if $unsigned {
                                    prop_assume!(left >= right)
                                }

                                let mut stdout = std::io::stdout();
                                let mut vm = Compactor::with_stdout(Box::new(&mut stdout));

                                vm.registers[0] = Value::$value_variant(left);
                                vm.registers[1] = Value::$value_variant(right);

                                let or = Instruction::Or(0.into(), 1.into());
                                or.execute(&mut vm).unwrap();

                                assert!(
                                    vm
                                        .prev_op
                                        .is_equal(&Value::$value_variant(left | right))
                                        .unwrap()
                                );
                            }

                            #[test]
                            fn bitwise_xor(left: $primitive, right: $primitive) {
                                if $unsigned {
                                    prop_assume!(left >= right)
                                }

                                let mut stdout = std::io::stdout();
                                let mut vm = Compactor::with_stdout(Box::new(&mut stdout));

                                vm.registers[0] = Value::$value_variant(left);
                                vm.registers[1] = Value::$value_variant(right);

                                let xor = Instruction::Xor(0.into(), 1.into());
                                xor.execute(&mut vm).unwrap();

                                assert!(
                                    vm
                                        .prev_op
                                        .is_equal(&Value::$value_variant(left ^ right))
                                        .unwrap()
                                );
                            }

                            #[test]
                            fn bitwise_not(int: $primitive) {
                                let mut stdout = std::io::stdout();
                                let mut vm = Compactor::with_stdout(Box::new(&mut stdout));

                                vm.registers[0] = Value::$value_variant(int);

                                let not = Instruction::Not(0.into());
                                not.execute(&mut vm).unwrap();

                                assert!(
                                    vm
                                        .prev_op
                                        .is_equal(&Value::$value_variant(!int))
                                        .unwrap()
                                );
                            }
                        }
                    }
                )?

                proptest! {
                    #[test]
                    fn add(left: $primitive, right: $primitive) {
                        if $unsigned {
                            prop_assume!(left >= right)
                        }

                        let mut stdout = std::io::stdout();
                        let mut vm = Compactor::with_stdout(Box::new(&mut stdout));

                        vm.registers[0] = Value::$value_variant(left);
                        vm.registers[1] = Value::$value_variant(right);

                        let add = Instruction::Add(0.into(), 1.into());
                        add.execute(&mut vm).unwrap();

                        assert!(
                            vm
                                .prev_op
                                .is_equal(
                                    &Value::$value_variant(left)
                                        .add_upflowing(&Value::$value_variant(right))
                                        .unwrap()
                                )
                                .unwrap()
                        );
                    }

                    #[test]
                    fn subtract(left: $primitive, right: $primitive) {
                        if $unsigned {
                            prop_assume!(left >= right)
                        }

                        let mut stdout = std::io::stdout();
                        let mut vm = Compactor::with_stdout(Box::new(&mut stdout));

                        vm.registers[0] = Value::$value_variant(left);
                        vm.registers[1] = Value::$value_variant(right);

                        let sub = Instruction::Sub(0.into(), 1.into());
                        sub.execute(&mut vm).unwrap();

                        assert!(
                            vm
                                .prev_op
                                .is_equal(
                                    &Value::$value_variant(left)
                                        .sub_upflowing(&Value::$value_variant(right))
                                        .unwrap(),
                                )
                                .unwrap()
                        );
                    }

                    #[test]
                    fn multiply(left: $primitive, right: $primitive) {
                        if $unsigned {
                            prop_assume!(left >= right)
                        }

                        let mut stdout = std::io::stdout();
                        let mut vm = Compactor::with_stdout(Box::new(&mut stdout));

                        vm.registers[0] = Value::$value_variant(left);
                        vm.registers[1] = Value::$value_variant(right);

                        let mult = Instruction::Mult(0.into(), 1.into());
                        mult.execute(&mut vm).unwrap();

                        assert!(
                            vm
                                .prev_op
                                .is_equal(
                                    &Value::$value_variant(left)
                                        .mult_upflowing(&Value::$value_variant(right))
                                        .unwrap(),
                                )
                                .unwrap()
                        );
                    }

                    #[test]
                    fn divide(left: $primitive, right: $primitive) {
                        if $unsigned {
                            prop_assume!(left >= right)
                        }

                        let mut stdout = std::io::stdout();
                        let mut vm = Compactor::with_stdout(Box::new(&mut stdout));

                        vm.registers[0] = Value::$value_variant(left);
                        vm.registers[1] = Value::$value_variant(right);

                        let div = Instruction::Div(0.into(), 1.into());
                        div.execute(&mut vm).unwrap();

                        assert!(
                            vm.prev_op
                                .is_equal(
                                    &Value::$value_variant(left)
                                        .div_upflowing(&Value::$value_variant(right))
                                        .unwrap(),
                                )
                                .unwrap()
                        );
                    }
                }
            }
        )*
    };
}

/*
#[test]
fn collect(int: i32, string in "\\PC*") {
    TODO: Rework this
    let mut vm = Vm::new(
        &crate::OptionBuilder::new("./misc_ops").build(),
        Box::new(stdout()),
    );

    let collect = Instruction::Collect;

    let discard = vm.gc.alloc(std::mem::size_of::<Value>()).unwrap();
    unsafe {
        vm.gc
            .write(discard, &<Value as Into<Vec<u8>>>::into(Value::I32(int)))
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
}*/

number_proptest! {
    u8_ops {
        internal: Byte,
        primitive: u8,
        unsigned: true,
        bitwise: bitwise
    },
    i8_ops {
        internal: IByte,
        primitive: i8,
        unsigned: false,
        bitwise: bitwise
    },
    i16_ops {
        internal: I16,
        primitive: i16,
        unsigned: false,
        bitwise: bitwise
    },
    i32_ops {
        internal: I32,
        primitive: i32,
        unsigned: false,
        bitwise: bitwise
    },
    i64_ops {
        internal: I64,
        primitive: i64,
        unsigned: false,
        bitwise: bitwise
    },

    f32_ops {
        internal: F32,
        primitive: f32,
        unsigned: false
    },
    f64_ops {
        internal: F64,
        primitive: f64,
        unsigned: false
    }
}
