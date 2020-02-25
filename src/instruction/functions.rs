use crate::{Index, Result, ReturnFrame, RuntimeError, RuntimeErrorTy, Value, Vm};

pub fn load(mut vm: &mut Vm, val: Value, reg: u8) -> Result<()> {
    trace!(
        "Loading val {:?} into {}: {:?}",
        &val,
        reg,
        &vm.registers[reg as usize]
    );

    vm.registers[reg as usize] = val;
    vm.index += Index(1);

    Ok(())
}

pub fn comp_to_reg(mut vm: &mut Vm, reg: u8) -> Result<()> {
    trace!("Loading previous comparison {} into {}", vm.prev_comp, reg);

    vm.registers[reg as usize] = Value::Bool(vm.prev_comp);
    vm.index += Index(1);

    Ok(())
}

pub fn op_to_reg(mut vm: &mut Vm, reg: u8) -> Result<()> {
    trace!("Loading previous operation {:?} into {}", &vm.prev_op, reg);

    vm.registers[reg as usize] = Value::None;
    std::mem::swap(&mut vm.registers[reg as usize], &mut vm.prev_op);
    vm.index += Index(1);

    Ok(())
}

pub fn drop(vm: &mut Vm, reg: u8) -> Result<()> {
    trace!(
        "Dropping register {}: {:?}",
        reg,
        &vm.registers[reg as usize]
    );

    (&mut vm.registers[reg as usize]).drop(&vm.gc)?;
    vm.index += Index(1);

    Ok(())
}

pub fn mov(vm: &mut Vm, target: u8, source: u8) -> Result<()> {
    trace!(
        "Moving {}: {:?} to {}: {:?}",
        source,
        &vm.registers[source as usize],
        target,
        &vm.registers[target as usize]
    );

    vm.registers[target as usize] = vm.registers[source as usize].clone();
    vm.index += Index(1);

    Ok(())
}

pub fn push(vm: &mut Vm, reg: u8) -> Result<()> {
    trace!(
        "Pushing register {}: {:?} to the stack",
        reg,
        &vm.registers[reg as usize]
    );

    vm.stack.push(vm.registers[reg as usize].take());
    vm.index += Index(1);

    Ok(())
}

pub fn pop(vm: &mut Vm, reg: u8) -> Result<()> {
    trace!("Popping to register {}: {:?}", reg, vm.stack.last());

    vm.registers[reg as usize] = vm.stack.pop().ok_or(RuntimeError {
        ty: RuntimeErrorTy::EmptyStack,
        message: "Attempted to pop from the stack, but no values were available".to_string(),
    })?;
    vm.index += Index(1);

    Ok(())
}

pub fn add(mut vm: &mut Vm, left: u8, right: u8) -> Result<()> {
    trace!(
        "Adding registers {}: {:?} and {}: {:?} = {:?}",
        left,
        &vm.registers[left as usize],
        right,
        &vm.registers[right as usize],
        vm.registers[left as usize]
            .clone()
            .add_upflowing(vm.registers[right as usize].clone(), &mut vm.gc)
    );

    vm.prev_op = vm.registers[left as usize]
        .clone()
        .add_upflowing(vm.registers[right as usize].clone(), &mut vm.gc)?;
    vm.index += Index(1);

    Ok(())
}

pub fn sub(mut vm: &mut Vm, left: u8, right: u8) -> Result<()> {
    trace!(
        "Subtracting registers {}: {:?} and {}: {:?}",
        left,
        &vm.registers[left as usize],
        right,
        &vm.registers[right as usize]
    );

    vm.prev_op = vm.registers[left as usize]
        .clone()
        .sub_upflowing(vm.registers[right as usize].clone(), &mut vm.gc)?;
    vm.index += Index(1);

    Ok(())
}

pub fn mult(mut vm: &mut Vm, left: u8, right: u8) -> Result<()> {
    trace!(
        "Multiplying registers {}: {:?} and {}: {:?}",
        left,
        &vm.registers[left as usize],
        right,
        &vm.registers[right as usize]
    );

    vm.prev_op = vm.registers[left as usize]
        .clone()
        .mult_upflowing(vm.registers[right as usize].clone(), &mut vm.gc)?;
    vm.index += Index(1);

    Ok(())
}

pub fn div(mut vm: &mut Vm, left: u8, right: u8) -> Result<()> {
    trace!(
        "Dividing registers {}: {:?} and {}: {:?}",
        left,
        &vm.registers[left as usize],
        right,
        &vm.registers[right as usize]
    );

    vm.prev_op = vm.registers[left as usize]
        .clone()
        .div_upflowing(vm.registers[right as usize].clone(), &mut vm.gc)?;
    vm.index += Index(1);

    Ok(())
}

pub fn print(vm: &mut Vm, reg: u8) -> Result<()> {
    trace!("Printing reg {:?}: {:?}", reg, &vm.registers[reg as usize]);

    if let Err(err) = write!(
        vm.stdout,
        "{}",
        vm.registers[reg as usize].to_string(&vm.gc)?
    ) {
        error!("Error printing to stdout: {:?}", err);
        return Err(RuntimeError {
            ty: RuntimeErrorTy::StdoutError,
            message: "Failed to print to stdout".to_string(),
        });
    }

    vm.index += Index(1);

    Ok(())
}

pub fn jump(vm: &mut Vm, index: i32) -> Result<()> {
    trace!(
        "Jumping by offset {} ({} + {} = {})",
        index,
        index,
        *vm.index,
        *vm.index as i32 + index
    );

    let index = if index.is_negative() {
        let (index, overflowed) = vm.index.overflowing_sub(index.abs() as u32);

        if overflowed {
            return Err(RuntimeError {
                ty: RuntimeErrorTy::InvalidJump,
                message: "Jump overflowed".to_string(),
            });
        }

        index
    } else {
        *vm.index + index as u32
    };

    vm.index = Index(index);

    Ok(())
}

pub fn jump_comp(vm: &mut Vm, index: i32) -> Result<bool> {
    if vm.prev_comp {
        trace!(
            "Comparison Jump: Prev Comp is {}, jumping by {} ({} + {} = {})",
            vm.prev_comp,
            index,
            index,
            *vm.index,
            *vm.index as i32 + index
        );

        vm.index = Index((*vm.index as i32 + index) as u32);
        Ok(true)
    } else {
        trace!(
            "Comparison Jump: Prev Comp is {}, not jumping",
            vm.prev_comp,
        );

        vm.index += Index(1);
        Ok(false)
    }
}

pub fn and(vm: &mut Vm, left: u8, right: u8) -> Result<()> {
    vm.prev_op = vm.registers[left as usize]
        .clone()
        .bit_and(vm.registers[right as usize].clone(), &mut vm.gc)?;
    vm.index += Index(1);

    Ok(())
}

pub fn or(vm: &mut Vm, left: u8, right: u8) -> Result<()> {
    vm.prev_op = vm.registers[left as usize]
        .clone()
        .bit_or(vm.registers[right as usize].clone(), &mut vm.gc)?;
    vm.index += Index(1);

    Ok(())
}

pub fn xor(vm: &mut Vm, left: u8, right: u8) -> Result<()> {
    vm.prev_op = vm.registers[left as usize]
        .clone()
        .bit_xor(vm.registers[right as usize].clone(), &mut vm.gc)?;
    vm.index += Index(1);

    Ok(())
}

pub fn not(vm: &mut Vm, reg: u8) -> Result<()> {
    vm.prev_op = vm.registers[reg as usize].clone().bit_not(&mut vm.gc)?;
    vm.index += Index(1);

    Ok(())
}

macro_rules! comparison_operator {
    ( $( $op_name:ident: $( $compare:ident $(,)? )* => $found_bool:literal || $else_bool:literal $(,)? )* ) => {
        $(
        pub fn $op_name(vm: &mut Vm, left: u8, right: u8) -> Result<()> {
            use crate::value::Compare;

            trace!("Comparing {}: {:?} and {}: {:?} by {} = {:?}",
                left,
                &vm.registers[left as usize],
                right,
                &vm.registers[right as usize],
                stringify!($op_name),
                match &vm.registers[left as usize].compare(&vm.registers[right as usize], &vm.gc)? {
                    $( Compare::$compare => Ok($found_bool), )*
                    Compare::Incomparable if !vm.options.fault_tolerant => Err(RuntimeError {
                        ty: RuntimeErrorTy::IncompatibleTypes,
                        message: format!(
                            concat!(
                                "Values of types '{}' and '{}' cannot be '",
                                stringify!($op_name),
                                "'ed",
                            ),
                            vm.registers[left as usize].name(),
                            vm.registers[right as usize].name()
                        ),
                    }),
                    _ => Ok($else_bool),
                }
            );

            let left = &vm.registers[left as usize];
            let right = &vm.registers[right as usize];

            vm.prev_comp = match left.compare(right, &vm.gc)? {
                $( Compare::$compare => Ok($found_bool), )*
                Compare::Incomparable if !vm.options.fault_tolerant => Err(RuntimeError {
                    ty: RuntimeErrorTy::IncompatibleTypes,
                    message: format!(
                        concat!(
                            "Values of types '{}' and '{}' cannot be '",
                            stringify!($op_name),
                            "'ed",
                        ),
                        left.name(),
                        right.name()
                    ),
                }),
                _ => Ok($else_bool),
            }?;
            vm.index += Index(1);

            Ok(())
        } )*
    };
}

comparison_operator! {
    eq:                 Equal           => true  || false,
    not_eq:             Equal           => false || true,
    greater_than:       Greater         => true  || false,
    less_than:          Less            => true  || false,
    greater_than_equal: Greater, Equal  => true  || false,
    less_than_equal:    Less, Equal     => true  || false
}

// TODO: Error on index out of bounds (Calling func that doesn't exist)
pub fn func(mut vm: &mut Vm, func: u32) -> Result<()> {
    trace!(
        "Jumping to function {} from function {}",
        func,
        vm.current_func
    );

    let mut registers: [Value; crate::NUMBER_REGISTERS] = array_init::array_init(|_| Value::None);
    std::mem::swap(&mut vm.registers, &mut registers);

    vm.return_stack.push(ReturnFrame {
        registers,
        index: vm.index + Index(1),
        function_index: vm.current_func,
        yield_point: None,
    });

    vm.index = Index(0);
    vm.current_func = func;

    Ok(())
}

pub fn yield_generator(vm: &mut Vm) -> Result<()> {
    trace!("Yielding from generator");

    let frame = if let Some(mut orig_frame) = vm.return_stack.pop() {
        trace!("Popping return frame");

        std::mem::swap(&mut vm.registers, &mut orig_frame.registers);
        let frame = ReturnFrame {
            registers: orig_frame.registers,
            index: vm.index + Index(1),
            function_index: vm.current_func,
            yield_point: Some(vm.index + Index(1)),
        };
        vm.index = orig_frame.index;
        vm.current_func = orig_frame.function_index;

        frame
    } else {
        error!("Returned with no frames from a generator");
        todo!("Is this an error?")
    };

    vm.stack.push(Value::Generator(Box::new(frame)));

    Ok(())
}

pub fn call_generator(vm: &mut Vm, func: u32, reg: u8) -> Result<()> {
    trace!("Calling generator {} with reg {}", func, reg);

    vm.return_stack.push(ReturnFrame {
        registers: vm.registers.clone(),
        index: vm.index + Index(1),
        function_index: vm.current_func,
        yield_point: None,
    });

    if let Value::Generator(gen) = vm.registers[reg as usize].clone() {
        trace!("Previously initialized generator");
        vm.index = gen.index;
        vm.registers = gen.registers;
        vm.current_func = gen.function_index;
    } else if vm.registers[reg as usize] == Value::Null || vm.registers[reg as usize] == Value::None
    {
        trace!("Fresh generator");
        vm.index = Index(0);
        vm.current_func = func;
    } else {
        todo!("Error")
    }

    Ok(())
}

pub fn copy(vm: &mut Vm, left: u8, right: u8) -> Result<()> {
    trace!(
        "Copying {}: {:?} to {}: {:?}",
        left,
        &vm.registers[left as usize],
        right,
        &vm.registers[right as usize]
    );

    vm.registers[right as usize] = vm.registers[left as usize].clone();
    vm.index += Index(1);

    Ok(())
}

pub fn ret(vm: &mut Vm) -> Result<()> {
    trace!("Executing a Return");

    // Get the most recent return frame
    if let Some(frame) = vm.return_stack.pop() {
        trace!("Popping return frame");

        // Set the important stuff from the frame
        vm.index = frame.index;
        vm.registers = frame.registers;
        vm.current_func = frame.function_index;

    // If there are no further stack frames, then return to main
    } else {
        info!("Returning with no return frames left, halting program");

        vm.finished_execution = true;
    }

    Ok(())
}

pub fn collect(vm: &mut Vm) -> Result<()> {
    trace!("Forcing a GC collect");

    vm.gc.collect();
    vm.index += Index(1);

    Ok(())
}

pub fn halt(vm: &mut Vm) -> Result<()> {
    vm.finished_execution = true;

    Ok(())
}

pub fn load_lib(vm: &mut Vm, name: u8, target: u8) -> Result<()> {
    use dlopen::raw::Library;
    use std::sync::Arc;

    let lib = if let Value::Str(name) = vm.registers[name as usize] {
        Library::open(name)
    } else if let Value::GcString(heap) = vm.registers[name as usize] {
        Library::open(&heap.fetch(&vm.gc)?)
    } else {
        return Err(RuntimeError {
            ty: RuntimeErrorTy::IncompatibleTypes,
            message: format!(
                "Expected `str`, got `{}`",
                vm.registers[name as usize].name()
            ),
        });
    }
    .map_err(|e| {
        error!("Failed to load dll: {:?}", e);
        RuntimeError {
            ty: RuntimeErrorTy::MissingFile,
            message: format!("{:?}", e),
        }
    })?;

    vm.registers[target as usize] = Value::Library(Arc::new(lib));

    vm.index += Index(1);

    Ok(())
}

pub fn exec_lib_func(vm: &mut Vm, name: u8, lib: u8, args: u16) -> Result<()> {
    use std::sync::Arc;

    let lib = if let Value::Library(ref lib) = vm.registers[lib as usize] {
        Arc::clone(lib)
    } else {
        return Err(RuntimeError {
            ty: RuntimeErrorTy::IncompatibleTypes,
            message: format!(
                "Expected `lib`, got `{}`",
                vm.registers[name as usize].name()
            ),
        });
    };

    let func: extern "C" fn(&mut crate::Gc, &[Value]) -> Result<Value> =
        if let Value::Str(name) = vm.registers[name as usize] {
            unsafe { (*lib).symbol(name) }
        } else if let Value::GcString(heap) = vm.registers[name as usize] {
            unsafe { (*lib).symbol(&heap.fetch(&vm.gc)?) }
        } else {
            return Err(RuntimeError {
                ty: RuntimeErrorTy::IncompatibleTypes,
                message: format!(
                    "Expected `str`, got `{}`",
                    vm.registers[name as usize].name()
                ),
            });
        }
        .map_err(|e| {
            error!("Failed to load dll: {:?}", e);
            RuntimeError {
                ty: RuntimeErrorTy::MissingFile,
                message: format!("{:?}", e),
            }
        })?;

    let values = {
        let mut vec = Vec::with_capacity(args as usize);
        for _ in 0..args {
            vec.push(vm.stack.pop().ok_or(RuntimeError {
                ty: RuntimeErrorTy::EmptyStack,
                message:
                    "Attempted to pop from the stack, but no values were available".to_string(),
            })?);
        }
        vec
    };

    vm.stack.push((func)(&mut vm.gc, &values)?);

    vm.index += Index(1);

    Ok(())
}

pub fn push_array(vm: &mut Vm, value: u8, array: u8) -> Result<()> {
    let value = vm.registers[value as usize].take();

    vm.index += Index(1);

    if let Value::Array(array) = &mut vm.registers[array as usize] {
        if value == Value::Null {
            Err(RuntimeError {
                ty: RuntimeErrorTy::IncompatibleTypes,
                message: format!(
                    "Cannot array push a value of type {} to an array",
                    value.name(),
                ),
            })
        } else {
            array.push(value);

            Ok(())
        }
    } else {
        Err(RuntimeError {
            ty: RuntimeErrorTy::IncompatibleTypes,
            message: format!(
                "Cannot array push to a value of type {}",
                vm.registers[array as usize].name()
            ),
        })
    }
}

pub fn pop_array(vm: &mut Vm, value: u8, array: u8) -> Result<()> {
    vm.index += Index(1);

    if let Value::Array(array) = &mut vm.registers[array as usize] {
        vm.registers[value as usize] = array.pop().unwrap_or(Value::Null);

        Ok(())
    } else {
        Err(RuntimeError {
            ty: RuntimeErrorTy::IncompatibleTypes,
            message: format!(
                "Cannot array pop from a value of type {}",
                vm.registers[array as usize].name()
            ),
        })
    }
}

pub fn index_array(vm: &mut Vm, index: u8, array: u8, target: u8) -> Result<()> {
    vm.index += Index(1);

    let index = vm.registers[index as usize]
        .to_usize(&vm.gc)
        .ok_or(RuntimeError {
            ty: RuntimeErrorTy::IncompatibleTypes,
            message: format!(
                "Cannot index an array with a value of type {}",
                vm.registers[index as usize].name()
            ),
        })?;

    if let Value::Array(array) = &mut vm.registers[array as usize] {
        vm.registers[target as usize] = array[index].clone();

        Ok(())
    } else {
        Err(RuntimeError {
            ty: RuntimeErrorTy::IncompatibleTypes,
            message: format!(
                "Cannot index into a value of type {}",
                vm.registers[array as usize].name()
            ),
        })
    }
}

pub fn remove_array(vm: &mut Vm, index: u8, array: u8, target: u8) -> Result<()> {
    vm.index += Index(1);

    let index = vm.registers[index as usize]
        .to_usize(&vm.gc)
        .ok_or(RuntimeError {
            ty: RuntimeErrorTy::IncompatibleTypes,
            message: format!(
                "Cannot index an array with a value of type {}",
                vm.registers[index as usize].name()
            ),
        })?;

    if let Value::Array(array) = &mut vm.registers[array as usize] {
        vm.registers[target as usize] = array.remove(index);

        Ok(())
    } else {
        Err(RuntimeError {
            ty: RuntimeErrorTy::IncompatibleTypes,
            message: format!(
                "Cannot index into a value of type {}",
                vm.registers[array as usize].name()
            ),
        })
    }
}

pub fn no_op(vm: &mut Vm) -> Result<()> {
    vm.index += Index(1);

    Ok(())
}

pub fn jump_point(vm: &mut Vm) -> Result<()> {
    vm.index += Index(1);

    Ok(())
}

pub fn illegal(_vm: &mut Vm) -> Result<()> {
    Err(RuntimeError {
        ty: RuntimeErrorTy::IllegalInstruction,
        message: "Illegal Instruction".to_string(),
    })
}
