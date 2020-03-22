use crate::{compactor::Compactor, return_frame::ReturnFrame, value::Value, NUMBER_REGISTERS};

use alloc::{boxed::Box, format};
use core::mem;
use crunch_error::runtime_prelude::*;

pub fn load(mut vm: &mut Compactor, val: Value, reg: u8) -> RuntimeResult<()> {
    trace!(
        "Loading val {:?} into {}: {:?}",
        &val,
        reg,
        &vm.registers[reg as usize]
    );

    if vm.register_marked_read_only(reg) {
        Err(RuntimeError::new(
            RuntimeErrorTy::ReadOnlyRegister,
            format!("Attempted to write to read-only register {}", reg),
        ))
    } else {
        vm.registers[reg as usize] = val;
        vm.index += 1;

        Ok(())
    }
}

pub fn comp_to_reg(mut vm: &mut Compactor, reg: u8) -> RuntimeResult<()> {
    trace!("Loading previous comparison {} into {}", vm.prev_comp, reg);

    vm.registers[reg as usize] = Value::Bool(vm.prev_comp);
    vm.index += 1;

    Ok(())
}

pub fn op_to_reg(mut vm: &mut Compactor, reg: u8) -> RuntimeResult<()> {
    trace!("Loading previous operation {:?} into {}", &vm.prev_op, reg);

    vm.registers[reg as usize] = Value::None;
    mem::swap(&mut vm.registers[reg as usize], &mut vm.prev_op);
    vm.index += 1;

    Ok(())
}

pub fn drop(vm: &mut Compactor, reg: u8) -> RuntimeResult<()> {
    trace!(
        "Dropping register {}: {:?}",
        reg,
        &vm.registers[reg as usize]
    );

    let _dropped = vm.registers[reg as usize].take();
    vm.index += 1;

    Ok(())
}

pub fn mov(vm: &mut Compactor, target: u8, source: u8) -> RuntimeResult<()> {
    trace!(
        "Moving {}: {:?} to {}: {:?}",
        source,
        &vm.registers[source as usize],
        target,
        &vm.registers[target as usize]
    );

    vm.registers[target as usize] = vm.registers[source as usize].take();
    vm.index += 1;

    Ok(())
}

pub fn push(vm: &mut Compactor, reg: u8) -> RuntimeResult<()> {
    trace!(
        "Pushing register {}: {:?} to the stack",
        reg,
        &vm.registers[reg as usize]
    );

    vm.stack.push(vm.registers[reg as usize].take());
    vm.index += 1;

    Ok(())
}

pub fn pop(vm: &mut Compactor, reg: u8) -> RuntimeResult<()> {
    trace!("Popping to register {}: {:?}", reg, vm.stack.last());

    vm.registers[reg as usize] = vm.stack.pop().ok_or(RuntimeError::new(
        RuntimeErrorTy::EmptyStack,
        "Attempted to pop from the stack, but no values were available",
    ))?;
    vm.index += 1;

    Ok(())
}

pub fn add(mut vm: &mut Compactor, left: u8, right: u8) -> RuntimeResult<()> {
    trace!(
        "Adding registers {}: {:?} and {}: {:?} = {:?}",
        left,
        &vm.registers[left as usize],
        right,
        &vm.registers[right as usize],
        vm.registers[left as usize].add_upflowing(&vm.registers[right as usize])
    );

    vm.prev_op = vm.registers[left as usize].add_upflowing(&vm.registers[right as usize])?;
    vm.index += 1;

    Ok(())
}

pub fn sub(mut vm: &mut Compactor, left: u8, right: u8) -> RuntimeResult<()> {
    trace!(
        "Subtracting registers {}: {:?} and {}: {:?}",
        left,
        &vm.registers[left as usize],
        right,
        &vm.registers[right as usize]
    );

    vm.prev_op = vm.registers[left as usize].sub_upflowing(&vm.registers[right as usize])?;
    vm.index += 1;

    Ok(())
}

pub fn mult(mut vm: &mut Compactor, left: u8, right: u8) -> RuntimeResult<()> {
    trace!(
        "Multiplying registers {}: {:?} and {}: {:?}",
        left,
        &vm.registers[left as usize],
        right,
        &vm.registers[right as usize]
    );

    vm.prev_op = vm.registers[left as usize].mult_upflowing(&vm.registers[right as usize])?;
    vm.index += 1;

    Ok(())
}

pub fn div(mut vm: &mut Compactor, left: u8, right: u8) -> RuntimeResult<()> {
    trace!(
        "Dividing registers {}: {:?} and {}: {:?}",
        left,
        &vm.registers[left as usize],
        right,
        &vm.registers[right as usize]
    );

    vm.prev_op = vm.registers[left as usize].div_upflowing(&vm.registers[right as usize])?;
    vm.index += 1;

    Ok(())
}

pub fn print(vm: &mut Compactor, reg: u8) -> RuntimeResult<()> {
    trace!("Printing reg {:?}: {:?}", reg, &vm.registers[reg as usize]);

    if let Err(err) = write!(vm.stdout, "{}", vm.registers[reg as usize].to_string()) {
        error!("Error printing to stdout: {:?}", err);
        return Err(RuntimeError::new(
            RuntimeErrorTy::StdoutError,
            "Failed to print to stdout",
        ));
    }

    vm.index += 1;

    Ok(())
}

pub fn jump(vm: &mut Compactor, index: i32) -> RuntimeResult<()> {
    trace!(
        "Jumping by offset {} ({} + {} = {})",
        index,
        index,
        vm.index,
        vm.index as i32 + index
    );

    let index = if index.is_negative() {
        let (index, overflowed) = vm.index.overflowing_sub(index.abs() as u32);

        if overflowed {
            return Err(RuntimeError::new(
                RuntimeErrorTy::InvalidJump,
                "Jump overflowed",
            ));
        }

        index
    } else {
        vm.index + index as u32
    };

    vm.index = index;

    Ok(())
}

pub fn jump_comp(vm: &mut Compactor, index: i32) -> RuntimeResult<bool> {
    if vm.prev_comp {
        trace!(
            "Comparison Jump: Prev Comp is {}, jumping by {} ({} + {} = {})",
            vm.prev_comp,
            index,
            index,
            vm.index,
            vm.index as i32 + index
        );

        vm.index = (vm.index as i32 + index) as u32;
        Ok(true)
    } else {
        trace!(
            "Comparison Jump: Prev Comp is {}, not jumping",
            vm.prev_comp,
        );

        vm.index += 1;
        Ok(false)
    }
}

pub fn and(vm: &mut Compactor, left: u8, right: u8) -> RuntimeResult<()> {
    vm.prev_op = vm.registers[left as usize].bit_and(&vm.registers[right as usize])?;
    vm.index += 1;

    Ok(())
}

pub fn or(vm: &mut Compactor, left: u8, right: u8) -> RuntimeResult<()> {
    vm.prev_op = vm.registers[left as usize].bit_or(&vm.registers[right as usize])?;
    vm.index += 1;

    Ok(())
}

pub fn xor(vm: &mut Compactor, left: u8, right: u8) -> RuntimeResult<()> {
    vm.prev_op = vm.registers[left as usize].bit_xor(&vm.registers[right as usize])?;
    vm.index += 1;

    Ok(())
}

pub fn not(vm: &mut Compactor, reg: u8) -> RuntimeResult<()> {
    vm.prev_op = vm.registers[reg as usize].bit_not()?;
    vm.index += 1;

    Ok(())
}

macro_rules! comparison_operator {
    ( $( $op_name:ident: $( $compare:ident $(,)? )* => $found_bool:literal || $else_bool:literal $(,)? )* ) => {
        $(
        pub fn $op_name(vm: &mut Compactor, left: u8, right: u8) -> RuntimeResult<()> {
            use crate::value::Compare;

            trace!("Comparing {}: {:?} and {}: {:?} by {} = {:?}",
                left,
                &vm.registers[left as usize],
                right,
                &vm.registers[right as usize],
                stringify!($op_name),
                match &vm.registers[left as usize].compare(&vm.registers[right as usize])? {
                    $( Compare::$compare => Ok($found_bool), )*

                    // If Incomparable, emit an error
                    Compare::Incomparable  => Err(RuntimeError::new(RuntimeErrorTy::IncompatibleTypes,
                        format!(
                            concat!(
                                "Values of types '{}' and '{}' cannot be '",
                                stringify!($op_name),
                                "'ed",
                            ),
                            vm.registers[left as usize].name(),
                            vm.registers[right as usize].name(),
                        ))
                    ),

                    // All other variants are unequal
                    _ => Ok($else_bool),
                }
            );

            let left = &vm.registers[left as usize];
            let right = &vm.registers[right as usize];

            vm.prev_comp = match left.compare(right)? {
                $( Compare::$compare => Ok($found_bool), )*

                // If Incomparable, emit an error
                Compare::Incomparable => Err(RuntimeError::new(RuntimeErrorTy::IncompatibleTypes,
                    format!(
                        concat!(
                            "Values of types '{}' and '{}' cannot be '",
                            stringify!($op_name),
                            "'ed",
                        ),
                        left.name(),
                        right.name(),
                    ))
                ),

                // All other variants are unequal
                _ => Ok($else_bool),
            }?;
            vm.index += 1;

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

// TODO: Error on index out of bounds (Calling func that doesn't exist
// ^ Shouldn't the function already be verified to exist via compilation?
pub fn func(mut vm: &mut Compactor, func: u32) -> RuntimeResult<()> {
    trace!(
        "Jumping to function {} from function {}",
        func,
        vm.current_func
    );

    let mut registers: [Value; NUMBER_REGISTERS + 2] = array_init::array_init(|_| Value::None);
    mem::swap(&mut vm.registers, &mut registers);

    vm.return_stack.push(ReturnFrame {
        registers,
        index: vm.index + 1,
        function_index: vm.current_func,
        yield_point: None,
    });

    vm.index = 0;
    vm.current_func = func;

    Ok(())
}

pub fn yield_generator(vm: &mut Compactor) -> RuntimeResult<()> {
    trace!("Yielding from generator");

    let frame = if let Some(mut orig_frame) = vm.return_stack.pop() {
        trace!("Popping return frame");

        mem::swap(&mut vm.registers, &mut orig_frame.registers);
        let frame = ReturnFrame {
            registers: orig_frame.registers,
            index: vm.index + 1,
            function_index: vm.current_func,
            yield_point: Some(vm.index + 1),
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

pub fn call_generator(vm: &mut Compactor, func: u32, reg: u8) -> RuntimeResult<()> {
    trace!("Calling generator {} with reg {}", func, reg);

    match vm.registers[reg as usize].take() {
        Value::Generator(gen) => {
            // TODO: Use a box pattern when rust/#29641 stabilizes
            // https://github.com/rust-lang/rust/issues/29641
            let ReturnFrame {
                mut registers,
                index,
                function_index,
                ..
            } = *gen;

            // Store the current vm registers in `registers`, then push that to the return stack
            mem::swap(&mut registers, &mut vm.registers);

            vm.return_stack.push(ReturnFrame {
                registers,
                index: vm.index + 1,
                function_index: vm.current_func,
                yield_point: None,
            });

            trace!("Previously initialized generator");
            vm.index = index;
            vm.current_func = function_index;
        }
        Value::Null | Value::None => {
            // Store the current vm registers in `tmp_registers`, then push that to the
            // return stack
            let mut tmp_registers = array_init::array_init(|_| Value::None);
            mem::swap(&mut tmp_registers, &mut vm.registers);

            vm.return_stack.push(ReturnFrame {
                registers: tmp_registers,
                index: vm.index + 1,
                function_index: vm.current_func,
                yield_point: None,
            });

            trace!("Fresh generator");
            vm.index = 0;
            vm.current_func = func;
        }

        _ => todo!("Error"),
    }

    Ok(())
}

pub fn copy(vm: &mut Compactor, left: u8, right: u8) -> RuntimeResult<()> {
    trace!(
        "Copying {}: {:?} to {}: {:?}",
        left,
        &vm.registers[left as usize],
        right,
        &vm.registers[right as usize]
    );

    vm.registers[right as usize] = vm.registers[left as usize].clone();
    vm.index += 1;

    Ok(())
}

pub fn ret(vm: &mut Compactor) -> RuntimeResult<()> {
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

pub fn collect(vm: &mut Compactor) -> RuntimeResult<()> {
    trace!("Forcing a GC scavenge");

    // unsafe {
    //     crate::CRUNCH_ALLOCATOR.with(|alloc| alloc.get().as_mut().unwrap().scavenge());
    // }
    todo!();

    vm.index += 1;

    Ok(())
}

pub fn halt(vm: &mut Compactor) -> RuntimeResult<()> {
    vm.finished_execution = true;

    Ok(())
}

#[cfg(feature = "dll-ffi")]
pub fn load_lib(vm: &mut Vm, name: u8, target: u8) -> RuntimeResult<()> {
    use alloc::sync::Arc;
    use dlopen::raw::Library;

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

    vm.index += 1;

    Ok(())
}

#[cfg(feature = "dll-ffi")]
pub fn exec_lib_func(vm: &mut Vm, name: u8, lib: u8, args: u16) -> RuntimeResult<()> {
    use core::sync::Arc;

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

    let func: extern "C" fn(&mut Gc, &[Value]) -> RuntimeResult<Value> =
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
                message: "Attempted to pop from the stack, but no values were available",
            })?);
        }
        vec
    };

    vm.stack.push((func)(&mut vm.gc, &values)?);

    vm.index += 1;

    Ok(())
}

pub fn push_array(vm: &mut Compactor, value: u8, array: u8) -> RuntimeResult<()> {
    let value = vm.registers[value as usize].take();

    vm.index += 1;

    if let Value::Array(array) = &mut vm.registers[array as usize] {
        if value == Value::Null {
            Err(RuntimeError::new(
                RuntimeErrorTy::IncompatibleTypes,
                format!(
                    "Cannot array push a value of type {} to an array",
                    value.name(),
                ),
            ))
        } else {
            array.push(value);

            Ok(())
        }
    } else {
        Err(RuntimeError::new(
            RuntimeErrorTy::IncompatibleTypes,
            format!(
                "Cannot array push to a value of type {}",
                vm.registers[array as usize].name()
            ),
        ))
    }
}

pub fn pop_array(vm: &mut Compactor, value: u8, array: u8) -> RuntimeResult<()> {
    vm.index += 1;

    if let Value::Array(array) = &mut vm.registers[array as usize] {
        vm.registers[value as usize] = array.pop().unwrap_or(Value::Null);

        Ok(())
    } else {
        Err(RuntimeError::new(
            RuntimeErrorTy::IncompatibleTypes,
            format!(
                "Cannot array pop from a value of type {}",
                vm.registers[array as usize].name()
            ),
        ))
    }
}

pub fn index_array(vm: &mut Compactor, index: u8, array: u8, target: u8) -> RuntimeResult<()> {
    vm.index += 1;

    let index = vm.registers[index as usize]
        .to_usize()
        .ok_or(RuntimeError::new(
            RuntimeErrorTy::IncompatibleTypes,
            format!(
                "Cannot index an array with a value of type {}",
                vm.registers[index as usize].name()
            ),
        ))?;

    if let Value::Array(array) = &mut vm.registers[array as usize] {
        vm.registers[target as usize] = array[index].clone();

        Ok(())
    } else {
        Err(RuntimeError::new(
            RuntimeErrorTy::IncompatibleTypes,
            format!(
                "Cannot index into a value of type {}",
                vm.registers[array as usize].name()
            ),
        ))
    }
}

pub fn remove_array(vm: &mut Compactor, index: u8, array: u8, target: u8) -> RuntimeResult<()> {
    vm.index += 1;

    let index = vm.registers[index as usize]
        .to_usize()
        .ok_or(RuntimeError::new(
            RuntimeErrorTy::IncompatibleTypes,
            format!(
                "Cannot index an array with a value of type {}",
                vm.registers[index as usize].name()
            ),
        ))?;

    if let Value::Array(array) = &mut vm.registers[array as usize] {
        vm.registers[target as usize] = array.remove(index);

        Ok(())
    } else {
        Err(RuntimeError::new(
            RuntimeErrorTy::IncompatibleTypes,
            format!(
                "Cannot index into a value of type {}",
                vm.registers[array as usize].name()
            ),
        ))
    }
}

pub fn no_op(vm: &mut Compactor) -> RuntimeResult<()> {
    vm.index += 1;

    Ok(())
}

pub fn jump_point(vm: &mut Compactor) -> RuntimeResult<()> {
    vm.index += 1;

    Ok(())
}

pub fn illegal(_vm: &mut Compactor) -> RuntimeResult<()> {
    Err(RuntimeError::new(
        RuntimeErrorTy::IllegalInstruction,
        "Illegal Instruction",
    ))
}
