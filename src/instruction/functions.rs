use crate::{
    Index, Result, ReturnFrame, RuntimeError, RuntimeErrorTy, RuntimeValue, Vm, NUMBER_REGISTERS,
};

pub fn load(mut vm: &mut Vm, val: RuntimeValue, reg: u8) -> Result<()> {
    trace!("Loading val into {}", reg);

    vm.registers[reg as usize] = val;
    vm.index += Index(1);

    Ok(())
}

pub fn comp_to_reg(mut vm: &mut Vm, reg: u8) -> Result<()> {
    trace!("Loading previous comparison into {}", reg);

    vm.registers[reg as usize] = RuntimeValue::Bool(vm.prev_comp);
    vm.index += Index(1);

    Ok(())
}

pub fn op_to_reg(mut vm: &mut Vm, reg: u8) -> Result<()> {
    trace!(
        "Loading previous operation ({:?}) into {}",
        &vm.prev_op,
        reg
    );

    vm.registers[reg as usize] = RuntimeValue::None;
    std::mem::swap(&mut vm.registers[reg as usize], &mut vm.prev_op);
    vm.index += Index(1);

    Ok(())
}

pub fn drop_reg(vm: &mut Vm, reg: u8) -> Result<()> {
    trace!("Clearing register {}", reg);

    vm.registers[reg as usize].drop(&mut vm.gc)?;
    vm.index += Index(1);

    Ok(())
}

pub fn add(mut vm: &mut Vm, left: u8, right: u8) -> Result<()> {
    println!("here");
    vm.prev_op =
        vm.registers[left as usize].add_upflowing(vm.registers[right as usize], &mut vm.gc)?;
    vm.index += Index(1);

    Ok(())
}

pub fn sub(mut vm: &mut Vm, left: u8, right: u8) -> Result<()> {
    println!("here");
    vm.prev_op =
        vm.registers[left as usize].sub_upflowing(vm.registers[right as usize], &mut vm.gc)?;
    vm.index += Index(1);

    Ok(())
}

pub fn mult(mut vm: &mut Vm, left: u8, right: u8) -> Result<()> {
    println!("here");
    vm.prev_op =
        vm.registers[left as usize].mult_upflowing(vm.registers[right as usize], &mut vm.gc)?;
    vm.index += Index(1);

    Ok(())
}

pub fn div(mut vm: &mut Vm, left: u8, right: u8) -> Result<()> {
    println!("here");
    vm.prev_op =
        vm.registers[left as usize].div_upflowing(vm.registers[right as usize], &mut vm.gc)?;
    vm.index += Index(1);

    Ok(())
}

pub fn print(vm: &mut Vm, reg: u8) -> Result<()> {
    trace!("Printing reg {:?}", reg);

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
        *vm.index + index as u32 + 1
    };

    vm.index = Index(index);

    Ok(())
}

pub fn jump_comp(vm: &mut Vm, index: i32) -> Result<()> {
    trace!(
        "Comparison Jump: Prev Comp is {}, jump amount is {}",
        vm.prev_comp,
        index
    );

    if vm.prev_comp {
        vm.index = Index((*vm.index as i32 + index + 1) as u32);
    } else {
        vm.index += Index(1);
    }

    Ok(())
}

pub fn and(vm: &mut Vm, left: u8, right: u8) -> Result<()> {
    vm.prev_op = vm.registers[left as usize].bit_and(vm.registers[right as usize], &mut vm.gc)?;
    vm.index += Index(1);

    Ok(())
}

pub fn or(vm: &mut Vm, left: u8, right: u8) -> Result<()> {
    vm.prev_op = vm.registers[left as usize].bit_or(vm.registers[right as usize], &mut vm.gc)?;
    vm.index += Index(1);

    Ok(())
}

pub fn xor(vm: &mut Vm, left: u8, right: u8) -> Result<()> {
    vm.prev_op = vm.registers[left as usize].bit_xor(vm.registers[right as usize], &mut vm.gc)?;
    vm.index += Index(1);

    Ok(())
}

pub fn not(vm: &mut Vm, reg: u8) -> Result<()> {
    vm.prev_op = vm.registers[reg as usize].bit_not(&mut vm.gc)?;
    vm.index += Index(1);

    Ok(())
}

pub fn eq(_vm: &mut Vm, _left: u8, _right: u8) -> Result<()> {
    todo!()
}

pub fn not_eq(_vm: &mut Vm, _left: u8, _right: u8) -> Result<()> {
    todo!()
}

pub fn greater_than(_vm: &mut Vm, _left: u8, _right: u8) -> Result<()> {
    todo!()
}

pub fn less_than(_vm: &mut Vm, _left: u8, _right: u8) -> Result<()> {
    todo!()
}

pub fn func(mut vm: &mut Vm, func: u32) -> Result<()> {
    trace!("Jumping to function {}", func);

    let mut registers = [RuntimeValue::None; NUMBER_REGISTERS];
    std::mem::swap(&mut vm.registers, &mut registers);

    vm.return_stack.push(ReturnFrame {
        registers,
        index: vm.index,
        function_index: vm.current_func,
        yield_point: None,
    });

    vm.index = Index(0);
    vm.current_func = Some(Index(func));

    while !vm.returning {
        trace!(
            "Executing Instruction: {:?}",
            vm.functions[func as usize][*vm.index as usize]
        );
        vm.functions[func as usize][*vm.index as usize]
            .clone()
            .execute(&mut vm)?;
    }
    vm.returning = false;

    // Note: Don't try to increment the index here, it will skip the instruction after the function call
    Ok(())
}

pub fn yield_generator(_vm: &mut Vm) -> Result<()> {
    todo!("Implement Generators/Coroutines")
}

pub fn ret(mut vm: &mut Vm) -> Result<()> {
    trace!("Executing a Return");

    // Get the most recent return frame
    if let Some(frame) = vm.return_stack.pop() {
        // Set the important stuff from the frame
        vm.index = frame.index;
        vm.registers = frame.registers;

        // If there is a function_index, then execute that function
        if let Some(function_index) = frame.function_index {
            trace!(
                "Executing Instruction: {:?}",
                vm.functions[*function_index as usize][*vm.index as usize]
            );
            while !vm.returning {
                vm.functions[*function_index as usize][*vm.index as usize]
                    .clone()
                    .execute(&mut vm)?;
            }

        // Otherwise, return to the execution of main
        } else {
            vm.returning = true;
            vm.index += Index(1);
        }

    // If there are no further stack frames, then halt execution
    } else {
        vm.returning = true;
        vm.index += Index(1);
    }

    Ok(())
}

pub fn collect(vm: &mut Vm) -> Result<()> {
    trace!("Forcing a GC collect");

    vm.gc.collect()?;
    vm.index += Index(1);

    Ok(())
}

pub fn halt(vm: &mut Vm) -> Result<()> {
    vm.finished_execution = true;

    Ok(())
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
    return Err(RuntimeError {
        ty: RuntimeErrorTy::IllegalInstruction,
        message: "Illegal Instruction".to_string(),
    });
}
