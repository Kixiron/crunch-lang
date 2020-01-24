use crate::{RuntimeError, RuntimeErrorTy, RuntimeValue, Vm};

pub extern "win64" fn load(vm: *mut Vm, val: *const RuntimeValue, reg: u8) -> usize {
    unsafe {
        let vm = if let Some(vm) = vm.as_mut() {
            vm
        } else {
            let err = Box::leak(Box::new(RuntimeError {
                ty: RuntimeErrorTy::JitError,
                message: "The VM pointer is null".to_string(),
            }));
            return err as *const RuntimeError as usize;
        };

        match crate::instruction::functions::load(vm, val.read(), reg) {
            Ok(_) => 0,
            Err(err) => Box::into_raw(Box::new(err)) as usize,
        }
    }
}

pub extern "win64" fn add(vm: *mut Vm, left: u8, right: u8) -> usize {
    unsafe {
        let vm = if let Some(vm) = vm.as_mut() {
            vm
        } else {
            let err = Box::leak(Box::new(RuntimeError {
                ty: RuntimeErrorTy::JitError,
                message: "The VM pointer is null".to_string(),
            }));
            return err as *const RuntimeError as usize;
        };

        match crate::instruction::functions::add(vm, left, right) {
            Ok(_) => 0,
            Err(err) => Box::into_raw(Box::new(err)) as usize,
        }
    }
}

pub extern "win64" fn sub(vm: *mut Vm, left: u8, right: u8) -> usize {
    unsafe {
        let vm = if let Some(vm) = vm.as_mut() {
            vm
        } else {
            let err = Box::leak(Box::new(RuntimeError {
                ty: RuntimeErrorTy::JitError,
                message: "The VM pointer is null".to_string(),
            }));
            return err as *const RuntimeError as usize;
        };

        match crate::instruction::functions::sub(vm, left, right) {
            Ok(_) => 0,
            Err(err) => Box::into_raw(Box::new(err)) as usize,
        }
    }
}

pub extern "win64" fn mult(vm: *mut Vm, left: u8, right: u8) -> usize {
    unsafe {
        let vm = if let Some(vm) = vm.as_mut() {
            vm
        } else {
            let err = Box::leak(Box::new(RuntimeError {
                ty: RuntimeErrorTy::JitError,
                message: "The VM pointer is null".to_string(),
            }));
            return err as *const RuntimeError as usize;
        };

        match crate::instruction::functions::mult(vm, left, right) {
            Ok(_) => 0,
            Err(err) => Box::into_raw(Box::new(err)) as usize,
        }
    }
}

pub extern "win64" fn div(vm: *mut Vm, left: u8, right: u8) -> usize {
    unsafe {
        let vm = if let Some(vm) = vm.as_mut() {
            vm
        } else {
            let err = Box::leak(Box::new(RuntimeError {
                ty: RuntimeErrorTy::JitError,
                message: "The VM pointer is null".to_string(),
            }));
            return err as *const RuntimeError as usize;
        };

        match crate::instruction::functions::div(vm, left, right) {
            Ok(_) => 0,
            Err(err) => Box::into_raw(Box::new(err)) as usize,
        }
    }
}

pub extern "win64" fn comp_to_reg(vm: *mut Vm, reg: u8) -> usize {
    unsafe {
        let vm = if let Some(vm) = vm.as_mut() {
            vm
        } else {
            let err = Box::leak(Box::new(RuntimeError {
                ty: RuntimeErrorTy::JitError,
                message: "The VM pointer is null".to_string(),
            }));
            return err as *const RuntimeError as usize;
        };

        match crate::instruction::functions::comp_to_reg(vm, reg) {
            Ok(_) => 0,
            Err(err) => Box::into_raw(Box::new(err)) as usize,
        }
    }
}

pub extern "win64" fn op_to_reg(vm: *mut Vm, reg: u8) -> usize {
    unsafe {
        let vm = if let Some(vm) = vm.as_mut() {
            vm
        } else {
            let err = Box::leak(Box::new(RuntimeError {
                ty: RuntimeErrorTy::JitError,
                message: "The VM pointer is null".to_string(),
            }));
            return err as *const RuntimeError as usize;
        };

        match crate::instruction::functions::op_to_reg(vm, reg) {
            Ok(_) => 0,
            Err(err) => Box::into_raw(Box::new(err)) as usize,
        }
    }
}

pub extern "win64" fn drop(vm: *mut Vm, reg: u8) -> usize {
    unsafe {
        let vm = if let Some(vm) = vm.as_mut() {
            vm
        } else {
            let err = Box::leak(Box::new(RuntimeError {
                ty: RuntimeErrorTy::JitError,
                message: "The VM pointer is null".to_string(),
            }));
            return err as *const RuntimeError as usize;
        };

        match crate::instruction::functions::drop(vm, reg) {
            Ok(_) => 0,
            Err(err) => Box::into_raw(Box::new(err)) as usize,
        }
    }
}

pub extern "win64" fn mov(vm: *mut Vm, input: u8, output: u8) -> usize {
    unsafe {
        let vm = if let Some(vm) = vm.as_mut() {
            vm
        } else {
            let err = Box::leak(Box::new(RuntimeError {
                ty: RuntimeErrorTy::JitError,
                message: "The VM pointer is null".to_string(),
            }));
            return err as *const RuntimeError as usize;
        };

        match crate::instruction::functions::mov(vm, input, output) {
            Ok(_) => 0,
            Err(err) => Box::into_raw(Box::new(err)) as usize,
        }
    }
}

pub extern "win64" fn print(vm: *mut Vm, reg: u8) -> usize {
    unsafe {
        let vm = if let Some(vm) = vm.as_mut() {
            vm
        } else {
            let err = Box::leak(Box::new(RuntimeError {
                ty: RuntimeErrorTy::JitError,
                message: "The VM pointer is null".to_string(),
            }));
            return err as *const RuntimeError as usize;
        };

        match crate::instruction::functions::print(vm, reg) {
            Ok(_) => 0,
            Err(err) => Box::into_raw(Box::new(err)) as usize,
        }
    }
}

pub extern "win64" fn jump(vm: *mut Vm, index: i32) -> usize {
    unsafe {
        let vm = if let Some(vm) = vm.as_mut() {
            vm
        } else {
            let err = Box::leak(Box::new(RuntimeError {
                ty: RuntimeErrorTy::JitError,
                message: "The VM pointer is null".to_string(),
            }));
            return err as *const RuntimeError as usize;
        };

        match crate::instruction::functions::jump(vm, index) {
            Ok(_) => 0,
            Err(err) => Box::into_raw(Box::new(err)) as usize,
        }
    }
}

pub extern "win64" fn jump_comp(vm: *mut Vm, index: i32) -> usize {
    unsafe {
        let vm = if let Some(vm) = vm.as_mut() {
            vm
        } else {
            let err = Box::leak(Box::new(RuntimeError {
                ty: RuntimeErrorTy::JitError,
                message: "The VM pointer is null".to_string(),
            }));
            return err as *const RuntimeError as usize;
        };

        match crate::instruction::functions::jump_comp(vm, index) {
            Ok(_) => 0,
            Err(err) => Box::into_raw(Box::new(err)) as usize,
        }
    }
}

pub extern "win64" fn and(vm: *mut Vm, left: u8, right: u8) -> usize {
    unsafe {
        let vm = if let Some(vm) = vm.as_mut() {
            vm
        } else {
            let err = Box::leak(Box::new(RuntimeError {
                ty: RuntimeErrorTy::JitError,
                message: "The VM pointer is null".to_string(),
            }));
            return err as *const RuntimeError as usize;
        };

        match crate::instruction::functions::and(vm, left, right) {
            Ok(_) => 0,
            Err(err) => Box::into_raw(Box::new(err)) as usize,
        }
    }
}

pub extern "win64" fn or(vm: *mut Vm, left: u8, right: u8) -> usize {
    unsafe {
        let vm = if let Some(vm) = vm.as_mut() {
            vm
        } else {
            let err = Box::leak(Box::new(RuntimeError {
                ty: RuntimeErrorTy::JitError,
                message: "The VM pointer is null".to_string(),
            }));
            return err as *const RuntimeError as usize;
        };

        match crate::instruction::functions::or(vm, left, right) {
            Ok(_) => 0,
            Err(err) => Box::into_raw(Box::new(err)) as usize,
        }
    }
}

pub extern "win64" fn xor(vm: *mut Vm, left: u8, right: u8) -> usize {
    unsafe {
        let vm = if let Some(vm) = vm.as_mut() {
            vm
        } else {
            let err = Box::leak(Box::new(RuntimeError {
                ty: RuntimeErrorTy::JitError,
                message: "The VM pointer is null".to_string(),
            }));
            return err as *const RuntimeError as usize;
        };

        match crate::instruction::functions::xor(vm, left, right) {
            Ok(_) => 0,
            Err(err) => Box::into_raw(Box::new(err)) as usize,
        }
    }
}

pub extern "win64" fn not(vm: *mut Vm, reg: u8) -> usize {
    unsafe {
        let vm = if let Some(vm) = vm.as_mut() {
            vm
        } else {
            let err = Box::leak(Box::new(RuntimeError {
                ty: RuntimeErrorTy::JitError,
                message: "The VM pointer is null".to_string(),
            }));
            return err as *const RuntimeError as usize;
        };

        match crate::instruction::functions::not(vm, reg) {
            Ok(_) => 0,
            Err(err) => Box::into_raw(Box::new(err)) as usize,
        }
    }
}

pub extern "win64" fn eq(vm: *mut Vm, left: u8, right: u8) -> usize {
    unsafe {
        let vm = if let Some(vm) = vm.as_mut() {
            vm
        } else {
            let err = Box::leak(Box::new(RuntimeError {
                ty: RuntimeErrorTy::JitError,
                message: "The VM pointer is null".to_string(),
            }));
            return err as *const RuntimeError as usize;
        };

        match crate::instruction::functions::eq(vm, left, right) {
            Ok(_) => 0,
            Err(err) => Box::into_raw(Box::new(err)) as usize,
        }
    }
}

pub extern "win64" fn not_eq(vm: *mut Vm, left: u8, right: u8) -> usize {
    unsafe {
        let vm = if let Some(vm) = vm.as_mut() {
            vm
        } else {
            let err = Box::leak(Box::new(RuntimeError {
                ty: RuntimeErrorTy::JitError,
                message: "The VM pointer is null".to_string(),
            }));
            return err as *const RuntimeError as usize;
        };

        match crate::instruction::functions::not_eq(vm, left, right) {
            Ok(_) => 0,
            Err(err) => Box::into_raw(Box::new(err)) as usize,
        }
    }
}

pub extern "win64" fn greater_than_eq(vm: *mut Vm, left: u8, right: u8) -> usize {
    unsafe {
        let vm = if let Some(vm) = vm.as_mut() {
            vm
        } else {
            let err = Box::leak(Box::new(RuntimeError {
                ty: RuntimeErrorTy::JitError,
                message: "The VM pointer is null".to_string(),
            }));
            return err as *const RuntimeError as usize;
        };

        match crate::instruction::functions::greater_than_equal(vm, left, right) {
            Ok(_) => 0,
            Err(err) => Box::into_raw(Box::new(err)) as usize,
        }
    }
}

pub extern "win64" fn greater_than(vm: *mut Vm, left: u8, right: u8) -> usize {
    unsafe {
        let vm = if let Some(vm) = vm.as_mut() {
            vm
        } else {
            let err = Box::leak(Box::new(RuntimeError {
                ty: RuntimeErrorTy::JitError,
                message: "The VM pointer is null".to_string(),
            }));
            return err as *const RuntimeError as usize;
        };

        match crate::instruction::functions::greater_than(vm, left, right) {
            Ok(_) => 0,
            Err(err) => Box::into_raw(Box::new(err)) as usize,
        }
    }
}

pub extern "win64" fn less_than(vm: *mut Vm, left: u8, right: u8) -> usize {
    unsafe {
        let vm = if let Some(vm) = vm.as_mut() {
            vm
        } else {
            let err = Box::leak(Box::new(RuntimeError {
                ty: RuntimeErrorTy::JitError,
                message: "The VM pointer is null".to_string(),
            }));
            return err as *const RuntimeError as usize;
        };

        match crate::instruction::functions::less_than(vm, left, right) {
            Ok(_) => 0,
            Err(err) => Box::into_raw(Box::new(err)) as usize,
        }
    }
}

pub extern "win64" fn less_than_eq(vm: *mut Vm, left: u8, right: u8) -> usize {
    unsafe {
        let vm = if let Some(vm) = vm.as_mut() {
            vm
        } else {
            let err = Box::leak(Box::new(RuntimeError {
                ty: RuntimeErrorTy::JitError,
                message: "The VM pointer is null".to_string(),
            }));
            return err as *const RuntimeError as usize;
        };

        match crate::instruction::functions::less_than_equal(vm, left, right) {
            Ok(_) => 0,
            Err(err) => Box::into_raw(Box::new(err)) as usize,
        }
    }
}

pub extern "win64" fn func(vm: *mut Vm, func: u32) -> usize {
    unsafe {
        let vm = if let Some(vm) = vm.as_mut() {
            vm
        } else {
            let err = Box::leak(Box::new(RuntimeError {
                ty: RuntimeErrorTy::JitError,
                message: "The VM pointer is null".to_string(),
            }));
            return err as *const RuntimeError as usize;
        };

        match crate::instruction::functions::func(vm, func) {
            Ok(_) => 0,
            Err(err) => Box::into_raw(Box::new(err)) as usize,
        }
    }
}

pub extern "win64" fn yield_generator(vm: *mut Vm) -> usize {
    unsafe {
        let vm = if let Some(vm) = vm.as_mut() {
            vm
        } else {
            let err = Box::leak(Box::new(RuntimeError {
                ty: RuntimeErrorTy::JitError,
                message: "The VM pointer is null".to_string(),
            }));
            return err as *const RuntimeError as usize;
        };

        match crate::instruction::functions::yield_generator(vm) {
            Ok(_) => 0,
            Err(err) => Box::into_raw(Box::new(err)) as usize,
        }
    }
}

pub extern "win64" fn ret(vm: *mut Vm) -> usize {
    unsafe {
        let vm = if let Some(vm) = vm.as_mut() {
            vm
        } else {
            let err = Box::leak(Box::new(RuntimeError {
                ty: RuntimeErrorTy::JitError,
                message: "The VM pointer is null".to_string(),
            }));
            return err as *const RuntimeError as usize;
        };

        match crate::instruction::functions::ret(vm) {
            Ok(_) => 0,
            Err(err) => Box::into_raw(Box::new(err)) as usize,
        }
    }
}

pub extern "win64" fn collect(vm: *mut Vm) -> usize {
    unsafe {
        let vm = if let Some(vm) = vm.as_mut() {
            vm
        } else {
            let err = Box::leak(Box::new(RuntimeError {
                ty: RuntimeErrorTy::JitError,
                message: "The VM pointer is null".to_string(),
            }));
            return err as *const RuntimeError as usize;
        };

        match crate::instruction::functions::collect(vm) {
            Ok(_) => 0,
            Err(err) => Box::into_raw(Box::new(err)) as usize,
        }
    }
}

pub extern "win64" fn halt(vm: *mut Vm) -> usize {
    unsafe {
        let vm = if let Some(vm) = vm.as_mut() {
            vm
        } else {
            let err = Box::leak(Box::new(RuntimeError {
                ty: RuntimeErrorTy::JitError,
                message: "The VM pointer is null".to_string(),
            }));
            return err as *const RuntimeError as usize;
        };

        match crate::instruction::functions::halt(vm) {
            Ok(_) => 0,
            Err(err) => Box::into_raw(Box::new(err)) as usize,
        }
    }
}

pub extern "win64" fn no_op(vm: *mut Vm) -> usize {
    unsafe {
        let vm = if let Some(vm) = vm.as_mut() {
            vm
        } else {
            let err = Box::leak(Box::new(RuntimeError {
                ty: RuntimeErrorTy::JitError,
                message: "The VM pointer is null".to_string(),
            }));
            return err as *const RuntimeError as usize;
        };

        match crate::instruction::functions::no_op(vm) {
            Ok(_) => 0,
            Err(err) => Box::into_raw(Box::new(err)) as usize,
        }
    }
}

pub extern "win64" fn illegal(vm: *mut Vm) -> usize {
    unsafe {
        let vm = if let Some(vm) = vm.as_mut() {
            vm
        } else {
            let err = Box::leak(Box::new(RuntimeError {
                ty: RuntimeErrorTy::JitError,
                message: "The VM pointer is null".to_string(),
            }));
            return err as *const RuntimeError as usize;
        };

        match crate::instruction::functions::illegal(vm) {
            Ok(_) => 0,
            Err(err) => Box::into_raw(Box::new(err)) as usize,
        }
    }
}

pub extern "win64" fn jump_point(vm: *mut Vm) -> usize {
    unsafe {
        let vm = if let Some(vm) = vm.as_mut() {
            vm
        } else {
            let err = Box::leak(Box::new(RuntimeError {
                ty: RuntimeErrorTy::JitError,
                message: "The VM pointer is null".to_string(),
            }));
            return err as *const RuntimeError as usize;
        };

        match crate::instruction::functions::ret(vm) {
            Ok(_) => 0,
            Err(err) => Box::into_raw(Box::new(err)) as usize,
        }
    }
}
