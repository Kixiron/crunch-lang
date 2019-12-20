use crate::Vm;

pub extern "win64" fn add(vm: *mut Vm, left: u8, right: u8) {
    unsafe {
        crate::instruction::functions::add(&mut (*vm), left, right)
            .expect("Probably should handle jit errors as well");
    }
}

pub extern "win64" fn sub(vm: *mut Vm, left: u8, right: u8) {
    unsafe {
        crate::instruction::functions::sub(&mut (*vm), left, right)
            .expect("Probably should handle jit errors as well");
    }
}

pub extern "win64" fn mult(vm: *mut Vm, left: u8, right: u8) {
    unsafe {
        crate::instruction::functions::mult(&mut (*vm), left, right)
            .expect("Probably should handle jit errors as well");
    }
}

pub extern "win64" fn div(vm: *mut Vm, left: u8, right: u8) {
    unsafe {
        crate::instruction::functions::div(&mut (*vm), left, right)
            .expect("Probably should handle jit errors as well");
    }
}

pub extern "win64" fn comp_to_reg(vm: *mut Vm, reg: u8) {
    unsafe {
        crate::instruction::functions::comp_to_reg(&mut (*vm), reg)
            .expect("Probably should handle jit errors as well");
    }
}

pub extern "win64" fn op_to_reg(vm: *mut Vm, reg: u8) {
    unsafe {
        crate::instruction::functions::op_to_reg(&mut (*vm), reg)
            .expect("Probably should handle jit errors as well");
    }
}

pub extern "win64" fn drop_reg(vm: *mut Vm, reg: u8) {
    unsafe {
        crate::instruction::functions::drop_reg(&mut (*vm), reg)
            .expect("Probably should handle jit errors as well");
    }
}

pub extern "win64" fn print(vm: *mut Vm, reg: u8) {
    unsafe {
        crate::instruction::functions::print(&mut (*vm), reg)
            .expect("Probably should handle jit errors as well");
    }
}

pub extern "win64" fn jump(vm: *mut Vm, index: i32) {
    unsafe {
        crate::instruction::functions::jump(&mut (*vm), index)
            .expect("Probably should handle jit errors as well");
    }
}

pub extern "win64" fn jump_comp(vm: *mut Vm, index: i32) {
    unsafe {
        crate::instruction::functions::jump_comp(&mut (*vm), index)
            .expect("Probably should handle jit errors as well");
    }
}

pub extern "win64" fn and(vm: *mut Vm, left: u8, right: u8) {
    unsafe {
        crate::instruction::functions::and(&mut (*vm), left, right)
            .expect("Probably should handle jit errors as well");
    }
}

pub extern "win64" fn or(vm: *mut Vm, left: u8, right: u8) {
    unsafe {
        crate::instruction::functions::or(&mut (*vm), left, right)
            .expect("Probably should handle jit errors as well");
    }
}

pub extern "win64" fn xor(vm: *mut Vm, left: u8, right: u8) {
    unsafe {
        crate::instruction::functions::xor(&mut (*vm), left, right)
            .expect("Probably should handle jit errors as well");
    }
}

pub extern "win64" fn not(vm: *mut Vm, reg: u8) {
    unsafe {
        crate::instruction::functions::not(&mut (*vm), reg)
            .expect("Probably should handle jit errors as well");
    }
}

pub extern "win64" fn eq(vm: *mut Vm, left: u8, right: u8) {
    unsafe {
        crate::instruction::functions::eq(&mut (*vm), left, right)
            .expect("Probably should handle jit errors as well");
    }
}

pub extern "win64" fn not_eq(vm: *mut Vm, left: u8, right: u8) {
    unsafe {
        crate::instruction::functions::not_eq(&mut (*vm), left, right)
            .expect("Probably should handle jit errors as well");
    }
}

pub extern "win64" fn greater_than(vm: *mut Vm, left: u8, right: u8) {
    unsafe {
        crate::instruction::functions::greater_than(&mut (*vm), left, right)
            .expect("Probably should handle jit errors as well");
    }
}

pub extern "win64" fn less_than(vm: *mut Vm, left: u8, right: u8) {
    unsafe {
        crate::instruction::functions::less_than(&mut (*vm), left, right)
            .expect("Probably should handle jit errors as well");
    }
}

pub extern "win64" fn func(vm: *mut Vm, func: u32) {
    unsafe {
        crate::instruction::functions::func(&mut (*vm), func)
            .expect("Probably should handle jit errors as well");
    }
}

pub extern "win64" fn yield_generator(vm: *mut Vm) {
    unsafe {
        crate::instruction::functions::yield_generator(&mut (*vm))
            .expect("Probably should handle jit errors as well");
    }
}

pub extern "win64" fn ret(vm: *mut Vm) {
    unsafe {
        crate::instruction::functions::ret(&mut (*vm))
            .expect("Probably should handle jit errors as well");
    }
}

pub extern "win64" fn collect(vm: *mut Vm) {
    unsafe {
        crate::instruction::functions::collect(&mut (*vm))
            .expect("Probably should handle jit errors as well");
    }
}

pub extern "win64" fn halt(vm: *mut Vm) {
    unsafe {
        crate::instruction::functions::halt(&mut (*vm))
            .expect("Probably should handle jit errors as well");
    }
}

pub extern "win64" fn no_op(vm: *mut Vm) {
    unsafe {
        crate::instruction::functions::no_op(&mut (*vm))
            .expect("Probably should handle jit errors as well");
    }
}

pub extern "win64" fn illegal(vm: *mut Vm) {
    unsafe {
        crate::instruction::functions::illegal(&mut (*vm))
            .expect("Probably should handle jit errors as well");
    }
}

pub extern "win64" fn jump_point(vm: *mut Vm) {
    unsafe {
        crate::instruction::functions::ret(&mut (*vm))
            .expect("Probably should handle jit errors as well");
    }
}
