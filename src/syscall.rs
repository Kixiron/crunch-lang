lazy_static::lazy_static! {
    pub static ref SYSCALL_TABLE: [usize; 1] = [sys_exit as usize];
}

// Can have 5 parameters at most

pub unsafe extern "C" fn sys_exit(exit_code: i32) -> ! {
    std::process::exit(exit_code)
}
