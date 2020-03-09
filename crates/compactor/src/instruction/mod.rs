mod bytecode_macro;
mod functions;
#[cfg(test)]
mod tests;

use crate::{compactor::Compactor, value::Value};
use crunch_error::runtime_prelude::*;

use alloc::boxed::Box;

/// Instructions for the [`Vm`]
///
/// [`Vm`]: crate::Vm
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Instruction {
    /// Load a [`Value`] into the selected register
    ///
    /// [`Value`]: crate::Value
    Load(Box<Value>, u8),

    /// Move the value from [`vm.prev_comp`] into the selected register
    ///
    /// [`vm.prev_comp`]: crate::Vm#prev_comp
    CompToReg(u8),

    /// Move the value from [`vm.prev_op`] into the selected register
    ///
    /// [`vm.prev_op`]: crate::Vm#prev_op
    OpToReg(u8),

    /// Drop the value at the selected register.  
    /// Note: Unroots [`Gc`] allocated objects
    ///
    /// [`Gc`]: crate::Gc
    Drop(u8),

    /// Moves the value at the first register into the second register
    ///
    Move(u8, u8),

    /// Copies the value at the first register into the second register
    ///
    Copy(u8, u8),

    /// Pushes the value at register to the [`Vm`] [`stack`]
    ///
    /// [`Vm`]: crate::Vm
    /// [`stack`]: crate::Vm#stack
    Push(u8),

    /// Pops off of the [`Vm`] [`stack`] into the selected register
    ///
    /// [`Vm`]: crate::Vm
    /// [`stack`]: crate::Vm#stack
    Pop(u8),

    /// Adds the first and second [`u8s`], storing the value in [`vm.prev_op`]
    ///
    /// [`vm.prev_op`]: crate::Vm#prev_op
    /// [`u8s`]: crate::u8
    Add(u8, u8),

    /// Subtracts the first and second [`u8s`], storing the value in [`vm.prev_op`]
    ///
    /// [`vm.prev_op`]: crate::Vm#prev_op
    /// [`u8s`]: crate::u8
    Sub(u8, u8),

    /// Multiplies the first and second [`u8s`], storing the value in [`vm.prev_op`]
    ///
    /// [`vm.prev_op`]: crate::Vm#prev_op
    /// [`u8s`]: crate::u8
    Mult(u8, u8),

    /// Divides the first and second [`u8s`], storing the value in [`vm.prev_op`]
    ///
    /// [`vm.prev_op`]: crate::Vm#prev_op
    /// [`u8s`]: crate::u8
    Div(u8, u8),

    /// Prints the value at the register to [`vm.stdout`]
    ///
    /// [`vm.stdout`]: crate::Vm#stdout
    Print(u8),

    /// Increments [`vm.index`] by the included value
    ///
    /// [`vm.index`]: crate::Vm#index
    Jump(i32),

    /// Increments [`vm.index`] by the included value if [`vm.prev_op`] is `true`
    ///
    /// [`vm.index`]: crate::Vm#index
    /// [`vm.prev_op`]: crate::Vm#prev_op
    JumpComp(i32),

    /// A no-op, used in parts of compilation
    JumpPoint(u32),

    /// Preforms a bitwise and (`&`) on the first and second [`u8s`], storing the value in [`vm.prev_op`]
    ///
    /// [`vm.prev_op`]: crate::Vm#prev_op
    /// [`u8s`]: crate::u8
    And(u8, u8),

    /// Preforms a bitwise or (`|`) on the first and second [`u8s`], storing the value in [`vm.prev_op`]
    ///
    /// [`vm.prev_op`]: crate::Vm#prev_op
    /// [`u8s`]: crate::u8
    Or(u8, u8),

    /// Preforms a bitwise xor (`^`) on the first and second [`u8s`], storing the value in [`vm.prev_op`]
    ///
    /// [`vm.prev_op`]: crate::Vm#prev_op
    /// [`u8s`]: crate::u8
    Xor(u8, u8),

    /// Preforms a bitwise not (`!`) on the register, storing the value in [`vm.prev_op`]
    ///
    /// [`vm.prev_op`]: crate::Vm#prev_op
    Not(u8),

    /// Preforms a comparison on the first and second [`u8s`], storing `true`
    /// if the values are equal into [`vm.prev_comp`]
    ///
    /// [`vm.prev_comp`]: crate::Vm#prev_op
    /// [`u8s`]: crate::u8
    Eq(u8, u8),

    /// Preforms a comparison on the first and second [`u8s`], storing `true`
    /// if the values are not equal into [`vm.prev_comp`]
    ///
    /// [`vm.prev_comp`]: crate::Vm#prev_op
    /// [`u8s`]: crate::u8
    NotEq(u8, u8),

    /// Preforms a comparison on the first and second [`u8s`], storing `true`
    /// if the the first value is greater than the other into [`vm.prev_comp`]
    ///
    /// [`vm.prev_comp`]: crate::Vm#prev_op
    /// [`u8s`]: crate::u8
    GreaterThan(u8, u8),

    /// Preforms a comparison on the first and second [`u8s`], storing `true`
    /// if the the first value is less than the other into [`vm.prev_comp`]
    ///
    /// [`vm.prev_comp`]: crate::Vm#prev_op
    /// [`u8s`]: crate::u8
    LessThan(u8, u8),

    /// Preforms a comparison on the first and second [`u8s`], storing `true`
    /// if the the first value is greater than or equal to the other into [`vm.prev_comp`]
    ///
    /// [`vm.prev_comp`]: crate::Vm#prev_op
    /// [`u8s`]: crate::u8
    GreaterThanEq(u8, u8),

    /// Preforms a comparison on the first and second [`u8s`], storing `true`
    /// if the the first value is less than or equal to the other into [`vm.prev_comp`]
    ///
    /// [`vm.prev_comp`]: crate::Vm#prev_op
    /// [`u8s`]: crate::u8
    LessThanEq(u8, u8),

    /// Changes [`vm.current_func`] to the contained value, thereby jumping to
    /// that function
    ///
    /// [`vm.current_func`]: crate::Vm#current_function
    Func(u32),

    // TODO: Make coroutines
    Yield,
    CallGenerator(u32, u8),

    /// Pushes the value of the first register to an Array contained in the second register
    ///
    PushArray {
        value: u8,
        array: u8,
    },

    /// Pops a value from the Array in the first register to the second register
    ///
    PopArray {
        array: u8,
        target: u8,
    },

    /// Indexes an Array, placing a copy of that value into `target`
    IndexArray {
        index: u8,
        array: u8,
        target: u8,
    },

    /// Removes the value at `index` from the array and places in in `target`
    RemoveArray {
        index: u8,
        array: u8,
        target: u8,
    },

    /// Returns to the last function on the [`return stack`] or exits execution if there are no
    /// frames to pop
    ///
    /// [`return stack`]: crate::Vm#return_stack
    Return,

    /// Manually calls for a [`Gc::collect`]
    ///
    /// [`Gc::collect`]: crate::Gc.collect
    Collect,

    /// Halts execution entirely
    Halt,

    /// Loads a library by the name supplied by the first register, storing a
    /// [`Value::Library`] in the second register
    ///
    /// [`Value::Library`]: crate::Value::Library
    #[cfg(feature = "dll-ffi")]
    LoadLib(u8, u8),

    /// Loads a function by the name supplied by the first register from the library stored in
    /// the second register, feeding it `n` values popped from the [`Vm`] [`stack`] as indicated by
    /// the `u16`
    ///
    /// [`Vm`]: crate::Vm
    /// [`stack`]: crate::Vm#stack
    // TODO: Make this lazy & store an arc of the original library to prevent
    // dangling symbols (Possibly gc stored struct or a boxed struct?)
    #[cfg(feature = "dll-ffi")]
    ExecLibFunc(u8, u8, u16),

    /// An illegal instruction
    Illegal,

    /// A no-op
    NoOp,
}

impl Instruction {
    /// The execution of each instruction
    // TODO: Document this bad boy
    pub fn execute(&self, vm: &mut Compactor) -> RuntimeResult<()> {
        trace!("Executing instruction {:?}", self);

        match self {
            Self::Load(val, reg) => functions::load(vm, (**val).clone(), *reg)?,
            Self::CompToReg(reg) => functions::comp_to_reg(vm, *reg)?,
            Self::OpToReg(reg) => functions::op_to_reg(vm, *reg)?,
            Self::Drop(reg) => functions::drop(vm, *reg)?,
            Self::Move(target, source) => functions::mov(vm, *target, *source)?,
            Self::Push(reg) => functions::push(vm, *reg)?,
            Self::Pop(reg) => functions::pop(vm, *reg)?,

            Self::Add(left, right) => functions::add(vm, *left, *right)?,
            Self::Sub(left, right) => functions::sub(vm, *left, *right)?,
            Self::Mult(left, right) => functions::mult(vm, *left, *right)?,
            Self::Div(left, right) => functions::div(vm, *left, *right)?,

            Self::Print(reg) => functions::print(vm, *reg)?,

            Self::Jump(index) => functions::jump(vm, *index)?,
            Self::JumpComp(index) => {
                functions::jump_comp(vm, *index)?;
            }

            Self::And(left, right) => functions::and(vm, *left, *right)?,
            Self::Or(left, right) => functions::or(vm, *left, *right)?,
            Self::Xor(left, right) => functions::xor(vm, *left, *right)?,
            Self::Not(reg) => functions::not(vm, *reg)?,

            Self::Eq(left, right) => functions::eq(vm, *left, *right)?,
            Self::NotEq(left, right) => functions::not_eq(vm, *left, *right)?,
            Self::GreaterThan(left, right) => functions::greater_than(vm, *left, *right)?,
            Self::LessThan(left, right) => functions::less_than(vm, *left, *right)?,
            Self::GreaterThanEq(left, right) => functions::greater_than_equal(vm, *left, *right)?,
            Self::LessThanEq(left, right) => functions::less_than_equal(vm, *left, *right)?,

            Self::Func(func) => functions::func(vm, *func)?,
            Self::Yield => functions::yield_generator(vm)?,
            Self::CallGenerator(func, reg) => functions::call_generator(vm, *func, *reg)?,
            Self::Return => functions::ret(vm)?,
            Self::Copy(left, right) => functions::copy(vm, *left, *right)?,

            Self::Collect => functions::collect(vm)?,
            Self::Halt => functions::halt(vm)?,

            #[cfg(feature = "dll-ffi")]
            Self::LoadLib(name, target) => functions::load_lib(vm, *name, *target)?,
            #[cfg(feature = "dll-ffi")]
            Self::ExecLibFunc(name, lib, args) => functions::exec_lib_func(vm, *name, *lib, *args)?,

            Self::PushArray { value, array } => functions::push_array(vm, *value, *array)?,
            Self::PopArray { array, target } => functions::pop_array(vm, *array, *target)?,
            Self::IndexArray {
                index,
                array,
                target,
            } => functions::index_array(vm, *index, *array, *target)?,
            Self::RemoveArray {
                index,
                array,
                target,
            } => functions::remove_array(vm, *index, *array, *target)?,

            Self::NoOp => functions::no_op(vm)?,
            Self::JumpPoint(_) => functions::jump_point(vm)?,
            Self::Illegal => functions::illegal(vm)?,
        }

        Ok(())
    }

    /// Turns the instruction into a string representation, for disassembly purposes
    #[must_use]
    pub fn to_str(&self) -> &'static str {
        match self {
            Self::Load(_, _) => "ld",
            Self::CompToReg(_) => "cmpr",
            Self::OpToReg(_) => "opr",
            Self::Drop(_) => "drop",
            Self::Move(_, _) => "mov",
            Self::Push(_) => "push",
            Self::Pop(_) => "pop",

            Self::Add(_, _) => "add",
            Self::Sub(_, _) => "sub",
            Self::Mult(_, _) => "mul",
            Self::Div(_, _) => "div",

            Self::Print(_) => "print",

            Self::Jump(_) => "jmp",
            Self::JumpComp(_) => "jmpcmp",
            Self::JumpPoint(_) => "jmppt",

            Self::And(_, _) => "and",
            Self::Or(_, _) => "or",
            Self::Xor(_, _) => "xor",
            Self::Not(_) => "not",

            Self::Eq(_, _) => "eq",
            Self::NotEq(_, _) => "neq",
            Self::GreaterThan(_, _) => "grt",
            Self::LessThan(_, _) => "let",
            Self::GreaterThanEq(_, _) => "grte",
            Self::LessThanEq(_, _) => "lete",

            Self::Func(_) => "call",
            Self::Yield => "yield",
            Self::CallGenerator(_, _) => "callgen",
            Self::Return => "ret",
            Self::Copy(_, _) => "copy",

            Self::Collect => "coll",
            Self::Halt => "halt",

            #[cfg(feature = "dll-ffi")]
            Self::LoadLib(_, _) => "ldlib",
            #[cfg(feature = "dll-ffi")]
            Self::ExecLibFunc(_, _, _) => "exlib",

            Self::PushArray { array: _, value: _ } => "pusharr",
            Self::PopArray {
                array: _,
                target: _,
            } => "poparr",
            Self::IndexArray {
                index: _,
                array: _,
                target: _,
            } => "idxarr",
            Self::RemoveArray {
                index: _,
                array: _,
                target: _,
            } => "rmarr",

            Self::Illegal => "illegal",
            Self::NoOp => "nop",
        }
    }
}
