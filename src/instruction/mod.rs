#[cfg(test)]
mod tests;

use super::{Register, RuntimeValue, Vm};

pub mod functions;

/// A type alias for results that could be a [`RuntimeError`]
pub type Result<T> = std::result::Result<T, RuntimeError>;

/// A Crunch Runtime Error
// TODO: Make this more detailed
#[repr(C)]
#[derive(Debug, Clone, Eq)]
pub struct RuntimeError {
    /// The type of error
    pub ty: RuntimeErrorTy,
    /// The error message
    pub message: String,
}

impl RuntimeError {
    /// Prints the formatted error to stdout
    // TODO: Make this fancy, and more detailed
    pub fn emit(&self) {
        println!("[Crunch Runtime Error: {:?}] {}", self.ty, self.message);
    }
}

impl PartialEq for RuntimeError {
    fn eq(&self, other: &Self) -> bool {
        self.ty == other.ty
    }
}

/// The type of [`RuntimeError`] that occurred
///
/// [`RuntimeError`]: crate::RuntimeError
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RuntimeErrorTy {
    /// An error in the [`Gc`]
    ///
    /// [`Gc`]: crate::Gc
    GcError,
    /// The user attempted to divide by zero
    DivideByZero,
    /// The two types are incompatible in the requested operation
    IncompatibleTypes,
    /// The program is missing a main function
    MissingMain,
    /// The requested variable is null
    NullVar,
    /// Thrown when an illegal instruction is executed
    IllegalInstruction,
    InvalidJump,
    MissingValue,
    MissingString,
    InvalidString,
    FileError,
    BytecodeError,
    CompilationError,
    MissingFile,
    InvalidInt,
    StdoutError,
    IntegerOverflow,
    MissingSymbol,
    JitError,
    EmptyStack,
}

/// Instructions for the [`Vm`]
///
/// [`Vm`]: crate::Vm
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Instruction {
    /// Load a [`RuntimeValue`] into the selected [`Register`]
    ///
    /// [`RuntimeValue`]: crate::RuntimeValue
    /// [`Register`]: crate::Register
    Load(RuntimeValue, Register),

    /// Move the value from [`vm.prev_comp`] into the selected [`Register`]
    ///
    /// [`vm.prev_comp`]: crate::Vm#prev_comp
    /// [`Register`]: crate::Register
    CompToReg(Register),

    /// Move the value from [`vm.prev_op`] into the selected [`Register`]
    ///
    /// [`vm.prev_op`]: crate::Vm#prev_op
    /// [`Register`]: crate::Register
    OpToReg(Register),

    /// Drop the value at the selected [`Register`].  
    /// Note: Unroots [`Gc`] allocated objects
    ///
    /// [`Gc`]: crate::Gc
    /// [`Register`]: crate::Register
    Drop(Register),

    /// Moves the value at the first [`Register`] into the second [`Register`]
    ///
    /// [`Register`]: crate::Register
    Move(Register, Register),

    /// Pushes the value at [`Register`] to the [`Vm`] [`stack`]
    ///
    /// [`Vm`]: crate::Vm
    /// [`stack`]: crate::Vm#stack
    /// [`Register`]: crate::Register
    Push(Register),

    /// Pops off of the [`Vm`] [`stack`] into the selected [`Register`]
    ///
    /// [`Vm`]: crate::Vm
    /// [`stack`]: crate::Vm#stack
    /// [`Register`]: crate::Register
    Pop(Register),

    /// Adds the first and second [`Registers`], storing the value in [`vm.prev_op`]
    ///
    /// [`vm.prev_op`]: crate::Vm#prev_op
    /// [`Registers`]: crate::Register
    Add(Register, Register),

    /// Subtracts the first and second [`Registers`], storing the value in [`vm.prev_op`]
    ///
    /// [`vm.prev_op`]: crate::Vm#prev_op
    /// [`Registers`]: crate::Register
    Sub(Register, Register),

    /// Multiplies the first and second [`Registers`], storing the value in [`vm.prev_op`]
    ///
    /// [`vm.prev_op`]: crate::Vm#prev_op
    /// [`Registers`]: crate::Register
    Mult(Register, Register),

    /// Divides the first and second [`Registers`], storing the value in [`vm.prev_op`]
    ///
    /// [`vm.prev_op`]: crate::Vm#prev_op
    /// [`Registers`]: crate::Register
    Div(Register, Register),

    /// Prints the value at the [`Register`] to [`vm.stdout`]
    ///
    /// [`vm.stdout`]: crate::Vm#stdout
    /// [`Register`]: crate::Register
    Print(Register),

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

    /// Preforms a bitwise and (`&`) on the first and second [`Registers`], storing the value in [`vm.prev_op`]
    ///
    /// [`vm.prev_op`]: crate::Vm#prev_op
    /// [`Registers`]: crate::Register
    And(Register, Register),

    /// Preforms a bitwise or (`|`) on the first and second [`Registers`], storing the value in [`vm.prev_op`]
    ///
    /// [`vm.prev_op`]: crate::Vm#prev_op
    /// [`Registers`]: crate::Register
    Or(Register, Register),

    /// Preforms a bitwise xor (`^`) on the first and second [`Registers`], storing the value in [`vm.prev_op`]
    ///
    /// [`vm.prev_op`]: crate::Vm#prev_op
    /// [`Registers`]: crate::Register
    Xor(Register, Register),

    /// Preforms a bitwise not (`!`) on the [`Register`], storing the value in [`vm.prev_op`]
    ///
    /// [`vm.prev_op`]: crate::Vm#prev_op
    /// [`Register`]: crate::Register
    Not(Register),

    /// Preforms a comparison on the first and second [`Registers`], storing `true`
    /// if the values are equal into [`vm.prev_comp`]
    ///
    /// [`vm.prev_comp`]: crate::Vm#prev_op
    /// [`Registers`]: crate::Register
    Eq(Register, Register),

    /// Preforms a comparison on the first and second [`Registers`], storing `true`
    /// if the values are not equal into [`vm.prev_comp`]
    ///
    /// [`vm.prev_comp`]: crate::Vm#prev_op
    /// [`Registers`]: crate::Register
    NotEq(Register, Register),

    /// Preforms a comparison on the first and second [`Registers`], storing `true`
    /// if the the first value is greater than the other into [`vm.prev_comp`]
    ///
    /// [`vm.prev_comp`]: crate::Vm#prev_op
    /// [`Registers`]: crate::Register
    GreaterThan(Register, Register),

    /// Preforms a comparison on the first and second [`Registers`], storing `true`
    /// if the the first value is less than the other into [`vm.prev_comp`]
    ///
    /// [`vm.prev_comp`]: crate::Vm#prev_op
    /// [`Registers`]: crate::Register
    LessThan(Register, Register),

    /// Preforms a comparison on the first and second [`Registers`], storing `true`
    /// if the the first value is greater than or equal to the other into [`vm.prev_comp`]
    ///
    /// [`vm.prev_comp`]: crate::Vm#prev_op
    /// [`Registers`]: crate::Register
    GreaterThanEq(Register, Register),

    /// Preforms a comparison on the first and second [`Registers`], storing `true`
    /// if the the first value is less than or equal to the other into [`vm.prev_comp`]
    ///
    /// [`vm.prev_comp`]: crate::Vm#prev_op
    /// [`Registers`]: crate::Register
    LessThanEq(Register, Register),

    /// Changes [`vm.current_func`] to the contained value, thereby jumping to
    /// that function
    ///
    /// [`vm.current_func`]: crate::Vm#current_function
    /// [`Register`]: crate::Register
    Func(u32),

    // TODO: Make coroutines
    Yield,

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

    /// Loads a library by the name supplied by the first [`Register`], storing a
    /// [`RuntimeValue::Library`] in the second [`Register`]
    ///
    /// [`RuntimeValue::Library`]: crate::RuntimeValue::Library
    /// [`Register`]: crate::Register
    LoadLib(Register, Register),

    /// Loads a function by the name supplied by the first [`Register`] from the library stored in
    /// the second [`Register`], feeding it `n` values popped from the [`Vm`] [`stack`] as indicated by
    /// the `u16`
    ///
    /// [`Vm`]: crate::Vm
    /// [`stack`]: crate::Vm#stack
    /// [`Register`]: crate::Register
    // TODO: Make this lazy & store an arc of the original library to prevent
    // dangling symbols (Possibly gc stored stuff?)
    ExecLibFunc(Register, Register, u16),

    /// An illegal instruction
    Illegal,

    /// A no-op
    NoOp,
}

impl Instruction {
    /// The execution of each instruction
    // TODO: Document this bad boy
    pub fn execute(&self, vm: &mut Vm) -> Result<()> {
        trace!("Executing instruction {:?}", self);

        match self {
            Self::Load(val, reg) => functions::load(vm, val.clone(), **reg)?,
            Self::CompToReg(reg) => functions::comp_to_reg(vm, **reg)?,
            Self::OpToReg(reg) => functions::op_to_reg(vm, **reg)?,
            Self::Drop(reg) => functions::drop(vm, **reg)?,
            Self::Move(target, source) => functions::mov(vm, **target, **source)?,
            Self::Push(reg) => functions::push(vm, **reg)?,
            Self::Pop(reg) => functions::pop(vm, **reg)?,

            Self::Add(left, right) => functions::add(vm, **left, **right)?,
            Self::Sub(left, right) => functions::sub(vm, **left, **right)?,
            Self::Mult(left, right) => functions::mult(vm, **left, **right)?,
            Self::Div(left, right) => functions::div(vm, **left, **right)?,

            Self::Print(reg) => functions::print(vm, **reg)?,

            Self::Jump(index) => functions::jump(vm, *index)?,
            Self::JumpComp(index) => {
                functions::jump_comp(vm, *index)?;
            }

            Self::And(left, right) => functions::and(vm, **left, **right)?,
            Self::Or(left, right) => functions::or(vm, **left, **right)?,
            Self::Xor(left, right) => functions::xor(vm, **left, **right)?,
            Self::Not(reg) => functions::not(vm, **reg)?,

            Self::Eq(left, right) => functions::eq(vm, **left, **right)?,
            Self::NotEq(left, right) => functions::not_eq(vm, **left, **right)?,
            Self::GreaterThan(left, right) => functions::greater_than(vm, **left, **right)?,
            Self::LessThan(left, right) => functions::less_than(vm, **left, **right)?,
            Self::GreaterThanEq(left, right) => functions::greater_than_equal(vm, **left, **right)?,
            Self::LessThanEq(left, right) => functions::less_than_equal(vm, **left, **right)?,

            Self::Func(func) => functions::func(vm, *func)?,
            Self::Yield => functions::yield_generator(vm)?,
            Self::Return => functions::ret(vm)?,

            Self::Collect => functions::collect(vm)?,
            Self::Halt => functions::halt(vm)?,

            Self::LoadLib(name, target) => functions::load_lib(vm, **name, **target)?,
            Self::ExecLibFunc(name, lib, args) => {
                functions::exec_lib_func(vm, **name, **lib, *args)?
            }

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
            Self::CompToReg(_) => "cmp",
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
            Self::Return => "ret",

            Self::Collect => "coll",
            Self::Halt => "halt",

            Self::LoadLib(_, _) => "ldlib",
            Self::ExecLibFunc(_, _, _) => "exlib",

            Self::Illegal => "illegal",
            Self::NoOp => "nop",
        }
    }
}
