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
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RuntimeErrorTy {
    /// An error in the `Gc`
    ///
    /// [`Gc`]: crate.Gc
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
}

/// Instructions for the VM
// TODO: Document all Instructions
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Instruction {
    /// Load a Value directly into a register
    Load(RuntimeValue, Register),
    CompToReg(Register),
    OpToReg(Register),
    Drop(Register),
    Move(Register, Register),

    Add(Register, Register),
    Sub(Register, Register),
    Mult(Register, Register),
    Div(Register, Register),

    Print(Register),

    Jump(i32),
    JumpComp(i32),
    JumpPoint(u32),

    And(Register, Register),
    Or(Register, Register),
    Xor(Register, Register),
    Not(Register),

    Eq(Register, Register),
    NotEq(Register, Register),
    GreaterThan(Register, Register),
    LessThan(Register, Register),
    GreaterThanEq(Register, Register),
    LessThanEq(Register, Register),

    Func(u32),
    Yield,
    Return,

    Collect,
    Halt,

    // TODO: Handle FFI with the following instructions
    // LoadLib(&'static str), // Loads a dynamic library
    // CallLib(&'static str, input: Value::ParamBuf, output: Value::ParamBuf),

    // An illegal instruction
    Illegal,
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
            Self::OpToReg(reg) => functions::comp_to_reg(vm, **reg)?,
            Self::Drop(reg) => functions::drop(vm, **reg)?,
            Self::Move(target, source) => functions::mov(vm, **target, **source)?,

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

            Self::Illegal => "illegal",
            Self::NoOp => "nop",
        }
    }
}
