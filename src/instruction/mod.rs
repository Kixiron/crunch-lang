#[cfg(test)]
mod tests;

use super::{Register, Value, Vm};

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
    /// Load a [`Value`] into the selected [`Register`]
    ///
    /// [`Value`]: crate::Value
    /// [`Register`]: crate::Register
    Load(Value, Register),

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

    /// Copies the value at the first [`Register`] into the second [`Register`]
    ///
    /// [`Register`]: crate::Register
    Copy(Register, Register),

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
    CallGenerator(u32, Register),

    /// Pushes the value of the first [`Register`] to an Array contained in the second [`Register`]
    ///
    /// [`Register`]: crate::Register
    PushArray {
        value: Register,
        array: Register,
    },

    /// Pops a value from the Array in the first [`Register`] to the second [`Register`]
    ///
    /// [`Register`]: crate::Register
    PopArray {
        array: Register,
        target: Register,
    },

    /// Indexes an Array, placing a copy of that value into `target`
    IndexArray {
        index: Register,
        array: Register,
        target: Register,
    },

    /// Removes the value at `index` from the array and places in in `target`
    RemoveArray {
        index: Register,
        array: Register,
        target: Register,
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

    /// Loads a library by the name supplied by the first [`Register`], storing a
    /// [`Value::Library`] in the second [`Register`]
    ///
    /// [`Value::Library`]: crate::Value::Library
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
    // dangling symbols (Possibly gc stored struct or a boxed struct?)
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
            Self::CallGenerator(func, reg) => functions::call_generator(vm, *func, **reg)?,
            Self::Return => functions::ret(vm)?,
            Self::Copy(left, right) => functions::copy(vm, **left, **right)?,

            Self::Collect => functions::collect(vm)?,
            Self::Halt => functions::halt(vm)?,

            Self::LoadLib(name, target) => functions::load_lib(vm, **name, **target)?,
            Self::ExecLibFunc(name, lib, args) => {
                functions::exec_lib_func(vm, **name, **lib, *args)?
            }

            Self::PushArray { value, array } => functions::push_array(vm, **value, **array)?,
            Self::PopArray { array, target } => functions::pop_array(vm, **array, **target)?,
            Self::IndexArray {
                index,
                array,
                target,
            } => functions::index_array(vm, **index, **array, **target)?,
            Self::RemoveArray {
                index,
                array,
                target,
            } => functions::remove_array(vm, **index, **array, **target)?,

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

            Self::LoadLib(_, _) => "ldlib",
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

#[macro_export]
macro_rules! bytecode {
    ($(
        $label_index:expr => {
            $( $code:tt )*
        }
    )*) => {{
        use $crate::instruction::Instruction;

        let len = [$( $label_index ),*].len();
        let mut functions: Vec<Option<Vec<Instruction>>> = vec![None; len];

        $(
            let mut bytes: Vec<Instruction> = Vec::new();
            bytecode!(@inst bytes => $($code)*);

            functions[$label_index] = Some(bytes);
        )*

        functions.into_iter().filter_map(|elem| elem).collect::<Vec<Vec<Instruction>>>()
    }};

    (@inst $bytes:expr => load $value:expr, $reg:expr; $($rest:tt)*) => {
        $bytes.push(Instruction::Load($value.into(), $reg.into()));
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr => print $reg:expr; $($rest:tt)*) => {
        $bytes.push(Instruction::Print($reg.into()));
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr => cmpr $reg:expr; $($rest:tt)*) => {
        $bytes.push(Instruction::CompToReg($reg.into()));
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr => opr $reg:expr; $($rest:tt)*) => {
        $bytes.push(Instruction::OpToReg($reg.into()));
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr => drop $reg:expr; $($rest:tt)*) => {
        $bytes.push(Instruction::Drop($reg.into()));
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr => move $source:expr, $target:expr; $($rest:tt)*) => {
        $bytes.push(Instruction::Move($source.into(), $target.into()));
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr => copy $source:expr, $target:expr; $($rest:tt)*) => {
        $bytes.push(Instruction::Copy($source.into(), $target.into()));
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr => push $reg:expr; $($rest:tt)*) => {
        $bytes.push(Instruction::Push($reg.into()));
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr => pop $reg:expr; $($rest:tt)*) => {
        $bytes.push(Instruction::Pop($reg.into()));
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr => add $left:expr, $right:expr; $($rest:tt)*) => {
        $bytes.push(Instruction::Add($left.into(), $right.into()));
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr => sub $left:expr, $right:expr; $($rest:tt)*) => {
        $bytes.push(Instruction::Sub($left.into(), $right.into()));
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr => mult $left:expr, $right:expr; $($rest:tt)*) => {
        $bytes.push(Instruction::Mult($left.into(), $right.into()));
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr => div $left:expr, $right:expr; $($rest:tt)*) => {
        $bytes.push(Instruction::Div($left.into(), $right.into()));
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr => jump $offset:expr; $($rest:tt)*) => {
        $bytes.push(Instruction::Jump($offset.into()));
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr => jumpcmp $offset:expr; $($rest:tt)*) => {
        $bytes.push(Instruction::JumpComp($offset.into()));
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr => and $left:expr, $right:expr; $($rest:tt)*) => {
        $bytes.push(Instruction::And($left.into(), $right.into()));
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr => or $left:expr, $right:expr; $($rest:tt)*) => {
        $bytes.push(Instruction::Or($left.into(), $right.into()));
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr => xor $left:expr, $right:expr; $($rest:tt)*) => {
        $bytes.push(Instruction::Xor($left.into(), $right.into()));
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr => not $reg:expr; $($rest:tt)*) => {
        $bytes.push(Instruction::Not($reg.into()));
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr => eq $left:expr, $right:expr; $($rest:tt)*) => {
        $bytes.push(Instruction::Eq($left.into(), $right.into()));
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr => neq $left:expr, $right:expr; $($rest:tt)*) => {
        $bytes.push(Instruction::NotEq($left.into(), $right.into()));
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr => grt $left:expr, $right:expr; $($rest:tt)*) => {
        $bytes.push(Instruction::GreaterThan($left.into(), $right.into()));
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr => less $left:expr, $right:expr; $($rest:tt)*) => {
        $bytes.push(Instruction::LessThan($left.into(), $right.into()));
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr => grteq $left:expr, $right:expr; $($rest:tt)*) => {
        $bytes.push(Instruction::GreaterThanEq($left.into(), $right.into()));
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr => lesseq $left:expr, $right:expr; $($rest:tt)*) => {
        $bytes.push(Instruction::LessThanEq($left.into(), $right.into()));
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr => func $func:expr; $($rest:tt)*) => {
        $bytes.push(Instruction::Func($func.into()));
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr => yield; $($rest:tt)*) => {
        $bytes.push(Instruction::Yield);
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr => gen $func:expr, $reg:expr; $($rest:tt)*) => {
        $bytes.push(Instruction::CallGenerator($func.into(), $reg.into()));
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr => pusharr $value:expr, $arr:expr; $($rest:tt)*) => {
        $bytes.push(Instruction::PushArray { value: $value.into(), array: $arr.into() });
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr => poparr $arr:expr, $target:expr; $($rest:tt)*) => {
        $bytes.push(Instruction::PopArray { array: $arr.into(), target: $target.into() });
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr => pusharr $value:expr, $arr:expr, $target:expr; $($rest:tt)*) => {
        $bytes.push(Instruction::IndexArray { value: $value.into(), array: $arr.into(), target: $target.into() });
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr => rmarr $value:expr, $arr:expr, $target:expr; $($rest:tt)*) => {
        $bytes.push(Instruction::RemoveArray { value: $value.into(), array: $arr.into(), target: $target.into() });
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr => ret; $($rest:tt)*) => {
        $bytes.push(Instruction::Return);
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr => collect; $($rest:tt)*) => {
        $bytes.push(Instruction::Collect);
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr => halt; $($rest:tt)*) => {
        $bytes.push(Instruction::Halt);
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr => ldlib $name:expr, $target:expr; $($rest:tt)*) => {
        $bytes.push(Instruction::LoadLib($name.into(), $target.into()));
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr => execlib $name:expr, $target:expr, $number:expr; $($rest:tt)*) => {
        $bytes.push(Instruction::ExecLibFunc($name.into(), $target.into(), $number.into()));
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr => noop; $($rest:tt)*) => {
        $bytes.push(Instruction::NoOp);
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr =>) => {};
}

#[test]
fn macro_test() {
    let functions = bytecode! {
        0 => {
            load 10i32, 0;
            print 0;
            ret;
        }
    };

    Vm::default().execute(&functions).unwrap();
}
