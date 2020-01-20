use super::{
    parser::Either, Gc, Index, Instruction, Register, Result, RuntimeValue, NUMBER_REGISTERS,
};
use std::time::Instant;

/// The initialized options for the VM
#[derive(Debug, Copy, Clone)]
pub struct VmOptions {
    pub fault_tolerant: bool,
}
impl Default for VmOptions {
    fn default() -> Self {
        Self {
            fault_tolerant: false,
        }
    }
}
impl From<&crate::Options> for VmOptions {
    fn from(&crate::Options { fault_tolerant, .. }: &crate::Options) -> Self {
        Self { fault_tolerant }
    }
}

#[derive(Debug, Clone)]
pub struct ReturnFrame {
    pub registers: [RuntimeValue; NUMBER_REGISTERS - 5],
    pub index: Index,
    // If function_index is None, then the main function is being returned to
    pub function_index: u32,
    pub yield_point: Option<Index>,
}

/// The VM environment for Crunch
pub struct Vm {
    /// The active VM Registers
    pub registers: [RuntimeValue; NUMBER_REGISTERS],
    /// The register snapshots for function calls
    pub return_stack: Vec<ReturnFrame>,
    /// The index of the current function
    pub current_func: u32,
    /// The current instruction index of the current function
    pub index: Index,
    /// Whether or not program execution is done
    pub finished_execution: bool,
    /// Whether or not the program is currently returning
    // TODO: Needed?
    pub returning: bool,
    /// The value of the previous operation
    pub prev_op: RuntimeValue,
    /// The status of the previous comparison
    pub prev_comp: bool,
    /// The Garbage Collector
    pub gc: Gc,
    /// The options initialized with the VM
    pub options: VmOptions,
    /// The stdout that the program will print to, recommended to be `std::io::stdout()`
    pub stdout: Box<dyn std::io::Write>,
    pub start_time: Option<Instant>,
}

impl Vm {
    /// Creates a new VM from functions and options
    #[inline]
    #[must_use]
    pub fn new(options: &crate::Options, stdout: Box<dyn std::io::Write>) -> Self {
        Self {
            registers: array_init::array_init(|_| RuntimeValue::None),
            return_stack: Vec::new(),
            current_func: 0,
            index: Index(0),
            finished_execution: false,
            returning: false,
            prev_op: RuntimeValue::None,
            prev_comp: false,
            gc: Gc::new(options),
            options: VmOptions::from(options),
            stdout,
            start_time: None,
        }
    }

    pub fn execute(&mut self, functions: Vec<Vec<Instruction>>) -> Result<()> {
        // let functions = functions
        //     .into_iter()
        //     .map(|function| Function::new(function))
        //     .collect::<Vec<_>>();

        while !self.finished_execution {
            functions[self.current_func as usize][*self.index as usize].execute(self)?;
        }

        Ok(())
    }

    #[inline]
    pub fn clear(&mut self, reg: Register) {
        self.registers[*reg as usize] = RuntimeValue::None;
    }

    #[inline]
    pub fn load(&mut self, value: RuntimeValue, reg: Register) {
        self.registers[*reg as usize] = value;
    }

    #[inline]
    #[must_use]
    pub fn get(&self, reg: Register) -> &RuntimeValue {
        &self.registers[*reg as usize]
    }

    #[inline]
    pub fn get_mut(&mut self, reg: Register) -> &mut RuntimeValue {
        &mut self.registers[*reg as usize]
    }
}

impl std::fmt::Debug for Vm {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fmt.debug_struct("Vm")
            .field("registers", &self.registers)
            .field("return_stack", &self.return_stack)
            .field("current_func", &self.current_func)
            .field("index", &self.index)
            .field("finished_execution", &self.finished_execution)
            .field("returning", &self.returning)
            .field("prev_op", &self.prev_op)
            .field("prev_comp", &self.prev_comp)
            .field("gc", &self.gc)
            .field("options", &self.options)
            .finish()
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub function: Vec<Instruction>,
    pub meta: RuntimeFunctionMeta,
}

impl Function {
    // const JIT_USAGE_THRESHOLD: usize = 0;

    #[must_use]
    pub fn new(function: Vec<Instruction>) -> Self {
        Self {
            function,
            meta: RuntimeFunctionMeta::new(),
        }
    }

    pub fn execute(&self, vm: &mut Vm) -> Result<()> {
        self.meta.usages += 1;

        while !vm.finished_execution {
            self.function[*vm.index as usize].execute(vm)?;
        }

        Ok(())
    }
}

#[derive(Debug, Copy, Clone)]
pub struct RuntimeFunctionMeta {
    pub usages: usize,
}

impl RuntimeFunctionMeta {
    pub const fn new() -> Self {
        Self { usages: 0 }
    }
}

impl Default for RuntimeFunctionMeta {
    fn default() -> Self {
        Self::new()
    }
}
