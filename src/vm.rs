use super::{Gc, Index, Instruction, Register, Result, RuntimeValue, NUMBER_REGISTERS};
use std::time::Instant;

/// The initialized options for the VM
#[derive(Debug, Copy, Clone)]
pub(crate) struct VmOptions {
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
pub(crate) struct ReturnFrame {
    pub registers: [RuntimeValue; NUMBER_REGISTERS - 5],
    pub index: Index,
    // If function_index is None, then the main function is being returned to
    pub function_index: u32,
    pub yield_point: Option<Index>,
}

/// The VM environment for Crunch
pub struct Vm {
    /// The active VM Registers
    pub(crate) registers: [RuntimeValue; NUMBER_REGISTERS],
    /// The register snapshots for function calls
    pub(crate) return_stack: Vec<ReturnFrame>,
    /// The index of the current function
    pub(crate) current_func: u32,
    /// The current instruction index of the current function
    pub(crate) index: Index,
    /// Whether or not program execution is done
    pub(crate) finished_execution: bool,
    /// Whether or not the program is currently returning
    // TODO: Needed?
    pub(crate) returning: bool,
    /// The value of the previous operation
    pub(crate) prev_op: RuntimeValue,
    /// The status of the previous comparison
    pub(crate) prev_comp: bool,
    /// The Garbage Collector
    pub(crate) gc: Gc,
    /// The options initialized with the VM
    pub(crate) options: VmOptions,
    /// The stdout that the program will print to, recommended to be `std::io::stdout()`
    ///
    /// [`std::io::stdout()`]: #stdout.std::io
    pub(crate) stdout: Box<dyn std::io::Write>,
    pub(crate) start_time: Option<Instant>,
}

impl Vm {
    /// Creates a new `Vm` from `options` and the selected `stdout` target (Required to be `Write`)
    ///
    /// [`Vm`]: crate.Vm
    /// [`options`]: crate.Options
    /// [`Write`]: std::io::Write
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

    /// Uses the current `Vm` to execute the given `functions`  
    /// Note: this means that any data left in the `Gc` will transfer to the new program's runtime
    ///
    /// # Errors
    ///
    /// Will return any `RuntimeError` thrown and not caught by the running program and program runtime
    ///
    /// [`Vm`]: crate.Vm
    /// [`functions`]: crate.Function
    /// [`Gc`]: crate.Gc
    /// [`RuntimeError`]: crate.RuntimeError
    pub fn execute(&mut self, functions: Vec<Vec<Instruction>>) -> Result<()> {
        while !self.finished_execution {
            functions[self.current_func as usize][*self.index as usize].execute(self)?;
        }

        Ok(())
    }

    #[inline]
    pub(crate) fn clear(&mut self, reg: Register) {
        self.registers[*reg as usize] = RuntimeValue::None;
    }

    #[inline]
    pub(crate) fn load(&mut self, value: RuntimeValue, reg: Register) {
        self.registers[*reg as usize] = value;
    }

    #[inline]
    #[must_use]
    pub(crate) fn get(&self, reg: Register) -> &RuntimeValue {
        &self.registers[*reg as usize]
    }

    #[inline]
    pub(crate) fn get_mut(&mut self, reg: Register) -> &mut RuntimeValue {
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
pub(crate) struct Function {
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
        // TODO: Collect runtime function information somehow
        // Possibly with indexes into associated metas?

        while !vm.finished_execution {
            self.function[*vm.index as usize].execute(vm)?;
        }

        Ok(())
    }
}

#[derive(Debug, Copy, Clone)]
pub(crate) struct RuntimeFunctionMeta {
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
