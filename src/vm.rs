use super::{Gc, Index, Instruction, Register, Value, NUMBER_REGISTERS};

/// The initialized options for the VM
#[derive(Debug, Copy, Clone)]
pub struct VmOptions {}

impl From<&crate::Options> for VmOptions {
    fn from(_options: &crate::Options) -> Self {
        Self {}
    }
}

/// The VM environment for Crunch
#[allow(missing_debug_implementations)]
pub struct Vm {
    /// The active VM Registers
    pub registers: [Value; NUMBER_REGISTERS],
    /// The register snapshots (For moving functions)
    pub snapshots: Vec<(Index, Option<Index>, [Value; NUMBER_REGISTERS])>,
    /// The index of the current function, None if the current function is the main
    pub current_func: Option<Index>,
    /// The functions of the program
    pub functions: Vec<Vec<Instruction>>,
    /// The function return stack
    pub return_stack: Vec<Index>,
    /// The current instruction index of the current function
    pub index: Index,
    /// Whether or not program execution is done
    pub finished_execution: bool,
    /// Whether or not the program is currently returning
    // TODO: Needed?
    pub returning: bool,
    /// The value of the previous operation
    pub prev_op: Value,
    /// The status of the previous comparison
    pub prev_comp: bool,
    /// The Garbage Collector
    pub gc: Gc,
    /// The options initialized with the VM
    pub options: VmOptions,
    /// The stdout that the program will print to, recommended to be `std::io::stdout()`
    pub stdout: Box<dyn std::io::Write>,
}

impl Vm {
    /// Creates a new VM from functions and options
    #[inline]
    pub fn new(
        functions: Vec<Vec<Instruction>>,
        options: &crate::Options,
        stdout: Box<dyn std::io::Write>,
    ) -> Self {
        Self {
            registers: array_init::array_init(|_| Value::None),
            snapshots: Vec::new(),
            current_func: None,
            functions,
            return_stack: Vec::new(),
            index: Index(0),
            finished_execution: false,
            returning: false,
            prev_op: Value::None,
            prev_comp: false,
            gc: Gc::new(options),
            options: VmOptions::from(options),
            stdout,
        }
    }

    #[inline]
    pub fn clear(&mut self, reg: Register) {
        self.registers[*reg as usize] = Value::None;
    }

    #[inline]
    pub fn load(&mut self, value: Value, reg: Register) {
        self.registers[*reg as usize] = value;
    }

    #[inline]
    pub fn get(&self, reg: Register) -> &Value {
        &self.registers[*reg as usize]
    }

    #[inline]
    pub fn get_mut(&mut self, reg: Register) -> &mut Value {
        &mut self.registers[*reg as usize]
    }

    #[inline]
    pub fn snapshot(&mut self) {
        let mut old_regs = array_init::array_init(|_| Value::None);
        std::mem::swap(&mut old_regs, &mut self.registers);
        self.snapshots
            .push((self.index, self.current_func, old_regs));

        // TODO: Clear registers after saving?
    }
}

impl Drop for Vm {
    fn drop(&mut self) {
        // TODO: This should do things
    }
}
