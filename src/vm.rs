use super::{Gc, Index, Instruction, Register, Value, NUMBER_REGISTERS};

#[derive(Debug)]
pub struct VmOptions {}

impl From<&crate::Options> for VmOptions {
    fn from(_options: &crate::Options) -> Self {
        Self {}
    }
}

#[allow(missing_debug_implementations)]
pub struct Vm {
    pub registers: [Value; NUMBER_REGISTERS],
    pub snapshots: Vec<(Index, Option<Index>, [Value; NUMBER_REGISTERS])>,
    pub current_func: Option<Index>,
    pub functions: Vec<Vec<Instruction>>,
    pub return_stack: Vec<Index>,
    pub environment: Environment,
    pub prev_op: Value,
    pub prev_comp: bool,
    pub gc: Gc,
    pub options: VmOptions,
}

impl Vm {
    #[inline]
    pub fn new(functions: Vec<Vec<Instruction>>, options: &crate::Options) -> Self {
        let registers = [Value::None; NUMBER_REGISTERS];
        let snapshots = Vec::new();
        let current_func = None;
        let return_stack = Vec::new();
        let environment = Environment::new();
        let gc = Gc::new(options);

        Self {
            registers,
            snapshots,
            current_func,
            functions,
            return_stack,
            environment,
            prev_op: Value::None,
            prev_comp: false,
            gc,
            options: VmOptions::from(options),
        }
    }

    #[inline]
    pub fn cleanup(&mut self) {}

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
        self.snapshots
            .push((self.environment.index, self.current_func, self.registers));

        // TODO: Clear registers after saving?
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Environment {
    pub index: Index,
    pub finished_execution: bool,
    pub returning: bool,
}

impl Environment {
    #[inline]
    pub const fn new() -> Self {
        Self {
            index: Index(0),
            finished_execution: false,
            returning: false,
        }
    }
}

impl Default for Environment {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}
