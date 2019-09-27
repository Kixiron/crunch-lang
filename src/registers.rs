use super::{
    Index, Instruction, Register, StringPointer, Value, NUMBER_HANDOFF_REGISTERS, NUMBER_REGISTERS,
    NUMBER_STRINGS,
};
use std::borrow::Cow;

#[allow(missing_debug_implementations)]
pub struct Registers {
    registers: [Value; NUMBER_REGISTERS],
    handoff_registers: [Value; NUMBER_HANDOFF_REGISTERS],
    strings: [Cow<'static, str>; NUMBER_STRINGS],
    snapshots: Vec<(Index, Option<Index>, [Value; NUMBER_REGISTERS])>,
    current_func: Option<Index>,
    functions: Vec<Vec<Instruction>>,
    return_stack: Vec<Index>,
    environment: Environment,
}

impl Registers {
    #[inline]
    pub fn new(functions: Vec<Vec<Instruction>>) -> Self {
        let registers = [Value::None; NUMBER_REGISTERS];
        let handoff_registers = [Value::None; NUMBER_HANDOFF_REGISTERS];
        let strings: [Cow<'static, str>; NUMBER_STRINGS] =
            array_init::array_init(|_| Cow::Borrowed(""));
        let snapshots = Vec::new();
        let current_func = None;
        let return_stack = Vec::new();
        let environment = Environment::new();

        Self {
            registers,
            handoff_registers,
            strings,
            snapshots,
            current_func,
            functions,
            return_stack,
            environment,
        }
    }

    #[inline]
    pub const fn registers(&self) -> &[Value; NUMBER_REGISTERS] {
        &self.registers
    }

    #[inline]
    pub fn registers_mut(&mut self) -> &mut [Value; NUMBER_REGISTERS] {
        &mut self.registers
    }

    #[inline]
    pub const fn handoff_registers(&self) -> &[Value; NUMBER_HANDOFF_REGISTERS] {
        &self.handoff_registers
    }

    #[inline]
    pub fn handoff_registers_mut(&mut self) -> &mut [Value; NUMBER_HANDOFF_REGISTERS] {
        &mut self.handoff_registers
    }

    #[inline]
    pub fn snapshots(&self) -> &Vec<(Index, Option<Index>, [Value; NUMBER_REGISTERS])> {
        &self.snapshots
    }

    #[inline]
    pub fn snapshots_mut(&mut self) -> &mut Vec<(Index, Option<Index>, [Value; NUMBER_REGISTERS])> {
        &mut self.snapshots
    }

    #[inline]
    pub const fn current_func(&self) -> &Option<Index> {
        &self.current_func
    }

    #[inline]
    pub fn current_func_mut(&mut self) -> &mut Option<Index> {
        &mut self.current_func
    }

    #[inline]
    pub fn functions(&self) -> &Vec<Vec<Instruction>> {
        &self.functions
    }

    #[inline]
    pub fn functions_mut(&mut self) -> &mut Vec<Vec<Instruction>> {
        &mut self.functions
    }

    #[inline]
    pub fn return_stack(&self) -> &Vec<Index> {
        &self.return_stack
    }

    #[inline]
    pub fn return_stack_mut(&mut self) -> &mut Vec<Index> {
        &mut self.return_stack
    }

    #[inline]
    pub const fn environment(&self) -> &Environment {
        &self.environment
    }

    #[inline]
    pub fn environment_mut(&mut self) -> &mut Environment {
        &mut self.environment
    }
}

impl Registers {
    #[inline]
    pub fn cleanup(&mut self) {
        for reg in self.registers.iter_mut() {
            *reg = Value::None;
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
    pub fn load_str(&mut self, value: Cow<'static, str>, reg: StringPointer) {
        self.strings[*reg as usize] = value;
    }

    #[inline]
    pub fn get(&self, reg: Register) -> &Value {
        &self.registers[*reg as usize]
    }

    #[inline]
    pub fn get_str(&self, reg: StringPointer) -> &str {
        &self.strings[*reg as usize]
    }

    #[inline]
    pub fn get_mut(&mut self, reg: Register) -> &mut Value {
        &mut self.registers[*reg as usize]
    }

    #[inline]
    pub fn get_str_mut(&mut self, reg: StringPointer) -> &mut Cow<'static, str> {
        &mut self.strings[*reg as usize]
    }

    #[inline]
    pub fn take_handoff(&mut self, reg: Register) -> Value {
        let mut value = Value::None;
        std::mem::swap(&mut value, self.get_mut(reg));
        value
    }

    #[inline]
    pub fn load_handoff(&mut self, handoff_reg: Register, value: Value) {
        self.handoff_registers[*handoff_reg as usize] = value;
    }

    #[inline]
    pub fn add_strings(
        &mut self,
        left: StringPointer,
        right: StringPointer,
        output: StringPointer,
    ) {
        let mut out = String::with_capacity(self.get_str(left).len() + self.get_str(right).len());
        out.push_str(self.get_str(left));
        out.push_str(self.get_str(right));
    
        *self.get_str_mut(output) = Cow::Owned(out);
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
    index: Index,
    finished_execution: bool,
    returning: bool,
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

    #[inline]
    pub fn index(&self) -> Index {
        self.index
    }

    #[inline]
    pub fn index_mut(&mut self) -> &mut Index {
        &mut self.index
    }

    #[inline]
    pub fn finished_execution(&self) -> bool {
        self.finished_execution
    }

    #[inline]
    pub fn finished_execution_mut(&mut self) -> &mut bool {
        &mut self.finished_execution
    }

    #[inline]
    pub fn returning(&self) -> bool {
        self.returning
    }

    #[inline]
    pub fn returning_mut(&mut self) -> &mut bool {
        &mut self.returning
    }
}

impl Default for Environment {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}
