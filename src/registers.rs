use super::{Index, Instruction, Register, StringPointer, Value, NUMBER_REGISTERS, NUMBER_STRINGS};

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Registers {
    registers: [Value; NUMBER_REGISTERS],
    strings: [String; NUMBER_STRINGS],
    snapshots: Vec<(Index, [Value; NUMBER_REGISTERS])>,
    pub functions: Vec<Vec<Instruction>>,
    pub return_stack: Vec<Index>,
    pub environment: Environment,
}

impl Registers {
    #[inline]
    pub fn new() -> Self {
        let registers = [Value::None; NUMBER_REGISTERS];
        let strings: [String; NUMBER_STRINGS] =
            array_init::array_init(|_| String::with_capacity(0));
        let environment = Environment::new();
        let return_stack = Vec::new();
        let snapshots = Vec::new();
        let functions = Vec::from(Vec::new());

        Self {
            registers,
            strings,
            snapshots,
            functions,
            return_stack,
            environment,
        }
    }

    #[inline]
    pub fn cleanup(&mut self) {
        for reg in &mut self.registers {
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
    pub fn load_str(&mut self, value: String, reg: StringPointer) {
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
    pub fn get_str_mut(&mut self, reg: StringPointer) -> &mut String {
        &mut self.strings[*reg as usize]
    }

    #[inline]
    pub fn add_strings(
        &mut self,
        left: StringPointer,
        right: StringPointer,
        output: StringPointer,
    ) {
        if left != output && right != output {
            unsafe {
                let left = {
                    let (ptr, len, capacity) = {
                        let string = &self.strings[*left as usize];
                        (string.as_ptr(), string.len(), string.capacity())
                    };

                    String::from_raw_parts(ptr as *mut _, len, capacity)
                };
                let right = {
                    let (ptr, len, capacity) = {
                        let string = &self.strings[*right as usize];
                        (string.as_ptr(), string.len(), string.capacity())
                    };

                    String::from_raw_parts(ptr as *mut _, len, capacity)
                };

                self.strings[*output as usize].push_str(&left);
                self.strings[*output as usize].push_str(&right);

                std::mem::forget(left);
                std::mem::forget(right);
            }
        } else if left == output {

        } else if right == output {

        }
    }

    #[inline]
    pub fn snapshot(&mut self) {
        self.snapshots
            .push((self.environment.index, self.registers));

        // TODO: Clear registers after saving?
    }
}

impl Default for Registers {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Environment {
    pub index: Index,
    pub finished_execution: bool,
    pub in_function: bool,
}

impl Environment {
    #[inline]
    pub const fn new() -> Self {
        Self {
            index: Index(0),
            finished_execution: false,
            in_function: false,
        }
    }
}

impl Default for Environment {
    fn default() -> Self {
        Self::new()
    }
}
