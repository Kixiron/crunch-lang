use super::{Index, Register, StringPointer, Value, NUMBER_REGISTERS, NUMBER_STRINGS};

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Registers {
    registers: [Value; NUMBER_REGISTERS],
    strings: [String; NUMBER_STRINGS],
    pub environment: Environment,
}

impl Registers {
    #[inline]
    pub fn new() -> Self {
        let registers = [Value::None; NUMBER_REGISTERS];
        let strings: [String; NUMBER_STRINGS] =
            array_init::array_init(|_| String::with_capacity(0));
        let environment = Environment::new();

        Self {
            registers,
            strings,
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
    #[allow(unsafe_code)]
    pub fn add_strings(&mut self, left: StringPointer, right: StringPointer) {
        if left == right {
            // Crappy costly add
            let ref_str = self.strings[*right as usize].to_string();
            self.strings[*left as usize].push_str(&ref_str);
        } else {
            // left == right is fatal with the unsafe code
            assert!(left != right, "Cannot add a string to itself");

            unsafe {
                let (ptr, len, capacity) = {
                    let string = &self.strings[*right as usize];
                    (
                        string.as_ptr() as *const u8,
                        string.len(),
                        string.capacity(),
                    )
                };
                let ref_str = String::from_raw_parts(ptr as *mut _, len, capacity);

                self.strings[*left as usize].push_str(&ref_str);

                std::mem::forget(ref_str);
            }
        }
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
}

impl Environment {
    #[inline]
    pub const fn new() -> Self {
        Self {
            index: Index(0),
            finished_execution: false,
        }
    }
}

impl Default for Environment {
    fn default() -> Self {
        Self::new()
    }
}
