#![warn(
    clippy::cargo,
    clippy::nursery,
    clippy::pedantic,
    clippy::perf,
    clippy::complexity,
    clippy::style,
    clippy::correctness,
    rust_2018_idioms
)]

const NUMBER_REGISTERS: usize = 10;
const NUMBER_STRINGS: usize = 10;
const NUMBER_GLOBALS: usize = 10;

mod value;

pub use value::Value;

pub struct Registers {
    registers: [Value; NUMBER_REGISTERS],
    strings: [String; NUMBER_STRINGS],
    environment: Environment,
}

impl Registers {
    #[inline]
    pub fn new() -> Self {
        let registers = [Value::None; NUMBER_REGISTERS];
        let strings: [String; NUMBER_STRINGS] = array_init::array_init(|_| String::default());
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
    pub fn clear(&mut self, reg: u8) {
        self.registers[reg as usize] = Value::None;
    }

    #[inline]
    pub fn load(&mut self, value: Value, reg: u8) {
        self.registers[reg as usize] = value;
    }

    #[inline]
    pub fn load_str(&mut self, value: String, reg: u8) {
        self.strings[reg as usize] = value;
    }

    #[inline]
    pub fn load_global(&mut self, value: Value, reg: u8) {
        self.environment.globals[reg as usize] = value;
    }

    #[inline]
    pub const fn get(&self, reg: u8) -> &Value {
        &self.registers[reg as usize]
    }

    #[inline]
    pub fn get_str(&self, reg: u8) -> &str {
        &self.strings[reg as usize]
    }

    #[inline]
    pub const fn get_global(&self, reg: u8) -> &Value {
        &self.environment.globals[reg as usize]
    }

    #[inline]
    pub fn get_mut(&mut self, reg: u8) -> &mut Value {
        &mut self.registers[reg as usize]
    }

    #[inline]
    pub fn get_str_mut(&mut self, reg: u8) -> &mut String {
        &mut self.strings[reg as usize]
    }

    #[inline]
    pub fn get_global_mut(&mut self, reg: u8) -> &mut Value {
        &mut self.environment.globals[reg as usize]
    }
}

impl Default for Registers {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

pub struct Environment {
    start_time: std::time::Instant,
    globals: [Value; NUMBER_GLOBALS],
}

impl Environment {
    pub fn new() -> Self {
        Self {
            start_time: std::time::Instant::now(),
            globals: [Value::None; NUMBER_GLOBALS],
        }
    }
}

impl Default for Environment {
    fn default() -> Self {
        Self::new()
    }
}

pub enum Instruction {
    LoadGlobal(Value, u8),
    LoadInt(i32, u8),
    LoadStr {
        string: String,
        str_reg: u8,
        reg: u8,
    },
    Drop(u8),
    AddStr {
        left: u8,
        right: u8,
    },
    AddInt {
        left: u8,
        right: u8,
    },
    SubInt {
        left: u8,
        right: u8,
    },
    Print(u8),
    Halt,
}

impl Instruction {
    #[inline]
    pub fn execute(self, registers: &mut Registers) {
        use Instruction::*;

        match self {
            LoadGlobal(value, reg) => {
                registers.load_global(value, reg);
            }
            LoadInt(value, reg) => {
                registers.load(Value::Int(value), reg);
            }
            LoadStr {
                string,
                str_reg,
                reg,
            } => {
                registers.load_str(string, str_reg);
                registers.load(Value::Str(str_reg), reg);
            }
            Drop(reg) => {
                registers.clear(reg);
            }
            AddStr { left, right } => {
                let string = registers.get_str(right).to_owned();
                registers.get_str_mut(left).push_str(&string);
            }
            AddInt { left, right } => {
                let int = *registers.get(right);
                (*registers.get_mut(left)) += int;
            }
            SubInt { left, right } => {
                let int = *registers.get(right);
                (*registers.get_mut(left)) -= int;
            }
            Print(reg) => match registers.registers[reg as usize] {
                Value::Str(reg) => println!("{}", registers.strings[reg as usize]),
                val => println!("{}", val),
            },
            Halt => {
                println!(
                    "Program Duration: {:?}",
                    registers.environment.start_time.elapsed().as_nanos()
                );
                registers.cleanup();
                std::process::exit(0);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        let instructions = {
            use Instruction::*;

            vec![
                LoadInt(10, 0),
                LoadInt(5, 1),
                LoadInt(10, 2),
                LoadInt(5, 3),
                AddInt { left: 0, right: 1 },
                SubInt { left: 2, right: 3 },
                Print(0),
                Print(2),
                Drop(0),
                Drop(1),
                Drop(2),
                Drop(3),
                LoadInt(10, 0),
                LoadInt(5, 1),
                SubInt { left: 0, right: 1 },
                Print(0),
                Drop(0),
                Drop(1),
                LoadStr {
                    string: "Hello from Crunch!".to_string(),
                    str_reg: 0,
                    reg: 0,
                },
                Print(0),
                LoadStr {
                    string: " The VM works!".to_string(),
                    str_reg: 1,
                    reg: 1,
                },
                AddStr { left: 0, right: 1 },
                Print(0),
                Drop(0),
                Drop(1),
                Halt,
            ]
        };

        let mut registers = Registers::new();

        for instruction in instructions.into_iter() {
            instruction.execute(&mut registers);
        }
    }
}
