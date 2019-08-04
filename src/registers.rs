use super::{StringPointer, Value, NUMBER_REGISTERS, NUMBER_STRINGS};
use derive_more::{Add, AddAssign, Constructor, Display, From, Into, Mul, MulAssign};
use serde::{Deserialize, Serialize};

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
    pub fn load_str(&mut self, value: String, reg: StringPointer) {
        self.strings[*reg as usize] = value;
    }

    #[inline]
    pub const fn get(&self, reg: u8) -> &Value {
        &self.registers[reg as usize]
    }

    #[inline]
    pub fn get_str(&self, reg: StringPointer) -> &str {
        &self.strings[*reg as usize]
    }

    #[inline]
    pub fn get_mut(&mut self, reg: u8) -> &mut Value {
        &mut self.registers[reg as usize]
    }

    #[inline]
    pub fn get_str_mut(&mut self, reg: StringPointer) -> &mut String {
        &mut self.strings[*reg as usize]
    }

    #[inline]
    #[allow(unsafe_code)]
    pub fn add_strings(&mut self, left: StringPointer, right: StringPointer) {
        assert!(left != right);

        unsafe {
            let (ptr, len, capacity) = {
                let string = &self.strings[*right as usize];
                (
                    string.as_ptr() as *const String,
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
    pub fn new() -> Self {
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

#[derive(
    Debug,
    Copy,
    Clone,
    PartialEq,
    Eq,
    Add,
    Mul,
    AddAssign,
    MulAssign,
    Constructor,
    Display,
    From,
    Into,
    Deserialize,
    Serialize,
)]
#[display(fmt = "{}", "_0")]
pub struct Index(usize);

impl Index {
    #[inline]
    pub const fn index(self) -> usize {
        self.0
    }

    #[inline]
    pub fn add(&mut self, amount: usize) {
        self.0 += amount
    }

    #[inline]
    pub fn sub(&mut self, amount: usize) {
        self.0 -= amount
    }

    #[inline]
    pub fn set(&mut self, index: usize) {
        self.0 = index;
    }
}

impl ::std::ops::Deref for Index {
    type Target = usize;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl ::std::ops::DerefMut for Index {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
