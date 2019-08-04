use super::{Registers, StringPointer, Value};
use serde::{Deserialize, Serialize};

#[derive(Debug, PartialEq, Eq, Clone, Deserialize, Serialize)]
#[repr(u8)]
pub enum Instruction {
    LoadInt(i32, u8),
    LoadStr(Box<LoadedString>),
    LoadBool(bool, u8),
    Drop(u8),
    AddStr {
        left: StringPointer,
        right: StringPointer,
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
    Jump(i16),
    CondJump {
        offset: i16,
        reg: u8,
    },
    Halt,
}

#[derive(Debug, PartialEq, Eq, Clone, Deserialize, Serialize)]
pub struct LoadedString {
    pub string: String,
    pub str_reg: u8,
    pub reg: u8,
}

impl LoadedString {
    #[inline]
    pub const fn new(string: String, str_reg: u8, reg: u8) -> Self {
        Self {
            string,
            str_reg,
            reg,
        }
    }

    #[inline]
    pub fn new_boxed(string: String, str_reg: u8, reg: u8) -> Box<Self> {
        Box::new(Self::new(string, str_reg, reg))
    }
}

impl Instruction {
    #[inline]
    pub fn execute(self, registers: &mut Registers) {
        use Instruction::*;

        match self {
            LoadInt(int, reg) => {
                registers.load(Value::Int(int), reg);

                registers.environment.index.add(1);
            }
            LoadStr(load) => {
                registers.load_str(load.string, load.str_reg.into());
                registers.load(Value::Str(load.str_reg.into()), load.reg);

                registers.environment.index.add(1);
            }
            LoadBool(boolean, reg) => {
                registers.load(Value::Bool(boolean), reg);

                registers.environment.index.add(1);
            }
            Drop(reg) => {
                registers.clear(reg);

                registers.environment.index.add(1);
            }
            AddStr { left, right } => {
                registers.add_strings(left, right);

                registers.environment.index.add(1);
            }
            AddInt { left, right } => {
                let int = *registers.get(right);
                (*registers.get_mut(left)) += int;

                registers.environment.index.add(1);
            }
            SubInt { left, right } => {
                let int = *registers.get(right);
                (*registers.get_mut(left)) -= int;

                registers.environment.index.add(1);
            }
            Print(reg) => {
                match registers.get(reg) {
                    Value::Str(ptr) => println!("{}", ptr.fetch(&registers)),
                    val => println!("{}", val),
                }

                registers.environment.index.add(1);
            }
            Jump(offset) => {
                #[allow(clippy::pedantic)]
                registers
                    .environment
                    .index
                    .set((registers.environment.index.index() as isize + offset as isize) as usize);
            }
            CondJump { offset, reg } => {
                if registers.get(reg) == &Value::Bool(true) {
                    #[allow(clippy::pedantic)]
                    registers.environment.index.set(
                        (registers.environment.index.index() as isize + offset as isize) as usize,
                    );
                } else {
                    registers.environment.index.add(1);
                }
            }
            Halt => {
                registers.cleanup();
                registers.environment.finished_execution = true;
            }
        }
    }
}
