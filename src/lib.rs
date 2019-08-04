#![deny(unsafe_code)]
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

pub const NUMBER_REGISTERS: usize = 10;
pub const NUMBER_STRINGS: usize = 10;

mod instruction;
mod registers;
mod value;

pub use instruction::*;
pub use registers::*;
pub use value::*;

#[inline]
pub fn to_bytecode(instructions: &[Instruction]) -> Result<Vec<Vec<u8>>, &'static str> {
    let mut vec = Vec::default();

    for instruction in instructions {
        match bincode::serialize(instruction) {
            Ok(bytecode) => vec.push(bytecode),
            Err(err) => {
                println!("{:?}", err);
                return Err("Serialization Error");
            }
        }
    }

    Ok(vec)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn init() {
        color_backtrace::install();
    }

    #[test]
    fn bytecode_test() {
        init();

        let instructions = {
            use Instruction::*;

            vec![
                LoadBool(true, 0),
                LoadBool(false, 1),
                CondJump { offset: 8, reg: 0 },
                CondJump { offset: 15, reg: 1 },
                LoadStr(LoadedString::new_boxed("Else".to_owned(), 0, 2)),
                Print(2),
                Drop(0),
                Drop(1),
                Drop(2),
                Halt,
                LoadStr(LoadedString::new_boxed("Else".to_owned(), 0, 2)),
                LoadStr(LoadedString::new_boxed("If".to_owned(), 1, 3)),
                AddStr {
                    left: 0.into(),
                    right: 1.into(),
                },
                Print(2),
                Drop(0),
                Drop(1),
                Drop(2),
                Halt,
                LoadStr(LoadedString::new_boxed("Else If".to_owned(), 0, 2)),
                Print(2),
                Drop(0),
                Drop(1),
                Drop(2),
                Halt,
            ]
        };

        println!("Instructions:");
        for (line, instruction) in instructions.iter().enumerate() {
            println!("  {:04}: {:?}", line, instruction);
        }

        println!("Bytecode:");
        for (line, instruction) in instructions.into_iter().enumerate() {
            let bytecode = bincode::serialize(&instruction).unwrap();

            print!("  {:04}: ", line);
            for byte in &bytecode {
                print!("{:02X} ", byte);
            }
            println!();
        }
    }

    #[test]
    fn instruction() {
        init();

        let instructions = {
            use Instruction::*;

            vec![
                LoadBool(true, 0),
                LoadBool(false, 1),
                CondJump { offset: 8, reg: 0 },
                CondJump { offset: 15, reg: 1 },
                LoadStr(LoadedString::new_boxed("Else".to_owned(), 0, 2)),
                Print(2),
                Drop(0),
                Drop(1),
                Drop(2),
                Halt,
                LoadStr(LoadedString::new_boxed("Else".to_owned(), 0, 2)),
                LoadStr(LoadedString::new_boxed("If".to_owned(), 1, 3)),
                AddStr {
                    left: 0.into(),
                    right: 1.into(),
                },
                Print(2),
                Drop(0),
                Drop(1),
                Drop(2),
                Halt,
                LoadStr(LoadedString::new_boxed("Else If".to_owned(), 0, 2)),
                Print(2),
                Drop(0),
                Drop(1),
                Drop(2),
                Halt,
            ]
        };

        let mut registers = Registers::new();

        while !registers.environment.finished_execution {
            instructions[*registers.environment.index]
                .clone()
                .execute(&mut registers);
        }
    }
}
