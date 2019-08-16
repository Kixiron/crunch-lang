use super::{Instruction, LoadedString, Register, StringPointer};

const INSTRUCTION_LENGTH: usize = 8;

fn decode(instruction: [u8; INSTRUCTION_LENGTH], string: Option<String>) -> Instruction {
    use std::convert::TryInto;

    match instruction[0] {
        0x00 => Instruction::LoadInt(
            i32::from_be_bytes(
                instruction[1..5]
                    .try_into()
                    .unwrap_or_else(|_| unsafe { std::hint::unreachable_unchecked() }),
            ),
            Register(instruction[6]),
        ),
        0x01 => Instruction::LoadStr(LoadedString::new_boxed(
            string.expect("Improper string distribution"),
            StringPointer(instruction[2]),
            Register(instruction[1]),
        )),
        0x02 => Instruction::LoadBool(instruction[1] != 0, Register(instruction[2])),
        0x03 => Instruction::Drop(Register(instruction[1])),
        0x04 => Instruction::DropStr(StringPointer(instruction[1])),
        0x05 => Instruction::AddStr {
            left: StringPointer(instruction[1]),
            right: StringPointer(instruction[2]),
            output: StringPointer(instruction[3]),
        },
        0x06 => Instruction::AddInt {
            left: Register(instruction[1]),
            right: Register(instruction[2]),
            output: Register(instruction[3]),
        },
        0x07 => Instruction::SubInt {
            left: Register(instruction[1]),
            right: Register(instruction[2]),
            output: Register(instruction[3]),
        },
        0x08 => Instruction::Print(Register(instruction[1])),
        0x09 => Instruction::Jump(i32::from_be_bytes(
            instruction[1..5]
                .try_into()
                .unwrap_or_else(|_| unsafe { std::hint::unreachable_unchecked() }),
        )),
        0x0A => Instruction::CondJump {
            index: i32::from_be_bytes(
                instruction[1..5]
                    .try_into()
                    .unwrap_or_else(|_| unsafe { std::hint::unreachable_unchecked() }),
            ),
            reg: Register(instruction[6]),
        },
        0x0B => Instruction::Halt,
        0x0C => Instruction::MultInt {
            left: Register(instruction[1]),
            right: Register(instruction[2]),
            output: Register(instruction[3]),
        },
        0x0D => Instruction::DivInt {
            left: Register(instruction[1]),
            right: Register(instruction[2]),
            output: Register(instruction[3]),
        },
        0x0E => Instruction::JumpLessThan {
            index: i32::from_be_bytes(
                instruction[1..5]
                    .try_into()
                    .unwrap_or_else(|_| unsafe { std::hint::unreachable_unchecked() }),
            ),
            reg: Register(instruction[6]),
            compare: Register(instruction[7]),
        },
        0x0F => Instruction::JumpGreaterThan {
            index: i32::from_be_bytes(
                instruction[1..5]
                    .try_into()
                    .unwrap_or_else(|_| unsafe { std::hint::unreachable_unchecked() }),
            ),
            reg: Register(instruction[6]),
            compare: Register(instruction[7]),
        },
        0x10 => Instruction::Return,
        0x11 => Instruction::FuncJump(
            u32::from_be_bytes(
                instruction[1..5]
                    .try_into()
                    .unwrap_or_else(|_| unsafe { std::hint::unreachable_unchecked() }),
            )
            .into(),
        ),

        _ => unsafe { std::hint::unreachable_unchecked() },
    }
}

fn encode(instruction: &Instruction) -> ([u8; INSTRUCTION_LENGTH], Option<String>) {
    let mut bytes = [0; INSTRUCTION_LENGTH];
    let mut string = None;

    match instruction {
        Instruction::LoadInt(int, reg) => {
            bytes[0] = 0x00;
            bytes[1..5].copy_from_slice(&int.to_be_bytes());
            bytes[6] = **reg;
        }
        Instruction::LoadStr(ref load) => {
            bytes[0] = 0x01;
            bytes[1] = *load.reg;
            bytes[2] = *load.str_reg;

            string = Some(load.string.clone());
        }
        Instruction::LoadBool(boolean, reg) => {
            bytes[0] = 0x02;
            bytes[1] = (*boolean).into();
            bytes[2] = **reg;
        }
        Instruction::Drop(reg) => {
            bytes[0] = 0x03;
            bytes[1] = **reg;
        }
        Instruction::DropStr(ptr) => {
            bytes[0] = 0x04;
            bytes[1] = **ptr;
        }
        Instruction::AddStr {
            left,
            right,
            output,
        } => {
            bytes[0] = 0x05;
            bytes[1] = **left;
            bytes[2] = **right;
            bytes[3] = **output;
        }
        Instruction::AddInt {
            left,
            right,
            output,
        } => {
            bytes[0] = 0x06;
            bytes[1] = **left;
            bytes[2] = **right;
            bytes[3] = **output;
        }
        Instruction::SubInt {
            left,
            right,
            output,
        } => {
            bytes[0] = 0x07;
            bytes[1] = **left;
            bytes[2] = **right;
            bytes[3] = **output;
        }
        Instruction::Print(reg) => {
            bytes[0] = 0x08;
            bytes[1] = **reg;
        }
        Instruction::Jump(index) => {
            bytes[0] = 0x09;
            bytes[1..5].copy_from_slice(&index.to_be_bytes());
        }
        Instruction::CondJump { index, reg } => {
            bytes[0] = 0x0A;
            bytes[1..5].copy_from_slice(&index.to_be_bytes());
            bytes[6] = **reg;
        }
        Instruction::Halt => {
            bytes[0] = 0x0B;
        }
        Instruction::MultInt {
            left,
            right,
            output,
        } => {
            bytes[0] = 0x0C;
            bytes[1] = **left;
            bytes[2] = **right;
            bytes[3] = **output;
        }
        Instruction::DivInt {
            left,
            right,
            output,
        } => {
            bytes[0] = 0x0D;
            bytes[1] = **left;
            bytes[2] = **right;
            bytes[3] = **output;
        }
        Instruction::JumpLessThan {
            index,
            reg,
            compare,
        } => {
            bytes[0] = 0x0E;
            bytes[1..5].copy_from_slice(&index.to_be_bytes());
            bytes[6] = **reg;
            bytes[7] = **compare;
        }
        Instruction::JumpGreaterThan {
            index,
            reg,
            compare,
        } => {
            bytes[0] = 0x0F;
            bytes[1..5].copy_from_slice(&index.to_be_bytes());
            bytes[6] = **reg;
            bytes[7] = **compare;
        }
        Instruction::Return => {
            bytes[0] = 0x10;
        }
        Instruction::FuncJump(index) => {
            bytes[0] = 0x11;
            bytes[1..5].copy_from_slice(&index.to_be_bytes());
        }
    }

    (bytes, string)
}

pub fn encode_instructions(instructions: &[Instruction]) -> Vec<u8> {
    let mut instruction_bytes = vec![0_u8; instructions.len() * INSTRUCTION_LENGTH];
    let mut instruction_strings: Vec<String> = Vec::default();

    for (index, instruction) in instructions.iter().enumerate() {
        let (bytes, string) = encode(instruction);

        instruction_bytes
            [index * INSTRUCTION_LENGTH..(index * INSTRUCTION_LENGTH) + INSTRUCTION_LENGTH]
            .copy_from_slice(&bytes);
        if let Some(string) = string {
            instruction_strings.push(string);
        }
    }

    let mut string_bytes: Vec<u8> = Vec::new();
    string_bytes.extend_from_slice(&(instruction_strings.len() as u32).to_be_bytes());

    for string in instruction_strings {
        string_bytes.extend_from_slice(&(string.len() as u32).to_be_bytes());
        string_bytes.extend_from_slice(string.as_bytes());
    }

    string_bytes.extend_from_slice(&instruction_bytes);
    string_bytes
}

#[inline]
pub fn decode_instructions(bytes: &[u8]) -> Vec<Instruction> {
    use std::{collections::VecDeque, convert::TryInto};

    let mut index = 0;
    let num_strings = take_usize(&mut index, bytes);
    let mut strings = VecDeque::with_capacity(num_strings);

    for _ in 0..num_strings {
        let len = take_usize(&mut index, bytes);

        strings.push_back(
            String::from_utf8(bytes[index..index + len].to_vec()).expect("Invalid string"),
        );
        index += len;
    }

    let mut instructions = Vec::with_capacity(bytes[index..].len() / INSTRUCTION_LENGTH);
    for chunk in bytes[index..].chunks(INSTRUCTION_LENGTH) {
        let chunk: [u8; INSTRUCTION_LENGTH] = chunk
            .try_into()
            .unwrap_or_else(|_| unsafe { std::hint::unreachable_unchecked() });

        let instruction = if chunk[0] == 0x01 {
            decode(chunk, strings.pop_front())
        } else {
            decode(chunk, None)
        };
        instructions.push(instruction);
    }

    instructions
}

fn take_usize(index: &mut usize, bytes: &[u8]) -> usize {
    use std::convert::TryInto;

    let int = u32::from_be_bytes(
        bytes[*index..*index + 4]
            .try_into()
            .unwrap_or_else(|_| unsafe { std::hint::unreachable_unchecked() }),
    ) as usize;

    *index += 4;

    int
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Instruction::*;

    fn debug_take_usize(index: &mut usize, bytes: &[u8]) -> ([u8; 4], usize) {
        use std::convert::TryInto;

        let int_bytes = bytes[*index..*index + 4]
            .try_into()
            .unwrap_or_else(|_| unsafe { std::hint::unreachable_unchecked() });
        let int = u32::from_be_bytes(int_bytes) as usize;

        *index += 4;

        (int_bytes, int)
    }

    #[test]
    fn test() {
        simple_logger::init().unwrap();

        let instructions: Vec<Instruction> = vec![Halt];
        let encoded = encode_instructions(&instructions);

        {
            use std::{collections::VecDeque, convert::TryInto};

            println!();

            let mut index = 0;
            let (num_string_bytes, num_strings) = debug_take_usize(&mut index, &encoded);
            let mut decoded_strings = Vec::with_capacity(num_strings);
            let mut strings = VecDeque::new();
            let mut raw_strings = Vec::new();

            for line in 0..num_strings {
                let (len_bytes, len) = debug_take_usize(&mut index, &encoded);

                strings.push_back(format!(
                    "    {:04}: Length: {} (Length Bytes: {})\n      String: {:?}\n      Bytes: {}",
                    line,
                    len,
                    len_bytes
                        .iter()
                        .map(|b| format!("{:02X}", b))
                        .collect::<Vec<String>>()
                        .join(" "),
                    String::from_utf8(encoded[index..index + len].to_vec())
                        .expect("Invalid string"),
                    encoded[index..index + len]
                        .iter()
                        .map(|b| format!("{:02X}", b))
                        .collect::<Vec<String>>()
                        .join(" ")
                ));
                decoded_strings.push(
                    String::from_utf8(encoded[index..index + len].to_vec())
                        .expect("Invalid string"),
                );
                raw_strings.push(
                    String::from_utf8(encoded[index..index + len].to_vec())
                        .expect("Invalid string"),
                );
                index += len;
            }

            let mut instructions = Vec::with_capacity(encoded[index..].len() / INSTRUCTION_LENGTH);
            let mut instruction_bytes = Vec::with_capacity(encoded[index..].len());

            for (line, chunk) in encoded[index..].chunks(INSTRUCTION_LENGTH).enumerate() {
                let chunk: [u8; INSTRUCTION_LENGTH] = chunk
                    .try_into()
                    .unwrap_or_else(|_| unsafe { std::hint::unreachable_unchecked() });
                let mut str_index = 0;

                instructions.push(format!(
                    "    {:04}: {:?}",
                    line,
                    if chunk[0] == 0x01 {
                        str_index += 1;
                        decode(chunk, Some(raw_strings[str_index - 1].clone()))
                    } else {
                        decode(chunk, None)
                    }
                ));
                instruction_bytes.push(format!(
                    "    {:04}: {}",
                    line,
                    chunk
                        .iter()
                        .map(|b| format!("{:02X}", b))
                        .collect::<Vec<String>>()
                        .join(" ")
                ));
            }

            println!("Instructions:\n{}", instructions.join("\n"));
            println!("Bytecode:");
            println!(
                "  Strings: {} total (Length Bytes: {})",
                num_strings,
                num_string_bytes
                    .iter()
                    .map(|b| format!("{:02X}", b))
                    .collect::<Vec<String>>()
                    .join(" ")
            );
            println!(
                "{}",
                strings.into_iter().collect::<Vec<String>>().join("\n")
            );
            println!("  Code:\n{}", instruction_bytes.join("\n"));
        }

        let decoded = decode_instructions(&encoded);

        {
            use std::io::Write;

            let mut file = std::fs::File::create("example.crunched").unwrap();
            file.write_all(&encoded).unwrap();
        }

        for (index, left) in instructions.iter().enumerate() {
            assert_eq!(left, &decoded[index]);
        }
        assert_eq!(decoded, instructions);
        println!("\nOutput:");
        let mut crunch = crate::Crunch::from(decoded);
        crunch.execute();
    }
}
