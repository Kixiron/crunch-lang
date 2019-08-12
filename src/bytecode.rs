use super::{Instruction, LoadedString, Register, StringPointer};

fn decode(
    instruction: [u8; 9],
    strings: &mut Vec<Option<String>>,
    str_index: &mut usize,
) -> Instruction {
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
        0x01 => {
            let string = if let Some(ref string) = strings[*str_index] {
                string.clone()
            } else {
                *str_index += 1;
                strings[*str_index].clone().expect("No avaliable string")
            };
            *str_index += 1;

            Instruction::LoadStr(LoadedString::new_boxed(
                string,
                StringPointer(instruction[2]),
                Register(instruction[1]),
            ))
        }
        0x02 => Instruction::LoadBool(instruction[1] != 0, Register(instruction[2])),
        0x03 => Instruction::Drop(Register(instruction[1])),
        0x04 => Instruction::DropStr(StringPointer(instruction[1])),
        0x05 => Instruction::AddStr {
            left: StringPointer(instruction[1]),
            right: StringPointer(instruction[2]),
        },
        0x06 => Instruction::AddInt {
            left: Register(instruction[1]),
            right: Register(instruction[2]),
        },
        0x07 => Instruction::SubInt {
            left: Register(instruction[1]),
            right: Register(instruction[2]),
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
            reg: Register(instruction[8]),
        },
        0x0B => Instruction::Halt,
        _ => unsafe { std::hint::unreachable_unchecked() },
    }
}

fn encode(instruction: &Instruction) -> ([u8; 9], Option<String>) {
    let mut bytes = [0; 9];
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
        Instruction::AddStr { left, right } => {
            bytes[0] = 0x05;
            bytes[1] = **left;
            bytes[2] = **right;
        }
        Instruction::AddInt { left, right } => {
            bytes[0] = 0x06;
            bytes[1] = **left;
            bytes[2] = **right;
        }
        Instruction::SubInt { left, right } => {
            bytes[0] = 0x07;
            bytes[1] = **left;
            bytes[2] = **right;
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
            bytes[8] = **reg;
        }
        Instruction::Halt => {
            bytes[0] = 0x0B;
        }
    }

    (bytes, string)
}

pub fn encode_instructions(instructions: &[Instruction]) -> Vec<u8> {
    let mut instruction_bytes = vec![0_u8; instructions.len() * 9];
    let mut instruction_strings: Vec<String> = Vec::default();

    for instruction in instructions {
        let (bytes, string) = encode(instruction);

        instruction_bytes.extend_from_slice(&bytes);
        if let Some(string) = string {
            instruction_strings.push(string);
        }
    }

    let mut string_bytes: Vec<u8> = Vec::new();
    string_bytes.extend_from_slice(&instruction_strings.len().to_be_bytes());
    for string in instruction_strings {
        string_bytes.extend_from_slice(&string.len().to_be_bytes());
        string_bytes.extend_from_slice(string.as_bytes());
    }

    string_bytes.extend_from_slice(&instruction_bytes);
    string_bytes
}

pub fn decode_instructions(bytes: &[u8]) -> Vec<Instruction> {
    use std::convert::TryInto;

    let mut index = 0_usize;
    let num_strings = take_usize(&mut index, bytes);
    let mut strings = Vec::with_capacity(num_strings);

    for _ in 0..num_strings {
        let len = take_usize(&mut index, bytes);

        strings.push(Some(
            String::from_utf8(bytes[index..index + len].to_vec()).expect("Invalid string"),
        ));
        index += len;
    }

    let mut instructions = Vec::with_capacity(
        usize::from_be_bytes(
            bytes[index..]
                .try_into()
                .unwrap_or_else(|_| unsafe { std::hint::unreachable_unchecked() }),
        ) / 9,
    );
    let mut str_index = 0_usize;

    for chunk in bytes[index..].chunks(9) {
        let chunk = chunk
            .try_into()
            .unwrap_or_else(|_| unsafe { std::hint::unreachable_unchecked() });
        instructions.push(decode(chunk, &mut strings, &mut str_index));
    }

    instructions
}

fn take_usize(index: &mut usize, bytes: &[u8]) -> usize {
    use std::convert::TryInto;

    let int = usize::from_be_bytes(
        bytes[*index..*index + 8]
            .try_into()
            .unwrap_or_else(|_| unsafe { std::hint::unreachable_unchecked() }),
    );

    *index += 8;

    int
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Instruction::*;

    #[test]
    fn test() {
        use std::convert::TryInto;

        let instructions = vec![
            LoadInt(10, Register(0)),
            Print(Register(0)),
            Drop(Register(0)),
            LoadBool(false, Register(0)),
            CondJump {
                index: 2,
                reg: Register(0),
            },
            Print(Register(0)),
            Drop(Register(0)),
            LoadStr(LoadedString::new_boxed(
                "test".to_string(),
                StringPointer(0),
                Register(0),
            )),
            Print(Register(0)),
            DropStr(StringPointer(0)),
            Drop(Register(0)),
            Halt,
        ];
        println!("here");
        let encoded = encode_instructions(&instructions);

        println!("{:?}", encoded);

        let decoded = decode_instructions(&encoded);

        println!("{:#?}", decoded);
        assert_eq!(decoded, instructions);

        let mut crunch = crate::Crunch::from(decoded);
        crunch.execute();
    }
}
