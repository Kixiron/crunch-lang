use super::{Instruction, Register, StringPointer};
use std::{borrow::Cow, collections::VecDeque};

pub const INSTRUCTION_LENGTH: usize = 8;
pub const INSTRUCTION_BYTES: [u8; 22] = [
    0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F,
    0x10, 0x11, 0x12, 0x13, 0x14, 0x15,
];

#[inline]
fn decode(instruction: [u8; INSTRUCTION_LENGTH], string: Option<&'static str>) -> Instruction {
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
        0x01 => Instruction::LoadStr(
            string.expect("Improper string distribution"),
            StringPointer(instruction[2]),
            Register(instruction[1]),
        ),
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
        0x12 => Instruction::FuncReturn,
        0x13 => Instruction::FuncCall(
            u32::from_be_bytes(
                instruction[1..5]
                    .try_into()
                    .unwrap_or_else(|_| unsafe { std::hint::unreachable_unchecked() }),
            )
            .into(),
        ),
        0x14 => Instruction::LoadHandoff(Register(instruction[1]), Register(instruction[2])),
        0x15 => Instruction::TakeHandoff(Register(instruction[1]), Register(instruction[2])),

        _ => unsafe { std::hint::unreachable_unchecked() },
    }
}

#[inline]
fn encode(instruction: &Instruction) -> ([u8; INSTRUCTION_LENGTH], Option<Cow<'static, str>>) {
    let mut bytes = [0; INSTRUCTION_LENGTH];
    let mut string = None;

    match instruction {
        Instruction::LoadInt(int, reg) => {
            bytes[0] = 0x00;
            bytes[1..5].copy_from_slice(&int.to_be_bytes());
            bytes[6] = **reg;
        }
        Instruction::LoadStr(input_string, ptr, reg) => {
            bytes[0] = 0x01;
            bytes[1] = **reg;
            bytes[2] = **ptr;

            string = Some(Cow::Borrowed(*input_string));
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
        Instruction::FuncReturn => {
            bytes[0] = 0x12;
        }
        Instruction::FuncCall(index) => {
            bytes[0] = 0x13;
            bytes[1..5].copy_from_slice(&index.to_be_bytes());
        }
        Instruction::LoadHandoff(output_reg, handoff_reg) => {
            bytes[0] = 0x14;
            bytes[1] = **output_reg;
            bytes[2] = **handoff_reg;
        }
        Instruction::TakeHandoff(handoff_reg, destination_reg) => {
            bytes[0] = 0x15;
            bytes[1] = **handoff_reg;
            bytes[2] = **destination_reg;
        }
    }

    (bytes, string)
}

#[inline]
pub fn encode_program(main: &[Instruction], functions: &[Vec<Instruction>]) -> Vec<u8> {
    // Encode the main function and strings
    let (main_bytes, main_strings) = encode_instructions(main);

    let (func_bytes, func_strings) = {
        let (mut func_bytes, mut func_strings) = (Vec::new(), Vec::new());

        func_bytes.extend_from_slice(&(functions.len() as u32).to_be_bytes());

        // Encode each function to bytecode and store all their strings
        for function in functions {
            // Encode the function
            let (bytes, strings) = encode_instructions(function);
            // Push the length of the function to the stored bytes
            func_bytes.extend_from_slice(&(function.len() as u32).to_be_bytes());
            // Push the instructions to the stored bytes
            func_bytes.extend_from_slice(&bytes);
            // Push the strings to the stored strings
            func_strings.extend_from_slice(&strings);
        }

        (func_bytes, func_strings)
    };

    // Encode the strings
    let mut strings = {
        // Combine the function strings and the main function strings
        let mut strings = func_strings;
        strings.extend_from_slice(&main_strings);

        // Prepare vec for encoded strings
        let mut string_bytes: Vec<u8> = Vec::new();
        string_bytes.extend_from_slice(&(strings.len() as u32).to_be_bytes());

        for string in strings {
            string_bytes.extend_from_slice(&(string.len() as u32).to_be_bytes());
            string_bytes.extend_from_slice(string.as_bytes());
        }

        string_bytes
    };

    strings.extend_from_slice(&func_bytes);
    strings.extend_from_slice(&main_bytes);

    // Layout:
    // Strings
    //     Function Strings
    //     Main Strings
    // Functions
    // Main
    strings
}

#[inline]
pub fn decode_program(bytes: &[u8]) -> (Vec<Instruction>, Vec<Vec<Instruction>>) {
    let mut index = 0;

    let mut strings = {
        let num_strings = take_usize(&mut index, bytes);
        let mut strings = VecDeque::with_capacity(num_strings);

        for _ in 0..num_strings {
            let len = take_usize(&mut index, bytes);

            strings.push_back(
                std::str::from_utf8(unsafe {
                    std::mem::transmute::<_, _>(&bytes[index..index + len])
                })
                .expect("Invalid string"),
            );
            index += len;
        }

        strings
    };

    let functions = {
        let num_functions = take_usize(&mut index, bytes);
        let mut functions = Vec::with_capacity(num_functions);

        for _ in 0..num_functions {
            let len = take_usize(&mut index, bytes);

            functions.push(decode_instructions(
                &bytes[index..index + (len * INSTRUCTION_LENGTH)],
                &mut strings,
            ));

            index += len * INSTRUCTION_LENGTH;
        }

        functions
    };

    let main = decode_instructions(&bytes[index..], &mut strings);

    (main, functions)
}

#[inline]
fn encode_instructions(instructions: &[Instruction]) -> (Vec<u8>, Vec<String>) {
    let mut instruction_bytes = vec![0_u8; instructions.len() * INSTRUCTION_LENGTH];
    let mut instruction_strings: Vec<String> = Vec::default();

    for (index, instruction) in instructions.iter().enumerate() {
        let (bytes, string) = encode(instruction);

        instruction_bytes
            [index * INSTRUCTION_LENGTH..(index * INSTRUCTION_LENGTH) + INSTRUCTION_LENGTH]
            .copy_from_slice(&bytes);
        if let Some(string) = string {
            instruction_strings.push(string.into_owned());
        }
    }

    (instruction_bytes, instruction_strings)
}

#[inline]
fn decode_instructions(bytes: &[u8], strings: &mut VecDeque<&'static str>) -> Vec<Instruction> {
    use std::convert::TryInto;

    let mut instructions = Vec::with_capacity(bytes.len() / INSTRUCTION_LENGTH);
    for chunk in bytes.chunks(INSTRUCTION_LENGTH) {
        let chunk: [u8; INSTRUCTION_LENGTH] = chunk.try_into().expect("Invalid chunk length");

        let instruction = if chunk[0] == 0x01 {
            decode(chunk, strings.pop_front())
        } else {
            decode(chunk, None)
        };
        instructions.push(instruction);
    }

    instructions
}

#[inline]
fn take_usize(index: &mut usize, bytes: &[u8]) -> usize {
    use std::convert::TryInto;

    let int = u32::from_be_bytes(
        bytes[*index..*index + 4]
            .try_into()
            .expect("Invalid u32 length"),
    ) as usize;

    *index += 4;

    int
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        use crate::{Index, Instruction::*};

        // simple_logger::init().unwrap();
        color_backtrace::install();

        let take_usize = |index: &mut usize, bytes: &[u8]| -> (usize, [u8; 4]) {
            use std::convert::TryInto;

            let bytes = bytes[*index..*index + 4]
                .try_into()
                .expect("Invalid u32 length");

            let int = u32::from_be_bytes(bytes) as usize;

            *index += 4;

            (int, bytes)
        };

        let (instructions, functions) = (
            vec![
                LoadStr("Hello from Crunch!", StringPointer(1), Register(0)),
                LoadHandoff(Register(0), Register(0)),
                FuncCall(Index(0)),
                Halt,
            ],
            vec![
                // println
                vec![
                    LoadStr("\n", StringPointer(0), Register(0)),
                    TakeHandoff(Register(0), Register(1)),
                    Print(Register(1)),
                    Print(Register(0)),
                    FuncReturn,
                ],
            ],
        );

        let encoded_program = encode_program(&instructions, &functions);

        {
            let display_bytes = |b: &[u8]| -> String {
                b.iter()
                    .map(|b| format!("{:02X}", b))
                    .collect::<Vec<String>>()
                    .join(" ")
            };

            let mut index = 0;

            let (mut strings, disp_strings) = {
                let (mut disp_strings, (num_strings, num_strings_bytes), mut strings) = (
                    String::new(),
                    take_usize(&mut index, &encoded_program),
                    VecDeque::new(),
                );

                disp_strings += &format!(
                    "Strings: (Number: {}, Bytes: {})\n",
                    num_strings,
                    display_bytes(&num_strings_bytes)
                );

                for i in 0..num_strings {
                    let (str_len, str_len_bytes) = take_usize(&mut index, &encoded_program);
                    let bytes = &encoded_program[index..index + str_len];
                    index += str_len;

                    disp_strings += &format!(
                        "  {:04}: Len: {}, Bytes: {}\n    String: {:?}\n    Bytes: {}\n",
                        i,
                        str_len,
                        display_bytes(&str_len_bytes),
                        String::from_utf8(bytes.to_vec()).unwrap(),
                        display_bytes(&bytes),
                    );

                    strings.push_back(
                        std::str::from_utf8(unsafe {
                            std::mem::transmute::<_, &'static [u8]>(bytes)
                        })
                        .unwrap(),
                    );
                }

                (strings, disp_strings)
            };

            let functions = {
                let (mut functions, (num_functions, num_functions_bytes)) =
                    (String::new(), take_usize(&mut index, &encoded_program));

                functions += &format!(
                    "Functions: (Number: {}, Bytes: {})\n",
                    num_functions,
                    display_bytes(&num_functions_bytes)
                );

                for i in 0..num_functions {
                    let (func_len, func_len_bytes) = take_usize(&mut index, &encoded_program);

                    functions += &format!(
                        "  {:04}: Len: {}, Bytes: {}\n    Instructions:\n{}\n    Bytes:\n{}\n",
                        i,
                        func_len,
                        display_bytes(&func_len_bytes),
                        decode_instructions(
                            &encoded_program[index..index + (func_len * INSTRUCTION_LENGTH)],
                            &mut strings,
                        )
                        .iter()
                        .enumerate()
                        .map(|(index, i)| format!("      {:04}: {:?}", index, i))
                        .collect::<Vec<String>>()
                        .join("\n"),
                        (&encoded_program[index..index + (func_len * INSTRUCTION_LENGTH)])
                            .chunks(INSTRUCTION_LENGTH)
                            .enumerate()
                            .map(|(index, c)| format!("      {:04}: {}", index, display_bytes(&c)))
                            .collect::<Vec<String>>()
                            .join("\n"),
                    );

                    index += func_len * INSTRUCTION_LENGTH;
                }

                functions
            };

            let main = {
                let mut main = String::new();

                main += "Main Function:\n  Instructions:\n";

                let instructions = decode_instructions(&encoded_program[index..], &mut strings);
                for (index, instruction) in instructions.iter().enumerate() {
                    main += &format!("    {:04}: {:?}\n", index, instruction);
                }

                main += "  Bytes:\n";
                for (index, bytes) in encoded_program[index..]
                    .chunks(INSTRUCTION_LENGTH)
                    .enumerate()
                {
                    main += &format!("    {:04}: {}\n", index, display_bytes(&bytes));
                }

                main
            };

            println!("\n{}", disp_strings);
            println!("{}", functions);
            println!("{}", main);
            println!("Output:");
        }

        let decoded_program = decode_program(&encoded_program);

        assert_eq!((instructions, functions), decoded_program);
        let mut crunch = crate::Crunch::from(decoded_program);
        crunch.execute();
    }
}
