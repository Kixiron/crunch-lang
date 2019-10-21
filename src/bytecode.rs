use super::{Instruction, Value};
use std::collections::VecDeque;

pub const INSTRUCTION_LENGTH: usize = 8;
#[cfg_attr(rustfmt, rustfmt::skip)]
pub const INSTRUCTION_BYTES: [u8; 25] = [
    0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 
    0x06, 0x07, 0x08, 0x09, 0x0A, 0x0B, 
    0x0C, 0x0D, 0x0E, 0x0F, 0x10, 0x11, 
    0x12, 0x13, 0x14, 0x15, 0x16, 0x17,
    0x18
];

#[inline]
fn decode_instruction(instruction: [u8; INSTRUCTION_LENGTH], value: Option<Value>) -> Instruction {
    use std::{convert::TryInto, mem::size_of};

    let instruction = match instruction[0] {
        0x00 => Instruction::Load(
            u32::from_be_bytes(instruction[1..size_of::<u32>() + 1].try_into().unwrap()),
            instruction[size_of::<u32>() + 2].into(),
        ),
        0x01 => Instruction::Cache(
            u32::from_be_bytes(instruction[1..size_of::<u32>() + 1].try_into().unwrap()),
            value.unwrap(),
        ),
        0x02 => Instruction::CompToReg(instruction[1].into()),
        0x03 => Instruction::OpToReg(instruction[1].into()),
        0x04 => Instruction::DropReg(instruction[1].into()),
        0x05 => Instruction::Drop(u32::from_be_bytes(
            instruction[1..size_of::<u32>() + 1].try_into().unwrap(),
        )),

        0x06 => Instruction::Add(instruction[1].into(), instruction[2].into()),
        0x07 => Instruction::Sub(instruction[1].into(), instruction[2].into()),
        0x08 => Instruction::Mult(instruction[1].into(), instruction[2].into()),
        0x09 => Instruction::Div(instruction[1].into(), instruction[2].into()),

        0x0A => Instruction::Print(instruction[1].into()),

        0x0B => Instruction::Jump(i32::from_be_bytes(
            instruction[1..size_of::<i32>() + 1].try_into().unwrap(),
        )),
        0x0C => Instruction::JumpComp(i32::from_be_bytes(
            instruction[1..size_of::<i32>() + 1].try_into().unwrap(),
        )),

        0x0D => Instruction::And(instruction[1].into(), instruction[2].into()),
        0x0E => Instruction::Or(instruction[1].into(), instruction[2].into()),
        0x0F => Instruction::Xor(instruction[1].into(), instruction[2].into()),
        0x10 => Instruction::Not(instruction[1].into()),

        0x11 => Instruction::Eq(instruction[1].into(), instruction[2].into()),
        0x12 => Instruction::NotEq(instruction[1].into(), instruction[2].into()),
        0x13 => Instruction::GreaterThan(instruction[1].into(), instruction[2].into()),
        0x14 => Instruction::LessThan(instruction[1].into(), instruction[2].into()),

        0x15 => Instruction::Return,
        0x16 => Instruction::Halt,

        0x17 => Instruction::Save(
            u32::from_be_bytes(instruction[1..size_of::<u32>() + 1].try_into().unwrap()),
            instruction[size_of::<u32>() + 2].into(),
        ),
        0x18 => Instruction::Collect,

        _ => Instruction::Illegal,
    };

    instruction
}

#[inline]
fn encode_instruction(instruction: Instruction) -> ([u8; INSTRUCTION_LENGTH], Option<Value>) {
    let mut bytes = [0; INSTRUCTION_LENGTH];
    let mut value = None;

    match instruction {
        Instruction::Load(heap_loc, reg) => {
            bytes[0] = 0x00;
            bytes[1..size_of::<u32>() + 1].copy_from_slice(&heap_loc.to_be_bytes());
            bytes[size_of::<u32>() + 2] = *reg;
        }
        Instruction::Cache(heap_loc, val) => {
            bytes[0] = 0x01;
            bytes[1..size_of::<u32>() + 1].copy_from_slice(&heap_loc.to_be_bytes());
            value = Some(val);
        }
        Instruction::Save(heap_loc, reg) => {
            bytes[0] = 0x17;
            bytes[1..size_of::<u32>() + 1].copy_from_slice(&heap_loc.to_be_bytes());
            bytes[size_of::<u32>() + 2] = *reg;
        }
        Instruction::CompToReg(reg) => {
            bytes[0] = 0x02;
            bytes[1] = *reg;
        }
        Instruction::OpToReg(reg) => {
            bytes[0] = 0x03;
            bytes[1] = *reg;
        }
        Instruction::DropReg(reg) => {
            bytes[0] = 0x04;
            bytes[1] = *reg;
        }
        Instruction::Drop(reg) => {
            bytes[0] = 0x05;
            bytes[1..size_of::<u32>() + 1].copy_from_slice(&reg.to_be_bytes());
        }

        Instruction::Add(left, right) => {
            bytes[0] = 0x06;
            bytes[1] = *left;
            bytes[2] = *right;
        }
        Instruction::Sub(left, right) => {
            bytes[0] = 0x07;
            bytes[1] = *left;
            bytes[2] = *right;
        }
        Instruction::Mult(left, right) => {
            bytes[0] = 0x08;
            bytes[1] = *left;
            bytes[2] = *right;
        }
        Instruction::Div(left, right) => {
            bytes[0] = 0x09;
            bytes[1] = *left;
            bytes[2] = *right;
        }

        Instruction::Print(reg) => {
            bytes[0] = 0x0A;
            bytes[1] = *reg;
        }

        Instruction::Jump(loc) => {
            bytes[0] = 0x0B;
            bytes[1..size_of::<i32>() + 1].copy_from_slice(&loc.to_be_bytes());
        }
        Instruction::JumpComp(loc) => {
            bytes[0] = 0x0C;
            bytes[1..size_of::<i32>() + 1].copy_from_slice(&loc.to_be_bytes());
        }

        Instruction::And(left, right) => {
            bytes[0] = 0x0D;
            bytes[1] = *left;
            bytes[2] = *right;
        }
        Instruction::Or(left, right) => {
            bytes[0] = 0x0E;
            bytes[1] = *left;
            bytes[2] = *right;
        }
        Instruction::Xor(left, right) => {
            bytes[0] = 0x0F;
            bytes[1] = *left;
            bytes[2] = *right;
        }
        Instruction::Not(reg) => {
            bytes[0] = 0x10;
            bytes[1] = *reg;
        }

        Instruction::Eq(left, right) => {
            bytes[0] = 0x11;
            bytes[1] = *left;
            bytes[2] = *right;
        }
        Instruction::NotEq(left, right) => {
            bytes[0] = 0x12;
            bytes[1] = *left;
            bytes[2] = *right;
        }
        Instruction::GreaterThan(left, right) => {
            bytes[0] = 0x13;
            bytes[1] = *left;
            bytes[2] = *right;
        }
        Instruction::LessThan(left, right) => {
            bytes[0] = 0x14;
            bytes[1] = *left;
            bytes[2] = *right;
        }

        Instruction::Collect => {
            bytes[0] = 0x18;
        }
        Instruction::Return => {
            bytes[0] = 0x15;
        }
        Instruction::Halt => {
            bytes[0] = 0x16;
        }

        Instruction::Illegal => unreachable!(
            "I mean, why are you *purposefully* making an Illegal Instruction? Just... Why?"
        ),
    }

    (bytes, value)
}

macro_rules! append {
    ($consumer:tt, $($victim:tt,)*) => {{
        $(
            $consumer.extend_from_slice(&$victim);
        )*
    }};
}

use std::{convert::TryInto, mem::size_of};

pub fn decode_program(program: &[u8]) -> (Vec<Instruction>, Vec<Vec<Instruction>>) {
    let (function_strings, main_strings, program) = decode_strings(program);

    let (mut function_values, program) = decode_values(program, function_strings);
    let (mut main_values, program) = decode_values(program, main_strings);

    let number_functions = u32::from_be_bytes(program[0..size_of::<u32>()].try_into().unwrap());
    let mut functions = Vec::with_capacity(number_functions as usize);

    let mut program = &program[size_of::<u32>()..];
    for _ in 0..number_functions {
        let (function, prog) = decode_function(program, &mut function_values);
        functions.push(function);
        program = prog;
    }

    let (main, _program) = decode_function(program, &mut main_values);

    (main, functions)
}

fn decode_function<'a>(
    program: &'a [u8],
    values: &mut VecDeque<Value>,
) -> (Vec<Instruction>, &'a [u8]) {
    let mut index = 0;

    let number_instructions =
        u32::from_be_bytes(program[index..index + size_of::<u32>()].try_into().unwrap());
    index += size_of::<u32>();

    let mut instructions = Vec::with_capacity(number_instructions as usize);

    for _ in 0..number_instructions {
        let bytes: [u8; INSTRUCTION_LENGTH] = program[index..index + INSTRUCTION_LENGTH]
            .try_into()
            .unwrap();

        let value = if bytes[0] == 0x01 {
            Some(values.pop_front().unwrap())
        } else {
            None
        };

        instructions.push(decode_instruction(bytes, value));

        index += INSTRUCTION_LENGTH;
    }

    (instructions, &program[index..])
}

fn decode_values(program: &[u8], mut strings: VecDeque<String>) -> (VecDeque<Value>, &[u8]) {
    let mut index = 0;

    let number_values =
        u32::from_be_bytes(program[index..index + size_of::<u32>()].try_into().unwrap());
    index += size_of::<u32>();

    let mut values = VecDeque::with_capacity(number_values as usize);

    for _ in 0..number_values {
        let bytes = program[index..index + 8].try_into().unwrap();

        let value = Value::from_bytes(bytes, &mut strings).unwrap();

        values.push_back(value);

        index += 8;
    }

    (values, &program[index..])
}

fn decode_strings(program: &[u8]) -> (VecDeque<String>, VecDeque<String>, &[u8]) {
    let mut index = 0;

    let number_function_strings =
        u32::from_be_bytes(program[index..index + size_of::<u32>()].try_into().unwrap());
    index += size_of::<u32>();

    let mut function_strings = VecDeque::with_capacity(number_function_strings as usize);

    for _ in 0..number_function_strings {
        let string_len =
            u32::from_be_bytes(program[index..index + size_of::<u32>()].try_into().unwrap());
        index += size_of::<u32>();

        let string =
            String::from_utf8(program[index..index + string_len as usize].to_vec()).unwrap();

        function_strings.push_back(string);
    }

    let number_main_strings =
        u32::from_be_bytes(program[index..index + size_of::<u32>()].try_into().unwrap());
    index += size_of::<u32>();

    let mut main_strings = VecDeque::with_capacity(number_main_strings as usize);

    for _ in 0..number_main_strings {
        let string_len =
            u32::from_be_bytes(program[index..index + size_of::<u32>()].try_into().unwrap());
        index += size_of::<u32>();

        println!(
            "String Length: {}; Index + String: {}",
            string_len,
            index + string_len as usize
        );

        let string =
            String::from_utf8(program[index..index + string_len as usize].to_vec()).unwrap();

        main_strings.push_back(string);
    }

    (function_strings, main_strings, &program[index..])
}

pub fn encode_program(main: Vec<Instruction>, functions: Vec<Vec<Instruction>>) -> Vec<u8> {
    let (main_bytes, main_values) = encode_function(main);
    let (function_bytes, function_values) = encode_functions(functions);

    let (main_value_bytes, main_value_strings) = encode_values(main_values);
    let (function_value_bytes, function_value_strings) = encode_values(function_values);

    let mut output_vec = Vec::with_capacity(
        main_bytes.len()
            + main_value_bytes.len()
            + main_value_strings.len()
            + function_bytes.len()
            + function_value_bytes.len()
            + function_value_strings.len(),
    );

    // Strings
    //      Functions
    //      Main
    // Values
    //      Functions
    //      Main
    // Instructions
    //      Functions
    //      Main

    append!(
        output_vec,
        function_value_strings,
        main_value_strings,
        function_value_bytes,
        main_value_bytes,
        function_bytes,
        main_bytes,
    );

    output_vec
}

fn encode_functions(functions: Vec<Vec<Instruction>>) -> (Vec<u8>, Vec<Value>) {
    // Create the output vector of bytes
    let mut output_bytes =
        Vec::with_capacity(size_of::<u32>() + (functions.len() * INSTRUCTION_LENGTH));

    // Create the output vector of values
    let mut output_values = Vec::new();

    // Add the number of functions to output_bytes
    output_bytes.extend_from_slice(&(functions.len() as u32).to_be_bytes());

    // For each function, encode all of its instructions and capture their values
    for function in functions {
        let (bytes, values) = encode_function(function);

        output_bytes.extend_from_slice(&bytes);
        output_values.extend_from_slice(&values);
    }

    (output_bytes, output_values)
}

fn encode_function(function: Vec<Instruction>) -> (Vec<u8>, Vec<Value>) {
    // Create the output vector of bytes
    let mut output_bytes =
        Vec::with_capacity(size_of::<u32>() + (function.len() * INSTRUCTION_LENGTH));

    // Add the length of the function to the output bytes
    output_bytes.extend_from_slice(&(function.len() as u32).to_be_bytes());

    // Create the output vector of values
    let mut output_values = Vec::new();

    // For each instruction, encode it and capture the Value (If any)
    for instruction in function {
        let (bytes, value) = encode_instruction(instruction);

        output_bytes.extend_from_slice(&bytes);

        if let Some(value) = value {
            output_values.push(value);
        }
    }

    (output_bytes, output_values)
}

fn encode_values(values: Vec<Value>) -> (Vec<u8>, Vec<u8>) {
    let mut value_bytes = Vec::with_capacity(size_of::<u32>() + (values.len() * 8));

    value_bytes.extend_from_slice(&(values.len() as u32).to_be_bytes());

    let mut value_strings = Vec::new();

    for value in values {
        let (bytes, string) = value.as_bytes();

        value_bytes.extend_from_slice(&bytes);

        if let Some(string) = string {
            // Add the length of the string
            value_strings.extend_from_slice(&(string.len() as u32).to_be_bytes());

            // Add the string
            value_strings.extend_from_slice(string);
        }
    }

    (value_bytes, value_strings)
}

pub fn disassemble(bytes: &[u8]) -> String {
    use super::NUMBER_REGISTERS;
    use std::{collections::HashMap, fmt::Write};

    let functions = {
        let (main, functions) = decode_program(bytes);
        let mut funcs = vec![main];
        funcs.extend_from_slice(&functions);
        funcs
    };

    let mut output = String::new();
    for (index, function) in functions.into_iter().enumerate() {
        if index == 0 {
            write!(&mut output, "=> Main Function\n").unwrap();
        } else {
            write!(&mut output, "=> Function {}\n", index).unwrap();
        }

        let mut registers: [Value; NUMBER_REGISTERS] = array_init::array_init(|_| Value::None);
        let mut heap: HashMap<u32, Value> = HashMap::new();

        for (instruction_index, instruction) in function.into_iter().enumerate() {
            match &instruction {
                Instruction::Load(heap_loc, reg) => {
                    registers[**reg as usize] = heap.get(&heap_loc).unwrap_or(&Value::None).clone();
                }
                Instruction::Cache(heap_loc, val) => {
                    heap.insert(*heap_loc, val.clone());
                }
                Instruction::Drop(heap_loc) => {
                    heap.remove(&heap_loc);
                }
                Instruction::DropReg(reg) => {
                    registers[**reg as usize] = Value::None;
                }
                Instruction::Save(heap_loc, reg) => {
                    heap.insert(*heap_loc, registers[**reg as usize].clone());
                    registers[**reg as usize] = Value::None;
                }
                _ => {}
            }

            let param = {
                use Instruction::*;

                match &instruction {
                    Load(heap, reg) | Save(heap, reg) => {
                        format!("{:p}, {}", *heap as *const u8, reg)
                    }
                    Cache(heap, ref val) => format!("{:p}, {}", *heap as *const u8, val),
                    CompToReg(reg) | OpToReg(reg) | DropReg(reg) => format!("{}", reg),
                    Drop(reg) => format!("{}", reg),

                    Print(reg) => {
                        if registers[**reg as usize] != Value::None {
                            format!("{}: {}", reg, registers[**reg as usize].to_string())
                        } else {
                            format!("{}", reg)
                        }
                    }

                    Jump(abs) | JumpComp(abs) => format!("{:04}", abs),

                    Not(reg) => format!(
                        "{}",
                        if registers[**reg as usize] != Value::None {
                            format!("{}: {}", reg, registers[**reg as usize].to_string())
                        } else {
                            format!("{}", reg)
                        }
                    ),

                    Add(left, right)
                    | Sub(left, right)
                    | Mult(left, right)
                    | Div(left, right)
                    | And(left, right)
                    | Or(left, right)
                    | Xor(left, right)
                    | Eq(left, right)
                    | NotEq(left, right)
                    | GreaterThan(left, right)
                    | LessThan(left, right) => format!(
                        "{}, {}",
                        if registers[**left as usize] != Value::None {
                            format!("{}: {:?}", left, registers[**left as usize].to_string())
                        } else {
                            format!("{}", left)
                        },
                        if registers[**right as usize] != Value::None {
                            format!("{}: {:?}", right, registers[**right as usize].to_string())
                        } else {
                            format!("{}", right)
                        }
                    ),

                    Return | Halt | Illegal | Collect => String::new(),
                }
            };

            write!(
                &mut output,
                "  {:04}: {} {}\n",
                instruction_index,
                instruction.to_str(),
                param,
            )
            .unwrap();
        }
    }

    output
}

#[inline]
fn decode_instructions(bytes: &[u8], values: &mut VecDeque<Value>) -> Vec<Instruction> {
    use std::convert::TryInto;

    let mut instructions = Vec::with_capacity(bytes.len() / INSTRUCTION_LENGTH);
    for chunk in bytes.chunks(INSTRUCTION_LENGTH) {
        let chunk: [u8; INSTRUCTION_LENGTH] = chunk.try_into().expect("Invalid chunk length");

        let instruction = if chunk[0] == 0x01 {
            decode_instruction(chunk, values.pop_front())
        } else {
            decode_instruction(chunk, None)
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
    fn byte_test() {
        use crate::Instruction::*;
        use std::io::Write;

        simple_logger::init().unwrap();
        color_backtrace::install();

        let (instructions, functions) = (
            vec![
                Cache(0, Value::Int(10)),
                Load(0, 0.into()),
                Print(0.into()),
                Cache(1, Value::Int(5)),
                Load(1, 1.into()),
                Print(1.into()),
                Div(0.into(), 1.into()),
                OpToReg(3.into()),
                Print(3.into()),
                Drop(0),
                Drop(1),
                Collect,
                Halt,
            ],
            Vec::new(),
        );

        let encoded_program = encode_program(instructions.clone(), functions.clone());
        let decoded_program = decode_program(&encoded_program);

        let mut file = std::fs::File::create("./examples/hello_world.crunched").unwrap();
        file.write_all(&encoded_program).unwrap();

        println!("{}", disassemble(&encoded_program));

        assert_eq!((instructions, functions), decoded_program);
        let mut crunch = crate::Crunch::from((
            decoded_program.0,
            decoded_program.1,
            crate::OptionBuilder::new("./byte_test").build(),
        ));
        crunch.execute();
    }
}
