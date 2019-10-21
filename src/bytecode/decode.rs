use super::INSTRUCTION_LENGTH;
use crate::{Instruction, Value};
use std::{collections::VecDeque, convert::TryInto, mem::size_of};

pub fn decode_program(program: &[u8]) -> (Vec<Instruction>, Vec<Vec<Instruction>>) {
    let (function_strings, main_strings, program) = decode_strings(program);

    let (mut function_values, program) = decode_values(program, function_strings);
    let (mut main_values, program) = decode_values(program, main_strings);

    let number_functions = u32::from_be_bytes(program[0..size_of::<u32>()].try_into().unwrap());
    let mut functions = Vec::with_capacity(number_functions as usize);

    let mut program = &program[size_of::<u32>()..];

    if number_functions != 0 {
        for _ in 0..number_functions {
            let (function, prog) = decode_function(program, &mut function_values);
            functions.push(function);
            program = prog;
        }
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

    if number_instructions != 0 {
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
    }

    (instructions, &program[index..])
}

fn decode_values(program: &[u8], mut strings: VecDeque<String>) -> (VecDeque<Value>, &[u8]) {
    let mut index = 0;

    let number_values =
        u32::from_be_bytes(program[index..index + size_of::<u32>()].try_into().unwrap());
    index += size_of::<u32>();

    let mut values = VecDeque::with_capacity(number_values as usize);

    if number_values != 0 {
        for _ in 0..number_values {
            let bytes = program[index..index + 8].try_into().unwrap();

            let value = Value::from_bytes(bytes, &mut strings).unwrap();

            values.push_back(value);

            index += 8;
        }
    }

    (values, &program[index..])
}

fn decode_strings(program: &[u8]) -> (VecDeque<String>, VecDeque<String>, &[u8]) {
    let mut index = 0;

    let number_function_strings =
        u32::from_be_bytes(program[index..index + size_of::<u32>()].try_into().unwrap());
    index += size_of::<u32>();

    let mut function_strings = VecDeque::with_capacity(number_function_strings as usize);

    if number_function_strings != 0 {
        for _ in 0..number_function_strings {
            let string_len =
                u32::from_be_bytes(program[index..index + size_of::<u32>()].try_into().unwrap());
            index += size_of::<u32>();

            let string =
                String::from_utf8(program[index..index + string_len as usize].to_vec()).unwrap();

            function_strings.push_back(string);
        }
    }

    let number_main_strings =
        u32::from_be_bytes(program[index..index + size_of::<u32>()].try_into().unwrap());
    index += size_of::<u32>();

    let mut main_strings = VecDeque::with_capacity(number_main_strings as usize);
    println!("Num Main Strings: {}", number_main_strings);

    if number_main_strings != 0 {
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
    }

    (function_strings, main_strings, &program[index..])
}

fn decode_instruction(instruction: [u8; INSTRUCTION_LENGTH], value: Option<Value>) -> Instruction {
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
