use super::INSTRUCTION_LENGTH;
use crate::{Instruction, Value, VALUE_LENGTH};
use std::mem::size_of;

// TODO: Document & Test all functions

pub fn encode_program(main: Vec<Instruction>, functions: Vec<Vec<Instruction>>) -> Vec<u8> {
    // Get the functions to a byte format and extract their values
    let (main_bytes, main_values) = encode_function(main);
    let (function_bytes, function_values) = encode_functions(functions);

    // Get the Values and Strings to a byte format
    let (main_value_bytes, main_value_strings) = encode_values(main_values);
    let (function_value_bytes, function_value_strings) = encode_values(function_values);

    // Initialize a vec to contain all of the raw data, sized to contain it all
    let mut output_vec = Vec::with_capacity(
        main_bytes.len()
            + main_value_bytes.len()
            + main_value_strings.len()
            + function_bytes.len()
            + function_value_bytes.len()
            + function_value_strings.len(),
    );

    // Just extends the consumer slice by all the victims
    macro_rules! append {
        ($consumer:tt < $($victim:tt,)*) => {{
            $(
                $consumer.extend_from_slice(&$victim);
            )*
        }};
    }

    append!(
        output_vec < function_value_strings,
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
    let mut value_bytes = Vec::with_capacity(size_of::<u32>() + (values.len() * VALUE_LENGTH));

    value_bytes.extend_from_slice(&(values.len() as u32).to_be_bytes());

    let mut value_strings = Vec::new();
    let mut number_strings = 0;

    for value in values {
        let (bytes, string) = value.as_bytes();

        value_bytes.extend_from_slice(&bytes);

        if let Some(string) = string {
            // Add the length of the string
            value_strings.extend_from_slice(&(string.len() as u32).to_be_bytes());

            // Add the string
            value_strings.extend_from_slice(string);

            // Increment the number of strings
            number_strings += 1;
        }
    }

    // Add the number of strings contained to the front of the strings
    let mut value_strings_raw = (number_strings as u32).to_be_bytes().as_ref().to_vec();
    value_strings_raw.extend_from_slice(&value_strings);

    (value_bytes, value_strings_raw)
}

fn encode_instruction(instruction: Instruction) -> ([u8; INSTRUCTION_LENGTH], Option<Value>) {
    let mut bytes = [0; INSTRUCTION_LENGTH];
    let mut value = None;

    match instruction {
        Instruction::Load(val, reg) => {
            bytes[0] = 0x00;
            bytes[size_of::<u32>() + 2] = *reg;
            value = Some(val);
        }
        Instruction::Cache(heap_loc, val, reg) => {
            bytes[0] = 0x01;
            bytes[1..size_of::<u32>() + 1].copy_from_slice(&heap_loc.to_be_bytes());
            bytes[size_of::<u32>() + 2] = *reg;
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
        Instruction::Syscall(offset, output, p1, p2, p3, p4, p5) => {
            bytes[0] = 0x19;
            bytes[1] = offset;
            bytes[2] = *output;
            bytes[3] = *p1;
            bytes[4] = *p2;
            bytes[5] = *p3;
            bytes[6] = *p4;
            bytes[7] = *p5;
        }

        Instruction::Illegal => {
            // TODO: Should this be allowed? What should an illegal instruction be legally encoded as?
            panic!(
                "I mean, why are you *purposefully* making an Illegal Instruction? Just... Why?"
            );
        }
    }

    (bytes, value)
}
