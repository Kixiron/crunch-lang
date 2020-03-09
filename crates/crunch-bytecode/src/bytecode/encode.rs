use super::INSTRUCTION_LENGTH;

use compactor::{Instruction, Value};
use crunch_error::log::error;

use alloc::vec::Vec;
use core::mem::{self, size_of};

// TODO: Document & Test all functions

#[derive(Debug, Clone)]
pub struct Encoder {
    functions: Vec<Vec<Instruction>>,
    bytes: Vec<u8>,
    strings: Vec<u8>,
    values: Vec<u8>,
    num_functions: usize,
}

impl Encoder {
    #[must_use]
    pub fn new(functions: Vec<Vec<Instruction>>) -> Self {
        let len = functions.iter().map(Vec::len).sum::<usize>() * INSTRUCTION_LENGTH;
        let num_functions = functions.len();

        Self {
            functions,
            bytes: Vec::with_capacity(len),
            strings: Vec::new(),
            values: Vec::new(),
            num_functions,
        }
    }

    #[must_use]
    pub fn encode(mut self) -> Vec<u8> {
        self.bytes
            .extend_from_slice(&self.num_functions.to_be_bytes());

        let (mut bytes, values) = self.encode_functions();
        self.encode_values(values);

        self.bytes.append(&mut self.strings);
        self.bytes.append(&mut self.values);
        self.bytes.append(&mut bytes);

        self.bytes
    }

    fn encode_values(&mut self, orig_values: Vec<Value>) {
        let mut values =
            Vec::with_capacity(size_of::<u32>() + (orig_values.len() * size_of::<Value>()));
        values.extend_from_slice(&(orig_values.len() as u32).to_be_bytes());

        let mut strings = Vec::new();
        let mut len = 0;

        for value in orig_values {
            let (val_bytes, string) = value.as_bytes();

            values.extend_from_slice(&val_bytes);

            if let Some(string) = string {
                // Add the length of the string
                strings.extend_from_slice(&(string.len() as u32).to_be_bytes());

                // Add the string
                strings.extend_from_slice(string.as_bytes());

                // Increment the number of strings
                len += 1;
            }
        }

        // Add the number of strings contained to the front of the strings
        let mut strings_raw = Vec::with_capacity(size_of::<u32>() + strings.len());
        strings_raw.extend_from_slice(&(len as u32).to_be_bytes());
        strings_raw.append(&mut strings);

        self.strings.append(&mut strings_raw);
        self.values.append(&mut values);
    }

    fn encode_functions(&mut self) -> (Vec<u8>, Vec<Value>) {
        let mut output_bytes =
            Vec::with_capacity(size_of::<u32>() + (self.num_functions * INSTRUCTION_LENGTH));

        // Create the output vector of values
        let mut output_values = Vec::new();

        // For each function, encode all of its instructions and capture their values
        let mut functions = Vec::new();
        mem::swap(&mut functions, &mut self.functions);
        for function in functions {
            for instruction in function {
                let (bytes, value) = self.encode_instruction(instruction);

                output_bytes.extend_from_slice(&bytes);

                if let Some(value) = value {
                    output_values.push(value);
                }
            }
        }

        (output_bytes, output_values)
    }

    fn encode_instruction(
        &self,
        instruction: Instruction,
    ) -> ([u8; INSTRUCTION_LENGTH], Option<Value>) {
        let mut bytes = [0x00; INSTRUCTION_LENGTH];
        let mut value = None;

        match instruction {
            Instruction::NoOp => {
                bytes[0] = 0x00;
            }
            Instruction::Load(val, reg) => {
                bytes[0] = 0x01;
                bytes[size_of::<u32>() + 2] = reg;
                value = Some(val);
            }
            Instruction::CompToReg(reg) => {
                bytes[0] = 0x03;
                bytes[1] = reg;
            }
            Instruction::OpToReg(reg) => {
                bytes[0] = 0x04;
                bytes[1] = reg;
            }
            Instruction::Drop(reg) => {
                bytes[0] = 0x05;
                bytes[1] = reg;
            }

            Instruction::Add(left, right) => {
                bytes[0] = 0x07;
                bytes[1] = left;
                bytes[2] = right;
            }
            Instruction::Sub(left, right) => {
                bytes[0] = 0x08;
                bytes[1] = left;
                bytes[2] = right;
            }
            Instruction::Mult(left, right) => {
                bytes[0] = 0x09;
                bytes[1] = left;
                bytes[2] = right;
            }
            Instruction::Div(left, right) => {
                bytes[0] = 0x0A;
                bytes[1] = left;
                bytes[2] = right;
            }

            Instruction::Print(reg) => {
                bytes[0] = 0x0B;
                bytes[1] = reg;
            }

            Instruction::Jump(loc) => {
                bytes[0] = 0x0C;
                bytes[1..=size_of::<i32>()].copy_from_slice(&loc.to_be_bytes());
            }
            Instruction::JumpComp(loc) => {
                bytes[0] = 0x0D;
                bytes[1..=size_of::<i32>()].copy_from_slice(&loc.to_be_bytes());
            }

            Instruction::And(left, right) => {
                bytes[0] = 0x0E;
                bytes[1] = left;
                bytes[2] = right;
            }
            Instruction::Or(left, right) => {
                bytes[0] = 0x0F;
                bytes[1] = left;
                bytes[2] = right;
            }
            Instruction::Xor(left, right) => {
                bytes[0] = 0x10;
                bytes[1] = left;
                bytes[2] = right;
            }
            Instruction::Not(reg) => {
                bytes[0] = 0x11;
                bytes[1] = reg;
            }

            Instruction::Eq(left, right) => {
                bytes[0] = 0x12;
                bytes[1] = left;
                bytes[2] = right;
            }
            Instruction::NotEq(left, right) => {
                bytes[0] = 0x13;
                bytes[1] = left;
                bytes[2] = right;
            }
            Instruction::GreaterThan(left, right) => {
                bytes[0] = 0x14;
                bytes[1] = left;
                bytes[2] = right;
            }
            Instruction::LessThan(left, right) => {
                bytes[0] = 0x15;
                bytes[1] = left;
                bytes[2] = right;
            }

            Instruction::Collect => {
                bytes[0] = 0x19;
            }
            Instruction::Return => {
                bytes[0] = 0x16;
            }
            Instruction::Halt => {
                bytes[0] = 0x17;
            }

            Instruction::Illegal | Instruction::JumpPoint(_) => {
                // TODO: Should this be allowed? What should an illegal instruction be legally encoded as?
                error!("Tried to encode {:?}", instruction);
                panic!(
                    "I mean, why are you *purposefully* making an Illegal Instruction? Just... Why?"
                );
            }

            _ => todo!("Implement procedural encoding/decoding"),
        }

        (bytes, value.map(|val| *val))
    }
}
