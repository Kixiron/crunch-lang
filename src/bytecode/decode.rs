use crate::{Instruction, Result, RuntimeError, RuntimeErrorTy, RuntimeValue, INSTRUCTION_LENGTH};
use std::{collections::VecDeque, convert::TryInto, mem::size_of};

// TODO: Document & Test all functions

macro_rules! take {
    ($self:tt, $ty:tt) => {{
        let int = $ty::from_be_bytes(
            $self.bytes[$self.index..$self.index + size_of::<$ty>()]
                .try_into()
                .unwrap_or_else(|_| unreachable!()),
        );

        $self.index += size_of::<$ty>();

        int
    }};
}

#[derive(Debug, Clone)]
struct FunctionMeta {
    values: VecDeque<RuntimeValue>,
}

#[derive(Debug, Clone)]
pub struct Decoder<'a> {
    bytes: &'a [u8],
    index: usize,
    functions: Vec<Vec<Instruction>>,
    function_meta: VecDeque<FunctionMeta>,
    number_functions: usize,
}

impl<'a> Decoder<'a> {
    pub fn new(bytes: &'a [u8]) -> Self {
        Self {
            bytes,
            index: 0,
            functions: Vec::new(),
            function_meta: VecDeque::new(),
            number_functions: 0,
        }
    }

    pub fn decode(mut self) -> Result<(Vec<Instruction>, Vec<Vec<Instruction>>)> {
        self.get_number_functions();
        self.fill_meta()?;
        self.fill_functions()?;

        dbg!(&self.functions);

        Ok((self.functions.remove(0), self.functions))
    }

    fn fill_functions(&mut self) -> Result<()> {
        let mut functions = Vec::with_capacity(self.number_functions);
        for _ in 0..self.number_functions {
            functions.push(self.decode_function()?);
        }
        self.functions = functions;

        Ok(())
    }

    fn get_number_functions(&mut self) {
        self.number_functions = take!(self, u32) as usize;
    }

    fn decode_function(&mut self) -> Result<Vec<Instruction>> {
        let function_len = take!(self, u32) as usize;

        let mut instructions = Vec::with_capacity(function_len);
        let mut meta = &mut self
            .function_meta
            .pop_front()
            .expect("Expected a FunctionMeta to decode a function");

        for _ in 0..function_len {
            instructions.push(self.decode_instruction(&mut meta)?);
        }

        Ok(instructions)
    }

    fn decode_instruction(&mut self, meta: &mut FunctionMeta) -> Result<Instruction> {
        macro_rules! take_from {
            ($self:tt, $offset:tt, $ty:tt) => {
                $ty::from_be_bytes(
                    $self.bytes[$self.index + $offset..$self.index + $offset + size_of::<$ty>()]
                        .try_into()
                        .unwrap_or_else(|_| unreachable!()),
                )
            };
        }

        let instruction = match self.bytes[self.index] {
            0x00 => Instruction::NoOp,
            0x01 => Instruction::Load(
                match meta.values.pop_front() {
                    Some(value) => value,
                    None => {
                        return Err(RuntimeError {
                            ty: RuntimeErrorTy::MissingValue,
                            message: "Not enough values were encoded".to_string(),
                        });
                    }
                },
                self.bytes[1].into(),
            ),
            0x02 => Instruction::Cache(
                take_from!(self, 1, u32),
                match meta.values.pop_front() {
                    Some(value) => value,
                    None => {
                        return Err(RuntimeError {
                            ty: RuntimeErrorTy::MissingValue,
                            message: "Not enough values were encoded".to_string(),
                        });
                    }
                },
                self.bytes[1].into(),
            ),
            0x03 => Instruction::CompToReg(self.bytes[1].into()),
            0x04 => Instruction::OpToReg(self.bytes[1].into()),
            0x05 => Instruction::DropReg(self.bytes[1].into()),
            0x06 => Instruction::Drop(take_from!(self, 1, u32)),

            0x07 => Instruction::Add(self.bytes[1].into(), self.bytes[2].into()),
            0x08 => Instruction::Sub(self.bytes[1].into(), self.bytes[2].into()),
            0x09 => Instruction::Mult(self.bytes[1].into(), self.bytes[2].into()),
            0x0A => Instruction::Div(self.bytes[1].into(), self.bytes[2].into()),

            0x0B => Instruction::Print(self.bytes[1].into()),

            0x0C => Instruction::Jump(take_from!(self, 1, i32)),
            0x0D => Instruction::JumpComp(take_from!(self, 1, i32)),

            0x0E => Instruction::And(self.bytes[1].into(), self.bytes[2].into()),
            0x0F => Instruction::Or(self.bytes[1].into(), self.bytes[2].into()),
            0x10 => Instruction::Xor(self.bytes[1].into(), self.bytes[2].into()),
            0x11 => Instruction::Not(self.bytes[1].into()),

            0x12 => Instruction::Eq(self.bytes[1].into(), self.bytes[2].into()),
            0x13 => Instruction::NotEq(self.bytes[1].into(), self.bytes[2].into()),
            0x14 => Instruction::GreaterThan(self.bytes[1].into(), self.bytes[2].into()),
            0x15 => Instruction::LessThan(self.bytes[1].into(), self.bytes[2].into()),

            0x16 => Instruction::Return,
            0x17 => Instruction::Halt,

            0x18 => Instruction::Save(
                take_from!(self, 1, u32),
                self.bytes[size_of::<u32>() + 2].into(),
            ),
            0x19 => Instruction::Collect,
            0x1A => Instruction::Syscall(
                self.bytes[1],
                self.bytes[2].into(),
                self.bytes[3].into(),
                self.bytes[4].into(),
                self.bytes[5].into(),
                self.bytes[6].into(),
                self.bytes[7].into(),
            ),

            _ => Instruction::Illegal,
        };

        self.index += INSTRUCTION_LENGTH;

        Ok(instruction)
    }

    fn fill_meta(&mut self) -> Result<()> {
        let mut meta = VecDeque::with_capacity(self.number_functions);
        for _ in 0..self.number_functions {
            let strings = self.take_strings()?;
            let values = self.take_values(strings)?;

            meta.push_back(FunctionMeta { values });
        }

        self.function_meta = meta;

        Ok(())
    }

    fn take_values(&mut self, mut strings: VecDeque<String>) -> Result<VecDeque<RuntimeValue>> {
        let number_values = take!(self, u32) as usize;

        let mut values = VecDeque::with_capacity(number_values);
        for _ in 0..number_values {
            let len = take!(self, u32) as usize;
            let bytes = self.bytes[self.index..self.index + len]
                .try_into()
                .unwrap_or_else(|_| unreachable!());

            // TODO: Handle this error and make it a RuntimeError
            let value =
                RuntimeValue::from_bytes(bytes, &mut strings).unwrap_or_else(|_| unreachable!());

            values.push_back(value);

            self.index += len;
        }

        Ok(values)
    }

    fn take_strings(&mut self) -> Result<VecDeque<String>> {
        let number_strings = take!(self, u32) as usize;

        let mut strings = VecDeque::with_capacity(number_strings);
        for _ in 0..number_strings {
            let len = take!(self, u32) as usize;

            let string = match String::from_utf8(self.bytes[self.index..self.index + len].to_vec())
            {
                Ok(string) => string,
                Err(_) => {
                    return Err(RuntimeError {
                        ty: RuntimeErrorTy::InvalidString,
                        message: "Incorrectly encoded string".to_string(),
                    });
                }
            };

            strings.push_back(string);
        }

        Ok(strings)
    }
}
