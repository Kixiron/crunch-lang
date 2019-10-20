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
fn decode(instruction: [u8; INSTRUCTION_LENGTH], value: Option<Value>) -> Instruction {
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
fn encode(instruction: Instruction) -> ([u8; INSTRUCTION_LENGTH], Option<Value>) {
    use std::mem::size_of;

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

        Instruction::Illegal => unreachable!(),
    }

    (bytes, value)
}

#[inline]
pub fn encode_program(main: &[Instruction], functions: &[Vec<Instruction>]) -> Vec<u8> {
    // Encode the main function and strings
    let (main_bytes, main_values) = encode_instructions(main);

    let (func_bytes, func_values) = {
        let (mut func_bytes, mut func_values) = (Vec::new(), Vec::new());

        func_bytes.extend_from_slice(&(functions.len() as u32).to_be_bytes());

        // Encode each function to bytecode and store all their strings
        for function in functions {
            // Encode the function
            let (bytes, values) = encode_instructions(function);
            // Push the length of the function to the stored bytes
            func_bytes.extend_from_slice(&(function.len() as u32).to_be_bytes());
            // Push the instructions to the stored bytes
            func_bytes.extend_from_slice(&bytes);
            // Push the strings to the stored strings
            func_values.extend_from_slice(&values);
        }

        (func_bytes, func_values)
    };

    // Encode the strings
    let mut values = {
        // Combine the function strings and the main function strings
        let mut values = func_values;
        values.extend_from_slice(&main_values);

        // Prepare vec for encoded strings
        let mut value_bytes: Vec<u8> = Vec::new();
        value_bytes.extend_from_slice(&(values.len() as u32).to_be_bytes());

        for val in values {
            value_bytes.extend_from_slice(&val.as_bytes());
        }

        value_bytes
    };

    values.extend_from_slice(&func_bytes);
    values.extend_from_slice(&main_bytes);

    // Layout:
    // Values
    //     Function Values
    //     Main Values
    // Functions
    // Main
    values
}

#[inline]
pub fn decode_program(bytes: &[u8]) -> (Vec<Instruction>, Vec<Vec<Instruction>>) {
    use std::convert::TryInto;

    let mut index = 0;

    let mut values = {
        let num_values = take_usize(&mut index, bytes);
        let mut values = VecDeque::with_capacity(num_values);

        for _ in 0..num_values {
            values
                .push_back(Value::from_bytes(bytes[index..index + 8].try_into().unwrap()).unwrap());
            index += 8;
        }

        values
    };

    let functions = {
        let num_functions = take_usize(&mut index, bytes);
        let mut functions = Vec::with_capacity(num_functions);

        for _ in 0..num_functions {
            let len = take_usize(&mut index, bytes);

            functions.push(decode_instructions(
                &bytes[index..index + (len * INSTRUCTION_LENGTH)],
                &mut values,
            ));

            index += len * INSTRUCTION_LENGTH;
        }

        functions
    };

    let main = decode_instructions(&bytes[index..], &mut values);

    (main, functions)
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

        let mut registers = [Value::None; NUMBER_REGISTERS];
        let mut heap: HashMap<u32, Value> = HashMap::new();

        for (instruction_index, instruction) in function.into_iter().enumerate() {
            match instruction {
                Instruction::Load(heap_loc, reg) => {
                    registers[*reg as usize] = heap.get(&heap_loc).unwrap_or(&Value::None).clone();
                }
                Instruction::Cache(heap_loc, val) => {
                    heap.insert(heap_loc, val);
                }
                Instruction::Drop(heap_loc) => {
                    heap.remove(&heap_loc);
                }
                Instruction::DropReg(reg) => {
                    registers[*reg as usize] = Value::None;
                }
                Instruction::Save(heap_loc, reg) => {
                    heap.insert(heap_loc, registers[*reg as usize]);
                    registers[*reg as usize] = Value::None;
                }
                _ => {}
            }

            let param = {
                use Instruction::*;

                match instruction {
                    Load(heap, reg) | Save(heap, reg) => {
                        format!("{:p}, {}", heap as *const u8, reg)
                    }
                    Cache(heap, val) => format!("{:p}, {}", heap as *const u8, val.to_string()),
                    CompToReg(reg) | OpToReg(reg) | DropReg(reg) => format!("{}", reg),
                    Drop(reg) => format!("{}", reg),

                    Print(reg) => {
                        if registers[*reg as usize] != Value::None {
                            format!("{}: {}", reg, registers[*reg as usize].to_string())
                        } else {
                            format!("{}", reg)
                        }
                    }

                    Jump(abs) | JumpComp(abs) => format!("{:04}", abs),

                    Not(reg) => format!(
                        "{}",
                        if registers[*reg as usize] != Value::None {
                            format!("{}: {}", reg, registers[*reg as usize].to_string())
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
                        if registers[*left as usize] != Value::None {
                            format!("{}: {:?}", left, registers[*left as usize].to_string())
                        } else {
                            format!("{}", left)
                        },
                        if registers[*right as usize] != Value::None {
                            format!("{}: {:?}", right, registers[*right as usize].to_string())
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
fn encode_instructions(instructions: &[Instruction]) -> (Vec<u8>, Vec<Value>) {
    let mut instruction_bytes = vec![0_u8; instructions.len() * INSTRUCTION_LENGTH];
    let mut values = Vec::default();

    for (index, instruction) in instructions.into_iter().enumerate() {
        let (bytes, val) = encode(instruction.clone());

        instruction_bytes
            [index * INSTRUCTION_LENGTH..(index * INSTRUCTION_LENGTH) + INSTRUCTION_LENGTH]
            .copy_from_slice(&bytes);
        if let Some(val) = val {
            values.push(val);
        }
    }

    (instruction_bytes, values)
}

#[inline]
fn decode_instructions(bytes: &[u8], values: &mut VecDeque<Value>) -> Vec<Instruction> {
    use std::convert::TryInto;

    let mut instructions = Vec::with_capacity(bytes.len() / INSTRUCTION_LENGTH);
    for chunk in bytes.chunks(INSTRUCTION_LENGTH) {
        let chunk: [u8; INSTRUCTION_LENGTH] = chunk.try_into().expect("Invalid chunk length");

        let instruction = if chunk[0] == 0x01 {
            decode(chunk, values.pop_front())
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

        let encoded_program = encode_program(&instructions, &functions);
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
