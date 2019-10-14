use super::{Index, Register, Value, Vm};

/// Instructions for the VM
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Instruction {
    /// Load a Value from the GC into a register
    Load(u32, Register),
    /// Cache a Value into the GC
    Cache(u32, Value),
    CompToReg(Register),
    OpToReg(Register),
    DropReg(u32, Register),
    Drop(u32),

    Add(Register, Register),
    Sub(Register, Register),
    Mult(Register, Register),
    Div(Register, Register),

    Print(Register),

    Jump(i32),
    JumpComp(i32),

    And(Register, Register),
    Or(Register, Register),
    Xor(Register, Register),
    Not(Register),

    Eq(Register, Register),
    NotEq(Register, Register),
    GreaterThan(Register, Register),
    LessThan(Register, Register),

    Return,
    Halt,

    Illegal,
}

#[test]
fn inst_test() {
    use Instruction::*;

    //simple_logger::init().unwrap();

    let inst = vec![
        Cache(0, Value::Bool(true)),
        Load(0, 0.into()),
        Print(0.into()),
        Eq(0.into(), 0.into()),
        JumpComp(2),
        Print(0.into()),
        Print(0.into()),
        Print(0.into()),
        Cache(1, Value::Int(1)),
        Load(1, 1.into()),
        Cache(2, Value::Int(0)),
        Load(2, 2.into()),
        Div(1.into(), 2.into()),
        Halt,
    ];

    let mut crunch = crate::Crunch::from((inst, Vec::new()));
    crunch.execute();
}

pub type Result<T> = std::result::Result<T, RuntimeError>;

#[derive(Debug, Clone)]
pub struct RuntimeError {
    pub ty: RuntimeErrorTy,
    pub message: String,
}

impl RuntimeError {
    pub fn emit(&self) {
        println!("[Crunch Runtime Error: {:?}] {}", self.ty, self.message);
    }
}

#[derive(Debug, Clone)]
pub enum RuntimeErrorTy {
    GcError,
    DivideByZero,
    IncompatibleTypes,
}

impl Instruction {
    /// The execution of each instruction
    #[inline]
    pub fn execute(&self, mut vm: &mut Vm) -> Result<()> {
        match self {
            Instruction::Load(heap_loc, reg) => {
                trace!("Loading val at {} into {}", heap_loc, reg);

                let val: Value = match vm.gc.fetch(*heap_loc as usize) {
                    Some(val) => val,
                    None => {
                        return Err(RuntimeError {
                            ty: RuntimeErrorTy::GcError,
                            message: "Failed to retrieve error value from GC".to_string(),
                        });
                    }
                };

                vm.registers[**reg as usize] = val;

                /*
                println!(
                    "vm: [{}]",
                    vm
                        .vm
                        .iter()
                        .map(|r| format!("{:?}", r))
                        .collect::<Vec<String>>()
                        .join(",")
                );
                */

                vm.environment.index += Index(1);
            }
            Instruction::Cache(heap_loc, val) => {
                trace!("Loading value onto heap at {}, Val: {:?}", heap_loc, val);

                if let Some((alloc_val, alloc_id)) = vm
                    .gc
                    .allocate_id(std::mem::size_of::<Value>(), *heap_loc as usize)
                {
                    unsafe {
                        if let Err(err) = vm.gc.write(alloc_id, val.to_owned(), Some(&alloc_val)) {
                            return Err(RuntimeError {
                                ty: RuntimeErrorTy::GcError,
                                message: err,
                            });
                        }
                    }

                    vm.gc.add_root(alloc_val);

                    vm.environment.index += Index(1);

                    assert_eq!(*heap_loc as usize, *alloc_id);
                } else {
                    return Err(RuntimeError {
                        ty: RuntimeErrorTy::GcError,
                        message: "Failed to allocate value to GC".to_string(),
                    });
                }
            }
            Instruction::CompToReg(reg) => {
                trace!("Loading previous comparison into {}", reg);

                vm.registers[**reg as usize] = Value::Bool(vm.prev_comp);
                vm.environment.index += Index(1);
            }
            Instruction::OpToReg(reg) => {
                trace!("Loading previous operation into {}", reg);

                std::mem::swap(&mut vm.registers[**reg as usize], &mut vm.prev_op);
                vm.environment.index += Index(1);
            }
            Instruction::DropReg(heap_loc, reg) => {
                trace!("Clearing register {}", reg);

                let mut val = Value::None;
                std::mem::swap(&mut val, &mut vm.registers[**reg as usize]);

                unsafe {
                    if let Err(err) = vm.gc.write(*heap_loc as usize, val, None) {
                        return Err(RuntimeError {
                            ty: RuntimeErrorTy::GcError,
                            message: err,
                        });
                    }
                }

                vm.environment.index += Index(1);
            }
            Instruction::Drop(id) => {
                trace!("Dropping {:?}", id);
                if vm.gc.remove_root(*id as usize).is_err() {
                    return Err(RuntimeError {
                        ty: RuntimeErrorTy::GcError,
                        message: "Attempted to drop non-existant value".to_string(),
                    });
                }
                vm.environment.index += Index(1);
            }

            Instruction::Add(left, right) => {
                vm.prev_op = (*(*vm).get(*left) + *(*vm).get(*right))?;
                vm.environment.index += Index(1);
            }
            Instruction::Sub(left, right) => {
                vm.prev_op = (*(*vm).get(*left) - *(*vm).get(*right))?;
                vm.environment.index += Index(1);
            }
            Instruction::Mult(left, right) => {
                vm.prev_op = (*(*vm).get(*left) * *(*vm).get(*right))?;
                vm.environment.index += Index(1);
            }
            Instruction::Div(left, right) => {
                vm.prev_op = (*(*vm).get(*left) / *(*vm).get(*right))?;
                vm.environment.index += Index(1);
            }

            Instruction::Print(reg) => {
                trace!("Printing reg {:?}", reg);

                println!("{}", vm.get(*reg));
                vm.environment.index += Index(1);
            }

            Instruction::Jump(index) => {
                trace!("Jumping by offset {}", index);

                let index = Index((*vm.environment.index as i32 + index + 1) as u32);
                vm.environment.index = index;
            }
            Instruction::JumpComp(index) => {
                trace!(
                    "Comparison Jump: Prev Comp is {}, jump amount {}",
                    vm.prev_comp,
                    index
                );

                if vm.prev_comp {
                    let index = Index((*vm.environment.index as i32 + index + 1) as u32);
                    vm.environment.index = index;
                } else {
                    vm.environment.index += Index(1);
                }
            }

            Instruction::And(left, right) => {
                vm.prev_op = (*(*vm).get(*left) & *(*vm).get(*right))?;
                vm.environment.index += Index(1);
            }
            Instruction::Or(left, right) => {
                vm.prev_op = (*(*vm).get(*left) | *(*vm).get(*right))?;
                vm.environment.index += Index(1);
            }
            Instruction::Xor(left, right) => {
                vm.prev_op = (*(*vm).get(*left) ^ *(*vm).get(*right))?;
                vm.environment.index += Index(1);
            }
            Instruction::Not(reg) => {
                vm.prev_op = (!*(*vm).get(*reg))?;
                vm.environment.index += Index(1);
            }

            Instruction::Eq(left, right) => {
                vm.prev_comp = *(*vm).get(*left) == *(*vm).get(*right);
                vm.environment.index += Index(1);
            }
            Instruction::NotEq(left, right) => {
                vm.prev_comp = *(*vm).get(*left) != *(*vm).get(*right);
                vm.environment.index += Index(1);
            }
            Instruction::GreaterThan(left, right) => {
                vm.prev_comp = *(*vm).get(*left) > *(*vm).get(*right);
                vm.environment.index += Index(1);
            }
            Instruction::LessThan(left, right) => {
                vm.prev_comp = *(*vm).get(*left) < *(*vm).get(*right);
                vm.environment.index += Index(1);
            }

            Instruction::Return => {
                vm.environment.returning = true;

                if let Some(context) = vm.snapshots.pop() {
                    vm.environment.index = context.0;
                    vm.registers = context.2;

                    if let Some(index) = context.1 {
                        vm.environment.returning = false;

                        while !vm.environment.returning {
                            vm.functions[*index as usize][*vm.environment.index as usize]
                                .clone()
                                .execute(&mut vm)?;
                        }
                    } else {
                        trace!("Returning to main");
                    }
                } else {
                    if let Some(location) = vm.return_stack.pop() {
                        vm.environment.index = location;
                    } else {
                        vm.cleanup();
                        vm.environment.finished_execution = true;
                    }

                    vm.environment.index += Index(1);
                }
            }
            Instruction::Halt => {
                vm.cleanup();
                vm.environment.finished_execution = true;
            }

            Instruction::Illegal => panic!("Illegal Instruction"),
        }

        Ok(())
    }
}

impl Instruction {
    #[allow(unused_variables)]
    pub fn to_str(&self) -> &'static str {
        match self {
            Self::Load(_, _) => "ld",
            Self::Cache(_, _) => "cache",
            Self::CompToReg(_) => "compr",
            Self::OpToReg(_) => "opr",
            Self::DropReg(_, _) => "dropr",
            Self::Drop(_) => "drop",

            Self::Add(_, _) => "add",
            Self::Sub(_, _) => "sub",
            Self::Mult(_, _) => "mul",
            Self::Div(_, _) => "div",

            Self::Print(_) => "print",

            Self::Jump(_) => "jmp",
            Self::JumpComp(_) => "jmpcmp",

            Self::And(_, _) => "and",
            Self::Or(_, _) => "or",
            Self::Xor(_, _) => "xor",
            Self::Not(_) => "not",

            Self::Eq(_, _) => "eq",
            Self::NotEq(_, _) => "neq",
            Self::GreaterThan(_, _) => "grt",
            Self::LessThan(_, _) => "let",

            Self::Return => "ret",
            Self::Halt => "halt",

            Self::Illegal => "illegal",
        }
    }
}
