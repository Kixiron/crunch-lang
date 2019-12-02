#![allow(dead_code)]

use crate::{Instruction, Register, Value};
use std::collections::{HashMap, HashSet};
use string_interner::{StringInterner, Sym};

pub trait Ident: Into<String> + AsRef<str> {}
impl<T: Into<String> + AsRef<str>> Ident for T {}

pub struct CodeBuilder {
    functions: HashMap<Sym, (Function, Option<u32>)>,
    interner: StringInterner<Sym>,
    gc_ids: HashSet<u32>,
    local_symbols: HashMap<Sym, u32>,
}

impl CodeBuilder {
    pub fn new() -> Self {
        Self {
            functions: HashMap::new(),
            interner: StringInterner::new(),
            gc_ids: HashSet::new(),
            local_symbols: HashMap::new(),
        }
    }

    pub fn function<N, F>(&mut self, name: N, function: F)
    where
        N: Ident,
        F: Fn(&mut Self, &mut BuilderContext),
    {
        let name = self.interner.get_or_intern(name);
        let mut context = BuilderContext::new();

        (function)(self, &mut context);

        let built_function = context.build(self);

        self.functions.insert(name, (built_function, None));
    }

    pub fn solidify_id(&mut self, old_id: u32) -> u32 {
        let mut id = old_id;
        loop {
            if self.gc_ids.get(&id).is_none() {
                self.gc_ids.insert(id);
                break;
            }

            id += 1;
        }

        id
    }

    pub fn build(mut self) -> (Vec<Instruction>, Vec<Vec<Instruction>>) {
        let mut main = Vec::new();
        let mut functions = Vec::new();

        for (sym, (func, _index)) in self.functions.clone().into_iter() {
            if let Some(ident) = self.interner.resolve(sym) {
                if ident == "main" {
                    main = func.build(&mut self);
                    continue;
                }
            }

            functions.push(func.build(&mut self));
        }

        (main, functions)
    }
}

#[derive(Clone, Debug)]
pub struct BuilderContext {
    block: Function,
}

impl BuilderContext {
    pub fn new() -> Self {
        Self {
            block: Function::new(),
        }
    }

    pub fn build(self, _builder: &mut CodeBuilder) -> Function {
        self.block
    }
}

#[derive(Clone, Debug)]
struct PartialInstruction {
    uninit_inst: Instruction,
    func_sym: Option<String>,
    global_sym: Option<String>,
    local_sym: Option<String>,
}

impl PartialInstruction {
    pub fn solidify(self, builder: &mut CodeBuilder) -> Instruction {
        match self.uninit_inst {
            Instruction::Cache(start_id, value, register) => {
                let concrete_id = builder.solidify_id(start_id);
                let symbol = builder
                    .interner
                    .get_or_intern(self.local_sym.expect("Expected a local Symbol"));

                builder.local_symbols.insert(symbol, concrete_id);

                Instruction::Cache(concrete_id, value, register)
            }

            Instruction::Drop(_) => Instruction::Drop(
                *builder
                    .local_symbols
                    .get(
                        &builder
                            .interner
                            .get_or_intern(self.local_sym.expect("No Local Symbol Provided")),
                    )
                    .expect("No Registered Local Symbol"),
            ),

            _ => self.uninit_inst,
        }
    }
}

impl From<Instruction> for PartialInstruction {
    fn from(inst: Instruction) -> Self {
        Self {
            uninit_inst: inst,
            func_sym: None,
            global_sym: None,
            local_sym: None,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Function {
    block: Vec<PartialInstruction>,
}

impl Function {
    pub fn new() -> Self {
        Self { block: Vec::new() }
    }

    pub fn build(self, builder: &mut CodeBuilder) -> Vec<Instruction> {
        let mut instructions = Vec::with_capacity(self.block.len());
        for inst in self.block {
            instructions.push(inst.solidify(builder));
        }

        let mut jumps: HashMap<u32, Option<u32>> = HashMap::new(); // HashMap<JumpId, Option<JumpIndex>>
        for (index, instruction) in instructions.iter_mut().enumerate() {
            match instruction {
                Instruction::Jump(id) | Instruction::JumpComp(id) => {
                    if let Some(Some(loc)) = jumps.get(&(*id as u32)) {
                        *id = *loc as i32 - index as i32;
                    } else {
                        jumps.insert(*id as u32, None);
                    }
                }
                Instruction::JumpPoint(id) => {
                    jumps.insert(*id, Some(index as u32));
                }

                _ => {}
            }
        }
        for (index, instruction) in instructions.iter_mut().enumerate() {
            match instruction {
                Instruction::Jump(id) | Instruction::JumpComp(id) => {
                    if let Some(Some(loc)) = jumps.get(&(*id as u32)) {
                        *id = *loc as i32 - index as i32;
                    } else {
                        jumps.insert(*id as u32, None);
                    }
                }
                Instruction::JumpPoint(id) => {
                    jumps.insert(*id, Some(index as u32));
                }

                _ => {}
            }
        }

        instructions
            .into_iter()
            .map(|i| {
                if let Instruction::JumpPoint(_) = i {
                    Instruction::NoOp
                } else {
                    i
                }
            })
            .collect()
    }

    pub fn inst_load(&mut self, register: impl Into<Register>, value: Value) -> &mut Self {
        self.block
            .push(Instruction::Load(value, register.into()).into());

        self
    }
    pub fn inst_cache(
        &mut self,
        register: impl Into<Register>,
        value: Value,
        name: impl Ident,
    ) -> &mut Self {
        let gc_id = &value as *const _ as u32;

        self.block.push(PartialInstruction {
            uninit_inst: Instruction::Cache(gc_id, value, register.into()),
            func_sym: None,
            global_sym: None,
            local_sym: Some(name.into()),
        });

        self
    }

    pub fn inst_comp_to_reg(&mut self, register: impl Into<Register>) -> &mut Self {
        self.block
            .push(Instruction::CompToReg(register.into()).into());

        self
    }
    pub fn inst_op_to_reg(&mut self, register: impl Into<Register>) -> &mut Self {
        self.block
            .push(Instruction::OpToReg(register.into()).into());

        self
    }

    pub fn inst_drop_reg(&mut self, register: impl Into<Register>) -> &mut Self {
        self.block
            .push(Instruction::DropReg(register.into()).into());

        self
    }
    pub fn inst_drop(&mut self, name: impl Ident) -> &mut Self {
        self.block.push(PartialInstruction {
            uninit_inst: Instruction::Drop(0).into(),
            func_sym: None,
            global_sym: None,
            local_sym: Some(name.into()),
        });

        self
    }

    pub fn inst_add(&mut self, left: impl Into<Register>, right: impl Into<Register>) -> &mut Self {
        self.block
            .push(Instruction::Add(left.into(), right.into()).into());

        self
    }
    pub fn inst_sub(&mut self, left: impl Into<Register>, right: impl Into<Register>) -> &mut Self {
        self.block
            .push(Instruction::Sub(left.into(), right.into()).into());

        self
    }
    pub fn inst_mult(
        &mut self,
        left: impl Into<Register>,
        right: impl Into<Register>,
    ) -> &mut Self {
        self.block
            .push(Instruction::Mult(left.into(), right.into()).into());

        self
    }
    pub fn inst_div(&mut self, left: impl Into<Register>, right: impl Into<Register>) -> &mut Self {
        self.block
            .push(Instruction::Div(left.into(), right.into()).into());

        self
    }

    pub fn inst_print(&mut self, register: impl Into<Register>) -> &mut Self {
        self.block.push(Instruction::Print(register.into()).into());

        self
    }

    pub fn inst_jump(&mut self, id: u32) -> &mut Self {
        self.block.push(Instruction::Jump(id as i32).into());

        self
    }
    pub fn inst_jump_comp(&mut self, id: u32) -> &mut Self {
        self.block.push(Instruction::JumpComp(id as i32).into());

        self
    }

    pub fn inst_jump_point(&mut self, id: u32) -> &mut Self {
        self.block.push(Instruction::JumpPoint(id).into());

        self
    }

    pub fn inst_and(&mut self, left: impl Into<Register>, right: impl Into<Register>) -> &mut Self {
        self.block
            .push(Instruction::And(left.into(), right.into()).into());

        self
    }
    pub fn inst_or(&mut self, left: impl Into<Register>, right: impl Into<Register>) -> &mut Self {
        self.block
            .push(Instruction::Or(left.into(), right.into()).into());

        self
    }
    pub fn inst_xor(&mut self, left: impl Into<Register>, right: impl Into<Register>) -> &mut Self {
        self.block
            .push(Instruction::Xor(left.into(), right.into()).into());

        self
    }
    pub fn inst_not(&mut self, register: impl Into<Register>) -> &mut Self {
        self.block.push(Instruction::Not(register.into()).into());

        self
    }

    pub fn inst_eq(&mut self, left: impl Into<Register>, right: impl Into<Register>) -> &mut Self {
        self.block
            .push(Instruction::Eq(left.into(), right.into()).into());

        self
    }
    pub fn inst_not_eq(
        &mut self,
        left: impl Into<Register>,
        right: impl Into<Register>,
    ) -> &mut Self {
        self.block
            .push(Instruction::NotEq(left.into(), right.into()).into());

        self
    }
    pub fn inst_greater_than(
        &mut self,
        left: impl Into<Register>,
        right: impl Into<Register>,
    ) -> &mut Self {
        self.block
            .push(Instruction::GreaterThan(left.into(), right.into()).into());

        self
    }
    pub fn inst_less_than(
        &mut self,
        left: impl Into<Register>,
        right: impl Into<Register>,
    ) -> &mut Self {
        self.block
            .push(Instruction::LessThan(left.into(), right.into()).into());

        self
    }

    pub fn inst_collect(&mut self) -> &mut Self {
        self.block.push(Instruction::Collect.into());

        self
    }
    pub fn inst_return(&mut self) -> &mut Self {
        self.block.push(Instruction::Return.into());

        self
    }
    pub fn inst_halt(&mut self) -> &mut Self {
        self.block.push(Instruction::Halt.into());

        self
    }
    pub fn inst_illegal(&mut self) -> &mut Self {
        self.block.push(Instruction::Illegal.into());

        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Crunch, OptionBuilder};

    #[test]
    fn codebuilder_test() {
        color_backtrace::install();

        let mut builder = CodeBuilder::new();

        builder.function("main", |_builder, ctx| {
            ctx.block
                .inst_load(0, Value::String("Hello World!\n".to_string()))
                .inst_load(1, Value::Int(10))
                .inst_load(2, Value::Int(0))
                .inst_load(3, Value::Int(1))
                .inst_jump_point(77)
                .inst_print(0)
                .inst_sub(1, 3)
                .inst_op_to_reg(1)
                .inst_greater_than(1, 2)
                .inst_jump_comp(77)
                .inst_halt();
        });

        let (main, functions) = builder.build();
        let crunch = (
            main,
            functions,
            OptionBuilder::new("./codebuilder_test")
                .debug_log(true)
                .build(),
        );

        Crunch::from(crunch).execute();
    }
}
