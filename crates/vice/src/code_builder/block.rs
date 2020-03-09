use super::FunctionContext;

use compactor::{Instruction, Value};
use crunch_error::compile_prelude::*;
use crunch_parser::string_interner::Sym;

#[derive(Debug, Copy, Clone)]
pub struct ResolutionInfo {
    pub function: Option<Sym>,
}

impl ResolutionInfo {
    pub fn function(func: Sym) -> Self {
        Self {
            function: Some(func),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Block {
    pub block: Vec<(Instruction, Option<ResolutionInfo>)>,
}

impl Block {
    pub fn new() -> Self {
        Self { block: Vec::new() }
    }

    pub fn push(&mut self, inst: Instruction, info: impl Into<Option<ResolutionInfo>>) {
        self.block.push((inst, info.into()));
    }
}

impl Default for Block {
    fn default() -> Self {
        Self::new()
    }
}

impl Block {
    pub fn inst_load(&mut self, register: u8, value: Value) -> &mut Self {
        self.push(Instruction::Load(Box::new(value), register), None);
        self
    }
    pub fn inst_mov(&mut self, target: u8, source: u8, ctx: &mut FunctionContext) -> &mut Self {
        let mut temp = None;
        std::mem::swap(&mut ctx.registers[source as usize], &mut temp);
        ctx.registers[target as usize] = temp;
        self.push(Instruction::Move(target, source), None);
        self
    }
    pub fn inst_push(&mut self, register: u8) -> &mut Self {
        self.push(Instruction::Push(register), None);
        self
    }
    pub fn inst_pop(&mut self, register: u8) -> &mut Self {
        self.push(Instruction::Pop(register), None);
        self
    }

    pub fn inst_comp_to_reg(&mut self, register: u8) -> &mut Self {
        self.push(Instruction::CompToReg(register), None);
        self
    }
    pub fn inst_op_to_reg(&mut self, register: u8) -> &mut Self {
        self.push(Instruction::OpToReg(register), None);
        self
    }

    #[track_caller]
    pub fn inst_drop(&mut self, register: u8, ctx: &mut FunctionContext) -> &mut Self {
        trace!(
            "Dropping reg {} from {:?}",
            register,
            std::panic::Location::caller()
        );

        self.push(Instruction::Drop(register), None);
        ctx.free_reg(register);
        self
    }

    pub fn inst_add(&mut self, left: u8, right: u8) -> &mut Self {
        self.push(Instruction::Add(left, right), None);
        self
    }
    pub fn inst_sub(&mut self, left: u8, right: u8) -> &mut Self {
        self.push(Instruction::Sub(left, right), None);
        self
    }
    pub fn inst_mult(&mut self, left: u8, right: u8) -> &mut Self {
        self.push(Instruction::Mult(left, right), None);
        self
    }
    pub fn inst_div(&mut self, left: u8, right: u8) -> &mut Self {
        self.push(Instruction::Div(left, right), None);
        self
    }

    pub fn inst_print(&mut self, register: u8) -> &mut Self {
        self.push(Instruction::Print(register), None);
        self
    }

    pub fn inst_jump(&mut self, id: u32) -> &mut Self {
        self.push(Instruction::Jump(id as i32), None);
        self
    }
    pub fn inst_jump_comp(&mut self, id: u32) -> &mut Self {
        self.push(Instruction::JumpComp(id as i32), None);
        self
    }

    pub fn inst_jump_point(&mut self, id: u32) -> &mut Self {
        self.push(Instruction::JumpPoint(id), None);
        self
    }

    pub fn inst_and(&mut self, left: u8, right: u8) -> &mut Self {
        self.push(Instruction::And(left, right), None);
        self
    }
    pub fn inst_or(&mut self, left: u8, right: u8) -> &mut Self {
        self.push(Instruction::Or(left, right), None);
        self
    }
    pub fn inst_xor(&mut self, left: u8, right: u8) -> &mut Self {
        self.push(Instruction::Xor(left, right), None);
        self
    }
    pub fn inst_not(&mut self, register: u8) -> &mut Self {
        self.push(Instruction::Not(register), None);
        self
    }

    pub fn inst_eq(&mut self, left: u8, right: u8) -> &mut Self {
        self.push(Instruction::Eq(left, right), None);
        self
    }
    pub fn inst_not_eq(&mut self, left: u8, right: u8) -> &mut Self {
        self.push(Instruction::NotEq(left, right), None);
        self
    }
    pub fn inst_greater_than(&mut self, left: u8, right: u8) -> &mut Self {
        self.push(Instruction::GreaterThan(left, right), None);
        self
    }
    pub fn inst_less_than(&mut self, left: u8, right: u8) -> &mut Self {
        self.push(Instruction::LessThan(left, right), None);
        self
    }
    pub fn inst_less_than_eq(&mut self, left: u8, right: u8) -> &mut Self {
        self.push(Instruction::LessThanEq(left, right), None);
        self
    }

    pub fn inst_greater_than_eq(&mut self, left: u8, right: u8) -> &mut Self {
        self.push(Instruction::GreaterThanEq(left, right), None);
        self
    }

    pub fn inst_yield(&mut self) -> &mut Self {
        self.push(Instruction::Yield, None);
        self
    }

    pub fn inst_call_generator(&mut self, func_name: Sym, reg: u8) -> &mut Self {
        self.push(
            Instruction::CallGenerator(0, reg),
            ResolutionInfo::function(func_name),
        );
        self
    }

    pub fn inst_copy(&mut self, left: u8, right: u8) -> &mut Self {
        self.push(Instruction::Copy(left, right), None);
        self
    }

    pub fn inst_collect(&mut self) -> &mut Self {
        self.push(Instruction::Collect, None);
        self
    }

    pub fn inst_return(&mut self) -> &mut Self {
        self.push(Instruction::Return, None);
        self
    }

    pub fn inst_func_call(&mut self, func_name: Sym) -> &mut Self {
        self.push(Instruction::Func(0), ResolutionInfo::function(func_name));
        self
    }

    pub fn inst_halt(&mut self) -> &mut Self {
        self.push(Instruction::Halt, None);
        self
    }

    pub fn inst_noop(&mut self) -> &mut Self {
        self.push(Instruction::NoOp, None);
        self
    }

    pub fn inst_illegal(&mut self) -> &mut Self {
        self.push(Instruction::Illegal, None);
        self
    }
}
