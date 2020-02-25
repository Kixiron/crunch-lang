use super::*;

#[derive(Debug, Clone)]
pub struct Block {
    pub block: Vec<PartialInstruction>,
}

impl Block {
    pub fn new() -> Self {
        Self { block: Vec::new() }
    }

    pub fn solidify(self, builder: &mut CodeBuilder) -> Result<Vec<Instruction>> {
        let mut instructions = Vec::with_capacity(self.block.len());

        for inst in self.block {
            instructions.push(inst.solidify(builder)?);
        }

        Ok(instructions)
    }
}

impl Default for Block {
    fn default() -> Self {
        Self::new()
    }
}

pub trait BlockOptimizer: Default {
    fn run(&mut self, block: &mut Block);
    fn reset(&mut self) {
        *self = Self::default();
    }
}

#[derive(Debug, Clone)]
pub struct NoopRemover;

impl NoopRemover {
    pub fn new() -> Self {
        Self
    }
}

impl Default for NoopRemover {
    fn default() -> Self {
        Self::new()
    }
}

impl BlockOptimizer for NoopRemover {
    fn run(&mut self, block: &mut Block) {
        let mut remove_indices = Vec::with_capacity(10);

        for (idx, inst) in block.block.iter().enumerate() {
            match inst.uninit_inst {
                Instruction::NoOp | Instruction::JumpPoint(_) => {
                    trace!("Found a no-op instruction");
                    remove_indices.push(idx);
                }
                _ => {}
            }
        }

        for (offset, idx) in remove_indices.into_iter().enumerate() {
            block.block.remove(idx - offset);
        }
    }
}

#[derive(Debug, Clone)]
pub struct StackDeduplicator {
    second_to_last: Option<Instruction>,
    last: Option<Instruction>,
    push_dropped: bool,
}

impl StackDeduplicator {
    pub fn new() -> Self {
        Self {
            second_to_last: None,
            last: None,
            push_dropped: false,
        }
    }
}

impl Default for StackDeduplicator {
    fn default() -> Self {
        Self::new()
    }
}

impl BlockOptimizer for StackDeduplicator {
    fn run(&mut self, block: &mut Block) {
        let mut remove_indices = Vec::with_capacity(10);

        for (idx, inst) in block.block.iter().enumerate() {
            if let Instruction::Pop(reg) = inst.uninit_inst {
                if let (Some(Instruction::Pop(pop_one)), Some(Instruction::Push(push))) =
                    (self.second_to_last.clone(), self.last.clone())
                {
                    if pop_one == push && push == reg {
                        trace!("Found a sequence of redundant Pop(r), Push(r), Pop(r)");
                        remove_indices.extend_from_slice(&[idx - 1, idx]);
                    }
                }
            }

            self.second_to_last = self.last.clone();
            self.last = Some(inst.uninit_inst.clone());
        }

        for (offset, idx) in remove_indices.into_iter().enumerate() {
            block.block.remove(idx - offset);
        }
    }
}

impl Block {
    pub fn inst_load(&mut self, register: impl Into<Register>, value: Value) -> &mut Self {
        self.block
            .push(Instruction::Load(Box::new(value), register.into()).into());

        self
    }
    pub fn inst_mov(
        &mut self,
        target: impl Into<Register>,
        source: impl Into<Register>,
        ctx: &mut FunctionContext,
    ) -> &mut Self {
        let (target, source) = (target.into(), source.into());

        let mut temp = None;
        std::mem::swap(&mut ctx.registers[*source as usize], &mut temp);
        ctx.registers[*target as usize] = temp;

        self.block.push(Instruction::Move(target, source).into());

        self
    }
    pub fn inst_push(&mut self, register: impl Into<Register>) -> &mut Self {
        let register = register.into();
        self.block.push(Instruction::Push(register).into());

        self
    }
    pub fn inst_pop(&mut self, register: impl Into<Register>) -> &mut Self {
        self.block.push(Instruction::Pop(register.into()).into());

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

    #[track_caller]
    pub fn inst_drop(
        &mut self,
        register: impl Into<Register>,
        ctx: &mut FunctionContext,
    ) -> &mut Self {
        let register = register.into();
        trace!(
            "Dropping reg {} from {:?}",
            register,
            std::panic::Location::caller()
        );

        self.block.push(Instruction::Drop(register).into());
        ctx.free_reg(register);

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
    pub fn inst_less_than_eq(
        &mut self,
        left: impl Into<Register>,
        right: impl Into<Register>,
    ) -> &mut Self {
        self.block
            .push(Instruction::LessThanEq(left.into(), right.into()).into());

        self
    }

    pub fn inst_greater_than_eq(
        &mut self,
        left: impl Into<Register>,
        right: impl Into<Register>,
    ) -> &mut Self {
        self.block
            .push(Instruction::GreaterThanEq(left.into(), right.into()).into());

        self
    }

    pub fn inst_yield(&mut self) -> &mut Self {
        self.block.push(Instruction::Yield.into());

        self
    }

    pub fn inst_call_generator(&mut self, func_name: Sym, reg: impl Into<Register>) -> &mut Self {
        let reg = reg.into();
        self.block.push(PartialInstruction {
            uninit_inst: Instruction::CallGenerator(0, reg),
            func_sym: Some(func_name),
            global_sym: None,
            local_sym: None,
        });

        self
    }

    pub fn inst_copy(
        &mut self,
        left: impl Into<Register>,
        right: impl Into<Register>,
    ) -> &mut Self {
        let (left, right) = (left.into(), right.into());
        self.block.push(Instruction::Copy(left, right).into());

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

    pub fn inst_func_call(&mut self, func_name: Sym) -> &mut Self {
        self.block.push(PartialInstruction {
            uninit_inst: Instruction::Func(0),
            func_sym: Some(func_name),
            global_sym: None,
            local_sym: None,
        });

        self
    }

    pub fn inst_halt(&mut self) -> &mut Self {
        self.block.push(Instruction::Halt.into());

        self
    }

    pub fn inst_noop(&mut self) -> &mut Self {
        self.block.push(Instruction::NoOp.into());

        self
    }

    pub fn inst_illegal(&mut self) -> &mut Self {
        self.block.push(Instruction::Illegal.into());

        self
    }
}
