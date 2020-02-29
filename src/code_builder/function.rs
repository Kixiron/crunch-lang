use super::*;

#[derive(Debug, Clone)]
pub struct FunctionContext {
    pub registers: [Option<Option<Sym>>; NUMBER_REGISTERS],
    pub variables: HashSet<Sym>,
    pub blocks: Vec<Block>,
    pub current_block: usize,
    pub name: Sym,
}

impl FunctionContext {
    #[inline]
    pub fn new(name: Sym) -> Self {
        let mut new = Self {
            registers: [None; NUMBER_REGISTERS],
            variables: HashSet::new(),
            blocks: Vec::with_capacity(20),
            current_block: 0,
            name,
        };
        new.blocks.push(Block::new());

        new
    }

    pub fn current_block(&mut self) -> &mut Block {
        &mut self.blocks[self.current_block]
    }

    pub fn get_block(&mut self, block: usize) -> &mut Block {
        &mut self.blocks[block]
    }

    pub fn add_block(&mut self) {
        self.blocks.push(Block::new());
        self.current_block = self.blocks.len() - 1;
    }

    pub fn move_to_block(&mut self, block: usize) {
        self.current_block = block
    }

    #[inline]
    pub fn push_block(&mut self, block: Block) {
        self.blocks.push(block);
    }

    #[inline]
    pub fn free_reg(&mut self, reg: impl Into<Register>) -> &mut Self {
        let reg = reg.into();
        trace!("Freeing register {}", reg.0);

        self.registers[*reg as usize] = None;

        self
    }

    pub fn inst_mov_block(
        &mut self,
        target: impl Into<Register>,
        source: impl Into<Register>,
        block: usize,
    ) -> &mut Block {
        let (target, source) = (target.into(), source.into());

        let mut temp = None;
        std::mem::swap(&mut self.registers[*source as usize], &mut temp);
        self.registers[*target as usize] = temp;

        self.blocks[block]
            .block
            .push(Instruction::Move(target, source).into());

        &mut self.blocks[block]
    }

    pub fn inst_drop_block(&mut self, register: impl Into<Register>, block: usize) -> &mut Block {
        let register = register.into();
        trace!(
            "Dropping reg {} from {:?}",
            register,
            std::panic::Location::caller()
        );

        self.blocks[block]
            .block
            .push(Instruction::Drop(register).into());
        self.free_reg(register);

        &mut self.blocks[block]
    }

    pub fn inst_push_arr(
        &mut self,
        array: impl Into<Register>,
        value: impl Into<Register>,
        block: usize,
    ) -> &mut Block {
        let (array, value) = (array.into(), value.into());

        self.blocks[block]
            .block
            .push(Instruction::PushArray { array, value }.into());
        self.free_reg(value);

        &mut self.blocks[block]
    }

    // TODO: Make this an option and push to the stack or something on an error
    #[inline]
    pub fn reserve_reg(&mut self, sym: impl Into<Option<Sym>>) -> Result<Register> {
        let (idx, _) = self
            .registers
            .iter()
            .enumerate()
            .find(|(_, idx)| idx.is_none())
            .ok_or({
                RuntimeError {
                    ty: RuntimeErrorTy::CompilationError,
                    message: "Failed to fetch available register".to_string(),
                }
            })?;

        let sym = sym.into();
        trace!(
            "Reserving register {} for {:?} in function {:?}",
            idx,
            sym,
            self.name
        );

        self.registers[idx] = Some(sym);
        Ok((idx as u8).into())
    }

    #[track_caller]
    pub fn get_cached_reg(&mut self, sym: Sym) -> Result<Register> {
        trace!("Getting cached register {:?}", sym);
        match self.registers.iter().position(|r| *r == Some(Some(sym))) {
            Some(pos) => Ok((pos as u8).into()),
            None => {
                error!(
                    "Failed to get cached register, attempted to get {:?} in function {:?} Loc: {:?}",
                    sym,
                    self.name,
                    std::panic::Location::caller(),
                );
                Err(RuntimeError {
                    ty: RuntimeErrorTy::CompilationError,
                    message: "Failed to fetch cached register".to_string(),
                })
            }
        }
    }

    #[inline]
    pub fn add_var(&mut self, sym: Sym) {
        self.variables.insert(sym);
    }

    pub fn build(mut self, builder: &mut CodeBuilder) -> Result<Vec<Instruction>> {
        trace!("Building and Optimizing function");

        let total = self.blocks.len();
        for (offset, idx) in FlowGraphAnalyzer::new()
            .analyze(&self.blocks.to_vec())
            .into_iter()
            .enumerate()
        {
            trace!("Removing block {}/{}", idx + 1, total);
            self.blocks.remove(idx - offset);
        }

        let len = self.blocks.len();
        let mut removed_blocks: Vec<usize> = Vec::with_capacity(10);
        for _ in 0..2 {
            for (_idx, mut block) in self.blocks.iter_mut().enumerate() {
                StackDeduplicator::new().run(&mut block);
                NoopRemover::new().run(&mut block);
            }

            for (offset, idx) in removed_blocks.drain(..).enumerate() {
                trace!("Removing block {}/{}", idx + 1, len);
                self.blocks.remove(idx - offset);
            }
        }

        let mut changes = Vec::with_capacity(20);
        for (current_index, block) in self.blocks.iter().enumerate() {
            for (inst_index, inst) in block.block.iter().enumerate() {
                match inst.uninit_inst {
                    Instruction::Jump(idx) | Instruction::JumpComp(idx) => {
                        let idx = idx as usize;

                        fn get_distance(
                            ctx: &FunctionContext,
                            (x1, y1): (usize, usize),
                            (x2, y2): (usize, usize),
                        ) -> usize {
                            if x1 == x2 {
                                y2 - y1
                            } else {
                                ctx.blocks[x1].block.len() - y1
                                    + y2
                                    + ctx.blocks[x1 + 1..x2]
                                        .iter()
                                        .map(|b| b.block.len())
                                        .sum::<usize>()
                            }
                        }

                        let offset = if idx > current_index {
                            get_distance(&self, (current_index, inst_index), (idx, 0)) as i32
                        } else {
                            -(get_distance(&self, (idx, 0), (current_index, inst_index)) as i32)
                        };

                        // if current_index as i32 + offset >= block.block.len() as i32 {
                        //     offset -= 1;
                        // }

                        changes.push((current_index, inst_index, offset));
                    }
                    _ => {}
                }
            }
        }

        for (block, instruction, offset) in changes {
            if let Instruction::Jump(_) = self.blocks[block].block[instruction].uninit_inst {
                self.blocks[block].block[instruction].uninit_inst = Instruction::Jump(offset);
            } else if let Instruction::JumpComp(_) =
                self.blocks[block].block[instruction].uninit_inst
            {
                self.blocks[block].block[instruction].uninit_inst = Instruction::JumpComp(offset);
            }
        }

        let mut instructions = Vec::with_capacity(self.blocks.len() * 2);
        for block in self.blocks {
            for instruction in block.block {
                instructions.push(instruction.solidify(builder)?);
            }
        }

        trace!("Finished Building and Optimizing function");
        Ok(instructions)
    }
}

impl std::ops::Index<usize> for FunctionContext {
    type Output = Block;

    fn index(&self, idx: usize) -> &Self::Output {
        &self.blocks[idx]
    }
}

impl std::ops::IndexMut<usize> for FunctionContext {
    fn index_mut(&mut self, idx: usize) -> &mut Block {
        &mut self.blocks[idx]
    }
}
