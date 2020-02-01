use super::*;

#[derive(Debug, Clone)]
pub struct FunctionContext {
    pub registers: [Option<Option<Sym>>; NUMBER_REGISTERS],
    pub variables: HashSet<Sym>,
    pub blocks: Vec<Block>,
    block_id: u32,
}

impl FunctionContext {
    #[inline]
    pub fn new() -> Self {
        Self {
            registers: [None; NUMBER_REGISTERS],
            variables: HashSet::new(),
            blocks: Vec::with_capacity(20),
            block_id: 0,
        }
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

    // TODO: Make this an option and push to the stack or something on an error
    #[inline]
    pub fn reserve_reg(&mut self, sym: impl Into<Option<Sym>>) -> Result<Register> {
        let (idx, _) = self
            .registers
            .iter()
            .enumerate()
            .rev()
            .find(|(_idx, r)| r.is_none())
            .ok_or({
                RuntimeError {
                    ty: RuntimeErrorTy::CompilationError,
                    message: "Failed to fetch available register".to_string(),
                }
            })?;

        let sym = sym.into();
        trace!("Reserving register {} for {:?}", idx, sym);

        self.registers[idx] = Some(sym);
        Ok((idx as u8).into())
    }

    #[track_caller]
    pub fn get_cached_reg(&mut self, sym: Sym) -> Result<Register> {
        match self.registers.iter().position(|r| *r == Some(Some(sym))) {
            Some(pos) => Ok((pos as u8).into()),
            None => {
                error!(
                    "Failed to get cached register, attempted to get {:?} Loc: {:?}",
                    sym,
                    std::panic::Location::caller(),
                );
                Err(RuntimeError {
                    ty: RuntimeErrorTy::CompilationError,
                    message: "Failed to fetch cached register".to_string(),
                })
            }
        }
    }

    pub fn reserve_nth_reg(&mut self, reg: impl Into<u8>) -> Result<Register> {
        let reg = reg.into();
        if self.registers.get(reg as usize) == Some(&None) {
            self.registers[reg as usize] = Some(None);
            Ok(reg.into())
        } else {
            error!("Failed to reserve nth register, attempted to get {}", reg);
            Err(RuntimeError {
                ty: RuntimeErrorTy::CompilationError,
                message: "Failed to reserve register".to_string(),
            })
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
            .analyze(&self.blocks.iter().cloned().collect::<Vec<_>>())
            .into_iter()
            .enumerate()
        {
            trace!("Removing block {}/{}", idx + 1, total);
            self.blocks.remove(idx - offset);
        }

        let len = self.blocks.len();
        let mut removed_blocks: Vec<usize> = Vec::with_capacity(10);
        for _ in 0..2 {
            for (idx, mut block) in self.blocks.iter_mut().enumerate() {
                StackDeduplicator::new().run(&mut block);
                NoopRemover::new().run(&mut block);
                // if EmptyBlockCuller::new().run(&mut block) {
                //     removed_blocks.push(idx);
                // }
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
                    Instruction::Jump(id) | Instruction::JumpComp(id) => {
                        let idx = self.blocks.iter().position(|b| b.id == id as u32).unwrap();
                        let mut offset = 0;

                        let range = if idx < current_index {
                            idx..=current_index
                        } else {
                            current_index..=idx
                        };

                        for i in range {
                            if let Some(block) = self.blocks.get(i) {
                                offset += block.block.len();
                            }
                        }

                        let offset = if idx < current_index {
                            -(offset as i32)
                        } else {
                            offset as i32
                        };

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

        let mut instructions = Vec::with_capacity({
            let mut capacity = 0;
            for block in self.blocks.iter() {
                capacity += block.block.len();
            }
            capacity
        });

        let mut dbg = Vec::new();
        for (idx, block) in self.blocks.into_iter().enumerate() {
            for instruction in block.block {
                dbg.push(instruction.solidify(builder)?);
            }

            trace!("Block #{}: {:?}", idx, &dbg);
            instructions.extend_from_slice(&dbg.drain(..).collect::<Vec<_>>());
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
