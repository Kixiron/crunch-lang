use super::{passes, Block, CodeBuilder};

use compactor::{Instruction, NUMBER_REGISTERS};
use crunch_error::compile_prelude::*;
use crunch_parser::string_interner::Sym;

use alloc::collections::BTreeSet;

#[derive(Debug, Clone)]
pub struct FunctionContext {
    pub registers: [Option<Option<Sym>>; NUMBER_REGISTERS],
    pub variables: BTreeSet<Sym>,
    pub blocks: Vec<Block>,
    pub current_block: usize,
    pub name: Sym,
}

impl FunctionContext {
    #[inline]
    pub fn new(name: Sym) -> Self {
        let mut new = Self {
            registers: [None; NUMBER_REGISTERS],
            variables: BTreeSet::new(),
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
    pub fn free_reg(&mut self, reg: u8) -> &mut Self {
        trace!("Freeing register {}", reg);

        self.registers[reg as usize] = None;

        self
    }

    pub fn inst_mov_block(&mut self, target: u8, source: u8, block: usize) -> &mut Block {
        let mut temp = None;
        std::mem::swap(&mut self.registers[source as usize], &mut temp);
        self.registers[target as usize] = temp;

        self.blocks[block].push_info(Instruction::Move(target, source), None);

        &mut self.blocks[block]
    }

    pub fn inst_drop_block(&mut self, register: u8, block: usize) -> &mut Block {
        trace!(
            "Dropping reg {} from {:?}",
            register,
            std::panic::Location::caller()
        );

        self.blocks[block].push_info(Instruction::Drop(register), None);
        self.free_reg(register);

        &mut self.blocks[block]
    }

    pub fn inst_push_arr(&mut self, array: u8, value: u8, block: usize) -> &mut Block {
        self.blocks[block].push_info(Instruction::PushArray { array, value }, None);
        self.free_reg(value);

        &mut self.blocks[block]
    }

    // TODO: Make this an option and push to the stack or something on an error
    #[inline]
    pub fn reserve_reg(&mut self, sym: impl Into<Option<Sym>>) -> CompileResult<u8> {
        let (idx, _) = self
            .registers
            .iter()
            .enumerate()
            .find(|(_, idx)| idx.is_none())
            .ok_or({
                CompileError::new(
                    CompileErrorTy::OverflowedRegisters,
                    "Failed to fetch available register",
                )
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
    pub fn get_cached_reg(&mut self, sym: Sym) -> CompileResult<u8> {
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
                Err(CompileError::new(
                    CompileErrorTy::OverflowedRegisters,
                    "Failed to fetch cached register",
                ))
            }
        }
    }

    #[inline]
    pub fn add_var(&mut self, sym: Sym) {
        self.variables.insert(sym);
    }

    pub fn build(mut self, builder: &mut CodeBuilder) -> CompileResult<Vec<Instruction>> {
        use passes::BlockOptimizer;

        trace!("Building and Optimizing function");

        let total = self.blocks.len();
        for (offset, idx) in passes::FlowGraphAnalyzer::new()
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
                passes::StackDeduplicator::new().run(&mut block);
                passes::NoopRemover::new().run(&mut block);
            }

            for (offset, idx) in removed_blocks.drain(..).enumerate() {
                trace!("Removing block {}/{}", idx + 1, len);
                self.blocks.remove(idx - offset);
            }
        }

        let mut changes = Vec::with_capacity(20);
        for (current_index, block) in self.blocks.iter().enumerate() {
            for (inst_index, (inst, _)) in block.block.iter().enumerate() {
                match inst {
                    Instruction::Jump(idx) | Instruction::JumpComp(idx) => {
                        let idx = *idx as usize;

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

                        changes.push((current_index, inst_index, offset));
                    }
                    _ => {}
                }
            }
        }

        for (block, instruction, offset) in changes {
            if let Instruction::Jump(_) = self.blocks[block].block[instruction].0 {
                self.blocks[block].block[instruction].0 = Instruction::Jump(offset);
            } else if let Instruction::JumpComp(_) = self.blocks[block].block[instruction].0 {
                self.blocks[block].block[instruction].0 = Instruction::JumpComp(offset);
            }
        }

        let mut instructions = Vec::with_capacity(self.blocks.len() * 5);
        for block in self.blocks {
            for (inst, info) in block.block {
                match inst {
                    Instruction::Func(_) => {
                        let function_name = info
                            .expect("No resolution info provided for a function call")
                            .function
                            .expect("No function symbol provided for a function call");

                        let (_instructions, func_index) =
                            if let Some(entry) = builder.functions.get(&function_name) {
                                entry
                            } else {
                                error!(
                                    "Failed to find the function {:?} ({:?})",
                                    function_name,
                                    builder.interner.resolve(function_name).unwrap()
                                );
                                return Err(CompileError::new(
                                    CompileErrorTy::MissingSymbol,
                            "A malformed function instruction was encountered during compilation",
                                ));
                            };

                        if let Some(func_index) = func_index {
                            instructions.push(Instruction::Func(*func_index));
                        } else {
                            let func_index =
                                if let Some(name) = builder.interner.resolve(function_name) {
                                    if name == "main" {
                                        0
                                    } else {
                                        builder.func_index += 1;
                                        builder.func_index - 1
                                    }
                                } else {
                                    builder.func_index += 1;
                                    builder.func_index - 1
                                };

                            let entry = builder
                                .functions
                                .get_mut(&function_name)
                                .expect("The check has already been preformed");

                            entry.1 = Some(func_index);

                            instructions.push(Instruction::Func(func_index));
                        }
                    }

                    Instruction::CallGenerator(_, reg) => {
                        let function_name = info
                            .expect("No resolution info provided for a generator call")
                            .function
                            .expect("No function symbol provided for a generator call");

                        let (_instructions, func_index) = if let Some(entry) =
                            builder.functions.get(&function_name)
                        {
                            entry
                        } else {
                            error!(
                                "Failed to find the function {:?} ({:?})",
                                function_name,
                                builder.interner.resolve(function_name).unwrap()
                            );
                            return Err(CompileError::new(
                                CompileErrorTy::MissingSymbol,
                                "A malformed function instruction was encountered during compilation",
                            ));
                        };

                        if let Some(func_index) = func_index {
                            instructions.push(Instruction::CallGenerator(*func_index, reg));
                        } else {
                            let func_index =
                                if let Some(name) = builder.interner.resolve(function_name) {
                                    if name == "main" {
                                        0
                                    } else {
                                        builder.func_index += 1;
                                        builder.func_index - 1
                                    }
                                } else {
                                    builder.func_index += 1;
                                    builder.func_index - 1
                                };

                            let entry = builder
                                .functions
                                .get_mut(&function_name)
                                .expect("The check has already been preformed");

                            entry.1 = Some(func_index);

                            instructions.push(Instruction::CallGenerator(func_index, reg));
                        }
                    }

                    other => instructions.push(other),
                }
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

// TODO: Trait for optimization passes
