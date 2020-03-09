use super::pass_prelude::*;

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

        for (idx, (inst, _)) in block.block.iter().enumerate() {
            match inst {
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
