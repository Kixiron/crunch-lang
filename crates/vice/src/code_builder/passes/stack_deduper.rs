use super::pass_prelude::*;

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

        for (idx, (inst, _)) in block.block.iter().enumerate() {
            if let Instruction::Pop(reg) = inst {
                if let (Some(Instruction::Pop(pop_one)), Some(Instruction::Push(push))) =
                    (self.second_to_last.clone(), self.last.clone())
                {
                    if pop_one == push && push == *reg {
                        trace!("Found a sequence of redundant Pop(r), Push(r), Pop(r)");
                        remove_indices.extend_from_slice(&[idx - 1, idx]);
                    }
                }
            }

            self.second_to_last = self.last.clone();
            self.last = Some(inst.clone());
        }

        for (offset, idx) in remove_indices.into_iter().enumerate() {
            block.block.remove(idx - offset);
        }
    }
}
