use super::pass_prelude::*;

pub struct FlowGraphAnalyzer {}

impl FlowGraphAnalyzer {
    pub fn new() -> Self {
        Self {}
    }

    pub fn analyze(self, blocks: &[Block]) -> Vec<usize> {
        let mut nodes = blocks
            .iter()
            .map(|block| (FlowNode::new(), block))
            .collect::<Vec<_>>();

        let mut to_visit = Vec::new();
        'visitor: for (idx, (node, block)) in nodes.iter_mut().enumerate() {
            if !node.is_touched() {
                trace!("Touched node {}/{}", idx + 1, blocks.len());
                node.touch();

                for (inst, _) in block.block.iter() {
                    match inst {
                        Instruction::Jump(idx) | Instruction::JumpComp(idx) => {
                            to_visit.push(*idx as usize)
                        }

                        Instruction::Halt | Instruction::Return => continue 'visitor,

                        _ => {}
                    }
                }
            }
        }

        'visitor_popper: while let Some(idx) = to_visit.pop() {
            if !nodes[idx].0.is_touched() {
                trace!("Touched node {}/{}", idx + 1, blocks.len());
                nodes[idx].0.touch();

                for (inst, _) in nodes[idx].1.block.iter() {
                    match inst {
                        Instruction::Jump(idx) | Instruction::JumpComp(idx) => {
                            to_visit.push(*idx as usize)
                        }

                        Instruction::Halt | Instruction::Return => continue 'visitor_popper,

                        _ => {}
                    }
                }
            }
        }

        let mut removed_blocks = Vec::with_capacity(10);

        for idx in nodes
            .into_iter()
            .enumerate()
            .filter_map(
                |(idx, (node, _))| {
                    if node.is_touched() {
                        None
                    } else {
                        Some(idx)
                    }
                },
            )
        {
            removed_blocks.push(idx);
        }

        removed_blocks
    }
}

#[derive(Debug, Copy, Clone)]
struct FlowNode {
    reached: bool,
}

impl FlowNode {
    pub fn new() -> Self {
        Self { reached: false }
    }

    pub fn touch(&mut self) {
        self.reached = true;
    }

    pub fn is_touched(self) -> bool {
        self.reached
    }
}
