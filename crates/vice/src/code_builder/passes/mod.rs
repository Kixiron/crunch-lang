mod flow_graph_analyzer;
mod noop_remover;
mod stack_deduper;

pub use flow_graph_analyzer::FlowGraphAnalyzer;
pub use noop_remover::NoopRemover;
pub use stack_deduper::StackDeduplicator;

use crate::code_builder::Block;

pub trait BlockOptimizer: Default {
    fn run(&mut self, block: &mut Block);

    fn reset(&mut self) {
        *self = Self::default();
    }
}

mod pass_prelude {
    pub use super::BlockOptimizer;
    pub use crate::code_builder::Block;

    pub use compactor::Instruction;
    pub use crunch_error::compile_prelude::*;

    pub use alloc::vec::Vec;
}
