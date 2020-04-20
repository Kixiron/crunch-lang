#![no_std]

extern crate alloc;

use alloc::{boxed::Box, vec::Vec};
use core::fmt;

pub struct SemanticAnalyzer {
    passes: Vec<Box<dyn SemanticPass>>,
}

impl fmt::Debug for SemanticAnalyzer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("SemanticAnalyzer")
            .field(
                "passes",
                &self
                    .passes
                    .iter()
                    .map(|pass| pass.name())
                    .collect::<Vec<_>>(),
            )
            .finish()
    }
}

pub trait SemanticPass {
    fn name(&self) -> &'static str;

    // TODO: Give access to other file's symbol tables
    fn analyze_file<'stmt, 'expr>(
        &mut self,
        tree: &[Ast<'stmt, 'expr>],
        local_symbol_table: &SymbolTable,
    );
}
