use crate::{error::ParseResult, parser::SyntaxTree};

use cfg_if::cfg_if;
use dashmap::DashMap;
use lasso::SmallSpur;

use alloc::{sync::Arc, vec::Vec};
use core::ops::Deref;

cfg_if! {
    if #[cfg(feature = "no-std")] {
        use hashbrown::HashMap;
    } else {
        use std::collections::HashMap;
    }
}

#[cfg(not(feature = "concurrent"))]
compile_error!("Make the single threaded one");

#[derive(Debug)]
pub struct SymbolTable<'expr, 'stmt> {
    packages: Arc<DashMap<SmallSpur, Package<'expr, 'stmt>>>,
}

#[derive(Debug)]
struct Package<'expr, 'stmt> {
    files: HashMap<SmallSpur, File<'expr, 'stmt>>,
}

#[derive(Debug)]
struct File<'expr, 'stmt> {
    imports: Vec<SmallSpur>,
    namespace: HashMap<SmallSpur, (Option<Scope>, NodeRef)>,
    nodes: SyntaxTree<'expr, 'stmt>,
}

#[derive(Debug)]
struct Scope {}

#[derive(Debug, Copy, Clone)]
struct NodeRef(usize);

impl<'expr, 'stmt> SymbolTable<'expr, 'stmt> {
    pub fn new() -> Self {
        todo!()
    }

    pub fn with_capacity(capacity: usize) -> Self {
        todo!()
    }

    pub fn build_table(&mut self, tree: &SyntaxTree) -> ParseResult<()> {
        for node in tree.deref() {}

        Ok(())
    }
}
