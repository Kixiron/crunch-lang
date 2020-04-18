use crate::{
    error::{Error, Locatable, Location, ParseResult, TypeError},
    FileId, Interner, SmallSpur,
};

use cfg_if::cfg_if;

cfg_if! {
    if #[cfg(feature = "concurrent")] {
        use alloc::sync::Arc;
        use dashmap::DashMap;
    } else {
        use lasso::Rodeo;
        use std::collections::HashMap;
    }
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct SymbolLocation {
    file: FileId,
    sym: SmallSpur,
}

impl SymbolLocation {
    pub fn new(file: FileId, sym: SmallSpur) -> Self {
        Self { file, sym }
    }
}

#[derive(Debug, Clone)]
pub struct SymbolTable {
    #[cfg(feature = "concurrent")]
    table: Arc<DashMap<SymbolLocation, ()>>,

    #[cfg(not(feature = "concurrent"))]
    table: HashMap<SymbolLocation, ()>,
}

impl SymbolTable {
    pub fn new() -> Self {
        cfg_if! {
            if #[cfg(feature = "concurrent")] {
                Self { table: Arc::new(DashMap::new()) }
            } else {
                Self { table: HashMap::new() }
            }
        }
    }

    pub fn insert(&mut self, interner: &Interner, key: SymbolLocation, val: ()) -> ParseResult<()> {
        if let Some(_prev) = self.table.insert(key, val) {
            Err(Locatable::new(
                Error::Type(TypeError::Redefinition(
                    interner.resolve(&key.sym).to_owned(),
                    Location::file(key.file),
                )),
                Location::file(key.file),
            ))
        } else {
            Ok(())
        }
    }
}
