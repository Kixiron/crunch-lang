use super::Sym;

use cfg_if::cfg_if;

cfg_if! {
    if #[cfg(feature = "concurrent")] {
        use dashmap::DashMap;
        use alloc::sync::Arc;

        type Table = Arc<DashMap<Sym, ()>>;
    } else {
        use hashbrown::HashMap;
        use alloc::rc::Rc;

        type Table = Rc<HashMap<Sym, ()>>;
    }
}

#[derive(Debug, Clone)]
pub struct SymbolTable {
    table: Table,
}
