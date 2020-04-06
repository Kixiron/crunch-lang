use lasso::SmallSpur;

use cfg_if::cfg_if;

cfg_if! {
    if #[cfg(feature = "concurrent")] {
        use dashmap::DashMap;
        use alloc::sync::Arc;

        type Table = Arc<DashMap<SmallSpur, ()>>;
    } else {
        cfg_if! {
            if #[cfg(feature = "no-std")] {
                use hashbrown::HashMap;
            } else {
                use std::collections::HashMap;
            }
        }
        use alloc::rc::Rc;

        type Table = Rc<HashMap<SmallSpur, ()>>;
    }
}

#[derive(Debug, Clone)]
pub struct SymbolTable {
    table: Table,
}
