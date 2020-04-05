use cfg_if::cfg_if;

cfg_if! {
    if #[cfg(feature = "concurrent")] {
        use lasso::ThreadedRodeo;
        use alloc::sync::Arc;
    } else {
        use lasso::Rodeo;
    }
}

pub use lasso::Cord;

#[derive(Debug, Clone)]
pub struct Interner {
    // TODO: DashMap-based interner
    #[cfg(feature = "concurrent")]
    interner: Arc<ThreadedRodeo<Cord>>,

    #[cfg(not(feature = "concurrent"))]
    interner: Rodeo<Cord>,
}

impl Interner {
    pub fn new() -> Self {
        cfg_if! {
            if #[cfg(feature = "concurrent")] {
                Self {
                    interner: Arc::new(ThreadedRodeo::with_capacity(100)),
                }

            } else {
                Self {
                    interner: Rodeo::with_capacity(100),
                }
            }
        }
    }

    pub fn resolve<'a>(&'a self, sym: &Cord) -> &'a str {
        self.interner.resolve(sym)
    }

    pub fn intern(&self, string: &str) -> Cord {
        self.interner.get_or_intern(string)
    }
}

impl Default for Interner {
    fn default() -> Self {
        Self::new()
    }
}
