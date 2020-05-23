use cfg_if::cfg_if;

cfg_if! {
    if #[cfg(feature = "concurrent")] {
        use lasso::ThreadedRodeo;
        use alloc::sync::Arc;
    } else {
        use lasso::Rodeo;
        use alloc::rc::Rc;
        use core::cell::RefCell;
    }
}

pub use lasso::Spur;

#[derive(Debug, Clone)]
pub struct Interner {
    #[cfg(feature = "concurrent")]
    interner: Arc<ThreadedRodeo<Spur>>,

    #[cfg(not(feature = "concurrent"))]
    interner: Rc<RefCell<Rodeo<Spur>>>,
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
                    interner: Rc::new(RefCell::new(Rodeo::with_capacity(100))),
                }
            }
        }
    }

    #[cfg(feature = "concurrent")]
    pub fn resolve<'a>(&'a self, sym: &Spur) -> &'a str {
        self.interner.resolve(sym)
    }

    #[cfg(feature = "concurrent")]
    pub fn intern(&mut self, string: &str) -> Spur {
        self.interner.get_or_intern(string)
    }

    #[cfg(not(feature = "concurrent"))]
    pub fn resolve<'a>(&'a self, sym: &Spur) -> &'a str {
        self.interner.borrow().resolve(sym)
    }

    #[cfg(not(feature = "concurrent"))]
    pub fn intern(&mut self, string: &str) -> Spur {
        self.interner.borrow_mut().get_or_intern(string)
    }
}

impl Default for Interner {
    fn default() -> Self {
        Self::new()
    }
}
