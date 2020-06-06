use cfg_if::cfg_if;
use crunch_shared::strings::StrT;
use lasso::Spur;

cfg_if! {
    if #[cfg(feature = "concurrent")] {
        use lasso::ThreadedRodeo;
        use alloc::sync::Arc;

        type StrInterner = Arc<ThreadedRodeo<str, Spur>>;
    } else {
        use lasso::Rodeo;
        use alloc::rc::Rc;

        type StrInterner = Rc<Rodeo<str, Spur>>;
    }
}

pub fn new_interner() -> StrInterner {
    #[cfg(feature = "concurrent")]
    return Arc::new(ThreadedRodeo::with_capacity(2048));

    #[cfg(not(feature = "concurrent"))]
    return Rc::new(Rodeo::with_capacity(2048));
}

// TODO: Node interning

#[derive(Debug, Clone)]
pub struct Context {
    interner: StrInterner,
}

impl Context {
    pub fn new() -> Self {
        Self {
            interner: new_interner(),
        }
    }

    pub fn resolve<'a>(&'a self, sym: StrT) -> &'a str {
        #[cfg(feature = "concurrent")]
        return self.interner.resolve(&sym.get());

        #[cfg(not(feature = "concurrent"))]
        return self.interner.resolve(&sym.get());
    }

    pub fn intern(&self, string: impl AsRef<str>) -> StrT {
        #[cfg(feature = "concurrent")]
        return StrT::from(self.interner.get_or_intern(string.as_ref()));

        #[cfg(not(feature = "concurrent"))]
        return StrT::from(
            Rc::get_mut(&mut self.interner)
                .expect("Multiple mutable borrows of an interner")
                .get_or_intern(string.as_ref()),
        );
    }
}

impl Default for Context {
    fn default() -> Self {
        Self::new()
    }
}
