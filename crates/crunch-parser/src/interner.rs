use alloc::string::String;
use cfg_if::cfg_if;

#[derive(Debug, Clone)]
pub struct Interner {
    // TODO: DashMap-based interner
    #[cfg(feature = "concurrent")]
    interner: alloc::sync::Arc<parking_lot::RwLock<string_interner::StringInterner<Sym>>>,

    #[cfg(not(feature = "concurrent"))]
    interner: string_interner::StringInterner<Sym>,
}

impl Interner {
    pub fn new() -> Self {
        cfg_if! {
            if #[cfg(feature = "concurrent")] {
                Self {
                    interner: alloc::sync::Arc::new(parking_lot::RwLock::new(
                        string_interner::StringInterner::with_capacity(100),
                    )),
                }

            } else {
                Self {
                    interner: string_interner::StringInterner::with_capacity(100),
                }
            }
        }
    }

    pub fn resolve(&self, sym: Sym) -> Option<String> {
        cfg_if! {
            if #[cfg(feature = "concurrent")] {
                self.interner.read().resolve(sym).map(str::to_string)
            } else {
                self.interner.resolve(sym).map(str::to_string)
            }
        }
    }

    pub fn intern(&self, string: &str) -> Sym {
        cfg_if! {
            if #[cfg(feature = "concurrent")] {
                if let Some(sym) = self.interner.read().get(string) {
                    return sym;
                }

                self.interner.write().get_or_intern(string)
            } else {
                if let Some(sym) = self.interner.get(string) {
                    return sym;
                }

                self.interner.get_or_intern(string)
            }
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct Sym(core::num::NonZeroU64);

impl string_interner::Symbol for Sym {
    /// Creates a `Sym` from the given `usize`.
    ///
    /// # Panics
    ///
    /// If the given `usize` is greater than `u32::MAX - 1`.
    fn from_usize(val: usize) -> Self {
        assert!(
            val < u64::max_value() as usize,
            "Symbol value {} is too large and not supported by `string_interner::Sym` type",
            val
        );
        Sym(
            core::num::NonZeroU64::new((val + 1) as u64).unwrap_or_else(|| {
                unreachable!("Should never fail because `val + 1` is nonzero and `<= u32::MAX`")
            }),
        )
    }

    fn to_usize(self) -> usize {
        (self.0.get() as usize) - 1
    }
}

impl From<usize> for Sym {
    fn from(val: usize) -> Self {
        <Sym as string_interner::Symbol>::from_usize(val)
    }
}
