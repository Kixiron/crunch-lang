#![cfg_attr(not(feature = "std"), no_std)]

extern crate alloc;
pub extern crate string_interner;

mod error;
mod files;
pub mod parser;
pub mod symbol_table;
mod token;

pub use parser::Parser;

#[cfg(feature = "concurrent")]
type SymbolTable = dashmap::DashMap<Sym, ()>;
#[cfg(not(feature = "concurrent"))]
type SymbolTable = hashbrown::HashMap<Sym, ()>;

// TODO: DashMap-based interner
#[cfg(feature = "concurrent")]
type Interner = alloc::sync::Arc<parking_lot::RwLock<string_interner::StringInterner<Sym>>>;
#[cfg(not(feature = "concurrent"))]
type Interner = string_interner::StringInterner<Sym>;

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
