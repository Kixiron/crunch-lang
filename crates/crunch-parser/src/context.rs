use crate::parser::{Ast, Expr, Stmt, Type};

use cfg_if::cfg_if;
use core::{cell::RefCell, fmt, num::NonZeroUsize};
use lasso::Spur;
use stadium::Stadium;

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

cfg_if! {
    if #[cfg(feature = "no-std")] {
        use hashbrown::HashSet;
    }  else {
        use std::collections::HashSet;
    }
}

/// A token for an interned string
#[cfg_attr(test, derive(serde::Deserialize, serde::Serialize))]
#[derive(Debug, Copy, Clone, PartialEq, Eq, Ord, PartialOrd, Hash)]
#[repr(transparent)]
pub struct StrT(Spur);

impl StrT {
    #[cfg(test)]
    pub fn new(key: usize) -> Self {
        use lasso::Key;

        Self(Spur::try_from_usize(key).unwrap())
    }
}

#[derive(Debug)]
pub struct Context<'ctx> {
    interner: StrInterner,
    interners: RefCell<Interners<'ctx>>,
    arenas: RefCell<Arenas<'ctx>>,
}

impl<'ctx> Context<'ctx> {
    pub fn new() -> Self {
        Self {
            interner: new_interner(),
            interners: RefCell::new(Interners::new()),
            arenas: RefCell::new(Arenas::new()),
        }
    }

    pub fn resolve<'a>(&'a self, sym: StrT) -> &'a str {
        #[cfg(feature = "concurrent")]
        return self.interner.resolve(&sym.0);

        #[cfg(not(feature = "concurrent"))]
        return self.interner.resolve(&sym.as_ref().0);
    }

    pub fn intern(&self, string: impl AsRef<str>) -> StrT {
        #[cfg(feature = "concurrent")]
        return StrT(self.interner.get_or_intern(string.as_ref()));

        #[cfg(not(feature = "concurrent"))]
        return StrT(
            Rc::get_mut(&mut self.interner)
                .expect("Multiple mutable borrows of an interner")
                .get_or_intern(string.as_ref()),
        );
    }

    pub fn store<I: Internable<'ctx>>(&'ctx self, internable: I) -> &'ctx I {
        internable.intern(self)
    }
}

impl<'ctx> Default for Context<'ctx> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'ctx> fmt::Debug for Arenas<'ctx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Arenas").finish()
    }
}

#[derive(Debug)]
struct Interners<'ctx> {
    stmt: HashSet<&'ctx Stmt<'ctx>>,
    expr: HashSet<&'ctx Expr<'ctx>>,
    types: HashSet<&'ctx Type<'ctx>>,
    ast: HashSet<&'ctx Ast<'ctx>>,
}

impl<'ctx> Interners<'ctx> {
    pub fn new() -> Self {
        Self {
            stmt: HashSet::with_capacity(100),
            expr: HashSet::with_capacity(100),
            types: HashSet::with_capacity(100),
            ast: HashSet::with_capacity(100),
        }
    }
}

struct Arenas<'ctx> {
    stmt: Stadium<'ctx, Stmt<'ctx>>,
    expr: Stadium<'ctx, Expr<'ctx>>,
    types: Stadium<'ctx, Type<'ctx>>,
    ast: Stadium<'ctx, Ast<'ctx>>,
}

impl<'ctx> Arenas<'ctx> {
    pub fn new() -> Self {
        Self {
            stmt: Stadium::with_capacity(NonZeroUsize::new(1024).unwrap()),
            expr: Stadium::with_capacity(NonZeroUsize::new(1024).unwrap()),
            types: Stadium::with_capacity(NonZeroUsize::new(1024).unwrap()),
            ast: Stadium::with_capacity(NonZeroUsize::new(1024).unwrap()),
        }
    }
}

pub trait Internable<'ctx> {
    fn intern(self, ctx: &'ctx Context<'ctx>) -> &'ctx Self;
}

impl<'ctx> Internable<'ctx> for Ast<'ctx> {
    fn intern(self, ctx: &'ctx Context<'ctx>) -> &'ctx Self {
        if let Some(interned) = ctx.interners.borrow().ast.get(&self) {
            *interned
        } else {
            // TODO: FML
            let allocated =
                unsafe { core::mem::transmute(&*ctx.arenas.borrow_mut().ast.store(self)) };
            ctx.interners.borrow_mut().ast.insert(allocated);

            allocated
        }
    }
}

impl<'ctx> Internable<'ctx> for Stmt<'ctx> {
    fn intern(self, ctx: &'ctx Context<'ctx>) -> &'ctx Self {
        if let Some(interned) = ctx.interners.borrow().stmt.get(&self) {
            *interned
        } else {
            // TODO: FML
            let allocated =
                unsafe { core::mem::transmute(&*ctx.arenas.borrow_mut().stmt.store(self)) };
            ctx.interners.borrow_mut().stmt.insert(allocated);

            allocated
        }
    }
}

impl<'ctx> Internable<'ctx> for Expr<'ctx> {
    fn intern(self, ctx: &'ctx Context<'ctx>) -> &'ctx Self {
        if let Some(interned) = ctx.interners.borrow().expr.get(&self) {
            *interned
        } else {
            // TODO: FML
            let allocated =
                unsafe { core::mem::transmute(&*ctx.arenas.borrow_mut().expr.store(self)) };
            ctx.interners.borrow_mut().expr.insert(allocated);

            allocated
        }
    }
}

impl<'ctx> Internable<'ctx> for Type<'ctx> {
    fn intern(self, ctx: &'ctx Context<'ctx>) -> &'ctx Self {
        if let Some(interned) = ctx.interners.borrow().types.get(&self) {
            *interned
        } else {
            // TODO: FML
            let allocated =
                unsafe { core::mem::transmute(&*ctx.arenas.borrow_mut().types.store(self)) };
            ctx.interners.borrow_mut().types.insert(allocated);

            allocated
        }
    }
}
