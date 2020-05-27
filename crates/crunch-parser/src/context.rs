use crate::parser::{Ast, Expr, Stmt, Type};

use cfg_if::cfg_if;
use core::{
    cell::RefCell,
    fmt,
    hash::{Hash, Hasher},
    num::NonZeroUsize,
    ops::Deref,
};
use lasso::Spur;
use stadium::{Stadium, Ticket};

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

    pub fn store<I: Internable<'ctx>>(&self, internable: I) -> Ticket<'ctx, I> {
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

#[derive(Debug, Copy, Clone)]
enum HashRef<'a, T> {
    Ptr(Ticket<'a, T>),
    Ref(&'a T),
}

impl<'a, T> HashRef<'a, T> {
    fn into_ptr(&self) -> Ticket<'a, T> {
        if let Self::Ptr(ptr) = self {
            *ptr
        } else {
            unreachable!("Internal error, only pointers can be stored");
        }
    }
}

impl<'a, T: PartialEq> PartialEq for HashRef<'a, T> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Ptr(l), Self::Ptr(r)) => **l == **r,
            (Self::Ref(l), Self::Ref(r)) => l == r,
            (Self::Ptr(l), Self::Ref(r)) => l.deref() == *r,
            (Self::Ref(l), Self::Ptr(r)) => *l == r.deref(),
        }
    }
}

impl<'a, T: Eq> Eq for HashRef<'a, T> {}

impl<'a, T: Hash> Hash for HashRef<'a, T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match *self {
            Self::Ptr(ptr) => ptr.deref().hash(state),
            Self::Ref(refer) => refer.hash(state),
        }
    }
}

#[derive(Debug)]
struct Interners<'ctx> {
    stmt: HashSet<HashRef<'ctx, Stmt<'ctx>>>,
    expr: HashSet<HashRef<'ctx, Expr<'ctx>>>,
    types: HashSet<HashRef<'ctx, Type<'ctx>>>,
    ast: HashSet<HashRef<'ctx, Ast<'ctx>>>,
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
    fn intern(self, ctx: &Context<'ctx>) -> Ticket<'ctx, Self>;
}

impl<'ctx> Internable<'ctx> for Ast<'ctx> {
    fn intern(self, ctx: &Context<'ctx>) -> Ticket<'ctx, Self> {
        if let Some(interned) = ctx.interners.borrow().ast.get(&HashRef::Ref(&self)) {
            // FIXME: FML
            unsafe { core::mem::transmute(interned.into_ptr()) }
        } else {
            let allocated = ctx.arenas.borrow_mut().ast.store(self);
            ctx.interners
                .borrow_mut()
                .ast
                .insert(HashRef::Ptr(allocated));

            allocated
        }
    }
}

impl<'ctx> Internable<'ctx> for Stmt<'ctx> {
    fn intern(self, ctx: &Context<'ctx>) -> Ticket<'ctx, Self> {
        if let Some(interned) = ctx.interners.borrow().stmt.get(&HashRef::Ref(&self)) {
            // FIXME: FML
            unsafe { core::mem::transmute(interned.into_ptr()) }
        } else {
            let allocated = ctx.arenas.borrow_mut().stmt.store(self);
            ctx.interners
                .borrow_mut()
                .stmt
                .insert(HashRef::Ptr(allocated));

            allocated
        }
    }
}

impl<'ctx> Internable<'ctx> for Expr<'ctx> {
    fn intern(self, ctx: &Context<'ctx>) -> Ticket<'ctx, Self> {
        if let Some(interned) = ctx.interners.borrow().expr.get(&HashRef::Ref(&self)) {
            // FIXME: FML
            unsafe { core::mem::transmute(interned.into_ptr()) }
        } else {
            let allocated = ctx.arenas.borrow_mut().expr.store(self);
            ctx.interners
                .borrow_mut()
                .expr
                .insert(HashRef::Ptr(allocated));

            allocated
        }
    }
}

impl<'ctx> Internable<'ctx> for Type<'ctx> {
    fn intern(self, ctx: &Context<'ctx>) -> Ticket<'ctx, Self> {
        if let Some(interned) = ctx.interners.borrow().types.get(&HashRef::Ref(&self)) {
            // FIXME: FML
            unsafe { core::mem::transmute(interned.into_ptr()) }
        } else {
            let allocated = ctx.arenas.borrow_mut().types.store(self);
            ctx.interners
                .borrow_mut()
                .types
                .insert(HashRef::Ptr(allocated));

            allocated
        }
    }
}
