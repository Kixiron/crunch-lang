use crate::{
    strings::StrInterner,
    trees::hir::{Expr, Item, Stmt, Type, TypeId},
    utils::HashMap,
};
use alloc::rc::Rc;
use core::{
    cell::RefCell,
    fmt::{Debug, Formatter, Result as FmtResult},
    mem,
};
use typed_arena::Arena;

// TODO: Node interning
// TODO: Arenas struct with `.with_arenas(|arenas| { .. })` method that keeps lifetimes sequestered and
//       automatically drops arenas at the end of the closure, also allow pre-allocation since we can reasonably
//       guess hir & mir tree sizes
// pub struct Arenas<'ctx> {
//     // TODO: Sharding or something real fancy & concurrent
//     // TODO: Better names
//     hir_item: Rc<Arena<Item<'ctx>>>,
//     hir_stmt: Rc<Arena<Stmt<'ctx>>>,
//     hir_expr: Rc<Arena<Expr<'ctx>>>,
//     hir_type: Rc<Arena<Type>>,
// }
//
// impl<'ctx> Arenas<'ctx> {
//     pub fn with_arenas<F, T>(&mut self, with: F) -> T
//     where
//         F: for<'arena> FnOnce(&'arena ArenaHandle<'arena>) -> T,
//     {
//         let return_value = {
//             let handle = ArenaHandle::new(self);
//             with(&handle)
//         };
//
//         // Clear the arenas
//         {
//             mem::take(&mut self.hir_item);
//             mem::take(&mut self.hir_stmt);
//             mem::take(&mut self.hir_expr);
//             mem::take(&mut self.hir_type);
//         }
//
//         return_value
//     }
// }
//
// pub struct ArenaHandle<'ctx> {
//     arenas: &'ctx Arenas<'ctx>
// }

#[derive(Clone)]
#[allow(missing_debug_implementations)]
pub struct Context<'ctx> {
    pub strings: StrInterner,
    // TODO: Have an Arenas struct that holds all arenas, hold an &'ctx Arenas<'ctx>
    hir_item: Rc<Arena<Item<'ctx>>>,
    hir_stmt: Rc<Arena<Stmt<'ctx>>>,
    hir_expr: Rc<Arena<Expr<'ctx>>>,
    hir_type: Rc<Arena<Type>>,
    // TODO: Maybe just use a vec for this
    hir_type_map: Rc<RefCell<HashMap<TypeId, &'ctx Type>>>,
    // TODO: Just an AtomicUsize for threading
    hir_type_id: Rc<RefCell<usize>>,
}

impl<'ctx> Context<'ctx> {
    #[inline]
    pub fn new() -> Self {
        Self {
            strings: StrInterner::new(),
            hir_item: Rc::new(Arena::new()),
            hir_stmt: Rc::new(Arena::new()),
            hir_expr: Rc::new(Arena::new()),
            hir_type: Rc::new(Arena::new()),
            hir_type_map: Rc::new(RefCell::new(HashMap::new())),
            hir_type_id: Rc::new(RefCell::new(0)),
        }
    }

    #[inline]
    pub const fn strings(&self) -> &StrInterner {
        &self.strings
    }

    // In regards to the following: Fuck you, you deal with this bullshit

    #[inline]
    pub fn hir_item(&self, item: Item<'ctx>) -> &'ctx Item<'ctx> {
        unsafe { mem::transmute::<&'_ Item<'ctx>, &'ctx Item<'ctx>>(self.hir_item.alloc(item)) }
    }

    // TODO: Replace with queries?
    #[inline]
    pub fn hir_stmt(&self, stmt: Stmt<'ctx>) -> &'ctx Stmt<'ctx> {
        unsafe { mem::transmute::<&'_ Stmt<'ctx>, &'ctx Stmt<'ctx>>(self.hir_stmt.alloc(stmt)) }
    }

    #[inline]
    pub fn hir_expr(&self, expr: Expr<'ctx>) -> &'ctx Expr<'ctx> {
        unsafe { mem::transmute::<&'_ Expr<'ctx>, &'ctx Expr<'ctx>>(self.hir_expr.alloc(expr)) }
    }

    #[inline]
    pub fn hir_type(&self, ty: Type) -> TypeId {
        let reference = unsafe { mem::transmute::<&'_ Type, &'ctx Type>(self.hir_type.alloc(ty)) };

        let id = TypeId::new(*self.hir_type_id.borrow());
        *self.hir_type_id.borrow_mut() += 1;

        // FIXME: https://github.com/rust-lang/rust/issues/62633
        let prev_type = self.hir_type_map.borrow_mut().insert(id, reference);
        assert!(prev_type.is_none(), "A HIR type was double-inserted");

        id
    }

    pub fn overwrite_hir_type(&self, target: TypeId, new: TypeId) {
        let new: &'ctx Type = *self
            .hir_type_map
            .borrow()
            .get(&new)
            .expect("Attempted to get a type that does not exist");

        self.hir_type_map
            .borrow_mut()
            .insert(target, new)
            .expect("Context::overwrite_hir_type was called but no type was overwritten");
    }

    #[inline]
    pub fn get_hir_type(&self, id: TypeId) -> Option<&'ctx Type> {
        self.hir_type_map.borrow().get(&id).map(|ty| *ty)
    }
}

impl Debug for Context<'_> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        f.debug_struct("Context").finish()
    }
}

impl<'ctx> Default for Context<'ctx> {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}
