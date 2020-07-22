use crate::{
    allocator::CRUNCHC_ALLOCATOR,
    strings::StrInterner,
    trees::{
        ast::{Expr as AstExpr, Item as AstItem, Stmt as AstStmt, Type as AstType},
        hir::{Expr as HirExpr, Item as HirItem, Stmt as HirStmt, Type as HirType, TypeId},
    },
    utils::HashMap,
};
use core::{
    cell::{Cell, RefCell},
    fmt::{Debug, Formatter, Result as FmtResult},
    mem,
};
use typed_arena::Arena;

// TODO: Node interning
// TODO: Arenas struct with `.with_arenas(|arenas| { .. })` method that keeps lifetimes sequestered and
//       automatically drops arenas at the end of the closure, also allow pre-allocation since we can reasonably
//       guess hir & mir tree sizes

pub struct Arenas<'ctx> {
    // AST Arenas
    ast_item: Arena<AstItem<'ctx>>,
    ast_stmt: Arena<AstStmt<'ctx>>,
    ast_expr: Arena<AstExpr<'ctx>>,
    ast_type: Arena<AstType<'ctx>>,

    // HIR Arenas
    hir_item: Arena<HirItem<'ctx>>,
    hir_stmt: Arena<HirStmt<'ctx>>,
    hir_expr: Arena<HirExpr<'ctx>>,
    hir_type: Arena<HirType>,
    // TODO: Maybe just use a vec for this
    hir_type_map: RefCell<HashMap<TypeId, &'ctx HirType>>,
    // TODO: Just an AtomicUsize for threading
    hir_type_id: Cell<usize>,
}

impl<'ctx> Arenas<'ctx> {
    #[inline]
    pub fn new() -> Self {
        Self {
            ast_item: Arena::new(),
            ast_stmt: Arena::new(),
            ast_expr: Arena::new(),
            ast_type: Arena::new(),
            hir_item: Arena::new(),
            hir_stmt: Arena::new(),
            hir_expr: Arena::new(),
            hir_type: Arena::new(),
            hir_type_map: RefCell::new(HashMap::new()),
            hir_type_id: Cell::new(0),
        }
    }

    #[inline]
    pub fn with_arenas<F, T>(&mut self, with: F) -> T
    where
        F: for<'arena> FnOnce(&'arena Arenas<'arena>) -> T,
    {
        let return_value = with(self);

        // Clear the arenas
        CRUNCHC_ALLOCATOR.record_region("deallocating arenas", || {
            mem::swap(&mut self.ast_item, &mut Arena::with_capacity(0));
            mem::swap(&mut self.ast_stmt, &mut Arena::with_capacity(0));
            mem::swap(&mut self.ast_expr, &mut Arena::with_capacity(0));
            mem::swap(&mut self.ast_type, &mut Arena::with_capacity(0));
            mem::swap(&mut self.hir_item, &mut Arena::with_capacity(0));
            mem::swap(&mut self.hir_stmt, &mut Arena::with_capacity(0));
            mem::swap(&mut self.hir_expr, &mut Arena::with_capacity(0));
            mem::swap(&mut self.hir_type, &mut Arena::with_capacity(0));
            self.hir_type_id.set(0);
            let mut type_map = self.hir_type_map.borrow_mut();
            type_map.clear();
            type_map.shrink_to_fit();
        });

        return_value
    }
}

impl<'ctx> Default for Arenas<'ctx> {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

impl Debug for Arenas<'_> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        f.debug_struct("Arenas").finish()
    }
}

#[derive(Clone)]
pub struct Context<'ctx> {
    arenas: &'ctx Arenas<'ctx>,
    // TODO: Pull strings out of refcells
    pub strings: StrInterner,
}

impl<'ctx> Context<'ctx> {
    #[inline]
    pub fn new(arenas: &'ctx Arenas<'ctx>) -> Self {
        Self {
            arenas,
            strings: StrInterner::new(),
        }
    }

    #[inline]
    pub const fn strings(&self) -> &StrInterner {
        &self.strings
    }

    // In regards to the following: Fuck you, you deal with this bullshit
    // TODO: Replace with queries?

    #[inline]
    pub fn ast_item(&self, item: AstItem<'ctx>) -> &'ctx AstItem<'ctx> {
        unsafe {
            mem::transmute::<&'_ AstItem<'ctx>, &'ctx AstItem<'ctx>>(
                self.arenas.ast_item.alloc(item),
            )
        }
    }

    #[inline]
    pub fn ast_stmt(&self, stmt: AstStmt<'ctx>) -> &'ctx AstStmt<'ctx> {
        unsafe {
            mem::transmute::<&'_ AstStmt<'ctx>, &'ctx AstStmt<'ctx>>(
                self.arenas.ast_stmt.alloc(stmt),
            )
        }
    }

    #[inline]
    pub fn ast_expr(&self, expr: AstExpr<'ctx>) -> &'ctx AstExpr<'ctx> {
        unsafe {
            mem::transmute::<&'_ AstExpr<'ctx>, &'ctx AstExpr<'ctx>>(
                self.arenas.ast_expr.alloc(expr),
            )
        }
    }

    #[inline]
    pub fn ast_type(&self, ty: AstType<'ctx>) -> &'ctx AstType<'ctx> {
        unsafe {
            mem::transmute::<&'_ AstType<'ctx>, &'ctx AstType<'ctx>>(self.arenas.ast_type.alloc(ty))
        }
    }

    #[inline]
    pub fn hir_item(&'ctx self, item: HirItem<'ctx>) -> &'ctx HirItem<'ctx> {
        unsafe {
            mem::transmute::<&'_ HirItem<'ctx>, &'ctx HirItem<'ctx>>(
                self.arenas.hir_item.alloc(item),
            )
        }
    }

    #[inline]
    pub fn hir_stmt(&self, stmt: HirStmt<'ctx>) -> &'ctx HirStmt<'ctx> {
        unsafe {
            mem::transmute::<&'_ HirStmt<'ctx>, &'ctx HirStmt<'ctx>>(
                self.arenas.hir_stmt.alloc(stmt),
            )
        }
    }

    #[inline]
    pub fn hir_expr(&self, expr: HirExpr<'ctx>) -> &'ctx HirExpr<'ctx> {
        unsafe {
            mem::transmute::<&'_ HirExpr<'ctx>, &'ctx HirExpr<'ctx>>(
                self.arenas.hir_expr.alloc(expr),
            )
        }
    }

    #[inline]
    pub fn hir_type(&self, ty: HirType) -> TypeId {
        let reference =
            unsafe { mem::transmute::<&'_ HirType, &'ctx HirType>(self.arenas.hir_type.alloc(ty)) };

        let current = self.arenas.hir_type_id.get();
        let id = TypeId::new(current);
        self.arenas.hir_type_id.set(current + 1);

        // FIXME: https://github.com/rust-lang/rust/issues/62633
        let prev_type = self.arenas.hir_type_map.borrow_mut().insert(id, reference);
        assert!(prev_type.is_none(), "A HIR type was double-inserted");

        id
    }

    #[inline]
    pub fn overwrite_hir_type(&self, target: TypeId, new: TypeId) {
        let new: &'ctx HirType = *self
            .arenas
            .hir_type_map
            .borrow()
            .get(&new)
            .expect("Attempted to get a type that does not exist");

        self.arenas
            .hir_type_map
            .borrow_mut()
            .insert(target, new)
            .expect("Context::overwrite_hir_type was called but no type was overwritten");
    }

    #[inline]
    pub fn get_hir_type(&self, id: TypeId) -> Option<&'ctx HirType> {
        self.arenas.hir_type_map.borrow().get(&id).map(|ty| *ty)
    }
}

impl Debug for Context<'_> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        f.debug_struct("Context").finish()
    }
}
