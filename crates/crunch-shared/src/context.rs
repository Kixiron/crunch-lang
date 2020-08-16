use crate::{
    allocator::CRUNCHC_ALLOCATOR,
    files::FileId,
    salsa,
    strings::StrInterner,
    trees::{
        ast::{Expr as AstExpr, Item as AstItem, Stmt as AstStmt, Type as AstType},
        hir::{Expr as HirExpr, Item as HirItem, Stmt as HirStmt, Type as HirType, TypeId},
    },
    utils::{HashMap, Hasher},
};
use alloc::sync::Arc;
use core::{
    cell::{Cell, RefCell},
    fmt::{Debug, Formatter, Result as FmtResult},
};
use std::sync::atomic::{AtomicU32, Ordering};
use typed_arena::Arena;

#[salsa::query_group(ContextDatabaseStorage)]
pub trait ContextDatabase: salsa::Database {
    // FIXME: Salsa won't allow bounded lifetimes, so we have to do this shit
    #[salsa::input]
    fn context(&self) -> &'static Context<'static>;

    // TODO: Add more arena queries once salsa allows lifetimes on dbs
    fn hir_type(&self, ty: HirType) -> TypeId;
}

fn hir_type(db: &dyn ContextDatabase, ty: HirType) -> TypeId {
    db.context().hir_type(ty)
}

// TODO: Node interning
// TODO: Arenas struct with `.with_arenas(|arenas| { .. })` method that keeps lifetimes sequestered and
//       automatically drops arenas at the end of the closure, also allow pre-allocation since we can reasonably
//       guess hir & mir tree sizes

pub struct OwnedArenas<'arena> {
    // AST Arenas
    pub ast_item: Arena<AstItem<'arena>>,
    pub ast_stmt: Arena<AstStmt<'arena>>,
    pub ast_expr: Arena<AstExpr<'arena>>,
    pub ast_type: Arena<AstType<'arena>>,

    // HIR Arenas
    pub hir_item: Arena<HirItem<'arena>>,
    pub hir_stmt: Arena<HirStmt<'arena>>,
    pub hir_expr: Arena<HirExpr<'arena>>,
    pub hir_type: Arena<HirType>,
    // TODO: Maybe just use a vec for this
    pub hir_type_map: RefCell<HashMap<TypeId, &'arena HirType>>,
    // TODO: Just an AtomicUsize for threading
    pub hir_type_id: Cell<usize>,
}

impl<'arena> OwnedArenas<'arena> {
    #[inline]
    pub fn new() -> Self {
        CRUNCHC_ALLOCATOR.record_region("init arenas", || Self {
            ast_item: Arena::new(),
            ast_stmt: Arena::new(),
            ast_expr: Arena::new(),
            ast_type: Arena::new(),
            hir_item: Arena::new(),
            hir_stmt: Arena::new(),
            hir_expr: Arena::new(),
            hir_type: Arena::new(),
            hir_type_map: RefCell::new(HashMap::with_capacity_and_hasher(1024, Hasher::default())),
            hir_type_id: Cell::new(0),
        })
    }
}

impl<'ctx> Default for OwnedArenas<'ctx> {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

impl Debug for OwnedArenas<'_> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        f.debug_struct("OwnedArenas").finish()
    }
}

#[derive(Clone)]
pub struct AstArena<'ar> {
    item: &'ar Arena<AstItem<'ar>>,
    stmt: &'ar Arena<AstStmt<'ar>>,
    expr: &'ar Arena<AstExpr<'ar>>,
    types: &'ar Arena<AstType<'ar>>,
}

impl<'ar> From<&'ar OwnedArenas<'ar>> for AstArena<'ar> {
    fn from(arenas: &'ar OwnedArenas<'ar>) -> Self {
        Self {
            item: &arenas.ast_item,
            stmt: &arenas.ast_stmt,
            expr: &arenas.ast_expr,
            types: &arenas.ast_type,
        }
    }
}

impl Debug for AstArena<'_> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        f.debug_struct("AstArena").finish()
    }
}

#[derive(Clone)]
pub struct HirArena<'ar> {
    item: &'ar Arena<HirItem<'ar>>,
    stmt: &'ar Arena<HirStmt<'ar>>,
    expr: &'ar Arena<HirExpr<'ar>>,
    types: &'ar Arena<HirType>,
    // TODO: Maybe just use a vec for this
    type_map: &'ar RefCell<HashMap<TypeId, &'ar HirType>>,
    // TODO: Just an AtomicUsize for threading
    type_id: &'ar Cell<usize>,
}

impl<'ar> From<&'ar OwnedArenas<'ar>> for HirArena<'ar> {
    fn from(arenas: &'ar OwnedArenas<'ar>) -> Self {
        Self {
            item: &arenas.hir_item,
            stmt: &arenas.hir_stmt,
            expr: &arenas.hir_expr,
            types: &arenas.hir_type,
            type_map: &arenas.hir_type_map,
            type_id: &arenas.hir_type_id,
        }
    }
}

impl Debug for HirArena<'_> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        f.debug_struct("HirArena").finish()
    }
}

#[derive(Debug, Clone)]
pub struct Arenas<'ar> {
    ast: AstArena<'ar>,
    hir: HirArena<'ar>,
}

impl<'ar> Arenas<'ar> {
    pub fn new(ast: AstArena<'ar>, hir: HirArena<'ar>) -> Self {
        Self { ast, hir }
    }
}

impl<'ar> From<&'ar OwnedArenas<'ar>> for Arenas<'ar> {
    fn from(owned: &'ar OwnedArenas<'ar>) -> Self {
        Self {
            ast: AstArena::from(owned),
            hir: HirArena::from(owned),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Context<'ctx> {
    arenas: Arenas<'ctx>,
    // TODO: Pull strings out of refcells
    strings: StrInterner,
    file_id: Arc<AtomicU32>,
}

impl<'ctx> Context<'ctx> {
    #[inline]
    pub fn new(arenas: Arenas<'ctx>) -> Self {
        Self {
            arenas,
            strings: Self::construct_string_interner(),
            file_id: Arc::new(AtomicU32::new(0)),
        }
    }

    /// Preloads the interner with frequently used static strings
    fn construct_string_interner() -> StrInterner {
        macro_rules! intern_static {
            (($strings:ident) => { $($string:literal),* $(,)? }) => {
                $(
                    $strings.intern_static($string);
                )*
            };
        }

        let strings = StrInterner::new();
        intern_static!((strings) => {
            "callconv",
            "main",
            "suspend",
        });

        strings
    }

    #[inline]
    pub const fn strings(&self) -> &StrInterner {
        &self.strings
    }

    pub fn next_file_id(&self) -> FileId {
        FileId::new(self.file_id.fetch_add(1, Ordering::Relaxed))
    }

    // In regards to the following: Fuck you, you deal with this bullshit
    // TODO: Replace with queries?

    #[inline]
    pub fn ast_item(&self, item: AstItem<'ctx>) -> &'ctx AstItem<'ctx> {
        self.arenas.ast.item.alloc(item)
    }

    #[inline]
    pub fn ast_stmt(&self, stmt: AstStmt<'ctx>) -> &'ctx AstStmt<'ctx> {
        self.arenas.ast.stmt.alloc(stmt)
    }

    #[inline]
    pub fn ast_expr(&self, expr: AstExpr<'ctx>) -> &'ctx AstExpr<'ctx> {
        self.arenas.ast.expr.alloc(expr)
    }

    #[inline]
    pub fn ast_type(&self, ty: AstType<'ctx>) -> &'ctx AstType<'ctx> {
        self.arenas.ast.types.alloc(ty)
    }

    #[inline]
    pub fn hir_item(&'ctx self, item: HirItem<'ctx>) -> &'ctx HirItem<'ctx> {
        self.arenas.hir.item.alloc(item)
    }

    #[inline]
    pub fn hir_stmt(&self, stmt: HirStmt<'ctx>) -> &'ctx HirStmt<'ctx> {
        self.arenas.hir.stmt.alloc(stmt)
    }

    #[inline]
    pub fn hir_expr(&self, expr: HirExpr<'ctx>) -> &'ctx HirExpr<'ctx> {
        self.arenas.hir.expr.alloc(expr)
    }

    #[inline]
    pub fn hir_type(&self, ty: HirType) -> TypeId {
        let reference = self.arenas.hir.types.alloc(ty);

        let current = self.arenas.hir.type_id.get();
        let id = TypeId::new(current);
        self.arenas.hir.type_id.set(current + 1);

        // FIXME: https://github.com/rust-lang/rust/issues/62633
        let prev_type = self.arenas.hir.type_map.borrow_mut().insert(id, reference);
        assert!(prev_type.is_none(), "A HIR type was double-inserted");

        id
    }

    #[inline]
    pub fn overwrite_hir_type(&self, target: TypeId, new: TypeId) {
        let new: &'ctx HirType = *self
            .arenas
            .hir
            .type_map
            .borrow()
            .get(&new)
            .expect("Attempted to get a type that does not exist");

        self.arenas
            .hir
            .type_map
            .borrow_mut()
            .insert(target, new)
            .expect("Context::overwrite_hir_type was called but no type was overwritten");
    }

    #[inline]
    pub fn get_hir_type(&self, id: TypeId) -> Option<&'ctx HirType> {
        self.arenas.hir.type_map.borrow().get(&id).copied()
    }
}
