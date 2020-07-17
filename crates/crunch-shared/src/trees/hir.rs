use crate::{
    error::{Locatable, Location, Span},
    strings::{StrInterner, StrT},
    trees::{CallConv, Ref, Sided},
};
#[cfg(feature = "no-std")]
use alloc::{
    borrow::ToOwned,
    string::{String, ToString},
    vec::Vec,
};
use core::fmt::Debug;
use serde::{Deserialize, Serialize};

// TODO: Make equivalents of everything in HIR, even though it's duplicated code
pub use crate::trees::{
    ast::{
        BinaryOp, CompOp, Float, Integer, Literal as AstLiteral, LiteralVal as AstLiteralVal, Rune,
        Text, Type as AstType, Vis,
    },
    ItemPath, Signedness,
};

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub enum Item {
    Function(Function),
    ExternFunc(ExternFunc),
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct Function {
    pub name: ItemPath,
    pub vis: Vis,
    pub args: Locatable<Vec<FuncArg>>,
    pub body: Block<Stmt>,
    pub ret: Type,
    pub loc: Location,
    pub sig: Location,
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct FuncArg {
    pub name: Var,
    pub kind: Type,
    pub loc: Location,
}

impl FuncArg {
    pub const fn location(&self) -> Location {
        self.loc
    }
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct ExternFunc {
    pub name: ItemPath,
    pub vis: Vis,
    pub args: Locatable<Vec<FuncArg>>,
    pub ret: Type,
    pub callconv: CallConv,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub enum Stmt {
    Item(Item),
    Expr(Expr),
    VarDecl(VarDecl),
}

impl From<Item> for Stmt {
    #[inline]
    fn from(item: Item) -> Self {
        Self::Item(item)
    }
}

impl From<Expr> for Stmt {
    #[inline]
    fn from(expr: Expr) -> Self {
        Self::Expr(expr)
    }
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct Expr {
    pub kind: ExprKind,
    pub loc: Location,
}

impl Expr {
    #[inline]
    pub const fn location(&self) -> Location {
        self.loc
    }
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub enum ExprKind {
    Match(Match),
    Scope(Block<Stmt>),
    Loop(Block<Stmt>),
    Return(Return),
    Continue,
    Break(Break),
    FnCall(FuncCall),
    Literal(Literal),
    Comparison(Sided<CompOp, Ref<Expr>>),
    Variable(Var, Type),
    Assign(Var, Ref<Expr>),
    BinOp(Sided<BinaryOp, Ref<Expr>>),
    Cast(Cast),
    Reference(Reference),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Deserialize, Serialize)]
pub enum Var {
    User(StrT),
    Auto(usize),
}

impl Var {
    pub fn to_string(&self, interner: &StrInterner) -> String {
        match *self {
            Self::User(var) => interner.resolve(var).as_ref().to_owned(),
            Self::Auto(var) => var.to_string(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct VarDecl {
    pub name: Var,
    pub value: Ref<Expr>,
    pub mutable: bool,
    pub ty: Type,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct FuncCall {
    pub func: ItemPath,
    pub args: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct Match {
    pub cond: Ref<Expr>,
    pub arms: Vec<MatchArm>,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct MatchArm {
    pub bind: Binding,
    pub guard: Option<Ref<Expr>>,
    pub body: Block<Stmt>,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct Binding {
    // TODO: Enum for mutability/referential status?
    pub reference: bool,
    pub mutable: bool,
    pub pattern: Pattern,
    pub ty: Option<Ref<Type>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Deserialize, Serialize)]
pub enum Pattern {
    Literal(Literal),
    Ident(StrT),
    ItemPath(ItemPath),
    Wildcard,
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct Return {
    pub val: Option<Ref<Expr>>,
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct Break {
    pub val: Option<Ref<Expr>>,
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct Block<T> {
    pub block: Vec<T>,
    pub loc: Location,
}

impl<T> Block<T> {
    #[inline]
    pub fn new(loc: Location) -> Self {
        Self {
            block: Vec::new(),
            loc,
        }
    }

    #[inline]
    pub fn with_capacity(loc: Location, capacity: usize) -> Self {
        Self {
            block: Vec::with_capacity(capacity),
            loc,
        }
    }

    #[inline]
    pub fn push(&mut self, item: T) {
        self.block.push(item);
    }

    #[inline]
    pub fn insert(&mut self, idx: usize, item: T) {
        self.block.insert(idx, item);
    }

    #[inline]
    pub fn location(&self) -> Location {
        self.loc
    }

    #[inline]
    pub fn span(&self) -> Span {
        self.loc.span()
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.block.len()
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.block.is_empty()
    }

    #[inline]
    pub fn iter<'a>(&'a self) -> impl Iterator<Item = &'a T> + 'a {
        self.block.iter()
    }

    #[inline]
    pub fn iter_mut<'a>(&'a mut self) -> impl Iterator<Item = &'a mut T> + 'a {
        self.block.iter_mut()
    }

    #[inline]
    pub fn from_iter<I: IntoIterator<Item = T>>(loc: Location, iter: I) -> Self {
        let mut block = Vec::with_capacity(10);
        for item in iter {
            block.push(item);
        }

        Self { block, loc }
    }
}

impl<T> Block<T>
where
    T: Clone,
{
    #[inline]
    pub fn extend_from_slice<S>(&mut self, slice: S)
    where
        S: AsRef<[T]>,
    {
        self.block.extend_from_slice(slice.as_ref())
    }
}

impl<T> Extend<T> for Block<T> {
    fn extend<I: IntoIterator<Item = T>>(&mut self, iter: I) {
        self.block.extend(iter)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Deserialize, Serialize)]
pub struct Type {
    pub kind: TypeKind,
    pub loc: Location,
}

impl Type {
    pub const fn location(&self) -> Location {
        self.loc
    }

    pub fn is_unknown(&self) -> bool {
        self.kind.is_unknown()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Deserialize, Serialize)]
pub enum TypeKind {
    Unknown,
    Integer {
        signed: Option<bool>,
        width: Option<u16>,
    },
    String,
    Bool,
    Unit,
    Absurd,
    Pointer {
        pointee: Ref<Type>,
        mutable: bool,
    },
    Array {
        element: Ref<Type>,
        length: u64,
    },
    Slice {
        element: Ref<Type>,
    },
    Reference {
        referee: Ref<Type>,
        mutable: bool,
    },
}

impl TypeKind {
    pub fn is_unknown(&self) -> bool {
        matches!(self, Self::Unknown)
    }
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct Cast {
    pub casted: Ref<Expr>,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct Reference {
    pub mutable: bool,
    pub reference: Ref<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Deserialize, Serialize)]
pub struct Literal {
    pub val: LiteralVal,
    pub ty: Type,
    pub loc: Location,
}

impl Literal {
    pub fn location(&self) -> Location {
        self.loc
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Deserialize, Serialize)]
pub enum LiteralVal {
    Integer(Integer),
    Bool(bool),
    String(Text),
    Rune(Rune),
    Float(Float),
    Array { elements: Vec<Literal> },
    // TODO: Tuples, slices, others?
}
