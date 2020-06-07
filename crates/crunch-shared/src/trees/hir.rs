use core::{
    fmt::{Debug, Display, Formatter, Result},
    ops::Deref,
};
use serde::{Deserialize, Serialize};

// TODO: Make equivalents of everything in HIR, even though it's duplicated code
pub use super::ast::{CompOp, ItemPath, Literal, Vis};

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub enum Hir {
    Function(Function),
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct Function {
    pub name: ItemPath,
    pub visibility: Vis,
    pub args: Vec<FuncArg>,
    pub body: Block<Stmt>,
    pub ret: TypeKind,
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct FuncArg {
    pub name: ItemPath,
    pub kind: TypeKind,
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub enum Stmt {
    Hir(Hir),
    Expr(Expr),
    VarDecl(VarDecl),
}

impl From<Hir> for Stmt {
    #[inline]
    fn from(hir: Hir) -> Self {
        Self::Hir(hir)
    }
}

impl From<Expr> for Stmt {
    #[inline]
    fn from(expr: Expr) -> Self {
        Self::Expr(expr)
    }
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub enum Expr {
    Match(Match),
    Scope(Block<Stmt>),
    Loop(Block<Stmt>),
    Return(Return),
    Continue,
    Break(Break),
    FnCall(FuncCall),
    Literal(Literal),
    Var(ItemPath),
    Comparison(Ref<Expr>, CompOp, Ref<Expr>),
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct VarDecl {
    pub name: ItemPath,
    pub value: Ref<Expr>,
    pub ty: Type,
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
    pub ty: TypeKind,
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct MatchArm {
    pub cond: Ref<Expr>,
    pub body: Block<Stmt>,
    pub ty: TypeKind,
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
}

impl<T> Block<T> {
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
}

impl<T> core::iter::FromIterator<T> for Block<T> {
    #[inline]
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let mut block = Vec::with_capacity(10);
        for item in iter {
            block.push(item);
        }

        Self { block }
    }
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct Type {
    pub name: ItemPath,
    pub kind: TypeKind,
}

#[allow(missing_copy_implementations)] // This eventually won't be copy
#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub enum TypeKind {
    Infer,
    Integer,
    String,
    Bool,
    Unit,
}

impl From<&crate::trees::ast::Type> for TypeKind {
    fn from(ty: &crate::trees::ast::Type) -> Self {
        use crate::trees::ast::{Signedness, Type};

        match ty {
            Type::Infer => Self::Infer,
            Type::Unit => Self::Unit,
            Type::Bool => Self::Bool,
            Type::String => Self::String,
            Type::Integer {
                sign: Signedness::Signed,
                width: 32,
            } => Self::Integer,

            _ => todo!(),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Deserialize, Serialize)]
pub struct Ref<T>(Box<T>);

impl<T> Ref<T> {
    #[inline]
    pub fn new(val: T) -> Self {
        Self(Box::new(val))
    }
}

impl<T> AsRef<T> for Ref<T> {
    #[inline]
    fn as_ref(&self) -> &T {
        &*self.0
    }
}

impl<T> Deref for Ref<T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}

impl<T: Debug> Debug for Ref<T> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        Debug::fmt(&*self.0, f)
    }
}

impl<T: Display> Display for Ref<T> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        Display::fmt(&*self.0, f)
    }
}
