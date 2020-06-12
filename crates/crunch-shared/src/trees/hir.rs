use crate::{
    error::{Location, Span},
    strings::StrT,
    trees::{Ref, Sided},
};
use core::fmt::Debug;
use serde::{Deserialize, Serialize};

// TODO: Make equivalents of everything in HIR, even though it's duplicated code
pub use super::ast::{CompOp, ItemPath, Literal, Vis};

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub enum Item {
    Function(Function),
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct Function {
    pub name: ItemPath,
    pub vis: Vis,
    pub args: Vec<FuncArg>,
    pub body: Block<Stmt>,
    pub ret: TypeKind,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct FuncArg {
    pub name: ItemPath,
    pub kind: TypeKind,
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
    Variable(StrT, TypeKind),
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct VarDecl {
    pub name: ItemPath,
    pub value: Ref<Expr>,
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
    pub loc: Location,
}

impl<T> Block<T> {
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
