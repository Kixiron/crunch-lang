use codespan::{FileId, Span};
use std::{borrow::Cow, fmt};

#[derive(Clone)]
pub enum Node<'a> {
    Func(Func<'a>),
}

impl<'a> fmt::Debug for Node<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Func(func) => write!(f, "{:#?}", func),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Type<'a> {
    Int,
    Float,
    String,
    Bool,
    Void,
    Infer,
    Custom(Ident<'a>),
}

#[derive(Clone)]
pub struct LocInfo {
    pub span: Span,
    pub file: FileId,
}

impl fmt::Display for LocInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "LocInfo {{ file: {}, span: {}..{} }}",
            format!("{:?}", self.file)
                .replace("FileId", "")
                .replace("(", "")
                .replace(")", ""),
            self.span.start(),
            self.span.end()
        )
    }
}

impl fmt::Debug for LocInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "LocInfo {{ file: {}, span: {}..{} }}",
            format!("{:?}", self.file)
                .replace("FileId", "")
                .replace("(", "")
                .replace(")", ""),
            self.span.start(),
            self.span.end()
        )
    }
}

#[derive(Debug, Clone)]
pub struct Ident<'a> {
    pub name: Cow<'a, str>,
    pub info: LocInfo,
}

impl<'a> Ident<'a> {
    pub fn from_token(token: super::Token<'a>, file: FileId) -> Self {
        Self {
            name: token.source,
            info: LocInfo {
                span: Span::new(token.range.0, token.range.1),
                file,
            },
        }
    }
}

#[derive(Debug, Clone)]
pub struct Func<'a> {
    pub name: Ident<'a>,
    pub params: Vec<FuncParam<'a>>,
    pub returns: Type<'a>,
    pub body: Vec<FuncBody<'a>>,
    pub info: LocInfo,
}

#[derive(Debug, Clone)]
pub struct FuncParam<'a> {
    pub name: Ident<'a>,
    pub ty: Type<'a>,
    pub info: LocInfo,
}

#[derive(Debug, Clone)]
pub struct FuncBody<'a> {
    pub expr: FuncExpr<'a>,
    pub info: LocInfo,
}

#[derive(Clone)]
pub enum FuncExpr<'a> {
    Binding(Binding<'a>),
    FuncCall(FuncCall<'a>),
    Assign(Assign<'a>),
    Builtin(Builtin<'a>),
}

impl<'a> fmt::Debug for FuncExpr<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Binding(b) => write!(f, "{:#?}", b),
            Self::FuncCall(c) => write!(f, "{:#?}", c),
            Self::Assign(a) => write!(f, "{:#?}", a),
            Self::Builtin(b) => write!(f, "{:#?}", b),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Builtin<'a> {
    Print(Vec<IdentLiteral<'a>>),
    Collect,
    Halt,
}

#[derive(Debug, Clone)]
pub struct Binding<'a> {
    pub name: Ident<'a>,
    pub val: BindingVal<'a>,
    pub ty: Type<'a>,
    pub info: LocInfo,
}

#[derive(Clone)]
pub enum BindingVal<'a> {
    Literal(Literal<'a>),
    Variable(Ident<'a>),
    BinOp(BinOp<'a>),
    FuncCall(FuncCall<'a>),
}

impl<'a> fmt::Debug for BindingVal<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Variable(i) => write!(f, "{:#?}", i),
            Self::Literal(l) => write!(f, "{:#?}", l),
            Self::BinOp(b) => write!(f, "{:#?}", b),
            Self::FuncCall(fu) => write!(f, "{:#?}", fu),
        }
    }
}

#[derive(Debug, Clone)]
pub struct BinOp<'a> {
    pub op: Op,
    pub left: BinOpSide<'a>,
    pub right: BinOpSide<'a>,
    pub info: LocInfo,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Op {
    Add,
    Sub,
}

#[derive(Clone)]
pub enum BinOpSide<'a> {
    Literal(Literal<'a>),
    Variable(Ident<'a>),
}

impl<'a> fmt::Debug for BinOpSide<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Variable(i) => write!(f, "{:#?}", i),
            Self::Literal(l) => write!(f, "{:#?}", l),
        }
    }
}

#[derive(Debug, Clone)]
pub struct FuncCall<'a> {
    pub func_name: Ident<'a>,
    pub params: Vec<IdentLiteral<'a>>,
    pub info: LocInfo,
}

#[derive(Clone)]
pub enum IdentLiteral<'a> {
    Variable(Ident<'a>),
    Literal(Literal<'a>),
}

impl<'a> fmt::Debug for IdentLiteral<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Variable(i) => write!(f, "{:#?}", i),
            Self::Literal(l) => write!(f, "{:#?}", l),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Assign<'a> {
    pub name: Ident<'a>,
    pub val: IdentLiteral<'a>,
    pub ty: Type<'a>,
    pub info: LocInfo,
}

#[derive(Debug, Clone)]
pub struct Literal<'a> {
    pub val: LiteralInner<'a>,
    pub info: LocInfo,
}

#[derive(Clone)]
pub enum LiteralInner<'a> {
    String(Cow<'a, str>),
    Int(i32),
    Float(f32),
    Bool(bool),
}

impl<'a> fmt::Debug for LiteralInner<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::String(s) => write!(f, "String({:?})", &s[1..s.len() - 1]),
            Self::Int(i) => write!(f, "Int({:?})", i),
            Self::Float(fl) => write!(f, "Float({:?})", fl),
            Self::Bool(b) => write!(f, "Bool({:?})", b),
        }
    }
}
