use super::TokenType;
use crate::RuntimeValue;
use codespan::{FileId, Span};
use std::fmt;

#[derive(Clone)]
pub enum Node<'a> {
    Func(Func<'a>),
    Import(Import<'a>),
}

impl<'a> fmt::Debug for Node<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Func(func) => write!(f, "{:#?}", func),
            Self::Import(import) => write!(f, "{:#?}", import),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type<'a> {
    Int,
    Float,
    String,
    Bool,
    Void,
    Infer,
    Custom(Ident<'a>),
}

impl<'a> fmt::Display for Type<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let string = match self {
            Self::Int => "int",
            Self::Float => "float",
            Self::String => "str",
            Self::Bool => "bool",
            Self::Void => "void",
            Self::Infer => "infer",
            Self::Custom(ident) => &*ident.name,
        };

        write!(f, "{}", string)
    }
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
    pub name: &'a str,
    pub info: LocInfo,
}

impl<'a> PartialEq for Ident<'a> {
    fn eq(&self, other: &Self) -> bool {
        *self.name == *other.name
    }
}

impl<'a> Eq for Ident<'a> {}

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

// #[derive(Debug, Clone)]
// pub struct TypeDecl<'a> {
//     pub name: Ident<'a>,
//     pub members: Vec<TypeMember<'a>>,
//     pub methods: Vec<TypeMethod<'a>>,
// }
//
// #[derive(Debug, Clone)]
// pub enum TypeMemberOrMethod<'a> {
//     Member(TypeMember<'a>),
//     Method(TypeMethod<'a>),
// }

#[derive(Debug, Clone)]
pub struct Import<'a> {
    pub file: std::path::PathBuf,
    pub alias: Option<Ident<'a>>,
    pub exposes: Exposes<'a>,
    pub ty: ImportType,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum ImportType {
    File,
    Package,
    Library,
}

#[derive(Debug, Clone)]
pub enum Exposes<'a> {
    All,
    File,
    Some(Vec<(Ident<'a>, Option<Ident<'a>>)>),
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
    NoOp,
}

impl<'a> fmt::Debug for FuncExpr<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Binding(b) => write!(f, "{:#?}", b),
            Self::FuncCall(c) => write!(f, "{:#?}", c),
            Self::Assign(a) => write!(f, "{:#?}", a),
            Self::Builtin(b) => write!(f, "{:#?}", b),
            Self::NoOp => write!(f, "NoOp"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Builtin<'a> {
    Print(Vec<IdentLiteral<'a>>),
    SyscallExit(IdentLiteral<'a>),
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
    Literal(Literal),
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
    Div,
    Mult,
}

impl std::convert::TryFrom<TokenType> for Op {
    type Error = TokenType;

    fn try_from(token: TokenType) -> Result<Self, Self::Error> {
        Ok(match token {
            TokenType::Plus => Self::Add,
            TokenType::Minus => Self::Sub,
            TokenType::Divide => Self::Div,
            TokenType::Star => Self::Mult,
            token => return Err(token),
        })
    }
}

#[derive(Clone)]
pub enum BinOpSide<'a> {
    Literal(Literal),
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
    Literal(Literal),
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
    pub info: LocInfo,
}

#[derive(Debug, Clone)]
pub struct Literal {
    pub val: LiteralInner,
    pub info: LocInfo,
}

#[derive(Clone)]
pub enum LiteralInner {
    String(String),
    Int(i32),
    Float(f32),
    Bool(bool),
}

impl<'a> LiteralInner {
    pub fn to_type(&self) -> Type<'a> {
        match self {
            Self::String(_) => Type::String,
            Self::Int(_) => Type::Int,
            Self::Float(_) => Type::Float,
            Self::Bool(_) => Type::Bool,
        }
    }
}

impl<'a> Into<RuntimeValue> for LiteralInner {
    fn into(self) -> RuntimeValue {
        match self {
            Self::String(s) => RuntimeValue::Str(Box::leak(s.to_string().into_boxed_str())),
            Self::Int(i) => RuntimeValue::I32(i),
            Self::Float(f) => RuntimeValue::F32(f),
            Self::Bool(b) => RuntimeValue::Bool(b),
        }
    }
}

impl<'a> fmt::Debug for LiteralInner {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::String(s) => write!(f, "String({:?})", s),
            Self::Int(i) => write!(f, "Int({:?})", i),
            Self::Float(fl) => write!(f, "Float({:?})", fl),
            Self::Bool(b) => write!(f, "Bool({:?})", b),
        }
    }
}
