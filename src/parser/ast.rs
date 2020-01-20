use std::path::PathBuf;
use string_interner::Sym;

#[derive(Debug, Clone)]
pub enum Either<L, R> {
    Left(L),
    Right(R),
}

#[derive(Debug, Clone)]
pub enum Program {
    FunctionDecl(FunctionDecl),
    TypeDecl(TypeDecl),
    Import(Import),
}

#[derive(Debug, Clone)]
pub struct FunctionDecl {
    pub visibility: Visibility,
    pub name: Sym,
    pub generics: Vec<Type>,
    pub arguments: Vec<(Sym, Type)>,
    pub returns: Type,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct TypeDecl {
    pub visibility: Visibility,
    pub name: Sym,
    pub generics: Vec<Type>,
    pub members: Vec<(Visibility, Sym, Type)>,
    pub methods: Vec<FunctionDecl>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Int,
    Float,
    String,
    Bool,
    Unit,
    Infer,
    Any,
    Custom(Sym),
}

impl Default for Type {
    fn default() -> Self {
        Self::Unit
    }
}

#[derive(Debug, Clone)]
pub struct Import {
    pub source: ImportSource,
    pub alias: Option<Sym>,
    pub exposes: Exposes,
}

#[derive(Debug, Clone)]
pub enum ImportSource {
    File(PathBuf),
    Package(Sym),
    Native(Sym),
}

#[derive(Debug, Clone)]
pub enum Exposes {
    All,
    File,
    Some(Vec<(Sym, Option<Sym>)>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Visibility {
    Library,
    Exposed,
    File,
}

impl Default for Visibility {
    fn default() -> Self {
        Self::File
    }
}

#[derive(Debug, Clone)]
pub struct VarDecl {
    pub name: Sym,
    pub ty: Type,
    pub expr: Expr,
}

#[derive(Debug, Clone)]
pub struct Assign {
    pub var: Sym,
    pub expr: Expr,
    pub ty: AssignType,
}

#[derive(Debug, Clone, Copy)]
pub enum AssignType {
    Normal,
    BinaryOp(BinaryOp),
}

#[derive(Debug, Clone)]
pub struct FunctionCall {
    pub name: Sym,
    pub generics: Vec<Type>,
    pub arguments: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct Conditional {
    pub _if: Vec<If>,
    pub _else: Option<Else>,
}

#[derive(Debug, Clone)]
pub struct If {
    pub condition: Expr,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct Else {
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct While {
    pub condition: Expr,
    pub body: Vec<Statement>,
    pub then: Option<Else>,
}

#[derive(Debug, Clone)]
pub struct Loop {
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct For {
    pub element: Sym,
    pub range: Expr,
    pub body: Vec<Statement>,
    pub then: Option<Else>,
}

#[derive(Debug, Clone)]
pub struct BinaryOperation {
    pub left: Box<Expr>,
    pub op: (BinaryOp, OperandType),
    pub right: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct Comparison {
    pub left: Box<Expr>,
    pub comparison: Comparator,
    pub right: Box<Expr>,
}

#[derive(Debug, Clone, Copy)]
pub enum OperandType {
    Normal,
    Fallible,
    Panicking,
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOp {
    Plus,
    Minus,
    Mult,
    Div,
    Xor,
    Or,
    And,
}

#[derive(Debug, Clone, Copy)]
pub enum Comparator {
    Equal,
    NotEqual,
    LessEqual,
    GreaterEqual,
    Less,
    Greater,
}

#[derive(Debug, Clone, Copy)]
pub enum Literal {
    String(Sym),
    Integer(i32),
    Boolean(bool),
}

#[derive(Debug, Clone)]
pub struct Range {
    start: Box<Expr>,
    end: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct Return {
    pub expr: Expr,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Literal(Literal),
    Range(Range),
    Comparison(Comparison),
    BinaryOperation(BinaryOperation),
    FunctionCall(FunctionCall),
    Ident(Sym),
    Expr(Box<Expr>),
}

#[derive(Debug, Clone)]
pub enum Statement {
    Conditional(Conditional),
    While(While),
    Loop(Loop),
    For(For),
    Assign(Assign),
    VarDecl(VarDecl),
    Return(Return),
    Continue,
    Break,
    Expr(Expr),
    Empty,
}
