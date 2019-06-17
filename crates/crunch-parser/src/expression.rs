use std::ops::Range;

#[derive(Debug)]
pub enum Expr {
    Variable {
        ident: String,
        kind: Box<Expr>,
        literal: Box<Expr>,
    },
    Literal(Literal),
    VariableType(LiteralKind),

    BinaryOp {
        operation: Op,
        left_hand: Box<Expr>,
        right_hand: Box<Expr>,
    },

    Scope(Box<Expr>),
    Method {
        name: String,
        parameters: Vec<Expr>,
        body: Box<Expr>,
    },

    For {
        item: String,
        collection: Box<Expr>,
        body: Box<Expr>,
    },
    While(Box<Expr>, Box<Expr>),
    Loop(Box<Expr>),

    If {
        condition: Box<Expr>,
        body: Box<Expr>,
        continuation: Option<Box<Expr>>,
    },
    ElseIf {
        condition: Box<Expr>,
        body: Box<Expr>,
        continuation: Option<Box<Expr>>,
    },
    Else {
        body: Box<Expr>,
    },

    Invalid(String, Range<usize>),
    Error(Vec<(codespan_reporting::Diagnostic, Option<Range<usize>>)>),
    EndOfFile,
    None,
}

impl PartialEq for Expr {
    fn eq(&self, other: &Self) -> bool {
        std::mem::discriminant(self) == std::mem::discriminant(other)
    }

    fn ne(&self, other: &Self) -> bool {
        std::mem::discriminant(self) != std::mem::discriminant(other)
    }
}

#[derive(Debug, PartialEq)]
pub struct Literal {
    pub kind: LiteralKind,
    pub value: LiteralValue,
}

#[derive(Debug, PartialEq)]
pub enum LiteralValue {
    String(String),
    Float(FloatType),
    Int(IntType),
    Bool(bool),
    Vector(Vec<Literal>),
    Null,
}

#[derive(Debug, PartialEq)]
pub enum LiteralKind {
    String,
    Float,
    Int,
    Vector,
    Bool,
}

#[derive(Debug, PartialEq)]
pub enum Op {
    Add,
    Subtract,
    Multiply,
    Divide,
    And,
    Or,
    Not,
}

#[allow(non_camel_case_types)]
#[derive(Debug, PartialEq)]
pub enum IntType {
    _i32(i32),
    _i64(i64),
    _i128(i128),
    _u32(u32, Sign),
    _u64(u64, Sign),
    _u128(u128, Sign),
}

#[allow(non_camel_case_types)]
#[derive(Debug, PartialEq)]
pub enum FloatType {
    _f32(f32),
    _f64(f64),
}

#[derive(Debug, PartialEq)]
pub enum Sign {
    Positive,
    Negative,
}
