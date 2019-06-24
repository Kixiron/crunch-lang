<<<<<<< HEAD
use crunch_error::EmittedError;
use std::ops::Range;
use typed_arena::Arena;

#[derive(Debug)]
pub enum Expr {
    Variable {
        ident: String,
        kind: Arena<Expr>,
        literal: Arena<Expr>,
    },
    Literal(Literal),
    VariableType(LiteralKind),

    BinaryOp {
        operation: Op,
        left_hand: Arena<Expr>,
        right_hand: Arena<Expr>,
    },

    Scope(Arena<Expr>),
    Method {
        name: String,
        parameters: Vec<Expr>,
        body: Arena<Expr>,
    },

    For {
        item: String,
        collection: Arena<Expr>,
        body: Arena<Expr>,
    },
    While(Arena<Expr>, Arena<Expr>),
    Loop(Arena<Expr>),

    If {
        condition: Arena<Expr>,
        body: Arena<Expr>,
        continuation: Option<Arena<Expr>>,
    },
    ElseIf {
        condition: Arena<Expr>,
        body: Arena<Expr>,
        continuation: Option<Arena<Expr>>,
    },
    Else {
        body: Arena<Expr>,
    },

    Invalid(String, Range<usize>),
    Error(Vec<EmittedError>),
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

#[derive(Debug, PartialEq, Clone)]
pub struct Literal {
    pub kind: LiteralKind,
    pub value: LiteralValue,
}

#[derive(Debug, Clone)]
pub enum LiteralValue {
    String(String),
    Float(FloatType),
    Int(IntType),
    Bool(bool),
    Vector(Vec<Literal>),
    Null,
    Error(Vec<EmittedError>),
}

impl PartialEq for LiteralValue {
    fn eq(&self, other: &Self) -> bool {
        std::mem::discriminant(self) == std::mem::discriminant(other)
    }

    fn ne(&self, other: &Self) -> bool {
        std::mem::discriminant(self) != std::mem::discriminant(other)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum LiteralKind {
    String,
    Float,
    Int,
    Vector,
    Bool,
}

#[derive(Debug, PartialEq, Clone)]
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
#[derive(Debug, PartialEq, Clone)]
pub enum IntType {
    _i32(i32),
    _i64(i64),
    _i128(i128),
    _u32(u32, Sign),
    _u64(u64, Sign),
    _u128(u128, Sign),
}

#[allow(non_camel_case_types)]
#[derive(Debug, PartialEq, Clone)]
pub enum FloatType {
    _f32(f32),
    _f64(f64),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Sign {
    Positive,
    Negative,
}
=======
use crunch_error::EmittedError;
use std::ops::Range;
use typed_arena::Arena;

#[derive(Debug)]
pub enum Expr {
    Variable {
        ident: String,
        kind: Arena<Expr>,
        literal: Arena<Expr>,
    },
    Literal(Literal),
    VariableType(LiteralKind),

    BinaryOp {
        operation: Op,
        left_hand: Arena<Expr>,
        right_hand: Arena<Expr>,
    },

    Scope(Arena<Expr>),
    Method {
        name: String,
        parameters: Vec<Expr>,
        body: Arena<Expr>,
    },

    For {
        item: String,
        collection: Arena<Expr>,
        body: Arena<Expr>,
    },
    While(Arena<Expr>, Arena<Expr>),
    Loop(Arena<Expr>),

    If {
        condition: Arena<Expr>,
        body: Arena<Expr>,
        continuation: Option<Arena<Expr>>,
    },
    ElseIf {
        condition: Arena<Expr>,
        body: Arena<Expr>,
        continuation: Option<Arena<Expr>>,
    },
    Else {
        body: Arena<Expr>,
    },

    Invalid(String, Range<usize>),
    Error(Vec<EmittedError>),
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

#[derive(Debug, PartialEq, Clone)]
pub struct Literal {
    pub kind: LiteralKind,
    pub value: LiteralValue,
}

#[derive(Debug, Clone)]
pub enum LiteralValue {
    String(String),
    Float(FloatType),
    Int(IntType),
    Bool(bool),
    Vector(Vec<Literal>),
    Null,
    Error(Vec<EmittedError>),
}

impl PartialEq for LiteralValue {
    fn eq(&self, other: &Self) -> bool {
        std::mem::discriminant(self) == std::mem::discriminant(other)
    }

    fn ne(&self, other: &Self) -> bool {
        std::mem::discriminant(self) != std::mem::discriminant(other)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum LiteralKind {
    String,
    Float,
    Int,
    Vector,
    Bool,
}

#[derive(Debug, PartialEq, Clone)]
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
#[derive(Debug, PartialEq, Clone)]
pub enum IntType {
    _i32(i32),
    _i64(i64),
    _i128(i128),
    _u32(u32, Sign),
    _u64(u64, Sign),
    _u128(u128, Sign),
}

#[allow(non_camel_case_types)]
#[derive(Debug, PartialEq, Clone)]
pub enum FloatType {
    _f32(f32),
    _f64(f64),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Sign {
    Positive,
    Negative,
}
>>>>>>> Refractored, fixed and tested int parser
