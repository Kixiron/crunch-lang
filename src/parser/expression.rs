use std::ops::Range;

#[derive(Debug, PartialEq)]
pub enum Expression {
    IntLiteral(IntType),
    FloatLiteral(FloatType),
    StringLiteral(String),
    Vector(Box<Vec<Expression>>),
    True,
    False,
    Null,

    Bool,
    Vec,
    Str,
    Int,

    And(Box<Expression>, Box<Expression>),
    Or(Box<Expression>, Box<Expression>),

    Multiplication(Box<Expression>, Box<Expression>),
    Division(Box<Expression>, Box<Expression>),
    Addition(Box<Expression>, Box<Expression>),
    Subtraction(Box<Expression>, Box<Expression>),

    Variable(String, Box<Expression>, Box<Expression>),
    Scope(Box<Expression>),
    Method {
        name: String,
        parameters: Box<Vec<Expression>>,
        body: Box<Expression>,
    },
    MethodParameter {
        name: String,
        kind: String,
    },
    For {
        item: String,
        collection: Box<Expression>,
        body: Box<Expression>,
    },
    While(Box<Expression>, Box<Expression>),
    Loop(Box<Expression>),

    If {
        condition: Box<Expression>,
        body: Box<Expression>,
        continuation: Option<Box<Expression>>,
    },
    ElseIf {
        condition: Box<Expression>,
        body: Box<Expression>,
        continuation: Option<Box<Expression>>,
    },
    Else {
        body: Box<Expression>,
    },

    Invalid(String, Range<usize>),
    EndOfFile,
    None,
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
