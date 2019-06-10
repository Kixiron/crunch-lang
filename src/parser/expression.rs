#[derive(Debug, PartialEq)]
pub enum Expression {
    IntLiteral(i32),
    FloatLiteral(f32),
    StringLiteral(String),
    Vector(Box<Vec<Expression>>),
    True,
    False,

    And(Box<Expression>, Box<Expression>),
    Or(Box<Expression>, Box<Expression>),

    Multiplication(Box<Expression>, Box<Expression>),
    Division(Box<Expression>, Box<Expression>),
    Addition(Box<Expression>, Box<Expression>),
    Subtraction(Box<Expression>, Box<Expression>),

    Variable(String, Box<Expression>),
    Scope(Box<Expression>),
    Method {
        name: String,
        parameters: Box<Vec<Expression>>,
        body: Box<Expression>,
    },
    MethodParameter {
        name: String,
        r#type: Option<String>,
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
        condition: Box<Expression>,
        body: Box<Expression>,
    },

    EndOfFile,
    None,
}
