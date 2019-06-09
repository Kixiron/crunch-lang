#[derive(Debug, PartialEq)]
pub enum Expression {
    IntLiteral(i32),
    FloatLiteral(f32),
    StringLiteral(String),
    Vector(Box<Vec<Expression>>),

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

    None,
}
