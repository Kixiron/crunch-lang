mod token_stream;

use logos::{self, Logos};
pub use token_stream::*;

#[derive(Logos, Debug, PartialEq, Clone, Copy)]
pub enum Token {
    #[end]
    EndOfFile,
    #[error]
    Crunch,

    #[token = "fn"]
    Function,
    #[token = "let"]
    Variable,
    #[token = "mut"]
    Mutable,
    #[token = "const"]
    Constant,
    #[token = "type"]
    Type,
    #[token = "and"]
    #[token = "&&"]
    And,
    #[token = "or"]
    #[token = "||"]
    Or,
    #[token = "not"]
    Not,
    #[token = "if"]
    If,
    #[token = "else if"]
    ElseIf,
    #[token = "else"]
    Else,
    #[token = "for"]
    For,
    #[token = "loop"]
    Loop,
    #[token = "while"]
    While,
    #[token = "in"]
    In,
    #[token = "continue"]
    Continue,
    #[token = "return"]
    Return,

    #[regex = "//[^\n]+\n"]
    Comment,
    #[regex = r#"/\*[^\*/]+\*/"#]
    MultilineComment,
    #[regex = "///[^\n]+\n"]
    DocComment,

    #[token = ":"]
    Colon,
    #[token = ";"]
    SemiColon,
    #[token = "."]
    Dot,
    #[token = ","]
    Comma,
    #[token = "?"]
    Question,
    #[token = "->"]
    Arrow,

    #[token = "int"]
    Int,
    #[token = "str"]
    Str,
    #[token = "void"]
    Void,
    #[regex = r#"'[^']+'"#]
    #[regex = r#""[^"]+""#]
    // " This comment exists to spare your/my syntax highlighting
    StrLiteral,
    // TODO: Allow underscores in integers for better readability
    #[regex = "-?[0-9]+"]
    IntLiteral,
    #[regex = r#"-?[0-9]+\.[0-9]+"#]
    FloatLiteral,

    #[token = "bool"]
    Bool,
    #[token = "true"]
    #[token = "false"]
    BoolLiteral,

    #[token = "null"]
    Null,

    #[regex = r#"\[[-0-9a-zA-Z_, ]+\]"#]
    VectorLiteral,

    #[regex = r#"Vec<[-0-9a-zA-Z_]+>"#]
    Vector,

    #[token = "}"]
    RightBrace,
    #[token = "{"]
    LeftBrace,

    #[token = "]"]
    RightBracket,
    #[token = "["]
    LeftBracket,

    #[token = ")"]
    RightParentheses,
    #[token = "("]
    LeftParentheses,

    #[token = "="]
    Equals,
    #[token = "=="]
    IsEqual,

    #[token = "!"]
    Bang,
    #[token = "!="]
    BangEquals,

    #[token = "<"]
    Less,
    #[token = "<="]
    LessEqual,

    #[token = ">"]
    Greater,
    #[token = ">="]
    GreaterEqual,

    #[token = "+"]
    Plus,
    #[token = "+="]
    PlusEquals,
    #[token = "++"]
    PlusOne,

    #[token = "-"]
    Minus,
    #[token = "-="]
    MinusEquals,
    #[token = "--"]
    MinusOne,

    #[token = "*"]
    Multiply,
    #[token = "*="]
    MultiplyEquals,

    #[token = "/"]
    Divide,
    #[token = "/="]
    DivideEquals,

    #[token = ">>"]
    BitshiftRight,
    #[token = ">>="]
    BitshiftRightEquals,

    #[token = "<<"]
    BitshiftLeft,
    #[token = "<<="]
    BitshiftLeftEquals,

    #[token = "&"]
    BitwiseAnd,
    #[token = "&="]
    BitwiseAndEquals,

    #[token = "|"]
    BitwiseOr,
    #[token = "|="]
    BitwiseOrEquals,

    #[token = "\0"]
    #[token = "\n"]
    #[token = "\t"]
    #[token = "\r"]
    #[token = " "]
    WhiteSpace,

    // TODO: Desired Regex is [a-zA-Z_]{1}[a-zA-Z0-9_]* but it overflows the stack, so logos needs to improve before that happens
    #[regex = "[a-zA-Z0-9_]+"]
    Identifier,
}

unsafe impl Send for Token {}
unsafe impl Sync for Token {}
