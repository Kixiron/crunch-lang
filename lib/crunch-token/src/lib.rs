mod token_stream;

use logos::{self, Logos};
pub use token_stream::*;

#[derive(Logos, Debug, PartialEq, Clone, Copy)]
pub enum Token {
    #[end]
    EndOfFile,
    #[error]
    Crunch,

    #[token = "method"]
    Method,
    #[token = "let"]
    Variable,
    #[regex = "[a-zA-Z_]+"]
    Identifier,

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

    #[token = "int"]
    Int,
    #[token = "str"]
    Str,
    #[regex = r#"'[^']+'"#]
    #[regex = r#""[^"]+""#]
    // " This comment exists to spare your/my syntax highlighting
    StrLiteral,
    #[regex = "-?[0-9]+"]
    IntLiteral,
    #[regex = r#"-?[0-9]+\.[0-9]+"#]
    FloatLiteral,

    #[token = "bool"]
    Bool,
    #[token = "true"]
    True,
    #[token = "false"]
    False,

    #[token = "null"]
    Null,

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

    #[token = "\t"]
    #[token = "    "]
    Indent,

    #[token = "\n"]
    Newline,

    #[token = "\r"]
    #[token = " "]
    WhiteSpace,
}
