use logos::{Lexer, Logos};
use std::borrow::Cow;

#[derive(Logos, Debug, PartialEq, Eq, Clone, Copy)]
pub enum TokenType {
    #[end]
    End,
    #[error]
    Error,
    #[token = ","]
    Comma,
    #[token = "let"]
    Let,
    #[token = "+"]
    Plus,
    #[token = ":"]
    Colon,
    #[token = "-"]
    Minus,
    #[token = "="]
    Equal,
    #[token = "=="]
    IsEqual,
    #[token = "["]
    LeftBrace,
    #[token = "]"]
    RightBrace,
    #[token = "/"]
    Divide,
    #[token = "*"]
    Star,
    #[token = "loop"]
    Loop,
    #[token = "while"]
    While,
    #[token = "if"]
    If,
    #[token = "else"]
    Else,
    #[token = "fn"]
    Function,
    #[token = "("]
    LeftParen,
    #[token = ")"]
    RightParen,
    #[token = "{"]
    LeftBracket,
    #[token = "}"]
    RightBracket,
    #[regex = "[^ \t\n\r\"\'!@#$%\\^&*()-+=,.<>/?;:\\[\\]{}\\\\|`~]+"]
    Ident,
    #[regex = "[1234567890]+"]
    Int,
    #[regex = "\"[^\"]*\""]
    #[regex = "'[^']*'"]
    String,
    #[token = " "]
    Space,
    #[regex = "::[^\r\n]*"]
    Comment,
    #[token = "    "]
    #[token = "\t"]
    Indent,
    #[token = "\n"]
    #[token = "\r\n"]
    Newline,
    #[token = "->"]
    RightArrow,
    #[token = "<-"]
    LeftArrow,
    #[token = "true"]
    #[token = "false"]
    Bool,
    #[token = "@print"]
    Print,
    #[token = "@collect"]
    Collect,
    #[token = "@halt"]
    Halt,
    #[token = "import"]
    Import,
    #[token = "exposing"]
    Exposing,
    #[token = "export"]
    Export,
    #[token = "as"]
    As,
    #[token = "@exit"]
    SyscallExit,
    #[token = "lib"]
    Library,
    #[token = "pkg"]
    Package,
}

impl std::fmt::Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let string = match self {
            Self::End => "EOF",
            Self::Error => "Error",
            Self::Comma => ",",
            Self::Let => "let",
            Self::Plus => "+",
            Self::Colon => ":",
            Self::Minus => "-",
            Self::Equal => "=",
            Self::IsEqual => "==",
            Self::LeftBrace => "[",
            Self::RightBrace => "]",
            Self::Divide => "/",
            Self::Star => "*",
            Self::Loop => "loop",
            Self::While => "while",
            Self::If => "if",
            Self::Else => "else",
            Self::Function => "fn",
            Self::LeftParen => "(",
            Self::RightParen => ")",
            Self::LeftBracket => "{",
            Self::RightBracket => "}",
            Self::Ident => "Ident",
            Self::Int => "int",
            Self::String => "str",
            Self::Space => " ",
            Self::Comment => "Comment",
            Self::Indent => "Indent",
            Self::Newline => "Newline",
            Self::RightArrow => "->",
            Self::LeftArrow => "<-",
            Self::Bool => "bool",
            Self::Print => "@print",
            Self::Collect => "@collect",
            Self::Halt => "@halt",
            Self::Import => "import",
            Self::Exposing => "exposing",
            Self::Export => "export",
            Self::As => "as",
            Self::SyscallExit => "@exit",
            Self::Library => "lib",
            Self::Package => "pkg",
        };

        write!(f, "{}", string)
    }
}

pub struct TokenStream<'a> {
    lexer: Lexer<TokenType, &'a str>,
    current: Token<'a>,
}

impl<'a> TokenStream<'a> {
    pub fn new(input: &'a str) -> Self {
        let lexer = TokenType::lexer(input);
        let current = Token::new(lexer.token, lexer.slice(), lexer.range());

        Self { current, lexer }
    }

    fn advance(&mut self) {
        self.current = Token::new(self.lexer.token, self.lexer.slice(), self.lexer.range());

        self.lexer.advance();
    }
}

impl<'a> std::fmt::Debug for TokenStream<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let lexer = {
            let lexer = self.lexer.clone();
            let current = Token::new(lexer.token, lexer.slice(), lexer.range());

            TokenStream { lexer, current }.collect::<Vec<Token<'a>>>()
        };

        f.debug_struct("TokenStream")
            .field("lexer", &lexer)
            .field("current", &self.current)
            .finish()
    }
}

impl<'a> Iterator for TokenStream<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.advance();

        match self.current.ty {
            TokenType::End => None,
            _ => Some(self.current.clone()),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Token<'a> {
    pub ty: TokenType,
    pub source: Cow<'a, str>,
    pub range: (u32, u32),
}

impl<'a> Token<'a> {
    pub fn new(ty: TokenType, source: &'a str, range: std::ops::Range<usize>) -> Self {
        Self {
            ty,
            source: Cow::Borrowed(source),
            range: (range.start as u32, range.end as u32),
        }
    }
}
