use logos::{Lexer, Logos};

use alloc::vec::Vec;
use core::{fmt, ops};

// TODO: Cull tokens
// TODO: Further organize tokens

#[derive(Logos, Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum TokenType {
    #[end]
    EOF,
    #[error]
    Error,

    #[regex = "[a-zA-Z_][a-zA-Z0-9_]*"]
    Ident,

    #[regex = "::[^\r\n]*"]
    #[regex = "::[^\n]*"]
    Comment,
    #[token = "\n"]
    #[token = "\r\n"]
    Newline,
    #[token = " "]
    Space,

    // TODO: Fix
    // #[regex = "b?\"\"\"[^\"\"\"]*\"\"\""]
    // #[regex = "b?'''[^''']*'''"]
    #[regex = r###"b?"(\\.|[^\\"])*""###] // " <- This is here to restore syntax highlighting
    #[regex = r#"b?'(\\.|[^\\'])*'"#]
    String,
    #[regex = r#"[+-]?0[xX][0-9a-fA-F][0-9a-fA-F_]*"#]
    #[regex = r#"[+-]?[0-9][0-9_]*"#]
    Int,
    // This is a crime
    // TODO: Fix these regexes, they capture integers
    #[token = "inf"]
    #[token = "NaN"]
    #[regex = r#"[+-]?[0-9][0-9_]*\.[0-9][0-9_]*([eE][+-]?[0-9][0-9_]*)?"#]
    #[regex = r#"[+-]?0[xX][0-9a-fA-F]+\.[0-9a-fA-F]+([pP][+-]?[0-9]+)?"#]
    Float,
    #[token = "true"]
    #[token = "false"]
    Bool,

    #[token = "fn"]
    Function,
    #[token = "import"]
    Import,
    #[token = "let"]
    Let,
    #[token = "type"]
    Type,
    #[token = "enum"]
    Enum,
    #[token = "trait"]
    Trait,

    #[token = "in"]
    In,
    #[token = "loop"]
    Loop,
    #[token = "while"]
    While,
    #[token = "if"]
    If,
    #[token = "else"]
    Else,
    #[token = "then"]
    Then,
    #[token = "for"]
    For,
    #[token = "return"]
    Return,
    #[token = "continue"]
    Continue,
    #[token = "break"]
    Break,
    #[token = "match"]
    Match,

    #[token = "exposing"]
    Exposing,
    #[token = "export"]
    Export,
    #[token = "as"]
    As,
    #[token = "lib"]
    Library,
    #[token = "end"]
    End,
    #[token = "pkg"]
    Package,
    #[token = "exposed"]
    Exposed,
    #[token = "empty"]
    Empty,

    #[token = "or"]
    Or,
    #[token = "and"]
    And,
    #[token = "where"]
    Where,

    #[token = "="]
    Equal,
    #[token = "+="]
    AddAssign,
    #[token = "-="]
    SubAssign,
    #[token = "*="]
    MultAssign,
    #[token = "/="]
    DivAssign,
    #[token = "%="]
    ModAssign,
    #[token = "**="]
    PowAssign,
    #[token = "<<="]
    ShlAssign,
    #[token = ">>="]
    ShrAssign,
    #[token = "|="]
    OrAssign,
    #[token = "&="]
    AndAssign,
    #[token = "^="]
    XorAssign,

    #[token = "=="]
    IsEqual,
    #[token = "!="]
    IsNotEqual,
    #[token = ">="]
    GreaterThanEqual,
    #[token = "<="]
    LessThanEqual,
    #[token = "<"]
    LeftCaret,
    #[token = ">"]
    RightCaret,

    #[token = "!"]
    Bang,

    #[token = "+"]
    Plus,
    #[token = "-"]
    Minus,
    #[token = "/"]
    Divide,
    #[token = "*"]
    Star,
    #[token = "%"]
    Modulo,
    #[token = "**"]
    DoubleStar,
    #[token = "<<"]
    Shl,
    #[token = ">>"]
    Shr,
    #[token = "|"]
    Pipe,
    #[token = "&"]
    Ampersand,
    #[token = "^"]
    Caret,

    #[token = "["]
    LeftBrace,
    #[token = "]"]
    RightBrace,
    #[token = "("]
    LeftParen,
    #[token = ")"]
    RightParen,
    #[token = "{"]
    LeftBracket,
    #[token = "}"]
    RightBracket,
    #[token = "->"]
    RightArrow,
    #[token = "<-"]
    LeftArrow,
    #[token = "=>"]
    RightRocket,

    #[token = "@"]
    AtSign,
    #[token = ","]
    Comma,
    #[token = ";"]
    Semicolon,
    #[token = ":"]
    Colon,
    #[token = "."]
    Dot,
    #[token = ".."]
    DoubleDot,
}

impl TokenType {
    pub fn to_str(&self) -> &'static str {
        match self {
            Self::EOF => "EOF",
            Self::Error => "Error",

            Self::Ident => "Ident",

            Self::Comment => "Comment",
            Self::Newline => "Newline",
            Self::Space => " ",

            Self::String => "str",
            Self::Int => "int",
            Self::Float => "float",
            Self::Bool => "bool",

            Self::Let => "let",
            Self::Return => "return",
            Self::Continue => "continue",
            Self::Break => "break",
            Self::Package => "pkg",
            Self::Exposed => "exposed",
            Self::Empty => "empty",
            Self::Then => "then",
            Self::For => "for",
            Self::Or => "or",
            Self::And => "and",
            Self::Type => "type",
            Self::Loop => "loop",
            Self::While => "while",
            Self::If => "if",
            Self::Else => "else",
            Self::Function => "fn",
            Self::Enum => "enum",
            Self::Trait => "trait",
            Self::Import => "import",
            Self::Exposing => "exposing",
            Self::Export => "export",
            Self::As => "as",
            Self::Library => "lib",
            Self::End => "end",
            Self::In => "in",
            Self::Match => "match",
            Self::Where => "where",

            Self::Equal => "=",
            Self::AddAssign => "+=",
            Self::SubAssign => "-=",
            Self::MultAssign => "*=",
            Self::DivAssign => "/=",
            Self::ModAssign => "%=",
            Self::PowAssign => "**=",
            Self::ShlAssign => "<<=",
            Self::ShrAssign => ">>=",
            Self::OrAssign => "|=",
            Self::AndAssign => "&=",
            Self::XorAssign => "^=",

            Self::IsEqual => "==",
            Self::IsNotEqual => "!=",
            Self::GreaterThanEqual => ">=",
            Self::LessThanEqual => "<=",
            Self::LeftCaret => "<",
            Self::RightCaret => ">",

            Self::Bang => "!",

            Self::Plus => "+",
            Self::Minus => "-",
            Self::Divide => "/",
            Self::Star => "*",
            Self::Modulo => "%",
            Self::DoubleStar => "**",
            Self::Pipe => "|",
            Self::Caret => "^",
            Self::Ampersand => "&",
            Self::Shl => "<<",
            Self::Shr => ">>",

            Self::LeftBrace => "[",
            Self::RightBrace => "]",
            Self::LeftParen => "(",
            Self::RightParen => ")",
            Self::LeftBracket => "{",
            Self::RightBracket => "}",
            Self::RightArrow => "->",
            Self::LeftArrow => "<-",
            Self::RightRocket => "=>",

            Self::AtSign => "@",
            Self::Comma => ",",
            Self::Semicolon => ";",
            Self::Colon => ":",
            Self::Dot => ".",
            Self::DoubleDot => "..",
        }
    }
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let string = self.to_str();

        write!(f, "{}", string)
    }
}

#[derive(Clone)]
pub struct TokenIter<'a> {
    lexer: Lexer<TokenType, &'a str>,
    current: Token<'a>,
}

impl<'a> TokenIter<'a> {
    pub fn new(input: &'a str) -> Self {
        let lexer = TokenType::lexer(input);
        let current = Token::new(lexer.token, lexer.slice(), lexer.range());

        Self { lexer, current }
    }

    pub fn get_next(&mut self) -> Option<Token<'a>> {
        self.current = Token::new(self.lexer.token, self.lexer.slice(), self.lexer.range());
        self.lexer.advance();

        match self.current.ty {
            TokenType::EOF => None,
            _ => Some(self.current),
        }
    }
}

impl<'a> Iterator for TokenIter<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.get_next()
    }
}

impl<'a> fmt::Debug for TokenIter<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let lexer = {
            let lexer = self.lexer.clone();
            let current = Token::new(lexer.token, lexer.slice(), lexer.range());

            TokenIter { lexer, current }.collect::<Vec<Token<'a>>>()
        };

        f.debug_struct("TokenIter")
            .field("lexer", &lexer)
            .field("current", &self.current)
            .finish()
    }
}

#[derive(Debug, Clone)]
pub struct TokenStream<'a> {
    token_stream: TokenIter<'a>,
    indent_level: u32,
    line_indent_level: u32,
    line_start: bool,
    skip_comments: bool,
    current: Option<Token<'a>>,
}

impl<'a> TokenStream<'a> {
    pub fn new(input: &'a str, skip_comments: bool) -> Self {
        Self {
            token_stream: TokenIter::new(input),
            indent_level: 0,
            line_indent_level: 0,
            line_start: false,
            skip_comments,
            current: None,
        }
    }

    pub fn next_token(&mut self) -> Option<Token<'a>> {
        if let Some(token) = self.token_stream.next() {
            match token.ty {
                TokenType::Space => self.next_token(),
                TokenType::Comment if self.skip_comments => self.next_token(),
                TokenType::EOF => None,
                _ => Some(token),
            }
        } else {
            None
        }
    }
}

impl<'a> Iterator for TokenStream<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

#[derive(PartialEq, Eq, Clone, Copy)]
pub struct Token<'a> {
    ty: TokenType,
    pub(crate) source: &'a str,
    range: (usize, usize),
}

impl<'a> Token<'a> {
    pub const fn new(ty: TokenType, source: &'a str, range: ops::Range<usize>) -> Self {
        Self {
            ty,
            source,
            range: (range.start, range.end),
        }
    }

    pub const fn ty(&self) -> TokenType {
        self.ty
    }

    pub const fn range(&self) -> ops::Range<usize> {
        self.range.0..self.range.1
    }

    pub const fn source(&self) -> &'a str {
        self.source
    }
}

impl<'a> Into<(usize, usize)> for &Token<'a> {
    fn into(self) -> (usize, usize) {
        self.range
    }
}

impl<'a> fmt::Debug for Token<'a> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.debug_struct("Token")
            .field("ty", &self.ty)
            .field("source", &self.source)
            .field("range", &(self.range.0..self.range.1))
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn break_up_weird_stuff() {
        let mut stream = TokenStream::new("bredcp", true).into_iter();

        assert_eq!(
            stream.next(),
            Some(Token {
                ty: TokenType::Ident,
                source: "bredcp",
                range: (0, 6)
            })
        );
        assert_eq!(stream.next(), None);

        let mut stream = TokenStream::new("breuyue", true).into_iter();

        assert_eq!(
            stream.next(),
            Some(Token {
                ty: TokenType::Ident,
                source: "breuyue",
                range: (0, 7)
            })
        );
        assert_eq!(stream.next(), None);
    }

    #[test]
    fn keyword_led_ident() {
        let mut exposed_function = TokenStream::new("exposed_function", true).into_iter();

        assert_eq!(
            exposed_function.next(),
            Some(Token {
                ty: TokenType::Ident,
                source: "exposed_function",
                range: (0, 16)
            })
        );
        assert_eq!(exposed_function.next(), None);
    }

    #[test]
    fn dont_capture_comments() {
        let mut exposed_function =
            TokenStream::new(":: fn something(int: str) -> bool {}\n", true).into_iter();

        assert_eq!(
            exposed_function.next(),
            Some(Token {
                ty: TokenType::Newline,
                source: "\n",
                range: (36, 37)
            })
        );
        assert_eq!(exposed_function.next(), None);

        let mut exposed_function =
            TokenStream::new(":: test<int, bool>(test2)\n", true).into_iter();

        assert_eq!(
            exposed_function.next(),
            Some(Token {
                ty: TokenType::Newline,
                source: "\n",
                range: (25, 26)
            })
        );
        assert_eq!(exposed_function.next(), None);
    }
}
