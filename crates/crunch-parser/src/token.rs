use core::{fmt, ops};
use logos::{Lexer, Logos};

// TODO: Organize all this

#[derive(Logos, Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum TokenType {
    #[regex = "[a-zA-Z_][a-zA-Z0-9_]*"]
    Ident,
    #[regex = "\"[^\"]*\""]
    #[regex = "'[^']*'"]
    String,
    #[regex = "[1234567890]+"]
    Int,
    #[regex = "::[^\r\n]*"]
    #[regex = "::[^\n]*"]
    Comment,
    #[token = "\n"]
    #[token = "\r\n"]
    Newline,
    #[end]
    EOF,
    #[error]
    Error,
    #[token = "@"]
    AtSign,
    #[token = "!"]
    Bang,
    #[token = ","]
    Comma,
    #[token = ";"]
    Semicolon,
    #[token = "in"]
    In,
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
    #[token = "!="]
    IsNotEqual,
    #[token = ">="]
    GreaterThanEqual,
    #[token = "<="]
    LessThanEqual,
    #[token = "["]
    LeftBrace,
    #[token = "]"]
    RightBrace,
    #[token = "/"]
    Divide,
    #[token = "%"]
    Modulo,
    #[token = "<<"]
    Shl,
    #[token = ">>"]
    Shr,
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
    #[token = " "]
    Space,
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
    #[token = "end"]
    End,
    #[token = "<"]
    LeftCaret,
    #[token = ">"]
    RightCaret,
    #[token = "|"]
    Pipe,
    #[token = "&"]
    Ampersand,
    #[token = "bin"]
    Binary,
    #[token = "^"]
    Caret,
    #[token = "."]
    Dot,
    #[token = "exposed"]
    Exposed,
    #[token = "empty"]
    Empty,
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
    #[token = "type"]
    Type,
    #[token = "or"]
    Or,
    #[token = "and"]
    And,
}

impl std::fmt::Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let string = match self {
            Self::Or => "or",
            Self::And => "and",
            Self::Type => "type",
            Self::EOF => "EOF",
            Self::Error => "Error",
            Self::Comma => ",",
            Self::Let => "let",
            Self::Plus => "+",
            Self::In => "in",
            Self::Dot => ".",
            Self::Colon => ":",
            Self::AtSign => "@",
            Self::Bang => "!",
            Self::Modulo => "%",
            Self::Shl => "<<",
            Self::Shr => ">>",
            Self::Semicolon => ";",
            Self::Minus => "-",
            Self::Equal => "=",
            Self::IsEqual => "==",
            Self::IsNotEqual => "!=",
            Self::GreaterThanEqual => ">=",
            Self::LessThanEqual => "<=",
            Self::LeftBrace => "[",
            Self::RightBrace => "]",
            Self::Divide => "/",
            Self::Star => "*",
            Self::Pipe => "|",
            Self::Caret => "^",
            Self::Ampersand => "&",
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
            Self::End => "end",
            Self::LeftCaret => "<",
            Self::RightCaret => ">",
            Self::Return => "return",
            Self::Continue => "continue",
            Self::Break => "break",
            Self::Binary => "bin",
            Self::Exposed => "exposed",
            Self::Empty => "empty",
            Self::Then => "then",
            Self::For => "for",
        };

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

impl<'a> std::fmt::Debug for TokenIter<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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
    pub ty: TokenType,
    pub source: &'a str,
    pub range: (usize, usize),
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
    fn token_test() {
        const CODE: &str = include_str!("../../../tests/parse_test.crunch");

        let tokens = TokenStream::new(CODE, true)
            .into_iter()
            .map(|t| t.ty)
            .collect::<Vec<_>>();

        println!("{:#?}", tokens);
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
        assert!(exposed_function.collect::<Vec<_>>().is_empty());
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
        assert!(exposed_function.collect::<Vec<_>>().is_empty());

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
        assert!(exposed_function.collect::<Vec<_>>().is_empty());
    }
}
