use logos::{Lexer, Logos};

use crate::error::Span;
use alloc::vec::Vec;
use core::{fmt, ops};

#[derive(Logos, Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum TokenType {
    #[error]
    #[regex(r"[ \t\f]+", logos::skip)]
    Error,

    #[regex(r"[\p{XID_Start}\p{Emoji_Presentation}][\p{XID_Continue}\p{Emoji_Presentation}]*")]
    Ident,

    #[regex("::[^\r\n]*", priority = 2)]
    #[regex("::[^\n]*", priority = 1)]
    Comment,
    #[regex(":::[^\r\n]*", priority = 4)]
    #[regex(":::[^\n]*", priority = 3)]
    DocComment,
    #[token("\n")]
    #[token("\r\n")]
    Newline,
    #[token(" ")]
    Space,

    #[regex("b?'[^']*'")]
    Rune,
    #[regex(r#"b?"(\\.|[^\\"])*""#)] // " <- This is here to restore syntax highlighting
    String,
    #[token("inf")]
    #[token("NaN")]
    #[regex(r#"[+-]?[0-9][0-9_]*\.[0-9][0-9_]*([eE][+-]?[0-9][0-9_]*)?"#)]
    #[regex(
        r#"[+-]?0x[0-9a-fA-F][0-9a-fA-F_]*\.[0-9a-fA-F][0-9a-fA-F_]*([pP][+-]?[0-9][0-9_]?)?"#
    )]
    Float,
    #[regex("[+-]?[0-9][0-9_]*")]
    #[regex("[+-]?0b[0-1][0-1_]*")]
    #[regex("[+-]?0x[0-9a-fA-F][0-9a-fA-F_]*")]
    Int,
    #[token("true")]
    #[token("false")]
    Bool,

    #[token("fn")]
    Function,
    #[token("import")]
    Import,
    #[token("let")]
    Let,
    #[token("type")]
    Type,
    #[token("enum")]
    Enum,
    #[token("trait")]
    Trait,
    #[token("comptime")]
    Comptime,

    #[token("in")]
    In,
    #[token("loop")]
    Loop,
    #[token("while")]
    While,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("then")]
    Then,
    #[token("for")]
    For,
    #[token("return")]
    Return,
    #[token("continue")]
    Continue,
    #[token("break")]
    Break,
    #[token("match")]
    Match,

    #[token("exposing")]
    Exposing,
    #[token("export")]
    Export,
    #[token("as")]
    As,
    #[token("lib")]
    Library,
    #[token("end")]
    End,
    #[token("pkg")]
    Package,
    #[token("exposed")]
    Exposed,
    #[token("empty")]
    Empty,

    #[token("or")]
    Or,
    #[token("and")]
    And,
    #[token("where")]
    Where,

    #[token("=")]
    Equal,
    #[token("+=")]
    AddAssign,
    #[token("-=")]
    SubAssign,
    #[token("*=")]
    MultAssign,
    #[token("/=")]
    DivAssign,
    #[token("%=")]
    ModAssign,
    #[token("**=")]
    PowAssign,
    #[token("<<=")]
    ShlAssign,
    #[token(">>=")]
    ShrAssign,
    #[token("|=")]
    OrAssign,
    #[token("&=")]
    AndAssign,
    #[token("^=")]
    XorAssign,

    #[token("==")]
    IsEqual,
    #[token("!=")]
    IsNotEqual,
    #[token(">=")]
    GreaterThanEqual,
    #[token("<=")]
    LessThanEqual,
    #[token("<")]
    LeftCaret,
    #[token(">")]
    RightCaret,

    #[token("!")]
    Bang,

    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("/")]
    Divide,
    #[token("*")]
    Star,
    #[token("%")]
    Modulo,
    #[token("**")]
    DoubleStar,
    #[token("<<")]
    Shl,
    #[token(">>")]
    Shr,
    #[token("|")]
    Pipe,
    #[token("&")]
    Ampersand,
    #[token("^")]
    Caret,

    #[token("[")]
    LeftBrace,
    #[token("]")]
    RightBrace,
    #[token("(")]
    LeftParen,
    #[token(")")]
    RightParen,
    #[token("{")]
    LeftBracket,
    #[token("}")]
    RightBracket,
    #[token("->")]
    RightArrow,
    #[token("<-")]
    LeftArrow,
    #[token("=>")]
    RightRocket,

    #[token("@")]
    AtSign,
    #[token(",")]
    Comma,
    #[token(";")]
    Semicolon,
    #[token(":")]
    Colon,
    #[token(".")]
    Dot,
    #[token("..")]
    DoubleDot,
}

impl TokenType {
    pub fn to_str(&self) -> &'static str {
        match self {
            Self::Error => "Error",

            Self::Ident => "Ident",

            Self::Comment => "Comment",
            Self::DocComment => "DocComment",
            Self::Newline => "Newline",
            Self::Space => " ",

            Self::String => "str",
            Self::Rune => "rune",
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
            Self::Comptime => "comptime",

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
    lexer: Lexer<'a, TokenType>,
    current: Option<Token<'a>>,
}

impl<'a> TokenIter<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut lexer = TokenType::lexer(input);

        let current = lexer
            .next()
            .map(|current| Token::new(current, lexer.slice(), lexer.span()));

        Self { lexer, current }
    }

    pub fn get_next(&mut self) -> Option<Token<'a>> {
        let next = self
            .lexer
            .next()
            .map(|current| Token::new(current, self.lexer.slice(), self.lexer.span()));

        core::mem::replace(&mut self.current, next)
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

            TokenIter {
                lexer,
                current: self.current.clone(),
            }
            .collect::<Vec<Token<'a>>>()
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
    skip_doc_comments: bool,
    current: Option<Token<'a>>,
}

impl<'a> TokenStream<'a> {
    pub fn new(input: &'a str, skip_comments: bool, skip_doc_comments: bool) -> Self {
        Self {
            token_stream: TokenIter::new(input),
            indent_level: 0,
            line_indent_level: 0,
            line_start: false,
            skip_comments,
            skip_doc_comments,
            current: None,
        }
    }

    pub fn next_token(&mut self) -> Option<Token<'a>> {
        if let Some(token) = self.token_stream.next() {
            match token.ty {
                TokenType::Space => self.next_token(),
                TokenType::Comment if self.skip_comments => self.next_token(),
                TokenType::DocComment if self.skip_doc_comments => self.next_token(),
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

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Token<'a> {
    pub(crate) ty: TokenType,
    pub(crate) source: &'a str,
    pub(crate) span: Span,
}

impl<'a> Token<'a> {
    pub fn new(ty: TokenType, source: &'a str, range: ops::Range<usize>) -> Self {
        Self {
            ty,
            source,
            span: Span::from(range),
        }
    }

    pub const fn ty(&self) -> TokenType {
        self.ty
    }

    pub const fn span(&self) -> Span {
        self.span
    }

    pub fn range(&self) -> ops::Range<usize> {
        self.span.into()
    }

    pub const fn source(&self) -> &'a str {
        self.source
    }
}

impl<'a> Into<ops::Range<usize>> for Token<'a> {
    fn into(self) -> ops::Range<usize> {
        self.span.into()
    }
}

impl<'a> Into<ops::Range<usize>> for &Token<'a> {
    fn into(self) -> ops::Range<usize> {
        self.span.into()
    }
}

impl<'a> Into<(usize, usize)> for Token<'a> {
    fn into(self) -> (usize, usize) {
        self.span.into()
    }
}

impl<'a> Into<(usize, usize)> for &Token<'a> {
    fn into(self) -> (usize, usize) {
        self.span.into()
    }
}

impl<'a> Into<[usize; 2]> for Token<'a> {
    fn into(self) -> [usize; 2] {
        self.span.into()
    }
}

impl<'a> Into<[usize; 2]> for &Token<'a> {
    fn into(self) -> [usize; 2] {
        self.span.into()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // Waiting on logos/#94 to fix this
    #[test]
    fn break_up_weird_stuff() {
        let mut stream = TokenStream::new("bredcp", true, true).into_iter();

        assert_eq!(
            stream.next(),
            Some(Token {
                ty: TokenType::Ident,
                source: "bredcp",
                span: Span::new(0, 6)
            })
        );
        assert_eq!(stream.next(), None);

        let mut stream = TokenStream::new("breuyue", true, true).into_iter();

        assert_eq!(
            stream.next(),
            Some(Token {
                ty: TokenType::Ident,
                source: "breuyue",
                span: Span::new(0, 7)
            })
        );
        assert_eq!(stream.next(), None);
    }

    #[test]
    fn keyword_led_ident() {
        let mut exposed_function = TokenStream::new("exposed_function", true, true).into_iter();

        assert_eq!(
            exposed_function.next(),
            Some(Token {
                ty: TokenType::Ident,
                source: "exposed_function",
                span: Span::new(0, 16)
            })
        );
        assert_eq!(exposed_function.next(), None);
    }

    #[test]
    fn dont_capture_comments() {
        let mut stream =
            TokenStream::new(":: fn something(int: str) -> bool {}\n", true, true).into_iter();

        assert_eq!(
            stream.next(),
            Some(Token {
                ty: TokenType::Newline,
                source: "\n",
                span: Span::new(36, 37)
            })
        );
        assert_eq!(stream.next(), None);

        let mut stream = TokenStream::new(":: test<int, bool>(test2)\n", true, true).into_iter();

        assert_eq!(
            stream.next(),
            Some(Token {
                ty: TokenType::Newline,
                source: "\n",
                span: Span::new(25, 26)
            })
        );
        assert_eq!(stream.next(), None);

        let mut stream =
            TokenStream::new("::: fn something(int: str) -> bool {}\n", true, true).into_iter();

        assert_eq!(
            stream.next(),
            Some(Token {
                ty: TokenType::Newline,
                source: "\n",
                span: Span::new(37, 38)
            })
        );
        assert_eq!(stream.next(), None);

        let mut stream = TokenStream::new("::: test<int, bool>(test2)\n", true, true).into_iter();

        assert_eq!(
            stream.next(),
            Some(Token {
                ty: TokenType::Newline,
                source: "\n",
                span: Span::new(26, 27)
            })
        );
        assert_eq!(stream.next(), None);
    }

    mod proptests {
        use super::*;
        use proptest::prelude::*;

        proptest! {
            #[test]
            fn strings(s in r#"b?"(\\.|[^\\"])*""#) {
                let mut stream = TokenStream::new(&s, true, true).into_iter();

                let cond = matches!(stream.next(), Some(Token { ty: _ty @ TokenType::String, .. }));
                prop_assert!(cond);
                prop_assert_eq!(stream.count(), 0);
            }

            #[test]
            fn runes(s in "b?'[^']*'") {
                let mut stream = TokenStream::new(&s, true, true).into_iter();

                let cond = matches!(stream.next(), Some(Token { ty: _ty @ TokenType::Rune, .. }));
                prop_assert!(cond);
                prop_assert_eq!(stream.count(), 0);
            }

            #[test]
            fn base10_int(s in "[+-]?[0-9][0-9_]*") {
                let mut stream = TokenStream::new(&s, true, true).into_iter();

                let cond = matches!(stream.next(), Some(Token { ty: _ty @ TokenType::Int, .. }));
                prop_assert!(cond);
                prop_assert_eq!(stream.count(), 0);
            }

            #[test]
            fn base16_int(s in "[+-]?0x[0-9a-fA-F][0-9a-fA-F_]*") {
                let mut stream = TokenStream::new(&s, true, true).into_iter();

                let cond = matches!(stream.next(), Some(Token { ty: _ty @ TokenType::Int, .. }));
                prop_assert!(cond);
                prop_assert_eq!(stream.count(), 0);
            }

            #[test]
            fn base2_int(s in "[+-]?0b[0-1][0-1_]*") {
                let mut stream = TokenStream::new(&s, true, true).into_iter();

                let cond = matches!(stream.next(), Some(Token { ty: _ty @ TokenType::Int, .. }));
                prop_assert!(cond);
                prop_assert_eq!(stream.count(), 0);
            }

            #[test]
            fn base10_float(s in "[+-]?[0-9][0-9_]*\\.[0-9][0-9_]*([eE][+-]?[0-9][0-9_]*)?") {
                let mut stream = TokenStream::new(&s, true, true).into_iter();

                let cond = matches!(stream.next(), Some(Token { ty: _ty @ TokenType::Float, .. }));
                prop_assert!(cond);
                prop_assert_eq!(stream.count(), 0);
            }

            #[test]
            fn base16_float(s in "[+-]?0x[0-9a-fA-F][0-9a-fA-F_]*\\.[0-9a-fA-F][0-9a-fA-F_]*([pP][+-]?[0-9][0-9_]?)?") {
                let mut stream = TokenStream::new(&s, true, true).into_iter();

                let cond = matches!(dbg!(stream.next()), Some(Token { ty: _ty @ TokenType::Float, .. }));
                prop_assert!(cond);
                prop_assert_eq!(stream.count(), 0);
            }
        }
    }
}
