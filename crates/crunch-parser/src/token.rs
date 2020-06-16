use alloc::vec::Vec;
use core::{fmt, ops};
use crunch_shared::error::Span;
use logos::{Lexer, Logos};

#[derive(Logos, Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum TokenType {
    #[error]
    #[regex(r"[ \t\f]+", logos::skip)]
    Error,

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

    #[regex(r"[\p{XID_Start}\p{Emoji_Presentation}][\p{XID_Continue}\p{Emoji_Presentation}]*")]
    Ident,

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
    #[token("const")]
    Const,
    #[token("extend")]
    Extend,
    #[token("with")]
    With,
    #[token("alias")]
    Alias,
    #[token("mut")]
    Mut,
    #[token("ref")]
    Ref,

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
    pub fn to_str(self) -> &'static str {
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
            Self::Const => "comptime",
            Self::Extend => "extend",
            Self::With => "with",
            Self::Alias => "alias",
            Self::Mut => "mut",
            Self::Ref => "ref",

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
pub struct TokenStream<'a> {
    lexer: Lexer<'a, TokenType>,
    skip_comments: bool,
    skip_doc_comments: bool,
}

impl<'a> TokenStream<'a> {
    pub fn new(input: &'a str, skip_comments: bool, skip_doc_comments: bool) -> Self {
        Self {
            lexer: TokenType::lexer(input),
            skip_comments,
            skip_doc_comments,
        }
    }
}

impl<'a> Iterator for TokenStream<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.lexer.next().and_then(|token| {
            let token = Token::new(token, self.lexer.slice(), self.lexer.span());
            match token.ty {
                TokenType::Space => self.next(),
                TokenType::Comment if self.skip_comments => self.next(),
                TokenType::DocComment if self.skip_doc_comments => self.next(),

                _ => Some(token),
            }
        })
    }
}

impl fmt::Debug for TokenStream<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let tokens = self.clone().collect::<Vec<Token<'_>>>();

        f.debug_struct("TokenStream")
            .field("lexer", &tokens)
            .field("skip_comments", &self.skip_comments)
            .field("skip_doc_comments", &self.skip_doc_comments)
            .finish()
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Token<'a> {
    pub(crate) ty: TokenType,
    pub(crate) source: &'a str,
    pub(crate) span: Span,
}

impl<'a> Token<'a> {
    pub const fn new(ty: TokenType, source: &'a str, range: ops::Range<usize>) -> Self {
        Self {
            ty,
            source,
            span: Span::new(range.start, range.end),
        }
    }

    pub const fn ty(&self) -> TokenType {
        self.ty
    }

    pub const fn span(&self) -> Span {
        self.span
    }

    pub const fn range(&self) -> ops::Range<usize> {
        self.span.start()..self.span.end()
    }

    pub const fn source(&self) -> &'a str {
        self.source
    }
}

impl Into<Span> for Token<'_> {
    fn into(self) -> Span {
        self.span()
    }
}

impl Into<Span> for &Token<'_> {
    fn into(self) -> Span {
        self.span()
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

impl fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} @ {:?}", self.source, self.span)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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

    #[cfg(not(any(target_arch = "wasm32", miri)))]
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

                let cond = matches!(stream.next(), Some(Token { ty: _ty @ TokenType::Float, .. }));
                prop_assert!(cond);
                prop_assert_eq!(stream.count(), 0);
            }
        }
    }
}
