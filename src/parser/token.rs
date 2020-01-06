use logos::{Lexer, Logos};

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
    Dedent,
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
    #[token = "end"]
    EndBlock,
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
            Self::Dedent => "Dedent",
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
            Self::EndBlock => "end",
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
            TokenType::End => None,
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

    // pub fn next_token_no_end(&mut self) -> Option<Token<'a>> {
    //     let current = if self.current.is_some() {
    //         let mut token = None;
    //         std::mem::swap(&mut token, &mut self.current);
    //         return token;
    //     } else if let Some(current) = self.token_stream.next() {
    //         current
    //     } else if self.indent_level > 0 {
    //         self.indent_level -= 1;
    //         return Some(Token::new(TokenType::Dedent, "", 0..0));
    //     } else {
    //         return None;
    //     };
    //
    //     let mut token = match current.ty {
    //         // Skip Indents while incrementing the indentation level
    //         TokenType::Indent => {
    //             self.line_indent_level += 1;
    //             self.next_token()
    //         }
    //         TokenType::Newline => {
    //             self.line_start = true;
    //             return Some(current);
    //         }
    //
    //         // Skip whitespace
    //         TokenType::Space => self.next_token(),
    //         TokenType::Comment if self.skip_comments => self.next_token(),
    //
    //         _ => Some(current),
    //     };
    //
    //     if self.line_start {
    //         self.line_start = false;
    //         let indent_insert = if self.line_indent_level < self.indent_level {
    //             self.current = token;
    //             Some(Token::new(TokenType::Dedent, "", 0..0))
    //         } else if self.line_indent_level > self.indent_level {
    //             self.current = token;
    //             Some(Token::new(TokenType::Indent, "", 0..0))
    //         } else if self.current.is_some() {
    //             std::mem::swap(&mut self.current, &mut token);
    //             token
    //         } else {
    //             token
    //         };
    //
    //         self.indent_level = self.line_indent_level;
    //         self.line_indent_level = 0;
    //
    //         indent_insert
    //     } else {
    //         token
    //     }
    // }

    pub fn next_token(&mut self) -> Option<Token<'a>> {
        if let Some(token) = self.token_stream.next() {
            match token.ty {
                TokenType::Space | TokenType::Indent | TokenType::Dedent => self.next_token(),
                TokenType::Comment if self.skip_comments => self.next_token(),
                TokenType::End => None,
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
    pub range: (u32, u32),
}

impl<'a> Token<'a> {
    pub const fn new(ty: TokenType, source: &'a str, range: std::ops::Range<usize>) -> Self {
        Self {
            ty,
            source,
            range: (range.start as u32, range.end as u32),
        }
    }
}

impl<'a> std::fmt::Debug for Token<'a> {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fmt.debug_struct("Token")
            .field("ty", &self.ty)
            .field("source", &self.source)
            .field("range", &(self.range.0..self.range.1))
            .finish()
    }
}

#[test]
fn token_test() {
    const CODE: &str = include_str!("../../examples/parse_test.crunch");

    let tokens = TokenStream::new(CODE, true)
        .into_iter()
        .map(|t| t.ty)
        .collect::<Vec<_>>();

    println!("{:#?}", tokens);
}
