use crate::token::{Token, TokenStream, TokenType};

use crunch_error::parse_prelude::{Diagnostic, Label, ParseResult};
use string_interner::{StringInterner, Symbol};

use core::{convert::TryFrom, num::NonZeroU64};
use std::sync::{Arc, RwLock};

mod ast;
mod expr;
mod stmt;
#[cfg(test)]
mod tests;

pub use ast::Ast;
pub use expr::{BinaryOperand, ComparisonOperand, Expression, Literal, PrefixOperand};
pub use stmt::Statement;

pub struct Parser<'a> {
    token_stream: TokenStream<'a>,
    next: Option<Token<'a>>,
    peek: Option<Token<'a>>,

    current_file: usize,
    string_interner: Arc<RwLock<StringInterner<Sym>>>,
}

/// Initialization and high-level usage
impl<'a> Parser<'a> {
    pub fn new(
        source: &'a str,
        current_file: usize,
        string_interner: Arc<RwLock<StringInterner<Sym>>>,
    ) -> Self {
        let (token_stream, next, peek) = Self::lex(source);

        Self {
            token_stream,
            next,
            peek,
            current_file,
            string_interner,
        }
    }

    pub fn parse() -> Result<(Vec<Ast>, Vec<Diagnostic<usize>>), Vec<Diagnostic<usize>>> {
        todo!()
    }

    pub fn lex(source: &'a str) -> (TokenStream<'a>, Option<Token<'a>>, Option<Token<'a>>) {
        let mut token_stream = TokenStream::new(source, true);
        let next = None;
        let peek = token_stream.next_token();

        (token_stream, next, peek)
    }
}

/// Utility functions
impl<'a> Parser<'a> {
    fn next(&mut self) -> ParseResult<Token<'a>> {
        let mut next = self.token_stream.next_token();
        std::mem::swap(&mut next, &mut self.peek);
        self.next = next;

        dbg!(next).ok_or(Diagnostic::error().with_message("Unexpected End Of File"))
    }

    fn current(&mut self) -> ParseResult<Token<'a>> {
        self.next
            .ok_or(Diagnostic::error().with_message("Unexpected End Of File"))
    }

    fn peek(&self) -> ParseResult<Token<'a>> {
        self.peek
            .ok_or(Diagnostic::error().with_message("Unexpected End Of File"))
    }

    fn eat(&mut self, expected: TokenType) -> ParseResult<Token<'a>> {
        let token = self.next()?;

        if token.ty() == expected {
            Ok(token)
        } else {
            Err(Diagnostic::error()
                .with_message(format!(
                    "Unexpected Token: Expected '{}', found '{}'",
                    expected, token.ty
                ))
                .with_labels(vec![Label::primary(
                    self.current_file,
                    token.range.0 as usize..token.range.1 as usize,
                )
                .with_message(format!("Expected {}", expected))]))
        }
    }

    fn current_precedence(&self) -> usize {
        self.peek
            .map(|p| {
                BinaryPrecedence::try_from(p.ty())
                    .map(|p| p.precedence())
                    .unwrap_or(0)
            })
            .unwrap_or(0)
    }

    fn intern_string(&self, string: &str) -> Sym {
        if let Some(sym) = self.string_interner.read().unwrap().get(string) {
            sym
        } else {
            self.string_interner.write().unwrap().get_or_intern(string)
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct Sym(NonZeroU64);

impl Symbol for Sym {
    /// Creates a `Sym` from the given `usize`.
    ///
    /// # Panics
    ///
    /// If the given `usize` is greater than `u32::MAX - 1`.
    fn from_usize(val: usize) -> Self {
        assert!(
            val < u64::MAX as usize,
            "Symbol value {} is too large and not supported by `string_interner::Sym` type",
            val
        );
        Sym(NonZeroU64::new((val + 1) as u64).unwrap_or_else(|| {
            unreachable!("Should never fail because `val + 1` is nonzero and `<= u32::MAX`")
        }))
    }

    fn to_usize(self) -> usize {
        (self.0.get() as usize) - 1
    }
}

impl From<usize> for Sym {
    fn from(val: usize) -> Self {
        Sym::from_usize(val)
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
#[rustfmt::skip]
pub enum BinaryPrecedence {
    Mul, Div, Mod,
    Add, Sub,
    Shl, Shr,
    Less, Greater, LessEq, GreaterEq,
    Eq, Ne,
    BitAnd,
    BitXor,
    BitOr,
    LogAnd,
    LogOr,
    Ternary,
    Assignment,
}

impl BinaryPrecedence {
    pub fn precedence(self) -> usize {
        match self {
            Self::Mul | Self::Div | Self::Mod => 11,
            Self::Add | Self::Sub => 10,
            Self::Shl | Self::Shr => 9,
            Self::Less | Self::Greater | Self::LessEq | Self::GreaterEq => 8,
            Self::Eq | Self::Ne => 7,
            Self::BitAnd => 6,
            Self::BitXor => 5,
            Self::BitOr => 4,
            Self::LogAnd => 3,
            Self::LogOr => 2,
            Self::Ternary => 1,
            Self::Assignment => 0,
        }
    }
}

impl TryFrom<TokenType> for BinaryPrecedence {
    type Error = ();

    fn try_from(t: TokenType) -> Result<BinaryPrecedence, ()> {
        Ok(match t {
            TokenType::Star => Self::Mul,
            TokenType::Divide => Self::Div,
            TokenType::Modulo => Self::Mod,
            TokenType::Plus => Self::Add,
            TokenType::Minus => Self::Sub,
            TokenType::Shl => Self::Shl,
            TokenType::Shr => Self::Shr,
            TokenType::LeftCaret => Self::Less,
            TokenType::RightCaret => Self::Greater,
            TokenType::LessThanEqual => Self::LessEq,
            TokenType::GreaterThanEqual => Self::GreaterEq,
            TokenType::IsEqual => Self::Eq,
            TokenType::IsNotEqual => Self::Ne,
            TokenType::Ampersand => Self::BitAnd,
            TokenType::Caret => Self::BitXor,
            TokenType::Pipe => Self::BitOr,
            TokenType::And => Self::LogAnd,
            TokenType::Or => Self::LogOr,
            TokenType::Equal => Self::Assignment,
            TokenType::If => Self::Ternary,

            _ => return Err(()),
        })
    }
}
