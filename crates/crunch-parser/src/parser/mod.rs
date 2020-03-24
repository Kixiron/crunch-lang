use crate::token::{Token, TokenStream, TokenType};

use crunch_error::{
    codespan_reporting,
    parse_prelude::{Diagnostic, Label, ParseResult},
};
use parking_lot::RwLock;
use string_interner::{StringInterner, Symbol};

use alloc::{format, sync::Arc, vec, vec::Vec};
use core::{convert::TryFrom, mem, num::NonZeroU64};

mod ast;
mod expr;
mod stmt;
mod string_escapes;
#[cfg(test)]
mod tests;

pub use ast::{
    Ast, Attribute, BuiltinType, Decorator, EnumVariant, ImportDest, ImportExposure, Type,
    TypeMember, Visibility,
};
pub use expr::{
    AssignmentType, BinaryOperand, ComparisonOperand, Expression, Literal, UnaryOperand,
};
pub use stmt::Statement;

// TODO: Make the parser a little more lax, it's kinda strict about whitespace

type Interner = Arc<RwLock<StringInterner<Sym>>>;

pub struct Parser<'a> {
    token_stream: TokenStream<'a>,
    next: Option<Token<'a>>,
    peek: Option<Token<'a>>,

    current_file: usize,
    string_interner: Interner,
}

/// Initialization and high-level usage
impl<'a> Parser<'a> {
    pub fn new(source: &'a str, current_file: usize, string_interner: Interner) -> Self {
        let (token_stream, next, peek) = Self::lex(source);

        Self {
            token_stream,
            next,
            peek,
            current_file,
            string_interner,
        }
    }

    // TODO: Decide when an error has occurred, and allow returning of warnings
    // TODO: Should this consume? Is there a viable reason to re-parse?
    // TODO: Maybe consuming is alright, the token stream itself is consumed, so nothing really usable is
    //       left in the parser after this function is run on it
    pub fn parse(mut self) -> Result<(Vec<Ast>, Vec<Diagnostic<usize>>), Vec<Diagnostic<usize>>> {
        let mut ast = Vec::with_capacity(20);
        let mut diagnostics = Vec::with_capacity(20);

        while self.peek().is_ok() {
            match self.ast() {
                Ok((node, diag)) => {
                    ast.push(node);
                    diagnostics.extend_from_slice(&diag);
                }

                Err(diag) => {
                    use codespan_reporting::diagnostic::Severity;

                    diagnostics.extend_from_slice(&diag);

                    if diag.last().map(|d| d.severity) == Some(Severity::Error)
                        || diag.last().map(|d| d.severity) == Some(Severity::Bug)
                    {
                        return Err(diagnostics);
                    }
                }
            }
        }

        Ok((ast, diagnostics))
    }

    // TODO: Source own lexer, logos is slow on compile times, maybe something generator-based
    //       whenever those stabilize?
    pub fn lex(source: &'a str) -> (TokenStream<'a>, Option<Token<'a>>, Option<Token<'a>>) {
        let mut token_stream = TokenStream::new(source, true);
        let next = None;
        let peek = token_stream.next_token();

        (token_stream, next, peek)
    }
}

/// Utility functions
impl<'a> Parser<'a> {
    #[inline(always)]
    fn next(&mut self) -> Result<Token<'a>, Vec<Diagnostic<usize>>> {
        let mut next = self.token_stream.next_token();
        mem::swap(&mut next, &mut self.peek);
        self.next = next;

        next.ok_or(vec![
            Diagnostic::error().with_message("Unexpected End Of File")
        ])
    }

    #[inline(always)]
    fn peek(&self) -> Result<Token<'a>, Vec<Diagnostic<usize>>> {
        self.peek.ok_or(vec![
            Diagnostic::error().with_message("Unexpected End Of File")
        ])
    }

    #[inline(always)]
    fn eat(&mut self, expected: TokenType) -> Result<Token<'a>, Vec<Diagnostic<usize>>> {
        let token = self.next()?;

        if token.ty() == expected {
            Ok(token)
        } else {
            Err(vec![Diagnostic::error()
                .with_message(format!(
                    "Unexpected Token: Expected '{}', found '{}'",
                    expected,
                    token.ty()
                ))
                .with_labels(vec![Label::primary(
                    self.current_file,
                    token.range(),
                )
                .with_message(format!("Expected {}", expected))])])
        }
    }

    #[inline(always)]
    fn current_precedence(&self) -> usize {
        self.peek
            .map(|p| {
                BinaryPrecedence::try_from(p.ty())
                    .map(|p| p.precedence())
                    .unwrap_or(0)
            })
            .unwrap_or(0)
    }

    #[inline(always)]
    fn intern_string(&self, string: &str) -> Sym {
        intern_string(&self.string_interner, string)
    }

    #[inline(always)]
    fn eat_ident(&mut self) -> Result<Sym, Vec<Diagnostic<usize>>> {
        let token = self.eat(TokenType::Ident)?;
        Ok(self.intern_string(token.source()))
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
            val < u64::max_value() as usize,
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

// Attempts to not acquire a write lock on the interner unless it has to
fn intern_string(interner: &Interner, string: &str) -> Sym {
    if let Some(sym) = interner.read().get(string) {
        return sym;
    }

    interner.write().get_or_intern(string)
}

#[derive(Debug, Copy, Clone, PartialEq)]
#[rustfmt::skip]
pub enum BinaryPrecedence {
    Mul, Div, Mod, Pow,
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
            Self::Mul | Self::Div | Self::Mod | Self::Pow => 11,
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
            TokenType::DoubleStar => Self::Pow,
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
            TokenType::Equal
            | TokenType::AddAssign
            | TokenType::SubAssign
            | TokenType::MultAssign
            | TokenType::DivAssign
            | TokenType::ModAssign
            | TokenType::ShlAssign
            | TokenType::ShrAssign
            | TokenType::OrAssign
            | TokenType::AndAssign
            | TokenType::XorAssign => Self::Assignment,
            TokenType::If => Self::Ternary,

            _ => return Err(()),
        })
    }
}
