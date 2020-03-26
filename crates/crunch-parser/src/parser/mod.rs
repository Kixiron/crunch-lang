use crate::{
    error::{Error, ErrorHandler, FileId, Locatable, Location, ParseResult, SyntaxError},
    token::{Token, TokenStream, TokenType},
    Interner, Sym,
};

use alloc::{format, rc::Rc, vec::Vec};
use core::{convert::TryFrom, mem, num::NonZeroUsize, ops};
use stadium::Stadium;

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
    AssignmentType, BinaryOperand, ComparisonOperand, Expr, Expression, Literal, UnaryOperand,
};
pub use stmt::{Statement, Stmt};

// TODO: Make the parser a little more lax, it's kinda strict about whitespace

pub struct Parser<'a> {
    token_stream: TokenStream<'a>,
    next: Option<Token<'a>>,
    peek: Option<Token<'a>>,

    error_handler: ErrorHandler,
    stack_frames: StackGuard,
    current_file: FileId,

    expr_arena: Stadium<Expression<'a>>,
    stmt_arena: Stadium<Statement<'a>>,

    string_interner: Interner,
}

pub struct SyntaxTree<'a> {
    ast: Vec<Ast<'a>>,
    exprs: Stadium<Expression<'a>>,
    stmts: Stadium<Statement<'a>>,
}

impl<'a> ops::Deref for SyntaxTree<'a> {
    type Target = [Ast<'a>];

    fn deref(&self) -> &Self::Target {
        &self.ast
    }
}

/// Initialization and high-level usage
impl<'a> Parser<'a> {
    pub fn new(source: &'a str, current_file: FileId, string_interner: Interner) -> Self {
        let (token_stream, next, peek) = Self::lex(source);

        Self {
            token_stream,
            next,
            peek,

            error_handler: ErrorHandler::new(),
            stack_frames: StackGuard::new(),
            current_file,

            expr_arena: Stadium::with_capacity(NonZeroUsize::new(512).unwrap()),
            stmt_arena: Stadium::with_capacity(NonZeroUsize::new(512).unwrap()),

            string_interner,
        }
    }

    // TODO: Decide when an error has occurred, and allow returning of warnings
    // TODO: Should this consume? Is there a viable reason to re-parse?
    // TODO: Maybe consuming is alright, the token stream itself is consumed, so nothing really usable is
    //       left in the parser after this function is run on it
    pub fn parse(mut self) -> Result<(SyntaxTree<'a>, ErrorHandler), ErrorHandler> {
        let mut ast = Vec::with_capacity(20);

        while self.peek().is_ok() {
            match self.ast() {
                Ok(node) => {
                    if let Some(node) = node {
                        ast.push(node);
                    }
                }

                Err(err) => {
                    self.error_handler.push_err(err);

                    todo!("eat until next top-level token")
                }
            }
        }

        let ast = SyntaxTree {
            ast,
            exprs: self.expr_arena,
            stmts: self.stmt_arena,
        };

        Ok((ast, self.error_handler))
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
    fn next(&mut self) -> ParseResult<Token<'a>> {
        let _frame = self.add_stack_frame()?;
        let mut next = self.token_stream.next_token();
        mem::swap(&mut next, &mut self.peek);
        self.next = next;

        next.ok_or(Locatable::file(Error::EndOfFile, self.current_file))
    }

    #[inline(always)]
    fn peek(&self) -> ParseResult<Token<'a>> {
        let _frame = self.add_stack_frame()?;
        self.peek
            .ok_or(Locatable::file(Error::EndOfFile, self.current_file))
    }

    #[inline(always)]
    fn eat(&mut self, expected: TokenType) -> ParseResult<Token<'a>> {
        let _frame = self.add_stack_frame()?;
        let token = self.next()?;

        if token.ty() == expected {
            Ok(token)
        } else {
            Err(Locatable::new(
                Error::Syntax(SyntaxError::Generic(format!(
                    "Expected {}, got {}",
                    expected,
                    token.ty()
                ))),
                Location::new(&token, self.current_file),
            ))
        }
    }

    #[inline(always)]
    fn eat_of(&mut self, expected: &[TokenType]) -> ParseResult<Token<'a>> {
        let _frame = self.add_stack_frame()?;
        let token = self.next()?;

        if expected.contains(&token.ty()) {
            Ok(token)
        } else {
            let expected = expected
                .iter()
                .map(|t| format!("'{}'", t))
                .collect::<Vec<_>>()
                .join(", ");

            Err(Locatable::new(
                Error::Syntax(SyntaxError::Generic(format!(
                    "Expected one of {}, got {}",
                    expected,
                    token.ty()
                ))),
                Location::new(&token, self.current_file),
            ))
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
    fn eat_ident(&mut self) -> ParseResult<Sym> {
        let token = self.eat(TokenType::Ident)?;
        Ok(self.intern_string(token.source()))
    }

    fn add_stack_frame(&self) -> ParseResult<StackGuard> {
        // TODO: Find out what this number should be
        #[cfg(debug_assertions)]
        const MAX_DEPTH: usize = 50;
        #[cfg(not(debug_assertions))]
        const MAX_DEPTH: usize = 150;

        let guard = self.stack_frames.clone();
        let depth = guard.frames();
        if depth > MAX_DEPTH {
            return Err(Locatable::file(
                Error::Syntax(SyntaxError::RecursionLimit(depth, MAX_DEPTH)),
                self.current_file,
            ));
        }

        Ok(guard)
    }
}

#[derive(Debug, Clone)]
struct StackGuard(Rc<()>);

impl StackGuard {
    pub fn new() -> Self {
        Self(Rc::new(()))
    }

    pub fn frames(&self) -> usize {
        Rc::strong_count(&self.0)
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
