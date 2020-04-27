use crate::{
    error::{Error, ErrorHandler, Locatable, Location, ParseResult, Span, SyntaxError},
    files::FileId,
    interner::Interner,
    token::{Token, TokenStream, TokenType},
};

#[cfg(feature = "logging")]
use log::{info, trace};
use stadium::Stadium;

use alloc::{format, rc::Rc, vec::Vec};
use core::{convert::TryFrom, fmt, mem, num::NonZeroUsize, ops};

mod ast;
mod expr;
mod stmt;
mod string_escapes;
#[cfg(test)]
mod tests;

pub use ast::{
    Ast, AstType, Attribute, BuiltinType, Decorator, Enum, EnumVariant, FuncArg, Function, Import,
    ImportDest, ImportExposure, Signedness, Trait, Type, TypeDecl, TypeMember, Visibility,
};
pub use expr::{
    AssignmentType, BinaryOperand, ComparisonOperand, Expr, Expression, Float, Integer, Literal,
    Rune, Sign, Text, UnaryOperand,
};
pub use stmt::{Statement, Stmt};

// TODO: Make the parser a little more lax, it's kinda strict about whitespace

pub struct SyntaxTree<'expr, 'stmt> {
    pub(crate) ast: Vec<Ast<'expr, 'stmt>>,
    pub(crate) __exprs: Stadium<'expr, Expression<'expr>>,
    pub(crate) __stmts: Stadium<'stmt, Statement<'expr, 'stmt>>,
}

impl<'expr, 'stmt> fmt::Debug for SyntaxTree<'expr, 'stmt> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_list().entries(&self.ast).finish()
    }
}

impl<'expr, 'stmt> ops::Deref for SyntaxTree<'expr, 'stmt> {
    type Target = [Ast<'expr, 'stmt>];

    fn deref(&self) -> &Self::Target {
        &self.ast
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct CurrentFile {
    file: FileId,
    length: usize,
    index: usize,
}

impl CurrentFile {
    pub const fn new(file: FileId, length: usize) -> Self {
        Self {
            file,
            length,
            index: 0,
        }
    }

    pub const fn file(&self) -> FileId {
        self.file
    }

    pub const fn length(&self) -> usize {
        self.length
    }

    pub const fn index(&self) -> usize {
        self.index
    }

    pub fn eof(&self) -> Location {
        Location::concrete(Span::new(self.length, self.length), self.file)
    }

    pub fn advance(&mut self, dist: usize) {
        self.index += dist;
    }

    pub const fn index_span(&self) -> Span {
        Span::new(self.index, self.index)
    }

    pub fn recursion(&self) -> Location {
        Location::concrete(self.index_span(), self.file)
    }
}

impl Into<FileId> for CurrentFile {
    fn into(self) -> FileId {
        self.file
    }
}

pub struct Parser<'src, 'expr, 'stmt> {
    token_stream: TokenStream<'src>,
    next: Option<Token<'src>>,
    peek: Option<Token<'src>>,

    error_handler: ErrorHandler,
    stack_frames: StackGuard,
    current_file: CurrentFile,

    expr_arena: Stadium<'expr, Expression<'expr>>,
    stmt_arena: Stadium<'stmt, Statement<'expr, 'stmt>>,

    string_interner: Interner,
}

/// Initialization and high-level usage
impl<'src, 'expr, 'stmt> Parser<'src, 'expr, 'stmt> {
    pub fn new(source: &'src str, current_file: CurrentFile, string_interner: Interner) -> Self {
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

    pub fn from_tokens(
        token_stream: TokenStream<'src>,
        next: Option<Token<'src>>,
        peek: Option<Token<'src>>,
        current_file: CurrentFile,
        string_interner: Interner,
    ) -> Self {
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

    pub fn parse(
        mut self,
    ) -> Result<(SyntaxTree<'expr, 'stmt>, Interner, ErrorHandler), ErrorHandler> {
        #[cfg(feature = "logging")]
        info!("Started parsing");

        let mut ast = Vec::with_capacity(20);

        while self.peek().is_ok() {
            #[cfg(feature = "logging")]
            trace!("Parsing top-level token");

            match self.ast() {
                Ok(node) => {
                    if let Some(node) = node {
                        ast.push(node);
                    }
                }

                Err(err) => {
                    self.error_handler.push_err(err);

                    if let Err(err) = self.stress_eat() {
                        self.error_handler.push_err(err);

                        #[cfg(feature = "logging")]
                        info!("Finished parsing unsuccessfully");
                        return Err(self.error_handler);
                    }
                }
            }
        }

        let ast = SyntaxTree {
            ast,
            __exprs: self.expr_arena,
            __stmts: self.stmt_arena,
        };

        #[cfg(feature = "logging")]
        info!("Finished parsing successfully");
        Ok((ast, self.string_interner, self.error_handler))
    }

    pub fn lex(source: &'src str) -> (TokenStream<'src>, Option<Token<'src>>, Option<Token<'src>>) {
        #[cfg(feature = "logging")]
        info!("Started lexing");

        let mut token_stream = TokenStream::new(source, true, true);
        let next = None;
        let peek = token_stream.next_token();

        #[cfg(feature = "logging")]
        info!("Finished lexing");
        (token_stream, next, peek)
    }

    pub fn error_handler_mut(&mut self) -> &mut ErrorHandler {
        &mut self.error_handler
    }
}

/// Utility functions
impl<'src, 'expr, 'stmt> Parser<'src, 'expr, 'stmt> {
    #[inline(always)]
    fn next(&mut self) -> ParseResult<Token<'src>> {
        let _frame = self.add_stack_frame()?;
        let mut next = self.token_stream.next_token();
        mem::swap(&mut next, &mut self.peek);
        self.next = next;

        if let Some(next) = self.next {
            self.current_file.advance(next.span().width());
        }

        next.ok_or_else(|| Locatable::new(Error::EndOfFile, self.current_file.eof()))
    }

    #[inline(always)]
    fn peek(&self) -> ParseResult<Token<'src>> {
        let _frame = self.add_stack_frame()?;
        self.peek
            .ok_or_else(|| Locatable::new(Error::EndOfFile, self.current_file.eof()))
    }

    /// Eats one of the `expected` token, ignoring (and consuming) any tokens included in `ignoring`
    #[inline(always)]
    fn eat<T>(&mut self, expected: TokenType, ignoring: T) -> ParseResult<Token<'src>>
    where
        T: AsRef<[TokenType]>,
    {
        let _frame = self.add_stack_frame()?;

        let ignoring = ignoring.as_ref();
        let mut token = self.next()?;

        // Assert that the expected token is not ignored, as that's likely a dev error
        debug_assert!(!ignoring.contains(&expected));

        loop {
            match token.ty() {
                ty if ty == expected => return Ok(token),
                ignored if ignoring.contains(&ignored) => {}
                _ => {
                    return Err(Locatable::new(
                        Error::Syntax(SyntaxError::Generic(format!(
                            "Expected {}, got {}",
                            expected,
                            token.ty()
                        ))),
                        Location::concrete(&token, self.current_file.file()),
                    ));
                }
            }

            token = self.next()?;
        }
    }

    /// Eats one of the `expected` tokens, ignoring (and consuming) any tokens included in `ignoring`
    #[inline(always)]
    fn eat_of<T, E>(&mut self, expected: T, ignoring: E) -> ParseResult<Token<'src>>
    where
        T: AsRef<[TokenType]>,
        E: AsRef<[TokenType]>,
    {
        let _frame = self.add_stack_frame()?;

        let expected = expected.as_ref();
        let ignoring = ignoring.as_ref();
        let mut token = self.next()?;

        // Assert that the two slices don't share any elements, as that's likely a dev error
        debug_assert!(ignoring.iter().any(|i| !expected.contains(i)));

        loop {
            match token.ty() {
                ty if expected.contains(&ty) => return Ok(token),
                ignored if ignoring.contains(&ignored) => {}
                _ => {
                    let expected = expected
                        .iter()
                        .map(|t| format!("'{}'", t))
                        .collect::<Vec<_>>()
                        .join(", ");

                    return Err(Locatable::new(
                        Error::Syntax(SyntaxError::Generic(format!(
                            "Expected one of {}, got {}",
                            expected,
                            token.ty()
                        ))),
                        Location::concrete(&token, self.current_file.file()),
                    ));
                }
            }

            token = self.next()?;
        }
    }

    fn stress_eat(&mut self) -> ParseResult<()> {
        const TOP_TOKENS: &[TokenType] = &[
            TokenType::Function,
            TokenType::Enum,
            TokenType::AtSign,
            TokenType::Exposed,
            TokenType::Package,
            TokenType::Trait,
            TokenType::Type,
            TokenType::Import,
        ];

        while !TOP_TOKENS.contains(&self.peek()?.ty()) {
            self.next()?;
        }

        Ok(())
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

    fn add_stack_frame(&self) -> ParseResult<StackGuard> {
        // TODO: Find out what this number should be
        #[cfg(debug_assertions)]
        const MAX_DEPTH: usize = 50;
        #[cfg(not(debug_assertions))]
        const MAX_DEPTH: usize = 150;

        let guard = self.stack_frames.clone();
        let depth = guard.frames();

        if depth > MAX_DEPTH {
            Err(Locatable::new(
                Error::Syntax(SyntaxError::RecursionLimit(depth, MAX_DEPTH)),
                self.current_file.recursion(),
            ))
        } else {
            Ok(guard)
        }
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
            TokenType::Colon
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
