use crate::{
    error::{Error, ErrorHandler, Locatable, Location, ParseResult, SyntaxError},
    interner::Interner,
    token::{Token, TokenStream, TokenType},
};

#[cfg(feature = "logging")]
use log::{info, trace};
use stadium::Stadium;

use alloc::{format, vec::Vec};
use core::{convert::TryFrom, fmt, mem, num::NonZeroUsize};

mod ast;
mod expr;
mod stmt;
mod string_escapes;
#[cfg(test)]
mod tests;
mod utils;

pub use ast::{
    Ast, Attribute, BuiltinType, Decorator, Enum, EnumVariant, FuncArg, Function, Import,
    ImportDest, ImportExposure, Signedness, Trait, Type, TypeDecl, TypeMember, Visibility,
};
pub use expr::{
    AssignmentType, BinaryOperand, ComparisonOperand, Expr, Expression, Float, Integer, Literal,
    Rune, Sign, Text, UnaryOperand,
};
pub use stmt::{Statement, Stmt};
pub use utils::{CurrentFile, SyntaxTree};

use utils::{BinaryPrecedence, StackGuard};

// TODO: Make the parser a little more lax, it's kinda strict about whitespace

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
                            "Expected {:?}, got {:?}",
                            expected.to_str(),
                            token.source()
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
                        .map(|t| format!("{:?}", t.to_str()))
                        .collect::<Vec<_>>()
                        .join(", ");

                    return Err(Locatable::new(
                        Error::Syntax(SyntaxError::Generic(format!(
                            "Expected one of {:?}, got {:?}",
                            expected,
                            token.source()
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

    #[inline(always)]
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

impl fmt::Debug for Parser<'_, '_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Parser")
            .field("token_stream", &self.token_stream)
            .field("next", &self.next)
            .field("peek", &self.peek)
            .field("error_handler", &self.error_handler)
            .field("stack_frames", &self.stack_frames)
            .field("current_file", &self.current_file)
            .field("string_interner", &self.string_interner)
            .finish()
    }
}
