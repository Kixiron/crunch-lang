use crate::token::{Token, TokenStream, TokenType};
use alloc::{format, vec::Vec};
use core::mem;
use crunch_shared::{
    context::Context,
    error::{Error, ErrorHandler, Locatable, Location, ParseResult, SyntaxError},
    files::CurrentFile,
    trees::ast::Item,
};

mod expr;
mod item;
mod patterns;
mod stmt;
mod string_escapes;
mod types;
mod utils;

use utils::StackGuard;

pub type ParserReturn<'ctx> = (Vec<&'ctx Item<'ctx>>, ErrorHandler);

// TODO: Make the parser a little more lax, it's kinda strict about whitespace

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct ParseConfig {
    pub max_errors: usize,
    #[doc(hidden)]
    pub __private: (),
}

impl Default for ParseConfig {
    fn default() -> Self {
        Self {
            max_errors: 200,
            __private: (),
        }
    }
}

#[derive(Debug)]
pub struct Parser<'src, 'ctx> {
    token_stream: TokenStream<'src>,
    next: Option<Token<'src>>,
    peek: Option<Token<'src>>,
    error_handler: ErrorHandler,
    stack_frames: StackGuard,
    current_file: CurrentFile,
    context: &'ctx Context<'ctx>,
    config: ParseConfig,
}

/// Initialization and high-level usage
impl<'src, 'ctx> Parser<'src, 'ctx> {
    // TODO: Take in a `ParseConfig`
    pub fn new(
        source: &'src str,
        config: ParseConfig,
        current_file: CurrentFile,
        context: &'ctx Context<'ctx>,
    ) -> Self {
        let (token_stream, next, peek) = Self::lex(source);

        Self {
            token_stream,
            next,
            peek,
            error_handler: ErrorHandler::new(),
            stack_frames: StackGuard::new(),
            current_file,
            context,
            config,
        }
    }

    pub fn from_tokens(
        token_stream: TokenStream<'src>,
        next: Option<Token<'src>>,
        peek: Option<Token<'src>>,
        current_file: CurrentFile,
        context: &'ctx Context<'ctx>,
    ) -> Self {
        Self {
            token_stream,
            next,
            peek,
            error_handler: ErrorHandler::new(),
            stack_frames: StackGuard::new(),
            current_file,
            context,
            config: ParseConfig::default(),
        }
    }

    pub fn parse(mut self) -> Result<ParserReturn<'ctx>, ErrorHandler> {
        let (mut items, mut errors) = (Vec::with_capacity(20), 0);

        while self.peek().is_ok() {
            match self.item() {
                Ok(node) => {
                    if let Some(node) = node {
                        items.push(node);
                    }
                }

                Err(err) => {
                    errors += 1;

                    if errors >= self.config.max_errors {
                        let loc = err.loc;
                        self.error_handler.push_err(err);
                        self.error_handler.push_err(Locatable::new(
                            Error::Syntax(SyntaxError::TooManyErrors(errors)),
                            loc,
                        ));

                        return Err(self.error_handler);
                    }

                    self.error_handler.push_err(err);
                    if self.stress_eat().is_err() {
                        return Err(self.error_handler);
                    }
                }
            }
        }

        if self.error_handler.is_fatal() {
            Err(self.error_handler)
        } else {
            Ok((items, self.error_handler))
        }
    }

    pub fn lex(source: &'src str) -> (TokenStream<'src>, Option<Token<'src>>, Option<Token<'src>>) {
        let mut token_stream = TokenStream::new(source, true, true);
        let next = None;
        let peek = token_stream.next();

        (token_stream, next, peek)
    }

    pub fn with_config(mut self, config: ParseConfig) -> Self {
        self.config = config;
        self
    }

    #[inline(always)]
    pub fn error_handler_mut(&mut self) -> &mut ErrorHandler {
        &mut self.error_handler
    }
}

/// Utility functions
impl<'src, 'ctx> Parser<'src, 'ctx> {
    #[inline(always)]
    fn next(&mut self) -> ParseResult<Token<'src>> {
        let mut next = self.token_stream.next();
        mem::swap(&mut next, &mut self.peek);
        self.next = next;

        next.ok_or_else(|| Locatable::new(Error::EndOfFile, self.current_file.eof()))
    }

    #[inline(always)]
    fn peek(&self) -> ParseResult<Token<'src>> {
        self.peek
            .ok_or_else(|| Locatable::new(Error::EndOfFile, self.current_file.eof()))
    }

    /// Eats one of the `expected` token, ignoring (and consuming) any tokens included in `ignoring`
    #[inline(always)]
    fn eat<T>(&mut self, expected: TokenType, ignoring: T) -> ParseResult<Token<'src>>
    where
        T: AsRef<[TokenType]>,
    {
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
                        Location::new(&token, self.current_file.file()),
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
        let expected = expected.as_ref();
        let ignoring = ignoring.as_ref();
        let mut token = self.next()?;

        // Assert that the two slices don't share any elements, as that's likely a dev error
        debug_assert!(
            ignoring.iter().all(|i| !expected.contains(i)),
            "Ignored set contains expected token: {:?}, {:?}",
            ignoring,
            expected,
        );

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
                            "Expected one of {}, got {:?}",
                            expected,
                            token.source()
                        ))),
                        Location::new(&token, self.current_file.file()),
                    ));
                }
            }

            token = self.next()?;
        }
    }

    #[inline(always)]
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
                self.current_file.eof(),
            ))
        } else {
            Ok(guard)
        }
    }
}
