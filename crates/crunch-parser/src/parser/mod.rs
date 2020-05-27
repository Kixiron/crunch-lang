use crate::{
    context::Context,
    error::{Error, ErrorHandler, Locatable, Location, ParseResult, SyntaxError},
    symbol_table::{Graph, MaybeSym, NodeId, Scope},
    token::{Token, TokenStream, TokenType},
};

use alloc::{format, vec::Vec};
use core::mem;
#[cfg(feature = "logging")]
use log::{info, trace};

mod ast;
mod expr;
mod patterns;
mod stmt;
mod string_escapes;
#[cfg(test)]
mod tests;
mod types;
mod utils;

pub use ast::{
    Alias, Ast, Attribute, Decorator, Enum, EnumVariant, ExtendBlock, FuncArg, Function, Import,
    ImportDest, ImportExposure, Trait, TypeDecl, TypeMember, Visibility,
};
pub use expr::{
    AssignmentType, BinaryOperand, ComparisonOperand, Expr, Float, Integer, Literal, Rune, Sign,
    Text, UnaryOperand,
};
pub use patterns::{Binding, Pattern};
pub use stmt::Stmt;
pub use types::{Signedness, Type};
pub use utils::{CurrentFile, ItemPath};

use utils::StackGuard;

type ReturnData<'ctx> = (ErrorHandler, Graph<Scope<'ctx>, MaybeSym>, NodeId);

// TODO: Make the parser a little more lax, it's kinda strict about whitespace

#[derive(Debug)]
pub struct Parser<'src, 'cxl, 'ctx> {
    token_stream: TokenStream<'src>,
    next: Option<Token<'src>>,
    peek: Option<Token<'src>>,
    error_handler: ErrorHandler,
    stack_frames: StackGuard,
    current_file: CurrentFile,
    context: &'cxl Context<'ctx>,
    symbol_table: Graph<Scope<'ctx>, MaybeSym>,
    module_scope: NodeId,
}

/// Initialization and high-level usage
impl<'src, 'cxl, 'ctx> Parser<'src, 'cxl, 'ctx> {
    pub fn new(source: &'src str, current_file: CurrentFile, context: &'cxl Context<'ctx>) -> Self {
        let (token_stream, next, peek) = Self::lex(source);
        let mut symbol_table = Graph::new();
        let module_scope = symbol_table
            .push_with_capacity(Scope::LocalScope(Vec::new(), Vec::with_capacity(10)), 10);

        Self {
            token_stream,
            next,
            peek,
            error_handler: ErrorHandler::new(),
            stack_frames: StackGuard::new(),
            current_file,
            context,
            symbol_table,
            module_scope,
        }
    }

    pub fn from_tokens(
        token_stream: TokenStream<'src>,
        next: Option<Token<'src>>,
        peek: Option<Token<'src>>,
        current_file: CurrentFile,
        context: &'cxl Context<'ctx>,
    ) -> Self {
        let mut symbol_table = Graph::new();
        let module_scope = symbol_table
            .push_with_capacity(Scope::LocalScope(Vec::new(), Vec::with_capacity(10)), 10);

        Self {
            token_stream,
            next,
            peek,
            error_handler: ErrorHandler::new(),
            stack_frames: StackGuard::new(),
            current_file,
            context,
            symbol_table,
            module_scope,
        }
    }

    pub fn parse(mut self) -> Result<ReturnData<'ctx>, ErrorHandler> {
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

        #[cfg(feature = "logging")]
        info!("Finished parsing successfully");
        Ok((self.error_handler, self.symbol_table, self.module_scope))
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

    #[inline(always)]
    pub fn error_handler_mut(&mut self) -> &mut ErrorHandler {
        &mut self.error_handler
    }
}

/// Utility functions
impl<'src, 'cxl, 'ctx> Parser<'src, 'cxl, 'ctx> {
    #[inline(always)]
    fn next(&mut self) -> ParseResult<Token<'src>> {
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
                self.current_file.recursion(),
            ))
        } else {
            Ok(guard)
        }
    }
}
