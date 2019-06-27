mod evaluate_expression;
pub mod expression;
mod parsers;

use crunch_token::{TokenData, TokenStream};
use shrinkwraprs::Shrinkwrap;

pub use expression::*;

/// The abstract syntax tree class
#[derive(Shrinkwrap, Debug)]
pub struct Ast(Vec<Expr>);

impl Ast {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn into_inner(self) -> Vec<Expr> {
        self.0
    }
}

impl Default for Ast {
    fn default() -> Self {
        Self(Vec::new())
    }
}

pub struct Parser<'source> {
    /// A peekable stored iterator
    token_stream: Vec<TokenData<'source>>,
    /// The current token
    current: TokenData<'source>,
    /// Whether or not the end of the file has been reached and the parsing should terminate
    end_of_file: bool,
    /// The index of the current token
    current_index: usize,
    /// Whether wrapped adding is allowed for the next() function
    /// See [`next`] for more detail
    allow_wrapping: bool,
}

/// The public interfaces of the Parser
impl<'source> Parser<'source> {
    pub fn new(token_stream: TokenStream<'source>) -> Self {
        Self {
            token_stream: token_stream.collect::<Vec<TokenData>>(),
            current: TokenData {
                kind: crunch_token::Token::WhiteSpace,
                source: " ",
                range: (0, 0),
            },
            end_of_file: false,
            current_index: usize::max_value(),
            allow_wrapping: true,
        }
    }

    /// Create an Abstract Syntax Tree from the token stream
    // TODO: Find out if a continual yielding of nodes is possible by using generators,
    // then feeding the generator to the evaluator in order to speed up interpretation time
    pub fn parse(mut self) -> Ast {
        // Create the parse tree's vector
        let mut tree = Vec::new();

        // While tokens are being supplied from the token stream
        while let Some(token) = self.next() {
            // While the end of file has not been reached
            if !self.end_of_file {
                // Fill current
                self.current = token;

                // Evaluate the current token as an Expr
                let expr = self.eval_expr(&token, &mut tree);
                // Push the Expr to the tree
                tree.push(expr);
            }
        }

        // Return the parse tree
        Ast(tree)
    }
}

/// The internal methods of the parser class
impl<'source> Parser<'source> {
    /// Get the next token while changing the current index
    fn next(&mut self) -> Option<TokenData<'source>> {
        // This whole block is for making the initialization of Parser as janky as possible.
        // If allow_wrapping is true, add 1 to the current index, which should be the maximum value of usize.
        // The add will purposefully overflow to 0, which happens to be the index of the first element in
        // the token_stream. After this happens, allow_wrapping is disabled and so that behavior never happens again.
        // After allow_wrapping is disabled, current_index is incremented by one as normal
        if self.allow_wrapping {
            let (current_index, allow_wrapping) =
                self.current_index.overflowing_add(1);
            self.allow_wrapping = allow_wrapping;
            self.current_index = current_index;
        } else {
            self.current_index += 1;
        }

        // If the requested index is invalid, return None
        if self.current_index > self.token_stream.len() - 1 {
            None

        // If the requested index is valid, return the token at the current index
        } else {
            self.current = self.token_stream[self.current_index];

            Some(self.token_stream[self.current_index])
        }
    }

    /// Peek the token at `current_index` + 1 without changing the Parser state
    fn peek(&mut self) -> Option<TokenData<'source>> {
        // This whole block is for making the initialization of Parser as janky as possible.
        // If allow_wrapping is true, add 1 to the current index, which should be the maximum value of usize.
        // The add will purposefully overflow to 0, which happens to be the index of the first element in
        // the token_stream. After this happens, allow_wrapping is disabled and so that behavior never happens again.
        // After allow_wrapping is disabled, current_index is incremented by one as normal
        let index = if self.allow_wrapping {
            let (index, allow_wrapping) = self.current_index.overflowing_add(1);
            self.allow_wrapping = allow_wrapping;
            index + 1 // Wraps to 0 but the peek should be on 1
        } else {
            self.current_index + 1
        };

        // If the requested index is invalid, return None
        if index > self.token_stream.len() - 1 {
            None

        // Else return the token at the requested index
        } else {
            Some(self.token_stream[index])
        }
    }

    // Gets the next token that is not whitespace
    fn next_checked<S, F>(
        &mut self,
        callback_success: S,
        target_token: Option<crunch_token::Token>,
        callback_failure: Option<Box<F>>,
    ) -> Expr
    where
        S: FnOnce(&mut Parser<'source>) -> Expr,
        F: FnOnce(&mut Parser<'source>) -> Expr + ?Sized,
    {
        use crunch_token::Token::WhiteSpace;

        if target_token.is_some() && callback_failure.is_none() {
            panic!("You must have a failure callback when there is a target token!");
        }

        // If there is a next token
        if let Some(token) = self.next() {
            // If the token is whitespace, call next_checked recursively until a token that is not whitespace is found
            if token.kind() == WhiteSpace {
                self.next_checked(
                    callback_success,
                    target_token,
                    callback_failure,
                )

            // If the token is not whitespace and the token is the correct type, execute the success callback
            } else if target_token.is_some()
                && Some(token.kind()) == target_token
            {
                // Execute the callback
                callback_success(self)

            // If not, execute the failure callback
            } else if let Some(callback_failure) = callback_failure {
                callback_failure(self)

            // Else execute the callback
            } else {
                callback_success(self)
            }

        // If there are no more tokens, the end of the file has been reached
        } else {
            self.end_of_file = true;
            Expr::EndOfFile
        }
    }

    // Current usage is broken
    fn peek_checked<'a, F>(&mut self, callback: F) -> Expr
    where
        F: FnOnce(&mut Parser<'source>, TokenData<'source>) -> Expr,
    {
        use crunch_token::Token::WhiteSpace;

        // Weird workaround I had to do to dereference and clone the inner TokenData
        // so that there were not two mutable references of the Parser active
        let peek =
            if let Some(token) = self.peek() { Some(token) } else { None };

        if let Some(token) = peek {
            // If the token is whitespace, call peek_checked recursively until a token that is not whitespace is found
            if token.kind() == WhiteSpace {
                self.peek_checked(callback)

            // If the token is not whitespace, execute the callback on it
            } else {
                // Execute the callback
                callback(self, token)
            }

        // If there are no more tokens, the end of the file has been reached
        } else {
            self.end_of_file = true;
            Expr::EndOfFile
        }
    }
}
