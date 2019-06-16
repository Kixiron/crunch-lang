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
}

pub struct Parser<'source> {
    /// A peekable stored iterator
    token_stream: Box<Vec<TokenData<'source>>>,
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
            token_stream: Box::new(token_stream.collect::<Vec<TokenData>>()),
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
                self.current = token.clone();

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
            let (current_index, allow_wrapping) = self.current_index.overflowing_add(1);
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
    fn next_checked<F>(&mut self, callback: F) -> Expr
    where
        F: FnOnce(&mut Parser<'source>) -> Expr,
    {
        use crunch_token::Token::WhiteSpace;

        // If there is a next token
        if let Some(token) = self.next() {
            // If the token is whitespace, call next_checked recursively until a token that is not whitespace is found
            if token.kind() == WhiteSpace {
                self.next_checked(callback)

            // If the token is not whitespace, set the current token and execute the callback
            } else {
                // Execute the callback
                callback(self)
            }

        // If there are no more tokens, the end of the file has been reached
        } else {
            println!("End of file reached");
            self.end_of_file = true;
            Expr::EndOfFile
        }
    }

    // Current usage is broken
    fn peek_checked<F>(&mut self, callback: F) -> Expr
    where
        F: FnOnce(&mut Parser<'source>, TokenData<'source>) -> Expr,
    {
        use crunch_token::Token::WhiteSpace;

        // Weird workaround I had to do to dereference and clone the inner TokenData
        // so that there were not two mutable references of the Parser active
        let peek = if let Some(token) = self.peek() {
            Some(token.clone())
        } else {
            None
        };

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

/// Evaluate an Expr
// TODO: This function is absolutely bogged down by the manual checking of valid `next` calls,
// Something needs to be done to either pass that onto a separate funcion or a macro
// TODO: Use peek() more often and consume only when needed
// TODO: See if manual indexing is faster than iteration
impl<'source> Parser<'source> {
    fn eval_expr(&mut self, token: &TokenData<'source>, mut tree: &mut Vec<Expr>) -> Expr {
        use crunch_token::Token;
        use parsers::*;

        // Match the token's kind and parse recursively based on that
        match token.kind() {
            // Literals
            Token::IntLiteral
            | Token::FloatLiteral
            | Token::StrLiteral
            | Token::BoolLiteral
            | Token::Null
            | Token::VectorLiteral => Expr::Literal(literal(self, token, &mut tree)),

            // Binary Operators
            Token::Multiply
            | Token::Divide
            | Token::Plus
            | Token::Minus
            | Token::Or
            | Token::And
            | Token::Not => binary_operation(self, token, &mut tree),

            // Variable declarations
            Token::Variable => variable(self, token, &mut tree),

            // Variable types
            Token::Int
            | Token::Str
            | Token::Bool
            | Token::Vector => variable_type(self, token, &mut tree),

            Token::For => self.next_checked(|parser: &mut Parser<'source>| -> Expr {
                if parser.current.kind() == Token::Identifier {
                    let item = parser.current.source().to_owned();

                    parser.next_checked(|parser: &mut Parser<'source>| -> Expr {
                        if token.kind() == Token::In {
                            parser.next_checked(|parser: &mut Parser<'source>| -> Expr {
                                let collection = Box::new(parser.eval_expr(&parser.current.clone(), &mut tree));

                                parser.next_checked(|parser: &mut Parser<'source>| -> Expr {
                                    Expr::For {
                                        item,
                                        collection,
                                        body: Box::new(parser.eval_expr(&parser.current.clone(), &mut tree)),
                                    }
                                })
                            })
                        } else {
                            // If the token is not an identifier, return an invalid Expr
                            return Expr::Invalid("A `for .. in` binding was used, but no identifer or variable name was supplied".to_string(), token.range());
                        }
                    })
                } else {
                    // If the token is not an identifier, return an invalid Expr
                    return Expr::Invalid("A `for` binding was used, but no identifer or variable name was supplied".to_string(), token.range());
                }
            }),

            Token::While => self.next_checked(|parser: &mut Parser<'source>| -> Expr {
                Expr::While(
                    Box::new(match tree.pop() {
                        Some(condition) => condition,
                        None => Expr::Invalid(
                            "No left-hand side was provided to the while loop!".to_string(),
                            parser.current.range(),
                        ),
                    }),
                    Box::new(parser.eval_expr(&parser.current.clone(), &mut tree)),
                )
            }),

            Token::Loop => self.next_checked(|parser: _| -> Expr {
                Expr::Loop(Box::new(parser.eval_expr(&parser.current.clone(), &mut tree)))
            }),

            // FIXME: The if's are broken
            Token::If => self.next_checked(|parser: _| -> Expr {
                let condition = Box::new(parser.eval_expr(&parser.current.clone(), &mut tree));

                parser.next_checked(|parser: _| -> Expr {
                    let body = if parser.current.kind() != Token::LeftBrace {
                        // If the token after the condition is not a `{`, then consume the next token and replace it with an Invalid
                        let previous = parser.current.range();

                        Box::new(parser.next_checked(|_parser: _| -> Expr {
                            Expr::Invalid("Expected a {".to_string(), previous)
                        }))
                    } else {
                        // If the token is a `{`, then consume the next token and evaluate it, storing it in body
                        Box::new(parser.next_checked(|parser: _| -> Expr {
                           parser.eval_expr(&parser.current.clone(), &mut tree)
                        }))
                    };

                    parser.next_checked(|parser: _| -> Expr {
                        if parser.current.kind() == Token::RightBrace {
                            parser.peek_checked(|parser: _, peeked: _| -> Expr {
                                // If the token is
                                if  peeked.kind() == Token::ElseIf
                                    ||  peeked.kind() == Token::Else
                                {
                                    let token = parser.next().expect("The peeked token does not exist!");

                                    Expr::If {
                                        condition,
                                        body,
                                        continuation: Some(Box::new(parser.eval_expr(&token, &mut tree))),
                                    }
                                } else if parser.current.kind() == Token::RightBrace {
                                    parser.next().expect("The peeked token does not exist!");

                                    parser.peek_checked(|parser: _, peeked: _| -> Expr {
                                        if peeked.kind() == Token::ElseIf
                                            || peeked.kind() == Token::Else
                                        {
                                            let token = parser.next().expect("The peeked token does not exist!");

                                            Expr::If {
                                                condition,
                                                body,
                                                continuation: Some(Box::new(parser.eval_expr(&token, &mut tree))),
                                            }
                                        } else {
                                            Expr::If {
                                                condition,
                                                body,
                                                continuation: None,
                                            }
                                        }
                                    })
                                } else {
                                    Expr::If {
                                        condition,
                                        body,
                                        continuation: None,
                                    }
                                }
                            })
                        } else {
                            Expr::Invalid("Expected a `}`".to_string(), parser.current.range())
                        }
                    })
                })
            }),

            Token::ElseIf => {
                if let Some(token) = self.next() {
                    let condition = Box::new(self.eval_expr(&token, &mut tree));

                    if let Some(token) = self.next() {
                        let body = Box::new(self.eval_expr(&token, &mut tree));

                        if let Some(token) = self.peek() {
                            if token.kind() == Token::ElseIf || token.kind() == Token::Else {
                                let token = self.next().expect("The peeked token does not exist");

                                Expr::ElseIf {
                                    condition,
                                    body,
                                    continuation: Some(Box::new(self.eval_expr(&token, &mut tree))),
                                }
                            } else if token.kind() == Token::LeftBrace {
                                self.next().unwrap();
                                if let Some(token) = self.peek() {
                                    if token.kind() == Token::ElseIf || token.kind() == Token::Else
                                    {
                                        let token = self
                                            .next()
                                            .clone()
                                            .expect("The peeked token does not exist");

                                        Expr::ElseIf {
                                            condition,
                                            body,
                                            continuation: Some(Box::new(
                                                self.eval_expr(&token, &mut tree),
                                            )),
                                        }
                                    } else {
                                        Expr::ElseIf {
                                            condition,
                                            body,
                                            continuation: None,
                                        }
                                    }
                                } else {
                                    self.end_of_file = true;
                                    Expr::None
                                }
                            } else {
                                self.end_of_file = true;
                                Expr::None
                            }
                        } else {
                            self.end_of_file = true;
                            Expr::None
                        }
                    } else {
                        self.end_of_file = true;
                        Expr::None
                    }
                } else {
                    self.end_of_file = true;
                    Expr::None
                }
            }

            Token::Else => {
                if let Some(token) = self.next() {
                    if let Some(token) = self.next() {
                        Expr::Else {
                            body: Box::new(self.eval_expr(&token, &mut tree)),
                        }
                    } else {
                        self.end_of_file = true;
                        Expr::None
                    }
                } else {
                    self.end_of_file = true;
                    Expr::None
                }
            }

            Token::WhiteSpace
            | Token::Comment
            | Token::MultilineComment
            | Token::DocComment => {
                self.next_checked(|parser: _| -> Expr {
                    // Ignore the meaningless token and evaluate and return the next Expr instead
                    parser.eval_expr(&token, &mut tree)
                })
            }

            Token::EndOfFile => {
                self.end_of_file = true;
                Expr::EndOfFile
            }

            _ => Expr::Invalid(
                "The token is not currently supported".to_string(),
                token.range(),
            ),
        }
    }
}
