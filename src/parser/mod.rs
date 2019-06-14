pub mod expression;
mod parsers;

use crate::crunch::Crunch;
use crunch_token::{TokenData, TokenStream};
pub use expression::*;
use std::iter::Peekable;

pub struct Parser<'crunch, 'source> {
    /// A peekable stored iterator
    // token_stream: Peekable<Box<dyn Iterator<Item = TokenData<'source>> + 'source>>,
    token_stream: Box<Vec<TokenData<'source>>>,
    /// A mutable reference to the crunch instance that spawned the parse
    crunch: &'crunch mut Crunch,
    /// The current token
    current: TokenData<'source>,
    /// Whether or not the end of the file has been reached and the parsing should terminate
    end_of_file: bool,
    current_index: usize,
    allow_wrapping: bool,
}

/// The public interfaces of the Parser
impl<'crunch, 'source> Parser<'crunch, 'source> {
    pub fn new(token_stream: TokenStream<'source>, crunch: &'crunch mut Crunch) -> Self {
        // // Workaround to appease type inference
        // let iter: Box<Iterator<Item = TokenData<'source>>> = Box::new(token_stream.into_iter());
        // Self {
        //     token_stream: iter.peekable(),
        //     crunch,
        //     // Kinda hacky, the token loop operates off of each token_stream.next(),
        //     // so it will skip the token loaded into `current` here
        //     current: TokenData {
        //         kind: crunch_token::Token::WhiteSpace,
        //         source: " ",
        //         range: (0, 0),
        //     },
        //     end_of_file: false,
        // }

        Self {
            token_stream: Box::new(token_stream.collect::<Vec<TokenData>>()),
            crunch,
            current: TokenData {
                kind: crunch_token::Token::WhiteSpace,
                source: " ",
                range: (0, 0),
            },
            end_of_file: false,
            current_index: 18446744073709551615,
            allow_wrapping: true,
        }
    }

    /// Create an Abstract Syntax Tree from the token stream
    // TODO: Find out if a continual yielding of nodes is possible by using generators,
    // then feeding the generator to the evaluator in order to speed up interpretation time
    pub fn parse(mut self) -> Vec<Expression> {
        // Remove whitespace and comments from the token stream
        // TODO: Find out if parse-time removal/ignorance is faster
        self.remove_whitespace();
        self.remove_comments();

        // Create the parse tree's vector
        let mut tree = Vec::new();

        // While tokens are being supplied from the token stream
        while let Some(token) = self.next() {
            // While the end of file has not been reached
            if !self.end_of_file {
                // Fill current
                self.current = token.clone();

                // Evaluate the current token as an expression
                let expr = self.eval_expr(&token, &mut tree);
                // Push the expression to the tree
                tree.push(expr);
            }
        }

        // tree.retain(|node| node != &Expression::None); // Remove any `None` from the tree

        // Return the parse tree
        tree
    }
}

/// The internal methods of the parser class
impl<'crunch, 'source> Parser<'crunch, 'source> {
    fn remove_whitespace(&mut self) {
        use crunch_token::Token::WhiteSpace;

        self.token_stream.retain(|token| token.kind() != WhiteSpace);

        // let iter: Box<Iterator<Item = TokenData<'source>>> = Box::new(
        //     self.token_stream
        //         .filter(|token| token.kind() != WhiteSpace)
        //         .into_iter(),
        // );
        //
        // Self {
        //     token_stream: iter.peekable(),
        //     crunch: self.crunch,
        //     current: self.current,
        //     end_of_file: self.end_of_file,
        // }
    }

    fn remove_comments(&mut self) {
        use crunch_token::Token::{Comment, DocComment, MultilineComment};

        // let mut token_stream = self.token_stream.collect::<Vec<TokenData<'source>>>();
        self.token_stream.retain(|token| {
            token.kind() != Comment
                && token.kind() != MultilineComment
                && token.kind() != DocComment
        });

        // let iter: Box<Iterator<Item = TokenData<'source>>> = Box::new(token_stream.into_iter());
        //
        // Self {
        //     token_stream: iter.peekable(),
        //     crunch: self.crunch,
        //     current: self.current,
        //     end_of_file: self.end_of_file,
        // }
    }

    // This is quite possibly the stupidest thing I've ever written
    fn next(&mut self) -> Option<TokenData<'source>> {
        if self.allow_wrapping {
            let (current_index, allow_wrapping) = self.current_index.overflowing_add(1);
            self.allow_wrapping = allow_wrapping;
            self.current_index = current_index;
        } else {
            self.current_index += 1;
        }

        if self.current_index > self.token_stream.len() - 1 {
            None
        } else {
            Some(self.token_stream[self.current_index])
        }
    }

    // FIXME: Ownership tussle
    fn peek(&mut self) -> Option<&TokenData<'source>> {
        if self.current_index + 1 > self.token_stream.len() - 1 {
            None
        } else {
            Some(&self.token_stream[self.current_index + 1])
        }
    }

    fn next_checked<F>(&mut self, callback: F) -> Expression
    where
        F: FnOnce(&mut Parser<'crunch, 'source>) -> Expression,
    {
        // If there is a next token
        if let Some(token) = self.next() {
            // Set the current token
            self.current = token;

            // Execute the callback
            callback(self)

        // If there are no more tokens, the end of the file has been reached
        } else {
            self.end_of_file = true;
            Expression::EndOfFile
        }
    }

    // Current usage is broken
    fn peek_checked<F>(&mut self, callback: F) -> Expression
    where
        F: FnOnce(&mut Parser<'crunch, 'source>, TokenData<'source>) -> Expression,
    {
        // Weird workaround I had to do to dereference and clone the inner TokenData
        // so that there were not two mutable references of the Parser active
        let peek = if let Some(token) = self.peek() {
            Some((*token).clone())
        } else {
            None
        };

        if let Some(token) = peek {
            // Execute the callback
            callback(self, token)

        // If there are no more tokens, the end of the file has been reached
        } else {
            self.end_of_file = true;
            Expression::EndOfFile
        }
    }
}

/// Evaluate an expression
// TODO: This function is absolutely bogged down by the manual checking of valid `next` calls,
// Something needs to be done to either pass that onto a separate funcion or a macro
// TODO: Use peek() more often and consume only when needed
// TODO: See if manual indexing is faster than iteration
impl<'crunch, 'source> Parser<'crunch, 'source> {
    fn eval_expr(
        &mut self,
        token: &TokenData<'source>,
        mut tree: &mut Vec<Expression>,
    ) -> Expression {
        use crunch_token::Token;
        use parsers::*;

        // Match the token's kind and parse recursively based on that
        match token.kind() {
            Token::IntLiteral => parse_int_token(self, token, &mut tree),

            Token::FloatLiteral => parse_float_token(self, token, &mut tree),

            Token::StrLiteral => {
                // The returned source string includes the quotes,
                // so the indexing is to remove the leading and trailing quotes
                Expression::StringLiteral(token.source()[1..token.source().len() - 1].to_owned())
            }

            // These directly transfer
            Token::Null => Expression::Null,
            Token::Bool => Expression::Bool,
            Token::Vector => Expression::Vec,
            Token::Str => Expression::Str,
            Token::Int => Expression::Int,

            // Multiplication grammar
            // (Expression -> Integer) * (Expression -> Integer) -> Value
            Token::Multiply => {
                // If there is a next token, continue
                if let Some(token) = self.next() {
                    // Create the multiplication token
                    Expression::Multiplication(
                        // The left-hand side of the operation is the last expression in the expression tree
                        Box::new(match tree.pop() {
                            // If there is a last expression, insert it into the
                            Some(expr) => expr,
                            // If there is no last expression, return an invalid expression
                            None => Expression::Invalid(
                                "No left-hand side was provided to the multiplication operation!"
                                    .to_string(),
                                token.range(),
                            ),
                        }),
                        // Populate the right-hand side with the next expression
                        Box::new(self.eval_expr(&token, &mut tree)),
                    )
                // If there is no next token, then the end of the file has been reached
                } else {
                    self.end_of_file = true;
                    Expression::EndOfFile
                }
            }

            // The following mathematical operations all follow the same structure as the above, so refer to it for clarification
            // TODO: Don't be lazy and fully comment all of them

            // Division grammar
            // (Expression -> Integer) / (Expression -> Integer) -> Value
            Token::Divide => {
                if let Some(token) = self.next() {
                    Expression::Division(
                        Box::new(match tree.pop() {
                            Some(expr) => expr,
                            None => Expression::Invalid(
                                "No left-hand side was provided to the division operation!"
                                    .to_string(),
                                token.range(),
                            ),
                        }),
                        Box::new(self.eval_expr(&token, &mut tree)),
                    )
                } else {
                    self.end_of_file = true;
                    Expression::EndOfFile
                }
            }

            // Addition grammar
            // (Expression) + (Expression) -> Value
            Token::Plus => {
                if let Some(token) = self.next() {
                    Expression::Addition(
                        Box::new(match tree.pop() {
                            Some(expr) => expr,
                            None => Expression::Invalid(
                                "No left-hand side was provided to the addition operation!"
                                    .to_string(),
                                token.range(),
                            ),
                        }),
                        Box::new(self.eval_expr(&token, &mut tree)),
                    )
                } else {
                    self.end_of_file = true;
                    Expression::EndOfFile
                }
            }

            // Subtraction grammar
            // (Expression -> Integer) - (Expression -> Integer) -> Value
            Token::Minus => {
                if let Some(token) = self.next() {
                    Expression::Subtraction(
                        Box::new(match tree.pop() {
                            Some(expr) => expr,
                            None => Expression::Invalid(
                                "No left-hand side was provided to the subtraction operation!"
                                    .to_string(),
                                token.range(),
                            ),
                        }),
                        Box::new(self.eval_expr(&token, &mut tree)),
                    )
                } else {
                    self.end_of_file = true;
                    Expression::EndOfFile
                }
            }

            // True and False directly transfer to their expression counterparts
            Token::True => Expression::True,
            Token::False => Expression::False,

            // And grammar
            // (Expression -> Bool) and (Expression -> Bool) -> Bool
            Token::And => {
                if let Some(token) = self.next() {
                    Expression::And(
                        Box::new(match tree.pop() {
                            Some(expr) => expr,
                            None => Expression::Invalid(
                                "No left-hand side was provided to the and operation!".to_string(),
                                token.range(),
                            ),
                        }),
                        Box::new(self.eval_expr(&token, &mut tree)),
                    )
                } else {
                    self.end_of_file = true;
                    Expression::EndOfFile
                }
            }

            // Or grammar
            // (Expression -> Bool) or (Expression -> Bool) -> Bool
            Token::Or => {
                if let Some(token) = self.next() {
                    Expression::Or(
                        Box::new(match tree.pop() {
                            Some(expr) => expr,
                            None => Expression::Invalid(
                                "No left-hand side was provided to the or operation!".to_string(),
                                token.range(),
                            ),
                        }),
                        Box::new(self.eval_expr(&token, &mut tree)),
                    )
                } else {
                    self.end_of_file = true;
                    Expression::EndOfFile
                }
            }

            // Vector grammar
            // [(Item..)]
            Token::LeftBracket => {
                // Create a Vec to hold the items of the vector
                // TODO: Is pre-allocation possible?
                let mut vector = Vec::new();

                // While there are tokens, parse them into the vector
                while let Some(token) = self.next() {
                    let token_kind = token.kind();

                    // If the token is not a closing bracket and is a variable
                    // TODO: At some point custom classes will also have to be included in this
                    if token_kind != Token::RightBracket && self.current.is_raw_var() {
                        // Evaluate and push to the Vec
                        vector.push(self.eval_expr(&token, &mut tree));

                    // If the token is a comma, continue
                    } else if token_kind == Token::Comma {
                        continue;

                    // If the token is a closing bracket, package the Vec and return it as an expression
                    } else if token_kind == Token::RightBracket {
                        return Expression::Vector(Box::new(vector));

                    // If something else is within the vector, it is invalid
                    } else {
                        // Push the invalid token to the vector
                        vector.push(Expression::Invalid(
                            "Invalid expression in a vector".to_string(),
                            token.range(),
                        ));
                    }
                }

                // If the while loop is ever broken out of, then a None value was returned from the iterator,
                // and the end of the file was reached
                // TODO: The vector should be returned here with an error referring to it's un-closed nature
                self.end_of_file = true;
                Expression::EndOfFile
            }

            // Let binding grammar
            // let (Identifier): (Type) = (Expression -> Value)
            Token::Variable => self.next_checked(|parser: _| -> Expression {
                if parser.current.kind() == Token::Identifier {
                    let var_name = parser.current.source().to_owned();

                    parser.next_checked(|parser: _| -> Expression {
                        if parser.current.kind() == Token::Colon {
                            parser.next_checked(|parser: _| -> Expression {
                                if parser.current.is_var_type() {
                                    let var_type = Box::new(parser.eval_expr(&parser.current.clone(), &mut tree));

                                    parser.next_checked(|parser: _| -> Expression {
                                        if parser.current.kind() == Token::Equals {
                                            parser.next_checked(|parser: _| -> Expression {
                                                if parser.current.is_raw_var() || parser.current.kind() == Token::LeftBracket {
                                                    let var_value = Box::new(parser.eval_expr(&parser.current.clone(), &mut tree));

                                                    parser.next_checked(|parser: _| -> Expression {
                                                        if parser.current.kind() == Token::SemiColon {
                                                            Expression::Variable(
                                                                var_name,
                                                                var_type,
                                                                var_value,
                                                            )
                                                        } else {
                                                            Expression::Invalid("Expected a `;`".to_string(), token.range())
                                                        }
                                                    })
                                                } else {
                                                    Expression::Invalid("A variable must have a value".to_string(), token.range())
                                                }
                                            })
                                        } else {
                                            Expression::Invalid(
                                                "Expected an `=`".to_string(),
                                                parser.current.range(),
                                            )
                                        }
                                    })
                                } else {
                                    Expression::Invalid("That is not a valid variable type".to_string(), parser.current.range())
                                }
                            })
                        } else {
                            Expression::Invalid("Expected an `=` or a `:`".to_string(), parser.current.range())
                        }
                    })
                } else {
                    Expression::Invalid("A `let` binding was used, but no identifier or variable name was given".to_string(), parser.current.range())
                }
            }),

            // For loop grammar:
            // for (Expression -> Item) in (Expression -> Collection)
            // ....(Expression -> Body) // Apply the body to/for each item in the collection
            Token::For => self.next_checked(|parser: &mut Parser<'crunch, 'source>| -> Expression {
                if parser.current.kind() == Token::Identifier {
                    let item = parser.current.source().to_owned();

                    parser.next_checked(|parser: &mut Parser<'crunch, 'source>| -> Expression {
                        if token.kind() == Token::In {
                            parser.next_checked(|parser: &mut Parser<'crunch, 'source>| -> Expression {
                                let collection = Box::new(parser.eval_expr(&parser.current.clone(), &mut tree));

                                parser.next_checked(|parser: &mut Parser<'crunch, 'source>| -> Expression {
                                    Expression::For {
                                        item,
                                        collection,
                                        body: Box::new(parser.eval_expr(&parser.current.clone(), &mut tree)),
                                    }
                                })
                            })
                        } else {
                            // If the token is not an identifier, return an invalid expression
                            return Expression::Invalid("A `for .. in` binding was used, but no identifer or variable name was supplied".to_string(), token.range());
                        }
                    })
                } else {
                    // If the token is not an identifier, return an invalid expression
                    return Expression::Invalid("A `for` binding was used, but no identifer or variable name was supplied".to_string(), token.range());
                }
            }),

            Token::While => self.next_checked(|parser: &mut Parser<'crunch, 'source>| -> Expression {
                Expression::While(
                    Box::new(match tree.pop() {
                        Some(condition) => condition,
                        None => Expression::Invalid(
                            "No left-hand side was provided to the while loop!".to_string(),
                            parser.current.range(),
                        ),
                    }),
                    Box::new(parser.eval_expr(&parser.current.clone(), &mut tree)),
                )
            }),

            Token::Loop => self.next_checked(|parser: _| -> Expression {
                Expression::Loop(Box::new(parser.eval_expr(&parser.current.clone(), &mut tree)))
            }),

            // FIXME: The if's are broken
            Token::If => self.next_checked(|parser: _| -> Expression {
                let condition = Box::new(parser.eval_expr(&parser.current.clone(), &mut tree));

                parser.next_checked(|parser: _| -> Expression {
                    let body = if parser.current.kind() != Token::LeftBrace {
                        // If the token after the condition is not a `{`, then consume the next token and replace it with an Invalid
                        let previous = parser.current.range();

                        Box::new(parser.next_checked(|_parser: _| -> Expression {
                            Expression::Invalid("Expected a {".to_string(), previous)
                        }))
                    } else {
                        // If the token is a `{`, then consume the next token and evaluate it, storing it in body
                        Box::new(parser.next_checked(|parser: _| -> Expression {
                           parser.eval_expr(&parser.current.clone(), &mut tree)
                        }))
                    };

                    parser.next_checked(|parser: _| -> Expression {
                        if parser.current.kind() == Token::RightBrace {
                            parser.peek_checked(|parser: _, peeked: _| -> Expression {
                                // If the token is
                                if  peeked.kind() == Token::ElseIf
                                    ||  peeked.kind() == Token::Else
                                {
                                    let token = parser.next().expect("The peeked token does not exist!");

                                    Expression::If {
                                        condition,
                                        body,
                                        continuation: Some(Box::new(parser.eval_expr(&token, &mut tree))),
                                    }
                                } else if parser.current.kind() == Token::RightBrace {
                                    parser.next().expect("The peeked token does not exist!");

                                    parser.peek_checked(|parser: _, peeked: _| -> Expression {
                                        if peeked.kind() == Token::ElseIf
                                            || peeked.kind() == Token::Else
                                        {
                                            let token = parser.next().expect("The peeked token does not exist!");

                                            Expression::If {
                                                condition,
                                                body,
                                                continuation: Some(Box::new(parser.eval_expr(&token, &mut tree))),
                                            }
                                        } else {
                                            Expression::If {
                                                condition,
                                                body,
                                                continuation: None,
                                            }
                                        }
                                    })
                                } else {
                                    Expression::If {
                                        condition,
                                        body,
                                        continuation: None,
                                    }
                                }
                            })
                        } else {
                            Expression::Invalid("Expected a `}`".to_string(), parser.current.range())
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

                                Expression::ElseIf {
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

                                        Expression::ElseIf {
                                            condition,
                                            body,
                                            continuation: Some(Box::new(
                                                self.eval_expr(&token, &mut tree),
                                            )),
                                        }
                                    } else {
                                        Expression::ElseIf {
                                            condition,
                                            body,
                                            continuation: None,
                                        }
                                    }
                                } else {
                                    self.end_of_file = true;
                                    Expression::None
                                }
                            } else {
                                self.end_of_file = true;
                                Expression::None
                            }
                        } else {
                            self.end_of_file = true;
                            Expression::None
                        }
                    } else {
                        self.end_of_file = true;
                        Expression::None
                    }
                } else {
                    self.end_of_file = true;
                    Expression::None
                }
            }

            Token::Else => {
                if let Some(token) = self.next() {
                    if let Some(token) = self.next() {
                        Expression::Else {
                            body: Box::new(self.eval_expr(&token, &mut tree)),
                        }
                    } else {
                        self.end_of_file = true;
                        Expression::None
                    }
                } else {
                    self.end_of_file = true;
                    Expression::None
                }
            }

            Token::WhiteSpace
            | Token::Comment
            | Token::MultilineComment
            | Token::DocComment => {
                if let Some(token) = self.next() {
                    // Ignore the meaningless token and evaluate and return the next expression instead
                    self.eval_expr(&token, &mut tree)
                } else {
                    self.end_of_file = true;
                    Expression::EndOfFile
                }
            }

            Token::EndOfFile => {
                self.end_of_file = true;
                Expression::EndOfFile
            }

            _ => Expression::Invalid(
                "The token is not currently supported".to_string(),
                token.range(),
            ),
        }
    }
}
