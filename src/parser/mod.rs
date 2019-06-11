pub mod expression;

use crate::crunch::Crunch;
use crunch_token::{TokenData, TokenStream};
pub use expression::*;
use std::iter::Peekable;

pub struct Parser<'crunch, 'source> {
    /// A peekable stored iterator
    token_stream: Peekable<Box<dyn Iterator<Item = TokenData<'source>> + 'source>>,
    /// A mutable reference to the crunch instance that spawned the parse
    crunch: &'crunch mut Crunch,
    /// The current token
    current: TokenData<'source>,
    /// Whether or not the end of the file has been reached and the parsing should terminate
    end_of_file: bool,
}

/// The public interfaces of the Parser
impl<'crunch, 'source> Parser<'crunch, 'source> {
    pub fn new(token_stream: TokenStream<'source>, crunch: &'crunch mut Crunch) -> Self {
        // Workaround to appease type inference
        let iter: Box<Iterator<Item = TokenData<'source>>> = Box::new(token_stream.into_iter());
        Self {
            token_stream: iter.peekable(),
            crunch,
            // Kinda hacky, the token loop operates off of each token_stream.next(),
            // so it will skip the token loaded into `current` here
            current: TokenData {
                kind: crunch_token::Token::WhiteSpace,
                source: " ",
                range: (0..0),
            },
            end_of_file: false,
        }
    }

    /// Create an Abstract Syntax Tree from the token stream
    // TODO: Find out if a continual yielding of nodes is possible by using generators,
    // then feeding the generator to the evaluator in order to speed up interpretation time
    pub fn parse(mut self) -> Vec<Expression> {
        // Remove whitespace and comments from the token stream
        // TODO: Find out if parse-time removal/ignorance is faster
        self = self.remove_whitespace().remove_comments();

        // Create the parse tree's vector
        let mut tree = Vec::new();

        // While tokens are being supplied from the token stream
        while let Some(token) = self.next() {
            // While the end of file has not been reached
            if !self.end_of_file {
                // Fill current
                self.current = token.clone();

                // Evaluate the current token as an expression
                let expr = self.eval_expr(token, &mut tree);
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
    #[inline]
    fn remove_whitespace(self) -> Self {
        use crunch_token::Token::WhiteSpace;

        let iter: Box<Iterator<Item = TokenData<'source>>> = Box::new(
            self.token_stream
                .filter(|token| token.kind() != WhiteSpace)
                .into_iter(),
        );

        Self {
            token_stream: iter.peekable(),
            crunch: self.crunch,
            current: self.current,
            end_of_file: self.end_of_file,
        }
    }

    #[inline]
    fn remove_comments(self) -> Self {
        use crunch_token::Token::{Comment, DocComment, MultilineComment};

        let mut token_stream = self.token_stream.collect::<Vec<TokenData<'source>>>();
        token_stream.retain(|token| {
            !(token.kind() == Comment
                || token.kind() == MultilineComment
                || token.kind() == DocComment)
        });

        let iter: Box<Iterator<Item = TokenData<'source>>> = Box::new(token_stream.into_iter());

        Self {
            token_stream: iter.peekable(),
            crunch: self.crunch,
            current: self.current,
            end_of_file: self.end_of_file,
        }
    }

    #[inline]
    fn next(&mut self) -> Option<TokenData<'source>> {
        self.token_stream.next()
    }

    // FIXME: Ownership tussle
    #[inline]
    fn peek(&mut self) -> Option<&TokenData<'source>> {
        self.token_stream.peek()
    }

    #[inline]
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
    #[inline]
    fn peek_checked<F>(&mut self, callback: F) -> Expression
    where
        F: FnOnce(&mut Parser<'crunch, 'source>) -> Expression,
    {
        // If there is a next token
        if let Some(token) = self.peek() {
            // Execute the callback
            callback(self)

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
impl<'crunch, 'source> Parser<'crunch, 'source> {
    fn eval_expr(
        &mut self,
        token: TokenData<'source>,
        mut tree: &mut Vec<Expression>,
    ) -> Expression {
        use crunch_token::Token;

        // Match the token's kind and parse recursively based on that
        match token.kind() {
            // Integer Literal grammar
            // (Expression -> Integer)
            Token::IntLiteral => {
                let source = token.source().to_owned().replace("_", "");
                let sign = if source.chars().nth(0) == Some('-') {
                    Sign::Negative
                } else {
                    Sign::Positive
                };

                // This is a thing.
                // It checks if each int type is parsable to, if not it assumes an overflow and
                // continues to the next largest int size until it hits u128, where it reports that the integer is invalid
                // TODO: No assumptions. Find out if the int has overflowed or is truly invalid
                // TODO: Maybe match prospective int's length against the string length of each int type to make parsing quicker?
                match source.parse::<i32>() {
                    Ok(int) => Expression::IntLiteral(IntType::_i32(int)),
                    Err(_) => match source.parse::<u32>() {
                        Ok(int) => Expression::IntLiteral(IntType::_u32(int, sign)),
                        Err(_) => match source.parse::<i64>() {
                            Ok(int) => Expression::IntLiteral(IntType::_i64(int)),
                            Err(_) => match source.parse::<u64>() {
                                Ok(int) => Expression::IntLiteral(IntType::_u64(int, sign)),
                                Err(_) => match source.parse::<i128>() {
                                    Ok(int) => Expression::IntLiteral(IntType::_i128(int)),
                                    Err(_) => match source.parse::<u128>() {
                                        Ok(int) => Expression::IntLiteral(IntType::_u128(int, sign)),
                                        Err(_) => Expression::Invalid(
                                            format!(
                                                "`{}` is not a valid integer. You might try removing invalid characters or making it shorter",
                                                token.source()
                                            ),
                                            token.range(),
                                        ),
                                    },
                                },
                            },
                        },
                    },
                }
            },

            // Float Literal grammar
            // (Expression -> Float)
            // First parse the number as a 32-bit float
            Token::FloatLiteral => match token.source().parse::<f32>() {
                Ok(float) => Expression::FloatLiteral(FloatType::_f32(float)),

                // If parsing fails, attempt to parse as a 64-bit float
                Err(err) => match token.source().parse::<f64>() {
                    Ok(float) => Expression::FloatLiteral(FloatType::_f64(float)),

                    // If all parsing attempts fail, then it is not valid
                    Err(err) => Expression::Invalid(
                        format!(
                            "`{}` is not a valid integer. You might try removing invalid characters or making it shorter",
                            token.source()
                        ),
                        token.range(),
                    ),
                },
            },

            // String Literal grammar
            // "(String)"
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
                        Box::new(self.eval_expr(token, &mut tree)),
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
                        Box::new(self.eval_expr(token, &mut tree)),
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
                        Box::new(self.eval_expr(token, &mut tree)),
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
                        Box::new(self.eval_expr(token, &mut tree)),
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
                        Box::new(self.eval_expr(token, &mut tree)),
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
                        Box::new(self.eval_expr(token, &mut tree)),
                    )
                } else {
                    self.end_of_file = true;
                    Expression::EndOfFile
                }
            }

            // Vector grammar
            // [(Expression -> Item)..]
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
                        vector.push(self.eval_expr(token, &mut tree));

                    // If the token is a comma, continue
                    } else if token_kind == Token::Comma || token_kind == Token::Newline || token_kind == Token::Indent {
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
            Token::Variable => self.next_checked(|parser: &mut Parser<'crunch, 'source>| -> Expression {
                if parser.current.kind() == Token::Identifier {
                    let var_name = parser.current.source().to_owned();

                    parser.next_checked(|parser: &mut Parser<'crunch, 'source>| -> Expression {
                        if parser.current.kind() == Token::Equals {
                            parser.next_checked(|parser: &mut Parser<'crunch, 'source>| -> Expression {
                                if parser.current.is_raw_var() || parser.current.kind() == Token::LeftBracket {
                                    Expression::Variable(
                                        var_name,
                                        None,
                                        Box::new(parser.eval_expr(parser.current.clone(), &mut tree)),
                                    )
                                } else {
                                    Expression::Invalid(
                                        "An invalid variable type was supplied!".to_string(),
                                        parser.current.range(),
                                    )
                                }
                            })
                        } else if parser.current.kind() == Token::Colon {
                            parser.next_checked(|parser: &mut Parser<'crunch, 'source>| -> Expression {
                                println!("{:?}", parser.current);
                                println!("{:?}", parser.current.is_var_type());
                                if parser.current.is_var_type() {
                                    let var_type = Box::new(parser.eval_expr(parser.current.clone(), &mut tree));

                                    parser.next_checked(|parser: &mut Parser<'crunch, 'source>| -> Expression {
                                        if parser.current.kind() == Token::Equals {
                                            parser.next_checked(|parser: &mut Parser<'crunch, 'source>| -> Expression {
                                                if parser.current.is_raw_var() || parser.current.kind() == Token::LeftBracket {
                                                    Expression::Variable(
                                                        var_name,
                                                        Some(var_type),
                                                        Box::new(parser.eval_expr(parser.current.clone(), &mut tree))
                                                    )
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
                                let collection = Box::new(parser.eval_expr(parser.current.clone(), &mut tree));

                                parser.next_checked(|parser: &mut Parser<'crunch, 'source>| -> Expression {
                                    Expression::For {
                                        item,
                                        collection,
                                        body: Box::new(parser.eval_expr(parser.current.clone(), &mut tree)),
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
                    Box::new(parser.eval_expr(parser.current.clone(), &mut tree)),
                )
            }),

            Token::Loop => self.next_checked(|parser: &mut Parser<'crunch, 'source>| -> Expression {
                Expression::Loop(Box::new(parser.eval_expr(parser.current.clone(), &mut tree)))
            }),

            Token::If => self.next_checked(|parser: &mut Parser<'crunch, 'source>| -> Expression {
                let condition = Box::new(parser.eval_expr(parser.current.clone(), &mut tree));

                parser.next_checked(|parser: &mut Parser<'crunch, 'source>| -> Expression {
                    let body = Box::new(parser.eval_expr(parser.current.clone(), &mut tree));

                    parser.peek_checked(|parser: &mut Parser<'crunch, 'source>| -> Expression {
                        if parser.current.kind() == Token::ElseIf
                            || parser.current.kind() == Token::Else
                        {
                            let token = parser.next().expect("The peeked token does not exist!");
                
                            Expression::If {
                                condition,
                                body,
                                continuation: Some(Box::new(parser.eval_expr(token, &mut tree))),
                            }
                        } else if parser.current.kind() == Token::Newline {
                            parser.next().expect("The peeked token does not exist!");

                            parser.peek_checked(|parser: &mut Parser<'crunch, 'source>| -> Expression {
                                if parser.current.kind() == Token::ElseIf 
                                    || parser.current.kind() == Token::Else
                                {
                                    let token = parser.next().expect("The peeked token does not exist!");
                
                                    Expression::If {
                                        condition,
                                        body,
                                        continuation: Some(Box::new(parser.eval_expr(token, &mut tree))),
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
                })
            }),

            Token::ElseIf => {
                if let Some(token) = self.next() {
                    let condition = Box::new(self.eval_expr(token, &mut tree));

                    if let Some(token) = self.next() {
                        let body = Box::new(self.eval_expr(token, &mut tree));

                        if let Some(token) = self.peek() {
                            if token.kind() == Token::ElseIf || token.kind() == Token::Else {
                                let token = self.next().expect("The peeked token does not exist");

                                Expression::ElseIf {
                                    condition,
                                    body,
                                    continuation: Some(Box::new(self.eval_expr(token, &mut tree))),
                                }
                            } else if token.kind() == Token::Newline {
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
                                                self.eval_expr(token, &mut tree),
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
                    let condition = Box::new(self.eval_expr(token, &mut tree));

                    if let Some(token) = self.next() {
                        Expression::Else {
                            condition,
                            body: Box::new(self.eval_expr(token, &mut tree)),
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

            // TODO: Currently, each indent is it's own scope, while it should be each level of indents,
            // Eg.
            // ....(Expression) <--------- 'a
            // ....(Expression)             |
            // ........(Expression) <---'b  |
            // ........(Expression) <--- |  |
            // ....(Expression)             |
            // ....(Expression) <-----------
            Token::Indent => {
                if let Some(token) = self.next() {
                    Expression::Scope(Box::new(self.eval_expr(token, &mut tree)))
                } else {
                    self.end_of_file = true;
                    Expression::EndOfFile
                }
            }

            Token::Newline
            | Token::WhiteSpace
            | Token::Comment
            | Token::MultilineComment
            | Token::DocComment => {
                if let Some(token) = self.next() {
                    // Ignore the meaningless token and evaluate and return the next expression instead
                    self.eval_expr(token, &mut tree)
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
