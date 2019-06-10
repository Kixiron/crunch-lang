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
}

/// Evaluate an expression
// TODO: This function is absolutely bogged down by the manual checking of valid `next` calls,
// Something needs to be done to either pass that onto a separate funcion or a macro
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
            Token::IntLiteral => match token.source().parse::<i32>() {
                Ok(int) => Expression::IntLiteral(int),
                // If unable to be parsed, return an invalid expression
                Err(_) => Expression::Invalid(
                    format!("`{}` is not a valid integer", token.source()),
                    token.range(),
                ),
            },
            Token::FloatLiteral => match token.source().parse::<f32>() {
                Ok(float) => Expression::FloatLiteral(float),
                Err(_) => {
                    // Keep same error message because floats are only compiler-side
                    Expression::Invalid(
                        format!("`{}` is not a valid integer", token.source()),
                        token.range(),
                    )
                }
            },

            // String Literal grammar
            // "(String)"
            Token::StrLiteral => {
                // The returned source string includes the quotes,
                // so the indexing is to remove the leading and trailing quotes
                Expression::StringLiteral(token.source()[1..token.source().len() - 1].to_owned())
            }

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
                    if token_kind != Token::RightBracket
                        && (token_kind == Token::StrLiteral
                            || token_kind == Token::IntLiteral
                            || token_kind == Token::FloatLiteral)
                    {
                        // Evaluate and push to the Vec
                        vector.push(self.eval_expr(token, &mut tree));

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
            // let (Identifier) = (Expression -> Value)
            Token::Variable => {
                if let Some(token) = self.next() {
                    // If the token is an identifier
                    if token.kind() == Token::Identifier {
                        // Save the variable's name for later
                        let var_name = token.source().to_owned();

                        // Get the next token
                        if let Some(token) = self.next() {
                            // If the token is an equals sign
                            if token.kind() == Token::Equals {
                                // Get the next token
                                if let Some(token) = self.next() {
                                    // Match the type of the token
                                    match token.kind() {
                                        // If a variable type, return a variable expression with the next token as the value
                                        // of the variable
                                        // TODO: THis will have to accept custom classes in the future
                                        Token::StrLiteral
                                        | Token::IntLiteral
                                        | Token::FloatLiteral
                                        | Token::LeftBracket => Expression::Variable(
                                            var_name,
                                            Box::new(self.eval_expr(token, &mut tree)),
                                        ),

                                        // If the token is not a variable type, return an invalid expression
                                        _ => Expression::Invalid(
                                            "An invalid variable type was supplied!".to_string(),
                                            token.range(),
                                        ),
                                    }
                                } else {
                                    self.end_of_file = true;
                                    Expression::EndOfFile
                                }
                            } else {
                                // No equal sign was used, so return an invalid expression
                                // TODO: This must also account for uninitialized variables
                                Expression::Invalid("A `let` binding was used, but no `=` was used to give it a value".to_string(), token.range())
                            }
                        } else {
                            self.end_of_file = true;
                            Expression::EndOfFile
                        }
                    } else {
                        // If no identifier is supplied, return an invalid expression
                        Expression::Invalid("A `let` binding was used, but no identifier or variable name was given".to_string(), token.range())
                    }
                } else {
                    self.end_of_file = true;
                    Expression::EndOfFile
                }
            }

            // For loop grammar:
            // for (Expression -> Item) in (Expression -> Collection)
            // ....(Expression -> Body) // Apply the body to/for each item in the collection
            Token::For => {
                // Get the next token
                if let Some(token) = self.next() {
                    // If the token is an Identifier, continue
                    if token.kind() == Token::Identifier {
                        // Store the item name
                        let item = token.source().to_owned();

                        if let Some(token) = self.next() {
                            if token.kind() == Token::In {
                                if let Some(token) = self.next() {
                                    // Evaluate the collection to be iterated over
                                    let collection = Box::new(self.eval_expr(token, &mut tree));

                                    if let Some(token) = self.next() {
                                        // Evaluate the body and return the complete expression
                                        return Expression::For {
                                            item,
                                            collection,
                                            body: Box::new(self.eval_expr(token, &mut tree)),
                                        };
                                    }
                                }
                            } else {
                                // If the token is not an identifier, return an invalid expression
                                return Expression::Invalid("A `for .. in` binding was used, but no identifer or variable name was supplied".to_string(), token.range());
                            }
                        }
                    } else {
                        // If the token is not an identifier, return an invalid expression
                        return Expression::Invalid("A `for` binding was used, but no identifer or variable name was supplied".to_string(), token.range());
                    }
                }

                self.end_of_file = true;
                Expression::EndOfFile
            }

            Token::While => {
                if let Some(token) = self.next() {
                    Expression::While(
                        Box::new(match tree.pop() {
                            Some(condition) => condition,
                            None => Expression::Invalid(
                                "No left-hand side was provided to the while loop!".to_string(),
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

            Token::Loop => {
                if let Some(token) = self.next() {
                    Expression::Loop(Box::new(self.eval_expr(token, &mut tree)))
                } else {
                    self.end_of_file = true;
                    Expression::EndOfFile
                }
            }

            Token::If => {
                if let Some(token) = self.next() {
                    let condition = Box::new(self.eval_expr(token, &mut tree));

                    if let Some(token) = self.next() {
                        let body = Box::new(self.eval_expr(token, &mut tree));

                        if let Some(token) = self.peek() {
                            if token.kind() == Token::ElseIf || token.kind() == Token::Else {
                                let token = self.next().expect("The peeked token does not exist");

                                Expression::If {
                                    condition,
                                    body,
                                    continuation: Some(Box::new(self.eval_expr(token, &mut tree))),
                                }
                            } else if token.kind() == Token::Newline {
                                self.next().unwrap();
                                if let Some(token) = self.peek() {
                                    if token.kind() == Token::ElseIf || token.kind() == Token::Else
                                    {
                                        let token =
                                            self.next().expect("The peeked token does not exist");

                                        Expression::If {
                                            condition,
                                            body,
                                            continuation: Some(Box::new(
                                                self.eval_expr(token, &mut tree),
                                            )),
                                        }
                                    } else {
                                        Expression::If {
                                            condition,
                                            body,
                                            continuation: None,
                                        }
                                    }
                                } else {
                                    Expression::If {
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
            }

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
