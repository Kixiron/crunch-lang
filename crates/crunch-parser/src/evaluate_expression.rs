use super::{expression::*, parsers::*, Parser};
use crunch_error::{EmittedError, ErrorCode};
use crunch_token::*;

/// Evaluate an Expr
// TODO: This function is absolutely bogged down by the manual checking of valid `next` calls,
// Something needs to be done to either pass that onto a separate funcion or a macro
impl<'source> Parser<'source> {
    pub fn eval_expr(&mut self, token: &TokenData<'source>, mut tree: &mut Vec<Expr>) -> Expr {
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
            Token::Int | Token::Str | Token::Bool | Token::Vector => {
                variable_type(self, token, &mut tree)
            }

            Token::For => self.next_checked(|parser: &mut Parser<'source>| -> Expr {
                if parser.current.kind() == Token::Identifier {
                    let item = parser.current.source().to_owned();

                    parser.next_checked(|parser: &mut Parser<'source>| -> Expr {
                        if parser.current.kind() == Token::In {
                            parser.next_checked(|parser: &mut Parser<'source>| -> Expr {
                                let collection =
                                    Box::new(parser.eval_expr(&parser.current.clone(), &mut tree));

                                parser.next_checked(|parser: &mut Parser<'source>| -> Expr {
                                    Expr::For {
                                        item,
                                        collection,
                                        body: Box::new(
                                            parser.eval_expr(&parser.current.clone(), &mut tree),
                                        ),
                                    }
                                })
                            })
                        } else {
                            // If the token is not an identifier, return an invalid Expr
                            Expr::Error(
                                vec![
                                    EmittedError::new_error("Missing an `in`", Some(ErrorCode::E002), &[parser.current.range()]),
                                    EmittedError::new_help("Try adding an `in` along with a collection to be iterated over in your for loop", None, &[]),
                                ]
                            )
                        }
                    })
                } else {
                    // If the token is not an identifier, return an invalid Expr
                Expr::Error(
                    vec![
                        EmittedError::new_error("Missing identifier", Some(ErrorCode::E001), &[parser.current.range()]),
                        EmittedError::new_help("Try using a valid identifier", None, &[]),
                        EmittedError::new_note("Variable names may only have upper and lowercase A-Z, 0-9 and underscores in them, and the first character must be either a letter or an underscore", None, &[])
                    ]
                )
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
                Expr::Loop(Box::new(
                    parser.eval_expr(&parser.current.clone(), &mut tree),
                ))
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
                                if peeked.kind() == Token::ElseIf || peeked.kind() == Token::Else {
                                    let token =
                                        parser.next().expect("The peeked token does not exist!");

                                    Expr::If {
                                        condition,
                                        body,
                                        continuation: Some(Box::new(
                                            parser.eval_expr(&token, &mut tree),
                                        )),
                                    }
                                } else if parser.current.kind() == Token::RightBrace {
                                    parser.next().expect("The peeked token does not exist!");

                                    parser.peek_checked(|parser: _, peeked: _| -> Expr {
                                        if peeked.kind() == Token::ElseIf
                                            || peeked.kind() == Token::Else
                                        {
                                            let token = parser
                                                .next()
                                                .expect("The peeked token does not exist!");

                                            Expr::If {
                                                condition,
                                                body,
                                                continuation: Some(Box::new(
                                                    parser.eval_expr(&token, &mut tree),
                                                )),
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

            Token::WhiteSpace | Token::Comment | Token::MultilineComment | Token::DocComment => {
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
