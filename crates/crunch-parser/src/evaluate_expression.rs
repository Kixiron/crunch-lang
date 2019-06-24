use super::{expression::*, parsers, Parser};
use crunch_token::*;
use typed_arena::Arena;

/// Evaluate an Expr
// TODO: This function is absolutely bogged down by the manual checking of valid `next` calls,
// Something needs to be done to either pass that onto a separate function or a macro
impl<'source> Parser<'source> {
    pub fn eval_expr(
        &mut self,
        token: &TokenData<'source>,
        mut tree: &mut Vec<Expr>,
    ) -> Expr {
        // Match the token's kind and parse recursively based on that
        match token.kind() {
            // Literals
            Token::IntLiteral
            | Token::FloatLiteral
            | Token::StrLiteral
            | Token::BoolLiteral
            | Token::Null
            | Token::VectorLiteral => {
                Expr::Literal(parsers::literal(token, &mut tree))
            }

            // Binary Operators
            Token::Multiply
            | Token::Divide
            | Token::Plus
            | Token::Minus
            | Token::Or
            | Token::And
            | Token::Not => parsers::binary_operation(self, token, &mut tree),

            // Variable declarations
            Token::Variable => parsers::variable(self, &mut tree),

            // Variable types
            Token::Int | Token::Str | Token::Bool | Token::Vector => {
                parsers::variable_type(token)
            }

            Token::For => parsers::for_loop(self, &mut tree),

            Token::Loop => self.next_checked(
                |parser: _| -> Expr {
                    Expr::Loop({
                        let body = Arena::with_capacity(1);
                        body.alloc(
                            parser
                                .eval_expr(&parser.current.clone(), &mut tree),
                        );
                        body
                    })
                },
                None,
                None::<Box<FnOnce(&mut Parser<'_>) -> Expr>>,
            ),

            Token::While => self.next_checked(
                |parser: &mut Parser<'source>| -> Expr {
                    let condition = Arena::with_capacity(1);
                    condition.alloc(
                        parser.eval_expr(&parser.current.clone(), &mut tree),
                    );

                    let body = Arena::with_capacity(1);
                    body.alloc(
                        parser.eval_expr(&parser.current.clone(), &mut tree),
                    );

                    Expr::While(condition, body)
                },
                None,
                None::<Box<FnOnce(&mut Parser<'_>) -> Expr>>,
            ),

            /*
            // FIXME: The if's are broken
            Token::If => self.next_checked(|parser: _| -> Expr {
                let condition = Arena::with_capacity(1);
                condition.alloc(parser.eval_expr(&parser.current.clone(), &mut tree));

                parser.next_checked(|parser: _| -> Expr {
                    let body = Arena::with_capacity(1);
                    if parser.current.kind() != Token::LeftBrace {
                        // If the token after the condition is not a `{`, then consume the next token and replace it with an Invalid
                        let previous = parser.current.range();

                        body.alloc(parser.next_checked(|_parser: _| -> Expr {
                            Expr::Invalid("Expected a {".to_string(), previous)
                        }));
                    } else {
                        // If the token is a `{`, then consume the next token and evaluate it, storing it in body
                        body.alloc(parser.next_checked(|parser: _| -> Expr {
                            parser.eval_expr(&parser.current.clone(), &mut tree)
                        }));
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
                                        continuation: Some({
                                            let continuation = Arena::with_capacity(1);
                                            continuation.alloc(parser.eval_expr(&token, &mut tree));
                                            continuation
                                        }),
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
                                                continuation: Some({
                                                    let continuation = Arena::with_capacity(1);
                                                    continuation.alloc(parser.eval_expr(&token, &mut tree));
                                                    continuation
                                                }),
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
                    let condition = Arena::with_capacity(1);
                    condition.alloc(self.eval_expr(&token, &mut tree));

                    if let Some(token) = self.next() {
                        let body = Arena::with_capacity(1);
                        body.alloc(self.eval_expr(&token, &mut tree));

                        if let Some(token) = self.peek() {
                            if token.kind() == Token::ElseIf || token.kind() == Token::Else {
                                let token = self.next().expect("The peeked token does not exist");

                                Expr::ElseIf {
                                    condition,
                                    body,
                                    continuation: Some({
                                        let continuation = Arena::with_capacity(1);
                                        continuation.alloc(self.eval_expr(&token, &mut tree));
                                        continuation
                                    }),
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
                                            continuation: Some({
                                                let continuation = Arena::with_capacity(1);
                                                continuation.alloc(self.eval_expr(&token, &mut tree));
                                                continuation
                                            }),
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
                if let Some(_token) = self.next() {
                    if let Some(token) = self.next() {
                        let body = Arena::with_capacity(1);
                        body.alloc(self.eval_expr(&token, &mut tree));

                        Expr::Else {
                            body,
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

            */
            Token::Function => parsers::function(self, token, &mut tree),

            Token::WhiteSpace
            | Token::Comment
            | Token::MultilineComment
            | Token::DocComment => {
                self.next_checked(
                    |parser: _| -> Expr {
                        // Ignore the meaningless token and evaluate and return the next Expr instead
                        parser.eval_expr(&token, &mut tree)
                    },
                    None,
                    None::<Box<FnOnce(&mut Parser<'_>) -> Expr>>,
                )
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
