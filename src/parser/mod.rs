pub mod expression;

use crate::crunch::Crunch;
use crunch_token::{TokenData, TokenStream};
pub use expression::*;
use std::iter::Peekable;

pub struct Parser<'crunch, 'source> {
    token_stream: Peekable<Box<dyn Iterator<Item = TokenData<'source>> + 'source>>,
    crunch: &'crunch mut Crunch,
    current: TokenData<'source>,
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

    pub fn parse(mut self) -> Vec<Expression> {
        self = self.remove_whitespace().remove_comments();

        let mut tree = Vec::default();

        // While tokens are being supplied from the token stream
        while let Some(token) = self.next() {
            if !self.end_of_file {
                self.current = token.clone();

                let expr = self.eval_expr(token, &mut tree);
                tree.push(expr);
            }
        }

        // tree.retain(|node| node != &Expression::None); // Remove any `None` from the tree
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
                .filter(|token| token.kind() != &WhiteSpace)
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
            !(*token.kind() == Comment
                || *token.kind() == MultilineComment
                || *token.kind() == DocComment)
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
impl<'crunch, 'source> Parser<'crunch, 'source> {
    fn eval_expr(
        &mut self,
        token: TokenData<'source>,
        mut tree: &mut Vec<Expression>,
    ) -> Expression {
        use crunch_token::Token;

        match *token.kind() {
            Token::IntLiteral => Expression::IntLiteral(
                token
                    .source()
                    .parse::<i32>()
                    .expect("Failed to parse IntLiteral"),
            ),
            Token::FloatLiteral => Expression::FloatLiteral(
                token
                    .source()
                    .parse::<f32>()
                    .expect("Failed to parse FloatLiteral"),
            ),
            Token::StrLiteral => {
                Expression::StringLiteral(token.source()[1..token.source().len() - 1].to_owned())
            }

            Token::Multiply => {
                if let Some(next) = self.next().clone() {
                    Expression::Multiplication(
                        Box::new(match tree.pop() {
                            Some(expr) => expr,
                            None => {
                                self.crunch.syntax_error(
                                    crate::crunch::SyntaxError::MissingLeftHand,
                                    None,
                                    Some("You must supply a left-hand item to multiply"),
                                );
                                Expression::None
                            }
                        }),
                        Box::new(self.eval_expr(next, &mut tree)),
                    )
                } else {
                    self.end_of_file = true;
                    Expression::None
                }
            }
            Token::Divide => {
                if let Some(next) = self.next().clone() {
                    Expression::Division(
                        Box::new(match tree.pop() {
                            Some(expr) => expr,
                            None => {
                                self.crunch.syntax_error(
                                    crate::crunch::SyntaxError::MissingLeftHand,
                                    None,
                                    Some("You must supply a left-hand item to divide"),
                                );
                                Expression::None
                            }
                        }),
                        Box::new(self.eval_expr(next, &mut tree)),
                    )
                } else {
                    self.end_of_file = true;
                    Expression::None
                }
            }
            Token::Plus => {
                if let Some(next) = self.next().clone() {
                    Expression::Addition(
                        Box::new(match tree.pop() {
                            Some(expr) => expr,
                            None => {
                                self.crunch.syntax_error(
                                    crate::crunch::SyntaxError::MissingLeftHand,
                                    None,
                                    Some("You must supply a left-hand item to add"),
                                );
                                Expression::None
                            }
                        }),
                        Box::new(self.eval_expr(next, &mut tree)),
                    )
                } else {
                    self.end_of_file = true;
                    Expression::None
                }
            }
            Token::Minus => {
                if let Some(next) = self.next().clone() {
                    Expression::Subtraction(
                        Box::new(match tree.pop() {
                            Some(expr) => expr,
                            None => {
                                self.crunch.syntax_error(
                                    crate::crunch::SyntaxError::MissingLeftHand,
                                    None,
                                    Some("You must supply a left-hand item to subtract"),
                                );
                                Expression::None
                            }
                        }),
                        Box::new(self.eval_expr(next, &mut tree)),
                    )
                } else {
                    self.end_of_file = true;
                    Expression::None
                }
            }

            Token::True => Expression::True,
            Token::False => Expression::False,

            Token::And => {
                if let Some(next) = self.next().clone() {
                    Expression::And(
                        Box::new(match tree.pop() {
                            Some(expr) => expr,
                            None => {
                                self.crunch.syntax_error(
                                    crate::crunch::SyntaxError::MissingLeftHand,
                                    None,
                                    Some("You must supply a left-hand item to apply and to"),
                                );
                                Expression::None
                            }
                        }),
                        Box::new(self.eval_expr(next, &mut tree)),
                    )
                } else {
                    self.end_of_file = true;
                    Expression::None
                }
            }
            Token::Or => {
                if let Some(next) = self.next().clone() {
                    Expression::Or(
                        Box::new(match tree.pop() {
                            Some(expr) => expr,
                            None => {
                                self.crunch.syntax_error(
                                    crate::crunch::SyntaxError::MissingLeftHand,
                                    None,
                                    Some("You must supply a left-hand item to apply or to"),
                                );
                                Expression::None
                            }
                        }),
                        Box::new(self.eval_expr(next, &mut tree)),
                    )
                } else {
                    self.end_of_file = true;
                    Expression::None
                }
            }

            Token::LeftBracket => {
                let mut vector = Vec::default();

                while let Some(token) = self.next().clone() {
                    if *token.kind() != Token::RightBracket
                        && (*token.kind() == Token::StrLiteral
                            || *token.kind() == Token::IntLiteral
                            || *token.kind() == Token::FloatLiteral)
                    {
                        vector.push(self.eval_expr(token, &mut tree));
                    } else if *token.kind() == Token::Comma {
                        continue;
                    } else if *token.kind() == Token::RightBracket {
                        return Expression::Vector(Box::new(vector));
                    } else {
                        self.crunch.syntax_error(
                            crate::crunch::SyntaxError::MissingClosingBracket,
                            Some(&token),
                            Some("You must close a vector when you open it with a ["),
                        );
                        return Expression::None;
                    }
                }

                self.end_of_file = true;
                Expression::None
            }

            Token::Variable => {
                if let Some(next) = self.next().clone() {
                    if *next.kind() == Token::Identifier {
                        let var_name = next.source().to_owned();

                        if let Some(next) = self.next().clone() {
                            if *next.kind() == Token::Equals {
                                if let Some(next) = self.next().clone() {
                                    match *next.kind() {
                                        Token::StrLiteral
                                        | Token::IntLiteral
                                        | Token::FloatLiteral
                                        | Token::LeftBracket => Expression::Variable(
                                            var_name,
                                            Box::new(self.eval_expr(next, &mut tree)),
                                        ),
                                        _ => {
                                            self.crunch.syntax_error(crate::crunch::SyntaxError::InvalidType, Some(&next), Some("You must supply a token of a valid type for a variable"));
                                            Expression::None
                                        }
                                    }
                                } else {
                                    self.end_of_file = true;
                                    Expression::None
                                }
                            } else {
                                self.crunch.syntax_error(
                                    crate::crunch::SyntaxError::InvalidType,
                                    Some(&next),
                                    Some("You must have an `=` to set a variable"),
                                );
                                Expression::None
                            }
                        } else {
                            self.end_of_file = true;
                            Expression::None
                        }
                    } else {
                        self.crunch.syntax_error(
                            crate::crunch::SyntaxError::NoIdentifier,
                            Some(&next),
                            Some("You must have an identifier for a variable"),
                        );
                        Expression::None
                    }
                } else {
                    self.end_of_file = true;
                    Expression::None
                }
            }

            Token::For => {
                if let Some(token) = self.next().clone() {
                    if *token.kind() == Token::Identifier {
                        let item = token.source().to_owned();

                        if let Some(token) = self.next().clone() {
                            if *token.kind() == Token::In {
                                if let Some(token) = self.next().clone() {
                                    let collection = Box::new(self.eval_expr(token, &mut tree));

                                    if let Some(token) = self.next().clone() {
                                        return Expression::For {
                                            item,
                                            collection,
                                            body: Box::new(self.eval_expr(token, &mut tree)),
                                        };
                                    }
                                }
                            }
                        }
                    }
                }

                self.end_of_file = true;
                Expression::None
            }
            Token::While => {
                if let Some(next) = self.next().clone() {
                    Expression::While(
                        Box::new(tree.pop().unwrap()),
                        Box::new(self.eval_expr(next, &mut tree)),
                    )
                } else {
                    self.end_of_file = true;
                    Expression::None
                }
            }
            Token::Loop => {
                if let Some(next) = self.next().clone() {
                    Expression::Loop(Box::new(self.eval_expr(next, &mut tree)))
                } else {
                    self.end_of_file = true;
                    Expression::None
                }
            }

            Token::If => {
                if let Some(token) = self.next().clone() {
                    let condition = Box::new(self.eval_expr(token, &mut tree));

                    if let Some(token) = self.next().clone() {
                        let body = Box::new(self.eval_expr(token, &mut tree));

                        if let Some(token) = self.peek().clone() {
                            if *token.kind() == Token::ElseIf || *token.kind() == Token::Else {
                                let token = self
                                    .next()
                                    .clone()
                                    .expect("The peeked token does not exist");

                                Expression::If {
                                    condition,
                                    body,
                                    continuation: Some(Box::new(self.eval_expr(token, &mut tree))),
                                }
                            } else if *token.kind() == Token::Newline {
                                self.next().unwrap();
                                if let Some(token) = self.peek() {
                                    if *token.kind() == Token::ElseIf
                                        || *token.kind() == Token::Else
                                    {
                                        let token = self
                                            .next()
                                            .clone()
                                            .expect("The peeked token does not exist");

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
                if let Some(token) = self.next().clone() {
                    let condition = Box::new(self.eval_expr(token, &mut tree));

                    if let Some(token) = self.next().clone() {
                        let body = Box::new(self.eval_expr(token, &mut tree));

                        if let Some(token) = self.peek().clone() {
                            if *token.kind() == Token::ElseIf || *token.kind() == Token::Else {
                                let token = self
                                    .next()
                                    .clone()
                                    .expect("The peeked token does not exist");

                                Expression::ElseIf {
                                    condition,
                                    body,
                                    continuation: Some(Box::new(self.eval_expr(token, &mut tree))),
                                }
                            } else if *token.kind() == Token::Newline {
                                self.next().unwrap();
                                if let Some(token) = self.peek() {
                                    if *token.kind() == Token::ElseIf
                                        || *token.kind() == Token::Else
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
                if let Some(token) = self.next().clone() {
                    let condition = Box::new(self.eval_expr(token, &mut tree));

                    if let Some(token) = self.next().clone() {
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

            Token::Indent => {
                if let Some(next) = self.next().clone() {
                    Expression::Scope(Box::new(self.eval_expr(next, &mut tree)))
                } else {
                    self.end_of_file = true;
                    Expression::None
                }
            }

            // TODO: Add redundancy removal of Comments/MultilineComments/DocComments
            Token::Newline | Token::WhiteSpace => {
                if let Some(next) = self.next().clone() {
                    self.eval_expr(next, &mut tree)
                } else {
                    self.end_of_file = true;
                    Expression::None
                }
            }

            Token::EndOfFile => {
                self.end_of_file = true;
                Expression::EndOfFile
            }

            _ => {
                self.crunch.syntax_error(
                    crate::crunch::SyntaxError::Unsupported,
                    Some(&token),
                    Some("This token is not supported yet!"),
                );
                Expression::None
            }
        }
    }
}
