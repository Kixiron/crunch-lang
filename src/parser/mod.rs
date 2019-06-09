pub mod expression;

use crate::crunch::Crunch;
use crunch_token::{TokenData, TokenStream};
pub use expression::*;

pub struct Parser<'crunch, 'source> {
    token_stream: Box<dyn Iterator<Item = TokenData<'source>> + 'source>,
    crunch: &'crunch mut Crunch,
    current: TokenData<'source>,
    end_of_file: bool,
}

/// The public interfaces of the Parser
impl<'crunch, 'source> Parser<'crunch, 'source> {
    pub fn new(token_stream: TokenStream<'source>, crunch: &'crunch mut Crunch) -> Self {
        Self {
            token_stream: Box::new(token_stream.into_iter()),
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

        tree.retain(|node| node != &Expression::None); // Remove any `None` from the tree
        tree
    }
}

/// The internal methods of the parser class
impl<'crunch, 'source> Parser<'crunch, 'source> {
    #[inline]
    fn skip_error(self, token: TokenData) -> Self {
        use crunch_token::Token::Newline;

        // Throw a syntax error
        self.crunch.syntax_error(&token);

        // Skip the remainder of the line to avoid excessive errors
        Self {
            token_stream: Box::new(
                self.token_stream
                    .skip_while(|token| token.kind() != &Newline)
                    .skip(1)
                    .into_iter(),
            ),
            crunch: self.crunch,
            current: self.current,
            end_of_file: self.end_of_file,
        }
    }

    #[inline]
    fn remove_whitespace(self) -> Self {
        use crunch_token::Token::WhiteSpace;

        Self {
            token_stream: Box::new(
                self.token_stream
                    .filter(|token| token.kind() != &WhiteSpace)
                    .into_iter(),
            ),
            crunch: self.crunch,
            current: self.current,
            end_of_file: self.end_of_file,
        }
    }

    #[inline]
    fn remove_comments(self) -> Self {
        use crunch_token::Token::{Comment, MultilineComment};

        let mut token_stream = self.token_stream.collect::<Vec<TokenData<'source>>>();
        token_stream
            .retain(|token| !(*token.kind() == Comment || *token.kind() == MultilineComment));

        Self {
            token_stream: Box::new(token_stream.into_iter()),
            crunch: self.crunch,
            current: self.current,
            end_of_file: self.end_of_file,
        }
    }

    #[inline]
    fn next_token(&mut self) {
        self.current = self.token_stream.next().unwrap();
    }

    #[inline]
    fn next(&mut self) -> Option<TokenData<'source>> {
        self.token_stream.next()
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
                        Box::new(tree.pop().unwrap()),
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
                        Box::new(tree.pop().unwrap()),
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
                        Box::new(tree.pop().unwrap()),
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
                        Box::new(tree.pop().unwrap()),
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
                        self.crunch.syntax_error(&token);
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
                                            self.crunch.syntax_error(&next);
                                            Expression::None
                                        }
                                    }
                                } else {
                                    self.end_of_file = true;
                                    Expression::None
                                }
                            } else {
                                self.crunch.syntax_error(&next);
                                Expression::None
                            }
                        } else {
                            self.end_of_file = true;
                            Expression::None
                        }
                    } else {
                        self.crunch.syntax_error(&next);
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

            Token::Indent => {
                if let Some(next) = self.next().clone() {
                    Expression::Scope(Box::new(self.eval_expr(next, &mut tree)))
                } else {
                    self.end_of_file = true;
                    Expression::None
                }
            }

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
                Expression::None
            }

            _ => {
                self.crunch.syntax_error(&token);
                Expression::None
            }
        }
    }
}
