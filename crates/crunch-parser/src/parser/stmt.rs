use super::*;

use alloc::{boxed::Box, vec::Vec};

// TODO: Use arenas over Boxes

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    If {
        condition: Expression,
        body: Vec<Statement>,
        arm: Option<Box<Statement>>,
    },
    Expression(Expression),
    VarDeclaration(Sym, Expression),
    Return(Option<Expression>),
    Break(Option<Expression>),
    Continue,
    While {
        condition: Expression,
        body: Vec<Statement>,
        then: Option<Vec<Statement>>,
    },
    Loop {
        body: Vec<Statement>,
        then: Option<Vec<Statement>>,
    },
    For {
        var: Expression,
        condition: Expression,
        body: Vec<Statement>,
        then: Option<Vec<Statement>>,
    },
    Match {
        var: Expression,
        arms: Vec<(Sym, Option<Expression>, Vec<Statement>)>,
    },
    Empty,
}

// TODO: Type ascription

/// Statement parsing
impl<'a> Parser<'a> {
    pub(super) fn stmt(&mut self) -> ParseResult<Statement> {
        match self.peek()?.ty() {
            TokenType::If => self.if_stmt(),

            TokenType::Newline => {
                self.eat(TokenType::Newline)?;
                self.stmt()
            }

            TokenType::Let => {
                self.eat(TokenType::Let)?;
                let var = self.eat_ident()?;

                self.eat(TokenType::Equal)?;
                let expr = self.expr()?;

                self.eat(TokenType::Newline)?;

                Ok(Statement::VarDeclaration(var, expr))
            }

            TokenType::Match => self.match_stmt(),

            TokenType::While => self.while_stmt(),

            TokenType::Loop => self.loop_stmt(),

            TokenType::For => self.for_stmt(),

            TokenType::Return => {
                self.eat(TokenType::Return)?;

                if self.peek()?.ty() == TokenType::Newline {
                    self.eat(TokenType::Newline)?;

                    Ok(Statement::Return(None))
                } else {
                    let expr = self.expr()?;
                    self.eat(TokenType::Newline)?;

                    Ok(Statement::Return(Some(expr)))
                }
            }

            TokenType::Break => {
                self.eat(TokenType::Break)?;

                if self.peek()?.ty() == TokenType::Newline {
                    self.eat(TokenType::Newline)?;

                    Ok(Statement::Break(None))
                } else {
                    let expr = self.expr()?;
                    self.eat(TokenType::Newline)?;

                    Ok(Statement::Break(Some(expr)))
                }
            }

            TokenType::Continue => {
                self.eat(TokenType::Continue)?;
                self.eat(TokenType::Newline)?;

                Ok(Statement::Continue)
            }

            TokenType::Empty => Ok(Statement::Empty),

            // Expressions
            TokenType::Ident
            | TokenType::Int
            | TokenType::String
            | TokenType::Float
            | TokenType::Bool
            | TokenType::Minus
            | TokenType::Bang
            | TokenType::Plus
            | TokenType::LeftBrace => {
                let expr = self.expr()?;
                self.eat(TokenType::Newline)?;

                Ok(Statement::Expression(expr))
            }

            ty => {
                Err(Diagnostic::error()
                    .with_message(format!("Expected a statement, got a `{}`", ty)))
            }
        }
    }

    fn statements(&mut self, breaks: &[TokenType], capacity: usize) -> ParseResult<Vec<Statement>> {
        let mut statements = Vec::with_capacity(capacity);
        while !breaks.contains(&self.peek()?.ty()) {
            statements.push(self.stmt()?);
        }
        statements.shrink_to_fit();

        Ok(statements)
    }

    fn if_stmt(&mut self) -> ParseResult<Statement> {
        self.eat(TokenType::If)?;
        let condition = self.expr()?;
        self.eat(TokenType::Newline)?;

        let body = self.statements(&[TokenType::End, TokenType::Else], 10)?;

        let next = self.next()?;
        let arm = if next.ty() == TokenType::Else && self.peek()?.ty() == TokenType::If {
            Some(Box::new(self.if_stmt()?))
        } else if next.ty() == TokenType::Else {
            self.eat(TokenType::Newline)?;

            let body = self.statements(&[TokenType::End, TokenType::Else], 10)?;

            Some(Box::new(Statement::If {
                condition: Expression::Literal(Literal::Bool(true)),
                body,
                arm: None,
            }))
        } else if next.ty() == TokenType::End {
            None
        } else {
            return Err(Diagnostic::error()
                .with_message(format!("Expected an `end`, got a `{}`", next.ty())));
        };

        Ok(Statement::If {
            condition,
            body,
            arm,
        })
    }

    fn match_stmt(&mut self) -> ParseResult<Statement> {
        self.eat(TokenType::Match)?;
        let var = self.expr()?;
        self.eat(TokenType::Newline)?;

        let mut arms = Vec::with_capacity(3);
        while self.peek()?.ty() != TokenType::End {
            let capture = self.eat_ident()?;

            let guard = if self.peek()?.ty() == TokenType::Where {
                self.eat(TokenType::Where)?;
                Some(self.expr()?)
            } else {
                None
            };
            self.eat(TokenType::RightRocket)?;
            self.eat(TokenType::Newline)?;

            let body = self.statements(&[TokenType::End], 5)?;

            self.eat(TokenType::End)?;
            self.eat(TokenType::Newline)?;

            arms.push((capture, guard, body));
        }
        arms.shrink_to_fit();

        self.eat(TokenType::End)?;

        Ok(Statement::Match { var, arms })
    }

    fn while_stmt(&mut self) -> ParseResult<Statement> {
        self.eat(TokenType::While)?;
        let condition = self.expr()?;
        self.eat(TokenType::Newline)?;

        let body = self.statements(&[TokenType::End, TokenType::Then], 10)?;
        let then = self.then_stmt()?;

        self.eat(TokenType::End)?;

        Ok(Statement::While {
            condition,
            body,
            then,
        })
    }

    fn loop_stmt(&mut self) -> ParseResult<Statement> {
        self.eat(TokenType::Loop)?;
        self.eat(TokenType::Newline)?;

        let body = self.statements(&[TokenType::End, TokenType::Then], 10)?;
        let then = self.then_stmt()?;

        self.eat(TokenType::End)?;

        Ok(Statement::Loop { body, then })
    }

    fn for_stmt(&mut self) -> ParseResult<Statement> {
        self.eat(TokenType::For)?;
        let var = self.expr()?;
        self.eat(TokenType::In)?;
        let condition = self.expr()?;
        self.eat(TokenType::Newline)?;

        let body = self.statements(&[TokenType::End, TokenType::Then], 10)?;
        let then = self.then_stmt()?;

        Ok(Statement::For {
            var,
            condition,
            body,
            then,
        })
    }

    fn then_stmt(&mut self) -> ParseResult<Option<Vec<Statement>>> {
        Ok(if self.peek()?.ty() == TokenType::Then {
            self.eat(TokenType::Then)?;
            self.eat(TokenType::Newline)?;

            let then = self.statements(&[TokenType::End, TokenType::Then], 3)?;

            Some(then)
        } else {
            None
        })
    }
}
