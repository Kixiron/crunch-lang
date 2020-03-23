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
                let (expr, diag) = self.expr()?;

                self.eat(TokenType::Newline)?;

                Ok((Statement::VarDeclaration(var, expr), diag))
            }

            TokenType::Match => self.match_stmt(),

            TokenType::While => self.while_stmt(),

            TokenType::Loop => self.loop_stmt(),

            TokenType::For => self.for_stmt(),

            TokenType::Return => {
                self.eat(TokenType::Return)?;

                if self.peek()?.ty() == TokenType::Newline {
                    self.eat(TokenType::Newline)?;

                    Ok((Statement::Return(None), Vec::new()))
                } else {
                    let (expr, diag) = self.expr()?;
                    self.eat(TokenType::Newline)?;

                    Ok((Statement::Return(Some(expr)), diag))
                }
            }

            TokenType::Break => {
                self.eat(TokenType::Break)?;

                if self.peek()?.ty() == TokenType::Newline {
                    self.eat(TokenType::Newline)?;

                    Ok((Statement::Break(None), Vec::new()))
                } else {
                    let (expr, diag) = self.expr()?;
                    self.eat(TokenType::Newline)?;

                    Ok((Statement::Break(Some(expr)), diag))
                }
            }

            TokenType::Continue => {
                self.eat(TokenType::Continue)?;
                self.eat(TokenType::Newline)?;

                Ok((Statement::Continue, Vec::new()))
            }

            TokenType::Empty => {
                self.eat(TokenType::Empty)?;

                Ok((Statement::Empty, Vec::new()))
            }

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
                let (expr, diag) = self.expr()?;
                self.eat(TokenType::Newline)?;

                Ok((Statement::Expression(expr), diag))
            }

            ty => {
                Err(vec![Diagnostic::error().with_message(format!(
                    "Expected a statement, got a `{}`",
                    ty
                ))])
            }
        }
    }

    fn statements(&mut self, breaks: &[TokenType], capacity: usize) -> ParseResult<Vec<Statement>> {
        let mut statements = Vec::with_capacity(capacity);
        let mut diagnostics = Vec::new();

        while !breaks.contains(&self.peek()?.ty()) {
            let (stmt, diag) = self.stmt()?;
            diagnostics.extend_from_slice(&diag);
            statements.push(stmt);
        }
        statements.shrink_to_fit();

        Ok((statements, diagnostics))
    }

    fn if_stmt(&mut self) -> ParseResult<Statement> {
        self.eat(TokenType::If)?;
        let (condition, mut diagnostics) = self.expr()?;
        self.eat(TokenType::Newline)?;

        let (body, diag) = self.statements(&[TokenType::End, TokenType::Else], 10)?;
        diagnostics.extend_from_slice(&diag);

        let next = self.next()?;
        let arm = if next.ty() == TokenType::Else && self.peek()?.ty() == TokenType::If {
            let (stmt, diag) = self.if_stmt()?;
            diagnostics.extend_from_slice(&diag);

            Some(Box::new(stmt))
        } else if next.ty() == TokenType::Else {
            self.eat(TokenType::Newline)?;

            let (body, diag) = self.statements(&[TokenType::End, TokenType::Else], 10)?;
            diagnostics.extend_from_slice(&diag);

            Some(Box::new(Statement::If {
                condition: Expression::Literal(Literal::Bool(true)),
                body,
                arm: None,
            }))
        } else if next.ty() == TokenType::End {
            None
        } else {
            diagnostics.push(
                Diagnostic::error()
                    .with_message(format!("Expected an `end`, got a `{}`", next.ty())),
            );

            return Err(diagnostics);
        };

        Ok((
            Statement::If {
                condition,
                body,
                arm,
            },
            diagnostics,
        ))
    }

    fn match_stmt(&mut self) -> ParseResult<Statement> {
        self.eat(TokenType::Match)?;
        let (var, mut diagnostics) = self.expr()?;
        self.eat(TokenType::Newline)?;

        let mut arms = Vec::with_capacity(3);
        while self.peek()?.ty() != TokenType::End {
            let capture = self.eat_ident()?;

            let guard = if self.peek()?.ty() == TokenType::Where {
                self.eat(TokenType::Where)?;
                let (expr, diag) = self.expr()?;
                diagnostics.extend_from_slice(&diag);

                Some(expr)
            } else {
                None
            };
            self.eat(TokenType::RightRocket)?;
            self.eat(TokenType::Newline)?;

            let (body, diag) = self.statements(&[TokenType::End], 5)?;
            diagnostics.extend_from_slice(&diag);

            self.eat(TokenType::End)?;
            self.eat(TokenType::Newline)?;

            arms.push((capture, guard, body));
        }
        arms.shrink_to_fit();

        self.eat(TokenType::End)?;

        Ok((Statement::Match { var, arms }, diagnostics))
    }

    fn while_stmt(&mut self) -> ParseResult<Statement> {
        self.eat(TokenType::While)?;
        let (condition, mut diagnostics) = self.expr()?;
        self.eat(TokenType::Newline)?;

        let (body, diag) = self.statements(&[TokenType::End, TokenType::Then], 10)?;
        diagnostics.extend_from_slice(&diag);

        let (then, diag) = self.then_stmt()?;
        diagnostics.extend_from_slice(&diag);

        self.eat(TokenType::End)?;

        Ok((
            Statement::While {
                condition,
                body,
                then,
            },
            diagnostics,
        ))
    }

    fn loop_stmt(&mut self) -> ParseResult<Statement> {
        self.eat(TokenType::Loop)?;
        self.eat(TokenType::Newline)?;

        let (body, mut diagnostics) = self.statements(&[TokenType::End, TokenType::Then], 10)?;

        let (then, diag) = self.then_stmt()?;
        diagnostics.extend_from_slice(&diag);

        self.eat(TokenType::End)?;

        Ok((Statement::Loop { body, then }, diagnostics))
    }

    fn for_stmt(&mut self) -> ParseResult<Statement> {
        self.eat(TokenType::For)?;
        let (var, mut diagnostics) = self.expr()?;
        self.eat(TokenType::In)?;
        let (condition, diag) = self.expr()?;
        diagnostics.extend_from_slice(&diag);
        self.eat(TokenType::Newline)?;

        let (body, diag) = self.statements(&[TokenType::End, TokenType::Then], 10)?;
        diagnostics.extend_from_slice(&diag);

        let (then, diag) = self.then_stmt()?;
        diagnostics.extend_from_slice(&diag);

        Ok((
            Statement::For {
                var,
                condition,
                body,
                then,
            },
            diagnostics,
        ))
    }

    fn then_stmt(&mut self) -> ParseResult<Option<Vec<Statement>>> {
        Ok(if self.peek()?.ty() == TokenType::Then {
            self.eat(TokenType::Then)?;
            self.eat(TokenType::Newline)?;

            let (then, diagnostics) = self.statements(&[TokenType::End, TokenType::Then], 3)?;

            (Some(then), diagnostics)
        } else {
            (None, Vec::new())
        })
    }
}
