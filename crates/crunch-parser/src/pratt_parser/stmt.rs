use super::*;

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
}

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
                let var = {
                    let source = self.eat(TokenType::Ident)?.source();
                    self.intern_string(source)
                };

                self.eat(TokenType::Equal)?;
                let expr = self.expr()?;

                self.eat(TokenType::Newline)?;

                Ok(Statement::VarDeclaration(var, expr))
            }

            TokenType::While => todo!(),
            TokenType::Loop => todo!(),
            TokenType::For => todo!(),

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

            // Covers function calls & Assignments
            TokenType::Ident => {
                let expr = self.expr()?;
                self.eat(TokenType::Newline)?;

                Ok(Statement::Expression(expr))
            }

            t => unimplemented!("{:?}", t),
        }
    }

    fn if_stmt(&mut self) -> ParseResult<Statement> {
        self.eat(TokenType::If)?;
        let condition = self.expr()?;
        self.eat(TokenType::Newline)?;

        let mut body = Vec::with_capacity(3);
        while self.peek()?.ty() != TokenType::Else && self.peek()?.ty() != TokenType::End {
            body.push(self.stmt()?);
        }

        let next = self.next()?;
        let arm = if next.ty() == TokenType::Else && self.peek()?.ty() == TokenType::If {
            Some(Box::new(self.if_stmt()?))
        } else if next.ty() == TokenType::Else {
            self.eat(TokenType::Newline)?;

            let mut body = Vec::with_capacity(3);
            while self.peek()?.ty() != TokenType::Else && self.peek()?.ty() != TokenType::End {
                body.push(self.stmt()?);
            }

            Some(Box::new(Statement::If {
                condition: Expression::Literal(Literal::Bool(true)),
                body,
                arm: None,
            }))
        } else {
            // TODO: Error
            assert!(next.ty() == TokenType::End);
            None
        };

        Ok(Statement::If {
            condition,
            body,
            arm,
        })
    }
}
