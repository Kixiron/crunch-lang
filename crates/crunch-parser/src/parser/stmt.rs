use crate::{
    context::StrT,
    error::{Error, Locatable, Location, ParseResult, SemanticError, Span, SyntaxError},
    parser::{Binding, Expr, Parser, Type},
    token::TokenType,
};
#[cfg(feature = "no-std")]
use alloc::{format, vec::Vec};
use crunch_proc::recursion_guard;
#[cfg(test)]
use serde::Serialize;
use stadium::Ticket;

#[cfg_attr(test, derive(Serialize))]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Stmt<'ctx> {
    If {
        condition: Ticket<'ctx, Expr<'ctx>>,
        body: Vec<Ticket<'ctx, Stmt<'ctx>>>,
        clauses: Vec<(Ticket<'ctx, Expr<'ctx>>, Vec<Ticket<'ctx, Stmt<'ctx>>>)>,
        else_clause: Option<Vec<Ticket<'ctx, Stmt<'ctx>>>>,
    },
    Expr(Ticket<'ctx, Expr<'ctx>>),
    VarDeclaration {
        name: StrT,
        ty: Locatable<Ticket<'ctx, Type<'ctx>>>,
        val: Ticket<'ctx, Expr<'ctx>>,
        constant: bool,
        mutable: bool,
    },
    Return(Option<Ticket<'ctx, Expr<'ctx>>>),
    Break(Option<Ticket<'ctx, Expr<'ctx>>>),
    Continue,
    While {
        condition: Ticket<'ctx, Expr<'ctx>>,
        body: Vec<Ticket<'ctx, Stmt<'ctx>>>,
        then: Option<Vec<Ticket<'ctx, Stmt<'ctx>>>>,
        else_clause: Option<Vec<Ticket<'ctx, Stmt<'ctx>>>>,
    },
    Loop {
        body: Vec<Ticket<'ctx, Stmt<'ctx>>>,
        else_clause: Option<Vec<Ticket<'ctx, Stmt<'ctx>>>>,
    },
    For {
        var: Ticket<'ctx, Expr<'ctx>>,
        condition: Ticket<'ctx, Expr<'ctx>>,
        body: Vec<Ticket<'ctx, Stmt<'ctx>>>,
        then: Option<Vec<Ticket<'ctx, Stmt<'ctx>>>>,
        else_clause: Option<Vec<Ticket<'ctx, Stmt<'ctx>>>>,
    },
    Match {
        var: Ticket<'ctx, Expr<'ctx>>,
        arms: Vec<(
            Binding<'ctx>,
            Option<Ticket<'ctx, Expr<'ctx>>>,
            Vec<Ticket<'ctx, Stmt<'ctx>>>,
        )>,
    },
    Empty,
}

// TODO: Type ascription

/// Statement parsing
impl<'src, 'cxl, 'ctx> Parser<'src, 'cxl, 'ctx> {
    #[recursion_guard]
    pub fn stmt(&mut self) -> ParseResult<Option<Ticket<'ctx, Stmt<'ctx>>>> {
        match self.peek()?.ty() {
            TokenType::If => {
                let stmt = self.if_stmt()?;
                Ok(Some(stmt))
            }

            TokenType::Newline => {
                self.eat(TokenType::Newline, [])?;
                Ok(None)
            }

            TokenType::Let | TokenType::Const => {
                let start_token =
                    self.eat_of([TokenType::Let, TokenType::Const], [TokenType::Newline])?;

                let constant = start_token.ty() == TokenType::Const;
                let mutable = self.peek()?.ty() == TokenType::Mut;
                if mutable {
                    self.eat(TokenType::Mut, [TokenType::Newline])?;
                }

                let (name, span) = {
                    let ident = self.eat(TokenType::Ident, [TokenType::Newline])?;
                    (self.context.intern(ident.source()), ident.span())
                };

                self.eat(TokenType::Colon, [TokenType::Newline])?;
                if self.peek()?.ty() == TokenType::Newline {
                    self.eat(TokenType::Newline, [])?;
                }

                let ty = if self.peek()?.ty() == TokenType::Equal {
                    self.eat(TokenType::Equal, [])?;
                    Locatable::new(
                        self.context.store(Type::default()),
                        Location::implicit(span, self.current_file),
                    )
                } else {
                    let ty = self.ascribed_type()?;
                    self.eat(TokenType::Colon, [TokenType::Newline])?;
                    self.eat(TokenType::Equal, [])?;
                    ty
                };

                let val = self.expr()?;
                let end_span = self.eat(TokenType::Newline, [])?.span();

                if constant && mutable {
                    return Err(Locatable::new(
                        Error::Semantic(SemanticError::MutableConstant),
                        Location::concrete(
                            Span::merge(start_token.span(), end_span),
                            self.current_file,
                        ),
                    ));
                }

                let stmt = self.context.store(Stmt::VarDeclaration {
                    name,
                    ty,
                    val,
                    constant,
                    mutable,
                });

                Ok(Some(stmt))
            }

            TokenType::Match => {
                let stmt = self.match_stmt()?;

                Ok(Some(stmt))
            }

            TokenType::While => {
                let stmt = self.while_stmt()?;
                Ok(Some(stmt))
            }

            TokenType::Loop => {
                let stmt = self.loop_stmt()?;
                Ok(Some(stmt))
            }

            TokenType::For => {
                let stmt = self.for_stmt()?;
                Ok(Some(stmt))
            }

            TokenType::Return => {
                self.eat(TokenType::Return, [TokenType::Newline])?;

                if self.peek()?.ty() == TokenType::Newline {
                    self.eat(TokenType::Newline, [])?;
                    let stmt = self.context.store(Stmt::Return(None));

                    Ok(Some(stmt))
                } else {
                    let expr = self.expr()?;
                    self.eat(TokenType::Newline, [])?;
                    let stmt = self.context.store(Stmt::Return(Some(expr)));

                    Ok(Some(stmt))
                }
            }

            TokenType::Break => {
                self.eat(TokenType::Break, [TokenType::Newline])?;

                if self.peek()?.ty() == TokenType::Newline {
                    self.eat(TokenType::Newline, [])?;
                    let stmt = self.context.store(Stmt::Break(None));

                    Ok(Some(stmt))
                } else {
                    let expr = self.expr()?;
                    self.eat(TokenType::Newline, [])?;
                    let stmt = self.context.store(Stmt::Break(Some(expr)));

                    Ok(Some(stmt))
                }
            }

            TokenType::Continue => {
                self.eat(TokenType::Continue, [TokenType::Newline])?;
                self.eat(TokenType::Newline, [])?;

                let stmt = self.context.store(Stmt::Continue);

                Ok(Some(stmt))
            }

            TokenType::Empty => {
                self.eat(TokenType::Empty, [TokenType::Newline])?;
                self.eat(TokenType::Newline, [])?;

                let stmt = self.context.store(Stmt::Empty);

                Ok(Some(stmt))
            }

            // Exprs
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
                self.eat(TokenType::Newline, [])?;
                let stmt = self.context.store(Stmt::Expr(expr));

                Ok(Some(stmt))
            }

            _ => {
                let token = self.peek()?;

                Err(Locatable::new(
                    Error::Syntax(SyntaxError::Generic(format!(
                        "Expected a statement, got a `{}`",
                        token.ty(),
                    ))),
                    Location::concrete(&self.peek()?, self.current_file),
                ))
            }
        }
    }

    #[recursion_guard]
    fn statements(
        &mut self,
        breaks: &[TokenType],
        capacity: usize,
    ) -> ParseResult<Vec<Ticket<'ctx, Stmt<'ctx>>>> {
        let mut statements = Vec::with_capacity(capacity);

        while let Ok(true) = self.peek().map(|p| !breaks.contains(&p.ty())) {
            let stmt = self.stmt()?;
            if let Some(stmt) = stmt {
                statements.push(stmt);
            }
        }

        Ok(statements)
    }

    #[recursion_guard]
    fn if_stmt(&mut self) -> ParseResult<Ticket<'ctx, Stmt<'ctx>>> {
        self.eat(TokenType::If, [TokenType::Newline])?;
        let condition = self.expr()?;
        self.eat(TokenType::Newline, [])?;

        let body = self.statements(&[TokenType::End, TokenType::Else], 10)?;

        let mut clauses = Vec::new();
        let mut else_clause = None;
        loop {
            let delimiter = self.eat_of([TokenType::Else, TokenType::End], [TokenType::Newline])?;

            match delimiter.ty() {
                TokenType::Else if self.peek()?.ty() == TokenType::If => {
                    self.eat(TokenType::If, [])?;
                    let condition = self.expr()?;
                    self.eat(TokenType::Newline, [])?;

                    let body = self.statements(&[TokenType::End, TokenType::Else], 10)?;
                    self.eat(TokenType::End, [TokenType::Newline])?;

                    clauses.push((condition, body));
                }

                TokenType::Else => {
                    self.eat(TokenType::Newline, [])?;
                    let body = self.statements(&[TokenType::End], 10)?;

                    else_clause = Some(body);
                    self.eat(TokenType::End, [TokenType::Newline])?;

                    break;
                }

                TokenType::End => break,

                _ => unreachable!(),
            }
        }

        let stmt = self.context.store(Stmt::If {
            condition,
            body,
            clauses,
            else_clause,
        });

        Ok(stmt)
    }

    #[recursion_guard]
    fn match_stmt(&mut self) -> ParseResult<Ticket<'ctx, Stmt<'ctx>>> {
        self.eat(TokenType::Match, [TokenType::Newline])?;
        let var = self.expr()?;
        self.eat(TokenType::Newline, [])?;

        let mut arms = Vec::with_capacity(3);
        while self.peek()?.ty() != TokenType::End {
            let binding = self.binding()?;

            let guard = if self.peek()?.ty() == TokenType::Where {
                self.eat(TokenType::Where, [TokenType::Newline])?;
                let expr = self.expr()?;

                Some(expr)
            } else {
                None
            };
            self.eat(TokenType::RightRocket, [TokenType::Newline])?;
            self.eat(TokenType::Newline, [])?;

            let body = self.statements(&[TokenType::End], 5)?;

            self.eat(TokenType::End, [TokenType::Newline])?;
            self.eat(TokenType::Newline, [])?;

            arms.push((binding, guard, body));
        }

        self.eat(TokenType::End, [TokenType::Newline])?;

        let stmt = self.context.store(Stmt::Match { var, arms });

        Ok(stmt)
    }

    #[recursion_guard]
    fn while_stmt(&mut self) -> ParseResult<Ticket<'ctx, Stmt<'ctx>>> {
        self.eat(TokenType::While, [TokenType::Newline])?;
        let condition = self.expr()?;
        self.eat(TokenType::Newline, [])?;

        let body = self.statements(&[TokenType::End, TokenType::Then], 10)?;

        let then = self.then_stmt()?;

        let else_clause = self.else_stmt()?;

        self.eat(TokenType::End, [TokenType::Newline])?;

        let stmt = self.context.store(Stmt::While {
            condition,
            body,
            then,
            else_clause,
        });

        Ok(stmt)
    }

    #[recursion_guard]
    fn loop_stmt(&mut self) -> ParseResult<Ticket<'ctx, Stmt<'ctx>>> {
        self.eat(TokenType::Loop, [TokenType::Newline])?;
        self.eat(TokenType::Newline, [])?;

        let body = self.statements(&[TokenType::End, TokenType::Then], 10)?;

        let else_clause = self.else_stmt()?;

        self.eat(TokenType::End, [TokenType::Newline])?;

        let stmt = self.context.store(Stmt::Loop { body, else_clause });

        Ok(stmt)
    }

    #[recursion_guard]
    fn for_stmt(&mut self) -> ParseResult<Ticket<'ctx, Stmt<'ctx>>> {
        self.eat(TokenType::For, [TokenType::Newline])?;
        let var = self.expr()?;
        self.eat(TokenType::In, [TokenType::Newline])?;
        let condition = self.expr()?;
        self.eat(TokenType::Newline, [])?;

        let body = self.statements(&[TokenType::End, TokenType::Then], 10)?;
        let then = self.then_stmt()?;
        let else_clause = self.else_stmt()?;
        let stmt = self.context.store(Stmt::For {
            var,
            condition,
            body,
            then,
            else_clause,
        });

        Ok(stmt)
    }

    #[recursion_guard]
    fn then_stmt(&mut self) -> ParseResult<Option<Vec<Ticket<'ctx, Stmt<'ctx>>>>> {
        if self.peek()?.ty() == TokenType::Then {
            self.eat(TokenType::Then, [TokenType::Newline])?;
            self.eat(TokenType::Newline, [])?;

            let then = self.statements(&[TokenType::End, TokenType::Else], 3)?;

            Ok(Some(then))
        } else {
            Ok(None)
        }
    }

    #[recursion_guard]
    fn else_stmt(&mut self) -> ParseResult<Option<Vec<Ticket<'ctx, Stmt<'ctx>>>>> {
        if self.peek()?.ty() == TokenType::Else {
            self.eat(TokenType::Else, [TokenType::Newline])?;
            self.eat(TokenType::Newline, [])?;

            let else_clause = self.statements(&[TokenType::End], 3)?;

            Ok(Some(else_clause))
        } else {
            Ok(None)
        }
    }
}
