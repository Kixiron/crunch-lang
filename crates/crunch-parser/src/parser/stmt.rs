use crate::{
    error::{Error, Locatable, Location, ParseResult, SemanticError, Span, SyntaxError},
    parser::{Expr, Expression, Literal, Parser, Type},
    token::TokenType,
};

use crunch_proc::recursion_guard;
use lasso::Spur;
use stadium::Ticket;

#[cfg(feature = "no-std")]
use alloc::{format, vec::Vec};

pub type Stmt<'expr, 'stmt> = Ticket<'stmt, Statement<'expr, 'stmt>>;

#[derive(Debug, Clone, PartialEq)]
pub enum Statement<'expr, 'stmt> {
    If {
        condition: Expr<'expr>,
        body: Vec<Stmt<'stmt, 'expr>>,
        arm: Option<Stmt<'stmt, 'expr>>,
    },
    Expression(Expr<'expr>),
    VarDeclaration {
        name: Spur,
        ty: Locatable<Type>,
        val: Expr<'expr>,
        constant: bool,
        mutable: bool,
    },
    Return(Option<Expr<'expr>>),
    Break(Option<Expr<'expr>>),
    Continue,
    While {
        condition: Expr<'expr>,
        body: Vec<Stmt<'stmt, 'expr>>,
        then: Option<Vec<Stmt<'stmt, 'expr>>>,
    },
    Loop {
        body: Vec<Stmt<'stmt, 'expr>>,
        then: Option<Vec<Stmt<'stmt, 'expr>>>,
    },
    For {
        var: Expr<'expr>,
        condition: Expr<'expr>,
        body: Vec<Stmt<'stmt, 'expr>>,
        then: Option<Vec<Stmt<'stmt, 'expr>>>,
    },
    Match {
        var: Expr<'expr>,
        arms: Vec<(Spur, Option<Expr<'expr>>, Vec<Stmt<'stmt, 'expr>>)>,
    },
    Empty,
}

// TODO: Type ascription

/// Statement parsing
impl<'src, 'expr, 'stmt> Parser<'src, 'expr, 'stmt> {
    #[recursion_guard]
    pub fn stmt(&mut self) -> ParseResult<Option<Stmt<'stmt, 'expr>>> {
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
                    (self.string_interner.intern(ident.source()), ident.span())
                };

                self.eat(TokenType::Colon, [TokenType::Newline])?;
                if self.peek()?.ty() == TokenType::Newline {
                    self.eat(TokenType::Newline, [])?;
                }

                let ty = if self.peek()?.ty() == TokenType::Equal {
                    self.eat(TokenType::Equal, [])?;
                    Locatable::new(Type::default(), Location::implicit(span, self.current_file))
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

                let stmt = self.stmt_arena.store(Statement::VarDeclaration {
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
                    let stmt = self.stmt_arena.store(Statement::Return(None));

                    Ok(Some(stmt))
                } else {
                    let expr = self.expr()?;
                    self.eat(TokenType::Newline, [])?;
                    let stmt = self.stmt_arena.store(Statement::Return(Some(expr)));

                    Ok(Some(stmt))
                }
            }

            TokenType::Break => {
                self.eat(TokenType::Break, [TokenType::Newline])?;

                if self.peek()?.ty() == TokenType::Newline {
                    self.eat(TokenType::Newline, [])?;
                    let stmt = self.stmt_arena.store(Statement::Break(None));

                    Ok(Some(stmt))
                } else {
                    let expr = self.expr()?;
                    self.eat(TokenType::Newline, [])?;
                    let stmt = self.stmt_arena.store(Statement::Break(Some(expr)));

                    Ok(Some(stmt))
                }
            }

            TokenType::Continue => {
                self.eat(TokenType::Continue, [TokenType::Newline])?;
                self.eat(TokenType::Newline, [])?;

                let stmt = self.stmt_arena.store(Statement::Continue);

                Ok(Some(stmt))
            }

            TokenType::Empty => {
                self.eat(TokenType::Empty, [TokenType::Newline])?;
                self.eat(TokenType::Newline, [])?;

                let stmt = self.stmt_arena.store(Statement::Empty);

                Ok(Some(stmt))
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
                let expr = self.expr()?;
                self.eat(TokenType::Newline, [])?;
                let stmt = self.stmt_arena.store(Statement::Expression(expr));

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
    ) -> ParseResult<Vec<Stmt<'stmt, 'expr>>> {
        let mut statements = Vec::with_capacity(capacity);

        let mut peek = self.peek()?.ty();
        while !breaks.contains(&peek) {
            let stmt = self.stmt()?;
            if let Some(stmt) = stmt {
                statements.push(stmt);
            }

            peek = self.peek()?.ty();
        }
        statements.shrink_to_fit();

        Ok(statements)
    }

    #[recursion_guard]
    fn if_stmt(&mut self) -> ParseResult<Stmt<'stmt, 'expr>> {
        self.eat(TokenType::If, [TokenType::Newline])?;
        let condition = self.expr()?;
        self.eat(TokenType::Newline, [])?;

        let body = self.statements(&[TokenType::End, TokenType::Else], 10)?;

        let delimiter = self.eat_of([TokenType::Else, TokenType::End], [TokenType::Newline])?;
        let arm = match delimiter.ty() {
            TokenType::Else if self.peek()?.ty() == TokenType::If => {
                let stmt = self.if_stmt()?;

                Some(stmt)
            }

            TokenType::Else => {
                self.eat(TokenType::Newline, [])?;

                let body = self.statements(&[TokenType::End], 10)?;
                let condition = self
                    .expr_arena
                    .store(Expression::Literal(Literal::Bool(true)));
                let stmt = self.stmt_arena.store(Statement::If {
                    condition,
                    body,
                    arm: None,
                });

                Some(stmt)
            }

            TokenType::End => None,

            _ => unreachable!(),
        };

        let stmt = self.stmt_arena.store(Statement::If {
            condition,
            body,
            arm,
        });

        Ok(stmt)
    }

    #[recursion_guard]
    fn match_stmt(&mut self) -> ParseResult<Stmt<'stmt, 'expr>> {
        self.eat(TokenType::Match, [TokenType::Newline])?;
        let var = self.expr()?;
        self.eat(TokenType::Newline, [])?;

        let mut arms = Vec::with_capacity(3);
        while self.peek()?.ty() != TokenType::End {
            let capture = {
                let ident = self.eat(TokenType::Ident, [TokenType::Newline])?;
                self.string_interner.intern(ident.source())
            };

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

            arms.push((capture, guard, body));
        }
        arms.shrink_to_fit();

        self.eat(TokenType::End, [TokenType::Newline])?;

        let stmt = self.stmt_arena.store(Statement::Match { var, arms });

        Ok(stmt)
    }

    #[recursion_guard]
    fn while_stmt(&mut self) -> ParseResult<Stmt<'stmt, 'expr>> {
        self.eat(TokenType::While, [TokenType::Newline])?;
        let condition = self.expr()?;
        self.eat(TokenType::Newline, [])?;

        let body = self.statements(&[TokenType::End, TokenType::Then], 10)?;

        let then = self.then_stmt()?;

        self.eat(TokenType::End, [TokenType::Newline])?;

        let stmt = self.stmt_arena.store(Statement::While {
            condition,
            body,
            then,
        });

        Ok(stmt)
    }

    #[recursion_guard]
    fn loop_stmt(&mut self) -> ParseResult<Stmt<'stmt, 'expr>> {
        self.eat(TokenType::Loop, [TokenType::Newline])?;
        self.eat(TokenType::Newline, [])?;

        let body = self.statements(&[TokenType::End, TokenType::Then], 10)?;

        let then = self.then_stmt()?;

        self.eat(TokenType::End, [TokenType::Newline])?;

        let stmt = self.stmt_arena.store(Statement::Loop { body, then });

        Ok(stmt)
    }

    #[recursion_guard]
    fn for_stmt(&mut self) -> ParseResult<Stmt<'stmt, 'expr>> {
        self.eat(TokenType::For, [TokenType::Newline])?;
        let var = self.expr()?;
        self.eat(TokenType::In, [TokenType::Newline])?;
        let condition = self.expr()?;
        self.eat(TokenType::Newline, [])?;

        let body = self.statements(&[TokenType::End, TokenType::Then], 10)?;
        let then = self.then_stmt()?;
        let stmt = self.stmt_arena.store(Statement::For {
            var,
            condition,
            body,
            then,
        });

        Ok(stmt)
    }

    #[recursion_guard]
    fn then_stmt(&mut self) -> ParseResult<Option<Vec<Stmt<'stmt, 'expr>>>> {
        if self.peek()?.ty() == TokenType::Then {
            self.eat(TokenType::Then, [TokenType::Newline])?;
            self.eat(TokenType::Newline, [])?;

            let then = self.statements(&[TokenType::End, TokenType::Then], 3)?;

            Ok(Some(then))
        } else {
            Ok(None)
        }
    }
}
