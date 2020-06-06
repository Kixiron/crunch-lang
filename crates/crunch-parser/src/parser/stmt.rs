use crate::{parser::Parser, token::TokenType};
#[cfg(feature = "no-std")]
use alloc::{format, vec::Vec};
use crunch_proc::recursion_guard;
use crunch_shared::{
    ast::{Block, Ref, Stmt, StmtKind, Type, VarDecl},
    error::{Error, Locatable, Location, ParseResult, SemanticError, Span, SyntaxError},
};

// TODO: Type ascription

/// Statement parsing
impl<'src> Parser<'src> {
    #[recursion_guard]
    pub fn stmt(&mut self) -> ParseResult<Option<Stmt>> {
        match self.peek()?.ty() {
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

                let (name, _span) = {
                    let ident = self.eat(TokenType::Ident, [TokenType::Newline])?;
                    (self.context.strings.intern(ident.source()), ident.span())
                };

                self.eat(TokenType::Colon, [TokenType::Newline])?;
                if self.peek()?.ty() == TokenType::Newline {
                    self.eat(TokenType::Newline, [])?;
                }

                let ty = if self.peek()?.ty() == TokenType::Equal {
                    self.eat(TokenType::Equal, [])?;
                    Type::default()
                } else {
                    let ty = self.ascribed_type()?;
                    self.eat(TokenType::Colon, [TokenType::Newline])?;
                    self.eat(TokenType::Equal, [])?;
                    ty
                };

                let val = Ref::new(self.expr()?);
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

                let kind = StmtKind::VarDecl(Ref::new(VarDecl {
                    name,
                    ty: Ref::new(ty),
                    val,
                    constant,
                    mutable,
                }));

                Ok(Some(Stmt { kind }))
            }

            TokenType::Empty => {
                self.eat(TokenType::Empty, [TokenType::Newline])?;
                self.eat(TokenType::Newline, [])?;

                let stmt = Stmt {
                    kind: StmtKind::Empty,
                };

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
                let expr = Ref::new(self.expr()?);
                self.eat(TokenType::Newline, [])?;
                let kind = StmtKind::Expr(expr);

                Ok(Some(Stmt { kind }))
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
    pub(super) fn block(&mut self, breaks: &[TokenType], capacity: usize) -> ParseResult<Block> {
        let mut stmts = Vec::with_capacity(capacity);

        while let Ok(true) = self.peek().map(|p| !breaks.contains(&p.ty())) {
            let stmt = self.stmt()?;
            if let Some(stmt) = stmt {
                stmts.push(stmt);
            }
        }

        Ok(Block { stmts })
    }
}
