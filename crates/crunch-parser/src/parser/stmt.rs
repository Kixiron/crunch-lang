use crate::{
    parser::Parser,
    token::{Token, TokenType},
};
#[cfg(feature = "no-std")]
use alloc::vec::Vec;
use crunch_shared::{
    crunch_proc::recursion_guard,
    error::{Error, Locatable, Location, ParseResult, SemanticError, Span},
    trees::ast::{Block, Stmt, StmtKind, Type, VarDecl},
};

// TODO: Type ascription

/// Statement parsing
impl<'src, 'ctx> Parser<'src, 'ctx> {
    #[recursion_guard]
    pub fn stmt(&mut self) -> ParseResult<Option<&'ctx Stmt<'ctx>>> {
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

                let (name, span) = {
                    let ident = self.eat(TokenType::Ident, [TokenType::Newline])?;
                    (self.intern_ident(ident), ident.span())
                };

                self.eat(TokenType::Colon, [TokenType::Newline])?;
                if self.peek()?.ty() == TokenType::Newline {
                    self.eat(TokenType::Newline, [])?;
                }

                let ty = if self.peek()?.ty() == TokenType::Equal {
                    self.eat(TokenType::Equal, [])?;
                    Locatable::new(
                        self.context.ast_type(Type::Unknown),
                        Location::new(span, self.current_file),
                    )
                } else {
                    let ty = self.ascribed_type()?;
                    self.eat(TokenType::Colon, [TokenType::Newline])?;
                    self.eat(TokenType::Equal, [])?;

                    ty
                };

                let val = self.expr()?;
                self.eat(TokenType::Newline, [])?;

                if constant && mutable {
                    return Err(Locatable::new(
                        Error::Semantic(SemanticError::MutableConstant),
                        Location::new(
                            Span::merge(start_token.span(), val.span()),
                            self.current_file,
                        ),
                    ));
                }

                let loc = Location::new(
                    Span::merge(start_token.span(), val.span()),
                    self.current_file,
                );
                let kind = StmtKind::VarDecl(VarDecl {
                    name,
                    ty,
                    val,
                    constant,
                    mutable,
                });

                Ok(Some(self.context.ast_stmt(Stmt { kind, loc })))
            }

            // Items
            TokenType::AtSign
            | TokenType::Exposed
            | TokenType::Package
            | TokenType::Function
            | TokenType::Type
            | TokenType::Extend
            | TokenType::Trait
            | TokenType::Import
            | TokenType::Alias => {
                let item = self.item()?.expect("An item should have been parsed");

                let loc = item.location();
                let kind = StmtKind::Item(item);

                Ok(Some(self.context.ast_stmt(Stmt { kind, loc })))
            }

            // Expressions
            _ => {
                let expr = self.expr()?;
                let end = self.eat(TokenType::Newline, [])?.span();

                let loc = Location::new(Span::merge(expr.span(), end), self.current_file);
                let kind = StmtKind::Expr(expr);

                Ok(Some(self.context.ast_stmt(Stmt { kind, loc })))
            }
        }
    }

    #[recursion_guard]
    pub(super) fn block(
        &mut self,
        breaks: &[TokenType],
        capacity: usize,
    ) -> ParseResult<Block<'ctx>> {
        Ok(self.block_returning(breaks, capacity)?.0)
    }

    pub(super) fn block_returning(
        &mut self,
        breaks: &[TokenType],
        capacity: usize,
    ) -> ParseResult<(Block<'ctx>, Token<'src>)> {
        let start = self.peek()?.span();

        let mut stmts = Vec::with_capacity(capacity);
        while let Ok(true) = self.peek().map(|p| !breaks.contains(&p.ty())) {
            let stmt = self.stmt()?;

            if let Some(stmt) = stmt {
                stmts.push(stmt);
            }
        }

        let end = self.eat_of(breaks, [TokenType::Newline])?;

        Ok((
            Block {
                stmts,
                loc: Location::new(Span::merge(start, end.span()), self.current_file),
            },
            end,
        ))
    }
}
