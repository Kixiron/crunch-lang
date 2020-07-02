use crate::{parser::Parser, token::TokenType};
#[cfg(feature = "no-std")]
use alloc::vec::Vec;
use crunch_shared::{
    crunch_proc::recursion_guard,
    error::{Error, Locatable, Location, ParseResult, SemanticError, Span},
    trees::{
        ast::{Block, Stmt, StmtKind, Type, VarDecl},
        Ref,
    },
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
                    Type::Infer
                } else {
                    let ty = self.ascribed_type()?;
                    self.eat(TokenType::Colon, [TokenType::Newline])?;
                    self.eat(TokenType::Equal, [])?;
                    ty
                };

                let val = Ref::new(self.expr()?);
                self.eat(TokenType::Newline, [])?;

                if constant && mutable {
                    return Err(Locatable::new(
                        Error::Semantic(SemanticError::MutableConstant),
                        Location::concrete(
                            Span::merge(start_token.span(), val.span()),
                            self.current_file,
                        ),
                    ));
                }

                let loc = Location::concrete(
                    Span::merge(start_token.span(), val.span()),
                    self.current_file,
                );
                let kind = StmtKind::VarDecl(Ref::new(VarDecl {
                    name,
                    ty: Ref::new(ty),
                    val,
                    constant,
                    mutable,
                }));

                Ok(Some(Stmt { kind, loc }))
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
                let item = Ref::new(self.item()?.expect("An item should have been parsed"));

                let loc = item.location();
                let kind = StmtKind::Item(item);

                Ok(Some(Stmt { kind, loc }))
            }

            // Expressions
            _ => {
                let expr = Ref::new(self.expr()?);
                let end = self.eat(TokenType::Newline, [])?.span();

                let loc = Location::concrete(Span::merge(expr.span(), end), self.current_file);
                let kind = StmtKind::Expr(expr);

                Ok(Some(Stmt { kind, loc }))
            }
        }
    }

    #[recursion_guard]
    pub(super) fn block(&mut self, breaks: &[TokenType], capacity: usize) -> ParseResult<Block> {
        let mut stmts = Vec::with_capacity(capacity);

        let (mut start, mut end) = (None, None);
        while let Ok(true) = self.peek().map(|p| !breaks.contains(&p.ty())) {
            let stmt = self.stmt()?;

            if let Some(stmt) = stmt {
                start = start.or_else(|| Some(stmt.location().span()));
                end = Some(stmt.location().span());

                stmts.push(stmt);
            }
        }

        Ok(Block {
            stmts,
            loc: Location::concrete(
                Span::merge(
                    start.unwrap_or(self.current_file.index_span()),
                    end.unwrap_or(self.current_file.index_span()),
                ),
                self.current_file,
            ),
        })
    }
}
