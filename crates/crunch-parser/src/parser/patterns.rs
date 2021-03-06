use crate::{parser::Parser, token::TokenType};
use crunch_shared::{
    crunch_proc::recursion_guard,
    error::ParseResult,
    tracing,
    trees::ast::{Binding, Pattern},
};

impl<'src, 'ctx> Parser<'src, 'ctx> {
    // TODO: Binding via patterns
    /// ```ebnf
    /// Binding ::= 'ref'? 'mut'? Pattern (':' Type)?
    /// ```
    #[recursion_guard]
    #[crunch_shared::instrument(name = "binding", skip(self))]
    pub(super) fn binding(&mut self) -> ParseResult<Binding<'ctx>> {
        let (mut reference, mut mutable) = (false, false);
        match self.peek()?.ty() {
            TokenType::Ref => {
                self.eat(TokenType::Ref, [])?;
                reference = true;

                if self.peek()?.ty() == TokenType::Mut {
                    self.eat(TokenType::Mut, [])?;
                    mutable = true;
                }
            }

            TokenType::Mut => {
                self.eat(TokenType::Mut, [])?;
                mutable = true;
            }

            _ => {}
        }

        let pattern = self.pattern()?;
        let ty = if self.peek().map(|t| t.ty()) == Ok(TokenType::Colon) {
            self.eat(TokenType::Colon, [])?;
            Some(self.ascribed_type()?)
        } else {
            None
        };

        Ok(Binding {
            reference,
            mutable,
            pattern,
            ty,
        })
    }

    /// ```ebnf
    /// Pattern ::= Literal | Ident | ItemPath
    /// ```
    #[recursion_guard]
    #[crunch_shared::instrument(name = "pattern", skip(self))]
    fn pattern(&mut self) -> ParseResult<Pattern<'ctx>> {
        let token = self.eat_of(
            [
                TokenType::Ident,
                TokenType::Int,
                TokenType::Bool,
                TokenType::Float,
                TokenType::String,
                TokenType::Rune,
            ],
            [TokenType::Newline],
        )?;

        let pattern = match token.ty() {
            TokenType::Int
            | TokenType::Bool
            | TokenType::Float
            | TokenType::String
            | TokenType::Rune => Pattern::Literal(self.literal(&token, self.current_file)?),

            TokenType::Ident => {
                let ident = self.intern_ident(token);

                if self.peek().map(|t| t.ty()) == Ok(TokenType::Dot) {
                    Pattern::ItemPath(self.item_path(ident)?)
                } else {
                    Pattern::Ident(ident)
                }
            }

            token => unreachable!("Failed to handle token: {:?}", token),
        };

        Ok(pattern)
    }
}
