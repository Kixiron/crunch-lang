use crate::{
    error::{Locatable, ParseResult},
    parser::{ItemPath, Literal, Parser, Type},
    token::TokenType,
};

use core::convert::TryFrom;
use crunch_proc::recursion_guard;
use lasso::Spur;
#[cfg(test)]
use serde::Serialize;

impl<'src, 'stmt, 'expr> Parser<'src, 'stmt, 'expr> {
    // TODO: Binding via patterns
    /// ```ebnf
    /// Binding ::= 'ref'? 'mut'? Pattern (':' Type)?
    /// ```
    #[recursion_guard]
    pub(super) fn binding(&mut self) -> ParseResult<Binding> {
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
    fn pattern(&mut self) -> ParseResult<Pattern> {
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
            | TokenType::Rune => Pattern::Literal(Literal::try_from((&token, self.current_file))?),

            TokenType::Ident => {
                let ident = self.string_interner.intern(token.source);

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

#[cfg_attr(test, derive(Serialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct Binding {
    // TODO: Enum for mutability/referential status?
    pub reference: bool,
    pub mutable: bool,
    pub pattern: Pattern,
    pub ty: Option<Locatable<Type>>,
}

// TODO: More patterns
#[cfg_attr(test, derive(Serialize))]
#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Literal(Literal),
    Ident(Spur),
    ItemPath(ItemPath),
}
