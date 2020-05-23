use crate::{
    error::{Location, ParseResult, Span},
    files::FileId,
    parser::{Ast, Expression, Parser, Statement},
    token::TokenType,
};
use alloc::{rc::Rc, vec, vec::Vec};
use core::{fmt, ops};
use crunch_proc::recursion_guard;
use lasso::Spur;
#[cfg(test)]
use serde::{Deserialize, Serialize};
use stadium::Stadium;

pub struct SyntaxTree<'expr, 'stmt> {
    pub(crate) ast: Vec<Ast<'expr, 'stmt>>,
    pub(crate) __exprs: Stadium<'expr, Expression<'expr>>,
    pub(crate) __stmts: Stadium<'stmt, Statement<'expr, 'stmt>>,
}

impl<'expr, 'stmt> fmt::Debug for SyntaxTree<'expr, 'stmt> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_list().entries(&self.ast).finish()
    }
}

impl<'expr, 'stmt> ops::Deref for SyntaxTree<'expr, 'stmt> {
    type Target = [Ast<'expr, 'stmt>];

    fn deref(&self) -> &Self::Target {
        &self.ast
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct CurrentFile {
    file: FileId,
    length: usize,
    index: usize,
}

impl CurrentFile {
    pub const fn new(file: FileId, length: usize) -> Self {
        Self {
            file,
            length,
            index: 0,
        }
    }

    pub const fn file(&self) -> FileId {
        self.file
    }

    pub const fn length(&self) -> usize {
        self.length
    }

    pub const fn index(&self) -> usize {
        self.index
    }

    pub fn eof(&self) -> Location {
        Location::concrete(Span::new(self.length, self.length), self.file)
    }

    pub fn advance(&mut self, dist: usize) {
        self.index += dist;
    }

    pub const fn index_span(&self) -> Span {
        Span::new(self.index, self.index)
    }

    pub fn recursion(&self) -> Location {
        Location::concrete(self.index_span(), self.file)
    }
}

impl Into<FileId> for CurrentFile {
    fn into(self) -> FileId {
        self.file
    }
}

#[derive(Debug, Clone)]
pub struct StackGuard(Rc<()>);

impl StackGuard {
    pub fn new() -> Self {
        Self(Rc::new(()))
    }

    pub fn frames(&self) -> usize {
        Rc::strong_count(&self.0)
    }
}

#[cfg_attr(test, derive(Deserialize, Serialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct ItemPath(Vec<Spur>);

impl ItemPath {
    pub fn new(path: impl Into<Self>) -> Self {
        path.into()
    }

    pub fn join(&self, other: impl Into<Self>) -> Self {
        let mut new = self.0.clone();
        new.extend(other.into().0.drain(..));

        Self(new)
    }
}

impl From<Spur> for ItemPath {
    fn from(seg: Spur) -> Self {
        Self(vec![seg])
    }
}

impl From<Vec<Spur>> for ItemPath {
    fn from(segs: Vec<Spur>) -> Self {
        Self(segs)
    }
}

impl ops::Deref for ItemPath {
    type Target = [Spur];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'src, 'stmt, 'expr> Parser<'src, 'stmt, 'expr> {
    /// ```ebnf
    /// ItemPath ::= Ident | Ident '.' Path
    /// ```
    #[recursion_guard]
    pub(crate) fn item_path(&mut self, path: impl Into<Option<Spur>>) -> ParseResult<ItemPath> {
        let mut path = if let Some(start) = path.into() {
            vec![start]
        } else {
            let segment = self.eat(TokenType::Ident, [TokenType::Newline])?.source();
            vec![self.string_interner.intern(segment)]
        };

        while self.peek()?.ty() != TokenType::Dot && self.peek()?.ty() == TokenType::Ident {
            if self.peek()?.ty() == TokenType::Newline {
                self.eat(TokenType::Newline, [])?;
                continue;
            }

            let segment = self.eat(TokenType::Ident, [TokenType::Newline])?.source();
            path.push(self.string_interner.intern(segment));

            self.eat(TokenType::Dot, [TokenType::Newline])?;
        }

        Ok(ItemPath::new(path))
    }
}

#[cfg(test)]
pub(super) fn serialize_u128_to_str<S: serde::Serializer>(
    int: &u128,
    serializer: S,
) -> Result<S::Ok, S::Error> {
    int.to_string().serialize(serializer)
}
