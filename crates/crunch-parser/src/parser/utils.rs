use crate::{
    context::StrT,
    error::{Location, ParseResult, Span},
    files::FileId,
    parser::Parser,
    token::TokenType,
};
use alloc::{rc::Rc, vec, vec::Vec};
use core::ops;
use crunch_proc::recursion_guard;
#[cfg(test)]
use serde::{Deserialize, Serialize};

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
pub struct ItemPath(Vec<StrT>);

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

impl From<StrT> for ItemPath {
    fn from(seg: StrT) -> Self {
        Self(vec![seg])
    }
}

impl From<Vec<StrT>> for ItemPath {
    fn from(segs: Vec<StrT>) -> Self {
        Self(segs)
    }
}

impl ops::Deref for ItemPath {
    type Target = [StrT];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'src, 'ctx> Parser<'src, 'ctx> {
    /// ```ebnf
    /// ItemPath ::= Ident | Ident '.' Path
    /// ```
    #[recursion_guard]
    pub(crate) fn item_path(&mut self, start: StrT) -> ParseResult<ItemPath> {
        let mut path = vec![start];

        if matches!(self.peek().map(|t| t.ty()), Ok(TokenType::Dot)) {
            self.eat(TokenType::Dot, [])?;
        } else {
            return Ok(ItemPath::new(path));
        }

        if let Ok(peek) = self.peek() {
            while peek.ty() == TokenType::Ident {
                let segment = self.eat(TokenType::Ident, [TokenType::Newline])?.source();
                path.push(self.context.intern(segment));

                if matches!(self.peek().map(|t| t.ty()), Ok(TokenType::Dot)) {
                    self.eat(TokenType::Dot, [TokenType::Newline])?;
                } else {
                    break;
                }
            }
        }

        Ok(ItemPath::new(path))
    }
}
