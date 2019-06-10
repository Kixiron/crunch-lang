use crate::Token;
use logos::Logos;
use std::{iter::Iterator, ops::Range};

#[derive(Clone)]
pub struct TokenStream<'source>(Lexer<'source>, bool);

impl<'source> TokenStream<'source> {
    pub fn new(lexeme: &'source str) -> TokenStream<'source> {
        TokenStream(Token::lexer(lexeme), true)
    }
}

impl<'source> Iterator for TokenStream<'source> {
    type Item = TokenData<'source>;

    fn next(&mut self) -> Option<TokenData<'source>> {
        if !self.1 {
            self.0.advance();
        }
        self.1 = false;

        if let Token::EndOfFile = self.0.token {
            return None;
        }

        Some(TokenData {
            kind: self.0.token.clone(),
            source: self.0.slice(),
            range: self.0.range(),
        })
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct TokenData<'a> {
    pub kind: Token,
    pub source: <&'a str as logos::Source<'a>>::Slice,
    pub range: Range<usize>,
}

impl<'a> TokenData<'a> {
    pub fn kind(&self) -> &Token {
        &self.kind
    }

    pub fn source(&self) -> <&'a str as logos::Source<'a>>::Slice {
        self.source
    }

    pub fn range(&self) -> &Range<usize> {
        &self.range
    }
}

type Lexer<'source> = logos::Lexer<Token, &'source str>;
