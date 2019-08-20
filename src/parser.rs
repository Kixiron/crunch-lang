use logos::{Lexer, Logos};
use std::{borrow::Cow, iter::Peekable};

#[derive(Debug)]
pub enum Node<'a> {
    Ident(Cow<'a, str>),
    Binding {
        ident: Box<Node<'a>>,
        ty: Ty<'a>,
        value: Box<Node<'a>>,
    },
    FunctionDecl {
        ident: Box<Node<'a>>,
        params: Vec<(Node<'a>, Ty<'a>)>,
        returns: Ty<'a>,
        body: Box<Node<'a>>,
    },
    Int(i32),
    Str(Cow<'a, str>),
    Block(Vec<Node<'a>>),
    FunctionCall {
        name: Cow<'a, str>,
        params: Vec<Node<'a>>,
    },
}

#[derive(Debug)]
pub enum Ty<'a> {
    Int,
    Str,
    Bool,
    Infer,
    Void,
    Custom(Cow<'a, str>),
}

impl<'a> From<Cow<'a, str>> for Ty<'a> {
    fn from(ty: Cow<'a, str>) -> Self {
        match &*ty {
            "int" => Ty::Int,
            "str" => Ty::Str,
            "bool" => Ty::Bool,
            "void" => Ty::Void,
            "_" => Ty::Infer,
            _ => Ty::Custom(ty),
        }
    }
}

pub fn parse<'a>(input: &'a str) -> Vec<Node<'a>> {
    let tokenstream = TokenStream::new(input).peekable();
    let mut ast = Vec::new();

    {
        let result = match IntoIterator::into_iter(tokenstream) {
            mut iter => loop {
                let next;
                match iter.next() {
                    Some(val) => next = val,
                    None => break,
                };
                let current = next;

                {
                    if let Some(node) = process_token(current, &mut iter, &mut ast).1 {
                        ast.push(node);
                    }
                }
            },
        };
        result
    }

    ast
}

fn process_token<'a>(
    token: Token<'a>,
    mut iter: &mut Peekable<TokenStream<'a>>,
    mut ast: &mut Vec<Node<'a>>,
) -> (TokenType, Option<Node<'a>>) {
    match token.ty {
        TokenType::Let => {
            let ident = {
                let ident =
                    process_token(iter.next().expect("Expected Ident"), &mut iter, &mut ast);
                if ident.0 != TokenType::Ident || ident.1.is_none() {
                    panic!("Expected Ident");
                }

                Box::new(ident.1.unwrap())
            };

            {
                let equals =
                    process_token(iter.next().expect("Expected Ident"), &mut iter, &mut ast);

                if equals.0 != TokenType::Equal {
                    panic!("Expected =");
                }
            }

            let value = Box::new(
                process_token(iter.next().expect("Expected Ident"), &mut iter, &mut ast)
                    .1
                    .expect("No value"),
            );

            {
                let newline =
                    process_token(iter.next().expect("Expected Ident"), &mut iter, &mut ast);
                if newline.0 != TokenType::Newline {
                    panic!("Expected Newline");
                }
            }

            (
                TokenType::Let,
                Some(Node::Binding {
                    ident,
                    ty: Ty::Infer,
                    value,
                }),
            )
        }
        TokenType::Function => {
            let ident = {
                let ident =
                    process_token(iter.next().expect("Expected Ident"), &mut iter, &mut ast);
                if ident.0 != TokenType::Ident || ident.1.is_none() {
                    panic!("Expected Ident");
                }

                Box::new(ident.1.unwrap())
            };

            {
                let paren =
                    process_token(iter.next().expect("Expected Ident"), &mut iter, &mut ast);

                if paren.0 != TokenType::LeftParen {
                    panic!("Expected (");
                }
            }

            let mut params = Vec::new();
            loop {
                let param =
                    process_token(iter.next().expect("Expected Ident"), &mut iter, &mut ast);
                if param.0 == TokenType::RightParen {
                    break;
                } else if param.0 == TokenType::Ident {
                    let colon_or_comma =
                        process_token(iter.next().expect("Expected Ident"), &mut iter, &mut ast);

                    match colon_or_comma.0 {
                        TokenType::Colon => {
                            let ty = process_token(
                                iter.next().expect("Expected Ident"),
                                &mut iter,
                                &mut ast,
                            );

                            if let Node::Ident(ident) = ty.1.expect("Expected Type") {
                                params.push((param.1.expect("Expected Ident"), Ty::from(ident)));
                            } else {
                                panic!("Expected Ident");
                            }
                        }
                        TokenType::Comma => {
                            params.push((param.1.expect("Expected Ident"), Ty::Infer))
                        }
                        TokenType::RightParen => {
                            params.push((param.1.expect("Expected Ident"), Ty::Infer));
                            break;
                        }

                        _ => panic!("Expected parameter type"),
                    }
                } else {
                    panic!("Expected parameter");
                }
            }

            let returns = {
                let ty = process_token(iter.next().expect("Expected Ident"), &mut iter, &mut ast);

                match ty.0 {
                    TokenType::Newline => {
                        let returns = Ty::Void;

                        let body = Box::new(
                            process_token(
                                iter.next().expect("Expected Ident"),
                                &mut iter,
                                &mut ast,
                            )
                            .1
                            .expect("Expected body"),
                        );

                        return (
                            TokenType::Function,
                            Some(Node::FunctionDecl {
                                ident,
                                params,
                                returns,
                                body,
                            }),
                        );
                    }
                    TokenType::RightArrow => {
                        let ty = process_token(
                            iter.next().expect("Expected Ident"),
                            &mut iter,
                            &mut ast,
                        );
                        if let Node::Ident(ident) = ty.1.expect("Expected Type") {
                            Ty::from(ident)
                        } else {
                            panic!("Expected Ident");
                        }
                    }
                    _ => panic!("Expected return type"),
                }
            };

            {
                let newline =
                    process_token(iter.next().expect("Expected Ident"), &mut iter, &mut ast);
                if newline.0 != TokenType::Newline {
                    panic!("Expected Newline");
                }
            }

            let body = Box::new(
                process_token(iter.next().expect("Expected Ident"), &mut iter, &mut ast)
                    .1
                    .expect("Expected body"),
            );

            (
                TokenType::Function,
                Some(Node::FunctionDecl {
                    ident,
                    params,
                    returns,
                    body,
                }),
            )
        }
        TokenType::Equal => (TokenType::Equal, None),
        TokenType::Ident => {
            if let Some(__paren) = iter.peek() {
                if __paren.ty == TokenType::LeftParen {
                    let _ = iter.next();

                    let name = token.source;

                    let mut params = Vec::new();
                    loop {
                        let param = process_token(
                            iter.next().expect("Expected Ident"),
                            &mut iter,
                            &mut ast,
                        );
                        if param.0 == TokenType::RightParen {
                            break;
                        } else if param.0 == TokenType::Ident {
                            let __comma_or_paren = process_token(
                                iter.next().expect("Expected Ident"),
                                &mut iter,
                                &mut ast,
                            );

                            match __comma_or_paren.0 {
                                TokenType::Comma => params.push(param.1.expect("Expected Ident")),
                                TokenType::RightParen => {
                                    params.push(param.1.expect("Expected Ident"));
                                    break;
                                }

                                _ => panic!("Expected parameter type"),
                            }
                        } else {
                            panic!("Expected parameter");
                        }
                    }

                    {
                        let newline = process_token(
                            iter.next().expect("Expected Ident"),
                            &mut iter,
                            &mut ast,
                        );
                        if newline.0 != TokenType::Newline {
                            panic!("Expected Newline");
                        }
                    }

                    return (TokenType::Ident, Some(Node::FunctionCall { name, params }));
                }
            }

            (TokenType::Ident, Some(Node::Ident(token.source)))
        }
        TokenType::Space => {
            process_token(iter.next().expect("Unexpected EOF"), &mut iter, &mut ast)
        }
        TokenType::Int => (
            TokenType::Int,
            Some(Node::Int(token.source.parse().unwrap())),
        ),
        TokenType::String => (
            TokenType::String,
            Some(Node::Str(Cow::Owned(String::from(
                &token.source[1..token.source.len() - 1],
            )))),
        ),
        TokenType::LeftParen => (TokenType::LeftParen, None),
        TokenType::RightParen => (TokenType::RightParen, None),
        TokenType::Newline => (TokenType::Newline, None),
        TokenType::End => (TokenType::End, None),
        TokenType::Indent => {
            let mut operations =
                vec![
                    process_token(iter.next().expect("Unexpected EOF"), &mut iter, &mut ast)
                        .1
                        .expect("Expected Expression"),
                ];

            while let Some(Node::Block(operation)) = process_token(
                match iter.next() {
                    Some(token) => token,
                    None => return (TokenType::Indent, Some(Node::Block(operations))),
                },
                &mut iter,
                &mut ast,
            )
            .1
            {
                operations.extend(operation);
            }

            (TokenType::Indent, Some(Node::Block(operations)))
        }
        _ => panic!("{:?}", token),
    }
}

#[allow(missing_debug_implementations)]
pub struct TokenStream<'a> {
    next: Lexer<TokenType, &'a str>,
    current: Token<'a>,
    eof: bool,
}

impl<'a> TokenStream<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut lexer = TokenType::lexer(input);
        let current = Token::new(lexer.token, lexer.slice(), lexer.range());
        lexer.advance();

        Self {
            current,
            next: lexer,
            eof: false,
        }
    }

    fn advance(&mut self) {
        self.current = Token::new(self.next.token, self.next.slice(), self.next.range());

        self.next.advance();
    }
}

impl<'a> Iterator for TokenStream<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if !self.eof {
            let token = self.current.clone();

            println!("{:?}", token);

            self.advance();

            if token.ty == TokenType::End {
                self.eof = true;
            }

            Some(token)
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        println!(
            "{:?}",
            parse("fn test(ident)\n    let i = 10\n    let j = 11\n    print(i)\n")
        );
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Token<'a> {
    pub ty: TokenType,
    pub source: Cow<'a, str>,
    pub range: (usize, usize),
}

impl<'a> Token<'a> {
    pub fn new(ty: TokenType, source: &'a str, range: std::ops::Range<usize>) -> Self {
        Self {
            ty,
            source: Cow::Borrowed(source),
            range: (range.start, range.end),
        }
    }
}

#[derive(Logos, Debug, PartialEq, Eq, Clone, Copy)]
pub enum TokenType {
    #[end]
    End,
    #[error]
    Error,
    #[token = ","]
    Comma,
    #[token = "let"]
    Let,
    #[token = "+"]
    Plus,
    #[token = ":"]
    Colon,
    #[token = "-"]
    Minus,
    #[token = "="]
    Equal,
    #[token = "=="]
    IsEqual,
    #[token = "["]
    LeftBrace,
    #[token = "]"]
    RightBrace,
    #[token = "/"]
    Divide,
    #[token = "*"]
    Star,
    #[token = "loop"]
    Loop,
    #[token = "while"]
    While,
    #[token = "if"]
    If,
    #[token = "else"]
    Else,
    #[token = "fn"]
    Function,
    #[token = "("]
    LeftParen,
    #[token = ")"]
    RightParen,
    #[token = "{"]
    LeftBracket,
    #[token = "}"]
    RightBracket,
    #[regex = "[^ \t\n\r\"\'!@#$%\\^&*()-+=,.<>/?;:\\[\\]{}\\\\|`~]+"]
    Ident,
    #[regex = "[1234567890]+"]
    Int,
    #[regex = "'[^']'"]
    String,
    #[token = " "]
    Space,
    #[regex = "::.*\n"]
    Comment,
    #[token = "    "]
    Indent,
    #[token = "\n"]
    Newline,
    #[token = "->"]
    RightArrow,
    #[token = "<-"]
    LeftArrow,
}
