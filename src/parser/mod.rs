mod ast;
mod token;

pub use ast::*;

use codespan::{FileId, Files, Span};
use codespan_reporting::diagnostic::{Diagnostic, Label, Severity};
use std::{borrow::Cow, char, collections::VecDeque};
use token::*;

type Result<T> = std::result::Result<T, Diagnostic>;

#[derive(Debug)]
pub struct Parser<'a> {
    token_stream: TokenStream<'a>,
    next: Option<Token<'a>>,
    peek: Option<Token<'a>>,
    codespan: Files,
    files: Vec<FileId>,
    current_file: FileId,
    diagnostics: Vec<Diagnostic>,
    error: bool,
}

impl<'a> Parser<'a> {
    pub fn new(filename: Option<&'a str>, input: &'a str) -> Self {
        let mut token_stream = TokenStream::new(input.trim_start()); // The trim_start is a sketchy solution to the weird problem
                                                                     //  of a `\r\n` precluding a function decl causing everything
                                                                     // to break. Smallest reproducible set:
                                                                     // "\r\nfn main()\r\n\t@print \"Welp.\\n\"\r\n"
        let peek = token_stream.next();
        let next = token_stream.next();
        let (codespan, files) = {
            let mut files = Files::new();
            let ids = vec![if let Some(filename) = filename {
                files.add(filename, input)
            } else {
                files.add("Crunch Source File", input)
            }];
            (files, ids)
        };
        let current_file = files[0];
        let diagnostics = Vec::new();

        Self {
            token_stream,
            next,
            peek,
            codespan,
            files,
            current_file,
            diagnostics,
            error: false,
        }
    }

    pub fn parse(
        &mut self,
    ) -> std::result::Result<(Vec<Node<'a>>, Vec<Diagnostic>), Vec<Diagnostic>> {
        let mut ast = Vec::new();

        loop {
            if let Some(token) = self.next.clone() {
                match token.ty {
                    TokenType::Function => {
                        ast.push(Node::Func(match self.parse_function(token.range.0) {
                            Ok(node) => node,
                            Err(err) => {
                                self.diagnostics.push(err);
                                continue;
                            }
                        }));
                    }

                    TokenType::Import => ast.push(Node::Import(match self.parse_import() {
                        Ok(node) => node,
                        Err(err) => {
                            self.diagnostics.push(err);
                            continue;
                        }
                    })),

                    TokenType::End => break,

                    // This is apparently kinda flawed
                    TokenType::Space | TokenType::Comment | TokenType::Newline => {
                        if let Err(err) = self.next(line!()) {
                            self.diagnostics.push(err);
                        }
                        continue;
                    }

                    TokenType::Error => {
                        self.error = true;
                        self.diagnostics.push(Diagnostic::new(
                            Severity::Error,
                            "Invalid token",
                            Label::new(
                                self.files[0],
                                token.range.0 as u32..token.range.1 as u32, // What?
                                format!("{:?} is not a valid token", token.source),
                            ),
                        ));
                    }

                    _ => {
                        self.error = true;
                        self.diagnostics.push(Diagnostic::new(
                            Severity::Error,
                            "Invalid top-level token",
                            Label::new(
                                self.files[0],
                                token.range.0 as u32..token.range.1 as u32, // What?
                                format!("Expected Function, got {:?}", token.ty),
                            ),
                        ));
                    }
                }
            } else {
                break;
            }

            if self.next(line!()).is_err() {
                return if self.error {
                    let mut diagnostics = Vec::new();
                    std::mem::swap(&mut diagnostics, &mut self.diagnostics);

                    Err(diagnostics)
                } else {
                    let mut diagnostics = Vec::new();
                    std::mem::swap(&mut diagnostics, &mut self.diagnostics);

                    Ok((ast, diagnostics))
                };
            }
        }

        if self.error {
            let mut diagnostics = Vec::new();
            std::mem::swap(&mut diagnostics, &mut self.diagnostics);

            Err(diagnostics)
        } else {
            let mut diagnostics = Vec::new();
            std::mem::swap(&mut diagnostics, &mut self.diagnostics);

            Ok((ast, diagnostics))
        }
    }

    fn parse_import(&mut self) -> Result<Import<'a>> {
        use std::path::PathBuf;

        self.eat_w()?;

        let file: PathBuf = {
            let source = self.eat(TokenType::String)?.source;
            source[1..source.len() - 1].split('.').collect()
        };

        self.eat_w()?;

        let (exposes, alias) = match self.peek()?.ty {
            TokenType::Exposing => {
                self.eat(TokenType::Exposing)
                    .unwrap_or_else(|_| unreachable!("Peek was Exposing"));
                self.eat_w()?;

                let peek = self.peek()?;
                if peek.ty == TokenType::Star {
                    self.eat(TokenType::Star)
                        .unwrap_or_else(|_| unreachable!("Peek was Star"));

                    (Exposes::All, None)
                } else if peek.ty == TokenType::Ident {
                    let mut imports = Vec::new();
                    let mut peek = self.peek()?;

                    while peek.ty != TokenType::Newline {
                        let ident =
                            Ident::from_token(self.eat(TokenType::Ident)?, self.current_file);

                        self.eat_w()?;

                        let token = self.peek()?;
                        let alias = if token.ty == TokenType::As {
                            self.eat(TokenType::As)
                                .unwrap_or_else(|_| unreachable!("Peek was As"));

                            self.eat_w()?;

                            Some(Ident::from_token(
                                self.eat(TokenType::Ident)?,
                                self.current_file,
                            ))
                        } else {
                            None
                        };

                        imports.push((ident, alias));

                        self.eat_w()?;
                        peek = self.peek()?;

                        if peek.ty == TokenType::Comma {
                            self.eat(TokenType::Comma)
                                .unwrap_or_else(|_| unreachable!("Peek was Comma"));
                        } else {
                            break;
                        }
                    }

                    self.eat(TokenType::Newline)?;

                    (Exposes::Some(imports), None)
                } else {
                    self.error = true;
                    return Err(Diagnostic::new(
                        Severity::Error,
                        "Expected exposed members",
                        Label::new(
                            self.files[0],
                            self.codespan.source_span(self.files[0]),
                            format!("You must expose something when using the `exposing` keyword"),
                        ),
                    ));
                }
            }

            TokenType::As => {
                self.eat(TokenType::As)
                    .unwrap_or_else(|_| unreachable!("Peek was As"));

                let alias = Ident::from_token(self.eat(TokenType::Ident)?, self.current_file);

                if self.peek()?.ty == TokenType::Exposing {
                    self.eat(TokenType::Exposing)
                        .unwrap_or_else(|_| unreachable!("Peek was Exposing"));
                    self.eat_w()?;

                    let peek = self.peek()?;
                    if peek.ty == TokenType::Star {
                        self.eat(TokenType::Star)
                            .unwrap_or_else(|_| unreachable!("Peek was Star"));

                        (Exposes::All, Some(alias))
                    } else if peek.ty == TokenType::Ident {
                        let mut imports = Vec::new();
                        let mut peek = self.peek()?;

                        while peek.ty != TokenType::Newline {
                            let ident =
                                Ident::from_token(self.eat(TokenType::Ident)?, self.current_file);

                            self.eat_w()?;

                            let token = self.peek()?;
                            let alias = if token.ty == TokenType::As {
                                self.eat(TokenType::As)
                                    .unwrap_or_else(|_| unreachable!("Peek was As"));

                                self.eat_w()?;

                                Some(Ident::from_token(
                                    self.eat(TokenType::Ident)?,
                                    self.current_file,
                                ))
                            } else {
                                None
                            };

                            imports.push((ident, alias));

                            self.eat_w()?;
                            peek = self.peek()?;

                            if peek.ty == TokenType::Comma {
                                self.eat(TokenType::Comma)
                                    .unwrap_or_else(|_| unreachable!("Peek was Comma"));
                            } else {
                                break;
                            }
                        }

                        self.eat(TokenType::Newline)?;

                        (Exposes::Some(imports), Some(alias))
                    } else {
                        self.error = true;
                        return Err(Diagnostic::new(
                            Severity::Error,
                            "Expected exposed members",
                            Label::new(
                                self.files[0],
                                self.codespan.source_span(self.files[0]),
                                format!(
                                    "You must expose something when using the `exposing` keyword"
                                ),
                            ),
                        ));
                    }
                } else {
                    (Exposes::All, Some(alias))
                }
            }

            _ => {
                self.eat_w()?;
                self.eat(TokenType::Newline)?;

                (Exposes::File, None)
            }
        };

        Ok(Import {
            file,
            alias,
            exposes,
        })
    }

    // fn parse_type_decl(&mut self, span_start: u32) -> Result<TypeDecl<'a>> {
    //     let name = Ident::from_token(self.eat(TokenType::Ident)?, self.current_file);
    //
    //     self.eat_of(&[TokenType::Newline, TokenType::Comment])?;
    //
    //     let (mut members, mut methods) = (Vec::new(), Vec::new());
    //     let mut span_end = span_start;
    //     while let Ok(token) = self.next(line!()) {
    //         if token.ty == TokenType::Indent {
    //             let member_or_method = self.parse_type_body(token.range.0)?;
    //             match member_or_method {
    //                 TypeMemberOrMethod::Method(method) => methods.push(method),
    //                 TypeMemberOrMethod::Member(member) => members.push(member),
    //             }
    //             span_end = token.range.1;
    //         } else {
    //             break;
    //         }
    //     }
    //
    //     Ok(TypeDecl {
    //         name,
    //         members,
    //         methods,
    //     })
    // }

    fn parse_function(&mut self, span_start: u32) -> Result<Func<'a>> {
        let name = Ident::from_token(self.eat(TokenType::Ident)?, self.current_file);

        self.eat(TokenType::LeftParen)?;

        self.eat_w()?;
        let mut params = Vec::new();
        while self.peek()?.ty != TokenType::RightParen {
            params.push(self.parse_named_parameter()?);
            self.eat_w()?;

            if self.peek()?.ty == TokenType::Comma {
                self.eat(TokenType::Comma)
                    .unwrap_or_else(|_| unreachable!("Peek was Comma"));
            }
        }

        self.eat(TokenType::RightParen)?;

        self.eat_w()?;
        let returns = if self.peek()?.ty == TokenType::RightArrow {
            self.eat(TokenType::RightArrow)
                .unwrap_or_else(|_| unreachable!("Peek was RightArrow"));
            self.parse_type()?.0
        } else {
            Type::Infer
        };

        self.eat_of(&[TokenType::Newline, TokenType::Comment])?;

        self.eat_w()?;
        let mut body = Vec::new();
        let mut span_end = span_start;
        while let Ok(token) = self.next(line!()) {
            if token.ty == TokenType::Indent {
                body.push(self.parse_func_body(token.range.0)?);
                span_end = token.range.1;
            } else if [TokenType::Newline, TokenType::Space].contains(&token.ty) {
                continue;
            } else {
                break;
            }
        }

        Ok(Func {
            name,
            params,
            returns,
            body,
            info: LocInfo {
                span: Span::new(span_start, span_end),
                file: self.current_file,
            },
        })
    }

    fn parse_func_body(&mut self, span_start: u32) -> Result<FuncBody<'a>> {
        self.eat_w()?;

        let token = self.next(line!())?;
        let (expr, span_end): (FuncExpr<'a>, u32) = match token.ty {
            TokenType::Let => {
                let name = Ident::from_token(self.eat(TokenType::Ident)?, self.current_file);

                let ty = if self.peek().unwrap().ty == TokenType::Colon {
                    self.eat(TokenType::Colon)
                        .unwrap_or_else(|_| unreachable!("Peek was Colon"));;
                    self.parse_type()?.0
                } else {
                    Type::Infer
                };

                self.eat(TokenType::Equal)?;

                let val = self.parse_binding_val()?;

                let span_end = self
                    .eat_of(&[TokenType::Newline, TokenType::Comment])?
                    .range
                    .1;

                (
                    FuncExpr::Binding(Binding {
                        name,
                        val,
                        ty,
                        info: LocInfo {
                            span: Span::new(span_start, span_end),
                            file: self.current_file,
                        },
                    }),
                    span_end,
                )
            }
            TokenType::Print => {
                self.eat_w()?;
                let mut params = Vec::new();

                let mut peek = self.peek()?;
                while peek.ty != TokenType::Newline && peek.ty != TokenType::Comma {
                    self.eat_w()?;

                    params.push(match peek.ty {
                        TokenType::String | TokenType::Bool | TokenType::Int => {
                            IdentLiteral::Literal(self.parse_variable()?)
                        }
                        TokenType::Ident => IdentLiteral::Variable(Ident::from_token(
                            self.eat(TokenType::Ident)?,
                            self.current_file,
                        )),
                        t => {
                            self.error = true;
                            return Err(Diagnostic::new(
                                Severity::Error,
                                "Invalid Function Parameter",
                                Label::new(
                                    self.files[0],
                                    self.codespan.source_span(self.files[0]),
                                    format!("Expected Ident or Literal, got {:?}", t),
                                ),
                            ));
                        }
                    });

                    self.eat_w()?;

                    if self.peek()?.ty == TokenType::Comma {
                        self.eat(TokenType::Comma)
                            .unwrap_or_else(|_| unreachable!("Peek was Comma"));
                    }

                    self.eat_w()?;

                    peek = self.peek()?;
                }

                let span_end = self
                    .eat_of(&[TokenType::Newline, TokenType::Comment])?
                    .range
                    .1;

                (FuncExpr::Builtin(Builtin::Print(params)), span_end)
            }
            TokenType::Collect => (FuncExpr::Builtin(Builtin::Collect), token.range.1),
            TokenType::Halt => (FuncExpr::Builtin(Builtin::Halt), token.range.1),
            TokenType::SyscallExit => {
                self.eat_w()?;

                let peek = self.peek()?;
                let exit_code = if peek.ty != TokenType::Newline && peek.ty != TokenType::Comma {
                    self.eat_w()?;

                    match peek.ty {
                        TokenType::String | TokenType::Bool | TokenType::Int => {
                            IdentLiteral::Literal(self.parse_variable()?)
                        }
                        TokenType::Ident => IdentLiteral::Variable(Ident::from_token(
                            self.eat(TokenType::Ident)?,
                            self.current_file,
                        )),
                        t => {
                            self.error = true;
                            return Err(Diagnostic::new(
                                Severity::Error,
                                "Invalid Function Parameter",
                                Label::new(
                                    self.files[0],
                                    self.codespan.source_span(self.files[0]),
                                    format!("Expected Ident or Literal, got {:?}", t),
                                ),
                            ));
                        }
                    }
                } else {
                    self.error = true;
                    return Err(Diagnostic::new(
                        Severity::Error,
                        "Invalid Function Parameters",
                        Label::new(
                            self.files[0],
                            self.codespan.source_span(self.files[0]),
                            format!("Exit takes one parameter"),
                        ),
                    ));
                };

                let span_end = self
                    .eat_of(&[TokenType::Newline, TokenType::Comment])?
                    .range
                    .1;

                (FuncExpr::Builtin(Builtin::SyscallExit(exit_code)), span_end)
            }
            _ => unimplemented!(),
        };

        Ok(FuncBody {
            expr,
            info: LocInfo {
                span: Span::new(span_start, span_end),
                file: self.current_file,
            },
        })
    }

    fn parse_binding_val(&mut self) -> Result<BindingVal<'a>> {
        self.eat_w()?;

        Ok(match self.peek()?.ty {
            TokenType::String | TokenType::Bool | TokenType::Int | TokenType::Ident => {
                let left = self.parse_bin_op_side()?;
                self.eat_w()?;
                let peek = self.peek()?;
                if [
                    TokenType::Plus,
                    TokenType::Minus,
                    TokenType::Divide,
                    TokenType::Star,
                ]
                .contains(&peek.ty)
                {
                    self.eat_of(&[
                        TokenType::Plus,
                        TokenType::Minus,
                        TokenType::Divide,
                        TokenType::Star,
                    ])?;

                    BindingVal::BinOp(BinOp {
                        op: Op::from(peek.ty),
                        left,
                        right: self.parse_bin_op_side()?,
                        info: LocInfo {
                            span: Span::new(0, 0),
                            file: self.current_file,
                        },
                    })
                } else {
                    match left {
                        BinOpSide::Literal(lit) => BindingVal::Literal(lit),
                        BinOpSide::Variable(var) => BindingVal::Variable(var),
                    }
                }
            }
            _ => crate::unreachable!("Impossible Token: {:?}", self.peek()),
        })
    }

    fn parse_bin_op_side(&mut self) -> Result<BinOpSide<'a>> {
        self.eat_w()?;

        let token = self.peek()?;
        match token.ty {
            TokenType::Ident => {
                self.next(line!())?;
                Ok(BinOpSide::Variable(Ident::from_token(
                    token,
                    self.current_file,
                )))
            }
            TokenType::String | TokenType::Bool | TokenType::Int => {
                Ok(BinOpSide::Literal(self.parse_variable()?))
            }
            _ => unimplemented!(),
        }
    }

    #[inline]
    fn escape_string(&mut self, string: &str) -> Result<String> {
        let mut queue: VecDeque<_> = String::from(string).chars().collect();
        let mut s = String::new();
        let error = Diagnostic::new(
            Severity::Error,
            "Invalid Escape String",
            Label::new(
                self.files[0],
                self.codespan.source_span(self.files[0]),
                "Invalid Escape String".to_string(),
            ),
        );

        while let Some(c) = queue.pop_front() {
            if c != '\\' {
                s.push(c);
                continue;
            }

            match queue.pop_front() {
                Some('n') => s.push('\n'),
                Some('r') => s.push('\r'),
                Some('t') => s.push('\t'),
                Some('\'') => s.push('\''),
                Some('\"') => s.push('\"'),
                Some('\\') => s.push('\\'),
                Some('u') => s.push(match unescape_unicode(&mut queue) {
                    Some(c) => c,
                    None => {
                        self.error = true;
                        return Err(error);
                    }
                }),
                Some('x') => s.push(match unescape_byte(&mut queue) {
                    Some(c) => c,
                    None => {
                        self.error = true;
                        return Err(error);
                    }
                }),
                Some('b') => s.push(match unescape_bits(&mut queue) {
                    Some(c) => c,
                    None => {
                        self.error = true;
                        return Err(error);
                    }
                }),
                _ => {
                    self.error = true;
                    return Err(error);
                }
            };
        }

        Ok(s)
    }

    #[inline]
    fn parse_variable(&mut self) -> Result<Literal<'a>> {
        self.eat_w()?;
        let token = self.next(line!())?;

        let (span_start, span_end) = token.range;
        let val = match token.ty {
            TokenType::Int => LiteralInner::Int(token.source.parse().unwrap()),
            TokenType::String => LiteralInner::String(Cow::Owned(
                self.escape_string(&(&*token.source)[1..token.source.len() - 1])?,
            )),
            TokenType::Bool => LiteralInner::Bool(token.source.parse().unwrap()),
            _ => unimplemented!("{:?}", token.ty),
        };

        Ok(Literal {
            val,
            info: LocInfo {
                span: Span::new(span_start, span_end),
                file: self.current_file,
            },
        })
    }

    #[inline]
    fn parse_named_parameter(&mut self) -> Result<FuncParam<'a>> {
        let name = Ident::from_token(self.eat(TokenType::Ident)?, self.current_file);
        let span_start = name.info.span.start();

        self.eat(TokenType::Colon)?;

        let (ty, span_end) = self.parse_type()?;

        Ok(FuncParam {
            name,
            ty,
            info: LocInfo {
                span: Span::new(span_start, span_end),
                file: self.current_file,
            },
        })
    }

    #[inline]
    fn parse_type(&mut self) -> Result<(Type<'a>, u32)> {
        let ident = self.eat(TokenType::Ident)?;
        let (range_start, range_end) = ident.range;

        Ok((
            match ident.source {
                Cow::Borrowed("void") => Type::Void,
                Cow::Borrowed("str") => Type::String,
                Cow::Borrowed("int") => Type::Int,
                Cow::Borrowed("bool") => Type::Bool,
                custom => Type::Custom(Ident {
                    name: custom,
                    info: LocInfo {
                        span: Span::new(range_start, range_end),
                        file: self.current_file,
                    },
                }),
            },
            range_start,
        ))
    }

    #[inline]
    fn next(&mut self, line: u32) -> Result<Token<'a>> {
        let mut next = self.token_stream.next();
        std::mem::swap(&mut next, &mut self.peek);
        self.next = next.clone();

        if let Some(next) = next {
            trace!("Got token: {:?}", next);
            Ok(next)
        } else {
            trace!("Failed to get next token on line {}", line);

            Err(Diagnostic::new(
                Severity::Error,
                "Unexpected End Of File",
                Label::new(
                    self.files[0],
                    self.codespan.source_span(self.files[0]),
                    "Unexpected End Of File".to_string(),
                ),
            ))
        }
    }

    #[inline]
    fn peek(&mut self) -> Result<Token<'a>> {
        if let Some(next) = self.peek.clone() {
            Ok(next)
        } else {
            trace!("Failed to peek next token");

            Err(Diagnostic::new(
                Severity::Error,
                "Unexpected End Of File",
                Label::new(
                    self.files[0],
                    self.codespan.source_span(self.files[0]),
                    format!("Unexpected End Of File"),
                ),
            ))
        }
    }

    #[inline]
    fn eat_w(&mut self) -> Result<()> {
        while let Some(token) = &self.peek {
            if token.ty != TokenType::Space {
                break;
            } else {
                trace!("Ate {:?} as whitespace", token);
                self.next(line!())?;
            }
        }

        Ok(())
    }

    #[inline]
    fn eat_all_of(&mut self, set: &[TokenType]) -> Result<()> {
        while let Some(token) = &self.peek {
            println!("{:?}, {}", token, set.contains(&token.ty));
            if !set.contains(&token.ty) {
                break;
            } else {
                trace!("Ate {:?} as part of set", token);
                self.next(line!())?;
            }
        }

        Ok(())
    }

    #[inline]
    fn eat(&mut self, expected: TokenType) -> Result<Token<'a>> {
        self.eat_w()?;

        let token = self.next(line!())?;

        if token.ty == expected {
            trace!("Ate {:?}", token);
            Ok(token)
        } else {
            trace!(
                "Failed to eat token, expected {:?} got {:?}. Token: {:?}",
                expected,
                token.ty,
                token
            );

            self.error = true;
            Err(Diagnostic::new(
                Severity::Error,
                format!(
                    "Unexpected Token: Expected {:?}, got {:?}",
                    expected, token.ty
                ),
                Label::new(
                    self.files[0],
                    token.range.0 as u32..token.range.1 as u32,
                    format!("Expected {:?}", expected),
                ),
            ))
        }
    }

    #[inline]
    fn eat_of(&mut self, expected: &[TokenType]) -> Result<Token<'a>> {
        self.eat_w()?;

        let token = self.next(line!())?;

        if expected.contains(&token.ty) {
            trace!("Ate {:?}", token);
            Ok(token)
        } else {
            trace!(
                "Failed to eat token, expected {:?}, got {:?}. Token: {:?}",
                expected,
                token.ty,
                token,
            );

            self.error = true;
            Err(Diagnostic::new(
                Severity::Error,
                format!(
                    "Unexpected Token: Expected one of [{}], got {:?}",
                    expected
                        .iter()
                        .map(|t| format!("{:?}", t))
                        .collect::<Vec<String>>()
                        .join(", "),
                    token.ty
                ),
                Label::new(
                    self.files[0],
                    self.codespan.source_span(self.files[0]),
                    format!("Unexpected Token: {:?}", token.ty),
                ),
            ))
        }
    }
}

fn unescape_unicode(queue: &mut VecDeque<char>) -> Option<char> {
    let mut s = String::with_capacity(4);

    if queue.pop_front() != Some('{') {
        return None;
    }

    for _ in 0..4 {
        if let Some(c) = queue.pop_front() {
            s.push(c);
        } else {
            return None;
        }
    }

    if queue.pop_front() != Some('}') {
        return None;
    }

    let u = match u32::from_str_radix(&s, 16) {
        Ok(u) => u,
        Err(_) => return None,
    };
    char::from_u32(u)
}

fn unescape_byte(queue: &mut VecDeque<char>) -> Option<char> {
    let mut s = String::with_capacity(2);

    if queue.pop_front() != Some('{') {
        return None;
    }

    for _ in 0..2 {
        if let Some(c) = queue.pop_front() {
            s.push(c);
        } else {
            return None;
        }
    }

    if queue.pop_front() != Some('}') {
        return None;
    }

    match u8::from_str_radix(&s, 16) {
        Ok(c) => Some(c as char),
        Err(_) => None,
    }
}

fn unescape_bits(queue: &mut VecDeque<char>) -> Option<char> {
    let mut s = String::with_capacity(8);

    if queue.pop_front() != Some('{') {
        return None;
    }

    for _ in 0..8 {
        if let Some(c) = queue.pop_front() {
            s.push(c);
        } else {
            return None;
        }
    }

    if queue.pop_front() != Some('}') {
        return None;
    }

    match u8::from_str_radix(&s, 2) {
        Ok(c) => Some(c as char),
        Err(_) => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_test() {
        use std::{fs::File, io::Write};

        const CODE: &str = include_str!("../../examples/parse_test.crunch");
        const FILENAME: &str = "parse_test.crunch";

        color_backtrace::install();
        // simple_logger::init().unwrap();

        let mut parser = Parser::new(Some(FILENAME), CODE);

        match parser.parse() {
            Ok(ast) => {
                println!("{:#?}", &ast);

                let options = crate::OptionBuilder::new("./examples/parse_test.crunch").build();
                let bytecode = match crate::interpreter::Interpreter::new(ast.0.clone(), &options)
                    .interpret()
                {
                    Ok(interp) => interp,
                    Err(err) => {
                        err.emit();
                        panic!("Runtime error while compiling");
                    }
                };
                let encoded =
                    crate::bytecode::encode_program(bytecode.0.clone(), bytecode.1.clone());

                println!(
                "# Source Code  \n\n```\n{}\n```\n\n# AST  \n\n```\n{:#?}\n```\n\n# Bytecode IR  \n\n```\n(\n\t[\n{}\n\t],\n\t[\n{}\n\t]\n)\n```\n\n# Raw Bytecode  \n\n```\n{}\n```\n\n\n",
                CODE,
                ast,
                bytecode.0.iter().map(|b| format!("\t\t{:?}", b)).collect::<Vec<String>>().join(",\n"), bytecode.1.iter().map(|b| b.iter().map(|a| format!("\t\t{:?}", a)).collect::<Vec<String>>().join(",\n")).collect::<Vec<String>>().join("],"),
                encoded
                    .iter()
                    .map(|b| format!("{:02X}", b))
                    .collect::<Vec<String>>()
                    .chunks(16)
                    .map(|c| c.join(" "))
                    .collect::<Vec<String>>()
                    .join("\n"),
            );

                let mut file = File::create("./examples/hello_world.crunched").unwrap();
                file.write_all(&encoded).unwrap();

                crate::Crunch::run_source_file(
                    crate::OptionBuilder::new("./examples/parse_test.crunch").build(),
                );
            }

            Err(err) => {
                let writer = codespan_reporting::term::termcolor::StandardStream::stderr(
                    codespan_reporting::term::termcolor::ColorChoice::Auto,
                );

                let config = codespan_reporting::term::Config::default();

                let mut files = Files::new();
                files.add(FILENAME, CODE);

                for e in err {
                    codespan_reporting::term::emit(&mut writer.lock(), &config, &files, &e)
                        .unwrap();
                }
            }
        }
    }
}
