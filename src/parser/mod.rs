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
        // The trim_start is a sketchy solution to the weird problem
        //  of a `\r\n` precluding a function decl causing everything
        // to break. Smallest reproducible set:
        // "\r\nfn main()\r\n\t@print \"Welp.\\n\"\r\n"
        // TODO: Fix it, I don't like it lying around
        let mut token_stream = TokenStream::new(input.trim_start());

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

        Self {
            token_stream,
            next,
            peek,
            codespan,
            files,
            current_file,
            diagnostics: Vec::new(),
            error: false,
        }
    }

    pub fn parse(
        &mut self,
    ) -> std::result::Result<(Vec<Node<'a>>, Vec<Diagnostic>), Vec<Diagnostic>> {
        let mut ast = Vec::new();

        // TODO: Make this loop operate off of peeks, and change all the top-level parsers
        // to consume their first token

        const TOP_LEVEL_TOKENS: [TokenType; 2] = [TokenType::Import, TokenType::Function];

        loop {
            if let Some(token) = self.next.clone() {
                match token.ty {
                    TokenType::Function => {
                        info!("Top Level Loop: Function");

                        ast.push(Node::Func(match self.parse_function(token.range.0) {
                            Ok(node) => node,
                            Err(err) => {
                                self.error = true;
                                self.diagnostics.push(err);
                                continue;
                            }
                        }));
                    }

                    TokenType::Import => {
                        info!("Top Level Loop: Import");
                        ast.push(Node::Import(match self.parse_import() {
                            Ok(node) => node,
                            Err(err) => {
                                self.error = true;
                                self.diagnostics.push(err);
                                continue;
                            }
                        }))
                    }

                    TokenType::End => break,

                    // This is apparently kinda flawed
                    TokenType::Space | TokenType::Comment | TokenType::Newline => {
                        if self.next(line!()).is_err() {
                            break;
                        } else {
                            continue;
                        }
                    }

                    TokenType::Error => {
                        self.error = true;
                        error!(
                            "[Parsing error on {}:{}]: Invalid token",
                            line!(),
                            column!(),
                        );
                        self.diagnostics.push(Diagnostic::new(
                            Severity::Error,
                            "Invalid token",
                            Label::new(
                                self.files[0],
                                token.range.0 as u32..token.range.1 as u32,
                                format!("{:?} is not a valid token", token.source),
                            ),
                        ));
                    }

                    t => {
                        self.error = true;
                        error!(
                            "[Parsing error on {}:{}]: Invalid top-level token: {:?}",
                            line!(),
                            column!(),
                            &t
                        );
                        self.diagnostics.push(Diagnostic::new(
                            Severity::Error,
                            "Invalid top-level token",
                            Label::new(
                                self.files[0],
                                token.range.0 as u32..token.range.1 as u32,
                                format!(
                                    "Found {:?}, expected one of {}",
                                    token.ty,
                                    TOP_LEVEL_TOKENS
                                        .iter()
                                        .map(|t| format!("'{}'", t))
                                        .collect::<Vec<String>>()
                                        .join(", "),
                                ),
                            ),
                        ));
                    }
                }
            } else {
                break;
            }

            if self.next(line!()).is_err() {
                let mut diagnostics = Vec::new();
                std::mem::swap(&mut diagnostics, &mut self.diagnostics);

                return if self.error {
                    Err(diagnostics)
                } else {
                    Ok((ast, diagnostics))
                };
            }
        }

        let mut diagnostics = Vec::new();
        std::mem::swap(&mut diagnostics, &mut self.diagnostics);

        if self.error {
            Err(diagnostics)
        } else {
            Ok((ast, diagnostics))
        }
    }

    // TODO: Clean this up
    fn parse_import(&mut self) -> Result<Import<'a>> {
        use std::path::PathBuf;

        info!("Parsing Import");

        let ty = self.parse_import_type()?;

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

                    self.eat_line_end()?;

                    (Exposes::All, None)
                } else if peek.ty == TokenType::Ident {
                    let (mut imports, mut peek) = (Vec::new(), self.peek()?);

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

                    self.eat_line_end()?;

                    (Exposes::Some(imports), None)
                } else {
                    self.error = true;
                    error!(
                        "[Parsing error on {}:{}]: Expected exposed members",
                        line!(),
                        column!(),
                    );
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
                        let (mut imports, mut peek) = (Vec::new(), self.peek()?);

                        while peek.ty != TokenType::Newline {
                            let ident =
                                Ident::from_token(self.eat(TokenType::Ident)?, self.current_file);

                            self.eat_w()?;
                            let token = self.peek()?;

                            let alias = if token.ty == TokenType::As {
                                self.eat(TokenType::As)
                                    .unwrap_or_else(|_| unreachable!("Peek was As"));

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

                        self.eat_line_end()?;

                        (Exposes::Some(imports), Some(alias))
                    } else {
                        self.error = true;
                        error!(
                            "[Parsing error on {}:{}]: Expected exposed members",
                            line!(),
                            column!(),
                        );
                        return Err(Diagnostic::new(
                            Severity::Error,
                            "Expected exposed members",
                            Label::new(
                                self.files[0],
                                self.codespan.source_span(self.files[0]),
                                "You must expose something when using the `exposing` keyword",
                            ),
                        ));
                    }
                } else {
                    self.eat_line_end()?;
                    (Exposes::All, Some(alias))
                }
            }

            _ => {
                self.eat_line_end()?;
                (Exposes::File, None)
            }
        };

        info!("Finished parsing Import");

        Ok(Import {
            file,
            alias,
            exposes,
            ty,
        })
    }

    fn parse_import_type(&mut self) -> Result<ImportType> {
        match self.peek()?.ty {
            TokenType::Library => Ok(ImportType::Library),
            TokenType::Package => Ok(ImportType::Package),
            _ => Ok(ImportType::File),
        }
    }

    fn parse_function(&mut self, span_start: u32) -> Result<Func<'a>> {
        info!("Parsing Function");

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

        self.eat_line_end()?;

        let (mut body, mut span_end) = (Vec::new(), span_start);

        self.eat_w()?;
        let mut token = self.peek()?;
        while token.ty == TokenType::Indent {
            self.eat(TokenType::Indent)?;

            self.eat_w()?;
            if self.peek()?.ty == TokenType::Comment {
                self.eat_line_end()?;
                token = self.peek()?;
                continue;
            }

            body.push(self.parse_func_body(token.range.0)?);
            span_end = token.range.1;

            self.eat_until(TokenType::Newline, true)?;
            if let Ok(peek) = self.peek() {
                token = peek;
            } else {
                break;
            }
        }

        info!("Finished parsing Function");

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
        info!("Parsing function body");

        self.eat_all_of(&[TokenType::Space, TokenType::Comment, TokenType::Newline])?;

        let token = self.peek()?;
        let (expr, span_end): (FuncExpr<'a>, u32) = match token.ty {
            TokenType::Ident => {
                let ident = self.eat(TokenType::Ident)?;
                let span_start = ident.range.0 as u32;
                let ident = Ident::from_token(ident, self.current_file);

                self.eat_w()?;

                match self.peek()?.ty {
                    TokenType::LeftParen => {
                        self.eat(TokenType::LeftParen)?;
                        self.eat_w()?;

                        let (mut params, mut next) = (Vec::new(), self.peek()?);

                        while next.ty != TokenType::RightParen {
                            match next.ty {
                                TokenType::String
                                | TokenType::Bool
                                | TokenType::Int
                                | TokenType::Ident => {
                                    params.push(self.ident_literal()?);
                                }

                                TokenType::Comma => {
                                    self.eat(TokenType::Comma)
                                        .unwrap_or_else(|_| unreachable!("Peek was Comma"));
                                }

                                t => {
                                    const EXPECTED_TOKENS: [TokenType; 5] = [
                                        TokenType::String,
                                        TokenType::Bool,
                                        TokenType::Int,
                                        TokenType::Ident,
                                        TokenType::Comma,
                                    ];

                                    self.error = true;
                                    error!(
                                        "[Parsing error on {}:{}]: Unexpected token: {:?}",
                                        line!(),
                                        column!(),
                                        &t
                                    );
                                    return Err(Diagnostic::new(
                                        Severity::Error,
                                        "Unexpected token",
                                        Label::new(
                                            self.files[0],
                                            self.codespan.source_span(self.files[0]),
                                            format!(
                                                "Found {}, expected one of {}",
                                                t,
                                                EXPECTED_TOKENS
                                                    .iter()
                                                    .map(|t| format!("'{}'", t))
                                                    .collect::<Vec<String>>()
                                                    .join(", "),
                                            ),
                                        ),
                                    ));
                                }
                            }

                            self.eat_w()?;
                            next = self.peek()?;
                        }

                        let span_end = self.eat(TokenType::RightParen)?.range.1;

                        (
                            FuncExpr::FuncCall(FuncCall {
                                func_name: ident,
                                params,
                                info: LocInfo {
                                    span: Span::new(span_start, span_end),
                                    file: self.current_file,
                                },
                            }),
                            span_end,
                        )
                    }

                    TokenType::Equal => {
                        self.eat(TokenType::Equal)?;

                        self.eat_w()?;
                        let span_end = self.peek()?.range.1;

                        let assignment = self.ident_literal()?;

                        self.eat_line_end()?;

                        (
                            FuncExpr::Assign(Assign {
                                name: ident,
                                val: assignment,
                                info: LocInfo {
                                    span: Span::new(span_start, span_end),
                                    file: self.current_file,
                                },
                            }),
                            span_end,
                        )
                    }

                    t => unimplemented!("Token: {:?}", t),
                }
            }

            TokenType::Let => {
                self.eat(TokenType::Let)?;

                let name = Ident::from_token(self.eat(TokenType::Ident)?, self.current_file);

                let ty = if self.peek().unwrap().ty == TokenType::Colon {
                    self.eat(TokenType::Colon)
                        .unwrap_or_else(|_| unreachable!("Peek was Colon"));

                    self.parse_type()?.0
                } else {
                    Type::Infer
                };

                self.eat(TokenType::Equal)?;

                let val = self.parse_binding_val()?;
                let span_end = self.eat_line_end_returning()?.range.1;

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
                self.eat(TokenType::Print)?;

                self.eat_w()?;
                let (mut params, mut peek) = (Vec::new(), self.peek()?);

                while peek.ty != TokenType::Newline && peek.ty != TokenType::Comma {
                    params.push(self.ident_literal()?);

                    self.eat_w()?;
                    if self.peek()?.ty == TokenType::Comma {
                        self.eat(TokenType::Comma)
                            .unwrap_or_else(|_| unreachable!("Peek was Comma"));
                    }

                    self.eat_w()?;
                    peek = self.peek()?;
                }

                let span_end = self.eat_line_end_returning()?.range.1;

                (FuncExpr::Builtin(Builtin::Print(params)), span_end)
            }

            TokenType::Collect => {
                self.eat(TokenType::Collect)?;
                (FuncExpr::Builtin(Builtin::Collect), token.range.1)
            }

            TokenType::Halt => {
                self.eat(TokenType::Halt)?;
                (FuncExpr::Builtin(Builtin::Halt), token.range.1)
            }

            TokenType::SyscallExit => {
                self.eat(TokenType::SyscallExit)?;

                self.eat_w()?;
                let peek = self.peek()?;

                let exit_code = if peek.ty != TokenType::Newline && peek.ty != TokenType::Comma {
                    self.ident_literal()?
                } else {
                    self.error = true;
                    error!(
                        "[Parsing error on {}:{}]: Invalid function parameter",
                        line!(),
                        column!(),
                    );
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

                let span_end = self.eat_line_end_returning()?.range.1;

                (FuncExpr::Builtin(Builtin::SyscallExit(exit_code)), span_end)
            }

            _ => unimplemented!("Haven't implemented all possible function body expressions"),
        };

        info!("Finished parsing Func Body");

        Ok(FuncBody {
            expr,
            info: LocInfo {
                span: Span::new(span_start, span_end),
                file: self.current_file,
            },
        })
    }

    fn ident_literal(&mut self) -> Result<IdentLiteral<'a>> {
        info!("Parsing Ident or Literal");

        self.eat_w()?;
        match self.peek()?.ty {
            TokenType::String | TokenType::Bool | TokenType::Int => {
                Ok(IdentLiteral::Literal(self.parse_variable()?))
            }

            TokenType::Ident => Ok(IdentLiteral::Variable(Ident::from_token(
                self.eat(TokenType::Ident)?,
                self.current_file,
            ))),

            t => {
                self.error = true;
                error!(
                    "[Parsing error on {}:{}]: Invalid function parameter: {:?}",
                    line!(),
                    column!(),
                    &t
                );
                Err(Diagnostic::new(
                    Severity::Error,
                    "Invalid Function Parameter",
                    Label::new(
                        self.files[0],
                        self.codespan.source_span(self.files[0]),
                        format!("Expected Ident or Literal, got '{}'", t),
                    ),
                ))
            }
        }
    }

    fn parse_binding_val(&mut self) -> Result<BindingVal<'a>> {
        info!("Parsing Binding Value");

        self.eat_w()?;
        let val = match self.peek()?.ty {
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
                    ])
                    .unwrap_or_else(|_| {
                        unreachable!(
                            "Peek was {:?}",
                            [
                                TokenType::Plus,
                                TokenType::Minus,
                                TokenType::Divide,
                                TokenType::Star,
                            ]
                        )
                    });

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
        };

        info!("Finished parsing Binding Value");

        Ok(val)
    }

    fn parse_bin_op_side(&mut self) -> Result<BinOpSide<'a>> {
        info!("Parsing BinOp Side");

        self.eat_w()?;
        let token = self.peek()?;

        let side = match token.ty {
            TokenType::Ident => {
                let token = self.eat(TokenType::Ident)?;
                BinOpSide::Variable(Ident::from_token(token, self.current_file))
            }

            TokenType::String | TokenType::Bool | TokenType::Int => {
                BinOpSide::Literal(self.parse_variable()?)
            }

            _ => unimplemented!(),
        };

        info!("Finished parsing Bin Op Side");

        Ok(side)
    }

    #[inline]
    fn parse_variable(&mut self) -> Result<Literal<'a>> {
        info!("Parsing Variable");

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

        info!("Finished parsing Variable");

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
        info!("Parsing Named Parameter");

        let name = Ident::from_token(self.eat(TokenType::Ident)?, self.current_file);
        let span_start = name.info.span.start();

        self.eat(TokenType::Colon)?;

        let (ty, span_end) = self.parse_type()?;

        info!("Finished parsing Named Param");

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
        info!("Parsing Type");

        let ident = self.eat(TokenType::Ident)?;
        let (range_start, range_end) = ident.range;

        info!("Finished parsing Type");

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
}

/// This block is parsing utilities
impl<'a> Parser<'a> {
    #[inline]
    fn next(&mut self, line: u32) -> Result<Token<'a>> {
        let mut next = self.token_stream.next();
        std::mem::swap(&mut next, &mut self.peek);
        self.next = next.clone();

        if let Some(next) = next {
            trace!("Got token: {:?}", next);
            Ok(next)
        } else {
            error!(
                "[Parsing error on {}:{}]: Unexpected EOF while calling next at line {}",
                line!(),
                column!(),
                line,
            );
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
            error!(
                "[Parsing error on {}:{}]: Unexpected EOF while peeking",
                line!(),
                column!(),
            );
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
            if token.ty == TokenType::Space || token.ty == TokenType::Comment {
                self.next(line!())?;
                continue;
            }

            break;
        }

        Ok(())
    }

    fn eat_until(&mut self, target: TokenType, including: bool) -> Result<()> {
        while let Some(token) = &self.peek {
            if token.ty != target {
                self.next(line!())?;
                continue;
            }

            if including {
                self.eat(target)?;
            }
            break;
        }

        Ok(())
    }

    #[inline]
    fn eat_all_of(&mut self, tokens: &[TokenType]) -> Result<()> {
        while let Some(token) = &self.peek {
            if tokens.contains(&token.ty) {
                self.next(line!())?;
                continue;
            }

            break;
        }

        Ok(())
    }

    #[inline]
    fn eat(&mut self, expected: TokenType) -> Result<Token<'a>> {
        self.eat_w()?;
        let token = self.next(line!())?;

        if token.ty == expected {
            Ok(token)
        } else {
            self.error = true;
            error!(
                "[Parsing error on {}:{}]: Failed to eat token, expected {:?} got {:?}. Token: {:?}",
                line!(),
                column!(),
                expected,
                token.ty,
                token
            );
            Err(Diagnostic::new(
                Severity::Error,
                format!(
                    "Unexpected Token: Expected '{}', found '{}'",
                    expected, token.ty
                ),
                Label::new(
                    self.files[0],
                    token.range.0 as u32..token.range.1 as u32,
                    format!("Expected {}", expected),
                ),
            ))
        }
    }

    #[inline]
    fn eat_of(&mut self, expected: &[TokenType]) -> Result<Token<'a>> {
        self.eat_w()?;

        let token = self.next(line!())?;

        if expected.contains(&token.ty) {
            Ok(token)
        } else {
            self.error = true;
            error!(
                "[Parsing error on {}:{}]: Unexpected token, expected {:?}, got {:?}. Token: {:?}",
                line!(),
                column!(),
                expected,
                token.ty,
                token,
            );
            Err(Diagnostic::new(
                Severity::Error,
                format!(
                    "Unexpected Token: Expected one of {}, got '{}'",
                    expected
                        .iter()
                        .map(|t| format!("'{}'", t))
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

    #[inline]
    fn eat_line_end(&mut self) -> Result<()> {
        self.eat_until(TokenType::Newline, true)
    }

    #[inline]
    fn eat_line_end_returning(&mut self) -> Result<Token<'a>> {
        let mut returned_token = None;
        while let Some(token) = &self.peek {
            if [TokenType::Space, TokenType::Comment, TokenType::Newline].contains(&token.ty) {
                returned_token = Some(token.clone());
                self.next(line!())?;

                continue;
            }

            break;
        }

        if let Some(token) = returned_token {
            Ok(token)
        } else {
            error!(
                "[Parsing error on {}:{}]: Failed to eat line end returning one of {:?}, got {:?}",
                line!(),
                column!(),
                &[TokenType::Space, TokenType::Comment, TokenType::Newline],
                returned_token
            );

            Err(Diagnostic::new(
                Severity::Error,
                "Failed to get token",
                Label::new(
                    self.files[0],
                    self.codespan.source_span(self.files[0]),
                    "Expected a Newline, but got None",
                ),
            ))
        }
    }

    #[inline]
    fn escape_string(&mut self, string: &str) -> Result<String> {
        info!("Parsing Escape String");

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
