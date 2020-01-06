mod ast;
mod token;

pub use ast::*;

use codespan::{FileId, Files, Span};
use codespan_reporting::diagnostic::{Diagnostic, Label, Severity};
use std::{char, collections::VecDeque, convert::TryFrom, panic::Location};
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
    indent_level: usize,
}

impl<'a> Parser<'a> {
    #[must_use]
    pub fn new(filename: Option<&'a str>, input: &'a str) -> Self {
        // The trim_start is a sketchy solution to the weird problem
        //  of a `\r\n` precluding a function decl causing everything
        // to break. Smallest reproducible set:
        // "\r\nfn main()\r\n\t@print \"Welp.\\n\"\r\n"
        // TODO: Fix it, I don't like it lying around
        let mut token_stream = TokenStream::new(input.trim_start(), true);

        let next = None;
        let peek = token_stream.next_token();
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
            indent_level: 0,
        }
    }

    pub fn parse(
        &mut self,
    ) -> std::result::Result<(Vec<Node<'a>>, Vec<Diagnostic>), Vec<Diagnostic>> {
        const TOP_LEVEL_TOKENS: [TokenType; 2] = [TokenType::Import, TokenType::Function];

        let mut ast = Vec::new();

        while let Ok(token) = self.peek() {
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

                TokenType::Newline => {
                    trace!("Eating the token {:?} at the top level", token);
                    if self.next().is_err() {
                        break;
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
                    if self.next().is_err() {
                        break;
                    }
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
                    if self.next().is_err() {
                        break;
                    }
                }
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

        self.eat(TokenType::Import)?;

        let ty = self.parse_import_type()?;

        let file: PathBuf = {
            let source = self.eat(TokenType::String)?.source;
            source[1..source.len() - 1].split('.').collect()
        };

        let (exposes, alias) = match self.peek()?.ty {
            TokenType::Exposing => {
                self.eat(TokenType::Exposing)
                    .unwrap_or_else(|_| unreachable!("Peek was Exposing"));

                let peek = self.peek()?;
                if peek.ty == TokenType::Star {
                    self.eat(TokenType::Star)
                        .unwrap_or_else(|_| unreachable!("Peek was Star"));

                    self.eat(TokenType::Newline)?;

                    (Exposes::All, None)
                } else if peek.ty == TokenType::Ident {
                    let (mut imports, mut peek) = (Vec::new(), self.peek()?);

                    while peek.ty != TokenType::Newline {
                        let ident =
                            Ident::from_token(self.eat(TokenType::Ident)?, self.current_file);

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
                            "You must expose something when using the `exposing` keyword"
                                .to_string(),
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
                    self.eat(TokenType::Newline)?;
                    (Exposes::All, Some(alias))
                }
            }

            _ => {
                self.eat(TokenType::Newline)?;
                (Exposes::File, None)
            }
        };

        info!("Finished parsing Import");

        let import = Import {
            file,
            alias,
            exposes,
            ty,
        };

        info!("Produced Import: {:#?}", &import);

        Ok(import)
    }

    fn parse_import_type(&mut self) -> Result<ImportType> {
        match self.peek()?.ty {
            TokenType::Library => {
                self.eat(TokenType::Library)?;
                Ok(ImportType::Library)
            }
            TokenType::Package => {
                self.eat(TokenType::Package)?;
                Ok(ImportType::Package)
            }
            _ => Ok(ImportType::File),
        }
    }

    fn parse_function_parameters(&mut self) -> Result<Vec<FuncParam<'a>>> {
        self.eat(TokenType::LeftParen)?;

        let mut params = Vec::new();
        while self.peek()?.ty != TokenType::RightParen {
            params.push(self.parse_named_parameter()?);

            if self.peek()?.ty == TokenType::Comma {
                self.eat(TokenType::Comma)
                    .unwrap_or_else(|_| unreachable!("Peek was Comma"));
            }
        }

        self.eat(TokenType::RightParen)?;

        Ok(params)
    }

    fn parse_function_return_type(&mut self) -> Result<Type<'a>> {
        if self.peek()?.ty == TokenType::RightArrow {
            self.eat(TokenType::RightArrow)
                .unwrap_or_else(|_| unreachable!("Peek was RightArrow"));

            Ok(self.parse_type()?.0)
        } else {
            Ok(Type::Void)
        }
    }

    fn parse_function_body(&mut self) -> Result<Vec<FuncExpr<'a>>> {
        let (mut body, mut token) = (Vec::new(), self.peek()?);
        while token.ty != TokenType::EndBlock {
            if let Some(expr) = self.parse_expr()? {
                body.push(expr);
            } else {
                trace!("`parse_expr` hit a EndBlock, finishing function body parsing");
                break;
            }

            token = self.peek()?;
        }

        Ok(body)
    }

    fn parse_function(&mut self, span_start: u32) -> Result<Func<'a>> {
        info!("Parsing Function");

        self.eat(TokenType::Function)?;

        let name = Ident::from_token(self.eat(TokenType::Ident)?, self.current_file);
        let parameters = self.parse_function_parameters()?;
        let returns = self.parse_function_return_type()?;

        self.eat(TokenType::Newline)?;

        let body = self.parse_function_body()?;

        self.eat(TokenType::EndBlock)?;

        info!("Finished parsing Function");

        Ok(Func {
            name,
            params: parameters,
            returns,
            body,
            info: LocInfo {
                span: Span::new(span_start, span_start),
                file: self.current_file,
            },
        })
    }

    fn parse_inline_parameters_parenless(&mut self) -> Result<Vec<IdentLiteral<'a>>> {
        let (mut params, mut peek) = (Vec::new(), self.peek()?);

        while peek.ty != TokenType::Newline && peek.ty != TokenType::Comma {
            params.push(self.ident_literal()?);

            if self.peek()?.ty == TokenType::Comma {
                self.eat(TokenType::Comma)
                    .unwrap_or_else(|_| unreachable!("Peek was Comma"));
            }

            peek = self.peek()?;
        }

        Ok(params)
    }

    fn parse_inline_parameters(&mut self) -> Result<Vec<IdentLiteral<'a>>> {
        self.eat(TokenType::LeftParen)?;

        let (mut params, mut next) = (Vec::new(), self.peek()?);

        while next.ty != TokenType::RightParen {
            match next.ty {
                TokenType::String | TokenType::Bool | TokenType::Int | TokenType::Ident => {
                    params.push(self.ident_literal()?);
                }

                TokenType::Comma => {
                    self.eat(TokenType::Comma)
                        .unwrap_or_else(|_| unreachable!("Peek was Comma"));
                }

                t => {
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
                                [
                                    TokenType::String,
                                    TokenType::Bool,
                                    TokenType::Int,
                                    TokenType::Ident,
                                    TokenType::Comma,
                                ]
                                .iter()
                                .map(|t| format!("'{}'", t))
                                .collect::<Vec<String>>()
                                .join(", "),
                            ),
                        ),
                    ));
                }
            }

            next = self.peek()?;
        }
        self.eat(TokenType::RightParen)?;

        Ok(params)
    }

    fn parse_ident_exprs(&mut self) -> Result<FuncExpr<'a>> {
        let ident = Ident::from_token(self.eat(TokenType::Ident)?, self.current_file);

        let ident_expr = match self.peek()?.ty {
            TokenType::LeftParen => {
                let params = self.parse_inline_parameters()?;
                self.eat(TokenType::Newline)?;

                FuncExpr::FuncCall(FuncCall {
                    func_name: ident,
                    params,
                    info: LocInfo {
                        span: Span::new(0, 0),
                        file: self.current_file,
                    },
                })
            }

            TokenType::Equal => {
                self.eat(TokenType::Equal)?;

                let assignment = self.ident_literal()?;
                self.eat(TokenType::Newline)?;

                FuncExpr::Assign(Assign {
                    name: ident,
                    val: assignment,
                    info: LocInfo {
                        span: Span::new(0, 0),
                        file: self.current_file,
                    },
                })
            }

            t => unimplemented!("Token: {:?}", t),
        };

        Ok(ident_expr)
    }

    fn parse_binding_expr(&mut self) -> Result<Binding<'a>> {
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
        self.eat(TokenType::Newline)?;

        Ok(Binding {
            name,
            val,
            ty,
            info: LocInfo {
                span: Span::new(0, 0),
                file: self.current_file,
            },
        })
    }

    fn parse_expr(&mut self) -> Result<Option<FuncExpr<'a>>> {
        let token = self.peek()?;

        let expr = match token.ty {
            TokenType::Ident => self.parse_ident_exprs()?,

            TokenType::Let => FuncExpr::Binding(Box::new(self.parse_binding_expr()?)),

            TokenType::Print => {
                self.eat(TokenType::Print)?;

                let params = self.parse_inline_parameters_parenless()?;
                self.eat(TokenType::Newline)?;

                FuncExpr::Builtin(Builtin::Print(params))
            }

            TokenType::Collect => {
                self.eat(TokenType::Collect)?;
                self.eat(TokenType::Newline)?;

                FuncExpr::Builtin(Builtin::Collect)
            }

            TokenType::Halt => {
                self.eat(TokenType::Halt)?;
                self.eat(TokenType::Newline)?;

                FuncExpr::Builtin(Builtin::Halt)
            }

            TokenType::If => {
                let (mut peek, mut if_clauses, mut else_body) = (token.ty, Vec::new(), None);
                while peek == TokenType::If || peek == TokenType::Else {
                    match self.conditional()? {
                        Either::Left(_if) => if_clauses.push(_if),
                        Either::Right(_else) => else_body = Some(_else),
                    }

                    peek = self.peek()?.ty;
                }
                self.eat(TokenType::EndBlock)?;

                FuncExpr::Conditional(Conditional {
                    if_clauses,
                    else_body,
                })
            }

            TokenType::Newline => {
                self.eat(TokenType::Newline)?;
                return self.parse_expr();
            }

            TokenType::EndBlock => return Ok(None),

            // TokenType::EndBlock => {
            //     return Err(Diagnostic::new(
            //         Severity::Error,
            //         "Expression terminated with an endblock, should be caught by compiler",
            //         Label::new(
            //             self.files[0],
            //             self.codespan.source_span(self.files[0]),
            //             "Expression terminated with an endblock, should be caught by compiler"
            //                 .to_string(),
            //         ),
            //     ));
            // }
            token => unimplemented!("Haven't implemented all possible expressions: {:?}", token),
        };

        Ok(Some(expr))
    }

    fn conditional(&mut self) -> Result<Either<If<'a>, Vec<FuncExpr<'a>>>> {
        #[derive(Debug, Copy, Clone, PartialEq, Eq)]
        enum CondType {
            If,
            ElseIf,
            Else,
        }

        trace!("Started parsing conditional");

        let cond = self.eat_of(&[TokenType::If, TokenType::Else])?;
        let cond = if cond.ty == TokenType::If {
            CondType::If
        } else if cond.ty == TokenType::Else && self.peek()?.ty == TokenType::If {
            self.eat(TokenType::If)?;
            CondType::ElseIf
        } else if cond.ty == TokenType::Else {
            CondType::Else
        } else {
            unreachable!("The only valid conditionals should be `if`, `else if` and `else`")
        };

        let condition = if cond != CondType::Else {
            Some(self.parse_binding_val()?)
        } else {
            None
        };
        self.eat(TokenType::Newline)?;

        let (mut token, mut body) = (self.peek()?, Vec::new());
        while token.ty != TokenType::EndBlock && token.ty != TokenType::Else {
            if let Some(expr) = self.parse_expr()? {
                body.push(expr);
            } else {
                trace!("`parse_expr` hit a EndBlock, finishing conditional parsing");
                break;
            }

            token = self.peek()?;
        }

        trace!("Finished parsing conditional");

        Ok(match cond {
            CondType::If | CondType::ElseIf => Either::Left(If {
                condition: condition.expect("`if` and `else if` clauses have conditions"),
                body,
            }),
            CondType::Else => Either::Right(body),
        })
    }

    #[track_caller]
    fn ident_literal(&mut self) -> Result<IdentLiteral<'a>> {
        info!("Parsing Ident or Literal");

        match self.peek()?.ty {
            TokenType::String | TokenType::Bool | TokenType::Int => {
                Ok(IdentLiteral::Literal(self.parse_variable()?))
            }

            TokenType::Ident => Ok(IdentLiteral::Variable(Ident::from_token(
                self.eat(TokenType::Ident)?,
                self.current_file,
            ))),

            t => {
                let loc = Location::caller();
                self.error = true;
                error!(
                    "[Parsing error in {} {}:{}]: Invalid function parameter: {:?}",
                    loc.file(),
                    loc.line(),
                    loc.column(),
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

        let val = match self.peek()?.ty {
            TokenType::String | TokenType::Bool | TokenType::Int | TokenType::Ident => {
                const TOKENS: [TokenType; 5] = [
                    TokenType::Plus,
                    TokenType::Minus,
                    TokenType::Divide,
                    TokenType::Star,
                    TokenType::IsEqual,
                ];

                let left = self.parse_bin_op_side()?;

                let peek = self.peek()?;
                if TOKENS.contains(&peek.ty) {
                    self.eat_of(&TOKENS)
                        .unwrap_or_else(|_| unreachable!("Peek was {:?}", TOKENS));

                    let op = match Op::try_from(peek.ty) {
                        Ok(op) => op,
                        Err(ty) => {
                            return Err(Diagnostic::new(
                                Severity::Error,
                                "Incorrect operator token",
                                Label::new(
                                    self.files[0],
                                    self.codespan.source_span(self.files[0]),
                                    format!(
                                        "Expected one of {}, got {}",
                                        TOKENS
                                            .iter()
                                            .map(|t| format!("'{}'", t))
                                            .collect::<Vec<String>>()
                                            .join(", "),
                                        ty
                                    ),
                                ),
                            ));
                        }
                    };

                    BindingVal::BinOp(BinOp {
                        op,
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
            token => unreachable!("Impossible Token: {:?}", token),
        };

        info!("Finished parsing Binding Value");

        Ok(val)
    }

    fn parse_bin_op_side(&mut self) -> Result<BinOpSide<'a>> {
        info!("Parsing BinOp Side");

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
    fn parse_variable(&mut self) -> Result<Literal> {
        info!("Parsing Variable");

        let token = self.next()?;

        let (span_start, span_end) = token.range;
        let val = match token.ty {
            TokenType::Int => LiteralInner::Int(token.source.parse().unwrap()),
            TokenType::String => LiteralInner::String(
                self.escape_string(&(&*token.source)[1..token.source.len() - 1])?,
            ),
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
                "void" => Type::Void,
                "str" => Type::String,
                "int" => Type::Int,
                "bool" => Type::Bool,
                "any" => Type::Any,
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

/// Parsing utilities
impl<'a> Parser<'a> {
    #[inline]
    #[track_caller]
    fn next(&mut self) -> Result<Token<'a>> {
        let loc = Location::caller();

        let mut next = self.token_stream.next_token();
        std::mem::swap(&mut next, &mut self.peek);
        self.next = next;

        if let Some(next) = next {
            trace!(
                "[{} {}:{}] Got token: {:?}",
                loc.file(),
                loc.line(),
                loc.column(),
                next
            );
            Ok(next)
        } else {
            error!(
                "[Parsing error in {} {}:{}]: Unexpected EOF",
                loc.file(),
                loc.line(),
                loc.column(),
            );
            Err(Diagnostic::new(
                Severity::Error,
                "Unexpected End Of File",
                Label::new(
                    self.files[0],
                    {
                        let end = self.codespan.source_span(self.files[0]).end();
                        end..end
                    },
                    "Unexpected End Of File".to_string(),
                ),
            ))
        }
    }

    #[inline]
    #[track_caller]
    fn peek(&mut self) -> Result<Token<'a>> {
        let loc = Location::caller();

        if let Some(next) = self.peek {
            trace!(
                "[{} {}:{}] Peeked token: {:?}",
                loc.file(),
                loc.line(),
                loc.column(),
                next
            );
            Ok(next)
        } else {
            error!(
                "[Parsing error in {} {}:{}]: Unexpected EOF",
                loc.file(),
                loc.line(),
                loc.column(),
            );
            Err(Diagnostic::new(
                Severity::Error,
                "Unexpected End Of File",
                Label::new(
                    self.files[0],
                    {
                        let end = self.codespan.source_span(self.files[0]).end();
                        end..end
                    },
                    "Unexpected End Of File".to_string(),
                ),
            ))
        }
    }

    #[inline]
    #[track_caller]
    fn eat(&mut self, expected: TokenType) -> Result<Token<'a>> {
        let loc = Location::caller();
        let token = self.next()?;

        if token.ty == expected {
            trace!(
                "[{} {}:{}] Ate {:?}",
                loc.file(),
                loc.line(),
                loc.column(),
                expected
            );
            Ok(token)
        } else {
            self.error = true;
            error!(
                "[Parsing error in {} {}:{}]: Failed to eat token, expected {:?} got {:?}. Token: {:?}",
                loc.file(),
                loc.line(),
                loc.column(),
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
        let token = self.next()?;

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
                Some('u') => s.push(if let Some(c) = unescape_unicode(&mut queue) {
                    c
                } else {
                    self.error = true;
                    return Err(error);
                }),
                Some('x') => s.push(if let Some(c) = unescape_byte(&mut queue) {
                    c
                } else {
                    self.error = true;
                    return Err(error);
                }),
                Some('b') => s.push(if let Some(c) = unescape_bits(&mut queue) {
                    c
                } else {
                    self.error = true;
                    return Err(error);
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
    trace!("Parsing Unicode Escape");
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

    trace!("Valid Unicode Escape");
    char::from_u32(u)
}

fn unescape_byte(queue: &mut VecDeque<char>) -> Option<char> {
    trace!("Parsing Hex Escape");
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
        Ok(c) => {
            trace!("Valid Hex Escape");
            Some(c as char)
        }
        Err(_) => None,
    }
}

fn unescape_bits(queue: &mut VecDeque<char>) -> Option<char> {
    trace!("Parsing Bit Escape");
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
        Ok(c) => {
            trace!("Valid Bit Escape");
            Some(c as char)
        }
        Err(_) => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_test() {
        const CODE: &str = include_str!("../../examples/parse_test.crunch");
        const FILENAME: &str = "parse_test.crunch";

        /*
        use token::TokenType::*;
        let _ = [
            [
                Load(I32(10), 31),
                Load(Bool(true), 30),
                Load(I32(1), 29),
                Load(I32(10), 28),
                Eq(29, 28),
                OpToReg(29),
                Drop(28),
                Eq(29, 30),
                Drop(29),
                JumpComp(2),
                Jump(5),
                JumpPoint(2),
                Load(Str("1 == 10"), 0),
                Func(1),
                Jump(5),
                JumpPoint(3),
                Load(Str("1 != 10"), 1),
                Func(2),
                Jump(1),
                JumpPoint(1),
                Drop(30),
                Return,
            ],
            [Print(0), Return],
            [Print(0), Load(Str("\n"), 31), Print(31), Drop(31), Return],
        ];
        */

        color_backtrace::install();
        // simple_logger::init().unwrap();

        println!(
            "{:#?}",
            TokenStream::new(CODE, true).into_iter().collect::<Vec<_>>()
        );

        let mut parser = Parser::new(Some(FILENAME), CODE);

        match parser.parse() {
            Ok(ast) => {
                println!("Parsing Successful\n{:#?}", &ast);

                let options = crate::OptionBuilder::new("./examples/parse_test.crunch").build();
                let bytecode =
                    match crate::interpreter::Interpreter::new(&options).interpret(ast.0.clone()) {
                        Ok(interp) => interp,
                        Err(err) => {
                            err.emit();
                            panic!("Runtime error while compiling");
                        }
                    };
                println!("Bytecode: {:?}", bytecode);

                crate::Crunch::run_source_file(
                    crate::OptionBuilder::new("./examples/parse_test.crunch").build(),
                );
            }

            Err(err) => {
                println!("Parsing not successful");
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
