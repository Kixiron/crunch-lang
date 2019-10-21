mod ast;
mod token;

pub use ast::*;

use codespan::{FileId, Files, Span};
use codespan_reporting::diagnostic::{Diagnostic, Label, Severity};
use std::borrow::Cow;
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
        let mut token_stream = TokenStream::new(input);
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
                    TokenType::End => break,
                    TokenType::Space | TokenType::Comment | TokenType::Newline => {
                        if let Err(err) = self.next(line!()) {
                            self.diagnostics.push(err);
                        }
                        continue;
                    }
                    TokenType::Error => {
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

    fn parse_function(&mut self, span_start: u32) -> Result<Func<'a>> {
        let name = Ident::from_token(self.eat(TokenType::Ident)?, self.current_file);

        self.eat(TokenType::LeftParen)?;

        let mut params = Vec::new();
        while self.peek()?.ty != TokenType::RightParen {
            params.push(self.parse_named_parameter()?);

            if self.peek()?.ty == TokenType::Comma {
                self.eat(TokenType::Comma)?;
            }
        }

        self.eat(TokenType::RightParen)?;

        self.eat_w()?;
        let returns = if self.next(line!())?.ty == TokenType::RightArrow {
            self.parse_type()?.0
        } else {
            Type::Infer
        };

        self.eat_of(&[TokenType::Newline, TokenType::Comment])?;

        let mut body = Vec::new();
        let mut span_end = span_start;
        while let Ok(token) = self.next(line!()) {
            if token.ty == TokenType::Indent {
                body.push(self.parse_func_body(token.range.0)?);
                span_end = token.range.1;
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
                    self.eat(TokenType::Colon)?;
                    self.parse_type()?.0
                } else {
                    Type::Infer
                };

                self.eat(TokenType::Equal)?;

                let val = self.parse_binding_val()?;

                let span_end = self.eat(TokenType::Newline)?.range.1;

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
                let mut params = Vec::new();

                let mut peek = self.peek()?;
                while peek.ty != TokenType::RightParen {
                    params.push(match peek.ty {
                        TokenType::String | TokenType::Bool | TokenType::Int => {
                            IdentLiteral::Literal(self.parse_variable()?)
                        }
                        TokenType::Ident => IdentLiteral::Variable(Ident::from_token(
                            self.eat(TokenType::Ident)?,
                            self.current_file,
                        )),
                        _ => {
                            return Err(Diagnostic::new(
                                Severity::Error,
                                "Invalid Function Parameter",
                                Label::new(
                                    self.files[0],
                                    self.codespan.source_span(self.files[0]),
                                    "Expected Ident or Literal, got ".to_string(),
                                ),
                            ))
                        }
                    });

                    if self.peek()?.ty == TokenType::Comma {
                        self.eat(TokenType::Comma)?;
                    }

                    peek = self.peek()?;
                }

                let span_end = self.eat(TokenType::RightParen)?.range.1;

                (FuncExpr::Builtin(Builtin::Print(params)), span_end)
            }
            TokenType::Collect => (FuncExpr::Builtin(Builtin::Collect), token.range.1),
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
            TokenType::String | TokenType::Bool | TokenType::Int => {
                BindingVal::Literal(self.parse_variable()?)
            }
            TokenType::Ident => unimplemented!(),
            _ => crate::unreachable!("Impossible Token: {:?}", self.peek()),
        })
    }

    #[inline]
    fn parse_variable(&mut self) -> Result<Literal<'a>> {
        self.eat_w()?;
        let token = self.next(line!())?;

        let (span_start, span_end) = token.range;
        let val = match token.ty {
            TokenType::Int => LiteralInner::Int(token.source.parse().unwrap()),
            TokenType::String => LiteralInner::String(token.source),
            TokenType::Bool => LiteralInner::Bool(token.source.parse().unwrap()),
            _ => {
                eprintln!("{:?}", token.ty);
                unimplemented!()
            }
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
            Ok(token)
        } else {
            trace!(
                "Failed to eat token, expected {:?} got {:?}",
                expected,
                token.ty
            );

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
            Ok(token)
        } else {
            trace!(
                "Failed to eat token, expected {:?}, got {:?}",
                expected,
                token.ty
            );

            Err(Diagnostic::new(
                Severity::Error,
                format!(
                    "Unexpected Token: Expected one of {}, got {:?}",
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

#[test]
fn parse_test() {
    const CODE: &str = include_str!("../../examples/parse_test.crunch");
    const FILENAME: &str = "parse_test.crunch";

    let mut parser = Parser::new(Some(FILENAME), CODE);

    match parser.parse() {
        Ok(ast) => {
            use std::io::Write;

            let mut file = std::fs::File::create("./OUTPUT.md").unwrap();
            let options = crate::OptionBuilder::new("./parse_test").build();
            let bytecode =
                match crate::interpreter::Interpreter::new(ast.0.clone(), &options).interpret() {
                    Ok(interp) => interp,
                    Err(err) => {
                        err.emit();
                        panic!("Runtime error while compiling");
                    }
                };
            file.write_all(
                format!(
                    "# Source Code  \n\n```\n{}\n```\n\n# AST  \n\n```\n{:#?}\n```\n\n# Bytecode IR  \n\n```\n{:#?}\n```\n\n# Raw Bytecode  \n\n```\n{}\n```\n",
                    CODE,
                    ast,
                    &bytecode,
                    super::bytecode::encode_program(bytecode.0.clone(), bytecode.1.clone())
                        .into_iter()
                        .map(|b| format!("{:02X}", b))
                        .collect::<Vec<String>>()
                        .chunks(16)
                        .map(|c| c.join(" "))
                        .collect::<Vec<String>>()
                        .join("\n"),
                ).as_bytes()
            ).unwrap();
        }

        Err(err) => {
            let writer = codespan_reporting::term::termcolor::StandardStream::stderr(
                codespan_reporting::term::termcolor::ColorChoice::Auto,
            );

            let config = codespan_reporting::term::Config::default();

            let mut files = Files::new();
            files.add(FILENAME, CODE);

            for e in err {
                codespan_reporting::term::emit(&mut writer.lock(), &config, &files, &e).unwrap();
            }
        }
    }
}
