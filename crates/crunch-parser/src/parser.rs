use crate::ast::*;
use crate::token::*;

use crunch_error::parse_prelude::*;
use pipeline_job::PipelineJob;
use string_interner::{StringInterner, Sym};

use alloc::{collections::VecDeque, sync::Arc};
use core::{char, ops, panic::Location as PanicLoc};

use std::sync::{Mutex, RwLock};

/*
  _________  ___  ___  _______           ________  ________  ________  ________  _______   ________          ________   _______   _______   ________  ________
 |\___   ___\\  \|\  \|\  ___ \         |\   __  \|\   __  \|\   __  \|\   ____\|\  ___ \ |\   __  \        |\   ___  \|\  ___ \ |\  ___ \ |\   ___ \|\   ____\
 \|___ \  \_\ \  \\\  \ \   __/|        \ \  \|\  \ \  \|\  \ \  \|\  \ \  \___|\ \   __/|\ \  \|\  \       \ \  \\ \  \ \   __/|\ \   __/|\ \  \_|\ \ \  \___|_
      \ \  \ \ \   __  \ \  \_|/__       \ \   ____\ \   __  \ \   _  _\ \_____  \ \  \_|/_\ \   _  _\       \ \  \\ \  \ \  \_|/_\ \  \_|/_\ \  \ \\ \ \_____  \
       \ \  \ \ \  \ \  \ \  \_|\ \       \ \  \___|\ \  \ \  \ \  \\  \\|____|\  \ \  \_|\ \ \  \\  \|       \ \  \\ \  \ \  \_|\ \ \  \_|\ \ \  \_\\ \|____|\  \
        \ \__\ \ \__\ \__\ \_______\       \ \__\    \ \__\ \__\ \__\\ _\ ____\_\  \ \_______\ \__\\ _\        \ \__\\ \__\ \_______\ \_______\ \_______\____\_\  \
         \|__|  \|__|\|__|\|_______|        \|__|     \|__|\|__|\|__|\|__|\_________\|_______|\|__|\|__|        \|__| \|__|\|_______|\|_______|\|_______|\_________\
                                                                         \|_________|                                                                   \|_________|


  _________  ________          ________  _______  _________        ___  _________  ________           ________  ___  ___  ___  _________
 |\___   ___\\   __  \        |\   ____\|\  ___ \|\___   ___\     |\  \|\___   ___\\   ____\         |\   ____\|\  \|\  \|\  \|\___   ___\
 \|___ \  \_\ \  \|\  \       \ \  \___|\ \   __/\|___ \  \_|     \ \  \|___ \  \_\ \  \___|_        \ \  \___|\ \  \\\  \ \  \|___ \  \_|
      \ \  \ \ \  \\\  \       \ \  \  __\ \  \_|/__  \ \  \       \ \  \   \ \  \ \ \_____  \        \ \_____  \ \   __  \ \  \   \ \  \
       \ \  \ \ \  \\\  \       \ \  \|\  \ \  \_|\ \  \ \  \       \ \  \   \ \  \ \|____|\  \        \|____|\  \ \  \ \  \ \  \   \ \  \
        \ \__\ \ \_______\       \ \_______\ \_______\  \ \__\       \ \__\   \ \__\  ____\_\  \         ____\_\  \ \__\ \__\ \__\   \ \__\
         \|__|  \|_______|        \|_______|\|_______|   \|__|        \|__|    \|__| |\_________\       |\_________\|__|\|__|\|__|    \|__|
                                                                                     \|_________|       \|_________|


  _________  ________  ________  _______  _________  ___  ___  _______   ________
 |\___   ___\\   __  \|\   ____\|\  ___ \|\___   ___\\  \|\  \|\  ___ \ |\   __  \
 \|___ \  \_\ \  \|\  \ \  \___|\ \   __/\|___ \  \_\ \  \\\  \ \   __/|\ \  \|\  \
      \ \  \ \ \  \\\  \ \  \  __\ \  \_|/__  \ \  \ \ \   __  \ \  \_|/_\ \   _  _\
       \ \  \ \ \  \\\  \ \  \|\  \ \  \_|\ \  \ \  \ \ \  \ \  \ \  \_|\ \ \  \\  \|
        \ \__\ \ \_______\ \_______\ \_______\  \ \__\ \ \__\ \__\ \_______\ \__\\ _\
         \|__|  \|_______|\|_______|\|_______|   \|__|  \|__|\|__|\|_______|\|__|\|__|
*/

pub struct Parser<'a> {
    token_stream: TokenStream<'a>,
    next: Option<Token<'a>>,
    peek: Option<Token<'a>>,
    current_file: usize,
    diagnostics: Vec<ParserDiagnostic>,
    error: bool,
    interner: Arc<RwLock<StringInterner<Sym>>>,
    job_queue: Arc<Mutex<VecDeque<PipelineJob>>>,
}

impl<'a> Parser<'a> {
    pub fn new(
        source: &'a str,
        current_file: usize,
        interner: Arc<RwLock<StringInterner<Sym>>>,
        job_queue: Arc<Mutex<VecDeque<PipelineJob>>>,
    ) -> Self {
        let mut token_stream = TokenStream::new(source, true);
        let next = None;
        let peek = token_stream.next_token();

        Self {
            token_stream,
            next,
            peek,
            current_file,
            diagnostics: Vec::with_capacity(20),
            error: false,
            interner,
            job_queue,
        }
    }

    pub fn parse(
        &mut self,
    ) -> std::result::Result<(Vec<Program>, Vec<ParserDiagnostic>), Vec<ParserDiagnostic>> {
        const TOP_LEVEL_TOKENS: [TokenType; 2] = [TokenType::Import, TokenType::Function];

        let mut ast = Vec::new();
        let mut attributes = Vec::with_capacity(5);
        let mut decorators = Vec::with_capacity(5);

        while let Ok(token) = self.peek() {
            match token.ty {
                TokenType::Library | TokenType::Exposed => {
                    attributes.push(match self.attribute() {
                        Ok(vis) => vis,
                        Err(err) => {
                            self.error = true;
                            self.diagnostics.push(err);
                            break;
                        }
                    });
                }

                TokenType::AtSign => {
                    info!("Top Level Loop: Decorator");

                    decorators.push(self.decorator().unwrap_or_else(|_| panic!()));
                }

                TokenType::Function => {
                    info!("Top Level Loop: Function");

                    ast.push(Program::FunctionDecl(
                        match self.function_declaration(
                            attributes.drain(..).collect(),
                            decorators.drain(..).collect(),
                        ) {
                            Ok(node) => node,
                            Err(err) => {
                                self.error = true;
                                self.diagnostics.push(err);
                                break;
                            }
                        },
                    ));
                }

                TokenType::Import => {
                    info!("Top Level Loop: Import");
                    ast.push(Program::Import(match self.parse_import() {
                        Ok(node) => node,
                        Err(err) => {
                            self.error = true;
                            self.diagnostics.push(err);
                            break;
                        }
                    }))
                }

                TokenType::Type => {
                    info!("Top Level Loop: Type");
                    ast.push(Program::TypeDecl(
                        match self.parse_type_decl(
                            attributes.drain(..).collect(),
                            decorators.drain(..).collect(),
                        ) {
                            Ok(node) => node,
                            Err(err) => {
                                self.error = true;
                                self.diagnostics.push(err);
                                break;
                            }
                        },
                    ))
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

                    self.diagnostics.push(
                        Diagnostic::error()
                            .with_message("Invalid token")
                            .with_code("E001")
                            .with_labels(vec![Label::primary(self.current_file, token.range())
                                .with_message(format!("{:?} is not a valid token", token.source))]),
                    );

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

                    self.diagnostics.push(
                        Diagnostic::error()
                            .with_message("Invalid top-level token")
                            .with_code("E001")
                            .with_labels(vec![Label::primary(self.current_file, token.range())
                                .with_message(format!(
                                    "Found {:?} but expected one of {}",
                                    token.ty,
                                    TOP_LEVEL_TOKENS
                                        .iter()
                                        .map(|t| format!("'{}'", t))
                                        .collect::<Vec<String>>()
                                        .join(", "),
                                ))]),
                    );

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
            info!("Finished parsing: {:#?}", &ast);
            Ok((ast, diagnostics))
        }
    }

    fn decorator(&mut self) -> ParseResult<Decorator> {
        todo!("decorators")
    }

    #[inline]
    fn intern(&mut self, string: &str) -> Sym {
        self.interner.write().unwrap().get_or_intern(string)
    }

    #[track_caller]
    fn attribute(&mut self) -> ParseResult<Attribute> {
        trace!("Parsing Visibility: {:?}", PanicLoc::caller());

        Ok(match self.peek()?.ty {
            TokenType::Exposed => {
                self.eat(TokenType::Exposed)?;
                Attribute::Exposure(Exposure::All)
            }
            TokenType::Library => {
                self.eat(TokenType::Library)?;
                Attribute::Exposure(Exposure::CurrentLib)
            }
            // TODO: Should this be an error?
            _ => Attribute::Exposure(Exposure::CurrentFile),
        })
    }

    /// Parses a type declaration
    ///
    /// ```crunch
    /// type Name<T>
    ///     generic: T
    ///     something: str
    ///
    ///     fn test()
    ///         println("Test")
    ///     end
    /// end
    /// ```
    ///
    /// ```ebnf
    /// TypeDeclaration ::= Visibility? 'type' Ident ( '<' Ident ',' '>' )? '\n' TypeArguments* Function* End
    /// ```
    fn parse_type_decl(
        &mut self,
        type_attributes: Vec<Attribute>,
        type_decorators: Vec<Decorator>,
    ) -> ParseResult<TypeDecl> {
        let start = self.eat(TokenType::Type)?.range.0;

        let name = self.eat(TokenType::Ident)?;
        let name = self.intern(name.source);

        let generics = self.generics()?;
        let decl_end = self.eat(TokenType::Newline)?.range.1;

        let decl_loc = Location::new(self.current_file, (start, decl_end));

        let mut attributes = Vec::with_capacity(5);
        let mut decorators = Vec::with_capacity(5);
        let mut members = Vec::new();
        let mut peek = self.peek()?;

        while peek.ty != TokenType::Function && peek.ty != TokenType::EndBlock {
            if peek.ty == TokenType::Newline {
                self.eat(TokenType::Newline)?;
                peek = self.peek()?;
                continue;
            } else if peek.ty == TokenType::Library || peek.ty == TokenType::Exposed {
                attributes.push(self.attribute()?);
                peek = self.peek()?;
                continue;
            }

            let member = self.eat(TokenType::Ident)?;
            let member = self.intern(member.source);

            self.eat(TokenType::Colon)?;
            let ty = self.parse_type()?;

            self.eat(TokenType::Newline)?;

            members.push((
                {
                    let mut attr: Vec<Attribute> = attributes.drain(..).collect();

                    if attr.is_empty() {
                        Exposure::default()
                    } else if attr.len() > 1 {
                        todo!("error")
                    } else {
                        attr.remove(0).as_exposure().unwrap()
                    }
                },
                member,
                ty,
            ));
            peek = self.peek()?;
        }

        let mut methods = Vec::new();
        while peek.ty != TokenType::EndBlock {
            if peek.ty == TokenType::Newline {
                self.eat(TokenType::Newline)?;
                peek = self.peek()?;
                continue;
            } else if peek.ty == TokenType::Library || peek.ty == TokenType::Exposed {
                attributes.push(self.attribute()?);
            }

            methods.push(self.function_declaration(
                attributes.drain(..).collect(),
                decorators.drain(..).collect(),
            )?);
            peek = self.peek()?;
        }

        let end = self.eat(TokenType::EndBlock)?.range.1;

        Ok(TypeDecl {
            decorators: type_decorators,
            attributes: type_attributes,
            name,
            generics,
            members,
            methods,
            decl_loc,
            loc: Location::new(self.current_file, (start, end)),
        })
    }

    // TODO: Clean this up
    fn parse_import(&mut self) -> ParseResult<Import> {
        info!("Parsing Import");

        let start = self.eat(TokenType::Import)?.range.0;

        let source = self.import_source()?;

        let (exposes, alias, end) = match self.peek()?.ty {
            TokenType::Exposing => {
                self.eat(TokenType::Exposing)?;

                let peek = self.peek()?;
                if peek.ty == TokenType::Star {
                    self.eat(TokenType::Star)?;

                    let end = self.eat(TokenType::Newline)?.range.1;

                    (Exposes::All, None, end)
                } else if peek.ty == TokenType::Ident {
                    let (mut imports, mut peek) = (Vec::new(), self.peek()?);

                    while peek.ty != TokenType::Newline {
                        let ident = self.eat(TokenType::Ident)?;
                        let ident = self.intern(ident.source);

                        let token = self.peek()?;
                        let alias = if token.ty == TokenType::As {
                            self.eat(TokenType::As)?;

                            let ident = self.eat(TokenType::Ident)?;
                            Some(self.intern(ident.source))
                        } else {
                            None
                        };

                        imports.push((ident, alias));

                        peek = self.peek()?;

                        if peek.ty == TokenType::Comma {
                            self.eat(TokenType::Comma)?;
                        } else {
                            break;
                        }
                    }

                    let end = self.eat(TokenType::Newline)?.range.1;

                    (Exposes::Some(imports), None, end)
                } else {
                    self.error = true;
                    error!(
                        "[Parsing error on {}:{}]: Expected exposed members",
                        line!(),
                        column!(),
                    );

                    return Err(Diagnostic::error()
                        .with_message("Expected exposed members")
                        .with_labels(vec![Label::primary(
                            self.current_file,
                            start..peek.range.1,
                        )
                        .with_message(
                            "You must expose something when using the `exposing` keyword",
                        )]));
                }
            }

            TokenType::As => {
                self.eat(TokenType::As)?;

                let alias = self.eat(TokenType::Ident)?;
                let alias = self.intern(alias.source);

                if self.peek()?.ty == TokenType::Exposing {
                    self.eat(TokenType::Exposing)?;

                    let peek = self.peek()?;

                    if peek.ty == TokenType::Star {
                        self.eat(TokenType::Star)?;
                        let end = self.eat(TokenType::Newline)?.range.1;

                        (Exposes::All, Some(alias), end)
                    } else if peek.ty == TokenType::Ident {
                        let (mut imports, mut peek) = (Vec::new(), self.peek()?);

                        while peek.ty != TokenType::Newline {
                            let ident = self.eat(TokenType::Ident)?;
                            let ident = self.intern(ident.source);

                            let token = self.peek()?;

                            let alias = if token.ty == TokenType::As {
                                self.eat(TokenType::As)?;

                                let ident = self.eat(TokenType::Ident)?;
                                Some(self.intern(ident.source))
                            } else {
                                None
                            };

                            imports.push((ident, alias));

                            peek = self.peek()?;

                            if peek.ty == TokenType::Comma {
                                self.eat(TokenType::Comma)?;
                            } else {
                                break;
                            }
                        }

                        let end = self.eat(TokenType::Newline)?.range.1;

                        (Exposes::Some(imports), Some(alias), end)
                    } else {
                        self.error = true;
                        error!(
                            "[Parsing error on {}:{}]: Expected exposed members",
                            line!(),
                            column!(),
                        );

                        return Err(Diagnostic::error()
                            .with_message("Expected exposed members")
                            .with_labels(vec![Label::primary(
                                self.current_file,
                                start..peek.range.1,
                            )
                            .with_message(
                                "You must expose something when using the `exposing` keyword",
                            )]));
                    }
                } else {
                    let end = self.eat(TokenType::Newline)?.range.1;
                    (Exposes::All, Some(alias), end)
                }
            }

            _ => {
                let end = self.eat(TokenType::Newline)?.range.1;
                (Exposes::File, None, end)
            }
        };

        info!("Finished parsing Import");

        Ok(Import {
            source,
            alias,
            exposes,
            loc: Location::new(self.current_file, (start, end)),
        })
    }

    fn import_source(&mut self) -> ParseResult<ImportSource> {
        todo!("Add files to the job queue")
    }

    fn generics(&mut self) -> ParseResult<Vec<Type>> {
        let mut generics = Vec::new();

        let mut peek = self.peek()?;
        let start = peek.range.0;

        if peek.ty == TokenType::LeftCaret {
            self.eat(TokenType::LeftCaret)?;

            peek = self.peek()?;
            while peek.ty != TokenType::RightCaret {
                generics.push(self.parse_type()?);

                peek = self.peek()?;
                if peek.ty == TokenType::Comma {
                    self.eat(TokenType::Comma)?;
                } else {
                    break;
                }
            }

            peek = self.peek()?;
            if peek.ty == TokenType::Ident {
                return Err(Diagnostic::new(Severity::Error)
                    .with_message("Missing comma")
                    .with_labels(vec![Label::primary(self.current_file, start..peek.range.1)
                        .with_message("Missing a comma between generic parameters")]));
            }

            self.eat(TokenType::RightCaret)?;
        }

        Ok(generics)
    }

    fn loop_loop(&mut self) -> ParseResult<Loop> {
        let start = self.eat(TokenType::Loop)?.range.0;
        let body = self.body()?;
        let end = self.eat(TokenType::EndBlock)?.range.1;

        Ok(Loop {
            body,
            loc: Location::new(self.current_file, (start, end)),
        })
    }

    // Note: Eats the EndBlock and returns the end of its span
    fn then(&mut self) -> ParseResult<(Option<Else>, usize)> {
        let then = if self.peek()?.ty == TokenType::Then {
            let start = self.eat(TokenType::Then)?.range.0;
            self.eat(TokenType::Newline)?;

            let body = self.body()?;
            let end = self.eat(TokenType::EndBlock)?.range.1;
            (
                Some(Else {
                    body,
                    loc: Location::new(self.current_file, (start, end)),
                }),
                end,
            )
        } else {
            let end = self.eat(TokenType::EndBlock)?.range.1;
            (None, end)
        };

        Ok(then)
    }

    fn while_loop(&mut self) -> ParseResult<While> {
        let start = self.eat(TokenType::While)?.range.0;
        let (condition, _) = self.expr()?;
        self.eat(TokenType::Newline)?;
        let body = self.body()?;
        let (then, end) = self.then()?;

        Ok(While {
            condition,
            body,
            then,
            loc: Location::new(self.current_file, (start, end)),
        })
    }

    fn for_loop(&mut self) -> ParseResult<For> {
        let start = self.eat(TokenType::For)?.range.0;
        let element = self.eat(TokenType::Ident)?;
        let element = self.intern(element.source);

        self.eat(TokenType::In)?;

        let (range, _) = self.expr()?;
        self.eat(TokenType::Newline)?;

        let body = self.body()?;
        let (then, end) = self.then()?;

        Ok(For {
            element,
            range,
            body,
            then,
            loc: Location::new(self.current_file, (start, end)),
        })
    }

    /// Parses a Variable's Declaration, eg.
    ///
    /// ```crunch
    /// let i = 10
    /// let i: int = 10
    /// ```
    ///
    /// ```ebnf
    /// VarDeclaration ::= 'let' Ident ( ':' Ident )? '=' Expr '\n'
    /// ```
    fn variable_decl(&mut self) -> ParseResult<VarDecl> {
        let start = self.eat(TokenType::Let)?.range.0;
        let name = self.eat(TokenType::Ident)?;
        let name = self.intern(name.source);

        let ty = if self.peek()?.ty == TokenType::Colon {
            self.eat(TokenType::Colon)?;
            self.parse_type()?
        } else {
            Type::Infer
        };

        self.eat(TokenType::Equal)?;
        let (expr, _) = self.expr()?;

        let end = self.eat(TokenType::Newline)?.range.1;

        Ok(VarDecl {
            name,
            ty,
            expr,
            loc: Location::new(self.current_file, (start, end)),
        })
    }

    #[allow(dead_code)]
    fn optionally_typed_argument(&mut self) -> ParseResult<Vec<(Sym, Type)>> {
        self.eat(TokenType::LeftParen)?;

        let mut params = Vec::new();
        while self.peek()?.ty != TokenType::RightParen {
            params.push(self.parse_typed_argument()?);

            if self.peek()?.ty == TokenType::Comma {
                self.eat(TokenType::Comma)?;
            } else {
                break;
            }
        }

        self.eat(TokenType::RightParen)?;

        Ok(params)
    }

    fn function_call_arguments(&mut self) -> ParseResult<(Vec<Expr>, usize)> {
        self.eat(TokenType::LeftParen)?;

        let mut params = Vec::new();
        while self.peek()?.ty != TokenType::RightParen {
            let (expr, _) = self.expr()?;
            params.push(expr);

            if self.peek()?.ty == TokenType::Comma {
                self.eat(TokenType::Comma)?;
            } else {
                break;
            }
        }

        let end = self.eat(TokenType::RightParen)?.range.1;

        Ok((params, end))
    }

    fn function_call(
        &mut self,
        input: impl Into<Option<(Sym, usize)>>,
    ) -> ParseResult<FunctionCall> {
        let (name, start) = if let Some((name, start)) = input.into() {
            (name, start)
        } else {
            let ident = self.eat(TokenType::Ident)?;
            (self.intern(ident.source), ident.range.0)
        };
        let generics = self.generics()?;
        let (arguments, end) = self.function_call_arguments()?;

        Ok(FunctionCall {
            name,
            generics,
            arguments,
            loc: Location::new(self.current_file, (start, end)),
        })
    }

    fn assign_type(&mut self) -> ParseResult<AssignType> {
        trace!("Parsing assign type");
        let next = self.next()?;
        let ty = match next.ty {
            TokenType::Equal => AssignType::Normal,
            TokenType::Caret
            | TokenType::Pipe
            | TokenType::Star
            | TokenType::Plus
            | TokenType::Minus
            | TokenType::Divide
            | TokenType::Ampersand => {
                self.eat(TokenType::Equal)?;
                AssignType::BinaryOp(self.binary_operand(next)?.0)
            }
            e => {
                return Err(Diagnostic::error()
                    .with_message("Invalid assignment type")
                    .with_labels(vec![Label::primary(
                        self.current_file,
                        next.range.0..next.range.1,
                    )
                    .with_message(format!(
                        "Expected one of '=', '^', '|', '+', '-', '&', '*' or '/', got '{}'",
                        e
                    ))]));
            }
        };

        Ok(ty)
    }

    fn assign(&mut self, input: impl Into<Option<(Sym, usize)>>) -> ParseResult<Assign> {
        let (var, start) = if let Some((var, start)) = input.into() {
            (var, start)
        } else {
            let ident = self.eat(TokenType::Ident)?;
            (self.intern(ident.source), ident.range.0)
        };

        let ty = self.assign_type()?;
        let (expr, _) = self.expr()?;
        let end = self.eat(TokenType::Newline)?.range.1;

        Ok(Assign {
            var,
            expr,
            ty,
            loc: Location::new(self.current_file, (start, end)),
        })
    }

    fn array(&mut self) -> ParseResult<(Expr, usize)> {
        trace!("Parsing array");

        self.eat(TokenType::LeftBrace)?;

        let mut array = Vec::with_capacity(5);

        if self.peek()?.ty != TokenType::RightBrace {
            array.push(self.expr()?.0);

            if self.peek()?.ty == TokenType::Semicolon {
                self.eat(TokenType::Semicolon)?;

                let range = self.peek()?.range();
                if let Literal::Integer(integer) = self.parse_literal()? {
                    array.reserve(integer as usize - 1);

                    let proto = array[0].clone();
                    for _ in 0..integer - 1 {
                        array.push(proto.clone());
                    }
                } else {
                    return Err(Diagnostic::error()
                        .with_message("Cannot fill arrays by size of non-integer")
                        .with_labels(vec![Label::primary(self.current_file, range)
                            .with_message("Expected an integer".to_string())]));
                }
            }
        }

        let end = self.eat(TokenType::RightBrace)?.range.1;

        Ok((Expr::Array(array), end))
    }

    fn expr(&mut self) -> ParseResult<(Expr, usize)> {
        trace!("Parsing expr");

        let token = self.peek()?;
        let expr = match token.ty {
            TokenType::LeftParen => {
                self.eat(TokenType::LeftParen)?;
                let (expr, _) = self.expr()?;
                let end = self.eat(TokenType::RightParen)?.range.1;

                (Expr::Expr(Box::new(expr)), end)
            }

            TokenType::LeftBrace => {
                todo!("Array indexing");
                // if let Some(prev) = prev.into() {
                //     self.array()?
                // } else {
                //     self.eat(TokenType::LeftBrace)?;
                //     let (expr, _) = self.expr()?;
                //     self.eat(TokenType::RightBrace)?;
                //     // self.expr()
                //     // Done fucked up with parsing `array[idx] += 10`
                // }
            }

            // TokenType::LeftCaret => todo!("Things that take generics"),
            TokenType::Ident => {
                let ident = self.eat(TokenType::Ident)?;

                match self.peek()?.ty {
                    TokenType::LeftParen => {
                        let interned = self.intern(ident.source);
                        let func =
                            Expr::FunctionCall(self.function_call((interned, ident.range.0))?);
                        self.extended_expr(func, ident.range.0)?
                    }

                    TokenType::Plus
                    | TokenType::Minus
                    | TokenType::Divide
                    | TokenType::Star
                    | TokenType::Pipe
                    | TokenType::Ampersand
                    | TokenType::Caret => {
                        let next = self.next()?;

                        let op = self.binary_operand(next)?;
                        let (right, end) = self.expr()?;
                        let right = Box::new(right);

                        let op = Expr::BinaryOperation(BinaryOperation {
                            left: Box::new(Expr::Ident(self.intern(ident.source))),
                            op,
                            right,
                            loc: Location::new(self.current_file, (ident.range.0, end)),
                        });
                        self.extended_expr(op, ident.range.0)?
                    }

                    _ => (Expr::Ident(self.intern(ident.source)), ident.range.1),
                }
            }

            TokenType::String | TokenType::Int | TokenType::Bool => {
                let lit = Expr::Literal(self.parse_literal()?);
                self.extended_expr(lit, token.range.0)?
            }

            TokenType::Newline => {
                self.eat(TokenType::Newline)?;
                self.expr()?
            }

            _ => {
                return Err(Diagnostic::error()
                    .with_message("Invalid token")
                    .with_labels(vec![Label::primary(self.current_file, token.range())
                        .with_message(format!(
                            "{:?} is not a valid expression",
                            token.source
                        ))]));
            }
        };

        Ok(expr)
    }

    fn extended_expr(&mut self, expr: Expr, start: usize) -> ParseResult<(Expr, usize)> {
        let next = self.peek()?;
        if [
            TokenType::Plus,
            TokenType::Minus,
            TokenType::Divide,
            TokenType::Star,
            TokenType::Pipe,
            TokenType::Ampersand,
            TokenType::Caret,
        ]
        .contains(&next.ty)
        {
            let next = self.next()?;
            let op = self.binary_operand(next)?;
            let (right, end) = self.expr()?;
            let right = Box::new(right);

            Ok((
                Expr::BinaryOperation(BinaryOperation {
                    left: Box::new(expr),
                    op,
                    right,
                    loc: Location::new(self.current_file, (start, end)),
                }),
                end,
            ))
        } else if self.peek()?.ty == TokenType::Dot {
            self.eat(TokenType::Dot)?;

            if self.peek()?.ty == TokenType::Dot {
                self.eat(TokenType::Dot)?;
                let (end, range_end) = self.expr()?;

                Ok((
                    Expr::Range(Range {
                        start: Box::new(expr),
                        end: Box::new(end),
                        loc: Location::new(self.current_file, (start, range_end)),
                    }),
                    range_end,
                ))
            } else {
                Err(Diagnostic::error()
                    .with_message("Function calls by dotted access are not yet implemented")
                    .with_labels(vec![Label::primary(self.current_file, next.range())
                        .with_message(
                            "Function calls by dotted access are not yet implemented",
                        )]))
            }
        } else {
            Ok((expr, start))
        }
    }

    fn binary_operand(
        &mut self,
        left: impl Into<Option<Token<'a>>>,
    ) -> ParseResult<(BinaryOp, OperandType)> {
        trace!("Parsing binary operand");

        if let Some(begin) = left.into() {
            let op = match begin.ty {
                TokenType::Plus => BinaryOp::Plus,
                TokenType::Minus => BinaryOp::Minus,
                TokenType::Divide => BinaryOp::Div,
                TokenType::Star => BinaryOp::Mult,
                TokenType::Pipe => BinaryOp::Or,
                TokenType::Ampersand => BinaryOp::And,
                TokenType::Caret => BinaryOp::Xor,
                e => {
                    return Err(Diagnostic::error()
                        .with_message("Invalid operand type")
                        .with_labels(vec![Label::primary(self.current_file, begin.range())
                            .with_message(format!(
                                "Expected one of '^', '|', '+', '-', '&', '*' or '/', got '{}'",
                                e
                            ))]));
                }
            };

            // TODO: Add more operand types
            let ty = match self.peek()? {
                _ => OperandType::Normal,
            };

            trace!("Finished parsing parsing binary operand");
            Ok((op, ty))
        } else {
            let next = self.peek()?;

            warn!("Stand-alone binary operands are unimplemented");

            Err(Diagnostic::error()
                .with_message("Stand-alone binary operands are unimplemented")
                .with_labels(vec![Label::primary(self.current_file, next.range())
                    .with_message("Stand-alone binary operands are unimplemented")]))
        }
    }

    fn conditional_clause(&mut self) -> ParseResult<(Either<If, Vec<Statement>>, usize)> {
        #[derive(Debug, Copy, Clone, PartialEq, Eq)]
        enum CondType {
            If,
            ElseIf,
            Else,
        }

        trace!("Started parsing conditional");

        let cond_token = self.eat_of(&[TokenType::If, TokenType::Else])?;
        let cond = if cond_token.ty == TokenType::If {
            CondType::If
        } else if cond_token.ty == TokenType::Else && self.peek()?.ty == TokenType::If {
            self.eat(TokenType::If)?;
            CondType::ElseIf
        } else if cond_token.ty == TokenType::Else {
            CondType::Else
        } else {
            unreachable!("The only valid conditionals should be `if`, `else if` and `else`")
        };

        let condition = if cond == CondType::Else {
            None
        } else {
            let (left, left_end) = self.expr()?;
            let (comparison, right, end) = if self.peek()?.ty == TokenType::Newline {
                let comparison = Comparator::Equal;
                let right = Box::new(Expr::Literal(Literal::Boolean(true)));
                (comparison, right, left_end)
            } else {
                let comparison = self.comparator()?;
                let (right, end) = self.expr()?;
                (comparison, Box::new(right), end)
            };

            Some(Expr::Comparison(Comparison {
                left: Box::new(left),
                comparison,
                right,
                loc: Location::new(self.current_file, (cond_token.range.0, end)),
            }))
        };
        self.eat(TokenType::Newline)?;

        let body = self.body()?;
        // HACK: Think through this better
        let end = self.peek()?.range.1;

        trace!("Finished parsing conditional");

        Ok((
            match cond {
                CondType::If | CondType::ElseIf => Either::Left(If {
                    condition: condition.expect("`if` and `else if` clauses have conditions"),
                    body,
                    loc: Location::new(self.current_file, (cond_token.range.0, end)),
                }),
                CondType::Else => Either::Right(body),
            },
            end,
        ))
    }

    fn comparator(&mut self) -> ParseResult<Comparator> {
        let token = self.peek()?;
        Ok(match token.ty {
            TokenType::IsEqual => {
                self.eat(TokenType::IsEqual)?;
                Comparator::Equal
            }
            TokenType::IsNotEqual => {
                self.eat(TokenType::IsNotEqual)?;
                Comparator::NotEqual
            }
            TokenType::GreaterThanEqual => {
                self.eat(TokenType::GreaterThanEqual)?;
                Comparator::GreaterEqual
            }
            TokenType::LessThanEqual => {
                self.eat(TokenType::LessThanEqual)?;
                Comparator::LessEqual
            }
            TokenType::LeftCaret => {
                self.eat(TokenType::LeftCaret)?;
                Comparator::Less
            }
            TokenType::RightCaret => {
                self.eat(TokenType::RightCaret)?;
                Comparator::Greater
            }

            e => {
                return Err(Diagnostic::error()
                    .with_message("Invalid Comparison modifier")
                    .with_labels(vec![Label::primary(self.current_file, token.range())
                        .with_message(format!(
                            "Expected one of '=', '!', '>', '<', got {}",
                            e
                        ))]));
            }
        })
    }

    fn conditional(&mut self) -> ParseResult<Conditional> {
        let start = self.peek()?.range.0;
        let (mut peek, mut if_clauses, mut else_body) = (self.peek()?, Vec::new(), None);
        while peek.ty == TokenType::If || peek.ty == TokenType::Else {
            match self.conditional_clause()? {
                (Either::Left(condition), _end) => if_clauses.push(condition),
                (Either::Right(body), end) => {
                    else_body = Some(Else {
                        body,
                        loc: Location::new(self.current_file, (peek.range.0, end)),
                    })
                }
            }

            peek = self.peek()?;
        }
        let end = self.eat(TokenType::EndBlock)?.range.1;

        Ok(Conditional {
            _if: if_clauses,
            _else: else_body,
            loc: Location::new(self.current_file, (start, end)),
        })
    }

    fn body(&mut self) -> ParseResult<Vec<Statement>> {
        trace!("Parsing body");

        let mut statements = Vec::new();

        loop {
            trace!("Parsing statement");

            let start_token = self.peek()?;
            let statement = match start_token.ty {
                TokenType::If => Statement::Conditional(self.conditional()?),
                TokenType::While => Statement::While(self.while_loop()?),
                TokenType::Loop => Statement::Loop(self.loop_loop()?),
                TokenType::For => Statement::For(self.for_loop()?),
                TokenType::Let => Statement::VarDecl(self.variable_decl()?),
                TokenType::Ident => {
                    let ident = self.eat(TokenType::Ident)?;

                    let peek = self.peek()?;
                    match peek.ty {
                        TokenType::LeftParen => {
                            let interned = self.intern(ident.source);
                            Statement::Expr(Expr::FunctionCall(
                                self.function_call((interned, ident.range.0))?,
                            ))
                        }

                        TokenType::Equal
                        | TokenType::Caret
                        | TokenType::Pipe
                        | TokenType::Star
                        | TokenType::Plus
                        | TokenType::Minus
                        | TokenType::Divide
                        | TokenType::Ampersand => {
                            let interned = self.intern(ident.source);
                            Statement::Assign(self.assign((interned, ident.range.0))?)
                        }

                        e => {
                            if let Ok((expr, _)) = self.expr() {
                                Statement::Expr(expr)
                            } else {
                                return Err(Diagnostic::error()
                                    .with_message("Unexpected token")
                                    .with_labels(vec![Label::primary(
                                        self.current_file,
                                        peek.range(),
                                    )
                                    .with_message(format!("Did not expect a {}", e))]));
                            }
                        }
                    }
                }
                TokenType::Return => {
                    let start = self.eat(TokenType::Return)?.range.0;
                    let (expr, end) = self.expr()?;
                    Statement::Return(Return {
                        expr: Some(expr),
                        loc: Location::new(self.current_file, (start, end)),
                    })
                }
                TokenType::Continue => {
                    self.eat(TokenType::Continue)?;
                    Statement::Continue
                }
                TokenType::Break => {
                    self.eat(TokenType::Break)?;
                    Statement::Break
                }
                TokenType::Empty => {
                    self.eat(TokenType::Empty)?;
                    Statement::Empty
                }
                TokenType::Newline => {
                    self.eat(TokenType::Newline)?;
                    continue;
                }
                // TODO: ( Expr '\n' )
                _ => break,
            };

            statements.push(statement);
        }

        Ok(statements)
    }

    #[track_caller]
    fn function_declaration(
        &mut self,
        func_attributes: Vec<Attribute>,
        func_decorators: Vec<Decorator>,
    ) -> ParseResult<FunctionDecl> {
        info!("Parsing Function: {:?}", PanicLoc::caller());

        let start = self.eat(TokenType::Function)?.range.1;

        let name = self.eat(TokenType::Ident)?;
        let name = self.intern(name.source);
        let generics = self.generics()?;
        let arguments = self.function_arguments()?;
        let returns = self.function_return()?;

        let decl_end = self.eat(TokenType::Newline)?.range.1;

        let body = self.body()?;

        let end = self.eat(TokenType::EndBlock)?.range.1;

        info!(
            "Finished parsing Function {:?}: {}",
            name,
            self.interner.read().unwrap().resolve(name).unwrap()
        );

        Ok(FunctionDecl {
            decorators: func_decorators,
            attributes: func_attributes,
            name,
            generics,
            arguments,
            returns,
            body,
            decl_loc: Location::new(self.current_file, (start, decl_end)),
            loc: Location::new(self.current_file, (start, end)),
        })
    }

    fn function_arguments(&mut self) -> ParseResult<Vec<(Sym, Type)>> {
        self.eat(TokenType::LeftParen)?;

        let mut params = Vec::new();
        while self.peek()?.ty != TokenType::RightParen {
            params.push(self.parse_typed_argument()?);

            if self.peek()?.ty == TokenType::Comma {
                self.eat(TokenType::Comma)?;
            } else {
                break;
            }
        }

        self.eat(TokenType::RightParen)?;

        Ok(params)
    }

    fn function_return(&mut self) -> ParseResult<Type> {
        if self.peek()?.ty == TokenType::RightArrow {
            self.eat(TokenType::RightArrow)?;

            Ok(self.parse_type()?)
        } else {
            Ok(Type::default())
        }
    }

    #[inline]
    fn parse_literal(&mut self) -> ParseResult<Literal> {
        info!("Parsing Variable");

        let token = self.next()?;

        let literal = match token.ty {
            // TODO: Allow other integer sizes
            TokenType::Int => Literal::Integer(token.source.parse().map_err(|_err| {
                Diagnostic::error()
                    .with_message("Invalid Integer")
                    .with_labels(vec![Label::primary(self.current_file, token.range())
                        .with_message("Invalid int")])
            })?),
            TokenType::String => Literal::String({
                let string = &self
                    .escape_string(token.range(), &(&*token.source)[1..token.source.len() - 1])?;
                self.intern(string)
            }),
            TokenType::Bool => Literal::Boolean(token.source.parse().map_err(|_err| {
                Diagnostic::error()
                    .with_message("Invalid Boolean")
                    .with_labels(vec![Label::primary(self.current_file, token.range())
                        .with_message("Invalid boolean, expected `true` or `false`")])
            })?),
            e => {
                return Err(Diagnostic::error()
                    .with_message("Invalid Literal")
                    .with_labels(vec![Label::primary(self.current_file, token.range())
                        .with_message(format!(
                            "Invalid literal, expected 'Integer', 'String' or 'Boolean', got '{}'",
                            e
                        ))]));
            }
        };

        info!("Finished parsing Variable");

        Ok(literal)
    }

    #[inline]
    fn parse_typed_argument(&mut self) -> ParseResult<(Sym, Type)> {
        info!("Parsing Named Parameter");

        let name = self.eat(TokenType::Ident)?;
        let name = self.intern(name.source);
        self.eat(TokenType::Colon)?;

        let ty = self.parse_type()?;

        info!("Finished parsing Named Param");

        Ok((name, ty))
    }

    #[inline]
    fn parse_type(&mut self) -> ParseResult<Type> {
        info!("Parsing Type");

        let ty = match self.eat(TokenType::Ident)?.source {
            "unit" => Type::Unit,
            "str" => Type::String,
            "int" => Type::Int,
            "bool" => Type::Bool,
            "any" => Type::Any,
            custom => Type::Custom(self.intern(custom)),
        };

        info!("Finished parsing Type");

        Ok(ty)
    }
}

/// Parsing utilities
impl<'a> Parser<'a> {
    #[inline]
    #[track_caller]
    fn next(&mut self) -> ParseResult<Token<'a>> {
        let loc = PanicLoc::caller();

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

            Err(Diagnostic::error().with_message("Unexpected End Of File"))
        }
    }

    #[inline]
    #[track_caller]
    fn peek(&mut self) -> ParseResult<Token<'a>> {
        let loc = PanicLoc::caller();

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

            Err(Diagnostic::error().with_message("Unexpected end of file"))
        }
    }

    #[inline]
    #[track_caller]
    fn eat(&mut self, expected: TokenType) -> ParseResult<Token<'a>> {
        let loc = PanicLoc::caller();
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

            Err(Diagnostic::error()
                .with_message(format!(
                    "Unexpected Token: Expected '{}', found '{}'",
                    expected, token.ty
                ))
                .with_labels(vec![Label::primary(
                    self.current_file,
                    token.range.0 as usize..token.range.1 as usize,
                )
                .with_message(format!("Expected {}", expected))]))
        }
    }

    #[inline]
    fn eat_of(&mut self, expected: &[TokenType]) -> ParseResult<Token<'a>> {
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

            Err(Diagnostic::error()
                .with_message(format!(
                    "Unexpected Token: Expected one of {}, got '{}'",
                    expected
                        .iter()
                        .map(|t| format!("'{}'", t))
                        .collect::<Vec<String>>()
                        .join(", "),
                    token.ty
                ))
                .with_labels(vec![Label::primary(
                    self.current_file,
                    token.range.0 as usize..token.range.1 as usize,
                )
                .with_message(format!("Unexpected Token: {:?}", token.ty))]))
        }
    }

    #[inline]
    fn escape_string(&mut self, range: ops::Range<usize>, string: &str) -> ParseResult<String> {
        info!("Parsing Escape String");

        let mut queue: VecDeque<_> = String::from(string).chars().collect();
        let mut s = String::new();
        let error = Diagnostic::error()
            .with_message("Invalid Escape String")
            .with_labels(vec![
                Label::primary(self.current_file, range).with_message("Invalid Escape String")
            ]);

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
