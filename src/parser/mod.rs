pub(crate) mod ast;
#[cfg(test)]
mod tests;
mod token;

pub(crate) use ast::*;

use codespan::{FileId, Files};
use codespan_reporting::diagnostic::{Diagnostic, Label, Severity};
use std::{char, collections::VecDeque, panic::Location as PanicLoc};
use string_interner::{StringInterner, Sym};
use token::*;

type Result<T> = std::result::Result<T, Diagnostic>;

#[derive(Debug)]
pub struct Parser<'a> {
    token_stream: TokenStream<'a>,
    next: Option<Token<'a>>,
    peek: Option<Token<'a>>,
    codespan: Files,
    files: Vec<FileId>,
    pub(crate) current_file: FileId,
    diagnostics: Vec<Diagnostic>,
    error: bool,
    pub interner: StringInterner<Sym>,
    current_path: Sym,
}

impl<'a> Parser<'a> {
    #[must_use]
    pub fn new(filename: Option<&'a str>, input: &'a str) -> Self {
        let mut token_stream = TokenStream::new(input, true);

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

        let mut interner = StringInterner::new();
        let current_path = interner.get_or_intern(filename.unwrap_or_else(|| ""));

        Self {
            token_stream,
            next,
            peek,
            codespan,
            files,
            current_file,
            diagnostics: Vec::new(),
            error: false,
            interner,
            current_path,
        }
    }

    pub fn parse(
        &mut self,
    ) -> std::result::Result<(Vec<Program>, Vec<Diagnostic>), Vec<Diagnostic>> {
        const TOP_LEVEL_TOKENS: [TokenType; 2] = [TokenType::Import, TokenType::Function];

        let mut ast = Vec::new();
        let mut visibility = None;

        while let Ok(token) = self.peek() {
            match token.ty {
                TokenType::Library | TokenType::Exposed => {
                    visibility = match self.visibility() {
                        Ok(vis) => Some(vis),
                        Err(err) => {
                            self.error = true;
                            self.diagnostics.push(err);
                            break;
                        }
                    };
                }

                TokenType::Function => {
                    info!("Top Level Loop: Function");

                    ast.push(Program::FunctionDecl(
                        match self.function_declaration(visibility.take()) {
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
                        match self.parse_type_decl(visibility.take()) {
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
                    self.diagnostics.push(Diagnostic::new(
                        Severity::Error,
                        "Invalid token",
                        Label::new(
                            self.current_file,
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
                            self.current_file,
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
            info!("Finished parsing: {:#?}", &ast);
            Ok((ast, diagnostics))
        }
    }

    #[inline]
    fn intern(&mut self, string: &str) -> Sym {
        self.interner.get_or_intern(string)
    }

    #[track_caller]
    fn visibility(&mut self) -> Result<Visibility> {
        trace!("Parsing Visibility: {:?}", PanicLoc::caller());

        Ok(match self.peek()?.ty {
            TokenType::Exposed => {
                self.eat(TokenType::Exposed)?;
                Visibility::Exposed
            }
            TokenType::Library => {
                self.eat(TokenType::Library)?;
                Visibility::Library
            }
            _ => Visibility::File,
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
    fn parse_type_decl(&mut self, type_visibility: Option<Visibility>) -> Result<TypeDecl> {
        let start = self.eat(TokenType::Type)?.range.0;

        let name = self.eat(TokenType::Ident)?;
        let name = self.intern(name.source);

        let generics = self.generics()?;
        self.eat(TokenType::Newline)?;

        let mut visibility = None;
        let mut members = Vec::new();
        let mut peek = self.peek()?;

        while peek.ty != TokenType::Function && peek.ty != TokenType::EndBlock {
            if peek.ty == TokenType::Newline {
                self.eat(TokenType::Newline)?;
                peek = self.peek()?;
                continue;
            } else if peek.ty == TokenType::Library || peek.ty == TokenType::Exposed {
                visibility = Some(self.visibility()?);
                peek = self.peek()?;
                continue;
            }

            let member = self.eat(TokenType::Ident)?;
            let member = self.intern(member.source);

            self.eat(TokenType::Colon)?;
            let ty = self.parse_type()?;

            self.eat(TokenType::Newline)?;

            members.push((visibility.take().unwrap_or_default(), member, ty));
            peek = self.peek()?;
        }

        let mut methods = Vec::new();
        while peek.ty != TokenType::EndBlock {
            if peek.ty == TokenType::Newline {
                self.eat(TokenType::Newline)?;
                peek = self.peek()?;
                continue;
            } else if peek.ty == TokenType::Library || peek.ty == TokenType::Exposed {
                visibility = Some(self.visibility()?);
            }

            methods.push(self.function_declaration(visibility.take())?);
            peek = self.peek()?;
        }

        let end = self.eat(TokenType::EndBlock)?.range.1;

        Ok(TypeDecl {
            visibility: type_visibility.unwrap_or_default(),
            name,
            generics,
            members,
            methods,
            loc: Location::new(self.current_file, (start, end)),
        })
    }

    // TODO: Clean this up
    fn parse_import(&mut self) -> Result<Import> {
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
                    return Err(Diagnostic::new(
                        Severity::Error,
                        "Expected exposed members",
                        Label::new(
                            self.current_file,
                            self.codespan.source_span(self.current_file),
                            "You must expose something when using the `exposing` keyword"
                                .to_string(),
                        ),
                    ));
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
                        return Err(Diagnostic::new(
                            Severity::Error,
                            "Expected exposed members",
                            Label::new(
                                self.current_file,
                                self.codespan.source_span(self.current_file),
                                "You must expose something when using the `exposing` keyword",
                            ),
                        ));
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

    fn import_source(&mut self) -> Result<ImportSource> {
        let source = match self.peek()?.ty {
            TokenType::Library => {
                self.eat(TokenType::Library)?;
                ImportSource::Package({
                    let source = self.eat(TokenType::String)?.source;
                    self.current_path = self.intern(&source[1..source.len() - 1]);
                    self.current_path
                })
            }
            TokenType::Binary => {
                self.eat(TokenType::Binary)?;
                ImportSource::Native({
                    let source = self.eat(TokenType::String)?.source;
                    self.current_path = self.intern(&source[1..source.len() - 1]);
                    self.current_path
                })
            }
            _ => ImportSource::File({
                let source = self.eat(TokenType::String)?.source;
                self.current_path = self.intern(&source[1..source.len() - 1]);
                source[1..source.len() - 1].split('.').collect()
            }),
        };

        Ok(source)
    }

    fn generics(&mut self) -> Result<Vec<Type>> {
        let mut generics = Vec::new();

        if self.peek()?.ty == TokenType::LeftCaret {
            self.eat(TokenType::LeftCaret)?;

            while self.peek()?.ty != TokenType::RightCaret {
                generics.push(self.parse_type()?);

                if self.peek()?.ty == TokenType::Comma {
                    self.eat(TokenType::Comma)?;
                } else {
                    break;
                }
            }

            if self.peek()?.ty == TokenType::Ident {
                return Err(Diagnostic::new(
                    Severity::Error,
                    "Missing comma",
                    Label::new(
                        self.current_file,
                        self.codespan.source_span(self.current_file),
                        "Missing a comma between generic parameters",
                    ),
                ));
            }

            self.eat(TokenType::RightCaret)?;
        }

        Ok(generics)
    }

    fn loop_loop(&mut self) -> Result<Loop> {
        let start = self.eat(TokenType::Loop)?.range.0;
        let body = self.body()?;
        let end = self.eat(TokenType::EndBlock)?.range.1;

        Ok(Loop {
            body,
            loc: Location::new(self.current_file, (start, end)),
        })
    }

    // Note: Eats the EndBlock and returns the end of its span
    fn then(&mut self) -> Result<(Option<Else>, u32)> {
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

    fn while_loop(&mut self) -> Result<While> {
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

    fn for_loop(&mut self) -> Result<For> {
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
    fn variable_decl(&mut self) -> Result<VarDecl> {
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
    fn optionally_typed_argument(&mut self) -> Result<Vec<(Sym, Type)>> {
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

    fn function_call_arguments(&mut self) -> Result<(Vec<Expr>, u32)> {
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

    fn function_call(&mut self, input: impl Into<Option<(Sym, u32)>>) -> Result<FunctionCall> {
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

    fn assign_type(&mut self) -> Result<AssignType> {
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
            | TokenType::Ampersand => AssignType::BinaryOp(self.binary_operand(next)?.0),
            e => {
                return Err(Diagnostic::new(
                    Severity::Error,
                    "Invalid assignment type",
                    Label::new(
                        self.current_file,
                        next.range.0..next.range.1,
                        format!(
                            "Expected one of '=', '^', '|', '+', '-', '&', '*' or '/', got '{}'",
                            e
                        ),
                    ),
                ));
            }
        };

        Ok(ty)
    }

    fn assign(&mut self, input: impl Into<Option<(Sym, u32)>>) -> Result<Assign> {
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

    fn expr(&mut self) -> Result<(Expr, u32)> {
        trace!("Parsing expr");

        let token = self.peek()?;
        let expr = match token.ty {
            TokenType::LeftParen => {
                self.eat(TokenType::LeftParen)?;
                let (expr, _) = self.expr()?;
                let end = self.eat(TokenType::RightParen)?.range.1;

                (Expr::Expr(Box::new(expr)), end)
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
                return Err(Diagnostic::new(
                    Severity::Error,
                    "Invalid token",
                    Label::new(
                        self.current_file,
                        token.range.0..token.range.1,
                        format!("{:?} is not a valid expression", token.source),
                    ),
                ));
            }
        };

        Ok(expr)
    }

    fn extended_expr(&mut self, expr: Expr, start: u32) -> Result<(Expr, u32)> {
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
                Err(Diagnostic::new(
                    Severity::Error,
                    "Function calls by dotted access are not yet implemented",
                    Label::new(
                        self.current_file,
                        next.range.0..next.range.1,
                        "Function calls by dotted access are not yet implemented".to_string(),
                    ),
                ))
            }
        } else {
            Ok((expr, start))
        }
    }

    fn binary_operand(
        &mut self,
        left: impl Into<Option<Token<'a>>>,
    ) -> Result<(BinaryOp, OperandType)> {
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
                    return Err(Diagnostic::new(
                        Severity::Error,
                        "Invalid operand type",
                        Label::new(
                            self.current_file,
                            begin.range.0..begin.range.1,
                            format!(
                                "Expected one of '^', '|', '+', '-', '&', '*' or '/', got '{}'",
                                e
                            ),
                        ),
                    ));
                }
            };

            // TODO: Add more operand types
            let ty = match self.peek()? {
                _ => OperandType::Normal,
            };

            Ok((op, ty))
        } else {
            let next = self.peek()?;

            warn!("Stand-alone binary operands are unimplemented");
            Err(Diagnostic::new(
                Severity::Error,
                "Stand-alone binary operands are unimplemented",
                Label::new(
                    self.current_file,
                    next.range.0..next.range.1,
                    "Stand-alone binary operands are unimplemented".to_string(),
                ),
            ))
        }
    }

    fn conditional_clause(&mut self) -> Result<(Either<If, Vec<Statement>>, u32)> {
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

    fn comparator(&mut self) -> Result<Comparator> {
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
                return Err(Diagnostic::new(
                    Severity::Error,
                    "Invalid Comparison modifier",
                    Label::new(
                        self.current_file,
                        token.range.0..token.range.1,
                        format!("Expected one of '=', '!', '>', '<', got {}", e),
                    ),
                ));
            }
        })
    }

    fn conditional(&mut self) -> Result<Conditional> {
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

    fn body(&mut self) -> Result<Vec<Statement>> {
        trace!("Parsing body");

        let mut statements = Vec::new();

        loop {
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
                            return Err(Diagnostic::new(
                                Severity::Error,
                                "Unexpected token",
                                Label::new(
                                    self.current_file,
                                    peek.range.0..peek.range.1,
                                    format!("Did not expect a {}", e),
                                ),
                            ));
                        }
                    }
                }
                TokenType::Return => {
                    let start = self.eat(TokenType::Return)?.range.0;
                    let (expr, end) = self.expr()?;
                    Statement::Return(Return {
                        expr,
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
    fn function_declaration(&mut self, visibility: Option<Visibility>) -> Result<FunctionDecl> {
        info!("Parsing Function: {:?}", PanicLoc::caller());

        let start = self.eat(TokenType::Function)?.range.1;

        let name = self.eat(TokenType::Ident)?;
        let name = self.intern(name.source);
        let generics = self.generics()?;
        let arguments = self.function_arguments()?;
        let returns = self.function_return()?;

        self.eat(TokenType::Newline)?;

        let body = self.body()?;

        let end = self.eat(TokenType::EndBlock)?.range.1;

        info!(
            "Finished parsing Function {:?}: {}",
            name,
            self.interner.resolve(name).unwrap()
        );

        Ok(FunctionDecl {
            name,
            generics,
            visibility: visibility.unwrap_or(Visibility::Library),
            arguments,
            returns,
            body,
            abs_path: self.current_path,
            loc: Location::new(self.current_file, (start, end)),
        })
    }

    fn function_arguments(&mut self) -> Result<Vec<(Sym, Type)>> {
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

    fn function_return(&mut self) -> Result<Type> {
        if self.peek()?.ty == TokenType::RightArrow {
            self.eat(TokenType::RightArrow)?;

            Ok(self.parse_type()?)
        } else {
            Ok(Type::default())
        }
    }

    #[inline]
    fn parse_literal(&mut self) -> Result<Literal> {
        info!("Parsing Variable");

        let token = self.next()?;

        let literal = match token.ty {
            // TODO: Allow larger integer sizes
            TokenType::Int => Literal::Integer(token.source.parse().map_err(|_err| {
                Diagnostic::new(
                    Severity::Error,
                    "Invalid Integer",
                    Label::new(
                        self.current_file,
                        token.range.0..token.range.1,
                        "Invalid int".to_string(),
                    ),
                )
            })?),
            TokenType::String => Literal::String({
                let string = &self.escape_string(&(&*token.source)[1..token.source.len() - 1])?;
                self.intern(string)
            }),
            TokenType::Bool => Literal::Boolean(token.source.parse().map_err(|_err| {
                Diagnostic::new(
                    Severity::Error,
                    "Invalid Boolean",
                    Label::new(
                        self.current_file,
                        token.range.0..token.range.1,
                        "Invalid bool".to_string(),
                    ),
                )
            })?),
            e => {
                return Err(Diagnostic::new(
                    Severity::Error,
                    "Invalid Literal",
                    Label::new(
                        self.current_file,
                        token.range.0..token.range.1,
                        format!(
                            "Invalid literal, expected 'Integer', 'String' or 'Boolean', got '{}'",
                            e
                        ),
                    ),
                ));
            }
        };

        info!("Finished parsing Variable");

        Ok(literal)
    }

    #[inline]
    fn parse_typed_argument(&mut self) -> Result<(Sym, Type)> {
        info!("Parsing Named Parameter");

        let name = self.eat(TokenType::Ident)?;
        let name = self.intern(name.source);
        self.eat(TokenType::Colon)?;

        let ty = self.parse_type()?;

        info!("Finished parsing Named Param");

        Ok((name, ty))
    }

    #[inline]
    fn parse_type(&mut self) -> Result<Type> {
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
    fn next(&mut self) -> Result<Token<'a>> {
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
            Err(Diagnostic::new(
                Severity::Error,
                "Unexpected End Of File",
                Label::new(
                    self.current_file,
                    {
                        let end = self.codespan.source_span(self.current_file).end();
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
            Err(Diagnostic::new(
                Severity::Error,
                "Unexpected End Of File",
                Label::new(
                    self.current_file,
                    {
                        let end = self.codespan.source_span(self.current_file).end();
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
            Err(Diagnostic::new(
                Severity::Error,
                format!(
                    "Unexpected Token: Expected '{}', found '{}'",
                    expected, token.ty
                ),
                Label::new(
                    self.current_file,
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
                    self.current_file,
                    self.codespan.source_span(self.current_file),
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
                self.current_file,
                self.codespan.source_span(self.current_file),
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
