mod ast;
#[cfg(test)]
mod tests;
mod token;

pub use ast::*;

use codespan::{FileId, Files};
use codespan_reporting::diagnostic::{Diagnostic, Label, Severity};
use std::{char, collections::VecDeque, panic::Location};
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
    current_file: FileId,
    diagnostics: Vec<Diagnostic>,
    error: bool,
    indent_level: usize,
    pub interner: StringInterner<Sym>,
    current_path: Sym,
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
            indent_level: 0,
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
                            continue;
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
                                continue;
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
                            continue;
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
                                continue;
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

    #[inline]
    fn intern(&mut self, string: &str) -> Sym {
        self.interner.get_or_intern(string)
    }

    #[track_caller]
    fn visibility(&mut self) -> Result<Visibility> {
        trace!("Parsing Visibility: {:?}", Location::caller());

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
        self.eat(TokenType::Type)?;

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

        self.eat(TokenType::EndBlock)?;

        Ok(TypeDecl {
            visibility: type_visibility.unwrap_or_default(),
            name,
            generics,
            members,
            methods,
        })
    }

    // TODO: Clean this up
    fn parse_import(&mut self) -> Result<Import> {
        info!("Parsing Import");

        self.eat(TokenType::Import)?;

        let source = self.import_source()?;

        let (exposes, alias) = match self.peek()?.ty {
            TokenType::Exposing => {
                self.eat(TokenType::Exposing)?;

                let peek = self.peek()?;
                if peek.ty == TokenType::Star {
                    self.eat(TokenType::Star)?;

                    self.eat(TokenType::Newline)?;

                    (Exposes::All, None)
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
                self.eat(TokenType::As)?;

                let alias = self.eat(TokenType::Ident)?;
                let alias = self.intern(alias.source);

                if self.peek()?.ty == TokenType::Exposing {
                    self.eat(TokenType::Exposing)?;

                    let peek = self.peek()?;

                    if peek.ty == TokenType::Star {
                        self.eat(TokenType::Star)?;

                        (Exposes::All, Some(alias))
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

        Ok(Import {
            source,
            alias,
            exposes,
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
                        self.files[0],
                        self.codespan.source_span(self.files[0]),
                        "Missing a comma between generic parameters",
                    ),
                ));
            }

            self.eat(TokenType::RightCaret)?;
        }

        Ok(generics)
    }

    fn loop_loop(&mut self) -> Result<Loop> {
        self.eat(TokenType::Loop)?;
        let body = self.body()?;
        self.eat(TokenType::EndBlock)?;

        Ok(Loop { body })
    }

    fn then(&mut self) -> Result<Option<Else>> {
        let then = if self.peek()?.ty == TokenType::Then {
            self.eat(TokenType::Then)?;
            self.eat(TokenType::Newline)?;

            let body = self.body()?;
            Some(Else { body })
        } else {
            None
        };

        Ok(then)
    }

    fn while_loop(&mut self) -> Result<While> {
        self.eat(TokenType::While)?;
        let condition = self.expr()?;
        self.eat(TokenType::Newline)?;
        let body = self.body()?;
        let then = self.then()?;

        Ok(While {
            condition,
            body,
            then,
        })
    }

    fn for_loop(&mut self) -> Result<For> {
        self.eat(TokenType::For)?;
        let element = self.eat(TokenType::Ident)?;
        let element = self.intern(element.source);
        let range = self.expr()?;
        let body = self.body()?;
        let then = self.then()?;

        Ok(For {
            element,
            range,
            body,
            then,
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
        self.eat(TokenType::Let)?;
        let name = self.eat(TokenType::Ident)?;
        let name = self.intern(name.source);

        let ty = if self.peek()?.ty == TokenType::Colon {
            self.eat(TokenType::Colon)?;
            self.parse_type()?
        } else {
            Type::Infer
        };

        self.eat(TokenType::Equal)?;
        let expr = self.expr()?;

        self.eat(TokenType::Newline)?;

        Ok(VarDecl { name, ty, expr })
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

    fn function_call_arguments(&mut self) -> Result<Vec<Expr>> {
        self.eat(TokenType::LeftParen)?;

        let mut params = Vec::new();
        while self.peek()?.ty != TokenType::RightParen {
            params.push(self.expr()?);

            if self.peek()?.ty == TokenType::Comma {
                self.eat(TokenType::Comma)?;
            } else {
                break;
            }
        }

        self.eat(TokenType::RightParen)?;

        Ok(params)
    }

    fn function_call(&mut self, name: impl Into<Option<Sym>>) -> Result<FunctionCall> {
        let name = if let Some(name) = name.into() {
            name
        } else {
            let ident = self.eat(TokenType::Ident)?;
            self.intern(ident.source)
        };
        let generics = self.generics()?;
        let arguments = self.function_call_arguments()?;

        Ok(FunctionCall {
            name,
            generics,
            arguments,
        })
    }

    fn assign_type(&mut self) -> Result<AssignType> {
        Ok(match self.next()?.ty {
            TokenType::Equal => AssignType::Normal,
            _ => todo!("Implement the rest of the assignment types"),
        })
    }

    fn assign(&mut self, var: impl Into<Option<Sym>>) -> Result<Assign> {
        let var = if let Some(var) = var.into() {
            var
        } else {
            let ident = self.eat(TokenType::Ident)?;
            self.intern(ident.source)
        };

        let ty = self.assign_type()?;
        let expr = self.expr()?;

        Ok(Assign { var, expr, ty })
    }

    fn expr(&mut self) -> Result<Expr> {
        let expr = match self.peek()?.ty {
            TokenType::LeftParen => {
                self.eat(TokenType::LeftParen)?;
                let expr = self.expr()?;
                self.eat(TokenType::RightParen)?;
                Expr::Expr(Box::new(expr))
            }

            TokenType::LeftCaret => todo!("Things that take generics"),

            TokenType::Ident => {
                let ident = self.eat(TokenType::Ident)?;
                let ident = self.intern(ident.source);

                match self.peek()?.ty {
                    TokenType::LeftParen => Expr::FunctionCall(self.function_call(ident)?),
                    _ => Expr::Ident(ident),
                }
            }

            TokenType::String | TokenType::Int | TokenType::Bool => {
                Expr::Literal(self.parse_literal()?)
            }
            _ => todo!("Implement the rest of the expressions"),
        };

        Ok(expr)
    }

    fn conditional_clause(&mut self) -> Result<Either<If, Vec<Statement>>> {
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

        let condition = if cond == CondType::Else {
            None
        } else {
            let left = Box::new(self.expr()?);
            let comparison = self.comparator()?;
            let right = Box::new(self.expr()?);

            Some(Expr::Comparison(Comparison {
                left,
                comparison,
                right,
            }))
        };
        self.eat(TokenType::Newline)?;

        let body = self.body()?;

        trace!("Finished parsing conditional");

        Ok(match cond {
            CondType::If | CondType::ElseIf => Either::Left(If {
                condition: condition.expect("`if` and `else if` clauses have conditions"),
                body,
            }),
            CondType::Else => Either::Right(body),
        })
    }

    fn comparator(&mut self) -> Result<Comparator> {
        Ok(match self.peek()?.ty {
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

            _ => todo!("Implement further equality tokens and handle invalid ones"),
        })
    }

    fn conditional(&mut self) -> Result<Conditional> {
        let (mut peek, mut if_clauses, mut else_body) = (self.peek()?.ty, Vec::new(), None);
        while peek == TokenType::If || peek == TokenType::Else {
            match self.conditional_clause()? {
                Either::Left(condition) => if_clauses.push(condition),
                Either::Right(body) => else_body = Some(Else { body }),
            }

            peek = self.peek()?.ty;
        }
        self.eat(TokenType::EndBlock)?;

        Ok(Conditional {
            _if: if_clauses,
            _else: else_body,
        })
    }

    fn body(&mut self) -> Result<Vec<Statement>> {
        trace!("Parsing body");

        let mut statements = Vec::new();

        loop {
            let statement = match self.peek()?.ty {
                TokenType::If => Statement::Conditional(self.conditional()?),
                TokenType::While => Statement::While(self.while_loop()?),
                TokenType::Loop => Statement::Loop(self.loop_loop()?),
                TokenType::For => Statement::For(self.for_loop()?),
                TokenType::Let => Statement::VarDecl(self.variable_decl()?),
                TokenType::Ident => {
                    let ident = self.eat(TokenType::Ident)?;
                    let ident = self.intern(ident.source);

                    match self.peek()?.ty {
                        TokenType::LeftParen => {
                            Statement::Expr(Expr::FunctionCall(self.function_call(ident)?))
                        }
                        TokenType::Equal => Statement::Assign(self.assign(ident)?),
                        _ => todo!("Write the error"),
                    }
                }
                TokenType::Return => {
                    self.eat(TokenType::Return)?;
                    Statement::Return(Return { expr: self.expr()? })
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
        info!("Parsing Function: {:?}", Location::caller());

        self.eat(TokenType::Function)?;

        let name = self.eat(TokenType::Ident)?;
        let name = self.intern(name.source);
        let generics = self.generics()?;
        let arguments = self.function_arguments()?;
        let returns = self.function_return()?;

        self.eat(TokenType::Newline)?;

        let body = self.body()?;

        self.eat(TokenType::EndBlock)?;

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
            TokenType::Int => Literal::Integer(token.source.parse().unwrap()),
            TokenType::String => Literal::String({
                let string = &self.escape_string(&(&*token.source)[1..token.source.len() - 1])?;
                self.intern(string)
            }),
            TokenType::Bool => Literal::Boolean(token.source.parse().unwrap()),
            _ => unimplemented!("{:?}", token.ty),
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
