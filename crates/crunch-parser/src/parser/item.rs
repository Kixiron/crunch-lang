use crate::{
    parser::{CurrentFile, Parser},
    token::{Token, TokenType},
};
use alloc::{borrow::ToOwned, format, string::ToString, vec::Vec};
use core::{mem, str::FromStr};
use crunch_shared::{
    crunch_proc::recursion_guard,
    error::{Error, Locatable, Location, ParseResult, Span, SyntaxError},
    trees::{
        ast::{
            Attribute, Decorator, Dest, Exposure, ExtendBlock, ExternBlock, ExternFunc, FuncArg,
            Item, ItemKind, Type, TypeMember, Variant, Vis,
        },
        CallConv,
    },
};

// TODO: Const blocks

impl<'src, 'ctx> Parser<'src, 'ctx> {
    #[recursion_guard]
    pub(super) fn item(&mut self) -> ParseResult<Option<&'ctx Item<'ctx>>> {
        let (mut decorators, mut attributes, mut vis) =
            (Vec::with_capacity(5), Vec::with_capacity(5), None);

        while self.peek().is_ok() {
            if let Some(node) = self.item_impl(&mut decorators, &mut attributes, &mut vis)? {
                return Ok(Some(node));
            }
        }

        Ok(None)
    }

    // Returns None when the function should be re-called, usually because an attribute or decorator was parsed
    #[recursion_guard]
    fn item_impl(
        &mut self,
        decorators: &mut Vec<Decorator<'ctx>>,
        attributes: &mut Vec<Attribute>,
        vis: &mut Option<Vis>,
    ) -> ParseResult<Option<&'ctx Item<'ctx>>> {
        let peek = self.peek()?;

        match peek.ty() {
            TokenType::AtSign => {
                self.decorator(decorators)?;

                Ok(None)
            }

            TokenType::Exposed | TokenType::Package => {
                // TODO: Check if `vis` is already assigned to
                *vis = Some(self.vis()?);

                Ok(None)
            }

            TokenType::Const => {
                let token = self.next()?;
                let attr = self.attr(&token, self.current_file)?;

                attributes.push(attr);

                Ok(None)
            }

            TokenType::Function => {
                let func = self.function(
                    mem::take(decorators),
                    mem::take(attributes),
                    vis.take().unwrap_or_default(),
                )?;

                Ok(Some(func))
            }

            TokenType::Type => {
                let ty = self.type_decl(
                    mem::take(decorators),
                    mem::take(attributes),
                    vis.take().unwrap_or_default(),
                )?;

                Ok(Some(ty))
            }

            TokenType::Extend => {
                let extension = self.extend_block(mem::take(decorators), mem::take(attributes))?;

                Ok(Some(extension))
            }

            TokenType::Enum => {
                let enu = self.enum_decl(
                    mem::take(decorators),
                    mem::take(attributes),
                    vis.take().unwrap_or_default(),
                )?;

                Ok(Some(enu))
            }

            TokenType::Trait => {
                let tra = self.trait_decl(
                    mem::take(decorators),
                    mem::take(attributes),
                    vis.take().unwrap_or_default(),
                )?;

                Ok(Some(tra))
            }

            TokenType::Import => {
                if !attributes.is_empty() {
                    Err(Locatable::new(
                        Error::Syntax(SyntaxError::NoAttributesAllowed("import".to_string())),
                        Location::new(&self.peek()?, self.current_file),
                    ))
                } else {
                    let import =
                        self.import(mem::take(decorators), vis.take().unwrap_or_default())?;

                    Ok(Some(import))
                }
            }

            TokenType::Alias => {
                let alias = self.alias(
                    mem::take(decorators),
                    mem::take(attributes),
                    vis.take().unwrap_or_default(),
                )?;
                Ok(Some(alias))
            }

            TokenType::Extern => {
                let extern_block =
                    self.extern_block(mem::take(decorators), mem::take(attributes))?;

                // Throw the visibility error after we parse the block so that we can do more useful work
                if vis.take().is_some() {
                    // TODO: Give visibility a span
                    Err(Locatable::new(
                        SyntaxError::NoVisibilityAllowed("External blocks".to_owned()).into(),
                        Location::new(extern_block.span(), self.current_file),
                    ))
                } else {
                    Ok(Some(extern_block))
                }
            }

            TokenType::Newline | TokenType::Space => {
                self.next()?;
                Ok(None)
            }

            ty => Err(Locatable::new(
                Error::Syntax(SyntaxError::InvalidTopLevel(format!("{}", ty))),
                Location::new(&self.peek()?, self.current_file),
            )),
        }
    }

    #[recursion_guard]
    fn import(
        &mut self,
        decorators: Vec<Decorator<'ctx>>,
        vis: Vis,
    ) -> ParseResult<&'ctx Item<'ctx>> {
        let start_span = self.eat(TokenType::Import, [TokenType::Newline])?.span();

        let file = self.eat(TokenType::Ident, [TokenType::Newline])?.source();
        let file = self.context.strings.intern(file);
        let file = self.item_path(file)?;

        let dest = if self.peek()?.ty() == TokenType::Library {
            self.eat(TokenType::Library, [TokenType::Newline])?;

            Dest::NativeLib
        } else if self.peek()?.ty() == TokenType::Package {
            self.eat(TokenType::Package, [TokenType::Newline])?;

            Dest::Package
        } else {
            Dest::Relative
        };

        let exposes = if self.peek()?.ty() == TokenType::Exposing {
            self.eat(TokenType::Exposing, [TokenType::Newline])?;

            if self.peek()?.ty() == TokenType::Star {
                self.eat(TokenType::Star, [TokenType::Newline])?;

                Exposure::All
            } else {
                let mut items = Vec::with_capacity(5);
                while self.peek()?.ty() != TokenType::Newline {
                    let (_span, member) = {
                        let ident = self.eat(TokenType::Ident, [TokenType::Newline])?;
                        (ident.span(), self.context.strings.intern(ident.source()))
                    };
                    let member = self.item_path(member)?;

                    let alias = if self.peek()?.ty() == TokenType::As {
                        self.eat(TokenType::As, [TokenType::Newline])?;
                        let alias = {
                            let ident = self.eat(TokenType::Ident, [TokenType::Newline])?;
                            self.context.strings.intern(ident.source())
                        };

                        alias
                    } else {
                        *member.last().expect("There should be at least one segment")
                    };

                    items.push((member, alias));

                    // TODO: Helpful error if they terminated it too soon
                    if self.peek()?.ty() == TokenType::Comma {
                        self.eat(TokenType::Comma, [TokenType::Newline])?;
                    } else {
                        break;
                    }
                }

                Exposure::Items(items)
            }
        } else {
            let alias = if self.peek()?.ty() == TokenType::As {
                self.eat(TokenType::As, [TokenType::Newline])?;
                let ident = self.eat(TokenType::Ident, [TokenType::Newline])?;

                self.context.strings.intern(ident.source())
            } else {
                // Get the last segment of the path as the alias if none is supplied
                file.iter().last().copied().ok_or(Locatable::new(
                    Error::Syntax(SyntaxError::MissingImport),
                    Location::new(&self.peek()?, self.current_file),
                ))?
            };

            Exposure::None(alias)
        };

        let end_span = self.eat(TokenType::Newline, [])?.span();

        // Import statements cannot have decorators, so throw an error if there are any
        if decorators.is_empty() {
            Ok(self.context.ast_item(Item {
                decorators,
                attrs: Vec::new(),
                kind: ItemKind::Import {
                    file,
                    dest,
                    exposes,
                },
                name: None,
                loc: Location::new(Span::merge(start_span, end_span), self.current_file),
                vis: Some(vis),
            }))
        } else {
            let first = decorators
                .iter()
                .next()
                .expect("There is at least one decorator")
                .loc
                .span();

            Err(Locatable::new(
                Error::Syntax(SyntaxError::NoDecoratorsAllowed("import".to_string())),
                Location::new(
                    Span::merge(
                        first,
                        decorators
                            .iter()
                            .last()
                            .map(|dec| dec.loc.span())
                            .unwrap_or(first),
                    ),
                    self.current_file,
                ),
            ))
        }
    }

    #[recursion_guard]
    fn trait_decl(
        &mut self,
        decorators: Vec<Decorator<'ctx>>,
        attrs: Vec<Attribute>,
        vis: Vis,
    ) -> ParseResult<&'ctx Item<'ctx>> {
        let start_span = self.eat(TokenType::Trait, [TokenType::Newline])?.span();
        let name = {
            let ident = self.eat(TokenType::Ident, [TokenType::Newline])?;
            self.context.strings.intern(ident.source())
        };
        let generics = self.generics()?;
        let sig_span_end = self.eat(TokenType::Newline, [])?.span();
        let _signature_span = Span::merge(start_span, sig_span_end);

        let (mut method_decorators, mut method_attributes) =
            (Vec::with_capacity(3), Vec::with_capacity(3));
        let mut method_vis = None;

        let mut methods = Vec::with_capacity(4);
        while self.peek()?.ty() != TokenType::End {
            match self.peek()?.ty() {
                TokenType::AtSign => {
                    self.decorator(&mut method_decorators)?;
                }

                TokenType::Exposed | TokenType::Package => method_vis = Some(self.vis()?),

                TokenType::Function => {
                    let method = self.function(
                        mem::take(&mut method_decorators),
                        mem::take(&mut method_attributes),
                        method_vis.unwrap_or_default(),
                    )?;

                    methods.push(method);
                }

                TokenType::Newline => {
                    self.eat(TokenType::Newline, [])?;
                }

                _ => {
                    return Err(Locatable::new(
                        Error::Syntax(SyntaxError::Generic("Only methods, attributes and decorators are allowed inside trait bodies".to_string())),
                        Location::new(&self.peek()?, self.current_file),
                    ));
                }
            }
        }
        let end_span = self.eat(TokenType::End, [TokenType::Newline])?.span();

        let kind = ItemKind::Trait { generics, methods };

        Ok(self.context.ast_item(Item {
            kind,
            decorators,
            attrs,
            name: Some(name),
            loc: Location::new(Span::merge(start_span, end_span), self.current_file),
            vis: Some(vis),
        }))
    }

    #[recursion_guard]
    fn enum_decl(
        &mut self,
        decorators: Vec<Decorator<'ctx>>,
        attrs: Vec<Attribute>,
        vis: Vis,
    ) -> ParseResult<&'ctx Item<'ctx>> {
        let start_span = self.eat(TokenType::Enum, [TokenType::Newline])?.span();
        let name = {
            let ident = self.eat(TokenType::Ident, [TokenType::Newline])?;
            self.context.strings.intern(ident.source())
        };
        let generics = self.generics()?;
        let sig_span_end = self.eat(TokenType::Newline, [])?.span();
        let _signature_span = Span::merge(start_span, sig_span_end);

        let mut variant_decorators = Vec::with_capacity(7);
        let mut variants = Vec::with_capacity(7);
        while self.peek()?.ty() != TokenType::End {
            match self.peek()?.ty() {
                TokenType::AtSign => {
                    self.decorator(&mut variant_decorators)?;
                }

                TokenType::Ident => {
                    let (name, _start_span) = {
                        let ident = self.eat(TokenType::Ident, [TokenType::Newline])?;
                        (self.context.strings.intern(ident.source()), ident.span())
                    };

                    let variant = if self.peek()?.ty() == TokenType::LeftParen {
                        self.eat(TokenType::LeftParen, [TokenType::Newline])?;

                        let mut elms = Vec::with_capacity(3);
                        while self.peek()?.ty() != TokenType::RightParen {
                            let ty = self.ascribed_type()?;
                            elms.push(ty);

                            // TODO: Nice error here
                            if self.peek()?.ty() == TokenType::Comma {
                                self.eat(TokenType::Comma, [TokenType::Newline])?;
                            } else {
                                break;
                            }
                        }
                        self.eat(TokenType::RightParen, [TokenType::Newline])?;

                        Variant::Tuple {
                            name,
                            elms,
                            decorators: mem::take(&mut variant_decorators),
                        }
                    } else {
                        Variant::Unit {
                            name,
                            decorators: mem::take(&mut variant_decorators),
                        }
                    };

                    let _end_span = self.eat(TokenType::Newline, [])?.span();

                    variants.push(variant);
                }

                TokenType::Newline => {
                    self.eat(TokenType::Newline, [])?;
                }

                ty => {
                    return Err(Locatable::new(
                        Error::Syntax(SyntaxError::Generic(format!(
                            "Only decorators and enum variants are allowed inside enum declarations, got a `{}`", 
                            ty,
                        ))),
                        Location::new(&self.peek()?, self.current_file),
                    ));
                }
            }
        }
        let end_span = self.eat(TokenType::End, [TokenType::Newline])?.span();

        let kind = ItemKind::Enum { generics, variants };

        Ok(self.context.ast_item(Item {
            kind,
            decorators,
            attrs,
            name: Some(name),
            loc: Location::new(Span::merge(start_span, end_span), self.current_file),
            vis: Some(vis),
        }))
    }

    #[recursion_guard]
    fn decorator(&mut self, decorators: &mut Vec<Decorator<'ctx>>) -> ParseResult<()> {
        let start = self.eat(TokenType::AtSign, [TokenType::Newline])?.span();
        let (name, name_span) = {
            let ident = self.eat(TokenType::Ident, [TokenType::Newline])?;

            (
                Locatable::new(
                    self.context.strings.intern(ident.source()),
                    Location::new(ident.span(), self.current_file),
                ),
                ident.span(),
            )
        };

        let (args, end_span) = if self.peek()?.ty() == TokenType::LeftParen {
            self.eat(TokenType::LeftParen, [TokenType::Newline])?;

            let mut args = Vec::with_capacity(5);
            while self.peek()?.ty() != TokenType::RightParen {
                let expr = self.expr()?;
                args.push(expr);

                if let Ok(peek) = self.peek() {
                    if peek.ty() == TokenType::Comma {
                        self.eat(TokenType::Comma, [TokenType::Newline])?;
                        continue;
                    }
                }

                break;
            }
            let end = self
                .eat(TokenType::RightParen, [TokenType::Newline])?
                .span();

            (args, Some(end))
        } else {
            (Vec::new(), None)
        };

        decorators.push(Decorator {
            name,
            args,
            loc: Location::new(
                Span::merge(start, end_span.unwrap_or(name_span)),
                self.current_file,
            ),
        });

        Ok(())
    }

    /// ```ebnf
    /// TypeDecl ::=
    ///     Decorator* Attribute* 'type' Ident Generics? '\n'
    ///         (Decorator* Attribute* Ident (':' Type)? '\n')+ | 'empty'
    ///     'end'
    /// ```
    #[recursion_guard]
    fn type_decl(
        &mut self,
        decorators: Vec<Decorator<'ctx>>,
        attrs: Vec<Attribute>,
        vis: Vis,
    ) -> ParseResult<&'ctx Item<'ctx>> {
        let start_span = self.eat(TokenType::Type, [TokenType::Newline])?.span();
        let name = {
            let ident = self.eat(TokenType::Ident, [TokenType::Newline])?;
            self.context.strings.intern(ident.source())
        };
        let generics = self.generics()?;
        let sig_span_end = self.eat(TokenType::Newline, [])?.span();

        let _signature_span = Span::merge(start_span, sig_span_end);

        let (mut member_decorators, mut member_attrs) =
            (Vec::with_capacity(3), Vec::with_capacity(3));

        let mut members = Vec::with_capacity(5);

        while self.peek()?.ty() != TokenType::End {
            match self.peek()?.ty() {
                TokenType::AtSign => {
                    self.decorator(&mut member_decorators)?;
                }

                TokenType::Exposed | TokenType::Package => {
                    let token = self.next()?;
                    let attr = self.attr(&token, self.current_file)?;

                    member_attrs.push(attr);
                }

                TokenType::Ident => {
                    let (name, name_span) = {
                        let ident = self.eat(TokenType::Ident, [TokenType::Newline])?;
                        (self.context.strings.intern(ident.source()), ident.span())
                    };

                    let ty = if self.peek()?.ty() == TokenType::Colon {
                        self.eat(TokenType::Colon, [TokenType::Newline])?;
                        self.ascribed_type()?
                    } else {
                        Locatable::new(
                            self.context.ast_type(Type::default()),
                            Location::new(name_span, self.current_file),
                        )
                    };

                    let member = TypeMember {
                        decorators: mem::take(&mut member_decorators),
                        attrs: mem::take(&mut member_attrs),
                        name,
                        ty,
                    };
                    let _end_span = self.eat(TokenType::Comma, [])?.span();

                    members.push(member);
                }

                TokenType::Newline => {
                    self.eat(TokenType::Newline, [])?;
                }

                ty => {
                    return Err(Locatable::new(
                        Error::Syntax(SyntaxError::InvalidTopLevel(format!("{}", ty))),
                        Location::new(&self.peek()?, self.current_file),
                    ));
                }
            }
        }
        let end_span = self.eat(TokenType::End, [TokenType::Newline])?.span();

        if !member_attrs.is_empty() || !member_decorators.is_empty() {
            return Err(Locatable::new(
                Error::Syntax(SyntaxError::Generic("Attributes and functions must be before members or methods in type declarations".to_string())),
                Location::new(&self.peek()?, self.current_file),
            ));
        }

        let kind = ItemKind::Type { generics, members };

        Ok(self.context.ast_item(Item {
            kind,
            decorators,
            attrs,
            name: Some(name),
            loc: Location::new(Span::merge(start_span, end_span), self.current_file),
            vis: Some(vis),
        }))
    }

    /// ```ebnf
    /// ExtendBlock ::=
    ///     Decorator* Attribute* 'extend' Type ('with' Type)? '\n'
    ///         TopNode+ | 'empty'
    ///     'end'
    /// ```
    #[recursion_guard]
    fn extend_block(
        &mut self,
        _decorators: Vec<Decorator<'ctx>>,
        mut _attrs: Vec<Attribute>,
    ) -> ParseResult<&'ctx Item<'ctx>> {
        let start = self.eat(TokenType::Extend, [TokenType::Newline])?.span();
        let target = self.ascribed_type()?;

        let extender = if self.peek()?.ty() == TokenType::With {
            self.eat(TokenType::With, [TokenType::Newline])?;
            Some(self.ascribed_type()?)
        } else {
            None
        };

        self.eat(TokenType::Newline, [])?;

        let mut items = Vec::with_capacity(5);
        let (mut decorators, mut attrs, mut vis) =
            (Vec::with_capacity(5), Vec::with_capacity(5), None);

        while self.peek()?.ty() != TokenType::End {
            if let Some(item) = self.item_impl(&mut decorators, &mut attrs, &mut vis)? {
                items.push(item);
            }
        }

        if !decorators.is_empty() {
            todo!("error")
        }
        if !attrs.is_empty() {
            todo!("error")
        }

        let end = self.eat(TokenType::End, [])?.span();

        let kind = ItemKind::ExtendBlock(ExtendBlock {
            target,
            extender,
            items,
        });

        Ok(self.context.ast_item(Item {
            kind,
            attrs,
            decorators,
            name: None,
            loc: Location::new(Span::merge(start, end), self.current_file),
            vis: None,
        }))
    }

    /// ```ebnf
    /// Decorator* Attribute* 'alias' Type = Type '\n'
    /// ```
    #[recursion_guard]
    fn alias(
        &mut self,
        decorators: Vec<Decorator<'ctx>>,
        attrs: Vec<Attribute>,
        vis: Vis,
    ) -> ParseResult<&'ctx Item<'ctx>> {
        let start = self.eat(TokenType::Alias, [TokenType::Newline])?.span();
        let alias = self.ascribed_type()?;
        self.eat(TokenType::Equal, [TokenType::Newline])?;

        let actual = self.ascribed_type()?;
        let end = self.eat(TokenType::Newline, [])?.span();

        let kind = ItemKind::Alias { alias, actual };

        Ok(self.context.ast_item(Item {
            kind,
            attrs,
            decorators,
            name: None,
            loc: Location::new(Span::merge(start, end), self.current_file),
            vis: Some(vis),
        }))
    }

    /// ```ebnf
    /// Function ::=
    ///     Vis? Decorator* Attribute* 'fn' Ident '(' FunctionArgs* ')' ('->' Type)? '\n'
    ///         Statement* | 'empty'
    ///     'end'
    /// ```
    #[recursion_guard]
    fn function(
        &mut self,
        decorators: Vec<Decorator<'ctx>>,
        attrs: Vec<Attribute>,
        vis: Vis,
    ) -> ParseResult<&'ctx Item<'ctx>> {
        let start_span = self.eat(TokenType::Function, [TokenType::Newline])?.span();
        let name = {
            let ident = self.eat(TokenType::Ident, [TokenType::Newline])?;
            self.context.strings.intern(ident.source())
        };
        let generics = self.generics()?;
        let args = self.function_args()?;

        let (returns, ret_span) = if self.peek()?.ty() == TokenType::RightArrow {
            let start = self.eat(TokenType::RightArrow, [])?.span();
            let ty = self.ascribed_type()?;
            // FIXME: Make types have spans
            let janky_span = Span::merge(start, start);

            (Some(ty), Some(janky_span))
        } else {
            (None, None)
        };
        self.eat(TokenType::Newline, [])?;
        let sig_span = Span::merge(
            start_span,
            ret_span.unwrap_or_else(|| args.location().span()),
        );

        let ret = returns.unwrap_or_else(|| {
            Locatable::new(
                self.context.ast_type(Type::default()),
                Location::new(ret_span.unwrap_or(sig_span), self.current_file),
            )
        });

        while self.peek()?.ty() == TokenType::Newline {
            self.eat(TokenType::Newline, [])?;
        }

        let body = self.block(&[TokenType::End], 20)?;
        let end_span = body.location().span();
        let sig = Location::new(sig_span, self.current_file);

        let kind = ItemKind::Func {
            generics,
            args,
            body,
            ret,
            sig,
        };

        Ok(self.context.ast_item(Item {
            kind,
            decorators,
            attrs,
            name: Some(name),
            loc: Location::new(Span::merge(start_span, end_span), self.current_file),
            vis: Some(vis),
        }))
    }

    /// ```ebnf
    /// FunctionArgs ::= '(' Args? ')'
    /// Args ::= Argument | Argument ',' Args
    /// Argument ::= Ident ':' Type
    /// ```
    #[recursion_guard]
    fn function_args(&mut self) -> ParseResult<Locatable<Vec<FuncArg<'ctx>>>> {
        let start = self.eat(TokenType::LeftParen, [TokenType::Newline])?.span();

        let mut args = Vec::with_capacity(7);
        while self.peek()?.ty() != TokenType::RightParen {
            let (name, name_span) =
                match self.eat_of([TokenType::Ident, TokenType::Const], [TokenType::Newline])? {
                    ident if ident.ty() == TokenType::Ident => {
                        (self.context.strings.intern(ident.source()), ident.span())
                    }

                    token if token.ty() == TokenType::Const => {
                        let ident = self.eat(TokenType::Ident, [TokenType::Newline])?;

                        (self.context.strings.intern(ident.source()), token.span())
                    }

                    _ => unreachable!(),
                };

            self.eat(TokenType::Colon, [TokenType::Newline])?;
            let ty = self.ascribed_type()?;

            // FIXME: Type span
            let loc = Location::new(name_span, self.current_file);
            let arg = FuncArg { name, ty, loc };

            args.push(arg);

            if self.peek()?.ty() == TokenType::Comma {
                self.eat(TokenType::Comma, [TokenType::Newline])?;
            } else {
                break;
            }
        }
        let end = self
            .eat(TokenType::RightParen, [TokenType::Newline])?
            .span();

        Ok(Locatable::new(
            args,
            Location::new(Span::merge(start, end), self.current_file),
        ))
    }

    /// ```ebnf
    /// ExternBlock ::=
    ///     Decorator* Attribute* 'extern'
    ///         ExternFunc*
    ///     'end'
    /// ```
    #[recursion_guard]
    fn extern_block(
        &mut self,
        decorators: Vec<Decorator<'ctx>>,
        attrs: Vec<Attribute>,
    ) -> ParseResult<&'ctx Item<'ctx>> {
        let start = self.eat(TokenType::Extern, [TokenType::Newline])?.span();
        let mut items = Vec::with_capacity(5);

        let (mut item_decorators, mut item_attributes, mut item_vis) =
            (Vec::new(), Vec::new(), None);
        while self.peek()?.ty() != TokenType::End {
            match self.peek()?.ty() {
                TokenType::AtSign => {
                    self.decorator(&mut item_decorators)?;
                }
                TokenType::Exposed | TokenType::Package => item_vis = Some(self.vis()?),

                TokenType::Function => {
                    let func = self.extern_func(
                        mem::take(&mut item_decorators),
                        mem::take(&mut item_attributes),
                        item_vis.unwrap_or_default(),
                    )?;

                    items.push(func);
                }

                TokenType::Newline => {
                    self.eat(TokenType::Newline, [])?;
                }

                _ => {
                    return Err(Locatable::new(
                        Error::Syntax(SyntaxError::Generic(
                            "Only external functions are allowed in extern blocks".to_string(),
                        )),
                        Location::new(&self.peek()?, self.current_file),
                    ));
                }
            }
        }
        let end = self.eat(TokenType::End, [TokenType::Newline])?.span();

        Ok(self.context.ast_item(Item {
            name: None,
            vis: None,
            attrs,
            decorators,
            kind: ItemKind::ExternBlock(ExternBlock { items }),
            loc: Location::new(Span::merge(start, end), self.current_file),
        }))
    }

    /// ```ebnf
    /// ExternFunc ::=
    ///     Vis? Decorator* Attribute* 'fn' Ident '(' FunctionArgs* ')' ('->' Type)? ';'
    /// ```
    #[recursion_guard]
    fn extern_func(
        &mut self,
        mut decorators: Vec<Decorator<'ctx>>,
        attrs: Vec<Attribute>,
        vis: Vis,
    ) -> ParseResult<&'ctx Item<'ctx>> {
        let start = self.eat(TokenType::Function, [TokenType::Newline])?.span();
        let name = {
            let ident = self.eat(TokenType::Ident, [TokenType::Newline])?;
            self.context.strings.intern(ident.source())
        };
        let generics = self.generics()?;
        let args = self.function_args()?;

        let returns = if self.peek()?.ty() == TokenType::RightArrow {
            self.eat(TokenType::RightArrow, [])?;
            Some(self.ascribed_type()?)
        } else {
            None
        };
        let end = self.eat(TokenType::Semicolon, [])?.span();

        let ret = returns.unwrap_or_else(|| {
            Locatable::new(
                self.context.ast_type(Type::default()),
                Location::new(Span::merge(start, end), self.current_file),
            )
        });
        let callconv = self.callconv(false, &mut decorators)?;

        Ok(self.context.ast_item(Item {
            name: Some(name),
            vis: Some(vis),
            attrs,
            decorators,
            kind: ItemKind::ExternFunc(ExternFunc {
                args,
                generics,
                ret,
                callconv,
            }),
            loc: Location::new(Span::merge(start, end), self.current_file),
        }))
    }

    fn callconv(
        &mut self,
        optional: bool,
        decorators: &mut Vec<Decorator<'ctx>>,
    ) -> ParseResult<CallConv> {
        let callconv = self.context.strings.intern("callconv");

        if let Some(idx) = decorators.iter().position(|dec| *dec.name == callconv) {
            let decorator = decorators.remove(idx);
            let expected = |loc| {
                Locatable::new(
                    SyntaxError::Generic(
                        "Expected a str literal as a calling convention".to_owned(),
                    )
                    .into(),
                    loc,
                )
            };

            let literal = decorator
                .args
                .iter()
                .find_map(|arg| arg.as_literal())
                .ok_or_else(|| expected(decorator.location()))?;
            let callconv = literal
                .val
                .as_string()
                .ok_or_else(|| expected(literal.location()))?;

            CallConv::from_str(&callconv.to_string())
                .map_err(|err| Locatable::new(err.into(), literal.location()))
        } else {
            if optional {
                Ok(CallConv::Crunch)
            } else {
                todo!("Error handling")
            }
        }
    }

    /// ```ebnf
    /// Generics ::= '[' GenericArgs? ']'
    /// GenericArgs ::= Type | Type ',' GenericArgs
    /// ```
    #[recursion_guard]
    pub(super) fn generics(
        &mut self,
    ) -> ParseResult<Option<Locatable<Vec<Locatable<&'ctx Type<'ctx>>>>>> {
        let peek = if let Ok(peek) = self.peek() {
            peek
        } else {
            return Ok(None);
        };

        if peek.ty() == TokenType::LeftBrace {
            let start = self.eat(TokenType::LeftBrace, [TokenType::Newline])?.span();

            let mut generics = Vec::with_capacity(5);
            while self.peek()?.ty() != TokenType::RightBrace {
                generics.push(self.ascribed_type()?);

                if self.peek()?.ty() == TokenType::Comma {
                    self.eat(TokenType::Comma, [TokenType::Newline])?;
                } else {
                    // TODO: Check if next is a `>` and if so emit a helpful error
                    break;
                }
            }

            let end = self
                .eat(TokenType::RightBrace, [TokenType::Newline])?
                .span();

            Ok(Some(Locatable::new(
                generics,
                Location::new(Span::merge(start, end), self.current_file),
            )))
        } else {
            Ok(None)
        }
    }

    /// ```ebnf
    /// Attribute ::= 'const'
    /// ```
    #[recursion_guard]
    fn attr(&self, token: &Token<'_>, file: CurrentFile) -> ParseResult<Attribute> {
        Ok(match token.ty() {
            TokenType::Const => Attribute::Const,

            _ => {
                return Err(Locatable::new(
                    Error::Syntax(SyntaxError::Generic(format!(
                        "Expected an attribute, got `{}`",
                        token.ty()
                    ))),
                    Location::new(token, file),
                ));
            }
        })
    }

    /// ```ebnf
    /// Vis ::= 'exposed' | 'pkg'
    /// ```
    #[recursion_guard]
    fn vis(&mut self) -> ParseResult<Vis> {
        Ok(
            match self
                .eat_of([TokenType::Exposed, TokenType::Package], [])?
                .ty()
            {
                TokenType::Exposed => Vis::Exposed,
                TokenType::Package => Vis::Package,

                _ => todo!("Error"),
            },
        )
    }
}
