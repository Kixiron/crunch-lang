use crate::{
    error::{Error, Locatable, Location, ParseResult, Span, SyntaxError},
    parser::{CurrentFile, Expr, Literal, Parser, Stmt, Type},
    token::{Token, TokenType},
};

use crunch_proc::recursion_guard;
use lasso::SmallSpur;

use alloc::{format, string::ToString, vec::Vec};
use core::{convert::TryFrom, mem};

// TODO: Const blocks
// TODO: Add back generics to funcs

#[derive(Debug, Clone, PartialEq)]
pub struct Function<'expr, 'stmt> {
    pub decorators: Vec<Locatable<Decorator<'expr>>>,
    pub attrs: Vec<Locatable<Attribute>>,
    pub name: SmallSpur,
    pub args: Vec<Locatable<FuncArg>>,
    pub returns: Locatable<Type>,
    pub body: Vec<Stmt<'expr, 'stmt>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncArg {
    pub name: Locatable<SmallSpur>,
    pub ty: Locatable<Type>,
    pub comptime: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeDecl<'expr> {
    pub decorators: Vec<Locatable<Decorator<'expr>>>,
    pub attrs: Vec<Locatable<Attribute>>,
    pub name: SmallSpur,
    pub generics: Vec<Locatable<Type>>,
    pub members: Vec<Locatable<TypeMember<'expr>>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeMember<'expr> {
    pub decorators: Vec<Locatable<Decorator<'expr>>>,
    pub attrs: Vec<Locatable<Attribute>>,
    pub name: SmallSpur,
    pub ty: Locatable<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Enum<'expr> {
    pub decorators: Vec<Locatable<Decorator<'expr>>>,
    pub attrs: Vec<Locatable<Attribute>>,
    pub name: SmallSpur,
    pub generics: Vec<Locatable<Type>>,
    pub variants: Vec<Locatable<EnumVariant<'expr>>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum EnumVariant<'expr> {
    Unit {
        name: SmallSpur,
        decorators: Vec<Locatable<Decorator<'expr>>>,
    },

    Tuple {
        name: SmallSpur,
        elements: Vec<Locatable<Type>>,
        decorators: Vec<Locatable<Decorator<'expr>>>,
    },
}

impl<'expr> EnumVariant<'expr> {
    pub fn name(&self) -> SmallSpur {
        match self {
            Self::Unit { name, .. } => *name,
            Self::Tuple { name, .. } => *name,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Trait<'expr, 'stmt> {
    pub decorators: Vec<Locatable<Decorator<'expr>>>,
    pub attrs: Vec<Locatable<Attribute>>,
    pub name: SmallSpur,
    pub generics: Vec<Locatable<Type>>,
    pub methods: Vec<Locatable<Function<'expr, 'stmt>>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Import {
    pub file: Locatable<SmallSpur>,
    pub dest: ImportDest,
    pub exposes: ImportExposure,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExtendBlock<'expr, 'stmt> {
    target: Locatable<Type>,
    extender: Option<Locatable<Type>>,
    nodes: Vec<Ast<'expr, 'stmt>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Alias<'expr> {
    pub decorators: Vec<Locatable<Decorator<'expr>>>,
    pub attrs: Vec<Locatable<Attribute>>,
    pub alias: Locatable<Type>,
    pub actual: Locatable<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Ast<'expr, 'stmt> {
    Function(Locatable<Function<'expr, 'stmt>>),
    Type(Locatable<TypeDecl<'expr>>),
    Enum(Locatable<Enum<'expr>>),
    Trait(Locatable<Trait<'expr, 'stmt>>),
    Import(Locatable<Import>),
    ExtendBlock(Locatable<ExtendBlock<'expr, 'stmt>>),
    Alias(Locatable<Alias<'expr>>),
}

impl<'expr, 'stmt> Ast<'expr, 'stmt> {
    pub fn name(&self) -> Option<SmallSpur> {
        match self {
            Self::Function(func) => Some(func.data().name),
            Self::Type(ty) => Some(ty.data().name),
            Self::Enum(e) => Some(e.data().name),
            Self::Trait(tr) => Some(tr.data().name),
            Self::Import(..) | Self::ExtendBlock(..) | Self::Alias(..) => None,
        }
    }

    pub fn is_import(&self) -> bool {
        if let Self::Import { .. } = self {
            true
        } else {
            false
        }
    }

    pub fn is_function(&self) -> bool {
        if let Self::Function { .. } = self {
            true
        } else {
            false
        }
    }

    pub fn location(&self) -> Location {
        match self {
            Self::Function(func) => func.loc(),
            Self::Type(ty) => ty.loc(),
            Self::Enum(en) => en.loc(),
            Self::Trait(tr) => tr.loc(),
            Self::Import(import) => import.loc(),
            Self::ExtendBlock(block) => block.loc(),
            Self::Alias(alias) => alias.loc(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ImportExposure {
    None(Locatable<SmallSpur>),
    All,
    Members(Vec<Locatable<(SmallSpur, Option<SmallSpur>)>>),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ImportDest {
    NativeLib,
    Package,
    Relative,
}

impl Default for ImportDest {
    fn default() -> Self {
        Self::Relative
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Decorator<'expr> {
    pub name: Locatable<SmallSpur>,
    pub args: Vec<Expr<'expr>>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Attribute {
    Visibility(Visibility),
    Const,
}

impl Attribute {
    #[inline]
    pub fn is_visibility(self) -> bool {
        if let Self::Visibility(_) = self {
            true
        } else {
            false
        }
    }

    #[inline]
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Visibility(vis) => vis.as_str(),
            Self::Const => "const",
        }
    }
}

impl<'a> TryFrom<(&Token<'a>, CurrentFile)> for Attribute {
    type Error = Locatable<Error>;

    fn try_from((token, file): (&Token<'a>, CurrentFile)) -> Result<Self, Self::Error> {
        Ok(match token.ty() {
            TokenType::Exposed => Self::Visibility(Visibility::Exposed),
            TokenType::Package => Self::Visibility(Visibility::Package),
            TokenType::Const => Self::Const,

            _ => {
                return Err(Locatable::new(
                    Error::Syntax(SyntaxError::Generic(format!(
                        "Expected an attribute, got `{}`",
                        token.ty()
                    ))),
                    Location::concrete(token, file),
                ));
            }
        })
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Visibility {
    FileLocal,
    Package,
    Exposed,
}

impl Visibility {
    pub fn as_str(self) -> &'static str {
        match self {
            Self::FileLocal => "file",
            Self::Package => "pkg",
            Self::Exposed => "exposed",
        }
    }
}

impl<'src, 'expr, 'stmt> Parser<'src, 'expr, 'stmt> {
    #[recursion_guard()]
    pub(super) fn ast(&mut self) -> ParseResult<Option<Ast<'expr, 'stmt>>> {
        let (mut decorators, mut attributes) = (Vec::with_capacity(5), Vec::with_capacity(5));

        while self.peek().is_ok() {
            if let Some(node) = self.ast_impl(&mut decorators, &mut attributes)? {
                return Ok(Some(node));
            }
        }

        Ok(None)
    }

    // Returns None when the function should be re-called, usually because an attribute or decorator was parsed
    #[recursion_guard]
    fn ast_impl(
        &mut self,
        decorators: &mut Vec<Locatable<Decorator<'expr>>>,
        attributes: &mut Vec<Locatable<Attribute>>,
    ) -> ParseResult<Option<Ast<'expr, 'stmt>>> {
        let peek = self.peek()?;
        match peek.ty() {
            TokenType::AtSign => {
                self.decorator(decorators)?;

                Ok(None)
            }

            TokenType::Exposed | TokenType::Package | TokenType::Const => {
                let token = self.next()?;
                let attr = Attribute::try_from((&token, self.current_file))?;
                attributes.push(Locatable::new(
                    attr,
                    Location::concrete(&token, self.current_file),
                ));

                Ok(None)
            }

            TokenType::Function => {
                let func = self.function(mem::take(decorators), mem::take(attributes))?;

                Ok(Some(func))
            }

            TokenType::Type => {
                let ty = self.type_decl(mem::take(decorators), mem::take(attributes))?;

                Ok(Some(ty))
            }

            TokenType::Extend => {
                let extension = self.extend_block(mem::take(decorators), mem::take(attributes))?;

                Ok(Some(extension))
            }

            TokenType::Enum => {
                let enu = self.enum_decl(mem::take(decorators), mem::take(attributes))?;

                Ok(Some(enu))
            }

            TokenType::Trait => {
                let tra = self.trait_decl(mem::take(decorators), mem::take(attributes))?;

                Ok(Some(tra))
            }

            TokenType::Import => {
                if !attributes.is_empty() {
                    Err(Locatable::new(
                        Error::Syntax(SyntaxError::NoAttributesAllowed("import")),
                        Location::concrete(&self.peek()?, self.current_file),
                    ))
                } else {
                    let import = self.import(mem::take(decorators))?;

                    Ok(Some(import))
                }
            }

            TokenType::Alias => {
                let alias = self.alias(mem::take(decorators), mem::take(attributes))?;
                Ok(Some(alias))
            }

            TokenType::Newline | TokenType::Space => {
                self.next()?;
                Ok(None)
            }

            ty => Err(Locatable::new(
                Error::Syntax(SyntaxError::InvalidTopLevel(ty)),
                Location::concrete(&self.peek()?, self.current_file),
            )),
        }
    }

    #[recursion_guard]
    fn import(
        &mut self,
        decorators: Vec<Locatable<Decorator<'expr>>>,
    ) -> ParseResult<Ast<'expr, 'stmt>> {
        let start_span = self.eat(TokenType::Import, [TokenType::Newline])?.span();

        let file = self.eat(TokenType::String, [TokenType::Newline])?;
        let literal = Literal::try_from((&file, self.current_file))?;
        let file = match literal {
            Literal::String(string) => Locatable::new(
                self.string_interner.intern(&string.to_string()),
                Location::concrete(file.span(), self.current_file),
            ),

            lit => {
                let err = if let Literal::Array(_) = lit {
                    Error::Syntax(SyntaxError::ImportByteStringLiteral)
                } else {
                    Error::Syntax(SyntaxError::ImportStringLiteral)
                };

                return Err(Locatable::new(
                    err,
                    Location::concrete(file.span(), self.current_file),
                ));
            }
        };

        let dest = if self.peek()?.ty() == TokenType::Library {
            self.eat(TokenType::Library, [TokenType::Newline])?;

            ImportDest::NativeLib
        } else if self.peek()?.ty() == TokenType::Package {
            self.eat(TokenType::Package, [TokenType::Newline])?;

            ImportDest::Package
        } else {
            ImportDest::default()
        };

        let exposes = if self.peek()?.ty() == TokenType::Exposing {
            self.eat(TokenType::Exposing, [TokenType::Newline])?;

            if self.peek()?.ty() == TokenType::Star {
                self.eat(TokenType::Star, [TokenType::Newline])?;

                ImportExposure::All
            } else {
                let mut members = Vec::with_capacity(5);
                while self.peek()?.ty() != TokenType::Newline {
                    let (span, member) = {
                        let ident = self.eat(TokenType::Ident, [TokenType::Newline])?;
                        (ident.span(), self.string_interner.intern(ident.source()))
                    };

                    let alias = if self.peek()?.ty() == TokenType::As {
                        self.eat(TokenType::As, [TokenType::Newline])?;
                        let alias = {
                            let ident = self.eat(TokenType::Ident, [TokenType::Newline])?;
                            self.string_interner.intern(ident.source())
                        };

                        Some(alias)
                    } else {
                        None
                    };

                    members.push(Locatable::new(
                        (member, alias),
                        Location::concrete(span, self.current_file),
                    ));

                    // TODO: Helpful error if they terminated it too soon
                    if self.peek()?.ty() == TokenType::Comma {
                        self.eat(TokenType::Comma, [TokenType::Newline])?;
                    } else {
                        break;
                    }
                }

                ImportExposure::Members(members)
            }
        } else {
            let alias = if self.peek()?.ty() == TokenType::As {
                self.eat(TokenType::As, [TokenType::Newline])?;

                let ident = self.eat(TokenType::Ident, [TokenType::Newline])?;
                Locatable::new(
                    self.string_interner.intern(ident.source()),
                    Location::concrete(ident.span(), self.current_file),
                )
            } else {
                // Get the last segment of the path as the alias if none is supplied
                let last_segment = self
                    .string_interner
                    .resolve(file.data())
                    .split('.')
                    .last()
                    .ok_or(Locatable::new(
                        Error::Syntax(SyntaxError::MissingImport),
                        Location::concrete(&self.peek()?, self.current_file),
                    ))?
                    .to_string();

                Locatable::new(
                    self.string_interner.intern(&last_segment),
                    Location::concrete(file.span(), self.current_file),
                )
            };

            ImportExposure::None(alias)
        };

        let end_span = self.eat(TokenType::Newline, [])?.span();
        let import = Import {
            file,
            dest,
            exposes,
        };

        // Import statements cannot have decorators, so throw an error if there are any
        if decorators.is_empty() {
            Ok(Ast::Import(Locatable::new(
                import,
                Location::concrete(Span::merge(start_span, end_span), self.current_file),
            )))
        } else {
            let first = decorators
                .iter()
                .next()
                .expect("There is at least one decorator")
                .span();

            Err(Locatable::new(
                Error::Syntax(SyntaxError::NoDecoratorsAllowed("import")),
                Location::concrete(
                    Span::merge(
                        first,
                        decorators
                            .iter()
                            .last()
                            .map(Locatable::span)
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
        decorators: Vec<Locatable<Decorator<'expr>>>,
        mut attrs: Vec<Locatable<Attribute>>,
    ) -> ParseResult<Ast<'expr, 'stmt>> {
        let start_span = self.eat(TokenType::Trait, [TokenType::Newline])?.span();
        let name = {
            let ident = self.eat(TokenType::Ident, [TokenType::Newline])?;
            self.string_interner.intern(ident.source())
        };
        let generics = self.generics()?;
        let sig_span_end = self.eat(TokenType::Newline, [])?.span();
        let signature_span = Span::merge(start_span, sig_span_end);

        let (mut method_decorators, mut method_attributes) =
            (Vec::with_capacity(3), Vec::with_capacity(3));

        let mut methods = Vec::with_capacity(4);
        while self.peek()?.ty() != TokenType::End {
            match self.peek()?.ty() {
                TokenType::AtSign => {
                    self.decorator(&mut method_decorators)?;
                }

                TokenType::Exposed | TokenType::Package => {
                    let token = self.next()?;
                    let attr = Attribute::try_from((&token, self.current_file))?;
                    method_attributes.push(Locatable::new(
                        attr,
                        Location::concrete(token, self.current_file),
                    ));
                }

                TokenType::Function => {
                    if !method_attributes
                        .iter()
                        .any(|attr| attr.data().is_visibility())
                    {
                        method_attributes.push(Locatable::new(
                            Attribute::Visibility(Visibility::FileLocal),
                            Location::implicit(self.current_file.index_span(), self.current_file),
                        ));
                    }

                    let method = self.function(
                        mem::take(&mut method_decorators),
                        mem::take(&mut method_attributes),
                    )?;

                    if let Ast::Function(method) = method {
                        methods.push(method);
                    } else {
                        unreachable!("Something really weird happened")
                    }
                }

                TokenType::Newline => {
                    self.eat(TokenType::Newline, [])?;
                }

                _ => {
                    return Err(Locatable::new(
                        Error::Syntax(SyntaxError::Generic("Only methods, attributes and decorators are allowed inside trait bodies".to_string())),
                        Location::concrete(&self.peek()?, self.current_file),
                    ));
                }
            }
        }
        let end_span = self.eat(TokenType::End, [TokenType::Newline])?.span();

        if !attrs.iter().any(|attr| attr.data().is_visibility()) {
            attrs.push(Locatable::new(
                Attribute::Visibility(Visibility::FileLocal),
                Location::concrete(signature_span, self.current_file),
            ));
        }

        let trait_decl = Trait {
            decorators,
            attrs,
            name,
            generics,
            methods,
        };

        Ok(Ast::Trait(Locatable::new(
            trait_decl,
            Location::concrete(Span::merge(start_span, end_span), self.current_file),
        )))
    }

    #[recursion_guard]
    fn enum_decl(
        &mut self,
        decorators: Vec<Locatable<Decorator<'expr>>>,
        mut attrs: Vec<Locatable<Attribute>>,
    ) -> ParseResult<Ast<'expr, 'stmt>> {
        let start_span = self.eat(TokenType::Enum, [TokenType::Newline])?.span();
        let name = {
            let ident = self.eat(TokenType::Ident, [TokenType::Newline])?;
            self.string_interner.intern(ident.source())
        };
        let generics = self.generics()?;
        let sig_span_end = self.eat(TokenType::Newline, [])?.span();
        let signature_span = Span::merge(start_span, sig_span_end);

        let mut variant_decorators = Vec::with_capacity(7);
        let mut variants = Vec::with_capacity(7);
        while self.peek()?.ty() != TokenType::End {
            match self.peek()?.ty() {
                TokenType::AtSign => {
                    self.decorator(&mut variant_decorators)?;
                }

                TokenType::Ident => {
                    let (name, start_span) = {
                        let ident = self.eat(TokenType::Ident, [TokenType::Newline])?;
                        (self.string_interner.intern(ident.source()), ident.span())
                    };

                    let variant = if self.peek()?.ty() == TokenType::LeftParen {
                        self.eat(TokenType::LeftParen, [TokenType::Newline])?;

                        let mut elements = Vec::with_capacity(3);
                        while self.peek()?.ty() != TokenType::RightParen {
                            let ty = self.ascribed_type()?;
                            elements.push(ty);

                            // TODO: Nice error here
                            if self.peek()?.ty() == TokenType::Comma {
                                self.eat(TokenType::Comma, [TokenType::Newline])?;
                            } else {
                                break;
                            }
                        }
                        self.eat(TokenType::RightParen, [TokenType::Newline])?;

                        EnumVariant::Tuple {
                            name,
                            elements,
                            decorators: mem::take(&mut variant_decorators),
                        }
                    } else {
                        EnumVariant::Unit {
                            name,
                            decorators: mem::take(&mut variant_decorators),
                        }
                    };

                    let end_span = self.eat(TokenType::Newline, [])?.span();

                    variants.push(Locatable::new(
                        variant,
                        Location::concrete(Span::merge(start_span, end_span), self.current_file),
                    ));
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
                        Location::concrete(&self.peek()?, self.current_file),
                    ));
                }
            }
        }
        let end_span = self.eat(TokenType::End, [TokenType::Newline])?.span();

        if !attrs.iter().any(|attr| attr.data().is_visibility()) {
            attrs.push(Locatable::new(
                Attribute::Visibility(Visibility::FileLocal),
                Location::concrete(signature_span, self.current_file),
            ));
        }

        let enum_decl = Enum {
            decorators,
            attrs,
            name,
            generics,
            variants,
        };

        Ok(Ast::Enum(Locatable::new(
            enum_decl,
            Location::concrete(Span::merge(start_span, end_span), self.current_file),
        )))
    }

    #[recursion_guard]
    fn decorator(&mut self, decorators: &mut Vec<Locatable<Decorator<'expr>>>) -> ParseResult<()> {
        let start = self.eat(TokenType::AtSign, [TokenType::Newline])?.span();
        let (name, name_span) = {
            let ident = self.eat(TokenType::Ident, [TokenType::Newline])?;
            let name = Locatable::new(
                self.string_interner.intern(ident.source()),
                Location::concrete(ident.span(), self.current_file),
            );

            (name, ident.span())
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

        decorators.push(Locatable::new(
            Decorator { name, args },
            Location::concrete(
                Span::merge(start, end_span.unwrap_or(name_span)),
                self.current_file,
            ),
        ));

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
        decorators: Vec<Locatable<Decorator<'expr>>>,
        mut attrs: Vec<Locatable<Attribute>>,
    ) -> ParseResult<Ast<'expr, 'stmt>> {
        let start_span = self.eat(TokenType::Type, [TokenType::Newline])?.span();
        let name = {
            let ident = self.eat(TokenType::Ident, [TokenType::Newline])?;
            self.string_interner.intern(ident.source())
        };
        let generics = self.generics()?;
        let sig_span_end = self.eat(TokenType::Newline, [])?.span();

        let signature_span = Span::merge(start_span, sig_span_end);

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
                    let attr = Attribute::try_from((&token, self.current_file))?;
                    member_attrs.push(Locatable::new(
                        attr,
                        Location::concrete(&token, self.current_file),
                    ));
                }

                TokenType::Ident => {
                    let (name, name_span) = {
                        let ident = self.eat(TokenType::Ident, [TokenType::Newline])?;
                        (self.string_interner.intern(ident.source()), ident.span())
                    };

                    let ty = if self.peek()?.ty() == TokenType::Colon {
                        self.eat(TokenType::Colon, [TokenType::Newline])?;
                        self.ascribed_type()?
                    } else {
                        Locatable::new(
                            Type::default(),
                            Location::implicit(name_span, self.current_file),
                        )
                    };

                    if !member_attrs.iter().any(|attr| attr.data().is_visibility()) {
                        member_attrs.push(Locatable::new(
                            Attribute::Visibility(Visibility::FileLocal),
                            Location::implicit(signature_span, self.current_file),
                        ));
                    }

                    let member = TypeMember {
                        decorators: mem::take(&mut member_decorators),
                        attrs: mem::take(&mut member_attrs),
                        name,
                        ty,
                    };
                    let end_span = self.eat(TokenType::Newline, [])?.span();

                    members.push(Locatable::new(
                        member,
                        Location::concrete(Span::merge(name_span, end_span), self.current_file),
                    ));
                }

                TokenType::Newline => {
                    self.eat(TokenType::Newline, [])?;
                }

                ty => {
                    return Err(Locatable::new(
                        Error::Syntax(SyntaxError::InvalidTopLevel(ty)),
                        Location::concrete(&self.peek()?, self.current_file),
                    ));
                }
            }
        }
        let end_span = self.eat(TokenType::End, [TokenType::Newline])?.span();

        if !member_attrs.is_empty() || !member_decorators.is_empty() {
            return Err(Locatable::new(
                Error::Syntax(SyntaxError::Generic("Attributes and functions must be before members or methods in type declarations".to_string())),
                Location::concrete(&self.peek()?, self.current_file),
            ));
        }

        if !attrs.iter().any(|attr| attr.data().is_visibility()) {
            attrs.push(Locatable::new(
                Attribute::Visibility(Visibility::FileLocal),
                Location::concrete(signature_span, self.current_file),
            ));
        }

        let type_decl = TypeDecl {
            decorators,
            attrs,
            name,
            generics,
            members,
        };

        Ok(Ast::Type(Locatable::new(
            type_decl,
            Location::concrete(Span::merge(start_span, end_span), self.current_file),
        )))
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
        _decorators: Vec<Locatable<Decorator<'expr>>>,
        mut _attrs: Vec<Locatable<Attribute>>,
    ) -> ParseResult<Ast<'expr, 'stmt>> {
        let start = self.eat(TokenType::Extend, [TokenType::Newline])?.span();
        let target = self.ascribed_type()?;

        let extender = if self.peek()?.ty() == TokenType::With {
            self.eat(TokenType::With, [TokenType::Newline])?;
            Some(self.ascribed_type()?)
        } else {
            None
        };

        self.eat(TokenType::Newline, [])?;

        let mut nodes = Vec::with_capacity(5);
        let (mut decorators, mut attributes) = (Vec::with_capacity(5), Vec::with_capacity(5));

        while self.peek()?.ty() != TokenType::End {
            if let Some(node) = self.ast_impl(&mut decorators, &mut attributes)? {
                nodes.push(node);
            }
        }

        if !decorators.is_empty() {
            todo!("error")
        }
        if !attributes.is_empty() {
            todo!("error")
        }

        let end = self.eat(TokenType::End, [])?.span();

        let block = ExtendBlock {
            target,
            extender,
            nodes,
        };

        Ok(Ast::ExtendBlock(Locatable::new(
            block,
            Location::concrete(Span::merge(start, end), self.current_file),
        )))
    }

    /// ```ebnf
    /// Decorator* Attribute* 'alias' Type = Type '\n'
    /// ```
    #[recursion_guard]
    fn alias(
        &mut self,
        decorators: Vec<Locatable<Decorator<'expr>>>,
        attrs: Vec<Locatable<Attribute>>,
    ) -> ParseResult<Ast<'expr, 'stmt>> {
        let start = self.eat(TokenType::Alias, [TokenType::Newline])?.span();
        let alias = self.ascribed_type()?;
        self.eat(TokenType::Equal, [TokenType::Newline])?;

        let actual = self.ascribed_type()?;
        let end = self.eat(TokenType::Newline, [])?.span();

        let alias = Alias {
            decorators,
            attrs,
            alias,
            actual,
        };

        Ok(Ast::Alias(Locatable::new(
            alias,
            Location::concrete(Span::merge(start, end), self.current_file),
        )))
    }

    /// ```ebnf
    /// Function ::=
    ///     Decorator* Attribute* 'fn' Ident '(' FunctionArgs* ')' ('->' Type)? '\n'
    ///         Statement* | 'empty'
    ///     'end'
    /// ```
    #[recursion_guard]
    fn function(
        &mut self,
        decorators: Vec<Locatable<Decorator<'expr>>>,
        mut attrs: Vec<Locatable<Attribute>>,
    ) -> ParseResult<Ast<'expr, 'stmt>> {
        let start_span = self.eat(TokenType::Function, [TokenType::Newline])?.span();
        let name = {
            let ident = self.eat(TokenType::Ident, [TokenType::Newline])?;
            self.string_interner.intern(ident.source())
        };
        let args = self.function_args()?;

        let returns = if self.peek()?.ty() == TokenType::RightArrow {
            self.eat(TokenType::RightArrow, [])?;
            Some(self.ascribed_type()?)
        } else {
            None
        };
        let sig_end_span = self.eat(TokenType::Newline, [])?.span();
        let signature_span = Span::merge(start_span, sig_end_span);

        let returns = returns.unwrap_or_else(|| {
            Locatable::new(
                Type::default(),
                Location::implicit(signature_span, self.current_file),
            )
        });

        while self.peek()?.ty() == TokenType::Newline {
            self.eat(TokenType::Newline, [])?;
        }

        let mut body = Vec::with_capacity(20);
        while self.peek()?.ty() != TokenType::End {
            if let Some(stmt) = self.stmt()? {
                body.push(stmt);
            }
        }

        let end_span = self.eat(TokenType::End, [TokenType::Newline])?.span();

        if !attrs.iter().any(|attr| attr.data().is_visibility()) {
            attrs.push(Locatable::new(
                Attribute::Visibility(Visibility::FileLocal),
                Location::implicit(signature_span, self.current_file),
            ));
        }

        let func = Function {
            decorators,
            attrs,
            name,
            args,
            returns,
            body,
        };

        Ok(Ast::Function(Locatable::new(
            func,
            Location::concrete(Span::merge(start_span, end_span), self.current_file),
        )))
    }

    /// ```ebnf
    /// FunctionArgs ::= '(' Args? ')'
    /// Args ::= Argument | Argument ',' Args
    /// Argument ::= Ident ( ':' Type )?
    /// ```
    #[recursion_guard]
    fn function_args(&mut self) -> ParseResult<Vec<Locatable<FuncArg>>> {
        self.eat(TokenType::LeftParen, [TokenType::Newline])?;

        let mut args = Vec::with_capacity(7);
        while self.peek()?.ty() != TokenType::RightParen {
            let (comptime, name, name_span) =
                match self.eat_of([TokenType::Ident, TokenType::Const], [TokenType::Newline])? {
                    ident if ident.ty() == TokenType::Ident => (
                        false,
                        Locatable::new(
                            self.string_interner.intern(ident.source()),
                            Location::concrete(ident.span(), self.current_file),
                        ),
                        ident.span(),
                    ),

                    token if token.ty() == TokenType::Const => {
                        let ident = self.eat(TokenType::Ident, [TokenType::Newline])?;
                        let name = Locatable::new(
                            self.string_interner.intern(ident.source()),
                            Location::concrete(ident.span(), self.current_file),
                        );

                        (true, name, token.span())
                    }

                    _ => unreachable!(),
                };

            if self.peek()?.ty() == TokenType::Newline {
                self.eat(TokenType::Newline, [])?;
                continue;
            }

            let (ty, ty_span) = if self.peek()?.ty() == TokenType::Colon {
                self.eat(TokenType::Colon, [TokenType::Newline])?;
                let ty = self.ascribed_type()?;
                let span = Some(ty.span());

                (ty, span)
            } else {
                (
                    Locatable::new(
                        Type::default(),
                        Location::implicit(name_span, self.current_file),
                    ),
                    None,
                )
            };

            let arg = FuncArg { name, ty, comptime };
            let arg_span = if let Some(end) = ty_span {
                Span::merge(name_span, end)
            } else {
                name_span
            };

            args.push(Locatable::new(
                arg,
                Location::concrete(arg_span, self.current_file),
            ));

            if self.peek()?.ty() == TokenType::Comma {
                self.eat(TokenType::Comma, [TokenType::Newline])?;
            } else {
                break;
            }
        }
        self.eat(TokenType::RightParen, [TokenType::Newline])?;

        Ok(args)
    }

    /// ```ebnf
    /// Generics ::= '[' GenericArgs? ']'
    /// GenericArgs ::= Type | Type ',' GenericArgs
    /// ```
    #[recursion_guard]
    fn generics(&mut self) -> ParseResult<Vec<Locatable<Type>>> {
        if self.peek()?.ty() == TokenType::LeftBrace {
            self.eat(TokenType::LeftBrace, [TokenType::Newline])?;

            let mut generics = Vec::with_capacity(5);
            while self.peek()?.ty() != TokenType::RightCaret {
                generics.push(self.ascribed_type()?);

                if self.peek()?.ty() == TokenType::Comma {
                    self.eat(TokenType::Comma, [TokenType::Newline])?;
                } else {
                    // TODO: Check if next is a `>` and if so emit a helpful error
                    break;
                }
            }
            self.eat(TokenType::RightBrace, [TokenType::Newline])?;

            Ok(generics)
        } else {
            Ok(Vec::new())
        }
    }
}
