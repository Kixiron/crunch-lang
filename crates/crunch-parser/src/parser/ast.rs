use crate::{
    error::{Error, Locatable, Location, ParseResult, SyntaxError},
    files::FileId,
    parser::{Expr, Literal, Parser, Stmt},
    symbol_table::{Symbol, SymbolLocation},
    token::{Token, TokenType},
};

use lasso::SmallSpur;

use alloc::{boxed::Box, format, string::ToString, vec::Vec};
use core::{convert::TryFrom, mem};

#[derive(Debug, Clone, PartialEq)]
pub enum Ast<'expr, 'stmt> {
    Function {
        decorators: Vec<Decorator<'expr>>,
        attributes: Vec<Attribute>,
        name: SmallSpur,
        args: Vec<(SmallSpur, Type, bool)>,
        returns: Type,
        body: Vec<Stmt<'expr, 'stmt>>,
    },

    Type {
        decorators: Vec<Decorator<'expr>>,
        attributes: Vec<Attribute>,
        name: SmallSpur,
        generics: Vec<Type>,
        members: Vec<TypeMember<'expr>>,
        methods: Vec<Ast<'expr, 'stmt>>,
    },

    Enum {
        decorators: Vec<Decorator<'expr>>,
        attributes: Vec<Attribute>,
        name: SmallSpur,
        generics: Vec<Type>,
        variants: Vec<EnumVariant<'expr>>,
    },

    Trait {
        decorators: Vec<Decorator<'expr>>,
        attributes: Vec<Attribute>,
        name: SmallSpur,
        generics: Vec<Type>,
        methods: Vec<Ast<'expr, 'stmt>>,
    },

    Import {
        file: SmallSpur,
        dest: ImportDest,
        exposes: ImportExposure,
    },
}

impl<'expr, 'stmt> Ast<'expr, 'stmt> {
    pub fn name(&self) -> SmallSpur {
        match self {
            Self::Function { name, .. } => *name,
            Self::Type { name, .. } => *name,
            Self::Enum { name, .. } => *name,
            Self::Trait { name, .. } => *name,
            Self::Import { file, .. } => *file,
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
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Builtin(BuiltinType),
    TraitObj(Vec<Type>),
    Bounded { ty: SmallSpur, bounds: Vec<Type> },
    Custom(SmallSpur),
    Infer,
}

impl<'a> Type {
    pub(crate) fn push(&mut self, ty: Type) {
        match self {
            Self::TraitObj(vec) => vec.push(ty),
            Self::Bounded { bounds, .. } => bounds.push(ty),
            Self::Builtin(BuiltinType::Array(val)) => **val = ty,
            Self::Builtin(BuiltinType::Tuple(types)) => types.push(ty),

            _ => unreachable!("Internal error, attempted to push to unpushable type"),
        }
    }
}

impl Default for Type {
    fn default() -> Self {
        Self::Infer
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum BuiltinType {
    Integer { sign: Signedness, width: u16 },
    IntReg(Signedness),
    IntPtr(Signedness),
    Float { width: u16 },
    Boolean,
    String,
    Rune,
    Unit,
    Absurd,
    Array(Box<Type>),
    Tuple(Vec<Type>),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Signedness {
    Unsigned,
    Signed,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ImportExposure {
    None(SmallSpur),
    All,
    Members(Vec<(SmallSpur, Option<SmallSpur>)>),
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
pub enum EnumVariant<'expr> {
    Unit {
        name: SmallSpur,
        decorators: Vec<Decorator<'expr>>,
    },
    Tuple {
        name: SmallSpur,
        elements: Vec<Type>,
        decorators: Vec<Decorator<'expr>>,
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
pub struct TypeMember<'expr> {
    pub decorators: Vec<Decorator<'expr>>,
    pub attributes: Vec<Attribute>,
    pub name: SmallSpur,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Decorator<'expr> {
    pub name: SmallSpur,
    pub args: Vec<Expr<'expr>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Attribute {
    Visibility(Visibility),
    Comptime,
}

impl Attribute {
    pub fn is_visibility(&self) -> bool {
        if let Self::Visibility(_) = self {
            true
        } else {
            false
        }
    }
}

impl<'a> TryFrom<(&Token<'a>, FileId)> for Attribute {
    type Error = Locatable<Error>;

    fn try_from((token, file): (&Token<'a>, FileId)) -> Result<Self, Self::Error> {
        Ok(match token.ty() {
            TokenType::Exposed => Self::Visibility(Visibility::Exposed),
            TokenType::Package => Self::Visibility(Visibility::Package),
            TokenType::Comptime => Self::Comptime,

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
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Visibility {
    FileLocal,
    Package,
    Exposed,
}

impl<'src, 'expr, 'stmt> Parser<'src, 'expr, 'stmt> {
    pub(super) fn ast(&mut self) -> ParseResult<Option<Ast<'expr, 'stmt>>> {
        let _frame = self.add_stack_frame()?;

        let (mut decorators, mut attributes) = (Vec::with_capacity(5), Vec::with_capacity(5));

        while self.peek().is_ok() {
            if let Some(node) = self.ast_impl(&mut decorators, &mut attributes)? {
                return Ok(Some(node));
            }
        }

        Ok(None)
    }

    // Returns None when the function should be re-called, usually because an attribute or decorator was parsed
    fn ast_impl(
        &mut self,
        decorators: &mut Vec<Decorator<'expr>>,
        attributes: &mut Vec<Attribute>,
    ) -> ParseResult<Option<Ast<'expr, 'stmt>>> {
        let _frame = self.add_stack_frame()?;

        let peek = self.peek()?;
        let node = match peek.ty() {
            TokenType::AtSign => {
                self.decorator(decorators)?;

                Ok(None)
            }

            TokenType::Exposed | TokenType::Package | TokenType::Comptime => {
                let attr = Attribute::try_from((&self.next()?, self.current_file))?;
                attributes.push(attr);

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
                    return Err(Locatable::new(
                        Error::Syntax(SyntaxError::NoAttributesAllowed("import")),
                        Location::new(&self.peek()?, self.current_file),
                    ));
                }

                let import = self.import(mem::take(decorators))?;

                Ok(Some(import))
            }

            TokenType::Newline | TokenType::Space => {
                self.next()?;
                Ok(None)
            }

            ty => Err(Locatable::new(
                Error::Syntax(SyntaxError::InvalidTopLevel(ty)),
                Location::new(&self.peek()?, self.current_file),
            )),
        };

        if let Ok(Some(ref node)) = node {
            self.symbol_table.insert(
                &self.string_interner,
                SymbolLocation::new(self.current_file, node.name()),
                Symbol::from(node),
            )?;
        }

        node
    }

    fn import(&mut self, mut decorators: Vec<Decorator<'expr>>) -> ParseResult<Ast<'expr, 'stmt>> {
        let _frame = self.add_stack_frame()?;
        decorators.shrink_to_fit();

        self.eat(TokenType::Import, [TokenType::Newline])?;

        let file = self.eat(TokenType::String, [TokenType::Newline])?;
        let literal = Literal::try_from((&file, self.current_file))?;
        let file = match literal {
            Literal::String(string) => self.string_interner.intern(&string.to_string()),

            lit => {
                let err = if let Literal::ByteVec(_) = lit {
                    Error::Syntax(SyntaxError::ImportByteStringLiteral)
                } else {
                    Error::Syntax(SyntaxError::ImportStringLiteral)
                };

                return Err(Locatable::new(err, Location::new(&file, self.current_file)));
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
                    let member = {
                        let ident = self.eat(TokenType::Ident, [TokenType::Newline])?;
                        self.string_interner.intern(ident.source())
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

                    members.push((member, alias));

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
                self.string_interner.intern(ident.source())
            } else {
                // Get the last segment of the path as the alias if none is supplied
                let last_segment = self
                    .string_interner
                    .resolve(&file)
                    .split('.')
                    .last()
                    .ok_or(Locatable::new(
                        Error::Syntax(SyntaxError::MissingImport),
                        Location::new(&self.peek()?, self.current_file),
                    ))?
                    .to_string();

                self.string_interner.intern(&last_segment)
            };

            ImportExposure::None(alias)
        };

        self.eat(TokenType::Newline, [])?;

        Ok(Ast::Import {
            file,
            dest,
            exposes,
        })
    }

    fn trait_decl(
        &mut self,
        mut decorators: Vec<Decorator<'expr>>,
        mut attributes: Vec<Attribute>,
    ) -> ParseResult<Ast<'expr, 'stmt>> {
        let _frame = self.add_stack_frame()?;
        decorators.shrink_to_fit();
        attributes.shrink_to_fit();

        if !attributes.iter().any(Attribute::is_visibility) {
            attributes.reserve(1);
            attributes.push(Attribute::Visibility(Visibility::FileLocal));
        }

        self.eat(TokenType::Trait, [TokenType::Newline])?;
        let name = {
            let ident = self.eat(TokenType::Ident, [TokenType::Newline])?;
            self.string_interner.intern(ident.source())
        };
        let generics = self.generics()?;
        self.eat(TokenType::Newline, [])?;

        let (mut method_decorators, mut method_attributes) =
            (Vec::with_capacity(3), Vec::with_capacity(3));

        let mut methods = Vec::with_capacity(4);
        while self.peek()?.ty() != TokenType::End {
            match self.peek()?.ty() {
                TokenType::AtSign => {
                    self.decorator(&mut method_decorators)?;
                }

                TokenType::Exposed | TokenType::Package => {
                    method_attributes
                        .push(Attribute::try_from((&self.next()?, self.current_file))?);
                }

                TokenType::Function => {
                    if !method_attributes.iter().any(Attribute::is_visibility) {
                        method_attributes.push(Attribute::Visibility(Visibility::FileLocal));
                    }
                    method_attributes.shrink_to_fit();
                    method_decorators.shrink_to_fit();

                    let method = self.function(
                        mem::take(&mut method_decorators),
                        mem::take(&mut method_attributes),
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
        methods.shrink_to_fit();

        self.eat(TokenType::End, [TokenType::Newline])?;

        Ok(Ast::Trait {
            decorators,
            attributes,
            name,
            generics,
            methods,
        })
    }

    fn enum_decl(
        &mut self,
        mut decorators: Vec<Decorator<'expr>>,
        mut attributes: Vec<Attribute>,
    ) -> ParseResult<Ast<'expr, 'stmt>> {
        let _frame = self.add_stack_frame()?;
        decorators.shrink_to_fit();
        attributes.shrink_to_fit();

        if !attributes.iter().any(Attribute::is_visibility) {
            attributes.reserve(1);
            attributes.push(Attribute::Visibility(Visibility::FileLocal));
        }

        self.eat(TokenType::Enum, [TokenType::Newline])?;
        let name = {
            let ident = self.eat(TokenType::Ident, [TokenType::Newline])?;
            self.string_interner.intern(ident.source())
        };
        let generics = self.generics()?;
        self.eat(TokenType::Newline, [])?;

        let mut variant_decorators = Vec::with_capacity(7);
        let mut variants = Vec::with_capacity(7);
        while self.peek()?.ty() != TokenType::End {
            match self.peek()?.ty() {
                TokenType::AtSign => {
                    self.decorator(&mut variant_decorators)?;
                }

                TokenType::Ident => {
                    let name = {
                        let ident = self.eat(TokenType::Ident, [TokenType::Newline])?;
                        self.string_interner.intern(ident.source())
                    };

                    if self.peek()?.ty() == TokenType::LeftParen {
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

                        variants.push(EnumVariant::Tuple {
                            name,
                            elements,
                            decorators: mem::take(&mut variant_decorators),
                        })
                    } else {
                        variants.push(EnumVariant::Unit {
                            name,
                            decorators: mem::take(&mut variant_decorators),
                        });
                    }

                    self.eat(TokenType::Newline, [])?;
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
        variants.shrink_to_fit();
        self.eat(TokenType::End, [TokenType::Newline])?;

        Ok(Ast::Enum {
            decorators,
            attributes,
            name,
            generics,
            variants,
        })
    }

    fn decorator(&mut self, decorators: &mut Vec<Decorator<'expr>>) -> ParseResult<()> {
        let _frame = self.add_stack_frame()?;

        self.eat(TokenType::AtSign, [TokenType::Newline])?;
        let name = {
            let ident = self.eat(TokenType::Ident, [TokenType::Newline])?;
            self.string_interner.intern(ident.source())
        };

        let args = if self.peek()?.ty() == TokenType::LeftParen {
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
            args.shrink_to_fit();
            self.eat(TokenType::RightParen, [TokenType::Newline])?;

            args
        } else {
            Vec::new()
        };

        decorators.push(Decorator { name, args });

        Ok(())
    }

    fn type_decl(
        &mut self,
        mut decorators: Vec<Decorator<'expr>>,
        mut attributes: Vec<Attribute>,
    ) -> ParseResult<Ast<'expr, 'stmt>> {
        let _frame = self.add_stack_frame()?;
        decorators.shrink_to_fit();
        attributes.shrink_to_fit();

        if !attributes.iter().any(Attribute::is_visibility) {
            attributes.reserve(1);
            attributes.push(Attribute::Visibility(Visibility::FileLocal));
        }

        self.eat(TokenType::Type, [TokenType::Newline])?;
        let name = {
            let ident = self.eat(TokenType::Ident, [TokenType::Newline])?;
            self.string_interner.intern(ident.source())
        };
        let generics = self.generics()?;
        self.eat(TokenType::Newline, [])?;

        let (mut member_decorators, mut member_attributes) =
            (Vec::with_capacity(3), Vec::with_capacity(3));

        let mut members = Vec::with_capacity(5);
        let mut methods = Vec::with_capacity(5);

        while self.peek()?.ty() != TokenType::End {
            match self.peek()?.ty() {
                TokenType::AtSign => {
                    self.decorator(&mut member_decorators)?;
                }

                TokenType::Exposed | TokenType::Package => {
                    member_attributes
                        .push(Attribute::try_from((&self.next()?, self.current_file))?);
                }

                TokenType::Function => {
                    let method = self.function(
                        mem::take(&mut member_decorators),
                        mem::take(&mut member_attributes),
                    )?;
                    methods.push(method);
                }

                TokenType::Ident => {
                    let name = {
                        let ident = self.eat(TokenType::Ident, [TokenType::Newline])?;
                        self.string_interner.intern(ident.source())
                    };

                    let ty = if self.peek()?.ty() == TokenType::Colon {
                        self.eat(TokenType::Colon, [TokenType::Newline])?;
                        self.ascribed_type()?
                    } else {
                        Type::default()
                    };

                    if !member_attributes.iter().any(Attribute::is_visibility) {
                        member_attributes.push(Attribute::Visibility(Visibility::FileLocal));
                    }
                    member_attributes.shrink_to_fit();
                    member_decorators.shrink_to_fit();

                    let member = TypeMember {
                        decorators: mem::take(&mut member_decorators),
                        attributes: mem::take(&mut member_attributes),
                        name,
                        ty,
                    };
                    self.eat(TokenType::Newline, [])?;

                    members.push(member);
                }

                TokenType::Newline => {
                    self.eat(TokenType::Newline, [])?;
                }

                ty => {
                    return Err(Locatable::new(
                        Error::Syntax(SyntaxError::InvalidTopLevel(ty)),
                        Location::new(&self.peek()?, self.current_file),
                    ));
                }
            }
        }
        self.eat(TokenType::End, [TokenType::Newline])?;

        if !member_attributes.is_empty() || !member_decorators.is_empty() {
            return Err(Locatable::new(
                Error::Syntax(SyntaxError::Generic("Attributes and functions must be before members or methods in type declarations".to_string())),
                Location::new(&self.peek()?, self.current_file),
            ));
        }

        Ok(Ast::Type {
            decorators,
            attributes,
            name,
            generics,
            members,
            methods,
        })
    }

    fn function(
        &mut self,
        mut decorators: Vec<Decorator<'expr>>,
        mut attributes: Vec<Attribute>,
    ) -> ParseResult<Ast<'expr, 'stmt>> {
        let _frame = self.add_stack_frame()?;
        decorators.shrink_to_fit();
        attributes.shrink_to_fit();

        if !attributes.iter().any(Attribute::is_visibility) {
            attributes.reserve(1);
            attributes.push(Attribute::Visibility(Visibility::FileLocal));
        }

        self.eat(TokenType::Function, [TokenType::Newline])?;
        let name = {
            let ident = self.eat(TokenType::Ident, [TokenType::Newline])?;
            self.string_interner.intern(ident.source())
        };
        let args = self.function_args()?;

        let returns = if self.peek()?.ty() == TokenType::RightArrow {
            self.eat(TokenType::RightArrow, [])?;
            self.ascribed_type()?
        } else {
            Type::default()
        };
        self.eat(TokenType::Newline, [])?;
        while self.peek()?.ty() == TokenType::Newline {
            self.eat(TokenType::Newline, [])?;
        }

        let mut body = Vec::with_capacity(20);
        while self.peek()?.ty() != TokenType::End {
            if let Some(stmt) = self.stmt()? {
                body.push(stmt);
            }
        }

        self.eat(TokenType::End, [TokenType::Newline])?;
        body.shrink_to_fit();

        Ok(Ast::Function {
            decorators,
            attributes,
            name,
            args,
            returns,
            body,
        })
    }

    fn function_args(&mut self) -> ParseResult<Vec<(SmallSpur, Type, bool)>> {
        let _frame = self.add_stack_frame()?;
        self.eat(TokenType::LeftParen, [TokenType::Newline])?;

        let mut args = Vec::with_capacity(7);
        while self.peek()?.ty() != TokenType::RightParen {
            let (comptime, ident) = match self.eat_of(
                [TokenType::Ident, TokenType::Comptime],
                [TokenType::Newline],
            )? {
                ident if ident.ty() == TokenType::Ident => {
                    (false, self.string_interner.intern(ident.source()))
                }
                token if token.ty() == TokenType::Comptime => (true, {
                    let ident = self.eat(TokenType::Ident, [TokenType::Newline])?;
                    self.string_interner.intern(ident.source())
                }),

                _ => unreachable!(),
            };

            if self.peek()?.ty() == TokenType::Newline {
                self.eat(TokenType::Newline, [])?;
                continue;
            }

            let ty = if self.peek()?.ty() == TokenType::Colon {
                self.eat(TokenType::Colon, [TokenType::Newline])?;
                self.ascribed_type()?
            } else {
                Type::default()
            };
            args.push((ident, ty, comptime));

            if self.peek()?.ty() == TokenType::Comma {
                self.eat(TokenType::Comma, [TokenType::Newline])?;
            } else {
                break;
            }
        }
        self.eat(TokenType::RightParen, [TokenType::Newline])?;
        args.shrink_to_fit();

        Ok(args)
    }

    fn generics(&mut self) -> ParseResult<Vec<Type>> {
        let _frame = self.add_stack_frame()?;

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
            generics.shrink_to_fit();

            Ok(generics)
        } else {
            Ok(Vec::new())
        }
    }

    pub(super) fn ascribed_type(&mut self) -> ParseResult<Type> {
        let _frame = self.add_stack_frame()?;

        let mut ty = match self.eat_of([TokenType::Type, TokenType::Ident], [TokenType::Newline])? {
            Token { source, .. } if source == "str" => {
                return Ok(Type::Builtin(BuiltinType::String));
            }
            Token { source, .. } if source == "rune" => {
                return Ok(Type::Builtin(BuiltinType::Rune));
            }
            Token { source, .. } if source == "bool" => {
                return Ok(Type::Builtin(BuiltinType::Boolean));
            }
            Token { source, .. } if source == "unit" => {
                return Ok(Type::Builtin(BuiltinType::Unit));
            }
            Token { source, .. } if source == "absurd" => {
                return Ok(Type::Builtin(BuiltinType::Absurd));
            }
            Token { source, .. } if source == "ureg" || source == "ireg" => {
                return Ok(Type::Builtin(BuiltinType::IntReg(
                    if source.chars().next().unwrap() == 'u' {
                        Signedness::Unsigned
                    } else {
                        Signedness::Signed
                    },
                )));
            }
            Token { source, .. } if source == "uptr" || source == "iptr" => {
                return Ok(Type::Builtin(BuiltinType::IntPtr(
                    if source.chars().next().unwrap() == 'u' {
                        Signedness::Unsigned
                    } else {
                        Signedness::Signed
                    },
                )));
            }

            Token { source, .. }
                if source.starts_with('u') && source.chars().skip(1).all(|c| c.is_numeric()) =>
            {
                let width: u16 = source.chars().skip(1).collect::<String>().parse().unwrap();
                return Ok(Type::Builtin(BuiltinType::Integer {
                    sign: Signedness::Unsigned,
                    width,
                }));
            }
            Token { source, .. }
                if source.starts_with('i') && source.chars().skip(1).all(|c| c.is_numeric()) =>
            {
                let width: u16 = source.chars().skip(1).collect::<String>().parse().unwrap();
                return Ok(Type::Builtin(BuiltinType::Integer {
                    sign: Signedness::Signed,
                    width,
                }));
            }
            Token { source, .. }
                if source.starts_with('f') && source.chars().skip(1).all(|c| c.is_numeric()) =>
            {
                let width: u16 = source.chars().skip(1).collect::<String>().parse().unwrap();
                return Ok(Type::Builtin(BuiltinType::Float { width }));
            }

            Token { source, .. } if source == "arr" => {
                // TODO: Maybe parse this immediately?
                Type::Builtin(BuiltinType::Array(Box::new(Type::Infer)))
            }
            Token { source, .. } if source == "tup" => {
                Type::Builtin(BuiltinType::Tuple(Vec::new()))
            }

            Token { ty, .. } if ty == TokenType::Type => Type::TraitObj(Vec::new()),
            Token { ty, source, .. }
                if ty == TokenType::Ident
                    && self.peek().map(|t| t.ty()) == Ok(TokenType::LeftBrace) =>
            {
                Type::Bounded {
                    ty: self.string_interner.intern(source),
                    bounds: Vec::new(),
                }
            }
            Token { ty, source, .. } if ty == TokenType::Ident => {
                return Ok(Type::Custom(self.string_interner.intern(source)));
            }

            _ => unreachable!(),
        };

        while self.peek()?.ty() == TokenType::Newline {
            self.eat(TokenType::Newline, [])?;
        }

        if self.peek()?.ty() == TokenType::LeftBrace {
            self.eat(TokenType::LeftBrace, [])?;
            while self.peek()?.ty() != TokenType::RightBrace {
                ty.push(self.ascribed_type()?);

                if self.peek()?.ty() == TokenType::Comma {
                    self.eat(TokenType::Comma, [TokenType::Newline])?;
                } else {
                    break;
                }
            }
            self.eat(TokenType::RightBrace, [])?;
        }

        if let Type::Bounded { ty, bounds } = ty {
            if bounds.is_empty() {
                Ok(Type::Custom(ty))
            } else {
                Ok(Type::Bounded { ty, bounds })
            }
        } else {
            Ok(ty)
        }
    }
}
