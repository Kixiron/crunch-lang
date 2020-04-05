use super::{Cord, Expr, Interner, Literal, Parser, Stmt};
use crate::{
    error::{Error, Locatable, Location, ParseResult, SyntaxError},
    files::FileId,
    token::{Token, TokenType},
};

use alloc::{boxed::Box, format, string::ToString, vec::Vec};
use core::{convert::TryFrom, mem};

#[derive(Debug, Clone, PartialEq)]
pub enum Ast<'expr, 'stmt> {
    Function {
        decorators: Vec<Decorator<'expr>>,
        attributes: Vec<Attribute>,
        name: Cord,
        generics: Vec<Cord>,
        args: Vec<(Cord, Type)>,
        returns: Type,
        body: Vec<Stmt<'expr, 'stmt>>,
    },

    Type {
        decorators: Vec<Decorator<'expr>>,
        attributes: Vec<Attribute>,
        name: Cord,
        generics: Vec<Cord>,
        members: Vec<TypeMember<'expr>>,
        methods: Vec<Ast<'expr, 'stmt>>,
    },

    Enum {
        decorators: Vec<Decorator<'expr>>,
        attributes: Vec<Attribute>,
        name: Cord,
        generics: Vec<Cord>,
        variants: Vec<EnumVariant<'expr>>,
    },

    Trait {
        decorators: Vec<Decorator<'expr>>,
        attributes: Vec<Attribute>,
        name: Cord,
        generics: Vec<Cord>,
        methods: Vec<Ast<'expr, 'stmt>>,
    },

    Import {
        file: Cord,
        dest: ImportDest,
        exposes: ImportExposure,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Builtin(BuiltinType),
    Custom(Cord),
    Infer,
}

impl<'a> TryFrom<(Token<'a>, FileId, &Interner)> for Type {
    type Error = Locatable<Error>;

    fn try_from(
        (token, file, interner): (Token<'a>, FileId, &Interner),
    ) -> Result<Self, Self::Error> {
        if token.ty() == TokenType::Ident {
            if let Ok(ty) = BuiltinType::try_from((token, file, interner)) {
                Ok(Self::Builtin(ty))
            } else {
                Ok(Self::Custom(interner.intern(token.source())))
            }
        } else {
            Err(Locatable::new(
                Error::Syntax(SyntaxError::Generic(
                    "Expected an ident as a type name".to_string(),
                )),
                Location::new(&token, file),
            ))
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
    Integer,
    Float,
    Boolean,
    String,
    Unit,
    Vec(Box<Type>),
}

impl<'a> TryFrom<(Token<'a>, FileId, &Interner)> for BuiltinType {
    type Error = Locatable<Error>;

    fn try_from(
        (mut token, file, interner): (Token<'a>, FileId, &Interner),
    ) -> Result<Self, Self::Error> {
        Ok(match token.source() {
            "str" => Self::String,
            "int" => Self::Integer,
            "float" => Self::Float,
            "bool" => Self::Boolean,
            "unit" => Self::Unit,
            ty if ty.starts_with('[') && ty.ends_with(']') => {
                token.source = &token.source()[1..ty.len() - 1];

                if token.source() == "" {
                    Self::Vec(Box::new(Type::Infer))
                } else {
                    Self::Vec(Box::new(Type::try_from((token, file, interner))?))
                }
            }

            _ => {
                return Err(Locatable::new(
                    Error::Syntax(SyntaxError::Generic(
                        "Expected an ident as a type name".to_string(),
                    )),
                    Location::new(&token, file),
                ));
            }
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ImportExposure {
    None(Cord),
    All,
    Members(Vec<(Cord, Option<Cord>)>),
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
        name: Cord,
        decorators: Vec<Decorator<'expr>>,
    },
    Tuple {
        name: Cord,
        elements: Vec<Type>,
        decorators: Vec<Decorator<'expr>>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeMember<'expr> {
    pub decorators: Vec<Decorator<'expr>>,
    pub attributes: Vec<Attribute>,
    pub name: Cord,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Decorator<'expr> {
    pub name: Cord,
    pub args: Vec<Expr<'expr>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Attribute {
    Visibility(Visibility),
}

impl Attribute {
    #[allow(irrefutable_let_patterns)]
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

        match self.peek()?.ty() {
            TokenType::AtSign => {
                self.decorator(decorators)?;

                Ok(None)
            }

            TokenType::Exposed | TokenType::Package => {
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

            TokenType::Newline => {
                self.eat(TokenType::Newline)?;
                Ok(None)
            }

            ty => Err(Locatable::new(
                Error::Syntax(SyntaxError::InvalidTopLevel(ty)),
                Location::new(&self.peek()?, self.current_file),
            )),
        }
    }

    fn import(&mut self, mut decorators: Vec<Decorator<'expr>>) -> ParseResult<Ast<'expr, 'stmt>> {
        let _frame = self.add_stack_frame()?;
        decorators.shrink_to_fit();

        self.eat(TokenType::Import)?;

        let file = self.eat(TokenType::String)?;
        let file = match Literal::try_from((&file, self.current_file))? {
            Literal::String(string) => self.intern_string(&string),

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
            self.eat(TokenType::Library)?;

            ImportDest::NativeLib
        } else if self.peek()?.ty() == TokenType::Package {
            self.eat(TokenType::Package)?;

            ImportDest::Package
        } else {
            ImportDest::default()
        };

        let exposes = if self.peek()?.ty() == TokenType::Exposing {
            self.eat(TokenType::Exposing)?;

            if self.peek()?.ty() == TokenType::Star {
                self.eat(TokenType::Star)?;

                ImportExposure::All
            } else {
                let mut members = Vec::with_capacity(5);
                while self.peek()?.ty() != TokenType::Newline {
                    let member = self.eat_ident()?;
                    let alias = if self.peek()?.ty() == TokenType::As {
                        self.eat(TokenType::As)?;

                        Some(self.eat_ident()?)
                    } else {
                        None
                    };

                    members.push((member, alias));

                    // TODO: Helpful error if they terminated it too soon
                    if self.peek()?.ty() == TokenType::Comma {
                        self.eat(TokenType::Comma)?;
                    } else {
                        break;
                    }
                }

                ImportExposure::Members(members)
            }
        } else {
            let alias = if self.peek()?.ty() == TokenType::As {
                self.eat(TokenType::As)?;

                self.eat_ident()?
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

                self.intern_string(&last_segment)
            };

            ImportExposure::None(alias)
        };

        self.eat(TokenType::Newline)?;

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

        self.eat(TokenType::Trait)?;
        let name = self.eat_ident()?;
        let generics = self.generics()?;
        self.eat(TokenType::Newline)?;

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
                    self.eat(TokenType::Newline)?;
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

        self.eat(TokenType::End)?;

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

        self.eat(TokenType::Enum)?;
        let name = self.eat_ident()?;
        let generics = self.generics()?;
        self.eat(TokenType::Newline)?;

        let mut variant_decorators = Vec::with_capacity(7);
        let mut variants = Vec::with_capacity(7);
        while self.peek()?.ty() != TokenType::End {
            match self.peek()?.ty() {
                TokenType::AtSign => {
                    self.decorator(&mut variant_decorators)?;
                }

                TokenType::Ident => {
                    let name = self.eat_ident()?;

                    if self.peek()?.ty() == TokenType::LeftParen {
                        self.eat(TokenType::LeftParen)?;

                        let mut elements = Vec::with_capacity(3);
                        while self.peek()?.ty() != TokenType::RightParen {
                            let ty = self.eat_type()?;
                            elements.push(ty);

                            // TODO: Nice error here
                            if self.peek()?.ty() == TokenType::Comma {
                                self.eat(TokenType::Comma)?;
                            } else {
                                break;
                            }
                        }
                        self.eat(TokenType::RightParen)?;

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

                    self.eat(TokenType::Newline)?;
                }

                TokenType::Newline => {
                    self.eat(TokenType::Newline)?;
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
        self.eat(TokenType::End)?;

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

        self.eat(TokenType::AtSign)?;
        let name = self.eat_ident()?;

        let args = if self.peek()?.ty() == TokenType::LeftParen {
            self.eat(TokenType::LeftParen)?;

            let mut args = Vec::with_capacity(5);
            while self.peek()?.ty() != TokenType::RightParen {
                let expr = self.expr()?;
                args.push(expr);

                if let Ok(peek) = self.peek() {
                    if peek.ty() == TokenType::Comma {
                        self.eat(TokenType::Comma)?;
                        continue;
                    }
                }

                break;
            }
            args.shrink_to_fit();
            self.eat(TokenType::RightParen)?;

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

        self.eat(TokenType::Type)?;
        let name = self.eat_ident()?;
        let generics = self.generics()?;
        self.eat(TokenType::Newline)?;

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
                    let name = self.eat_ident()?;

                    let ty = if self.peek()?.ty() == TokenType::Colon {
                        self.eat(TokenType::Colon)?;
                        self.eat_type()?
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
                    self.eat(TokenType::Newline)?;

                    members.push(member);
                }

                TokenType::Newline => {
                    self.eat(TokenType::Newline)?;
                }

                ty => {
                    return Err(Locatable::new(
                        Error::Syntax(SyntaxError::InvalidTopLevel(ty)),
                        Location::new(&self.peek()?, self.current_file),
                    ));
                }
            }
        }
        self.eat(TokenType::End)?;

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

        self.eat(TokenType::Function)?;
        let name = self.eat_ident()?;
        let generics = self.generics()?;
        let args = self.function_args()?;

        let returns = if self.peek()?.ty() == TokenType::RightArrow {
            self.eat(TokenType::RightArrow)?;
            self.eat_type()?
        } else {
            Type::default()
        };
        self.eat(TokenType::Newline)?;

        let mut body = Vec::with_capacity(20);
        while self.peek()?.ty() != TokenType::End {
            if let Some(stmt) = self.stmt()? {
                body.push(stmt);
            }
        }
        self.eat(TokenType::End)?;
        body.shrink_to_fit();

        Ok(Ast::Function {
            decorators,
            attributes,
            name,
            generics,
            args,
            returns,
            body,
        })
    }

    fn function_args(&mut self) -> ParseResult<Vec<(Cord, Type)>> {
        let _frame = self.add_stack_frame()?;
        self.eat(TokenType::LeftParen)?;

        let mut args = Vec::with_capacity(7);
        while self.peek()?.ty() != TokenType::RightParen {
            let ident = self.eat_ident()?;

            let ty = if self.peek()?.ty() == TokenType::Colon {
                self.eat(TokenType::Colon)?;
                self.eat_type()?
            } else {
                Type::default()
            };
            args.push((ident, ty));

            if self.peek()?.ty() == TokenType::Comma {
                self.eat(TokenType::Comma)?;
            } else {
                // TODO: Check if next is a `>` and if so emit a helpful error
                break;
            }
        }
        self.eat(TokenType::RightParen)?;
        args.shrink_to_fit();

        Ok(args)
    }

    fn eat_type(&mut self) -> ParseResult<Type> {
        let _frame = self.add_stack_frame()?;
        let ty = Type::try_from((
            self.eat(TokenType::Ident)?,
            self.current_file,
            &self.string_interner,
        ))?;

        Ok(ty)
    }

    fn generics(&mut self) -> ParseResult<Vec<Cord>> {
        let _frame = self.add_stack_frame()?;

        if self.peek()?.ty() == TokenType::LeftCaret {
            self.eat(TokenType::LeftCaret)?;

            let mut generics = Vec::with_capacity(5);
            while self.peek()?.ty() != TokenType::RightCaret {
                generics.push(self.eat_ident()?);

                if self.peek()?.ty() == TokenType::Comma {
                    self.eat(TokenType::Comma)?;
                } else {
                    // TODO: Check if next is a `>` and if so emit a helpful error
                    break;
                }
            }
            self.eat(TokenType::RightCaret)?;
            generics.shrink_to_fit();

            Ok(generics)
        } else {
            Ok(Vec::new())
        }
    }
}
