use super::*;

use alloc::{boxed::Box, string::ToString, vec::Vec};
use core::mem;

#[derive(Debug, Clone, PartialEq)]
pub enum Ast {
    Function {
        decorators: Vec<Decorator>,
        attributes: Vec<Attribute>,
        name: Sym,
        generics: Vec<Sym>,
        args: Vec<(Sym, Type)>,
        returns: Type,
        body: Vec<Statement>,
    },

    Type {
        decorators: Vec<Decorator>,
        attributes: Vec<Attribute>,
        name: Sym,
        generics: Vec<Sym>,
        members: Vec<TypeMember>,
        methods: Vec<Ast>,
    },

    Enum {
        decorators: Vec<Decorator>,
        attributes: Vec<Attribute>,
        name: Sym,
        generics: Vec<Sym>,
        variants: Vec<EnumVariant>,
    },

    Trait {
        decorators: Vec<Decorator>,
        attributes: Vec<Attribute>,
        name: Sym,
        generics: Vec<Sym>,
        methods: Vec<Ast>,
    },

    Import {
        file: Sym,
        dest: ImportDest,
        exposes: ImportExposure,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Builtin(BuiltinType),
    Custom(Sym),
    Infer,
}

impl<'a> TryFrom<(Token<'a>, usize, &Interner)> for Type {
    type Error = Vec<Diagnostic<usize>>;

    fn try_from(
        (token, file, interner): (Token<'a>, usize, &Interner),
    ) -> Result<Self, Self::Error> {
        if token.ty() == TokenType::Ident {
            if let Ok(ty) = BuiltinType::try_from((token, file, interner)) {
                Ok(Self::Builtin(ty))
            } else {
                Ok(Self::Custom(super::intern_string(interner, token.source())))
            }
        } else {
            Err(vec![Diagnostic::error().with_message(format!(
                "Expected an ident for a type name, got `{}`",
                token.ty()
            ))])
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
    Vec(Box<Type>),
}

impl<'a> TryFrom<(Token<'a>, usize, &Interner)> for BuiltinType {
    type Error = Vec<Diagnostic<usize>>;

    fn try_from(
        (mut token, file, interner): (Token<'a>, usize, &Interner),
    ) -> Result<Self, Self::Error> {
        Ok(match token.source() {
            "str" => Self::String,
            "int" => Self::Integer,
            "float" => Self::Float,
            "bool" => Self::Boolean,
            ty if ty.starts_with("[") && ty.ends_with("]") => {
                token.source = &token.source()[1..ty.len() - 1];

                if token.source() == "" {
                    Self::Vec(Box::new(Type::Infer))
                } else {
                    Self::Vec(Box::new(Type::try_from((token, file, interner))?))
                }
            }

            ty => {
                return Err(vec![Diagnostic::error().with_message(format!(
                    "Expected an ident for type name, got `{}`",
                    ty
                ))])
            }
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ImportExposure {
    None(Sym),
    All,
    Members(Vec<(Sym, Option<Sym>)>),
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
pub enum EnumVariant {
    Unit {
        name: Sym,
        decorators: Vec<Decorator>,
    },
    Tuple {
        name: Sym,
        elements: Vec<Type>,
        decorators: Vec<Decorator>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeMember {
    pub decorators: Vec<Decorator>,
    pub attributes: Vec<Attribute>,
    pub name: Sym,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Decorator {
    pub name: Sym,
    pub args: Vec<Expression>,
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

impl TryFrom<TokenType> for Attribute {
    type Error = Vec<Diagnostic<usize>>;

    fn try_from(ty: TokenType) -> Result<Self, Self::Error> {
        Ok(match ty {
            TokenType::Exposed => Self::Visibility(Visibility::Exposed),
            TokenType::Package => Self::Visibility(Visibility::Package),

            ty => {
                return Err(vec![Diagnostic::error()
                    .with_message(format!("Expected an attribute, got `{}`", ty))]);
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

impl<'a> Parser<'a> {
    pub(super) fn ast(&mut self) -> ParseResult<Ast> {
        let (mut decorators, mut attributes) = (Vec::with_capacity(5), Vec::with_capacity(5));
        let mut diagnostics = Vec::with_capacity(10);

        while self.peek().is_ok() {
            match self.ast_impl(&mut decorators, &mut attributes)? {
                (Some(node), diag) => {
                    diagnostics.extend_from_slice(&diag);
                    return Ok((node, diagnostics));
                }
                (None, diag) => diagnostics.extend_from_slice(&diag),
            }
        }

        diagnostics.push(Diagnostic::error().with_message("Unexpected EOF"));
        Err(diagnostics)
    }

    // Returns None when the function should be re-called, usually because an attribute or decorator was parsed
    fn ast_impl(
        &mut self,
        decorators: &mut Vec<Decorator>,
        attributes: &mut Vec<Attribute>,
    ) -> ParseResult<Option<Ast>> {
        match self.peek()?.ty() {
            TokenType::AtSign => {
                let (_, diag) = self.decorator(decorators)?;

                Ok((None, diag))
            }

            TokenType::Exposed | TokenType::Package => {
                let attr = Attribute::try_from(self.next()?.ty())?;
                attributes.push(attr);

                Ok((None, Vec::new()))
            }

            TokenType::Function => {
                let (func, diag) = self.function(mem::take(decorators), mem::take(attributes))?;
                Ok((Some(func), diag))
            },

            TokenType::Type => {
                let (ty, diag) = self.type_decl(mem::take(decorators), mem::take(attributes))?;
                Ok((Some(
                ty
            ), diag))
            },

            TokenType::Enum => {
                let (enu, diag) = self.enum_decl(mem::take(decorators), mem::take(attributes))?;
                Ok((Some(
                enu
            ), diag))}

            TokenType::Trait => {
                let (tra, diag) = self.trait_decl(mem::take(decorators), mem::take(attributes))?;
                Ok((Some(tra), diag))
            },

            TokenType::Import => {
                if attributes.len() != 0 {
                    return Err(vec![Diagnostic::error()
                        .with_message("Import declarations cannot have attributes")]);
                }

                let (import, diag) = self.import(mem::take(decorators))?;

                Ok((Some(import), diag))
            }

            TokenType::Newline => {
                self.eat(TokenType::Newline)?;
                Ok((None, Vec::new()))
            }

            ty => Err(vec![
                Diagnostic::error().with_message(format!(
                    "Expected a function, type, enum, trait, import, decorator or attribute, got a `{}`", 
                    ty,
                ))]),
        }
    }

    fn import(&mut self, mut decorators: Vec<Decorator>) -> ParseResult<Ast> {
        decorators.shrink_to_fit();

        self.eat(TokenType::Import)?;

        let file = if let Literal::String(string) =
            Literal::try_from(&self.eat(TokenType::String)?)?
        {
            self.intern_string(&string)
        } else {
            unreachable!("Ate a string token, the only literal type produced should be a String")
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
                    .read()
                    .resolve(file)
                    .expect("Shouldn't have a resolution problem, just interned the file's path")
                    .split('.')
                    .last()
                    .ok_or(vec![
                        Diagnostic::error().with_message("You must declare a file to import from")
                    ])?
                    .to_string();

                self.intern_string(&last_segment)
            };

            ImportExposure::None(alias)
        };

        self.eat(TokenType::Newline)?;

        Ok((
            Ast::Import {
                file,
                dest,
                exposes,
            },
            Vec::new(),
        ))
    }

    fn trait_decl(
        &mut self,
        mut decorators: Vec<Decorator>,
        mut attributes: Vec<Attribute>,
    ) -> ParseResult<Ast> {
        decorators.shrink_to_fit();
        attributes.shrink_to_fit();

        if !attributes.iter().any(Attribute::is_visibility) {
            attributes.reserve(1);
            attributes.push(Attribute::Visibility(Visibility::FileLocal));
        }

        self.eat(TokenType::Trait)?;
        let name = self.eat_ident()?;
        let (generics, mut diagnostics) = self.generics()?;
        self.eat(TokenType::Newline)?;

        let (mut method_decorators, mut method_attributes) =
            (Vec::with_capacity(3), Vec::with_capacity(3));

        let mut methods = Vec::with_capacity(4);
        while self.peek()?.ty() != TokenType::End {
            match self.peek()?.ty() {
                TokenType::AtSign => {
                    let (_, diag) = self.decorator(&mut method_decorators)?;
                    diagnostics.extend_from_slice(&diag);
                }

                TokenType::Exposed | TokenType::Package => {
                    method_attributes.push(Attribute::try_from(self.next()?.ty())?);
                }

                TokenType::Function => {
                    if !method_attributes.iter().any(Attribute::is_visibility) {
                        method_attributes.push(Attribute::Visibility(Visibility::FileLocal));
                    }
                    method_attributes.shrink_to_fit();
                    method_decorators.shrink_to_fit();

                    let (method, diag) = self.function(
                        mem::take(&mut method_decorators),
                        mem::take(&mut method_attributes),
                    )?;

                    diagnostics.extend_from_slice(&diag);
                    methods.push(method);
                }

                TokenType::Newline => {
                    self.eat(TokenType::Newline)?;
                }

                _ => {
                    diagnostics.push(Diagnostic::error().with_message(format!(
                        "Only methods, attributes and decorators are allowed inside trait bodies"
                    )));
                    return Err(diagnostics);
                }
            }
        }
        methods.shrink_to_fit();

        self.eat(TokenType::End)?;

        Ok((
            Ast::Trait {
                decorators,
                attributes,
                name,
                generics,
                methods,
            },
            diagnostics,
        ))
    }

    fn enum_decl(
        &mut self,
        mut decorators: Vec<Decorator>,
        mut attributes: Vec<Attribute>,
    ) -> ParseResult<Ast> {
        decorators.shrink_to_fit();
        attributes.shrink_to_fit();

        if !attributes.iter().any(Attribute::is_visibility) {
            attributes.reserve(1);
            attributes.push(Attribute::Visibility(Visibility::FileLocal));
        }

        self.eat(TokenType::Enum)?;
        let name = self.eat_ident()?;
        let (generics, mut diagnostics) = self.generics()?;
        self.eat(TokenType::Newline)?;

        let mut variant_decorators = Vec::with_capacity(7);
        let mut variants = Vec::with_capacity(7);
        while self.peek()?.ty() != TokenType::End {
            match self.peek()?.ty() {
                TokenType::AtSign => {
                    let (_, diag) = self.decorator(&mut variant_decorators)?;
                    diagnostics.extend_from_slice(&diag);
                }

                TokenType::Ident => {
                    let name = self.eat_ident()?;

                    if self.peek()?.ty() == TokenType::LeftParen {
                        self.eat(TokenType::LeftParen)?;

                        let mut elements = Vec::with_capacity(3);
                        while self.peek()?.ty() != TokenType::RightParen {
                            let (ty, diag) = self.eat_type()?;
                            elements.push(ty);
                            diagnostics.extend_from_slice(&diag);

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
                    diagnostics.push(Diagnostic::error().with_message(format!(
                        "Only decorators and enum variants are allowed inside enum declarations, got a `{}`", 
                        ty,
                    )));
                    return Err(diagnostics);
                }
            }
        }
        variants.shrink_to_fit();
        self.eat(TokenType::End)?;

        Ok((
            Ast::Enum {
                decorators,
                attributes,
                name,
                generics,
                variants,
            },
            diagnostics,
        ))
    }

    fn decorator(&mut self, decorators: &mut Vec<Decorator>) -> ParseResult<()> {
        let mut diagnostics = Vec::new();

        self.eat(TokenType::AtSign)?;
        let name = self.eat_ident()?;

        let args = if self.peek()?.ty() == TokenType::LeftParen {
            self.eat(TokenType::LeftParen)?;

            let mut args = Vec::with_capacity(5);
            while self.peek()?.ty() != TokenType::RightParen {
                let (expr, diag) = self.expr()?;
                args.push(expr);
                diagnostics.extend_from_slice(&diag);

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

        Ok(((), diagnostics))
    }

    fn type_decl(
        &mut self,
        mut decorators: Vec<Decorator>,
        mut attributes: Vec<Attribute>,
    ) -> ParseResult<Ast> {
        decorators.shrink_to_fit();
        attributes.shrink_to_fit();

        if !attributes.iter().any(Attribute::is_visibility) {
            attributes.reserve(1);
            attributes.push(Attribute::Visibility(Visibility::FileLocal));
        }

        self.eat(TokenType::Type)?;
        let name = self.eat_ident()?;
        let (generics, mut diagnostics) = self.generics()?;
        self.eat(TokenType::Newline)?;

        let (mut member_decorators, mut member_attributes) =
            (Vec::with_capacity(3), Vec::with_capacity(3));

        let mut members = Vec::with_capacity(5);
        let mut methods = Vec::with_capacity(5);

        while self.peek()?.ty() != TokenType::End {
            match self.peek()?.ty() {
                TokenType::AtSign => {
                    let (_, diag) = self.decorator(&mut member_decorators)?;
                    diagnostics.extend_from_slice(&diag);
                }

                TokenType::Exposed | TokenType::Package => {
                    member_attributes.push(Attribute::try_from(self.next()?.ty())?);
                }

                TokenType::Function => {
                    let (method, diag) = self.function(
                        mem::take(&mut member_decorators),
                        mem::take(&mut member_attributes),
                    )?;
                    methods.push(method);
                    diagnostics.extend_from_slice(&diag);
                }

                TokenType::Ident => {
                    let mut diagnostics = Vec::new();
                    let name = self.eat_ident()?;

                    let ty = if self.peek()?.ty() == TokenType::Colon {
                        self.eat(TokenType::Colon)?;
                        let (ty, diag) = self.eat_type()?;
                        diagnostics.extend_from_slice(&diag);

                        ty
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

                _ => {
                    diagnostics.push(Diagnostic::error().with_message(
                        "You can only have members and methods inside of type declarations",
                    ));
                    return Err(diagnostics);
                }
            }
        }
        self.eat(TokenType::End)?;

        if member_attributes.len() != 0 || member_decorators.len() != 0 {
            diagnostics.push(Diagnostic::error().with_message(
                "Attributes and functions must be before members or methods in type declarations",
            ));
            return Err(diagnostics);
        }

        Ok((
            Ast::Type {
                decorators,
                attributes,
                name,
                generics,
                members,
                methods,
            },
            diagnostics,
        ))
    }

    fn function(
        &mut self,
        mut decorators: Vec<Decorator>,
        mut attributes: Vec<Attribute>,
    ) -> ParseResult<Ast> {
        decorators.shrink_to_fit();
        attributes.shrink_to_fit();

        if !attributes.iter().any(Attribute::is_visibility) {
            attributes.reserve(1);
            attributes.push(Attribute::Visibility(Visibility::FileLocal));
        }

        self.eat(TokenType::Function)?;
        let name = self.eat_ident()?;
        let (generics, mut diagnostics) = self.generics()?;
        let (args, diag) = self.function_args()?;
        diagnostics.extend_from_slice(&diag);

        let returns = if self.peek()?.ty() == TokenType::RightArrow {
            self.eat(TokenType::RightArrow)?;

            let (ty, diag) = self.eat_type()?;
            diagnostics.extend_from_slice(&diag);

            ty
        } else {
            Type::default()
        };
        self.eat(TokenType::Newline)?;

        let mut body = Vec::with_capacity(20);
        while self.peek()?.ty() != TokenType::End {
            let (stmt, diag) = self.stmt()?;
            body.push(stmt);
            diagnostics.extend_from_slice(&diag);
        }
        self.eat(TokenType::End)?;
        body.shrink_to_fit();

        Ok((
            Ast::Function {
                decorators,
                attributes,
                name,
                generics,
                args,
                returns,
                body,
            },
            diagnostics,
        ))
    }

    fn function_args(&mut self) -> ParseResult<Vec<(Sym, Type)>> {
        let mut diagnostics = Vec::new();
        self.eat(TokenType::LeftParen)?;

        let mut args = Vec::with_capacity(7);
        while self.peek()?.ty() != TokenType::RightParen {
            let ident = self.eat_ident()?;

            let ty = if self.peek()?.ty() == TokenType::Colon {
                self.eat(TokenType::Colon)?;

                let (ty, diag) = self.eat_type()?;
                diagnostics.extend_from_slice(&diag);

                ty
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

        Ok((args, diagnostics))
    }

    fn eat_type(&mut self) -> ParseResult<Type> {
        let ty = Type::try_from((
            self.eat(TokenType::Ident)?,
            self.current_file,
            &self.string_interner,
        ))?;

        Ok((ty, Vec::new()))
    }

    fn generics(&mut self) -> ParseResult<Vec<Sym>> {
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

            Ok((generics, Vec::new()))
        } else {
            Ok((Vec::new(), Vec::new()))
        }
    }
}
