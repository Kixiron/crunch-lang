use crate::{
    error::{Error, Locatable, Location, ParseResult, Span, SyntaxError},
    interner::Interner,
    parser::{CurrentFile, Expr, Literal, Parser, Stmt},
    symbol_table::{Symbol, SymbolLocation},
    token::{Token, TokenType},
};

use lasso::SmallSpur;

use alloc::{boxed::Box, format, string::ToString, vec::Vec};
use core::{convert::TryFrom, fmt, mem};

// TODO: Rename `comptime` const
// TODO: Const blocks
// TODO: Remove types-by-value, add back generics to funcs

#[derive(Debug, Clone, PartialEq)]
pub struct Function<'expr, 'stmt> {
    pub decorators: Vec<Decorator<'expr>>,
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
pub struct TypeDecl<'expr, 'stmt> {
    pub decorators: Vec<Decorator<'expr>>,
    pub attrs: Vec<Locatable<Attribute>>,
    pub name: SmallSpur,
    pub generics: Vec<Locatable<Type>>,
    pub members: Vec<Locatable<TypeMember<'expr>>>,
    pub methods: Vec<Locatable<Function<'expr, 'stmt>>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeMember<'expr> {
    pub decorators: Vec<Decorator<'expr>>,
    pub attrs: Vec<Locatable<Attribute>>,
    pub name: SmallSpur,
    pub ty: Locatable<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Enum<'expr> {
    pub decorators: Vec<Decorator<'expr>>,
    pub attrs: Vec<Locatable<Attribute>>,
    pub name: SmallSpur,
    pub generics: Vec<Locatable<Type>>,
    pub variants: Vec<EnumVariant<'expr>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum EnumVariant<'expr> {
    Unit {
        name: SmallSpur,
        decorators: Vec<Decorator<'expr>>,
    },
    Tuple {
        name: SmallSpur,
        elements: Vec<Locatable<Type>>,
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
pub struct Trait<'expr, 'stmt> {
    pub decorators: Vec<Decorator<'expr>>,
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
pub enum Ast<'expr, 'stmt> {
    Function(Locatable<Function<'expr, 'stmt>>),
    Type(Locatable<TypeDecl<'expr, 'stmt>>),
    Enum(Locatable<Enum<'expr>>),
    Trait(Locatable<Trait<'expr, 'stmt>>),
    Import(Locatable<Import>),
}

impl<'expr, 'stmt> Ast<'expr, 'stmt> {
    pub fn name(&self) -> SmallSpur {
        match self {
            Self::Function(func) => func.data().name,
            Self::Type(ty) => ty.data().name,
            Self::Enum(e) => e.data().name,
            Self::Trait(tr) => tr.data().name,
            Self::Import(import) => *import.data().file.data(),
        }
    }

    pub fn type_of(node: &Self) -> AstType {
        match node {
            Self::Function { .. } => AstType::Function,
            Self::Type { .. } => AstType::Type,
            Self::Enum { .. } => AstType::Enum,
            Self::Trait { .. } => AstType::Trait,
            Self::Import { .. } => AstType::Import,
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
        }
    }
}

pub enum AstType {
    Function,
    Type,
    Enum,
    Trait,
    Import,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Builtin(BuiltinType),
    TraitObj(Vec<Locatable<Type>>),
    Bounded {
        ty: SmallSpur,
        bounds: Vec<Locatable<Type>>,
    },
    Custom(SmallSpur),
    Infer,
}

impl<'a> Type {
    pub(crate) fn push(&mut self, ty: Locatable<Type>) {
        match self {
            Self::TraitObj(vec) => vec.push(ty),
            Self::Bounded { bounds, .. } => bounds.push(ty),
            Self::Builtin(BuiltinType::Array(val)) => **val = ty,
            Self::Builtin(BuiltinType::Tuple(types)) => types.push(ty),

            _ => unreachable!("Internal error, attempted to push to unpushable type"),
        }
    }

    pub fn to_string(&self, interner: &Interner) -> String {
        match self {
            Self::Builtin(builtin) => builtin.to_string(interner),
            Self::TraitObj(types) => format!(
                "type[{}]",
                types
                    .iter()
                    .map(|ty| ty.data().to_string(interner))
                    .collect::<Vec<_>>()
                    .join(", "),
            ),
            Self::Bounded { ty, bounds } => format!(
                "{}[{}]",
                interner.resolve(ty),
                bounds
                    .iter()
                    .map(|ty| ty.data().to_string(interner))
                    .collect::<Vec<_>>()
                    .join(", "),
            ),
            Self::Custom(ty) => interner.resolve(ty).to_owned(),
            Self::Infer => "infer".to_string(),
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
    Array(Box<Locatable<Type>>),
    Tuple(Vec<Locatable<Type>>),
}

impl BuiltinType {
    pub fn to_string(&self, interner: &Interner) -> String {
        match self {
            Self::Integer { sign, width } => format!("{}{}", sign, width),
            Self::IntReg(sign) => format!("{}reg", sign),
            Self::IntPtr(sign) => format!("{}ptr", sign),
            Self::Float { width } => format!("f{}", width),
            Self::Boolean => "bool".to_string(),
            Self::String => "str".to_string(),
            Self::Rune => "rune".to_string(),
            Self::Unit => "unit".to_string(),
            Self::Absurd => "absurd".to_string(),
            Self::Array(arr) => format!("[{}]", arr.data().to_string(interner)),
            Self::Tuple(types) => format!(
                "({})",
                types
                    .iter()
                    .map(|ty| ty.data().to_string(interner))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Signedness {
    Unsigned,
    Signed,
}

impl fmt::Display for Signedness {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Unsigned => write!(f, "u"),
            Self::Signed => write!(f, "i"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ImportExposure {
    None(SmallSpur),
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
    pub name: SmallSpur,
    pub args: Vec<Expr<'expr>>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
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

    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Visibility(vis) => vis.as_str(),
            Self::Comptime => "comptime",
        }
    }
}

impl<'a> TryFrom<(&Token<'a>, CurrentFile)> for Attribute {
    type Error = Locatable<Error>;

    fn try_from((token, file): (&Token<'a>, CurrentFile)) -> Result<Self, Self::Error> {
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
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::FileLocal => "file",
            Self::Package => "pkg",
            Self::Exposed => "exposed",
        }
    }
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
        attributes: &mut Vec<Locatable<Attribute>>,
    ) -> ParseResult<Option<Ast<'expr, 'stmt>>> {
        let _frame = self.add_stack_frame()?;

        let peek = self.peek()?;
        let node = match peek.ty() {
            TokenType::AtSign => {
                self.decorator(decorators)?;

                Ok(None)
            }

            TokenType::Exposed | TokenType::Package | TokenType::Comptime => {
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
                        Location::concrete(&self.peek()?, self.current_file),
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
                Location::concrete(&self.peek()?, self.current_file),
            )),
        };

        if let Ok(Some(ref node)) = node {
            self.symbol_table.insert(
                &self.string_interner,
                SymbolLocation::new(node.location(), node.name()),
                Symbol::from(node),
            )?;
        }

        node
    }

    fn import(&mut self, mut decorators: Vec<Decorator<'expr>>) -> ParseResult<Ast<'expr, 'stmt>> {
        let _frame = self.add_stack_frame()?;
        decorators.shrink_to_fit();

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
                self.string_interner.intern(ident.source())
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

                self.string_interner.intern(&last_segment)
            };

            ImportExposure::None(alias)
        };

        let end_span = self.eat(TokenType::Newline, [])?.span();
        let import = Import {
            file,
            dest,
            exposes,
        };

        Ok(Ast::Import(Locatable::new(
            import,
            Location::concrete(Span::merge(start_span, end_span), self.current_file),
        )))
    }

    fn trait_decl(
        &mut self,
        mut decorators: Vec<Decorator<'expr>>,
        mut attrs: Vec<Locatable<Attribute>>,
    ) -> ParseResult<Ast<'expr, 'stmt>> {
        let _frame = self.add_stack_frame()?;
        decorators.shrink_to_fit();
        attrs.shrink_to_fit();

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
                    method_attributes.shrink_to_fit();
                    method_decorators.shrink_to_fit();

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
        methods.shrink_to_fit();
        let end_span = self.eat(TokenType::End, [TokenType::Newline])?.span();

        if !attrs.iter().any(|attr| attr.data().is_visibility()) {
            attrs.reserve(1);
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

    fn enum_decl(
        &mut self,
        mut decorators: Vec<Decorator<'expr>>,
        mut attrs: Vec<Locatable<Attribute>>,
    ) -> ParseResult<Ast<'expr, 'stmt>> {
        let _frame = self.add_stack_frame()?;
        decorators.shrink_to_fit();
        attrs.shrink_to_fit();

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
                        Location::concrete(&self.peek()?, self.current_file),
                    ));
                }
            }
        }
        variants.shrink_to_fit();
        let end_span = self.eat(TokenType::End, [TokenType::Newline])?.span();

        if !attrs.iter().any(|attr| attr.data().is_visibility()) {
            attrs.reserve(1);
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
        mut attrs: Vec<Locatable<Attribute>>,
    ) -> ParseResult<Ast<'expr, 'stmt>> {
        let _frame = self.add_stack_frame()?;
        decorators.shrink_to_fit();
        attrs.shrink_to_fit();

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
        let mut methods = Vec::with_capacity(5);

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

                TokenType::Function => {
                    let method = self.function(
                        mem::take(&mut member_decorators),
                        mem::take(&mut member_attrs),
                    )?;

                    if let Ast::Function(method) = method {
                        methods.push(method);
                    } else {
                        unreachable!("Something really weird happened")
                    }
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
                    member_attrs.shrink_to_fit();
                    member_decorators.shrink_to_fit();

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
            attrs.reserve(1);
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
            methods,
        };

        Ok(Ast::Type(Locatable::new(
            type_decl,
            Location::concrete(Span::merge(start_span, end_span), self.current_file),
        )))
    }

    fn function(
        &mut self,
        mut decorators: Vec<Decorator<'expr>>,
        mut attrs: Vec<Locatable<Attribute>>,
    ) -> ParseResult<Ast<'expr, 'stmt>> {
        let _frame = self.add_stack_frame()?;
        decorators.shrink_to_fit();
        attrs.shrink_to_fit();

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
        body.shrink_to_fit();

        if !attrs.iter().any(|attr| attr.data().is_visibility()) {
            attrs.reserve(1);
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

    fn function_args(&mut self) -> ParseResult<Vec<Locatable<FuncArg>>> {
        let _frame = self.add_stack_frame()?;
        self.eat(TokenType::LeftParen, [TokenType::Newline])?;

        let mut args = Vec::with_capacity(7);
        while self.peek()?.ty() != TokenType::RightParen {
            let (comptime, name, name_span) = match self.eat_of(
                [TokenType::Ident, TokenType::Comptime],
                [TokenType::Newline],
            )? {
                ident if ident.ty() == TokenType::Ident => (
                    false,
                    Locatable::new(
                        self.string_interner.intern(ident.source()),
                        Location::concrete(ident.span(), self.current_file),
                    ),
                    ident.span(),
                ),

                token if token.ty() == TokenType::Comptime => {
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
        args.shrink_to_fit();

        Ok(args)
    }

    fn generics(&mut self) -> ParseResult<Vec<Locatable<Type>>> {
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

    pub(super) fn ascribed_type(&mut self) -> ParseResult<Locatable<Type>> {
        let _frame = self.add_stack_frame()?;

        let token = self.eat_of([TokenType::Type, TokenType::Ident], [TokenType::Newline])?;
        let start_span = token.span();

        let (mut ty, mut end_span) = match token {
            Token { source, .. } if source == "str" => {
                return Ok(Locatable::new(
                    Type::Builtin(BuiltinType::String),
                    Location::concrete(start_span, self.current_file),
                ));
            }

            Token { source, .. } if source == "rune" => {
                return Ok(Locatable::new(
                    Type::Builtin(BuiltinType::Rune),
                    Location::concrete(start_span, self.current_file),
                ));
            }

            Token { source, .. } if source == "bool" => {
                return Ok(Locatable::new(
                    Type::Builtin(BuiltinType::Boolean),
                    Location::concrete(start_span, self.current_file),
                ));
            }

            Token { source, .. } if source == "unit" => {
                return Ok(Locatable::new(
                    Type::Builtin(BuiltinType::Unit),
                    Location::concrete(start_span, self.current_file),
                ));
            }

            Token { source, .. } if source == "absurd" => {
                return Ok(Locatable::new(
                    Type::Builtin(BuiltinType::Absurd),
                    Location::concrete(start_span, self.current_file),
                ));
            }

            Token { source, .. } if source == "ureg" || source == "ireg" => {
                let sign = if source.chars().next().unwrap() == 'u' {
                    Signedness::Unsigned
                } else {
                    Signedness::Signed
                };

                return Ok(Locatable::new(
                    Type::Builtin(BuiltinType::IntReg(sign)),
                    Location::concrete(start_span, self.current_file),
                ));
            }

            Token { source, .. } if source == "uptr" || source == "iptr" => {
                let sign = if source.chars().next().unwrap() == 'u' {
                    Signedness::Unsigned
                } else {
                    Signedness::Signed
                };

                return Ok(Locatable::new(
                    Type::Builtin(BuiltinType::IntPtr(sign)),
                    Location::concrete(start_span, self.current_file),
                ));
            }

            Token { source, .. }
                if source.starts_with('u') && source.chars().skip(1).all(|c| c.is_numeric()) =>
            {
                let width: u16 = source.chars().skip(1).collect::<String>().parse().unwrap();
                let int = Type::Builtin(BuiltinType::Integer {
                    sign: Signedness::Unsigned,
                    width,
                });

                return Ok(Locatable::new(
                    int,
                    Location::concrete(start_span, self.current_file),
                ));
            }

            Token { source, .. }
                if source.starts_with('i') && source.chars().skip(1).all(|c| c.is_numeric()) =>
            {
                let width: u16 = source.chars().skip(1).collect::<String>().parse().unwrap();

                return Ok(Locatable::new(
                    Type::Builtin(BuiltinType::Integer {
                        sign: Signedness::Signed,
                        width,
                    }),
                    Location::concrete(start_span, self.current_file),
                ));
            }

            Token { source, .. }
                if source.starts_with('f') && source.chars().skip(1).all(|c| c.is_numeric()) =>
            {
                let width: u16 = source.chars().skip(1).collect::<String>().parse().unwrap();

                return Ok(Locatable::new(
                    Type::Builtin(BuiltinType::Float { width }),
                    Location::concrete(start_span, self.current_file),
                ));
            }

            Token { source, span, .. } if source == "arr" => {
                // TODO: Maybe parse this immediately?
                (
                    Type::Builtin(BuiltinType::Array(Box::new(Locatable::new(
                        Type::Infer,
                        Location::implicit(span, self.current_file),
                    )))),
                    span,
                )
            }

            Token { source, span, .. } if source == "tup" => {
                (Type::Builtin(BuiltinType::Tuple(Vec::new())), span)
            }

            Token { ty, span, .. } if ty == TokenType::Type => (Type::TraitObj(Vec::new()), span),

            Token { ty, source, span }
                if ty == TokenType::Ident
                    && self.peek().map(|t| t.ty()) == Ok(TokenType::LeftBrace) =>
            {
                (
                    Type::Bounded {
                        ty: self.string_interner.intern(source),
                        bounds: Vec::new(),
                    },
                    span,
                )
            }

            Token { ty, source, span } if ty == TokenType::Ident => {
                return Ok(Locatable::new(
                    Type::Custom(self.string_interner.intern(source)),
                    Location::concrete(span, self.current_file),
                ));
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

            end_span = self.eat(TokenType::RightBrace, [])?.span();
        }

        if let Type::Bounded { ty, bounds } = ty {
            let ty = if bounds.is_empty() {
                Type::Custom(ty)
            } else {
                Type::Bounded { ty, bounds }
            };

            Ok(Locatable::new(
                ty,
                Location::concrete(Span::merge(start_span, end_span), self.current_file),
            ))
        } else {
            Ok(Locatable::new(
                ty,
                Location::concrete(Span::merge(start_span, end_span), self.current_file),
            ))
        }
    }
}
