use crate::{
    context::{Context, StrT},
    error::{Error, Locatable, Location, ParseResult, Span, SyntaxError},
    parser::{CurrentFile, Integer, ItemPath, Literal, Parser},
    token::{Token, TokenType},
};
use alloc::{
    format,
    string::{String, ToString},
    vec,
    vec::Vec,
};
use core::{convert::TryFrom, fmt};
use crunch_proc::recursion_guard;
#[cfg(test)]
use serde::{Deserialize, Serialize};

type PrefixParselet<'src, 'ctx> =
    fn(&'ctx mut Parser<'src, 'ctx>, Token<'src>) -> ParseResult<Locatable<&'ctx Type<'ctx>>>;

type PostfixParselet<'src, 'ctx> = fn(
    &'ctx mut Parser<'src, 'ctx>,
    Token<'src>,
    Locatable<&'ctx Type<'ctx>>,
) -> ParseResult<Locatable<&'ctx Type<'ctx>>>;

type InfixParselet<'src, 'ctx> = fn(
    &'ctx mut Parser<'src, 'ctx>,
    Token<'src>,
    Locatable<&'ctx Type<'ctx>>,
) -> ParseResult<Locatable<&'ctx Type<'ctx>>>;

impl<'src, 'ctx> Parser<'src, 'ctx> {
    /// ```ebnf
    /// Type ::=
    ///     'str' | 'rune' | 'bool' | 'unit' | 'absurd'
    ///     | 'ureg' | 'ireg' | 'uptr' | 'iptr' | Path
    ///     | 'i' [0-9]+ | 'u' [0-9]+ | 'f' [0-9]+
    ///     | '!' Type | 'infer'
    ///     | '(' Type ')'
    ///     | Type '&' Type
    ///     | Type '|' Type
    ///     | Path '[' Type ']'
    ///     | 'type' '[' Type ']'
    ///     | 'slice' '[' Type ']'
    ///     | 'const' Ident (':' Type)?
    ///     | Type 'if' Expr 'else' Type
    ///     | 'arr' '[' Literal, Type ']'
    ///     | 'tup' '[' (Type | Type ',' Type)+ ']'
    ///     | 'fn' '(' (Type | Type ',' Type)? ')' ('->' Type)?
    /// ```
    // TODO: Decide about `Self` and `self`
    #[recursion_guard]
    pub(super) fn ascribed_type(&'ctx mut self) -> ParseResult<Locatable<&'ctx Type<'ctx>>> {
        self.ascribed_type_internal(0)
    }

    #[inline(always)]
    fn type_precedence(&self) -> usize {
        self.peek
            .map(|p| {
                TypePrecedence::try_from(p.ty())
                    .map(|p| p.precedence())
                    .unwrap_or(0)
            })
            .unwrap_or(0)
    }

    #[recursion_guard]
    fn ascribed_type_internal(
        &'ctx mut self,
        precedence: usize,
    ) -> ParseResult<Locatable<&'ctx Type<'ctx>>> {
        let mut token = self.next()?;

        let prefix = Self::type_prefix(token.ty());
        if let Some(prefix) = prefix {
            let mut left = prefix(self, token)?;

            if let Ok(peek) = self.peek() {
                let postfix = Self::type_postfix(peek.ty());
                if let Some(postfix) = postfix {
                    token = self.next()?;
                    left = postfix(self, token, left)?;
                }
            }

            while precedence < self.type_precedence() {
                token = self.next()?;

                let infix = Self::type_infix(token.ty());
                if let Some(infix) = infix {
                    left = infix(self, token, left)?;
                } else {
                    break;
                }
            }

            Ok(left)
        } else {
            Err(Locatable::new(
                Error::Syntax(SyntaxError::Generic(format!(
                    "Could not parse `{}`",
                    token.ty()
                ))),
                Location::concrete(&token, self.current_file),
            ))
        }
    }

    fn type_prefix(ty: TokenType) -> Option<PrefixParselet<'src, 'ctx>> {
        let prefix: PrefixParselet = match ty {
            // Types and builtins
            TokenType::Ident => |parser, token| {
                let _frame = parser.add_stack_frame()?;

                let (ty, end) = match token.source() {
                    "str" => (Type::String, None),
                    "rune" => (Type::Rune, None),
                    "bool" => (Type::Boolean, None),
                    "unit" => (Type::Unit, None),
                    "absurd" => (Type::Absurd, None),
                    "infer" => (Type::Infer, None),

                    "ureg" | "ireg" => {
                        let sign = if token.source() == "ureg" {
                            Signedness::Unsigned
                        } else {
                            Signedness::Signed
                        };

                        (Type::IntReg(sign), None)
                    }

                    "uptr" | "iptr" => {
                        let sign = if token.source() == "uptr" {
                            Signedness::Unsigned
                        } else {
                            Signedness::Signed
                        };

                        (Type::IntPtr(sign), None)
                    }

                    "arr" => {
                        parser.eat(TokenType::LeftBrace, [TokenType::Newline])?;

                        let int = parser.eat(TokenType::Int, [TokenType::Newline])?;
                        let Integer { sign, bits: len } =
                            Literal::try_from((&int, parser.current_file))?
                                .into_integer()
                                .expect("An integer was eaten, so it should be parsed as well");

                        if sign.is_negative() {
                            return Err(Locatable::new(
                                Error::Syntax(SyntaxError::NegativeArrayLen),
                                Location::concrete(int.span(), parser.current_file),
                            ));
                        }

                        parser.eat(TokenType::Comma, [TokenType::Newline])?;

                        let ty = parser.ascribed_type()?;
                        let end = parser
                            .eat(TokenType::RightBrace, [TokenType::Newline])?
                            .span();

                        (Type::Array(len, ty), Some(end))
                    }

                    "slice" => {
                        parser.eat(TokenType::LeftBrace, [TokenType::Newline])?;
                        let ty = parser.ascribed_type()?;
                        let end = parser
                            .eat(TokenType::RightBrace, [TokenType::Newline])?
                            .span();

                        (Type::Slice(ty), Some(end))
                    }

                    "tup" => {
                        parser.eat(TokenType::LeftBrace, [TokenType::Newline])?;

                        let mut types = Vec::with_capacity(3);
                        while parser.peek()?.ty() != TokenType::RightBrace {
                            if token.ty() == TokenType::Newline {
                                parser.eat(TokenType::Newline, [])?;
                                continue;
                            }

                            types.push(parser.ascribed_type()?);

                            // TODO: Helpful hint
                            if parser.peek()?.ty() == TokenType::Comma {
                                parser.eat(TokenType::Comma, [])?;
                            } else {
                                break;
                            }
                        }
                        let end = parser
                            .eat(TokenType::RightBrace, [TokenType::Newline])?
                            .span();

                        (Type::Tuple(types), Some(end))
                    }

                    uint if uint.starts_with('u')
                        && uint.len() > 1
                        && uint.chars().skip(1).all(char::is_numeric) =>
                    {
                        let int = uint.chars().skip(1).collect::<String>();
                        let width: u16 = lexical_core::parse(int.as_bytes()).map_err(|_| {
                                Locatable::new(
                                    Error::Syntax(SyntaxError::Generic(format!(
                                        "Unsigned integer types must be between `u1` and `u65536`, `{}` is out of range",
                                        uint,
                                    ))),
                                    Location::concrete(token.span(), parser.current_file),
                                )
                            })?;

                        (
                            Type::Integer {
                                sign: Signedness::Unsigned,
                                width,
                            },
                            None,
                        )
                    }

                    int if int.starts_with('i')
                        && int.len() > 1
                        && int.chars().skip(1).all(char::is_numeric) =>
                    {
                        let int = int.chars().skip(1).collect::<String>();
                        let width: u16 = lexical_core::parse(int.as_bytes()).map_err(|_| {
                                Locatable::new(
                                    Error::Syntax(SyntaxError::Generic(format!(
                                        "Integer types must be between `i1` and `i65536`, `{}` is out of range",
                                        int,
                                    ))),
                                    Location::concrete(token.span(), parser.current_file),
                                )
                            })?;

                        (
                            Type::Integer {
                                sign: Signedness::Signed,
                                width,
                            },
                            None,
                        )
                    }

                    float
                        if float.starts_with('f')
                            && float.len() > 1
                            && float.chars().skip(1).all(char::is_numeric) =>
                    {
                        let float = float.chars().skip(1).collect::<String>();
                        let width: u16 = lexical_core::parse(float.as_bytes()).map_err(|_| {
                                Locatable::new(
                                    Error::Syntax(SyntaxError::Generic(format!(
                                        "Float types must be between `f1` and `f65536`, `{}` is out of range",
                                        float,
                                    ))),
                                    Location::concrete(token.span(), parser.current_file),
                                )
                            })?;

                        (Type::Float { width }, None)
                    }

                    custom => {
                        let custom = parser.context.intern(custom);
                        let path = parser.item_path(custom)?;

                        if parser.peek().map(|t| t.ty()) == Ok(TokenType::LeftBrace) {
                            parser.eat(TokenType::LeftBrace, [])?;

                            let mut bounds = Vec::with_capacity(3);
                            while parser.peek()?.ty() != TokenType::RightBrace {
                                if token.ty() == TokenType::Newline {
                                    parser.eat(TokenType::Newline, [])?;
                                    continue;
                                }

                                bounds.push(parser.ascribed_type()?);

                                // TODO: Helpful hint
                                if parser.peek()?.ty() == TokenType::Comma {
                                    parser.eat(TokenType::Comma, [])?;
                                } else {
                                    break;
                                }
                            }

                            let end = parser.eat(TokenType::RightBrace, [])?.span();

                            (Type::Bounded { path, bounds }, Some(end))
                        } else {
                            (Type::ItemPath(path), None)
                        }
                    }
                };

                let ty = parser.context.store(ty);

                Ok(Locatable::new(
                    ty,
                    Location::concrete(
                        Span::merge(token.span(), end.unwrap_or_else(|| token.span())),
                        parser.current_file,
                    ),
                ))
            },

            // Negation
            TokenType::Bang => |parser, bang| {
                let _frame = parser.add_stack_frame()?;

                let ty = parser.ascribed_type()?;
                let end = ty.span();

                Ok(Locatable::new(
                    parser.context.store(Type::Not(ty)),
                    Location::concrete(Span::merge(bang.span(), end), parser.current_file),
                ))
            },

            // Grouping via parentheses
            TokenType::LeftParen => |parser, paren| {
                let _frame = parser.add_stack_frame()?;

                let ty = parser.ascribed_type()?;
                let ty = parser.context.store(Type::Parenthesised(ty));
                let end = parser
                    .eat(TokenType::RightParen, [TokenType::Newline])?
                    .span();

                Ok(Locatable::new(
                    ty,
                    Location::concrete(Span::merge(paren.span(), end), parser.current_file),
                ))
            },

            // Function pointers
            TokenType::Function => |parser, func| {
                let _frame = parser.add_stack_frame()?;
                parser.eat(TokenType::LeftParen, [TokenType::Newline])?;

                let mut params = Vec::with_capacity(5);
                while parser.peek()?.ty() != TokenType::RightParen {
                    if parser.peek()?.ty() == TokenType::Newline {
                        parser.eat(TokenType::Newline, [])?;
                        continue;
                    }

                    params.push(parser.ascribed_type()?);
                }

                let end = parser
                    .eat(TokenType::RightParen, [TokenType::Newline])?
                    .span();

                let returns = if parser.peek().map(|t| t.ty()) == Ok(TokenType::RightArrow) {
                    parser.ascribed_type()?
                } else {
                    Locatable::new(
                        parser.context.store(Type::Infer),
                        Location::implicit(Span::merge(func.span(), end), parser.current_file),
                    )
                };
                let end = returns.span();

                let ty = parser.context.store(Type::Function {
                    params,
                    returns: returns,
                });

                Ok(Locatable::new(
                    ty,
                    Location::concrete(Span::merge(func.span(), end), parser.current_file),
                ))
            },

            // Trait objects
            TokenType::Type => |parser, ty| {
                let _frame = parser.add_stack_frame()?;

                parser.eat(TokenType::LeftBrace, [TokenType::Newline])?;

                let mut types = Vec::with_capacity(3);
                while parser.peek()?.ty() != TokenType::RightBrace {
                    if parser.peek()?.ty() == TokenType::Newline {
                        parser.eat(TokenType::Newline, [])?;
                        continue;
                    }

                    types.push(parser.ascribed_type()?);

                    // TODO: Nice error
                    if parser.peek()?.ty() == TokenType::Comma {
                        parser.eat(TokenType::Comma, [])?;
                    } else {
                        break;
                    }
                }

                let end = parser.eat(TokenType::RightBrace, [])?.span();

                Ok(Locatable::new(
                    parser.context.store(Type::TraitObj(types)),
                    Location::concrete(Span::merge(ty.span(), end), parser.current_file),
                ))
            },

            // Const parameters
            TokenType::Const => |parser, cons| {
                let _frame = parser.add_stack_frame()?;

                let ident = parser.eat(TokenType::Ident, [TokenType::Newline])?;
                let ident = parser.context.intern(ident.source());

                let ty = if parser.peek()?.ty() == TokenType::Colon {
                    parser.eat(TokenType::Colon, [])?;

                    parser.ascribed_type()?
                } else {
                    Locatable::new(
                        parser.context.store(Type::Infer),
                        Location::implicit(cons.span(), parser.current_file),
                    )
                };
                let end = ty.span();

                Ok(Locatable::new(
                    parser.context.store(Type::Const(ident, ty)),
                    Location::concrete(Span::merge(cons.span(), end), parser.current_file),
                ))
            },

            _ => return None,
        };

        Some(prefix)
    }

    fn type_postfix(_ty: TokenType) -> Option<PostfixParselet<'src, 'ctx>> {
        None
    }

    fn type_infix(ty: TokenType) -> Option<InfixParselet<'src, 'ctx>> {
        let infix: InfixParselet = match ty {
            TokenType::Ampersand | TokenType::Pipe => |parser, operand, left| {
                let _frame = parser.add_stack_frame()?;

                let right = parser.ascribed_type()?;

                let start = left.span();
                let end = right.span();

                let ty = parser.context.store(Type::Operand(
                    left,
                    TypeOperand::try_from((&operand, parser.current_file))?,
                    right,
                ));

                Ok(Locatable::new(
                    ty,
                    Location::concrete(Span::merge(start, end), parser.current_file),
                ))
            },

            // TokenType::If => |parser, _if, true_arm| {
            //     let _frame = parser.add_stack_frame()?;
            //
            //     let condition = parser.expr()?;
            //     parser.eat(TokenType::Else, [TokenType::Newline])?;
            //
            //     let false_arm = parser.ascribed_type()?;
            //     let ty = Type::InlineConditional {
            //         true_arm,
            //         condition,
            //         false_arm,
            //     };
            //
            //     Ok(ty)
            // },
            _ => return None,
        };

        Some(infix)
    }
}

#[cfg_attr(test, derive(Deserialize, Serialize))]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type<'ctx> {
    Operand(
        Locatable<&'ctx Type<'ctx>>,
        TypeOperand,
        Locatable<&'ctx Type<'ctx>>,
    ),
    Const(StrT, Locatable<&'ctx Type<'ctx>>),
    Not(Locatable<&'ctx Type<'ctx>>),
    Parenthesised(Locatable<&'ctx Type<'ctx>>),
    Function {
        params: Vec<Locatable<&'ctx Type<'ctx>>>,
        returns: Locatable<&'ctx Type<'ctx>>,
    },
    TraitObj(Vec<Locatable<&'ctx Type<'ctx>>>),
    Bounded {
        path: ItemPath,
        bounds: Vec<Locatable<&'ctx Type<'ctx>>>,
    },
    ItemPath(ItemPath),
    Infer,
    Integer {
        sign: Signedness,
        width: u16,
    },
    IntReg(Signedness),
    IntPtr(Signedness),
    Float {
        width: u16,
    },
    Boolean,
    String,
    Rune,
    Unit,
    Absurd,
    Array(u128, Locatable<&'ctx Type<'ctx>>),
    Slice(Locatable<&'ctx Type<'ctx>>),
    Tuple(Vec<Locatable<&'ctx Type<'ctx>>>),
}

impl<'ctx> Type<'ctx> {
    pub fn internal_types(&self) -> Vec<ItemPath> {
        let mut buf = Vec::with_capacity(1);
        self.internal_types_inner(&mut buf);

        buf
    }

    fn internal_types_inner(&self, buf: &mut Vec<ItemPath>) {
        match self {
            Self::Operand(rhs, _, lhs) => {
                rhs.internal_types_inner(buf);
                lhs.internal_types_inner(buf);
            }
            Self::Const(name, ty) => {
                buf.push(ItemPath::new(vec![*name]));
                ty.internal_types_inner(buf);
            }
            Self::Not(ty) => ty.internal_types_inner(buf),
            Self::Parenthesised(ty) => ty.internal_types_inner(buf),
            Self::Function { params, returns } => {
                for param in params {
                    param.internal_types_inner(buf);
                }

                returns.internal_types_inner(buf);
            }
            Self::TraitObj(types) => {
                for ty in types {
                    ty.internal_types_inner(buf);
                }
            }
            Self::Bounded { path, bounds } => {
                buf.push(path.clone());

                for bound in bounds {
                    bound.internal_types_inner(buf);
                }
            }
            Self::ItemPath(path) => buf.push(path.clone()),

            _ => {}
        }
    }

    pub fn to_string(&self, context: &'ctx Context<'ctx>) -> String {
        match self {
            Self::Infer => "infer".to_string(),
            Self::Not(ty) => format!("!{}", ty.to_string(context)),
            Self::Parenthesised(ty) => format!("({})", ty.to_string(context)),
            Self::Const(ident, ty) => format!(
                "const {}: {}",
                context.resolve(*ident),
                ty.to_string(context)
            ),
            Self::Operand(left, op, right) => format!(
                "{} {} {}",
                left.to_string(context),
                op,
                right.to_string(context)
            ),
            Self::Function { params, returns } => format!(
                "fn({}) -> {}",
                params
                    .iter()
                    .map(|ty| ty.to_string(context))
                    .collect::<Vec<String>>()
                    .join(", "),
                returns.to_string(context)
            ),
            Self::TraitObj(traits) => format!(
                "type[{}]",
                traits
                    .iter()
                    .map(|ty| ty.to_string(context))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Self::Bounded { path, bounds } => format!(
                "{}[{}]",
                path.iter()
                    .map(|seg| context.resolve(*seg))
                    .collect::<Vec<&str>>()
                    .join("."),
                bounds
                    .iter()
                    .map(|ty| ty.to_string(context))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Self::ItemPath(path) => path
                .iter()
                .map(|seg| context.resolve(*seg))
                .collect::<Vec<&str>>()
                .join("."),
            Self::Integer { sign, width } => format!(
                "{}{}",
                match sign {
                    Signedness::Signed => "i",
                    Signedness::Unsigned => "u",
                },
                width
            ),
            Self::IntPtr(sign) => format!(
                "{}ptr",
                match sign {
                    Signedness::Signed => "i",
                    Signedness::Unsigned => "u",
                },
            ),
            Self::IntReg(sign) => format!(
                "{}reg",
                match sign {
                    Signedness::Signed => "i",
                    Signedness::Unsigned => "u",
                },
            ),
            Self::Float { width } => format!("f{}", width),
            Self::Boolean => "bool".to_string(),
            Self::String => "str".to_string(),
            Self::Rune => "rune".to_string(),
            Self::Unit => "unit".to_string(),
            Self::Absurd => "absurd".to_string(),
            Self::Array(len, ty) => format!("arr[{}, {}]", len, ty.to_string(context)),
            Self::Slice(ty) => format!("slice{}]", ty.to_string(context)),
            Self::Tuple(types) => format!(
                "tup[{}]",
                types
                    .iter()
                    .map(|ty| ty.to_string(context))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
        }
    }
}

impl<'ctx> Default for Type<'ctx> {
    fn default() -> Self {
        Self::Infer
    }
}

#[cfg_attr(test, derive(Deserialize, Serialize))]
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum TypeOperand {
    And,
    Or,
}

impl fmt::Display for TypeOperand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::And => write!(f, "&"),
            Self::Or => write!(f, "|"),
        }
    }
}

impl<'src> TryFrom<(&Token<'src>, CurrentFile)> for TypeOperand {
    type Error = Locatable<Error>;

    fn try_from((token, file): (&Token<'src>, CurrentFile)) -> Result<Self, Self::Error> {
        match token.ty() {
            TokenType::Ampersand => Ok(Self::And),
            TokenType::Pipe => Ok(Self::Or),

            ty => Err(Locatable::new(
                Error::Syntax(SyntaxError::Generic(format!(
                    "Expected one of {}, got '{}'",
                    [TokenType::Ampersand, TokenType::Pipe]
                        .iter()
                        .map(|t| t.to_str())
                        .collect::<Vec<_>>()
                        .join(", "),
                    ty,
                ))),
                Location::concrete(token, file),
            )),
        }
    }
}

#[cfg_attr(test, derive(Deserialize, Serialize))]
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
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

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TypePrecedence {
    And,
    Or,
    Ternary,
}

impl TypePrecedence {
    pub fn precedence(self) -> usize {
        match self {
            Self::And => 6,
            Self::Or => 4,
            Self::Ternary => 1,
        }
    }
}

impl TryFrom<TokenType> for TypePrecedence {
    type Error = ();

    fn try_from(t: TokenType) -> Result<TypePrecedence, ()> {
        Ok(match t {
            TokenType::Ampersand => Self::And,
            TokenType::Pipe => Self::Or,
            TokenType::If | TokenType::Function => Self::Ternary,

            _ => return Err(()),
        })
    }
}

// TODO: Test the rest
#[cfg(test)]
mod tests {
    use super::*;
    use crate::files::FileId;

    #[test]
    fn builtins() {
        let types = [
            ("str", Type::String),
            ("rune", Type::Rune),
            ("bool", Type::Boolean),
            ("unit", Type::Unit),
            ("absurd", Type::Absurd),
            ("ureg", Type::IntReg(Signedness::Unsigned)),
            ("ireg", Type::IntReg(Signedness::Signed)),
            ("uptr", Type::IntPtr(Signedness::Unsigned)),
            ("iptr", Type::IntPtr(Signedness::Signed)),
        ];
        let mut context = Context::new();

        for (src, correct) in types.iter() {
            let current_file = CurrentFile::new(FileId::new(0), src.len());
            let ty = Parser::new(src, current_file, &mut context)
                .ascribed_type()
                .unwrap();

            assert_eq!(&*ty, &correct);
        }
    }

    #[test]
    fn ints() {
        let types = [
            (
                "i0",
                Type::Integer {
                    sign: Signedness::Signed,
                    width: 0,
                },
            ),
            (
                "i65535",
                Type::Integer {
                    sign: Signedness::Signed,
                    width: 65535,
                },
            ),
            (
                "u0",
                Type::Integer {
                    sign: Signedness::Unsigned,
                    width: 0,
                },
            ),
            (
                "u65535",
                Type::Integer {
                    sign: Signedness::Unsigned,
                    width: 65535,
                },
            ),
            ("f0", Type::Float { width: 0 }),
            ("f65535", Type::Float { width: 65535 }),
        ];
        let mut context = Context::new();

        for (src, correct) in types.iter() {
            let current_file = CurrentFile::new(FileId::new(0), src.len());
            let ty = Parser::new(src, current_file, &mut context)
                .ascribed_type()
                .unwrap();

            assert_eq!(&*ty, &correct);
        }
    }

    #[test]
    fn not() {
        let types = [
            (
                "!str",
                Type::Not(Box::new(Locatable {
                    data: Type::String,
                    loc: Location::concrete(Span::from(1..4), FileId::new(0)),
                })),
            ),
            (
                "!rune",
                Type::Not(Box::new(Locatable {
                    data: Type::Rune,
                    loc: Location::concrete(Span::from(1..5), FileId::new(0)),
                })),
            ),
        ];
        let mut context = Context::new();

        for (src, correct) in types.iter() {
            let current_file = CurrentFile::new(FileId::new(0), src.len());
            let ty = Parser::new(src, current_file, &mut context)
                .ascribed_type()
                .unwrap();

            assert_eq!(&*ty, &correct);
        }
    }

    #[test]
    fn paren() {
        let types = [
            (
                "(str)",
                Type::Parenthesised(Box::new(Locatable {
                    data: Type::String,
                    loc: Location::concrete(Span::from(1..4), FileId::new(0)),
                })),
            ),
            (
                "(rune)",
                Type::Parenthesised(Box::new(Locatable {
                    data: Type::Rune,
                    loc: Location::concrete(Span::from(1..5), FileId::new(0)),
                })),
            ),
        ];
        let mut context = Context::new();

        for (src, correct) in types.iter() {
            let current_file = CurrentFile::new(FileId::new(0), src.len());
            let ty = Parser::new(src, current_file, &mut context)
                .ascribed_type()
                .unwrap();

            assert_eq!(&*ty, &correct);
        }
    }

    #[test]
    fn operands() {
        let mut context = Context::new();

        let types = [
            (
                "str & rune",
                Type::Operand(
                    Locatable {
                        data: context.store(Type::String),
                        loc: Location::concrete(Span::from(0..3), FileId::new(0)),
                    },
                    TypeOperand::And,
                    Locatable {
                        data: context.store(Type::Rune),
                        loc: Location::concrete(Span::from(6..10), FileId::new(0)),
                    },
                ),
            ),
            (
                "str | rune",
                Type::Operand(
                    Locatable {
                        data: context.store(Type::String),
                        loc: Location::concrete(Span::from(0..3), FileId::new(0)),
                    },
                    TypeOperand::Or,
                    Locatable {
                        data: context.store(Type::Rune),
                        loc: Location::concrete(Span::from(6..10), FileId::new(0)),
                    },
                ),
            ),
        ];

        for (src, correct) in types.iter() {
            let current_file = CurrentFile::new(FileId::new(0), src.len());
            let ty = Parser::new(src, current_file, &mut context)
                .ascribed_type()
                .unwrap();

            assert_eq!(&*ty, &correct);
        }
    }

    #[cfg(not(any(target_arch = "wasm32", miri)))]
    mod proptests {
        use super::*;
        use proptest::prelude::*;

        proptest! {
            #[test]
            fn int_types(s in r#"i[0-9]+"#) {
                let mut parser = Parser::new(&s, CurrentFile::new(FileId(0), s.len()), &mut Context::default());

                let ty = parser.ascribed_type();
                match ty.as_ref().map(|t| &**t) {
                    Ok(Type::Integer { sign: Signedness::Signed, .. }) => {},
                    Err(Locatable { data: _data @ Error::Syntax(SyntaxError::Generic(..)), .. }) => {}

                    _ => prop_assert!(false),
                }
            }

            #[test]
            fn uint_types(s in r#"u[0-9]+"#) {
                let mut parser = Parser::new(&s, CurrentFile::new(FileId(0), s.len()), &mut Context::default());

                let ty = parser.ascribed_type();
                match ty.as_ref().map(|t| &**t) {
                    Ok(Type::Integer { sign: Signedness::Unsigned, .. }) => {},
                    Err(Locatable { data: _data @ Error::Syntax(SyntaxError::Generic(..)), .. }) => {}

                    _ => prop_assert!(false),
                }
            }

            #[test]
            fn float(s in r#"f[0-9]+"#) {
                let mut parser = Parser::new(&s, CurrentFile::new(FileId(0), s.len()), &mut Context::default());

                let ty = parser.ascribed_type();
                match ty.as_ref().map(|t| &**t) {
                    Ok(Type::Float { .. }) => {},
                    Err(Locatable { data: _data @ Error::Syntax(SyntaxError::Generic(..)), .. }) => {}

                    _ => prop_assert!(false),
                }
            }
        }
    }
}
