use crate::{
    error::{Error, Locatable, Location, ParseResult, Span, SyntaxError},
    interner::Interner,
    parser::{CurrentFile, Integer, Literal, Parser},
    token::{Token, TokenType},
};

use lasso::Spur;

use alloc::{
    borrow::ToOwned,
    boxed::Box,
    format,
    string::{String, ToString},
    vec::Vec,
};
use core::{convert::TryFrom, fmt};

type PrefixParselet<'src, 'expr, 'stmt> =
    fn(&mut Parser<'src, 'expr, 'stmt>, Token<'src>) -> ParseResult<Locatable<Type>>;
type PostfixParselet<'src, 'expr, 'stmt> = fn(
    &mut Parser<'src, 'expr, 'stmt>,
    Token<'src>,
    Locatable<Type>,
) -> ParseResult<Locatable<Type>>;
type InfixParselet<'src, 'expr, 'stmt> = fn(
    &mut Parser<'src, 'expr, 'stmt>,
    Token<'src>,
    Locatable<Type>,
) -> ParseResult<Locatable<Type>>;

impl<'src, 'stmt, 'expr> Parser<'src, 'stmt, 'expr> {
    /// ```ebnf
    /// Type ::=
    ///     'str' | 'rune' | 'bool' | 'unit' | 'absurd'
    ///     | 'ureg' | 'ireg' | 'uptr' | 'iptr' | Ident
    ///     | 'i' [0-9]+ | 'u' [0-9]+ | 'f' [0-9]+
    ///     | '!' Type | 'infer'
    ///     | '(' Type ')'
    ///     | Type '&' Type
    ///     | Type '|' Type
    ///     | Ident '[' Type ']'
    ///     | 'type' '[' Type ']'
    ///     | 'slice' '[' Type ']'
    ///     | 'const' Ident (':' Type)?
    ///     | Type 'if' Expr 'else' Type
    ///     | 'arr' '[' Literal, Type ']'
    ///     | 'tup' '[' (Type | Type ',' Type)+ ']'
    ///     | 'fn' '(' (Type | Type ',' Type)? ')' ('->' Type)?
    /// ```
    // TODO: Decide about `Self` and `self`
    pub(super) fn ascribed_type(&mut self) -> ParseResult<Locatable<Type>> {
        let _frame = self.add_stack_frame()?;

        self.ascribed_type_internal(0)
    }

    fn ascribed_type_internal(&mut self, precedence: usize) -> ParseResult<Locatable<Type>> {
        let _frame = self.add_stack_frame()?;
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

            while precedence < self.current_precedence() {
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

    fn type_prefix(ty: TokenType) -> Option<PrefixParselet<'src, 'expr, 'stmt>> {
        let prefix: PrefixParselet = match ty {
            // Types and builtins
            TokenType::Ident => |parser, token| {
                let _frame = parser.add_stack_frame()?;

                let (ty, end) = match token.source() {
                    "str" => (Type::Builtin(BuiltinType::String), None),
                    "rune" => (Type::Builtin(BuiltinType::Rune), None),
                    "bool" => (Type::Builtin(BuiltinType::Boolean), None),
                    "unit" => (Type::Builtin(BuiltinType::Unit), None),
                    "absurd" => (Type::Builtin(BuiltinType::Absurd), None),
                    "infer" => (Type::Infer, None),

                    "ureg" | "ireg" => {
                        let sign = if token.source() == "ureg" {
                            Signedness::Unsigned
                        } else {
                            Signedness::Signed
                        };

                        (Type::Builtin(BuiltinType::IntReg(sign)), None)
                    }

                    "uptr" | "iptr" => {
                        let sign = if token.source() == "uptr" {
                            Signedness::Unsigned
                        } else {
                            Signedness::Signed
                        };

                        (Type::Builtin(BuiltinType::IntPtr(sign)), None)
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

                        (
                            Type::Builtin(BuiltinType::Array(len, Box::new(ty))),
                            Some(end),
                        )
                    }

                    "slice" => {
                        parser.eat(TokenType::LeftBrace, [TokenType::Newline])?;
                        let ty = parser.ascribed_type()?;
                        let end = parser
                            .eat(TokenType::RightBrace, [TokenType::Newline])?
                            .span();

                        (Type::Builtin(BuiltinType::Slice(Box::new(ty))), Some(end))
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

                        (Type::Builtin(BuiltinType::Tuple(types)), Some(end))
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
                            Type::Builtin(BuiltinType::Integer {
                                sign: Signedness::Unsigned,
                                width,
                            }),
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
                            Type::Builtin(BuiltinType::Integer {
                                sign: Signedness::Signed,
                                width,
                            }),
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

                        (Type::Builtin(BuiltinType::Float { width }), None)
                    }

                    custom => {
                        let ty = parser.string_interner.intern(custom);
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

                            (Type::Bounded { ty, bounds }, Some(end))
                        } else {
                            (Type::Custom(ty), None)
                        }
                    }
                };

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

                let ty = Box::new(parser.ascribed_type()?);
                let end = ty.span();

                Ok(Locatable::new(
                    Type::Not(ty),
                    Location::concrete(Span::merge(bang.span(), end), parser.current_file),
                ))
            },

            // Grouping via parentheses
            TokenType::LeftParen => |parser, paren| {
                let _frame = parser.add_stack_frame()?;

                let ty = Type::Parenthesised(Box::new(parser.ascribed_type()?));
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
                        Type::Infer,
                        Location::implicit(Span::merge(func.span(), end), parser.current_file),
                    )
                };
                let end = returns.span();

                let ty = Type::Function {
                    params,
                    returns: Box::new(returns),
                };

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
                    Type::TraitObj(types),
                    Location::concrete(Span::merge(ty.span(), end), parser.current_file),
                ))
            },

            // Const parameters
            TokenType::Const => |parser, cons| {
                let _frame = parser.add_stack_frame()?;

                let ident = parser.eat(TokenType::Ident, [TokenType::Newline])?;
                let ident = parser.string_interner.intern(ident.source());

                let ty = if parser.peek()?.ty() == TokenType::Colon {
                    parser.eat(TokenType::Colon, [])?;

                    parser.ascribed_type()?
                } else {
                    Locatable::new(
                        Type::Infer,
                        Location::implicit(cons.span(), parser.current_file),
                    )
                };
                let end = ty.span();

                Ok(Locatable::new(
                    Type::Const(ident, Box::new(ty)),
                    Location::concrete(Span::merge(cons.span(), end), parser.current_file),
                ))
            },

            _ => return None,
        };

        Some(prefix)
    }

    fn type_postfix(_ty: TokenType) -> Option<PostfixParselet<'src, 'expr, 'stmt>> {
        None
    }

    fn type_infix(ty: TokenType) -> Option<InfixParselet<'src, 'expr, 'stmt>> {
        let infix: InfixParselet = match ty {
            TokenType::Ampersand | TokenType::Pipe => |parser, operand, left| {
                let _frame = parser.add_stack_frame()?;

                let right = parser.ascribed_type()?;

                let start = left.span();
                let end = right.span();

                let ty = Type::Operand(
                    Box::new(left),
                    TypeOperand::try_from((&operand, parser.current_file))?,
                    Box::new(right),
                );

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

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Operand(Box<Locatable<Type>>, TypeOperand, Box<Locatable<Type>>),
    Const(Spur, Box<Locatable<Type>>),
    Not(Box<Locatable<Type>>),
    Parenthesised(Box<Locatable<Type>>),
    Function {
        params: Vec<Locatable<Type>>,
        returns: Box<Locatable<Type>>,
    },
    Builtin(BuiltinType),
    TraitObj(Vec<Locatable<Type>>),
    Bounded {
        ty: Spur,
        bounds: Vec<Locatable<Type>>,
    },
    Custom(Spur),
    Infer,
}

impl Type {
    pub fn to_string(&self, interner: &Interner) -> String {
        match self {
            Type::Infer => "infer".to_string(),
            Type::Not(ty) => format!("!{}", ty.data().to_string(interner)),
            Type::Parenthesised(ty) => format!("({})", ty.data().to_string(interner)),
            Type::Const(ident, ty) => format!(
                "const {}: {}",
                interner.resolve(ident),
                ty.data().to_string(interner)
            ),
            Type::Operand(left, op, right) => format!(
                "{} {} {}",
                left.data().to_string(interner),
                op,
                right.data().to_string(interner)
            ),
            Type::Function { params, returns } => format!(
                "fn({}) -> {}",
                params
                    .iter()
                    .map(|ty| ty.data().to_string(interner))
                    .collect::<Vec<String>>()
                    .join(", "),
                returns.data().to_string(interner)
            ),
            Type::TraitObj(traits) => format!(
                "type[{}]",
                traits
                    .iter()
                    .map(|ty| ty.data().to_string(interner))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Type::Bounded { ty, bounds } => format!(
                "{}[{}]",
                interner.resolve(ty),
                bounds
                    .iter()
                    .map(|ty| ty.data().to_string(interner))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Type::Builtin(b) => b.to_string(interner),
            Type::Custom(c) => interner.resolve(c).to_owned(),
        }
    }
}

impl Default for Type {
    fn default() -> Self {
        Self::Infer
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
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
    Array(u128, Box<Locatable<Type>>),
    Slice(Box<Locatable<Type>>),
    Tuple(Vec<Locatable<Type>>),
}

impl BuiltinType {
    pub fn to_string(&self, interner: &Interner) -> String {
        match self {
            BuiltinType::Integer { sign, width } => format!(
                "{}{}",
                match sign {
                    Signedness::Signed => "i",
                    Signedness::Unsigned => "u",
                },
                width
            ),
            BuiltinType::IntPtr(sign) => format!(
                "{}ptr",
                match sign {
                    Signedness::Signed => "i",
                    Signedness::Unsigned => "u",
                },
            ),
            BuiltinType::IntReg(sign) => format!(
                "{}reg",
                match sign {
                    Signedness::Signed => "i",
                    Signedness::Unsigned => "u",
                },
            ),
            BuiltinType::Float { width } => format!("f{}", width),
            BuiltinType::Boolean => "bool".to_string(),
            BuiltinType::String => "str".to_string(),
            BuiltinType::Rune => "rune".to_string(),
            BuiltinType::Unit => "unit".to_string(),
            BuiltinType::Absurd => "absurd".to_string(),
            BuiltinType::Array(len, ty) => {
                format!("arr[{}, {}]", len, ty.data().to_string(interner))
            }
            BuiltinType::Slice(ty) => format!("slice{}]", ty.data().to_string(interner)),
            BuiltinType::Tuple(types) => format!(
                "tup[{}]",
                types
                    .iter()
                    .map(|ty| ty.data().to_string(interner))
                    .collect::<Vec<String>>()
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

// TODO: Test the rest
#[cfg(test)]
mod tests {
    use super::*;
    use crate::files::FileId;

    #[test]
    fn builtins() {
        let types = [
            ("str", BuiltinType::String),
            ("rune", BuiltinType::Rune),
            ("bool", BuiltinType::Boolean),
            ("unit", BuiltinType::Unit),
            ("absurd", BuiltinType::Absurd),
            ("ureg", BuiltinType::IntReg(Signedness::Unsigned)),
            ("ireg", BuiltinType::IntReg(Signedness::Signed)),
            ("uptr", BuiltinType::IntPtr(Signedness::Unsigned)),
            ("iptr", BuiltinType::IntPtr(Signedness::Signed)),
        ];
        let string_interner = Interner::new();

        for (src, correct) in types.iter() {
            let current_file = CurrentFile::new(FileId::new(0), src.len());
            let ty = Parser::new(src, current_file, string_interner.clone())
                .ascribed_type()
                .unwrap();

            assert_eq!(ty.data(), &Type::Builtin(correct.clone()));
        }
    }

    #[test]
    fn ints() {
        let types = [
            (
                "i0",
                BuiltinType::Integer {
                    sign: Signedness::Signed,
                    width: 0,
                },
            ),
            (
                "i65535",
                BuiltinType::Integer {
                    sign: Signedness::Signed,
                    width: 65535,
                },
            ),
            (
                "u0",
                BuiltinType::Integer {
                    sign: Signedness::Unsigned,
                    width: 0,
                },
            ),
            (
                "u65535",
                BuiltinType::Integer {
                    sign: Signedness::Unsigned,
                    width: 65535,
                },
            ),
            ("f0", BuiltinType::Float { width: 0 }),
            ("f65535", BuiltinType::Float { width: 65535 }),
        ];
        let string_interner = Interner::new();

        for (src, correct) in types.iter() {
            let current_file = CurrentFile::new(FileId::new(0), src.len());
            let ty = Parser::new(src, current_file, string_interner.clone())
                .ascribed_type()
                .unwrap();

            assert_eq!(ty.data(), &Type::Builtin(correct.clone()));
        }
    }

    #[test]
    fn not() {
        let types = [
            (
                "!str",
                Type::Not(Box::new(Locatable {
                    data: Type::Builtin(BuiltinType::String),
                    loc: Location::concrete(Span::from(1..4), FileId::new(0)),
                })),
            ),
            (
                "!rune",
                Type::Not(Box::new(Locatable {
                    data: Type::Builtin(BuiltinType::Rune),
                    loc: Location::concrete(Span::from(1..5), FileId::new(0)),
                })),
            ),
        ];
        let string_interner = Interner::new();

        for (src, correct) in types.iter() {
            let current_file = CurrentFile::new(FileId::new(0), src.len());
            let ty = Parser::new(src, current_file, string_interner.clone())
                .ascribed_type()
                .unwrap();

            assert_eq!(ty.data(), correct);
        }
    }

    #[test]
    fn paren() {
        let types = [
            (
                "(str)",
                Type::Parenthesised(Box::new(Locatable {
                    data: Type::Builtin(BuiltinType::String),
                    loc: Location::concrete(Span::from(1..4), FileId::new(0)),
                })),
            ),
            (
                "(rune)",
                Type::Parenthesised(Box::new(Locatable {
                    data: Type::Builtin(BuiltinType::Rune),
                    loc: Location::concrete(Span::from(1..5), FileId::new(0)),
                })),
            ),
        ];
        let string_interner = Interner::new();

        for (src, correct) in types.iter() {
            let current_file = CurrentFile::new(FileId::new(0), src.len());
            let ty = Parser::new(src, current_file, string_interner.clone())
                .ascribed_type()
                .unwrap();

            assert_eq!(ty.data(), correct);
        }
    }

    #[test]
    fn operands() {
        let types = [
            (
                "str & rune",
                Type::Operand(
                    Box::new(Locatable {
                        data: Type::Builtin(BuiltinType::String),
                        loc: Location::concrete(Span::from(0..3), FileId::new(0)),
                    }),
                    TypeOperand::And,
                    Box::new(Locatable {
                        data: Type::Builtin(BuiltinType::Rune),
                        loc: Location::concrete(Span::from(6..10), FileId::new(0)),
                    }),
                ),
            ),
            (
                "str | rune",
                Type::Operand(
                    Box::new(Locatable {
                        data: Type::Builtin(BuiltinType::String),
                        loc: Location::concrete(Span::from(0..3), FileId::new(0)),
                    }),
                    TypeOperand::Or,
                    Box::new(Locatable {
                        data: Type::Builtin(BuiltinType::Rune),
                        loc: Location::concrete(Span::from(6..10), FileId::new(0)),
                    }),
                ),
            ),
        ];
        let string_interner = Interner::new();

        for (src, correct) in types.iter() {
            let current_file = CurrentFile::new(FileId::new(0), src.len());
            let ty = Parser::new(src, current_file, string_interner.clone())
                .ascribed_type()
                .unwrap();

            assert_eq!(ty.data(), correct);
        }
    }

    #[cfg(not(any(target_arch = "wasm32", miri)))]
    mod proptests {
        use super::*;
        use proptest::prelude::*;

        proptest! {
            #[test]
            fn int_types(s in r#"i[0-9]+"#) {
                let mut parser = Parser::new(&s, CurrentFile::new(FileId(0), s.len()), Interner::default());

                let ty = parser.ascribed_type();
                match ty.as_ref().map(|t| t.data()) {
                    Ok(Type::Builtin(BuiltinType::Integer { sign: Signedness::Signed, .. })) => {},
                    Err(Locatable { data: _data @ Error::Syntax(SyntaxError::Generic(..)), .. }) => {}

                    _ => prop_assert!(false),
                }
            }

            #[test]
            fn uint_types(s in r#"u[0-9]+"#) {
                let mut parser = Parser::new(&s, CurrentFile::new(FileId(0), s.len()), Interner::default());

                let ty = parser.ascribed_type();
                match ty.as_ref().map(|t| t.data()) {
                    Ok(Type::Builtin(BuiltinType::Integer { sign: Signedness::Unsigned, .. })) => {},
                    Err(Locatable { data: _data @ Error::Syntax(SyntaxError::Generic(..)), .. }) => {}

                    _ => prop_assert!(false),
                }
            }

            #[test]
            fn float(s in r#"f[0-9]+"#) {
                let mut parser = Parser::new(&s, CurrentFile::new(FileId(0), s.len()), Interner::default());

                let ty = parser.ascribed_type();
                match ty.as_ref().map(|t| t.data()) {
                    Ok(Type::Builtin(BuiltinType::Float { .. })) => {},
                    Err(Locatable { data: _data @ Error::Syntax(SyntaxError::Generic(..)), .. }) => {}

                    _ => prop_assert!(false),
                }
            }
        }
    }
}
