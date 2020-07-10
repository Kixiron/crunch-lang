use crate::{
    parser::{string_escapes, Parser},
    token::{Token, TokenType},
};
use alloc::{format, rc::Rc, string::ToString, vec, vec::Vec};
use crunch_shared::{
    crunch_proc::recursion_guard,
    error::{Error, Locatable, Location, ParseResult, Span, SyntaxError, Warning},
    files::CurrentFile,
    strings::StrT,
    trees::{
        ast::{AssignKind, BinaryOp, CompOp, Float, Integer, Literal, Sign, UnaryOp},
        ItemPath,
    },
};

#[derive(Debug, Clone)]
pub struct StackGuard(Rc<()>);

impl StackGuard {
    pub fn new() -> Self {
        Self(Rc::new(()))
    }

    pub fn frames(&self) -> usize {
        Rc::strong_count(&self.0)
    }
}

impl<'src> Parser<'src> {
    /// ```ebnf
    /// ItemPath ::= Ident | Ident '.' Path
    /// ```
    #[recursion_guard]
    pub(crate) fn item_path(&mut self, start: StrT) -> ParseResult<ItemPath> {
        let mut path = vec![start];

        if matches!(self.peek().map(|t| t.ty()), Ok(TokenType::Dot)) {
            self.eat(TokenType::Dot, [])?;
        } else {
            return Ok(ItemPath::new(path));
        }

        if let Ok(peek) = self.peek() {
            while peek.ty() == TokenType::Ident {
                let segment = self.eat(TokenType::Ident, [TokenType::Newline])?.source();
                path.push(self.context.strings.intern(segment));

                if matches!(self.peek().map(|t| t.ty()), Ok(TokenType::Dot)) {
                    self.eat(TokenType::Dot, [TokenType::Newline])?;
                } else {
                    break;
                }
            }
        }

        Ok(ItemPath::new(path))
    }

    #[recursion_guard]
    pub(crate) fn literal(&mut self, token: &Token<'_>, file: CurrentFile) -> ParseResult<Literal> {
        let mut source: &str = &token.source();
        // TODO: Const this
        let format = lexical_core::NumberFormat::ignore(b'_').unwrap();

        match token.ty() {
            TokenType::Float => {
                if token.source() == "inf" {
                    return Ok(Literal::Float(Float(f64::to_bits(core::f64::INFINITY))));
                } else if token.source() == "NaN" {
                    return Ok(Literal::Float(Float(f64::to_bits(core::f64::NAN))));
                }

                let negative = match source.chars().next() {
                    Some('-') => {
                        source = &source[1..];
                        true
                    }
                    Some('+') => {
                        source = &source[1..];
                        false
                    }
                    _ => false,
                };

                // TODO: Make this detect longer strings of `_`s
                // TODO: Factor this out into a function
                for (idx, _) in source.match_indices("__") {
                    let span = Span::new(token.range().start + idx, token.range().start + idx + 2);

                    self.error_handler.push_warning(Locatable::new(
                        Warning::TooManyUnderscores,
                        Location::new(span, self.current_file),
                    ));
                }

                let mut float = if source.chars().take(2).eq(['0', 'x'].iter().copied()) {
                    lexical_core::parse_format_radix::<f64>(source[2..].as_bytes(), 16, format)
                        .map_err(|_| {
                            Locatable::new(
                                Error::Syntax(SyntaxError::InvalidLiteral("float".to_string())),
                                Location::new(token, file),
                            )
                        })?
                } else {
                    lexical_core::parse_format(source.as_bytes(), format).map_err(|_| {
                        Locatable::new(
                            Error::Syntax(SyntaxError::InvalidLiteral("float".to_string())),
                            Location::new(token, file),
                        )
                    })?
                };

                if negative {
                    float = -float;
                }

                Ok(Literal::Float(Float(f64::to_bits(float))))
            }

            TokenType::Rune => {
                let byte_rune = if source.chars().next() == Some('b') {
                    source = &source[1..];
                    true
                } else {
                    false
                };

                let rune = if let Some('\'') = source.chars().next() {
                    string_escapes::unescape_rune(source[1..].chars()).map_err(|(err, range)| {
                        Locatable::new(
                            err,
                            Location::new(
                                (
                                    token.range().start + 3 + range.start,
                                    token.range().start + 3 + range.end,
                                ),
                                file,
                            ),
                        )
                    })?
                } else {
                    unreachable!()
                };

                if byte_rune {
                    Ok(Literal::Integer(Integer {
                        sign: Sign::Positive,
                        bits: rune.as_u32() as u128,
                    }))
                } else {
                    Ok(Literal::Rune(rune))
                }
            }

            TokenType::String => {
                let byte_str = if source.chars().next() == Some('b') {
                    source = &source[1..];
                    true
                } else {
                    false
                };

                let string = match (source.chars().next(), source.chars().last()) {
                    (Some('"'), Some('"')) => {
                        string_escapes::unescape_string(source[1..source.len() - 1].chars())
                            .map_err(|(err, range)| {
                                Locatable::new(
                                    err,
                                    Location::new(
                                        (
                                            token.range().start + 1 + range.start,
                                            token.range().start + 1 + range.end,
                                        ),
                                        file,
                                    ),
                                )
                            })?
                    }

                    _ => unreachable!(),
                };

                if byte_str {
                    Ok(Literal::Array(
                        string
                            .to_bytes()
                            .into_iter()
                            .map(|b| {
                                Literal::Integer(Integer {
                                    sign: Sign::Positive,
                                    bits: b as u128,
                                })
                            })
                            .collect(),
                    ))
                } else {
                    Ok(Literal::String(string))
                }
            }

            TokenType::Int => {
                let sign = match source.chars().next() {
                    Some('-') => {
                        source = &source[1..];
                        Sign::Negative
                    }
                    Some('+') => {
                        source = &source[1..];
                        Sign::Positive
                    }
                    _ => Sign::Positive,
                };

                // TODO: Make this detect longer strings of `_`s
                // TODO: Factor this out into a function
                for (idx, _) in source.match_indices("__") {
                    let span = Span::new(token.range().start + idx, token.range().start + idx + 2);

                    self.error_handler.push_warning(Locatable::new(
                        Warning::TooManyUnderscores,
                        Location::new(span, self.current_file),
                    ));
                }

                let int = if source.chars().take(2).eq(['0', 'x'].iter().copied()) {
                    lexical_core::parse_format_radix::<u128>(source[2..].as_bytes(), 16, format)
                        .map_err(|_| {
                            Locatable::new(
                                Error::Syntax(SyntaxError::InvalidLiteral("int".to_string())),
                                Location::new(token, file),
                            )
                        })?
                } else if source.chars().take(2).eq(['0', 'b'].iter().copied()) {
                    lexical_core::parse_format_radix::<u128>(source[2..].as_bytes(), 2, format)
                        .map_err(|_| {
                            Locatable::new(
                                Error::Syntax(SyntaxError::InvalidLiteral("int".to_string())),
                                Location::new(token, file),
                            )
                        })?
                } else {
                    lexical_core::parse_format_radix::<u128>(source.as_bytes(), 10, format)
                        .map_err(|_| {
                            Locatable::new(
                                Error::Syntax(SyntaxError::InvalidLiteral("int".to_string())),
                                Location::new(token, file),
                            )
                        })?
                };

                Ok(Literal::Integer(Integer { sign, bits: int }))
            }

            TokenType::Bool => Ok(Literal::Bool(token.source().parse::<bool>().map_err(
                |_| {
                    Locatable::new(
                        Error::Syntax(SyntaxError::InvalidLiteral("bool".to_string())),
                        Location::new(token, file),
                    )
                },
            )?)),

            ty => Err(Locatable::new(
                Error::Syntax(SyntaxError::Generic(format!("Invalid Literal: '{}'", ty))),
                Location::new(token, file),
            )),
        }
    }

    #[recursion_guard]
    pub(crate) fn assign_kind(
        &self,
        token: &Token<'_>,
        file: CurrentFile,
    ) -> ParseResult<AssignKind> {
        const ASSIGN_TOKENS: &[TokenType] = &[
            TokenType::Equal,
            TokenType::AddAssign,
            TokenType::SubAssign,
            TokenType::MultAssign,
            TokenType::DivAssign,
            TokenType::ModAssign,
            TokenType::PowAssign,
            TokenType::ShlAssign,
            TokenType::ShrAssign,
            TokenType::OrAssign,
            TokenType::AndAssign,
            TokenType::XorAssign,
        ];

        let kind = match token.ty() {
            TokenType::Equal => AssignKind::Normal,
            TokenType::AddAssign => AssignKind::BinaryOp(BinaryOp::Add),
            TokenType::SubAssign => AssignKind::BinaryOp(BinaryOp::Sub),
            TokenType::MultAssign => AssignKind::BinaryOp(BinaryOp::Mult),
            TokenType::DivAssign => AssignKind::BinaryOp(BinaryOp::Div),
            TokenType::ModAssign => AssignKind::BinaryOp(BinaryOp::Mod),
            TokenType::PowAssign => AssignKind::BinaryOp(BinaryOp::Pow),
            TokenType::ShlAssign => AssignKind::BinaryOp(BinaryOp::Shl),
            TokenType::ShrAssign => AssignKind::BinaryOp(BinaryOp::Shr),
            TokenType::OrAssign => AssignKind::BinaryOp(BinaryOp::BitOr),
            TokenType::AndAssign => AssignKind::BinaryOp(BinaryOp::BitAnd),
            TokenType::XorAssign => AssignKind::BinaryOp(BinaryOp::BitXor),

            ty => {
                return Err(Locatable::new(
                    Error::Syntax(SyntaxError::Generic(format!(
                        "Expected one of {}, got '{}'",
                        ASSIGN_TOKENS
                            .iter()
                            .map(|t| t.to_str())
                            .collect::<Vec<_>>()
                            .join(", "),
                        ty,
                    ))),
                    Location::new(token, file),
                ));
            }
        };

        Ok(kind)
    }

    #[recursion_guard]
    pub(crate) fn comp_op(&self, token: &Token<'_>, file: CurrentFile) -> ParseResult<CompOp> {
        const COMPARE_TOKENS: &[TokenType] = &[
            TokenType::RightCaret,
            TokenType::LeftCaret,
            TokenType::GreaterThanEqual,
            TokenType::LessThanEqual,
            TokenType::IsEqual,
            TokenType::IsNotEqual,
        ];

        let op = match token.ty() {
            TokenType::RightCaret => CompOp::Greater,
            TokenType::LeftCaret => CompOp::Less,
            TokenType::GreaterThanEqual => CompOp::GreaterEqual,
            TokenType::LessThanEqual => CompOp::LessEqual,
            TokenType::IsEqual => CompOp::Equal,
            TokenType::IsNotEqual => CompOp::NotEqual,

            ty => {
                return Err(Locatable::new(
                    Error::Syntax(SyntaxError::Generic(format!(
                        "Expected one of {}, got '{}'",
                        COMPARE_TOKENS
                            .iter()
                            .map(|t| t.to_str())
                            .collect::<Vec<_>>()
                            .join(", "),
                        ty,
                    ))),
                    Location::new(token, file),
                ));
            }
        };

        Ok(op)
    }

    #[recursion_guard]
    pub(crate) fn bin_op(&self, token: &Token<'_>, file: CurrentFile) -> ParseResult<BinaryOp> {
        let op = match token.ty() {
            TokenType::Plus => BinaryOp::Add,
            TokenType::Minus => BinaryOp::Sub,
            TokenType::Star => BinaryOp::Mult,
            TokenType::Divide => BinaryOp::Div,
            TokenType::Modulo => BinaryOp::Mod,
            TokenType::DoubleStar => BinaryOp::Pow,
            TokenType::Ampersand => BinaryOp::BitAnd,
            TokenType::Pipe => BinaryOp::BitOr,
            TokenType::Caret => BinaryOp::BitXor,
            TokenType::Shl => BinaryOp::Shl,
            TokenType::Shr => BinaryOp::Shr,

            ty => {
                return Err(Locatable::new(
                    Error::Syntax(SyntaxError::Generic(format!(
                        "Expected a binary operand, got `{}`",
                        ty
                    ))),
                    Location::new(token, file),
                ));
            }
        };

        Ok(op)
    }

    #[recursion_guard]
    pub(crate) fn unary_op(&self, token: &Token<'_>, file: CurrentFile) -> ParseResult<UnaryOp> {
        let op = match token.ty() {
            TokenType::Plus => UnaryOp::Positive,
            TokenType::Minus => UnaryOp::Negative,
            TokenType::Bang => UnaryOp::Not,

            ty => {
                return Err(Locatable::new(
                    Error::Syntax(SyntaxError::Generic(format!(
                        "Expected a unary operand, got `{}`",
                        ty
                    ))),
                    Location::new(token, file),
                ));
            }
        };

        Ok(op)
    }
}
