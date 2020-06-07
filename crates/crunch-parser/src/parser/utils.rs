use crate::{
    parser::{string_escapes, Parser},
    token::{Token, TokenType},
};
use alloc::{
    format,
    rc::Rc,
    string::{String, ToString},
    vec,
    vec::Vec,
};
use crunch_proc::recursion_guard;
use crunch_shared::{
    error::{Error, Locatable, Location, ParseResult, Span, SyntaxError},
    files::FileId,
    strings::StrT,
    trees::ast::{AssignKind, BinaryOp, CompOp, Float, Integer, ItemPath, Literal, Sign, UnaryOp},
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
    pub(crate) fn literal(&self, token: &Token<'_>, file: CurrentFile) -> ParseResult<Literal> {
        let mut chars: Vec<char> = token.source().chars().filter(|c| *c != '_').collect();

        match token.ty() {
            TokenType::Float => {
                if token.source() == "inf" {
                    return Ok(Literal::Float(Float(f64::to_bits(core::f64::INFINITY))));
                } else if token.source() == "NaN" {
                    return Ok(Literal::Float(Float(f64::to_bits(core::f64::NAN))));
                }

                let negative = if chars.get(0).copied() == Some('-') {
                    chars.remove(0);
                    true
                } else if chars.get(0).copied() == Some('+') {
                    chars.remove(0);
                    false
                } else {
                    false
                };

                let mut float = if chars.get(..2) == Some(&['0', 'x']) {
                    use hexponent::FloatLiteral;

                    let float = chars
                        .into_iter()
                        .collect::<String>()
                        .parse::<FloatLiteral>()
                        .map_err(|err| {
                            use hexponent::ParseErrorKind;

                            match err.kind {
                                ParseErrorKind::ExponentOverflow => Locatable::new(
                                    Error::Syntax(SyntaxError::LiteralOverflow(
                                        "float".to_string(),
                                        format!("'{}' is too large, only floats of up to 64 bits are supported", token.source()),
                                    )),
                                    Location::concrete(token, file),
                                ),

                                ParseErrorKind::MissingPrefix
                                | ParseErrorKind::MissingDigits
                                | ParseErrorKind::MissingExponent
                                | ParseErrorKind::MissingEnd => unreachable!(),
                            }
                        })?;

                    float.convert::<f64>().inner()
                } else {
                    let string = chars.into_iter().collect::<String>();

                    lexical_core::parse(string.as_bytes()).map_err(|err| {
                        use lexical_core::ErrorCode;

                        match err.code {
                            ErrorCode::Overflow => Locatable::new(
                                Error::Syntax(SyntaxError::LiteralOverflow(
                                    "float".to_string(),
                                    format!("'{}' is too large, only floats of up to 64 bits are supported", token.source()),
                                )),
                                Location::concrete(token, file),
                            ),
                            ErrorCode::Underflow => Locatable::new(
                                Error::Syntax(SyntaxError::LiteralUnderflow(
                                    "float".to_string(),
                                    format!("'{}' is too small, only floats of up to 64 bits are supported", token.source()),
                                )),
                                Location::concrete(token, file),
                            ),

                            err => unreachable!("Internal error: Failed to handle all float errors (Error: {:?})", err),
                        }
                    })?
                };

                if negative {
                    float = -float;
                }

                Ok(Literal::Float(Float(f64::to_bits(float))))
            }

            TokenType::Rune => {
                let byte_rune = if chars.get(0).copied() == Some('b') {
                    chars.remove(0);
                    true
                } else {
                    false
                };

                let rune = if let Some('\'') = chars.get(0) {
                    chars.drain(..1).for_each(drop);
                    chars.drain(chars.len() - 1..).for_each(drop);

                    string_escapes::unescape_rune(chars).map_err(|(err, range)| {
                        Locatable::new(
                            err,
                            Location::concrete(
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
                let byte_str = if chars.get(0).copied() == Some('b') {
                    chars.remove(0);
                    true
                } else {
                    false
                };

                let string = match chars.get(0) {
                    Some('"') if chars.get(..3) == Some(&['"', '"', '"']) => {
                        chars.drain(..3).for_each(drop);
                        chars.drain(chars.len() - 3..).for_each(drop);

                        string_escapes::unescape_string(chars).map_err(|(err, range)| {
                            Locatable::new(
                                err,
                                Location::concrete(
                                    (
                                        token.range().start + 3 + range.start,
                                        token.range().start + 3 + range.end,
                                    ),
                                    file,
                                ),
                            )
                        })?
                    }

                    Some('"') => {
                        chars.drain(..1).for_each(drop);
                        chars.drain(chars.len() - 1..).for_each(drop);

                        string_escapes::unescape_string(chars).map_err(|(err, range)| {
                            Locatable::new(
                                err,
                                Location::concrete(
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
                let sign = if chars.get(0).copied() == Some('-') {
                    chars.remove(0);
                    Sign::Negative
                } else if chars.get(0).copied() == Some('+') {
                    chars.remove(0);
                    Sign::Positive
                } else {
                    Sign::Positive
                };

                let int = if chars.get(..2) == Some(&['0', 'x']) {
                    let string = chars[2..].iter().collect::<String>();

                    u128::from_str_radix(&string, 16).map_err(|_| {
                        Locatable::new(
                            Error::Syntax(SyntaxError::InvalidLiteral("int".to_string())),
                            Location::concrete(token, file),
                        )
                    })?
                } else if chars.get(..2) == Some(&['0', 'b']) {
                    let string = chars[2..].iter().collect::<String>();

                    u128::from_str_radix(&string, 2).map_err(|_| {
                        Locatable::new(
                            Error::Syntax(SyntaxError::InvalidLiteral("int".to_string())),
                            Location::concrete(token, file),
                        )
                    })?
                } else {
                    let string = chars.into_iter().collect::<String>();

                    u128::from_str_radix(&string, 10).map_err(|_| {
                        Locatable::new(
                            Error::Syntax(SyntaxError::InvalidLiteral("int".to_string())),
                            Location::concrete(token, file),
                        )
                    })?
                };

                Ok(Literal::Integer(Integer { sign, bits: int }))
            }

            TokenType::Bool => Ok(Literal::Bool(token.source().parse::<bool>().map_err(
                |_| {
                    Locatable::new(
                        Error::Syntax(SyntaxError::InvalidLiteral("bool".to_string())),
                        Location::concrete(token, file),
                    )
                },
            )?)),

            ty => Err(Locatable::new(
                Error::Syntax(SyntaxError::Generic(format!("Invalid Literal: '{}'", ty))),
                Location::concrete(token, file),
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
                    Location::concrete(token, file),
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
                    Location::concrete(token, file),
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
                    Location::concrete(token, file),
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
                    Location::concrete(token, file),
                ));
            }
        };

        Ok(op)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct CurrentFile {
    file: FileId,
    length: usize,
    index: usize,
}

impl CurrentFile {
    pub const fn new(file: FileId, length: usize) -> Self {
        Self {
            file,
            length,
            index: 0,
        }
    }

    pub const fn file(&self) -> FileId {
        self.file
    }

    pub const fn length(&self) -> usize {
        self.length
    }

    pub const fn index(&self) -> usize {
        self.index
    }

    pub fn eof(&self) -> Location {
        Location::concrete(Span::new(self.length, self.length), self.file)
    }

    pub fn advance(&mut self, dist: usize) {
        self.index += dist;
    }

    pub const fn index_span(&self) -> Span {
        Span::new(self.index, self.index)
    }

    pub fn recursion(&self) -> Location {
        Location::concrete(self.index_span(), self.file)
    }
}

impl Into<FileId> for CurrentFile {
    fn into(self) -> FileId {
        self.file
    }
}
