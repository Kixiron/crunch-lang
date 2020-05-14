use crate::{
    error::{Error, Locatable, Location, ParseResult, SyntaxError},
    parser::{string_escapes, CurrentFile, Parser},
    token::{Token, TokenType},
};

use crunch_proc::recursion_guard;
use lasso::Spur;
use stadium::Ticket;

use alloc::{
    format,
    string::{String, ToString},
    vec::Vec,
};
use core::{convert::TryFrom, fmt, iter::FromIterator};

pub type Expr<'expr> = Ticket<'expr, Expression<'expr>>;

#[derive(Debug, Clone, PartialEq)]
pub enum Expression<'expr> {
    Variable(Spur),
    UnaryExpr(UnaryOperand, Expr<'expr>),
    BinaryOp(Expr<'expr>, BinaryOperand, Expr<'expr>),
    InlineConditional {
        true_arm: Expr<'expr>,
        condition: Expr<'expr>,
        false_arm: Expr<'expr>,
    },
    Parenthesised(Expr<'expr>),
    FunctionCall {
        caller: Expr<'expr>,
        arguments: Vec<Expr<'expr>>,
    },
    MemberFunctionCall {
        member: Expr<'expr>,
        function: Expr<'expr>,
    },
    Literal(Literal),
    Comparison(Expr<'expr>, ComparisonOperand, Expr<'expr>),
    IndexArray {
        array: Expr<'expr>,
        index: Expr<'expr>,
    },
    Array(Vec<Expr<'expr>>),
    Tuple(Vec<Expr<'expr>>),
    Assignment(Expr<'expr>, AssignmentType, Expr<'expr>),
    Range(Expr<'expr>, Expr<'expr>),
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum AssignmentType {
    Normal,
    BinaryOp(BinaryOperand),
}

impl<'src> TryFrom<(&Token<'src>, CurrentFile)> for AssignmentType {
    type Error = Locatable<Error>;

    fn try_from((token, file): (&Token<'src>, CurrentFile)) -> Result<Self, Self::Error> {
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

        Ok(match token.ty() {
            TokenType::Equal => Self::Normal,
            TokenType::AddAssign => Self::BinaryOp(BinaryOperand::Add),
            TokenType::SubAssign => Self::BinaryOp(BinaryOperand::Sub),
            TokenType::MultAssign => Self::BinaryOp(BinaryOperand::Mult),
            TokenType::DivAssign => Self::BinaryOp(BinaryOperand::Div),
            TokenType::ModAssign => Self::BinaryOp(BinaryOperand::Mod),
            TokenType::PowAssign => Self::BinaryOp(BinaryOperand::Pow),
            TokenType::ShlAssign => Self::BinaryOp(BinaryOperand::Shl),
            TokenType::ShrAssign => Self::BinaryOp(BinaryOperand::Shr),
            TokenType::OrAssign => Self::BinaryOp(BinaryOperand::BitOr),
            TokenType::AndAssign => Self::BinaryOp(BinaryOperand::BitAnd),
            TokenType::XorAssign => Self::BinaryOp(BinaryOperand::BitXor),

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
        })
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum ComparisonOperand {
    Greater,
    Less,
    GreaterEqual,
    LessEqual,
    Equal,
    NotEqual,
}

impl<'src> TryFrom<(&Token<'src>, CurrentFile)> for ComparisonOperand {
    type Error = Locatable<Error>;

    fn try_from((token, file): (&Token<'src>, CurrentFile)) -> Result<Self, Self::Error> {
        const COMPARE_TOKENS: &[TokenType] = &[
            TokenType::RightCaret,
            TokenType::LeftCaret,
            TokenType::GreaterThanEqual,
            TokenType::LessThanEqual,
            TokenType::IsEqual,
            TokenType::IsNotEqual,
        ];

        Ok(match token.ty() {
            TokenType::RightCaret => Self::Greater,
            TokenType::LeftCaret => Self::Less,
            TokenType::GreaterThanEqual => Self::GreaterEqual,
            TokenType::LessThanEqual => Self::LessEqual,
            TokenType::IsEqual => Self::Equal,
            TokenType::IsNotEqual => Self::NotEqual,

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
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Integer(Integer),
    Bool(bool),
    String(Text),
    Rune(Rune),
    Float(Float),
    Array(Vec<Literal>),
}

impl Literal {
    pub fn into_integer(self) -> Option<Integer> {
        if let Self::Integer(int) = self {
            Some(int)
        } else {
            None
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
#[repr(transparent)]
pub struct Text(Vec<Rune>);

impl Text {
    pub fn new(runes: Vec<Rune>) -> Self {
        Self(runes)
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        self.to_string().into_bytes()
    }
}

impl fmt::Debug for Text {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{:?}",
            &String::from_iter(
                self.0
                    .iter()
                    .map(|r| core::char::from_u32(r.as_u32()).unwrap()),
            )
        )
    }
}

impl fmt::Display for Text {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            String::from_iter(
                self.0
                    .iter()
                    .map(|r| core::char::from_u32(r.as_u32()).unwrap())
            )
        )
    }
}

impl From<&str> for Text {
    fn from(string: &str) -> Self {
        Self(string.chars().map(Rune::from).collect())
    }
}

#[derive(Copy, Clone, PartialEq, Eq)]
#[repr(transparent)]
pub struct Rune(u32);

impl Rune {
    pub fn as_u32(self) -> u32 {
        self.0
    }

    pub fn from_u32(i: u32) -> Option<Self> {
        core::char::from_u32(i).map(|i| Self(i as u32))
    }

    pub fn as_char(self) -> char {
        core::char::from_u32(self.0).unwrap()
    }

    pub fn from_char(i: char) -> Self {
        Self(i as u32)
    }
}

impl PartialEq<char> for Rune {
    fn eq(&self, other: &char) -> bool {
        self.as_u32() == *other as u32
    }
}

impl From<char> for Rune {
    fn from(c: char) -> Self {
        Self(c as u32)
    }
}

impl From<u32> for Rune {
    fn from(i: u32) -> Self {
        Self(i)
    }
}

impl fmt::Debug for Rune {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", &self.as_char())
    }
}

impl fmt::Display for Rune {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", &self.as_char())
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Integer {
    pub sign: Sign,
    pub bits: u128,
}

impl fmt::Display for Integer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", &self.sign, &self.bits)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Sign {
    Positive,
    Negative,
}

impl Sign {
    pub fn is_negative(self) -> bool {
        self == Self::Negative
    }
}

impl fmt::Display for Sign {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Positive => "",
                Self::Negative => "-",
            },
        )
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Float {
    F32(f32),
    F64(f64),
}

impl fmt::Display for Float {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::F32(fl) => write!(f, "{}", fl),
            Self::F64(fl) => write!(f, "{}", fl),
        }
    }
}

// TODO: Actually make this throw useful errors
// FIXME: Not unicode-aware, will panic on unicode boundaries
// TODO: Make these errors actually helpful, and verify that the parsing matches with what is lexed
impl<'src> TryFrom<(&Token<'src>, CurrentFile)> for Literal {
    type Error = Locatable<Error>;

    fn try_from((token, file): (&Token<'src>, CurrentFile)) -> Result<Self, Self::Error> {
        let mut chars: Vec<char> = token.source().chars().filter(|c| *c != '_').collect();

        match token.ty() {
            TokenType::Float => {
                if token.source() == "inf" {
                    return Ok(Literal::Float(Float::F64(core::f64::INFINITY)));
                } else if token.source() == "NaN" {
                    return Ok(Literal::Float(Float::F64(core::f64::NAN)));
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
                                        "float",
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
                                    "float",
                                    format!("'{}' is too large, only floats of up to 64 bits are supported", token.source()),
                                )),
                                Location::concrete(token, file),
                            ),
                            ErrorCode::Underflow => Locatable::new(
                                Error::Syntax(SyntaxError::LiteralUnderflow(
                                    "float",
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

                Ok(Literal::Float(Float::F64(float)))
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
                            Error::Syntax(SyntaxError::InvalidLiteral("int")),
                            Location::concrete(token, file),
                        )
                    })?
                } else if chars.get(..2) == Some(&['0', 'b']) {
                    let string = chars[2..].iter().collect::<String>();

                    u128::from_str_radix(&string, 2).map_err(|_| {
                        Locatable::new(
                            Error::Syntax(SyntaxError::InvalidLiteral("int")),
                            Location::concrete(token, file),
                        )
                    })?
                } else {
                    let string = chars.into_iter().collect::<String>();

                    u128::from_str_radix(&string, 10).map_err(|_| {
                        Locatable::new(
                            Error::Syntax(SyntaxError::InvalidLiteral("int")),
                            Location::concrete(token, file),
                        )
                    })?
                };

                Ok(Literal::Integer(Integer { sign, bits: int }))
            }

            TokenType::Bool => Ok(Self::Bool(token.source().parse::<bool>().map_err(
                |_| {
                    Locatable::new(
                        Error::Syntax(SyntaxError::InvalidLiteral("bool")),
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
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum BinaryOperand {
    Add,
    Sub,
    Mult,
    Div,
    Mod,
    Pow,
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,
}

impl<'src> TryFrom<(&Token<'src>, CurrentFile)> for BinaryOperand {
    type Error = Locatable<Error>;

    fn try_from((token, file): (&Token<'src>, CurrentFile)) -> Result<Self, Self::Error> {
        Ok(match token.ty() {
            TokenType::Plus => Self::Add,
            TokenType::Minus => Self::Sub,
            TokenType::Star => Self::Mult,
            TokenType::Divide => Self::Div,
            TokenType::Modulo => Self::Mod,
            TokenType::DoubleStar => Self::Pow,
            TokenType::Ampersand => Self::BitAnd,
            TokenType::Pipe => Self::BitOr,
            TokenType::Caret => Self::BitXor,
            TokenType::Shl => Self::Shl,
            TokenType::Shr => Self::Shr,

            ty => {
                return Err(Locatable::new(
                    Error::Syntax(SyntaxError::Generic(format!(
                        "Expected a binary operand, got `{}`",
                        ty
                    ))),
                    Location::concrete(token, file),
                ));
            }
        })
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum UnaryOperand {
    Positive,
    Negative,
    Not,
}

impl<'src> TryFrom<(&Token<'src>, CurrentFile)> for UnaryOperand {
    type Error = Locatable<Error>;

    fn try_from((token, file): (&Token<'src>, CurrentFile)) -> Result<Self, Self::Error> {
        Ok(match token.ty() {
            TokenType::Plus => Self::Positive,
            TokenType::Minus => Self::Negative,
            TokenType::Bang => Self::Not,

            ty => {
                return Err(Locatable::new(
                    Error::Syntax(SyntaxError::Generic(format!(
                        "Expected a unary operand, got `{}`",
                        ty
                    ))),
                    Location::concrete(token, file),
                ));
            }
        })
    }
}

type PrefixParselet<'src, 'expr, 'stmt> =
    fn(&mut Parser<'src, 'expr, 'stmt>, Token<'src>) -> ParseResult<Expr<'expr>>;
type PostfixParselet<'src, 'expr, 'stmt> =
    fn(&mut Parser<'src, 'expr, 'stmt>, Token<'src>, Expr<'expr>) -> ParseResult<Expr<'expr>>;
type InfixParselet<'src, 'expr, 'stmt> =
    fn(&mut Parser<'src, 'expr, 'stmt>, Token<'src>, Expr<'expr>) -> ParseResult<Expr<'expr>>;

/// Expression Parsing
impl<'src, 'expr, 'stmt> Parser<'src, 'expr, 'stmt> {
    #[recursion_guard]
    pub fn expr(&mut self) -> ParseResult<Expr<'expr>> {
        self.parse_expression(0)
    }

    #[recursion_guard]
    fn parse_expression(&mut self, precedence: usize) -> ParseResult<Expr<'expr>> {
        let mut token = self.next()?;

        let prefix = Self::expr_prefix(token);
        if let Some(prefix) = prefix {
            let mut left = prefix(self, token)?;

            if let Ok(peek) = self.peek() {
                let postfix = Self::expr_postfix(peek);
                if let Some(postfix) = postfix {
                    token = self.next()?;
                    left = postfix(self, token, left)?;
                }
            }

            while precedence < self.current_precedence() {
                token = self.next()?;

                let infix = Self::expr_infix(token);
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

    fn expr_prefix(token: Token) -> Option<PrefixParselet<'src, 'expr, 'stmt>> {
        let prefix: PrefixParselet = match token.ty() {
            // Array and tuple literals
            TokenType::Ident if token.source() == "arr" || token.source() == "tup" => {
                |parser, token| {
                    let _frame = parser.add_stack_frame()?;

                    parser.eat(TokenType::LeftBrace, [TokenType::Newline])?;

                    let mut elements = Vec::with_capacity(5);
                    while parser.peek()?.ty() != TokenType::RightBrace {
                        let elm = parser.expr()?;
                        elements.push(elm);

                        if parser.peek()?.ty() == TokenType::Comma {
                            parser.eat(TokenType::Comma, [TokenType::Newline])?;
                        } else {
                            break;
                        }
                    }

                    elements.shrink_to_fit();
                    parser.eat(TokenType::RightBrace, [TokenType::Newline])?;

                    let expr = if token.source() == "arr" {
                        Expression::Array(elements)
                    } else {
                        Expression::Tuple(elements)
                    };
                    let expr = parser.expr_arena.store(expr);

                    Ok(expr)
                }
            }

            // Variables
            TokenType::Ident => |parser, token| {
                use alloc::borrow::Cow;
                use unicode_normalization::{IsNormalized, UnicodeNormalization};

                let _frame = parser.add_stack_frame()?;

                // Performs zero temp allocations if it's already NFKC-normalised.
                let normalized = match unicode_normalization::is_nfkc_quick(token.source().chars())
                {
                    IsNormalized::Yes => Cow::Borrowed(token.source()),
                    _ => Cow::Owned(token.source().nfkc().collect()),
                };

                let ident = parser.string_interner.intern(&normalized);
                let expr = parser.expr_arena.store(Expression::Variable(ident));

                Ok(expr)
            },

            // Literals
            TokenType::Int
            | TokenType::Bool
            | TokenType::Float
            | TokenType::String
            | TokenType::Rune => |parser, lit| {
                let expr = parser
                    .expr_arena
                    .store(Expression::Literal(Literal::try_from((
                        &lit,
                        parser.current_file,
                    ))?));

                Ok(expr)
            },

            // Prefix Operators
            TokenType::Minus | TokenType::Bang | TokenType::Plus => |parser, token| {
                let _frame = parser.add_stack_frame()?;
                let operand = parser.expr()?;
                let expr = parser.expr_arena.store(Expression::UnaryExpr(
                    UnaryOperand::try_from((&token, parser.current_file))?,
                    operand,
                ));

                Ok(expr)
            },

            // Grouping via parentheses
            TokenType::LeftParen => |parser, _paren| {
                let _frame = parser.add_stack_frame()?;

                let expr = parser.expr()?;
                parser.eat(TokenType::RightParen, [TokenType::Newline])?;
                let expr = parser.expr_arena.store(Expression::Parenthesised(expr));

                Ok(expr)
            },

            _ => return None,
        };

        Some(prefix)
    }

    fn expr_postfix(token: Token) -> Option<PostfixParselet<'src, 'expr, 'stmt>> {
        let postfix: PostfixParselet = match token.ty() {
            // Function calls
            TokenType::LeftParen => |parser, _left_paren, caller| {
                let _frame = parser.add_stack_frame()?;

                let mut arguments = Vec::with_capacity(5);

                while parser.peek()?.ty() != TokenType::RightParen {
                    let arg = parser.expr()?;
                    arguments.push(arg);

                    if parser.peek()?.ty() == TokenType::Comma {
                        parser.eat(TokenType::Comma, [TokenType::Newline])?;
                    } else {
                        break;
                    }
                }

                arguments.shrink_to_fit();
                parser.eat(TokenType::RightParen, [TokenType::Newline])?;
                let expr = parser
                    .expr_arena
                    .store(Expression::FunctionCall { caller, arguments });

                Ok(expr)
            },

            // Dotted function calls
            TokenType::Dot => |parser, _dot, member| {
                let _frame = parser.add_stack_frame()?;
                let function = parser.expr()?;
                let expr = parser
                    .expr_arena
                    .store(Expression::MemberFunctionCall { member, function });

                Ok(expr)
            },

            // Ranges
            TokenType::DoubleDot => |parser, _, start| {
                let _frame = parser.add_stack_frame()?;
                let end = parser.expr()?;
                let expr = parser.expr_arena.store(Expression::Range(start, end));

                Ok(expr)
            },

            // Array indexing
            TokenType::LeftBrace => Self::index_array,

            // Assignments
            TokenType::AddAssign
            | TokenType::SubAssign
            | TokenType::MultAssign
            | TokenType::DivAssign
            | TokenType::ModAssign
            | TokenType::PowAssign
            | TokenType::ShlAssign
            | TokenType::ShrAssign
            | TokenType::OrAssign
            | TokenType::AndAssign
            | TokenType::XorAssign => |parser, assign, left| {
                let _frame = parser.add_stack_frame()?;

                let assign = AssignmentType::try_from((&assign, parser.current_file))?;
                let right = parser.expr()?;
                let expr = parser
                    .expr_arena
                    .store(Expression::Assignment(left, assign, right));

                Ok(expr)
            },

            TokenType::Colon => |parser, _colon, left| {
                let _frame = parser.add_stack_frame()?;

                let equal = parser.eat(TokenType::Equal, [TokenType::Newline])?;
                let assign = AssignmentType::try_from((&equal, parser.current_file))?;
                let right = parser.expr()?;
                let expr = parser
                    .expr_arena
                    .store(Expression::Assignment(left, assign, right));

                Ok(expr)
            },

            _ => return None,
        };

        Some(postfix)
    }

    fn expr_infix(token: Token) -> Option<InfixParselet<'src, 'expr, 'stmt>> {
        let infix: InfixParselet = match token.ty() {
            // Binary Operations
            TokenType::Plus
            | TokenType::Minus
            | TokenType::Star
            | TokenType::Divide
            | TokenType::Modulo
            | TokenType::DoubleStar
            | TokenType::Ampersand
            | TokenType::Pipe
            | TokenType::Caret
            | TokenType::Shl
            | TokenType::Shr => |parser, operand, left| {
                let _frame = parser.add_stack_frame()?;
                let right = parser.expr()?;
                let expr = parser.expr_arena.store(Expression::BinaryOp(
                    left,
                    BinaryOperand::try_from((&operand, parser.current_file))?,
                    right,
                ));

                Ok(expr)
            },

            // Comparisons
            TokenType::RightCaret
            | TokenType::LeftCaret
            | TokenType::GreaterThanEqual
            | TokenType::LessThanEqual
            | TokenType::IsEqual
            | TokenType::IsNotEqual => |parser, comparison, left| {
                let _frame = parser.add_stack_frame()?;
                let right = parser.expr()?;
                let expr = parser.expr_arena.store(Expression::Comparison(
                    left,
                    ComparisonOperand::try_from((&comparison, parser.current_file))?,
                    right,
                ));

                Ok(expr)
            },

            // <ret> if <cond> else <cond>
            TokenType::If => |parser, _, true_arm| {
                let _frame = parser.add_stack_frame()?;

                let condition = parser.expr()?;
                parser.eat(TokenType::Else, [TokenType::Newline])?;
                let false_arm = parser.expr()?;
                let expr = parser.expr_arena.store(Expression::InlineConditional {
                    true_arm,
                    condition,
                    false_arm,
                });

                Ok(expr)
            },

            // Array indexing
            TokenType::LeftBrace => Self::index_array,

            _ => return None,
        };

        Some(infix)
    }

    #[recursion_guard]
    fn index_array(
        &mut self,
        _left_bracket: Token<'src>,
        array: Expr<'expr>,
    ) -> ParseResult<Expr<'expr>> {
        let index = self.expr()?;
        self.eat(TokenType::RightBrace, [TokenType::Newline])?;
        let expr = self
            .expr_arena
            .store(Expression::IndexArray { array, index });

        Ok(expr)
    }
}
