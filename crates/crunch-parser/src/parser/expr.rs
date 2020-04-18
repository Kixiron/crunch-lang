use crate::{
    error::{Error, Locatable, Location, ParseResult, SyntaxError},
    files::FileId,
    parser::{string_escapes, Parser},
    token::{Token, TokenType},
};

use lasso::SmallSpur;
use stadium::Ticket;

use alloc::{format, rc::Rc, string::String, vec::Vec};
use core::{convert::TryFrom, fmt, iter::FromIterator, ops::Deref};

pub type Expr<'expr> = Ticket<'expr, Expression<'expr>>;

#[derive(Clone, PartialEq)]
pub enum Expression<'expr> {
    Variable(SmallSpur),
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
    Assignment(Expr<'expr>, AssignmentType, Expr<'expr>),
    Range(Expr<'expr>, Expr<'expr>),
}

#[cfg(not(feature = "no-std"))]
impl<'expr> fmt::Debug for Expression<'expr> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        thread_local! {
            static STACK_GUARD: Rc<()> = Rc::new(());
        }

        STACK_GUARD.with(|guard| {
            let guard = guard.clone();

            if dbg!(Rc::strong_count(&guard)) > 100 {
                return f.debug_struct("...More statements").finish();
            }

            match self {
                Self::Variable(var) => f.debug_tuple("Variable").field(var).finish(),
                Self::UnaryExpr(op, expr) => f
                    .debug_tuple("UnaryExpr")
                    .field(op)
                    .field(expr.deref())
                    .finish(),
                Self::BinaryOp(lhs, op, rhs) => f
                    .debug_tuple("BinaryOp")
                    .field(lhs.deref())
                    .field(op)
                    .field(rhs.deref())
                    .finish(),
                Self::InlineConditional {
                    true_arm,
                    condition,
                    false_arm,
                } => f
                    .debug_struct("BinaryOp")
                    .field("true_arm", true_arm.deref())
                    .field("condition", condition.deref())
                    .field("false_arm", false_arm.deref())
                    .finish(),
                Self::Parenthesised(expr) => f.debug_tuple("Parenthesized").field(expr).finish(),
                Self::FunctionCall { caller, arguments } => f
                    .debug_struct("FunctionCall")
                    .field("caller", caller.deref())
                    .field("arguments", arguments)
                    .finish(),
                Self::MemberFunctionCall { member, function } => f
                    .debug_struct("MemberFunctionCall")
                    .field("member", member.deref())
                    .field("function", function.deref())
                    .finish(),
                Self::Literal(lit) => f.debug_tuple("Literal").field(lit).finish(),
                Self::Comparison(lhs, op, rhs) => f
                    .debug_tuple("Comparison")
                    .field(lhs.deref())
                    .field(op)
                    .field(rhs.deref())
                    .finish(),
                Self::IndexArray { array, index } => f
                    .debug_struct("IndexArray")
                    .field("array", array.deref())
                    .field("index", index.deref())
                    .finish(),
                Self::Array(arr) => f.debug_tuple("Array").field(arr).finish(),
                Self::Assignment(lhs, assign, rhs) => f
                    .debug_tuple("Assignment")
                    .field(lhs.deref())
                    .field(assign)
                    .field(rhs.deref())
                    .finish(),
                Self::Range(start, finish) => f
                    .debug_tuple("Range")
                    .field(start.deref())
                    .field(finish.deref())
                    .finish(),
            }
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum AssignmentType {
    Normal,
    BinaryOp(BinaryOperand),
}

impl<'src> TryFrom<(&Token<'src>, FileId)> for AssignmentType {
    type Error = Locatable<Error>;

    fn try_from((token, file): (&Token<'src>, FileId)) -> Result<Self, Self::Error> {
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
                    Location::new(token, file),
                ));
            }
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ComparisonOperand {
    Greater,
    Less,
    GreaterEqual,
    LessEqual,
    Equal,
    NotEqual,
}

impl<'src> TryFrom<(&Token<'src>, FileId)> for ComparisonOperand {
    type Error = Locatable<Error>;

    fn try_from((token, file): (&Token<'src>, FileId)) -> Result<Self, Self::Error> {
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
                    Location::new(token, file),
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
    ByteVec(Vec<u8>),
    Float(Float),
}

/// A utf-32 string
#[derive(Debug, Clone, PartialEq, Eq)]
#[repr(transparent)]
pub struct Text(Vec<Rune>);

impl Text {
    pub fn new(runes: Vec<Rune>) -> Self {
        Self(runes)
    }

    pub fn to_string(&self) -> String {
        String::from_iter(
            self.0
                .iter()
                .map(|r| core::char::from_u32(r.as_u32()).unwrap()),
        )
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        self.to_string().into_bytes()
    }
}

impl fmt::Display for Text {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", &self.to_string())
    }
}

impl From<&str> for Text {
    fn from(string: &str) -> Self {
        Self(string.chars().map(|c| Rune::from(c)).collect())
    }
}

/// A unicode codepoint
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
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

impl fmt::Display for Rune {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", &self.as_char())
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Integer {
    sign: Sign,
    bits: u128,
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
impl<'src> TryFrom<(&Token<'src>, FileId)> for Literal {
    type Error = Locatable<Error>;

    fn try_from((token, file): (&Token<'src>, FileId)) -> Result<Self, Self::Error> {
        Ok(match token.ty() {
            TokenType::Float => {
                let mut string = token.source();

                if string == "inf" {
                    return Ok(Literal::Float(Float::F64(core::f64::INFINITY)));
                } else if string == "NaN" {
                    return Ok(Literal::Float(Float::F64(core::f64::NAN)));
                }

                let negative = if &string[..1] == "-" {
                    string = &string[1..];
                    true
                } else if &string[..1] == "+" {
                    string = &string[1..];
                    false
                } else {
                    false
                };

                let mut float = if string.len() >= 2 && &string[..2] == "0x" || &string[..2] == "0X"
                {
                    use hexponent::FloatLiteral;

                    let float: FloatLiteral = string.parse().map_err(|_| {
                        Locatable::new(
                            Error::Syntax(SyntaxError::InvalidLiteral("float")),
                            Location::new(token, file),
                        )
                    })?;
                    float.convert::<f64>().inner()
                } else {
                    string.parse::<f64>().map_err(|_| {
                        Locatable::new(
                            Error::Syntax(SyntaxError::InvalidLiteral("float")),
                            Location::new(token, file),
                        )
                    })?
                };

                if negative {
                    float = -float;
                }

                Literal::Float(Float::F64(float))
            }

            TokenType::String => {
                let mut string = token.source().chars().collect::<Vec<char>>();

                let byte_str = if string[0] == 'b' {
                    string.remove(0);
                    true
                } else {
                    false
                };

                let string = match string[0] {
                    '\'' | '"'
                        if string.len() >= 6
                            && (string[..3] == ['\'', '\'', '\'']
                                || string[..3] == ['"', '"', '"']) =>
                    {
                        string.drain(..3).for_each(drop);
                        string.drain(string.len() - 3..).for_each(drop);

                        match string_escapes::unescape_string(string) {
                            Ok(s) => Ok(s),
                            Err((err, range)) => Err(Locatable::new(
                                err,
                                Location::new(
                                    (
                                        token.range().start + 3 + range.start,
                                        token.range().start + 3 + range.end,
                                    ),
                                    file,
                                ),
                            )),
                        }?
                    }

                    '\'' | '"' => {
                        string.drain(..1).for_each(drop);
                        string.drain(string.len() - 1..).for_each(drop);

                        match string_escapes::unescape_string(string) {
                            Ok(s) => Ok(s),
                            Err((err, range)) => Err(Locatable::new(
                                err,
                                Location::new(
                                    (
                                        token.range().start + 1 + range.start,
                                        token.range().start + 1 + range.end,
                                    ),
                                    file,
                                ),
                            )),
                        }?
                    }

                    _ => unreachable!(),
                };

                if byte_str {
                    Literal::ByteVec(string.to_bytes().to_vec())
                } else {
                    Literal::String(string)
                }
            }

            TokenType::Int => {
                let mut string = token.source();

                let sign = if &string[..1] == "-" {
                    string = &string[1..];
                    Sign::Negative
                } else if &string[..1] == "+" {
                    string = &string[1..];
                    Sign::Positive
                } else {
                    Sign::Positive
                };

                let int = if string.len() >= 2 && (&string[..2] == "0x" || &string[..2] == "0X") {
                    u128::from_str_radix(&string[2..], 16).map_err(|_| {
                        Locatable::new(
                            Error::Syntax(SyntaxError::InvalidLiteral("int")),
                            Location::new(token, file),
                        )
                    })?
                } else {
                    u128::from_str_radix(string, 10).map_err(|_| {
                        Locatable::new(
                            Error::Syntax(SyntaxError::InvalidLiteral("int")),
                            Location::new(token, file),
                        )
                    })?
                };

                Literal::Integer(Integer { sign, bits: int })
            }

            TokenType::Bool => Self::Bool(token.source().parse::<bool>().map_err(|_| {
                Locatable::new(
                    Error::Syntax(SyntaxError::InvalidLiteral("bool")),
                    Location::new(token, file),
                )
            })?),

            ty => {
                return Err(Locatable::new(
                    Error::Syntax(SyntaxError::Generic(format!("Invalid Literal: '{}'", ty))),
                    Location::new(token, file),
                ))
            }
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
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

impl<'src> TryFrom<(&Token<'src>, FileId)> for BinaryOperand {
    type Error = Locatable<Error>;

    fn try_from((token, file): (&Token<'src>, FileId)) -> Result<Self, Self::Error> {
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
                    Location::new(token, file),
                ));
            }
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOperand {
    Positive,
    Negative,
    Not,
}

impl<'src> TryFrom<(&Token<'src>, FileId)> for UnaryOperand {
    type Error = Locatable<Error>;

    fn try_from((token, file): (&Token<'src>, FileId)) -> Result<Self, Self::Error> {
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
                    Location::new(token, file),
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
    pub fn expr(&mut self) -> ParseResult<Expr<'expr>> {
        let _frame = self.add_stack_frame()?;

        self.parse_expression(0)
    }

    fn parse_expression(&mut self, precedence: usize) -> ParseResult<Expr<'expr>> {
        let _frame = self.add_stack_frame()?;
        let mut token = self.next()?;

        let prefix = Self::prefix(token.ty());
        if let Some(prefix) = prefix {
            let mut left = prefix(self, token)?;

            if let Ok(peek) = self.peek() {
                let postfix = Self::postfix(peek.ty());
                if let Some(postfix) = postfix {
                    token = self.next()?;
                    left = postfix(self, token, left)?;
                }
            }

            while precedence < self.current_precedence() {
                token = self.next()?;

                let infix = Self::infix(token.ty());
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
                Location::new(&token, self.current_file),
            ))
        }
    }

    fn prefix(ty: TokenType) -> Option<PrefixParselet<'src, 'expr, 'stmt>> {
        Some(match ty {
            // Variables
            TokenType::Ident => |parser, token| {
                let _frame = parser.add_stack_frame()?;

                let ident = parser.string_interner.intern(token.source());
                let expr = parser.expr_arena.alloc(Expression::Variable(ident));

                Ok(expr)
            },

            // Literals
            TokenType::Int | TokenType::Bool | TokenType::Float | TokenType::String => {
                |parser, lit| {
                    let expr = parser
                        .expr_arena
                        .alloc(Expression::Literal(Literal::try_from((
                            &lit,
                            parser.current_file,
                        ))?));

                    Ok(expr)
                }
            }

            // Prefix Operators
            TokenType::Minus | TokenType::Bang | TokenType::Plus => |parser, token| {
                let _frame = parser.add_stack_frame()?;
                let operand = parser.expr()?;
                let expr = parser.expr_arena.alloc(Expression::UnaryExpr(
                    UnaryOperand::try_from((&token, parser.current_file))?,
                    operand,
                ));

                Ok(expr)
            },

            // Grouping via parentheses
            TokenType::LeftParen => |parser, _| {
                let _frame = parser.add_stack_frame()?;

                let expr = parser.expr()?;
                parser.eat(TokenType::RightParen)?;
                let expr = parser.expr_arena.alloc(Expression::Parenthesised(expr));

                Ok(expr)
            },

            // Array literals
            TokenType::LeftBrace => |parser, _| {
                let _frame = parser.add_stack_frame()?;

                let mut elements = Vec::with_capacity(5);

                while parser.peek()?.ty() != TokenType::RightBrace {
                    let elm = parser.expr()?;
                    elements.push(elm);

                    if parser.peek()?.ty() == TokenType::Comma {
                        parser.eat(TokenType::Comma)?;
                    } else {
                        break;
                    }
                }

                elements.shrink_to_fit();
                parser.eat(TokenType::RightBrace)?;
                let expr = parser.expr_arena.alloc(Expression::Array(elements));

                Ok(expr)
            },

            _ => return None,
        })
    }

    fn postfix(ty: TokenType) -> Option<PostfixParselet<'src, 'expr, 'stmt>> {
        Some(match ty {
            // Function calls
            TokenType::LeftParen => |parser, _left_paren, caller| {
                let _frame = parser.add_stack_frame()?;

                let mut arguments = Vec::with_capacity(5);

                while parser.peek()?.ty() != TokenType::RightParen {
                    let arg = parser.expr()?;
                    arguments.push(arg);

                    if parser.peek()?.ty() == TokenType::Comma {
                        parser.eat(TokenType::Comma)?;
                    } else {
                        break;
                    }
                }

                arguments.shrink_to_fit();
                parser.eat(TokenType::RightParen)?;
                let expr = parser
                    .expr_arena
                    .alloc(Expression::FunctionCall { caller, arguments });

                Ok(expr)
            },

            // Dotted function calls
            TokenType::Dot => |parser, _dot, member| {
                let _frame = parser.add_stack_frame()?;
                let function = parser.expr()?;
                let expr = parser
                    .expr_arena
                    .alloc(Expression::MemberFunctionCall { member, function });

                Ok(expr)
            },

            // Ranges
            TokenType::DoubleDot => |parser, _, start| {
                let _frame = parser.add_stack_frame()?;
                let end = parser.expr()?;
                let expr = parser.expr_arena.alloc(Expression::Range(start, end));

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
                    .alloc(Expression::Assignment(left, assign, right));

                Ok(expr)
            },

            TokenType::Colon => |parser, _colon, left| {
                let _frame = parser.add_stack_frame()?;

                let equal = parser.eat(TokenType::Equal)?;
                let assign = AssignmentType::try_from((&equal, parser.current_file))?;
                let right = parser.expr()?;
                let expr = parser
                    .expr_arena
                    .alloc(Expression::Assignment(left, assign, right));

                Ok(expr)
            },

            _ => return None,
        })
    }

    fn infix(ty: TokenType) -> Option<InfixParselet<'src, 'expr, 'stmt>> {
        Some(match ty {
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
                let expr = parser.expr_arena.alloc(Expression::BinaryOp(
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
                let expr = parser.expr_arena.alloc(Expression::Comparison(
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
                parser.eat(TokenType::Else)?;
                let false_arm = parser.expr()?;
                let expr = parser.expr_arena.alloc(Expression::InlineConditional {
                    true_arm,
                    condition,
                    false_arm,
                });

                Ok(expr)
            },

            // Array indexing
            TokenType::LeftBrace => Self::index_array,

            _ => return None,
        })
    }

    fn index_array(
        &mut self,
        _left_bracket: Token<'src>,
        array: Expr<'expr>,
    ) -> ParseResult<Expr<'expr>> {
        let _frame = self.add_stack_frame()?;

        let index = self.expr()?;
        self.eat(TokenType::RightBrace)?;
        let expr = self
            .expr_arena
            .alloc(Expression::IndexArray { array, index });

        Ok(expr)
    }
}
