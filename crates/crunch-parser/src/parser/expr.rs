use crate::{
    error::{Error, Locatable, Location, ParseResult, SyntaxError},
    files::FileId,
    parser::{string_escapes, Parser},
    token::{Token, TokenType},
};

use lasso::SmallSpur;
use stadium::Ticket;

use alloc::{format, string::String, vec::Vec};
use core::convert::TryFrom;

pub type Expr<'expr> = Ticket<'expr, Expression<'expr>>;

#[derive(Debug, Clone, PartialEq)]
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
    I32(i32),
    Bool(bool),
    String(String),
    ByteVec(Vec<u8>),
    F32(f32),
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
                    return Ok(Literal::F32(core::f32::INFINITY));
                } else if string == "NaN" {
                    return Ok(Literal::F32(core::f32::NAN));
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
                    float.convert::<f32>().inner()
                } else {
                    string.parse::<f32>().map_err(|_| {
                        Locatable::new(
                            Error::Syntax(SyntaxError::InvalidLiteral("float")),
                            Location::new(token, file),
                        )
                    })?
                };

                if negative {
                    float = -float;
                }

                Literal::F32(float)
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
                    Literal::ByteVec(string.as_bytes().to_vec())
                } else {
                    Literal::String(string)
                }
            }

            TokenType::Int => {
                let mut string = token.source();

                let negative = if &string[..1] == "-" {
                    string = &string[1..];
                    true
                } else if &string[..1] == "+" {
                    string = &string[1..];
                    false
                } else {
                    false
                };

                let mut int = if string.len() >= 2 && (&string[..2] == "0x" || &string[..2] == "0X")
                {
                    i32::from_str_radix(&string[2..], 16).map_err(|_| {
                        Locatable::new(
                            Error::Syntax(SyntaxError::InvalidLiteral("int")),
                            Location::new(token, file),
                        )
                    })?
                } else {
                    i32::from_str_radix(string, 10).map_err(|_| {
                        Locatable::new(
                            Error::Syntax(SyntaxError::InvalidLiteral("int")),
                            Location::new(token, file),
                        )
                    })?
                };

                if negative {
                    int = -int;
                }

                Literal::I32(int)
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
    pub(super) fn expr(&mut self) -> ParseResult<Expr<'expr>> {
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
            TokenType::Equal
            | TokenType::AddAssign
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
