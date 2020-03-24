use super::*;
use crate::token::Token;

use crunch_error::parse_prelude::{trace, Diagnostic, ParseResult};

use alloc::{boxed::Box, format, string::String, vec::Vec};
use core::convert::TryFrom;

// TODO: Use arenas over Boxes

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Variable(Sym),
    UnaryExpr(UnaryOperand, Box<Expression>),
    BinaryOp(Box<Expression>, BinaryOperand, Box<Expression>),
    InlineConditional {
        true_arm: Box<Expression>,
        condition: Box<Expression>,
        false_arm: Box<Expression>,
    },
    Parenthesised(Box<Expression>),
    FunctionCall {
        caller: Box<Expression>,
        arguments: Vec<Expression>,
    },
    MemberFunctionCall {
        member: Box<Expression>,
        function: Box<Expression>,
    },
    Literal(Literal),
    Comparison(Box<Expression>, ComparisonOperand, Box<Expression>),
    IndexArray {
        array: Box<Expression>,
        index: Box<Expression>,
    },
    Array(Vec<Expression>),
    RepeatedArray(Box<Expression>, Box<Expression>),
    Assignment(Box<Expression>, AssignmentType, Box<Expression>),
    Range(Box<Expression>, Box<Expression>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum AssignmentType {
    Normal,
    BinaryOp(BinaryOperand),
}

impl TryFrom<TokenType> for AssignmentType {
    type Error = Vec<Diagnostic<usize>>;

    fn try_from(ty: TokenType) -> Result<Self, Self::Error> {
        Ok(match ty {
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
                return Err(vec![Diagnostic::error()
                    .with_message(format!("Expected an assignment, got `{}`", ty))]);
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

impl TryFrom<TokenType> for ComparisonOperand {
    type Error = Vec<Diagnostic<usize>>;

    fn try_from(ty: TokenType) -> Result<Self, Self::Error> {
        Ok(match ty {
            TokenType::RightCaret => Self::Greater,
            TokenType::LeftCaret => Self::Less,
            TokenType::GreaterThanEqual => Self::GreaterEqual,
            TokenType::LessThanEqual => Self::LessEqual,
            TokenType::IsEqual => Self::Equal,
            TokenType::IsNotEqual => Self::NotEqual,

            _ => {
                return Err(vec![Diagnostic::error()
                    .with_message(format!("Expected a comparison, got `{}`", ty))]);
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
impl<'a> TryFrom<&Token<'a>> for Literal {
    type Error = Vec<Diagnostic<usize>>;

    fn try_from(token: &Token<'a>) -> Result<Self, Self::Error> {
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
                        vec![Diagnostic::error().with_message("Invalid hex float literal")]
                    })?;
                    float.convert::<f32>().inner()
                } else {
                    string.parse::<f32>().map_err(|_| {
                        vec![Diagnostic::error().with_message("Invalid float literal")]
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
                            && (&string[..3] == &['\'', '\'', '\'']
                                || &string[..3] == &['"', '"', '"']) =>
                    {
                        string.drain(..3).for_each(|d| drop(d));
                        string.drain(string.len() - 3..).for_each(|d| drop(d));

                        string_escapes::unescape_string(string)?
                    }

                    '\'' | '"' => {
                        string.drain(..1).for_each(|d| drop(d));
                        string.drain(string.len() - 1..).for_each(|d| drop(d));

                        string_escapes::unescape_string(string)?
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
                        vec![Diagnostic::error().with_message("Invalid hex int literal")]
                    })?
                } else {
                    i32::from_str_radix(string, 10).map_err(|_| {
                        vec![Diagnostic::error().with_message("Invalid int literal")]
                    })?
                };

                if negative {
                    int = -int;
                }

                Literal::I32(int)
            }

            TokenType::Bool => Self::Bool(
                token
                    .source()
                    .parse::<bool>()
                    .map_err(|_| vec![Diagnostic::error().with_message("Invalid bool literal")])?,
            ),

            ty => {
                return Err(vec![
                    Diagnostic::error().with_message(format!("`{}` is not a valid literal", ty))
                ])
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

impl TryFrom<TokenType> for BinaryOperand {
    type Error = Vec<Diagnostic<usize>>;

    fn try_from(ty: TokenType) -> Result<Self, Self::Error> {
        Ok(match ty {
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
                return Err(vec![Diagnostic::error()
                    .with_message(format!("Expected a binary operand, got `{}`", ty))]);
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

impl TryFrom<TokenType> for UnaryOperand {
    type Error = Vec<Diagnostic<usize>>;

    fn try_from(ty: TokenType) -> Result<Self, Self::Error> {
        Ok(match ty {
            TokenType::Plus => Self::Positive,
            TokenType::Minus => Self::Negative,
            TokenType::Bang => Self::Not,

            ty => {
                return Err(vec![Diagnostic::error()
                    .with_message(format!("Expected a unary operand, got `{}`", ty))])
            }
        })
    }
}

type PrefixParselet<'a> = fn(&mut Parser<'a>, Token<'a>) -> ParseResult<Expression>;
type PostfixParselet<'a> = fn(&mut Parser<'a>, Token<'a>, Expression) -> ParseResult<Expression>;
type InfixParselet<'a> = fn(&mut Parser<'a>, Token<'a>, Expression) -> ParseResult<Expression>;

/// Expression Parsing
impl<'a> Parser<'a> {
    pub(super) fn expr(&mut self) -> ParseResult<Expression> {
        self.parse_expression(0)
    }

    fn parse_expression(&mut self, precedence: usize) -> ParseResult<Expression> {
        let _frame = self.add_stack_frame()?;
        let mut token = self.next()?;

        if let Some(prefix) = Self::prefix(token.ty()) {
            let (mut left, mut diag) = prefix(self, token)?;

            if let Ok(peek) = self.peek() {
                if let Some(postfix) = Self::postfix(peek.ty()) {
                    token = self.next()?;

                    let (postfix, post_diag) = postfix(self, token, left)?;
                    diag.extend_from_slice(&post_diag);
                    left = postfix;
                }
            }

            while precedence < self.current_precedence() {
                token = self.next()?;

                if let Some(infix) = Self::infix(token.ty()) {
                    let (infix, in_diag) = infix(self, token, left)?;
                    diag.extend_from_slice(&in_diag);
                    left = infix;
                } else {
                    break;
                }
            }

            Ok((left, diag))
        } else {
            Err(vec![
                Diagnostic::error().with_message(format!("Could not parse `{}`", token.ty()))
            ])
        }
    }

    fn prefix(ty: TokenType) -> Option<PrefixParselet<'a>> {
        Some(match ty {
            // Variables
            TokenType::Ident => |parser, token| {
                let ident = parser.intern_string(token.source());

                Ok((Expression::Variable(ident), Vec::new()))
            },

            // Literals
            TokenType::Int | TokenType::Bool | TokenType::Float | TokenType::String => {
                |_parser, lit| Ok((Expression::Literal(Literal::try_from(&lit)?), Vec::new()))
            }

            // Prefix Operators
            TokenType::Minus | TokenType::Bang | TokenType::Plus => |parser, token| {
                let (operand, diagnostics) = parser.expr()?;

                Ok((
                    Expression::UnaryExpr(UnaryOperand::try_from(token.ty())?, Box::new(operand)),
                    diagnostics,
                ))
            },

            // Grouping via parentheses
            TokenType::LeftParen => |parser, _| {
                let (expr, diag) = parser.expr()?;
                parser.eat(TokenType::RightParen)?;

                Ok((Expression::Parenthesised(Box::new(expr)), diag))
            },

            // Array literals
            TokenType::LeftBrace => |parser, _| {
                let mut diagnostics = Vec::new();
                let mut elements = Vec::with_capacity(5);

                while parser.peek()?.ty() != TokenType::RightBrace {
                    let (elm, diag) = parser.expr()?;
                    diagnostics.extend_from_slice(&diag);
                    elements.push(elm);

                    if parser.peek()?.ty() == TokenType::Comma {
                        parser.eat(TokenType::Comma)?;
                    } else {
                        break;
                    }
                }

                elements.shrink_to_fit();
                parser.eat(TokenType::RightBrace)?;

                Ok((Expression::Array(elements), diagnostics))
            },

            _ => return None,
        })
    }

    fn postfix(ty: TokenType) -> Option<PostfixParselet<'a>> {
        Some(match ty {
            // Function calls
            TokenType::LeftParen => |parser, _left_paren, caller| {
                let mut diagnostics = Vec::new();
                let mut arguments = Vec::with_capacity(5);

                while parser.peek()?.ty() != TokenType::RightParen {
                    let (arg, diag) = parser.expr()?;
                    diagnostics.extend_from_slice(&diag);
                    arguments.push(arg);

                    if parser.peek()?.ty() == TokenType::Comma {
                        parser.eat(TokenType::Comma)?;
                    } else {
                        break;
                    }
                }

                arguments.shrink_to_fit();
                parser.eat(TokenType::RightParen)?;

                Ok((
                    Expression::FunctionCall {
                        caller: Box::new(caller),
                        arguments,
                    },
                    diagnostics,
                ))
            },

            // Dotted function calls
            TokenType::Dot => |parser, _dot, member| {
                let (func, diag) = parser.expr()?;

                Ok((
                    Expression::MemberFunctionCall {
                        member: Box::new(member),
                        function: Box::new(func),
                    },
                    diag,
                ))
            },

            // Ranges
            TokenType::DoubleDot => |parser, _, start| {
                let (end, diag) = parser.expr()?;

                Ok((Expression::Range(Box::new(start), Box::new(end)), diag))
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
                trace!("Postfix assignment");

                let assign = AssignmentType::try_from(assign.ty())?;
                let (right, diagnostics) = parser.expr()?;

                Ok((
                    Expression::Assignment(Box::new(left), assign, Box::new(right)),
                    diagnostics,
                ))
            },

            _ => return None,
        })
    }

    fn infix(ty: TokenType) -> Option<InfixParselet<'a>> {
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
                trace!("Infix Binary Op");
                let (right, diagnostics) = parser.expr()?;

                Ok((
                    Expression::BinaryOp(
                        Box::new(left),
                        BinaryOperand::try_from(operand.ty())?,
                        Box::new(right),
                    ),
                    diagnostics,
                ))
            },

            // Comparisons
            TokenType::RightCaret
            | TokenType::LeftCaret
            | TokenType::GreaterThanEqual
            | TokenType::LessThanEqual
            | TokenType::IsEqual
            | TokenType::IsNotEqual => |parser, comparison, left| {
                trace!("Comparison Op");
                let (right, diagnostics) = parser.expr()?;

                Ok((
                    Expression::Comparison(
                        Box::new(left),
                        ComparisonOperand::try_from(comparison.ty())?,
                        Box::new(right),
                    ),
                    diagnostics,
                ))
            },

            // <ret> if <cond> else <cond>
            TokenType::If => |parser, _, true_arm| {
                let (condition, mut diagnostics) = parser.expr()?;
                parser.eat(TokenType::Else)?;
                let (false_arm, diag) = parser.expr()?;
                diagnostics.extend_from_slice(&diag);

                Ok((
                    Expression::InlineConditional {
                        true_arm: Box::new(true_arm),
                        condition: Box::new(condition),
                        false_arm: Box::new(false_arm),
                    },
                    diagnostics,
                ))
            },

            // Array indexing
            TokenType::LeftBrace => Self::index_array,

            _ => return None,
        })
    }

    fn index_array(
        &mut self,
        _left_bracket: Token<'a>,
        array: Expression,
    ) -> ParseResult<Expression> {
        trace!("Index Array");

        let (index, diagnostics) = self.expr()?;
        self.eat(TokenType::RightBrace)?;

        Ok((
            Expression::IndexArray {
                array: Box::new(array),
                index: Box::new(index),
            },
            diagnostics,
        ))
    }
}
