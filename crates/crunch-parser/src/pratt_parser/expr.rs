use super::*;
use crate::token::Token;

use crunch_error::parse_prelude::{trace, Diagnostic, ParseResult};

use core::convert::TryFrom;

// TODO: Use arenas over Boxes

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Variable(Sym),
    PrefixExpr(PrefixOperand, Box<Expression>),
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
    Literal(Literal),
    Comparison(Box<Expression>, ComparisonOperand, Box<Expression>),
    IndexArray {
        array: Box<Expression>,
        index: Box<Expression>,
    },
    Array(Vec<Expression>),
    // TODO: Actually parse out assignments
    Assignment(Box<Expression>, AssignmentType, Box<Expression>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum AssignmentType {
    Normal,
    BinaryOp(BinaryOperand),
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
    type Error = ();

    fn try_from(ty: TokenType) -> Result<Self, Self::Error> {
        Ok(match ty {
            TokenType::RightCaret => Self::Greater,
            TokenType::LeftCaret => Self::Less,
            TokenType::GreaterThanEqual => Self::GreaterEqual,
            TokenType::LessThanEqual => Self::LessEqual,
            TokenType::IsEqual => Self::Equal,
            TokenType::IsNotEqual => Self::NotEqual,

            _ => return Err(()),
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    I32(i32),
    Bool(bool),
}

impl<'a> TryFrom<&Token<'a>> for Literal {
    type Error = ();

    // TODO: strings & floats
    fn try_from(token: &Token<'a>) -> Result<Self, Self::Error> {
        Ok(match token.ty() {
            // TODO: Better integer parsing
            TokenType::Int => Self::I32(token.source().parse::<i32>().unwrap()),
            TokenType::Bool => Self::Bool(token.source().parse::<bool>().unwrap()),

            _ => return Err(()),
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOperand {
    Plus,
    Minus,
    Mult,
    Divide,
    Modulo,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    Shl,
    Shr,
}

impl TryFrom<TokenType> for BinaryOperand {
    type Error = ();

    fn try_from(ty: TokenType) -> Result<Self, Self::Error> {
        Ok(match ty {
            TokenType::Plus => Self::Plus,
            TokenType::Minus => Self::Minus,
            TokenType::Star => Self::Mult,
            TokenType::Divide => Self::Divide,
            TokenType::Modulo => Self::Modulo,
            TokenType::Ampersand => Self::BitwiseAnd,
            TokenType::Pipe => Self::BitwiseOr,
            TokenType::Caret => Self::BitwiseXor,
            TokenType::Shl => Self::Shl,
            TokenType::Shr => Self::Shr,

            _ => return Err(()),
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum PrefixOperand {
    Negative,
    Not,
}

impl TryFrom<TokenType> for PrefixOperand {
    type Error = (); // TODO: Maybe should be Diagnostic?

    fn try_from(ty: TokenType) -> Result<Self, Self::Error> {
        Ok(match ty {
            TokenType::Minus => Self::Negative,
            TokenType::Bang => Self::Not,

            _ => return Err(()),
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
        let mut token = self.next()?;

        if let Some(prefix) = Self::prefix(token.ty()) {
            let mut left = prefix(self, token)?;
            if let Ok(peek) = self.peek() {
                if let Some(postfix) = Self::postfix(peek.ty()) {
                    token = self.next()?;
                    left = postfix(self, token, left)?;
                }
            }

            while precedence < self.current_precedence() {
                token = self.next()?;

                if let Some(infix) = Self::infix(token.ty()) {
                    left = infix(self, token, left)?;
                } else {
                    continue;
                }
            }

            Ok(left)
        } else {
            Err(Diagnostic::error().with_message(format!("Could not parse `{}`", token.ty())))
        }
    }

    fn prefix(ty: TokenType) -> Option<PrefixParselet<'a>> {
        Some(match ty {
            // Variables
            TokenType::Ident => |parser, token| {
                trace!("Prefix Ident");
                let ident = parser
                    .string_interner
                    .write()
                    .unwrap()
                    .get_or_intern(token.source());

                Ok(Expression::Variable(ident))
            },

            // Literals
            TokenType::Int | TokenType::Bool => |_parser, lit| {
                trace!("Prefix Literal");

                Ok(Expression::Literal(Literal::try_from(&lit).unwrap()))
            },

            // Prefix Operators
            TokenType::Minus | TokenType::Bang => |parser, token| {
                trace!("Prefix Operators");
                let operand = Box::new(parser.expr()?);

                Ok(Expression::PrefixExpr(
                    PrefixOperand::try_from(token.ty()).unwrap(),
                    operand,
                ))
            },

            // Grouping via parentheses
            TokenType::LeftParen => |parser, _| {
                trace!("Prefix Left Paren");
                let expr = Box::new(parser.expr()?);
                parser.eat(TokenType::RightParen)?;

                Ok(Expression::Parenthesised(expr))
            },

            // Array literals
            TokenType::LeftBrace => |parser, _| {
                trace!("Prefix array literal");

                let mut elements = Vec::with_capacity(5);
                loop {
                    if let Ok(peek) = parser.peek() {
                        if peek.ty() != TokenType::RightParen {
                            elements.push(parser.expr()?);

                            if let Ok(peek) = parser.peek() {
                                if peek.ty() == TokenType::Comma {
                                    parser.eat(TokenType::Comma)?;
                                    continue;
                                }
                            }

                            break;
                        }
                    }

                    break;
                }
                parser.eat(TokenType::RightBrace)?;

                Ok(Expression::Array(elements))
            },

            _ => return None,
        })
    }

    fn postfix(ty: TokenType) -> Option<PostfixParselet<'a>> {
        Some(match ty {
            // Function calls
            TokenType::LeftParen => Self::function_call,

            // Array indexing
            TokenType::LeftBrace => Self::index_array,

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
            | TokenType::Ampersand
            | TokenType::Pipe
            | TokenType::Caret
            | TokenType::Shl
            | TokenType::Shr => |parser, operand, left| {
                trace!("Infix Binary Op");
                let right = Box::new(parser.expr()?);

                Ok(Expression::BinaryOp(
                    Box::new(left),
                    BinaryOperand::try_from(operand.ty()).unwrap(),
                    right,
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
                let right = Box::new(parser.expr()?);

                Ok(Expression::Comparison(
                    Box::new(left),
                    ComparisonOperand::try_from(comparison.ty()).unwrap(),
                    right,
                ))
            },

            // <ret> if <cond> else <cond>
            TokenType::If => |parser, _, true_arm| {
                trace!("Infix Conditional");
                let condition = Box::new(parser.expr()?);
                parser.eat(TokenType::Else)?;
                let false_arm = Box::new(parser.expr()?);

                Ok(Expression::InlineConditional {
                    true_arm: Box::new(true_arm),
                    condition,
                    false_arm,
                })
            },

            // Function calls
            TokenType::LeftParen => Self::function_call,

            // Array indexing
            TokenType::LeftBrace => Self::index_array,

            _ => return None,
        })
    }

    fn function_call(&mut self, _: Token<'a>, caller: Expression) -> ParseResult<Expression> {
        trace!("Function Call");

        let mut arguments = Vec::with_capacity(5);
        loop {
            if let Ok(peek) = self.peek() {
                if peek.ty() != TokenType::RightParen {
                    arguments.push(self.expr()?);

                    if let Ok(peek) = self.peek() {
                        if peek.ty() == TokenType::Comma {
                            self.eat(TokenType::Comma)?;
                            continue;
                        }
                    }

                    break;
                }
            }

            break;
        }
        self.eat(TokenType::RightParen)?;

        Ok(Expression::FunctionCall {
            caller: Box::new(caller),
            arguments,
        })
    }

    fn index_array(&mut self, _: Token<'a>, array: Expression) -> ParseResult<Expression> {
        trace!("Index Array");

        let index = Box::new(self.expr()?);
        self.eat(TokenType::RightBrace)?;

        Ok(Expression::IndexArray {
            array: Box::new(array),
            index,
        })
    }
}
