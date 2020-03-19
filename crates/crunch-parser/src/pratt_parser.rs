use crate::token::{Token, TokenStream, TokenType};

use crunch_error::parse_prelude::{trace, Diagnostic, Label, ParseResult};
use string_interner::{StringInterner, Sym};

use core::convert::TryFrom;
use std::sync::{Arc, RwLock};

// TODO: Use arenas over Boxes

#[derive(Debug, Copy, Clone)]
#[rustfmt::skip]
enum BinaryPrecedence {
    Mul, Div, Mod,
    Add, Sub,
    Shl, Shr,
    Less, Greater, LessEq, GreaterEq,
    Eq, Ne,
    BitAnd,
    BitXor,
    BitOr,
    LogAnd,
    LogOr,
    Ternary,
    Assignment,
}

impl BinaryPrecedence {
    fn precedence(self) -> usize {
        match self {
            Self::Mul | Self::Div | Self::Mod => 11,
            Self::Add | Self::Sub => 10,
            Self::Shl | Self::Shr => 9,
            Self::Less | Self::Greater | Self::LessEq | Self::GreaterEq => 8,
            Self::Eq | Self::Ne => 7,
            Self::BitAnd => 6,
            Self::BitXor => 5,
            Self::BitOr => 4,
            Self::LogAnd => 3,
            Self::LogOr => 2,
            Self::Ternary => 1,
            Self::Assignment => 0,
        }
    }
}

impl TryFrom<TokenType> for BinaryPrecedence {
    type Error = ();

    fn try_from(t: TokenType) -> Result<BinaryPrecedence, ()> {
        Ok(match t {
            TokenType::Star => Self::Mul,
            TokenType::Divide => Self::Div,
            TokenType::Modulo => Self::Mod,
            TokenType::Plus => Self::Add,
            TokenType::Minus => Self::Sub,
            TokenType::Shl => Self::Shl,
            TokenType::Shr => Self::Shr,
            TokenType::LeftCaret => Self::Less,
            TokenType::RightCaret => Self::Greater,
            TokenType::LessThanEqual => Self::LessEq,
            TokenType::GreaterThanEqual => Self::GreaterEq,
            TokenType::IsEqual => Self::Eq,
            TokenType::IsNotEqual => Self::Ne,
            TokenType::Ampersand => Self::BitAnd,
            TokenType::Caret => Self::BitXor,
            TokenType::Pipe => Self::BitOr,
            TokenType::And => Self::LogAnd,
            TokenType::Or => Self::LogOr,
            TokenType::Equal => Self::Assignment,
            TokenType::If => Self::Ternary,

            _ => return Err(()),
        })
    }
}

#[derive(Debug, Clone)]
enum Expression {
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
}

#[derive(Debug, Clone)]
enum Literal {
    I32(i32),
    Bool(bool),
}

#[derive(Debug, Clone)]
enum BinaryOperand {
    Plus,
    Minus,
    Star,
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
            TokenType::Star => Self::Star,
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

#[derive(Debug, Clone)]
enum PrefixOperand {
    Minus,
    Not,
}

impl TryFrom<TokenType> for PrefixOperand {
    type Error = (); // TODO: Maybe should be Diagnostic?

    fn try_from(ty: TokenType) -> Result<Self, Self::Error> {
        Ok(match ty {
            TokenType::Minus => Self::Minus,
            TokenType::Bang => Self::Not,

            _ => return Err(()),
        })
    }
}

type PrefixParselet<'a> = Box<dyn Fn(&mut Parser<'a>, Token<'a>) -> ParseResult<Expression>>;

type InfixParselet<'a> =
    Box<dyn Fn(&mut Parser<'a>, Token<'a>, Expression) -> ParseResult<Expression>>;

pub struct Parser<'a> {
    token_stream: TokenStream<'a>,
    next: Option<Token<'a>>,
    peek: Option<Token<'a>>,

    current_file: usize,
    string_interner: Arc<RwLock<StringInterner<Sym>>>,
}

/// Initialization and high-level usage
impl<'a> Parser<'a> {
    pub fn new(
        source: &'a str,
        current_file: usize,
        string_interner: Arc<RwLock<StringInterner<Sym>>>,
    ) -> Self {
        let (token_stream, next, peek) = Self::lex(source);

        Self {
            token_stream,
            next,
            peek,
            current_file,
            string_interner,
        }
    }

    fn lex(source: &'a str) -> (TokenStream<'a>, Option<Token<'a>>, Option<Token<'a>>) {
        let mut token_stream = TokenStream::new(source, true);
        let next = None;
        let peek = token_stream.next_token();

        (token_stream, next, peek)
    }

    // TODO: Optimize with lazy closures that persist after the function has been called instead of
    // making a new one each and every time
    fn prefix_parselet(ty: TokenType) -> Option<PrefixParselet<'a>> {
        Some(match ty {
            // Variables
            TokenType::Ident => Box::new(|parser, token| {
                trace!("Prefix Ident");
                let ident = parser
                    .string_interner
                    .write()
                    .unwrap()
                    .get_or_intern(token.source());

                Ok(Expression::Variable(ident))
            }),

            // Literals
            TokenType::Int => Box::new(|_parser, int| {
                trace!("Prefix Int");
                let int = int.source().parse::<i32>().unwrap();

                Ok(Expression::Literal(Literal::I32(int)))
            }),
            TokenType::Bool => Box::new(|_parser, boolean| {
                trace!("Prefix Bool");
                let boolean = boolean.source().parse::<bool>().unwrap();

                Ok(Expression::Literal(Literal::Bool(boolean)))
            }),

            // Prefix Operators
            TokenType::Minus | TokenType::Bang => Box::new(|parser, token| {
                trace!("Prefix Operators");
                let operand = Box::new(parser.parse_expression(parser.current_precedence())?);

                Ok(Expression::PrefixExpr(
                    PrefixOperand::try_from(token.ty()).unwrap(),
                    operand,
                ))
            }),

            // Grouping via parentheses
            TokenType::LeftParen => Box::new(|parser, _| {
                trace!("Prefix Left Paren");
                let expr = Box::new(parser.parse_expression(0)?);
                parser.eat(TokenType::RightParen)?;

                Ok(Expression::Parenthesised(expr))
            }),

            _ => return None,
        })
    }

    fn infix_parselet(ty: TokenType) -> Option<InfixParselet<'a>> {
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
            | TokenType::Shr => Box::new(|parser, operand, left| {
                trace!("Infix Binary Op");
                let right = Box::new(parser.parse_expression(parser.current_precedence())?);

                Ok(Expression::BinaryOp(
                    Box::new(left),
                    BinaryOperand::try_from(operand.ty()).unwrap(),
                    right,
                ))
            }),

            // <ret> if <cond> else <cond>
            TokenType::If => Box::new(|parser, _, true_arm| {
                trace!("Infix Conditional");
                let condition = Box::new(parser.parse_expression(parser.current_precedence())?);
                parser.eat(TokenType::Else)?;
                let false_arm = Box::new(parser.parse_expression(parser.current_precedence())?);

                Ok(Expression::InlineConditional {
                    true_arm: Box::new(true_arm),
                    condition,
                    false_arm,
                })
            }),

            // Function calls
            TokenType::LeftParen => Box::new(|parser, _, caller| {
                trace!("Infix Function Call");
                let mut arguments = Vec::with_capacity(5);
                loop {
                    if let Ok(peek) = parser.peek() {
                        if peek.ty() != TokenType::RightParen {
                            arguments.push(parser.parse_expression(parser.current_precedence())?);

                            continue;
                        }
                    }

                    break;
                }
                parser.eat(TokenType::RightParen)?;

                Ok(Expression::FunctionCall {
                    caller: Box::new(caller),
                    arguments,
                })
            }),

            _ => return None,
        })
    }
}

#[doc(hidden)]
mod utility {
    use super::*;

    /// Utility functions
    impl<'a> Parser<'a> {
        pub(super) fn next(&mut self) -> ParseResult<Token<'a>> {
            let mut next = self.token_stream.next_token();
            std::mem::swap(&mut next, &mut self.peek);
            self.next = next;

            next.ok_or(Diagnostic::error().with_message("Unexpected End Of File"))
        }

        pub(super) fn peek(&self) -> ParseResult<Token<'a>> {
            self.peek
                .ok_or(Diagnostic::error().with_message("Unexpected End Of File"))
        }

        pub(super) fn eat(&mut self, expected: TokenType) -> ParseResult<Token<'a>> {
            let token = self.next()?;

            if token.ty() == expected {
                Ok(token)
            } else {
                Err(Diagnostic::error()
                    .with_message(format!(
                        "Unexpected Token: Expected '{}', found '{}'",
                        expected, token.ty
                    ))
                    .with_labels(vec![Label::primary(
                        self.current_file,
                        token.range.0 as usize..token.range.1 as usize,
                    )
                    .with_message(format!("Expected {}", expected))]))
            }
        }

        pub(super) fn current_precedence(&self) -> usize {
            BinaryPrecedence::try_from(self.peek.unwrap().ty())
                .map(|op| op.precedence())
                .unwrap_or(0)
        }
    }
}

/// Parsing operations
impl<'a> Parser<'a> {
    fn parse_expression(&mut self, precedence: usize) -> ParseResult<Expression> {
        let mut token = self.next()?;

        if let Some(prefix) = Self::prefix_parselet(token.ty()) {
            let mut left = prefix(self, token)?;

            while precedence < self.current_precedence() {
                token = self.next()?;

                if let Some(infix) = Self::infix_parselet(token.ty()) {
                    left = infix(self, token, left)?;
                } else {
                    panic!("No idea how to get here")
                }
            }

            Ok(left)
        } else {
            Err(Diagnostic::error().with_message(format!("Could not parse `{}`", token.ty())))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn new_interner() -> Arc<RwLock<StringInterner<Sym>>> {
        Arc::new(RwLock::new(StringInterner::new()))
    }

    fn emit_diagnostic(source: &str, diagnostic: Diagnostic<usize>) {
        use crunch_error::{codespan_reporting, parse_prelude::SimpleFiles};

        let mut files = SimpleFiles::new();
        files.add("<test file>", source);
        let writer = codespan_reporting::term::termcolor::StandardStream::stderr(
            codespan_reporting::term::termcolor::ColorChoice::Auto,
        );
        let config = codespan_reporting::term::Config::default();

        codespan_reporting::term::emit(&mut writer.lock(), &config, &files, &diagnostic).unwrap();
    }

    #[test]
    fn expressions() {
        simple_logger::init().unwrap();

        let source = "!5 + -10 / (test(54, test_again) % 200 if 10 else 52";

        let mut parser = Parser::new(source, 0, new_interner());
        match parser.parse_expression(0) {
            Ok(expr) => println!("{:?}", expr),
            Err(err) => emit_diagnostic(source, err),
        }
    }
}
