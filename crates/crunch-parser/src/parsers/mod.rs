mod binary_operations;
mod literal;
mod variable;
mod variable_parsing;
mod prelude {
    pub use crate::*;
    pub use crunch_token::*;
}

pub use binary_operations::*;
pub use literal::*;
pub use variable::*;
pub use variable_parsing::*;

use prelude::*;

pub fn parse_int<'source>(token: &TokenData<'source>) -> LiteralValue {
    let source = token.source().to_owned().replace("_", "");

    let sign = if source.chars().nth(0) == Some('-') {
        Sign::Negative
    } else {
        Sign::Positive
    };

    // This is a thing.
    // It checks if each int type is parsable to, if not it assumes an overflow and
    // continues to the next largest int size until it hits u128, where it reports that the integer is invalid
    // TODO: No assumptions. Find out if the int has overflowed or is truly invalid
    // TODO: Maybe match prospective int's length against the string length of each int type to make parsing quicker?
    match source.parse::<i32>() {
        Ok(int) => LiteralValue::Int(IntType::_i32(int)),
        Err(_) => match source.parse::<u32>() {
            Ok(int) => LiteralValue::Int(IntType::_u32(int, sign)),
            Err(_) => match source.parse::<i64>() {
                Ok(int) => LiteralValue::Int(IntType::_i64(int)),
                Err(_) => match source.parse::<u64>() {
                    Ok(int) => LiteralValue::Int(IntType::_u64(int, sign)),
                    Err(_) => match source.parse::<i128>() {
                        Ok(int) => LiteralValue::Int(IntType::_i128(int)),
                        Err(_) => match source.parse::<u128>() {
                            Ok(int) => LiteralValue::Int(IntType::_u128(int, sign)),
                            Err(_) => panic!(
                                format!(
                                    "`{}` is not a valid integer. You might try removing invalid characters or making it shorter",
                                    token.source()
                                ),

                            ),
                        },
                    },
                },
            },
        },
    }
}

fn parse_vector<'source>(
    parser: &mut Parser<'source>,
    token: &TokenData<'source>,
    mut tree: &mut Vec<Expr>,
) -> LiteralValue {
    let mut vector: Vec<Literal> = Vec::new();

    {
        let stream = TokenStream::new(&token.source()[1..token.source.len() - 1]).filter(|token| {
            token.kind() == Token::WhiteSpace
                || token.kind() == Token::Comma
                || token.kind() == Token::LeftBracket
                || token.kind() == Token::RightBracket
        });

        for token in stream {
            vector.push(literal(parser, &token, &mut tree))
        }
    }

    LiteralValue::Vector(vector)
}

fn parse_float<'source>(token: &TokenData<'source>) -> LiteralValue {
    match token.source().parse::<f32>() {
        Ok(float) => LiteralValue::Float(FloatType::_f32(float)),

        // If parsing fails, attempt to parse as a 64-bit float
        Err(_) => match token.source().parse::<f64>() {
            Ok(float) => LiteralValue::Float(FloatType::_f64(float)),

            // If all parsing attempts fail, then it is not valid
            Err(_) => panic!("Unable to parse float"),
        },
    }
}

pub fn literal<'source>(
    mut parser: &mut Parser<'source>,
    token: &TokenData<'source>,
    mut tree: &mut Vec<Expr>,
) -> Literal {
    use std::str::FromStr;

    let kind = match token.kind() {
        Token::IntLiteral => LiteralKind::Int,
        Token::FloatLiteral => LiteralKind::Float,
        Token::StrLiteral => LiteralKind::String,
        Token::BoolLiteral => LiteralKind::Bool,
        Token::VectorLiteral => LiteralKind::Vector,

        _ => panic!("Invalid type"),
    };

    let value = match token.kind() {
        Token::IntLiteral => parse_int(&token),
        Token::FloatLiteral => parse_float(&token),
        Token::StrLiteral => {
            LiteralValue::String(token.source()[1..token.source().len() - 1].to_owned())
        }
        Token::BoolLiteral => {
            LiteralValue::Bool(bool::from_str(token.source()).expect("Failed to parse bool"))
        }
        Token::Null => LiteralValue::Null,
        Token::VectorLiteral => parse_vector(&mut parser, &token, &mut tree),

        // Should be impossible
        // TODO: More graceful error handling
        _ => panic!(format!(
            "An impossible token occurred: {:?} in the `binary_operation` function",
            token
        )),
    };

    Literal { kind, value }
}

pub fn binary_operation<'source>(
    parser: &mut Parser<'source>,
    token: &TokenData<'source>,
    mut tree: &mut Vec<Expr>,
) -> Expr {
    let operation = match token.kind() {
        Token::Multiply => Op::Multiply,
        Token::Divide => Op::Divide,
        Token::Plus => Op::Add,
        Token::Minus => Op::Subtract,
        Token::Or => Op::Or,
        Token::And => Op::And,
        Token::Not => Op::Not,
        // TODO: Add all binary operations

        // Should be impossible
        // TODO: More graceful error handling
        _ => panic!(format!(
            "An impossible token occurred: {:?} in the `binary_operation` function",
            token
        )),
    };

    Expr::BinaryOp {
        operation,
        // The left-hand side of the operation is the last Expr in the Expr tree
        left_hand: Box::new(match tree.pop() {
            // If there is a last Expr, insert it into the
            Some(expr) => expr,
            // If there is no last Expr, return an invalid Expr
            None => Expr::Invalid(
                "No left-hand side was provided to the multiplication operation!".to_string(),
                parser.current.range(),
            ),
        }),
        // Populate the right-hand side with the next Expr
        right_hand: Box::new(parser.eval_expr(&parser.current.clone(), &mut tree)),
    }
}
