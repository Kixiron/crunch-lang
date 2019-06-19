use super::prelude::*;

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
        Token::IntLiteral => super::parse_int(&token),
        Token::FloatLiteral => super::parse_float(&token),
        Token::StrLiteral => {
            // Trim off the `"`'s from the string
            LiteralValue::String(token.source()[1..token.source().len() - 1].to_owned())
        }
        Token::BoolLiteral => {
            LiteralValue::Bool(bool::from_str(token.source()).expect("Failed to parse bool"))
        }
        Token::Null => LiteralValue::Null,
        Token::VectorLiteral => super::parse_vector(&mut parser, &token, &mut tree),

        // Should be impossible
        // TODO: More graceful error handling
        _ => panic!(format!(
            "An impossible token occurred: {:?} in the `binary_operation` function",
            token
        )),
    };

    Literal { kind, value }
}
