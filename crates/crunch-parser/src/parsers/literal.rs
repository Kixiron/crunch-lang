use super::prelude::*;

pub fn literal<'source>(
    token: &TokenData<'source>,
    mut tree: &mut Vec<Expr>,
) -> Literal {
    use std::str::FromStr;

    let (kind, value) = match token.kind() {
        Token::IntLiteral => (LiteralKind::Int, super::parse_int(&token)),
        Token::FloatLiteral => (LiteralKind::Float, super::parse_float(&token)),
        Token::StrLiteral => (
            LiteralKind::String,
            LiteralValue::String(
                token.source()[1..token.source().len() - 1].to_owned(),
            ),
        ),
        Token::BoolLiteral => (
            LiteralKind::Bool,
            LiteralValue::Bool(
                bool::from_str(token.source()).expect("Failed to parse bool"),
            ),
        ),
        Token::VectorLiteral => {
            (LiteralKind::Vector, super::parse_vector(&token, &mut tree))
        }

        _ => unreachable!(),
    };

    Literal { kind, value }
}
