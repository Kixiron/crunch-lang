use super::prelude::*;

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

pub fn parse_vector<'source>(
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
            vector.push(super::literal(parser, &token, &mut tree))
        }
    }

    LiteralValue::Vector(vector)
}

pub fn parse_float<'source>(token: &TokenData<'source>) -> LiteralValue {
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
