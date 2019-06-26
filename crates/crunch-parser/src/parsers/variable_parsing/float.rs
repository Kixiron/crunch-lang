use super::prelude::*;

pub fn parse_float<'source>(token: &TokenData<'source>) -> LiteralValue {
    match token.source().parse::<f32>() {
        Ok(float) => LiteralValue::Float(FloatType::_f32(float)),

        // If parsing fails, attempt to parse as a 64-bit float
        Err(_) => match token.source().parse::<f64>() {
            Ok(float) => LiteralValue::Float(FloatType::_f64(float)),

            // If all parsing attempts fail, then it is not valid
            Err(err) => panic!("{:?}", err),
        },
    }
}
