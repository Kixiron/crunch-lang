use super::prelude::*;

pub fn parse_float<'source>(token: &TokenData<'source>) -> LiteralValue {
    // Process and trim the source string
    let source = token.source().trim().replace("_", "");

    // The same error-testing logic applied to parsing integers would be applied here, but
    // std::num::ParseFloatError is incredibly basic and only has the variants `Empty` and
    // `Invalid`, which does not really help anything. An external float parser might solve
    // this, but for now slower float parsing will be the solution

    match source.parse::<f32>() {
        Ok(float) => LiteralValue::Float(FloatType::_f32(float)),

        _ => match source.parse::<f64>() {
            Ok(float) => LiteralValue::Float(FloatType::_f64(float)),

            _ => LiteralValue::Error(vec![EmittedError::new_error(
                "Invalid integer. Try making it shorter or removing invalid characters",
                None,
                &[token.range()],
            )]),
        },
    }
}

// TODO: Tests
#[cfg(test)]
mod tests {}
