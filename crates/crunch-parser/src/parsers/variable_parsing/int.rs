use super::prelude::*;

lazy_static::lazy_static! {
    // Using lazy_static to load int parse errors to compare against any errors returned by the parsing of integers
    // Waiting on https://github.com/rust-lang/rust/issues/22639 to be stabilized before more efficient matching can be done
    pub static ref INT_PARSE_OVERFLOW: std::num::ParseIntError = "1231123123123123".parse::<u32>().err().unwrap();
    pub static ref INT_PARSE_UNDERFLOW: std::num::ParseIntError = "-11231123123123123".parse::<i32>().err().unwrap();
    pub static ref INT_PARSE_INVALID_CHAR: std::num::ParseIntError = "error".parse::<u32>().err().unwrap();
}

// Convenience macro to throw an invalid integer error
macro_rules! return_parse_error {
    ($token:ident, $err:expr) => {{
        if $err == Some(&*INT_PARSE_INVALID_CHAR) {
            LiteralValue::Error(vec![EmittedError::new_error(
                "Invalid characters in integer",
                None,
                &[$token.range()],
            )])
        } else {
            LiteralValue::Error(vec![EmittedError::new_error(
                "Invalid integer. Try making it shorter",
                None,
                &[$token.range()],
            )])
        }
    }};
}

pub fn parse_int<'source>(token: &TokenData<'source>) -> LiteralValue {
    if token.source().starts_with("0x") {
        parse_hex(token)
    } else {
        parse_integer(token)
    }
}

// TODO: Find a way to use the same unsigned system as parse_integer
fn parse_hex<'source>(token: &TokenData<'source>) -> LiteralValue {
    // Process and trim the source string
    let source = token.source().trim().trim_start_matches("0x");
    debug_assert!(!source.starts_with("0x"), "Error parsing integer");

    match i32::from_str_radix(source, 16) {
        // Parse for i32
        Ok(int) => LiteralValue::Int(IntType::_i32(int)), // If the parse is successful, return the integer
        Err(ref err)
            if err != &*INT_PARSE_OVERFLOW && err != &*INT_PARSE_UNDERFLOW =>
        {
            return_parse_error!(token, Some(err))
        }

        // Parse for i64
        _ => match i64::from_str_radix(source, 16) {
            Ok(int) => LiteralValue::Int(IntType::_i64(int)),
            Err(ref err)
                if err != &*INT_PARSE_OVERFLOW
                    && err != &*INT_PARSE_UNDERFLOW =>
            {
                return_parse_error!(token, Some(err))
            }

            // Parse for i128
            _ => match i128::from_str_radix(source, 16) {
                Ok(int) => LiteralValue::Int(IntType::_i128(int)),
                _ => return_parse_error!(token, None),
            },
        },
    }
}

fn parse_integer<'source>(token: &TokenData<'source>) -> LiteralValue {
    // Process and trim the source string
    let source = token.source().trim().replace("_", "");

    // Lazily get the sign of the integer if required by one of the unsigned int types
    let sign = if source.chars().nth(0) == Some('-') {
        Sign::Negative
    } else {
        Sign::Positive
    };

    // This is a thing.
    // It checks if each int type is parsable to, if not it assumes an overflow and
    // continues to the next largest int size until it hits u128, where it reports that the integer is invalid
    // TODO: Maybe match prospective int's length against the string length of each int type to make parsing quicker?
    // Waiting on https://github.com/rust-lang/rust/issues/22639 to be stabilized before more efficient matching can be done
    match source.parse::<i32>() {
        // Parse for i32
        Ok(int) => LiteralValue::Int(IntType::_i32(int)), // If the parse is successful, return the integer
        Err(ref err)
            if err != &*INT_PARSE_OVERFLOW && err != &*INT_PARSE_UNDERFLOW =>
        {
            return_parse_error!(token, Some(err))
        }

        // Parse for u32
        _ => match {
            if sign == Sign::Negative {
                source[1..].parse::<u32>()
            } else {
                source.parse::<u32>()
            }
        } {
            Ok(int) => LiteralValue::Int(IntType::_u32(int, sign)),
            Err(ref err)
                if err != &*INT_PARSE_OVERFLOW
                    && err != &*INT_PARSE_UNDERFLOW =>
            {
                return_parse_error!(token, Some(err))
            }

            // Parse for i64
            _ => match source.parse::<i64>() {
                Ok(int) => LiteralValue::Int(IntType::_i64(int)),
                Err(ref err)
                    if err != &*INT_PARSE_OVERFLOW
                        && err != &*INT_PARSE_UNDERFLOW =>
                {
                    return_parse_error!(token, Some(err))
                }

                // Parse for u64
                _ => match {
                    if sign == Sign::Negative {
                        source[1..].parse::<u64>()
                    } else {
                        source.parse::<u64>()
                    }
                } {
                    Ok(int) => LiteralValue::Int(IntType::_u64(int, sign)),
                    Err(ref err)
                        if err != &*INT_PARSE_OVERFLOW
                            && err != &*INT_PARSE_UNDERFLOW =>
                    {
                        return_parse_error!(token, Some(err))
                    }

                    // Parse for i128
                    _ => match source.parse::<i128>() {
                        Ok(int) => LiteralValue::Int(IntType::_i128(int)),
                        Err(ref err)
                            if err != &*INT_PARSE_OVERFLOW
                                && err != &*INT_PARSE_UNDERFLOW =>
                        {
                            return_parse_error!(token, Some(err))
                        }

                        // Parse for u128
                        _ => match {
                            if sign == Sign::Negative {
                                source[1..].parse::<u128>()
                            } else {
                                source.parse::<u128>()
                            }
                        } {
                            Ok(int) => {
                                LiteralValue::Int(IntType::_u128(int, sign))
                            }
                            _ => return_parse_error!(token, None),
                        },
                    },
                },
            },
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;

    #[test]
    fn static_errors() {
        assert_eq!(
            format!("{:?}", *INT_PARSE_OVERFLOW),
            "ParseIntError { kind: Overflow }"
        );

        assert_eq!(
            format!("{:?}", *INT_PARSE_UNDERFLOW),
            "ParseIntError { kind: Underflow }"
        );

        assert_eq!(
            format!("{:?}", *INT_PARSE_INVALID_CHAR),
            "ParseIntError { kind: InvalidDigit }"
        );
    }

    #[test]
    fn parse_overflow() {
        let overflow_all = TokenData {
            kind: Token::IntLiteral,
            source: "44028236692093846346339997460743176821145",
            range: (0, 1),
        };
        assert_eq!(
            parse_int(&overflow_all),
            LiteralValue::Error(vec![EmittedError::new_error(
                "Invalid integer. Try removing invalid characters or making it shorter",
                None,
                &[],
            )])
        );
    }

    #[test]
    fn parse_32_bit() {
        let i32_max = TokenData {
            kind: Token::IntLiteral,
            source: "2147483647",
            range: (0, 1),
        };
        assert_eq!(
            parse_int(&i32_max),
            LiteralValue::Int(IntType::_i32(2147483647))
        );
        drop(i32_max);

        let i32_min = TokenData {
            kind: Token::IntLiteral,
            source: "-2147483647",
            range: (0, 1),
        };
        assert_eq!(
            parse_int(&i32_min),
            LiteralValue::Int(IntType::_i32(-2147483647))
        );
        drop(i32_min);

        let u32_max_positive = TokenData {
            kind: Token::IntLiteral,
            source: "4294967295",
            range: (0, 1),
        };
        assert_eq!(
            parse_int(&u32_max_positive),
            LiteralValue::Int(IntType::_u32(4294967295, Sign::Positive))
        );
        drop(u32_max_positive);

        let u32_max_negative = TokenData {
            kind: Token::IntLiteral,
            source: "-4294967295",
            range: (0, 1),
        };
        assert_eq!(
            parse_int(&u32_max_negative),
            LiteralValue::Int(IntType::_u32(4294967295, Sign::Negative))
        );
    }

    #[test]
    fn parse_64_bit() {
        let i64_max = TokenData {
            kind: Token::IntLiteral,
            source: "9223372036854775807",
            range: (0, 1),
        };
        assert_eq!(
            parse_int(&i64_max),
            LiteralValue::Int(IntType::_i64(9223372036854775807))
        );
        drop(i64_max);

        let i64_min = TokenData {
            kind: Token::IntLiteral,
            source: "-9223372036854775807",
            range: (0, 1),
        };
        assert_eq!(
            parse_int(&i64_min),
            LiteralValue::Int(IntType::_i64(-9223372036854775807))
        );
        drop(i64_min);

        let u64_max_positive = TokenData {
            kind: Token::IntLiteral,
            source: "18446744073709551615",
            range: (0, 1),
        };
        assert_eq!(
            parse_int(&u64_max_positive),
            LiteralValue::Int(IntType::_u64(
                18446744073709551615,
                Sign::Positive
            ))
        );
        drop(u64_max_positive);

        let u64_max_negative = TokenData {
            kind: Token::IntLiteral,
            source: "-18446744073709551615",
            range: (0, 1),
        };
        assert_eq!(
            parse_int(&u64_max_negative),
            LiteralValue::Int(IntType::_u64(
                18446744073709551615,
                Sign::Negative
            ))
        );
    }

    #[test]
    fn parse_128_bit() {
        let i128_max = TokenData {
            kind: Token::IntLiteral,
            source: "170141183460469231731687303715884105727",
            range: (0, 1),
        };
        assert_eq!(
            parse_int(&i128_max),
            LiteralValue::Int(IntType::_i128(
                170141183460469231731687303715884105727
            ))
        );
        drop(i128_max);

        let i128_min = TokenData {
            kind: Token::IntLiteral,
            source: "-170141183460469231731687303715884105727",
            range: (0, 1),
        };
        assert_eq!(
            parse_int(&i128_min),
            LiteralValue::Int(IntType::_i128(
                -170141183460469231731687303715884105727
            ))
        );
        drop(i128_min);

        let u128_max_positive = TokenData {
            kind: Token::IntLiteral,
            source: "34028236692093846346337460743176821145",
            range: (0, 1),
        };
        assert_eq!(
            parse_int(&u128_max_positive),
            LiteralValue::Int(IntType::_u128(
                34028236692093846346337460743176821145,
                Sign::Positive
            ))
        );
        drop(u128_max_positive);

        let u128_max_negative = TokenData {
            kind: Token::IntLiteral,
            source: "-34028236692093846346337460743176821145",
            range: (0, 1),
        };
        assert_eq!(
            parse_int(&u128_max_negative),
            LiteralValue::Int(IntType::_u128(
                34028236692093846346337460743176821145,
                Sign::Negative
            ))
        );
    }

    #[test]
    fn parse_hex() {
        let hex = TokenData {
            kind: Token::IntLiteral,
            source: "0xF0A23",
            range: (0, 1),
        };
        assert_eq!(parse_int(&hex), LiteralValue::Int(IntType::_i32(985635)));
    }

    proptest! {
        #[test]
        fn proptest_does_not_crash(int in "\\PC*") {
            parse_int(&TokenData {
                kind: Token::IntLiteral,
                source: &int,
                range: (0, int.len()),
            });
        }

        #[test]
        fn proptest_does_not_crash_int(int in "[^0x]{2}\\PC*") {
            // " Once again, I value my syntax highlighting
            parse_int(&TokenData {
                kind: Token::IntLiteral,
                source: &int,
                range: (0, int.len()),
            });
        }

        #[test]
        fn proptest_does_not_crash_hex(int in "0x\\PC*") {
            parse_int(&TokenData {
                kind: Token::IntLiteral,
                source: &int,
                range: (0, int.len()),
            });
        }

        #[test]
        fn proptest_int(int in r#"-?[0-9_]+"#) {
            parse_int(&TokenData {
                kind: Token::IntLiteral,
                source: &int,
                range: (0, int.len()),
            });
        }

        #[test]
        fn proptest_hex(int in "0x[0-9A-Fa-f]+") {
            parse_int(&TokenData {
                kind: Token::IntLiteral,
                source: &int,
                range: (0, int.len()),
            });
        }
    }
}
