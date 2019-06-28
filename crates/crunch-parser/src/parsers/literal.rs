use super::prelude::*;

pub fn literal<'source>(token: &TokenData<'source>) -> Literal {
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
        Token::BoolLiteral => (LiteralKind::Bool, {
            if let Ok(boolean) = bool::from_str(token.source()) {
                LiteralValue::Bool(boolean)
            } else {
                LiteralValue::Error(vec![EmittedError::new_error(
                    "Invalid boolean",
                    None,
                    &[token.range()],
                )])
            }
        }),
        Token::VectorLiteral => {
            (LiteralKind::Vector, super::parse_vector(&token))
        }

        // Not unreachable, variables from within vectors are not checked for type, and so may fall through to this case
        _ => (
            LiteralKind::Error,
            LiteralValue::Error(vec![EmittedError::new_error(
                "Invalid type in vector",
                None,
                &[token.range()],
            )]),
        ),
    };

    Literal { kind, value }
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;

    #[test]
    fn int() {
        let int = TokenData {
            kind: Token::IntLiteral,
            source: "123456",
            range: (0, 5),
        };

        assert_eq!(
            literal(&int),
            Literal {
                kind: LiteralKind::Int,
                value: LiteralValue::Int(IntType::_i32(123456)),
            }
        );
    }

    #[test]
    fn float() {
        let float = TokenData {
            kind: Token::FloatLiteral,
            source: "1.23456",
            range: (0, 5),
        };

        assert_eq!(
            literal(&float),
            Literal {
                kind: LiteralKind::Float,
                value: LiteralValue::Float(FloatType::_f32(1.23456)),
            }
        );
    }

    #[test]
    fn string() {
        let string = TokenData {
            kind: Token::StrLiteral,
            source: "Test String",
            range: (0, 10),
        };

        assert_eq!(
            literal(&string),
            Literal {
                kind: LiteralKind::String,
                value: LiteralValue::String(String::from("Test String")),
            }
        );
    }

    #[test]
    fn bool() {
        let true_bool = TokenData {
            kind: Token::BoolLiteral,
            source: "true",
            range: (0, 3),
        };

        assert_eq!(
            literal(&true_bool),
            Literal {
                kind: LiteralKind::Bool,
                value: LiteralValue::Bool(true),
            }
        );

        let false_bool = TokenData {
            kind: Token::BoolLiteral,
            source: "false",
            range: (0, 4),
        };

        assert_eq!(
            literal(&false_bool),
            Literal {
                kind: LiteralKind::Bool,
                value: LiteralValue::Bool(false),
            }
        );
    }

    #[test]
    fn int_vector() {
        let int_vector = TokenData {
            kind: Token::VectorLiteral,
            source: "[1, 2, 3, 4, 5]",
            range: (0, 14),
        };

        let vec_literal = vec![
            Literal {
                kind: LiteralKind::Int,
                value: LiteralValue::Int(IntType::_i32(1)),
            },
            Literal {
                kind: LiteralKind::Int,
                value: LiteralValue::Int(IntType::_i32(2)),
            },
            Literal {
                kind: LiteralKind::Int,
                value: LiteralValue::Int(IntType::_i32(3)),
            },
            Literal {
                kind: LiteralKind::Int,
                value: LiteralValue::Int(IntType::_i32(4)),
            },
            Literal {
                kind: LiteralKind::Int,
                value: LiteralValue::Int(IntType::_i32(5)),
            },
        ];

        assert_eq!(
            literal(&int_vector),
            Literal {
                kind: LiteralKind::Vector,
                value: LiteralValue::Vector(vec_literal),
            }
        );
    }

    #[test]
    fn float_vector() {
        let float_vector = TokenData {
            kind: Token::VectorLiteral,
            source: "[1.23, 2.34, 3.45, 4.56, 5.67]",
            range: (0, 14),
        };

        let vec_literal = vec![
            Literal {
                kind: LiteralKind::Float,
                value: LiteralValue::Float(FloatType::_f32(1.23)),
            },
            Literal {
                kind: LiteralKind::Float,
                value: LiteralValue::Float(FloatType::_f32(2.34)),
            },
            Literal {
                kind: LiteralKind::Float,
                value: LiteralValue::Float(FloatType::_f32(3.45)),
            },
            Literal {
                kind: LiteralKind::Float,
                value: LiteralValue::Float(FloatType::_f32(4.56)),
            },
            Literal {
                kind: LiteralKind::Float,
                value: LiteralValue::Float(FloatType::_f32(5.67)),
            },
        ];

        assert_eq!(
            literal(&float_vector),
            Literal {
                kind: LiteralKind::Vector,
                value: LiteralValue::Vector(vec_literal),
            }
        );
    }

    #[test]
    fn string_vector() {
        let string_vector = TokenData {
            kind: Token::VectorLiteral,
            source: "[\"test\", \"test\", \"test\", \"test\", \"test\"]",
            range: (0, 14),
        };

        let vec_literal = vec![
            Literal {
                kind: LiteralKind::String,
                value: LiteralValue::String(String::from("test")),
            },
            Literal {
                kind: LiteralKind::String,
                value: LiteralValue::String(String::from("test")),
            },
            Literal {
                kind: LiteralKind::String,
                value: LiteralValue::String(String::from("test")),
            },
            Literal {
                kind: LiteralKind::String,
                value: LiteralValue::String(String::from("test")),
            },
            Literal {
                kind: LiteralKind::String,
                value: LiteralValue::String(String::from("test")),
            },
        ];

        assert_eq!(
            literal(&string_vector),
            Literal {
                kind: LiteralKind::Vector,
                value: LiteralValue::Vector(vec_literal),
            }
        );
    }

    #[test]
    fn bool_vector() {
        let bool_vector = TokenData {
            kind: Token::VectorLiteral,
            source: "[true, false, true, false, true]",
            range: (0, 14),
        };

        let vec_literal = vec![
            Literal {
                kind: LiteralKind::Bool,
                value: LiteralValue::Bool(true),
            },
            Literal {
                kind: LiteralKind::Bool,
                value: LiteralValue::Bool(false),
            },
            Literal {
                kind: LiteralKind::Bool,
                value: LiteralValue::Bool(true),
            },
            Literal {
                kind: LiteralKind::Bool,
                value: LiteralValue::Bool(false),
            },
            Literal {
                kind: LiteralKind::Bool,
                value: LiteralValue::Bool(true),
            },
        ];

        assert_eq!(
            literal(&bool_vector),
            Literal {
                kind: LiteralKind::Vector,
                value: LiteralValue::Vector(vec_literal),
            }
        );
    }

    #[test]
    fn vector_vector() {
        let vector_vector = TokenData {
            kind: Token::VectorLiteral,
            source: "[[true, false, true], [false, true, false], [true, false, true], [false, true, false], [true, false, true]]",
            range: (0, 14),
        };

        let vec_literal = Literal {
            kind: LiteralKind::Vector,
            value: LiteralValue::Vector(vec![
                Literal {
                    kind: LiteralKind::Vector,
                    value: LiteralValue::Vector(vec![
                        Literal {
                            kind: LiteralKind::Bool,
                            value: LiteralValue::Bool(true),
                        },
                        Literal {
                            kind: LiteralKind::Bool,
                            value: LiteralValue::Bool(false),
                        },
                        Literal {
                            kind: LiteralKind::Bool,
                            value: LiteralValue::Bool(true),
                        },
                    ]),
                },
                Literal {
                    kind: LiteralKind::Vector,
                    value: LiteralValue::Vector(vec![
                        Literal {
                            kind: LiteralKind::Bool,
                            value: LiteralValue::Bool(false),
                        },
                        Literal {
                            kind: LiteralKind::Bool,
                            value: LiteralValue::Bool(true),
                        },
                        Literal {
                            kind: LiteralKind::Bool,
                            value: LiteralValue::Bool(false),
                        },
                    ]),
                },
                Literal {
                    kind: LiteralKind::Vector,
                    value: LiteralValue::Vector(vec![
                        Literal {
                            kind: LiteralKind::Bool,
                            value: LiteralValue::Bool(true),
                        },
                        Literal {
                            kind: LiteralKind::Bool,
                            value: LiteralValue::Bool(false),
                        },
                        Literal {
                            kind: LiteralKind::Bool,
                            value: LiteralValue::Bool(true),
                        },
                    ]),
                },
                Literal {
                    kind: LiteralKind::Vector,
                    value: LiteralValue::Vector(vec![
                        Literal {
                            kind: LiteralKind::Bool,
                            value: LiteralValue::Bool(false),
                        },
                        Literal {
                            kind: LiteralKind::Bool,
                            value: LiteralValue::Bool(true),
                        },
                        Literal {
                            kind: LiteralKind::Bool,
                            value: LiteralValue::Bool(false),
                        },
                    ]),
                },
                Literal {
                    kind: LiteralKind::Vector,
                    value: LiteralValue::Vector(vec![
                        Literal {
                            kind: LiteralKind::Bool,
                            value: LiteralValue::Bool(true),
                        },
                        Literal {
                            kind: LiteralKind::Bool,
                            value: LiteralValue::Bool(false),
                        },
                        Literal {
                            kind: LiteralKind::Bool,
                            value: LiteralValue::Bool(true),
                        },
                    ]),
                },
            ]),
        };

        assert_eq!(literal(&vector_vector), vec_literal);
    }

    proptest! {
        #[test]
        fn proptest_does_not_crash_int(string in "\\PC*") {
            literal(&TokenData {
                kind: Token::IntLiteral,
                source: &string,
                range: (0, string.len()),
            });
        }

        #[test]
        fn proptest_does_not_crash_str(string in r#""\\PC*""#) {
            literal(&TokenData {
                kind: Token::StrLiteral,
                source: &string,
                range: (0, string.len()),
            });
        }

        #[test]
        fn proptest_str_result_correct(string in r#""\\PC*""#) {
            assert_eq!(literal(&TokenData {
                kind: Token::StrLiteral,
                source: &string,
                range: (0, string.len())
            }), Literal {
                kind: LiteralKind::String,
                value: LiteralValue::String(String::from(&string[1..string.len() - 1])), // String
            });
        }

        #[test]
        fn proptest_does_not_crash_float(string in "\\PC*") {
            literal(&TokenData {
                kind: Token::FloatLiteral,
                source: &string,
                range: (0, string.len()),
            });
        }

        #[test]
        fn proptest_does_not_crash_bool(string in "\\PC*") {
            literal(&TokenData {
                kind: Token::BoolLiteral,
                source: &string,
                range: (0, string.len()),
            });
        }

        #[test]
        fn proptest_does_not_crash_vector(string in r#"\[[-0-9a-zA-Z_]+\]"#) {
            literal(&TokenData {
                kind: Token::VectorLiteral,
                source: &string,
                range: (0, string.len()),
            });
        }
    }
}
