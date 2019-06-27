use super::prelude::*;

pub fn parse_vector<'source>(token: &TokenData<'source>) -> LiteralValue {
    let mut vector: Vec<Literal> = Vec::new();

    {
        let stream =
            TokenStream::new(&token.source()[1..token.source.len() - 1])
                .filter(|token| {
                    token.kind() == Token::WhiteSpace
                        || token.kind() == Token::Comma
                        || token.kind() == Token::LeftBracket
                        || token.kind() == Token::RightBracket
                });

        for token in stream {
            vector.push(crate::parsers::literal::literal(&token))
        }
    }

    LiteralValue::Vector(vector)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parsers::literal::literal;
    use proptest::prelude::*;

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
    fn vector_vector() {}

    proptest! {
        #[test]
        fn proptest_does_not_crash(vector in "\\PC*") {
            parse_vector(&TokenData {
                kind: Token::VectorLiteral,
                source: &vector,
                range: (0, vector.len()),
            });
        }

        #[test]
        fn proptest_vector(vector in r#"Vec<[-0-9a-zA-Z_]+>"#) {
            parse_vector(&TokenData {
                kind: Token::VectorLiteral,
                source: &vector,
                range: (0, vector.len()),
            });
        }
    }
}
