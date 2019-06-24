use super::prelude::*;

pub fn variable<'source>(
    parser: &mut Parser<'source>,
    mut tree: &mut Vec<Expr>,
) -> Expr {
    parser.next_checked(
        |parser: _| -> Expr {
            let ident = parser.current.source().to_owned();

            parser.next_checked(
                |parser: _| -> Expr {
                    parser.next_checked(
                        |parser: _| -> Expr {
                            let kind = Arena::with_capacity(1);
                            kind.alloc(
                                parser.eval_expr(
                                    &parser.current.clone(),
                                    &mut tree,
                                ),
                            );

                            parser.next_checked(
                                |parser: _| -> Expr {
                                    parser.next_checked(
                                        |parser: _| -> Expr {
                                            let literal =
                                                Arena::with_capacity(1);
                                            literal.alloc(parser.eval_expr(
                                                &parser.current.clone(),
                                                &mut tree,
                                            ));

                                            parser.next_checked(
                                                |_parser: _| -> Expr {
                                                    Expr::Variable {
                                                        ident,
                                                        kind,
                                                        literal,
                                                    }
                                                },
                                                Some(Token::SemiColon),
                                                Some(Box::new(
                                                    |parser: &mut Parser<'source>| -> Expr {
                                                        Expr::Invalid(
                                                            "Expected a ;"
                                                                .to_string(),
                                                            parser
                                                                .current
                                                                .range(),
                                                        )
                                                    },
                                                )),
                                            )
                                        },
                                        None,
                                        None::<
                                            Box<
                                                FnOnce(&mut Parser<'_>) -> Expr,
                                            >,
                                        >,
                                    )
                                },
                                Some(Token::Equals),
                                Some(Box::new(|parser: &mut Parser<'source>| -> Expr {
                                    Expr::Invalid(
                                        "Expected an =".to_string(),
                                        parser.current.range(),
                                    )
                                })),
                            )
                        },
                        None,
                        None::<Box<FnOnce(&mut Parser<'_>) -> Expr>>,
                    )
                },
                Some(Token::Colon),
                Some(Box::new(|parser: &mut Parser<'source>| -> Expr {
                    Expr::Invalid(
                        "Expected a :".to_string(),
                        parser.current.range(),
                    )
                })),
            )
        },
        Some(Token::Identifier),
        Some(Box::new(|parser: &mut Parser<'source>| -> Expr {
            Expr::Invalid(
                "Expected an identifier".to_string(),
                parser.current.range(),
            )
        })),
    )
}

pub fn variable_type<'source>(token: &TokenData<'source>) -> Expr {
    Expr::VariableType(match token.kind() {
        Token::Int => LiteralKind::Int,
        Token::Str => LiteralKind::String,
        Token::Bool => LiteralKind::Bool,
        Token::Vector => LiteralKind::Vector,

        _ => unreachable!(),
    })
}
