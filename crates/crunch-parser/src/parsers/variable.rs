use super::prelude::*;

pub fn variable<'source>(
    parser: &mut Parser<'source>,
    _token: &TokenData<'source>,
    mut tree: &mut Vec<Expr>,
) -> Expr {
    parser.next_checked(|parser: _| -> Expr {
        if parser.current.kind() == Token::Identifier {
            let ident = parser.current.source().to_owned();

            parser.next_checked(|parser: _| -> Expr {
                if parser.current.kind() == Token::Colon {
                    parser.next_checked(|parser: _| -> Expr {
                        let kind = Arena::with_capacity(1);
                        kind.alloc(
                            parser
                                .eval_expr(&parser.current.clone(), &mut tree),
                        );

                        parser.next_checked(|parser: _| -> Expr {
                            if parser.current.kind() == Token::Equals {
                                parser.next_checked(|parser: _| -> Expr {
                                    let literal = Arena::with_capacity(1);
                                    literal.alloc(parser.eval_expr(
                                        &parser.current.clone(),
                                        &mut tree,
                                    ));

                                    parser.next_checked(|parser: _| -> Expr {
                                        if parser.current.kind()
                                            == Token::SemiColon
                                        {
                                            Expr::Variable {
                                                ident,
                                                kind,
                                                literal,
                                            }
                                        } else {
                                            Expr::Invalid(
                                                "Expected a ;".to_string(),
                                                parser.current.range(),
                                            )
                                        }
                                    })
                                })
                            } else {
                                Expr::Invalid(
                                    "Expected an =".to_string(),
                                    parser.current.range(),
                                )
                            }
                        })
                    })
                } else {
                    Expr::Invalid(
                        "Expected a :".to_string(),
                        parser.current.range(),
                    )
                }
            })
        } else {
            Expr::Invalid(
                "Expected an identifier".to_string(),
                parser.current.range(),
            )
        }
    })
}

pub fn variable_type<'source>(
    _parser: &mut Parser<'source>,
    token: &TokenData<'source>,
    _tree: &mut Vec<Expr>,
) -> Expr {
    Expr::VariableType(match token.kind() {
        Token::Int => LiteralKind::Int,
        Token::Str => LiteralKind::String,
        Token::Bool => LiteralKind::Bool,
        Token::Vector => LiteralKind::Vector,

        _ => panic!("Impossible token"),
    })
}
