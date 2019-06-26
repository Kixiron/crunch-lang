use super::prelude::*;

pub fn function<'source>(
    parser: &mut Parser<'source>,
    _token: &TokenData<'source>,
    mut _tree: &mut Vec<Expr>,
) -> Expr {
    use std::cell::RefCell;

    /*
    parser.next_checked(
        |parser: _| -> Expr {
            let ident = parser.current.source().to_owned();

            parser.next_checked(
                |parser: _| -> Expr {
                    let mut parameters: RefCell<Vec<Expr>> = RefCell::new(Vec::new());

                    let mut current = Token::WhiteSpace;

                    while parser.current.kind() != Token::RightParentheses {
                       if parser.current.kind() == Token::Identifier {
                            let param_ident = parser.current.source().to_string();

                            parameters.borrow_mut().push(parser.next_checked(
                                |parser: _| -> Expr {
                                    parser.next_checked(
                                        |parser: _| -> Expr {
                                            match parser.current.kind() {
                                                Token::Int
                                                | Token::Str
                                                | Token::Bool
                                                | Token::Vector => {
                                                    let kind = match parser.current.kind() {
                                                        Token::Int => LiteralKind::Int,
                                                        Token::Str => LiteralKind::String,
                                                        Token::Bool => LiteralKind::Bool,
                                                        Token::Vector => LiteralKind::Vector,

                                                        _ => unreachable!(),
                                                    };

                                                    Expr::Parameter {
                                                        ident: param_ident, kind
                                                    }
                                                },
                                                _ => Expr::Error(vec![
                                                    EmittedError::new_error(
                                                        "Expected a variable type",
                                                        None,
                                                        &[parser
                                                            .current
                                                            .range()],
                                                    ),
                                                ]),
                                            }
                                        },
                                        None,
                                        None::<
                                            Box<dyn
                                                FnOnce(&mut Parser<'_>) -> Expr,
                                            >,
                                        >,
                                    )
                                },
                                Some(Token::Colon),
                                Some(Box::new(|parser: &mut Parser<'source>| -> Expr {
                                    Expr::Error(vec![EmittedError::new_error(
                                        "Expected a colon",
                                        None,
                                        &[parser.current.range()],
                                    )])
                                })),
                            ));

                            let body = Arena::new();
                            body.alloc(Expr::None);
                            break Expr::Method {
                                name: ident.clone(),
                                parameters: parameters.into_inner(),
                                body,
                            };
                        } else {
                            break Expr::Error(vec![EmittedError::new_error(
                                "Expected a parameter",
                                None,
                                &[parser.current.range()],
                            )]);
                        }
                    }
                },
                Some(Token::LeftParentheses),
                Some(Box::new(|parser: &mut Parser<'source>| -> Expr {
                    Expr::Error(vec![EmittedError::new_error(
                        "Expected a `(`",
                        Some(ErrorCode::E005),
                        &[parser.current.range()],
                    )])
                })),
            )
        },
        Some(Token::Identifier),
        Some(Box::new(|parser: &mut Parser<'source>| -> Expr {
            Expr::Error(vec![EmittedError::new_error(
                "Expected an identifier for the function name",
                Some(ErrorCode::E004),
                &[parser.current.range()],
            )])
        })),
    )
    */
    Expr::None
}
