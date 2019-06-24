use super::prelude::*;

pub fn function<'source>(
    parser: &mut Parser<'source>,
    _token: &TokenData<'source>,
    mut _tree: &mut Vec<Expr>,
) -> Expr {
    parser.next_checked(
        |parser: _| -> Expr {
            let ident = parser.current.source().to_owned();

            parser.next_checked(
                |parser: _| -> Expr {
                    // TODO: Parse things
                    Expr::None
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
}
