use super::prelude::*;

pub fn for_loop<'source>(
    parser: &mut Parser<'source>,
    mut tree: &mut Vec<Expr>,
) -> Expr {
    parser.next_checked(|parser: &mut Parser<'source>| -> Expr {
        let item = parser.current.source().to_owned();

        parser.next_checked(|parser: &mut Parser<'source>| -> Expr {
            parser.next_checked(|parser: &mut Parser<'source>| -> Expr {
                let collection = Arena::with_capacity(1);
                collection.alloc(parser.eval_expr(&parser.current.clone(), &mut tree));

                parser.next_checked(|parser: &mut Parser<'source>| -> Expr {
                    // TODO: This needs more checks for brackets and such
                    Expr::For {
                        item,
                        collection,
                        body: {
                            let body = Arena::with_capacity(1);
                            body.alloc(parser.eval_expr(&parser.current.clone(), &mut tree));
                            body
                        },
                    }
                }, None, None::<Box<FnOnce(&mut Parser<'source>) -> Expr>>,)
            },
            Some(Token::Identifier),
            Some(Box::new(|parser: &mut Parser<'source>| -> Expr {
                Expr::Error(vec![
                    EmittedError::new_error("Missing a collection to iterate over!", Some(ErrorCode::E005), &[parser.current.range()])
                ])
            })))

        },
        Some(Token::In),
        Some(Box::new(|parser: &mut Parser<'source>| -> Expr {
            Expr::Error(
                vec![
                    EmittedError::new_error("Missing an `in`", Some(ErrorCode::E002), &[parser.current.range()]),
                    EmittedError::new_help("Try adding an `in` along with a collection to be iterated over in your for loop", None, &[]),
                ]
            )
        })))
    },
    Some(Token::Identifier),
    Some(Box::new(|parser: &mut Parser<'source>| -> Expr {
        Expr::Error(
            vec![
                EmittedError::new_error("Missing identifier", Some(ErrorCode::E001), &[parser.current.range()]),
                EmittedError::new_help("Try using a valid identifier", None, &[]),
                EmittedError::new_note("Variable names may only have upper and lowercase A-Z, 0-9 and underscores in them, and the first character must be either a letter or an underscore", None, &[])
            ]
        )
    })))
}
