use super::prelude::*;

pub fn binary_operation<'source>(
    parser: &mut Parser<'source>,
    token: &TokenData<'source>,
    mut tree: &mut Vec<Expr>,
) -> Expr {
    let operation = match token.kind() {
        Token::Multiply => Op::Multiply,
        Token::Divide => Op::Divide,
        Token::Plus => Op::Add,
        Token::Minus => Op::Subtract,
        Token::Or => Op::Or,
        Token::And => Op::And,
        Token::Not => Op::Not,

        // TODO: Add all binary operations
        _ => unreachable!(),
    };

    let left_hand = Arena::with_capacity(1);
    left_hand.alloc(match tree.pop() {
        // If there is a last Expr, insert it into the
        Some(expr) => expr,
        // If there is no last Expr, return an invalid Expr
        None => Expr::Invalid(
            "No left-hand side was provided to the multiplication operation!"
                .to_string(),
            parser.current.range(),
        ),
    });

    let right_hand = Arena::with_capacity(1);
    right_hand.alloc(parser.eval_expr(&parser.current.clone(), &mut tree));

    Expr::BinaryOp {
        operation,
        // The left-hand side of the operation is the last Expr in the Expr tree
        left_hand,
        // Populate the right-hand side with the next Expr
        right_hand,
    }
}
