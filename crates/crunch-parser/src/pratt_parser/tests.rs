use super::*;

use crunch_error::parse_prelude::{trace, Diagnostic, Label, ParseResult};
use string_interner::{StringInterner, Symbol};

use core::{convert::TryFrom, num::NonZeroU64};
use std::sync::{Arc, RwLock};

// fn emit_diagnostic(
//     source: &str,
//     diagnostic: crunch_error::codespan_reporting::diagnostic::Diagnostic<usize>,
// ) {
//     use crunch_error::{codespan_reporting, parse_prelude::SimpleFiles};
//
//     let mut files = SimpleFiles::new();
//     files.add("<test file>", source);
//     let writer = codespan_reporting::term::termcolor::StandardStream::stderr(
//         codespan_reporting::term::termcolor::ColorChoice::Auto,
//     );
//     let config = codespan_reporting::term::Config::default();
//
//     codespan_reporting::term::emit(&mut writer.lock(), &config, &files, &diagnostic).unwrap();
// }

// TODO: Make this a result when Diagnostics are Debug
fn parse_expr<'a>(source: &'a str, idents: Vec<&str>) -> Option<Expression> {
    let interner = Arc::new(RwLock::new(StringInterner::new()));
    for i in idents {
        interner.write().unwrap().get_or_intern(i);
    }

    Parser::new(source, 0, interner).expr().ok()
}
fn parse_stmt<'a>(source: &'a str, idents: Vec<&str>) -> Option<Statement> {
    let interner = Arc::new(RwLock::new(StringInterner::new()));
    for i in idents {
        interner.write().unwrap().get_or_intern(i);
    }

    Parser::new(source, 0, interner).stmt().ok()
}

#[test]
fn expr() {
    assert_eq!(
        Some(Expression::UnaryExpr(
            UnaryOperand::Not,
            Box::new(Expression::BinaryOp(
                Box::new(Expression::Literal(Literal::I32(5))),
                BinaryOperand::Add,
                Box::new(Expression::UnaryExpr(
                    UnaryOperand::Negative,
                    Box::new(Expression::BinaryOp(
                        Box::new(Expression::Literal(Literal::I32(10))),
                        BinaryOperand::Div,
                        Box::new(Expression::BinaryOp(
                            Box::new(Expression::Parenthesised(Box::new(Expression::BinaryOp(
                                Box::new(Expression::FunctionCall {
                                    caller: Box::new(Expression::Variable(0.into())),
                                    arguments: vec![
                                        Expression::Literal(Literal::I32(54)),
                                        Expression::Variable(1.into())
                                    ]
                                }),
                                BinaryOperand::Mod,
                                Box::new(Expression::InlineConditional {
                                    true_arm: Box::new(Expression::Literal(Literal::I32(200))),
                                    condition: Box::new(Expression::Literal(Literal::I32(10))),
                                    false_arm: Box::new(Expression::Literal(Literal::I32(52))),
                                })
                            )))),
                            BinaryOperand::Sub,
                            Box::new(Expression::Parenthesised(Box::new(Expression::Literal(
                                Literal::Bool(true)
                            ))))
                        ))
                    ))
                ))
            )),
        )),
        parse_expr(
            "!5 + -10 / (test(54, test_again) % 200 if 10 else 52) - (true)",
            vec!["test", "test_again"]
        )
    );
}

#[test]
fn single_paren_expr() {
    assert_eq!(
        Some(Expression::Comparison(
            Box::new(Expression::Literal(Literal::I32(10))),
            ComparisonOperand::Equal,
            Box::new(Expression::Parenthesised(Box::new(Expression::Literal(
                Literal::I32(0)
            ))))
        )),
        parse_expr("10 == (0)", Vec::new())
    );
}

#[test]
fn index_array_expr() {
    assert_eq!(
        Some(Expression::IndexArray {
            array: Box::new(Expression::Variable(0.into())),
            index: Box::new(Expression::Literal(Literal::I32(0))),
        }),
        parse_expr("array[0]", vec!["array"])
    );
}

#[test]
fn array_literal_expr() {
    assert_eq!(
        Some(Expression::Array(vec![
            Expression::Literal(Literal::Bool(true)),
            Expression::Variable(0.into()),
            Expression::BinaryOp(
                Box::new(Expression::Literal(Literal::I32(100))),
                BinaryOperand::Div,
                Box::new(Expression::Literal(Literal::I32(5)))
            )
        ])),
        parse_expr("[true, test, 100 / 5]", vec!["test"]),
    );
}

#[test]
fn comparisons_expr() {
    let ten = Box::new(Expression::Literal(Literal::I32(10)));
    let nine = Box::new(Expression::Literal(Literal::I32(9)));

    assert_eq!(
        Some(Expression::Comparison(
            ten.clone(),
            ComparisonOperand::Greater,
            nine.clone()
        )),
        parse_expr("10 > 9", Vec::new())
    );

    assert_eq!(
        Some(Expression::Comparison(
            ten.clone(),
            ComparisonOperand::Less,
            nine.clone()
        )),
        parse_expr("10 < 9", Vec::new())
    );

    assert_eq!(
        Some(Expression::Comparison(
            ten.clone(),
            ComparisonOperand::GreaterEqual,
            nine.clone()
        )),
        parse_expr("10 >= 9", Vec::new())
    );

    assert_eq!(
        Some(Expression::Comparison(
            ten.clone(),
            ComparisonOperand::LessEqual,
            nine.clone()
        )),
        parse_expr("10 <= 9", Vec::new())
    );

    assert_eq!(
        Some(Expression::Comparison(
            ten.clone(),
            ComparisonOperand::Equal,
            nine.clone()
        )),
        parse_expr("10 == 9", Vec::new())
    );

    assert_eq!(
        Some(Expression::Comparison(
            ten.clone(),
            ComparisonOperand::NotEqual,
            nine.clone()
        )),
        parse_expr("10 != 9", Vec::new())
    );
}

#[test]
fn literals_expr() {
    assert_eq!(
        Some(Expression::Literal(Literal::I32(100))),
        parse_expr("100", Vec::new())
    );

    assert_eq!(
        Some(Expression::Literal(Literal::Bool(false))),
        parse_expr("false", Vec::new())
    );

    assert_eq!(
        Some(Expression::Literal(Literal::Bool(true))),
        parse_expr("true", Vec::new())
    );
}

#[test]
fn func_call_expr() {
    assert_eq!(
        Some(Expression::FunctionCall {
            caller: Box::new(Expression::Variable(0.into())),
            arguments: vec![
                Expression::Variable(1.into()),
                Expression::Literal(Literal::I32(0))
            ],
        }),
        parse_expr("test(test2, 0)", vec!["test", "test2"])
    );

    assert_eq!(
        Some(Expression::FunctionCall {
            caller: Box::new(Expression::Variable(0.into())),
            arguments: Vec::new(),
        }),
        parse_expr("test_func()", vec!["test_func"])
    );
}

#[test]
fn dotted_func_call_expr() {
    assert_eq!(
        Some(Expression::FunctionCall {
            caller: Box::new(Expression::Variable(1.into())),
            arguments: vec![Expression::Variable(0.into())],
        }),
        parse_expr("test_func.function()", vec!["test_func", "function"])
    );
}

#[test]
fn assignment_expr() {
    assert_eq!(
        Some(Expression::Assignment(
            Box::new(Expression::Variable(0.into())),
            AssignmentType::Normal,
            Box::new(Expression::Literal(Literal::I32(100)))
        )),
        parse_expr("i = 100", vec!["i"])
    );

    assert_eq!(
        Some(Expression::Assignment(
            Box::new(Expression::Variable(0.into())),
            AssignmentType::BinaryOp(BinaryOperand::Add),
            Box::new(Expression::Literal(Literal::I32(100)))
        )),
        parse_expr("i += 100", vec!["i"])
    );

    assert_eq!(
        Some(Expression::Assignment(
            Box::new(Expression::Variable(0.into())),
            AssignmentType::BinaryOp(BinaryOperand::Sub),
            Box::new(Expression::Literal(Literal::I32(100)))
        )),
        parse_expr("i -= 100", vec!["i"])
    );

    assert_eq!(
        Some(Expression::Assignment(
            Box::new(Expression::Variable(0.into())),
            AssignmentType::BinaryOp(BinaryOperand::Mult),
            Box::new(Expression::Literal(Literal::I32(100)))
        )),
        parse_expr("i *= 100", vec!["i"])
    );

    assert_eq!(
        Some(Expression::Assignment(
            Box::new(Expression::Variable(0.into())),
            AssignmentType::BinaryOp(BinaryOperand::Div),
            Box::new(Expression::Literal(Literal::I32(100)))
        )),
        parse_expr("i /= 100", vec!["i"])
    );

    assert_eq!(
        Some(Expression::Assignment(
            Box::new(Expression::Variable(0.into())),
            AssignmentType::BinaryOp(BinaryOperand::Mod),
            Box::new(Expression::Literal(Literal::I32(100)))
        )),
        parse_expr("i %= 100", vec!["i"])
    );

    assert_eq!(
        Some(Expression::Assignment(
            Box::new(Expression::Variable(0.into())),
            AssignmentType::BinaryOp(BinaryOperand::Pow),
            Box::new(Expression::Literal(Literal::I32(100)))
        )),
        parse_expr("i **= 100", vec!["i"])
    );

    assert_eq!(
        Some(Expression::Assignment(
            Box::new(Expression::Variable(0.into())),
            AssignmentType::BinaryOp(BinaryOperand::BitAnd),
            Box::new(Expression::Literal(Literal::I32(100)))
        )),
        parse_expr("i &= 100", vec!["i"])
    );

    assert_eq!(
        Some(Expression::Assignment(
            Box::new(Expression::Variable(0.into())),
            AssignmentType::BinaryOp(BinaryOperand::BitOr),
            Box::new(Expression::Literal(Literal::I32(100)))
        )),
        parse_expr("i |= 100", vec!["i"])
    );

    assert_eq!(
        Some(Expression::Assignment(
            Box::new(Expression::Variable(0.into())),
            AssignmentType::BinaryOp(BinaryOperand::BitXor),
            Box::new(Expression::Literal(Literal::I32(100)))
        )),
        parse_expr("i ^= 100", vec!["i"])
    );
}

#[test]
fn bin_ops_expr() {
    let ten = Box::new(Expression::Literal(Literal::I32(10)));
    let nine = Box::new(Expression::Literal(Literal::I32(9)));

    assert_eq!(
        Some(Expression::BinaryOp(
            ten.clone(),
            BinaryOperand::Add,
            nine.clone()
        )),
        parse_expr("10 + 9", Vec::new())
    );

    assert_eq!(
        Some(Expression::BinaryOp(
            ten.clone(),
            BinaryOperand::Sub,
            nine.clone()
        )),
        parse_expr("10 - 9", Vec::new())
    );

    assert_eq!(
        Some(Expression::BinaryOp(
            ten.clone(),
            BinaryOperand::Mult,
            nine.clone()
        )),
        parse_expr("10 * 9", Vec::new())
    );

    assert_eq!(
        Some(Expression::BinaryOp(
            ten.clone(),
            BinaryOperand::Div,
            nine.clone()
        )),
        parse_expr("10 / 9", Vec::new())
    );

    assert_eq!(
        Some(Expression::BinaryOp(
            ten.clone(),
            BinaryOperand::Mod,
            nine.clone()
        )),
        parse_expr("10 % 9", Vec::new())
    );

    assert_eq!(
        Some(Expression::BinaryOp(
            ten.clone(),
            BinaryOperand::Pow,
            nine.clone()
        )),
        parse_expr("10 ** 9", Vec::new())
    );

    assert_eq!(
        Some(Expression::BinaryOp(
            ten.clone(),
            BinaryOperand::BitAnd,
            nine.clone()
        )),
        parse_expr("10 & 9", Vec::new())
    );

    assert_eq!(
        Some(Expression::BinaryOp(
            ten.clone(),
            BinaryOperand::BitOr,
            nine.clone()
        )),
        parse_expr("10 | 9", Vec::new())
    );

    assert_eq!(
        Some(Expression::BinaryOp(
            ten.clone(),
            BinaryOperand::BitXor,
            nine.clone()
        )),
        parse_expr("10 ^ 9", Vec::new())
    );

    assert_eq!(
        Some(Expression::BinaryOp(
            ten.clone(),
            BinaryOperand::Shl,
            nine.clone()
        )),
        parse_expr("10 << 9", Vec::new())
    );

    assert_eq!(
        Some(Expression::BinaryOp(
            ten.clone(),
            BinaryOperand::Shr,
            nine.clone()
        )),
        parse_expr("10 >> 9", Vec::new())
    );
}

#[test]
fn prefix_ops_expr() {
    assert_eq!(
        Some(Expression::UnaryExpr(
            UnaryOperand::Positive,
            Box::new(Expression::Literal(Literal::I32(0)))
        )),
        parse_expr("+0", Vec::new())
    );

    assert_eq!(
        Some(Expression::UnaryExpr(
            UnaryOperand::Not,
            Box::new(Expression::Literal(Literal::I32(0)))
        )),
        parse_expr("!0", Vec::new())
    );

    assert_eq!(
        Some(Expression::UnaryExpr(
            UnaryOperand::Negative,
            Box::new(Expression::Literal(Literal::I32(0)))
        )),
        parse_expr("-0", Vec::new())
    );
}

#[test]
fn inline_conditional_expr() {
    assert_eq!(
        Some(Expression::InlineConditional {
            true_arm: Box::new(Expression::Literal(Literal::I32(0))),
            condition: Box::new(Expression::Literal(Literal::Bool(true))),
            false_arm: Box::new(Expression::Literal(Literal::I32(1))),
        }),
        parse_expr("0 if true else 1", Vec::new())
    );
}

#[test]
fn if_stmt() {
    assert_eq!(
        Some(Statement::If {
            condition: Expression::Literal(Literal::Bool(true)),
            body: vec![Statement::Expression(Expression::FunctionCall {
                caller: Box::new(Expression::Variable(0.into())),
                arguments: vec![Expression::Literal(Literal::I32(1))],
            })],
            arm: None,
        }),
        parse_stmt("if true\nprintln(1)\nend", vec!["println"])
    );

    assert_eq!(
        Some(Statement::If {
            condition: Expression::Literal(Literal::Bool(true)),
            body: vec![Statement::Expression(Expression::FunctionCall {
                caller: Box::new(Expression::Variable(0.into())),
                arguments: vec![Expression::Literal(Literal::I32(1))],
            })],
            arm: Some(Box::new(Statement::If {
                condition: Expression::Literal(Literal::Bool(true)),
                body: vec![Statement::Expression(Expression::FunctionCall {
                    caller: Box::new(Expression::Variable(0.into())),
                    arguments: vec![Expression::Literal(Literal::I32(2))],
                })],
                arm: None,
            })),
        }),
        parse_stmt(
            "if true\nprintln(1)\nelse\nprintln(2)\nend",
            vec!["println"]
        )
    );

    assert_eq!(
        Some(Statement::If {
            condition: Expression::Literal(Literal::Bool(true)),
            body: vec![Statement::Expression(Expression::FunctionCall {
                caller: Box::new(Expression::Variable(0.into())),
                arguments: vec![Expression::Literal(Literal::I32(1))],
            })],
            arm: Some(Box::new(Statement::If {
                condition: Expression::Comparison(
                    Box::new(Expression::Literal(Literal::I32(1))),
                    ComparisonOperand::Greater,
                    Box::new(Expression::Literal(Literal::I32(5))),
                ),
                body: vec![Statement::Expression(Expression::FunctionCall {
                    caller: Box::new(Expression::Variable(0.into())),
                    arguments: vec![Expression::Literal(Literal::I32(2))],
                })],
                arm: None,
            })),
        }),
        parse_stmt(
            "if true\nprintln(1)\nelse if 1 > 5\nprintln(2)\nend",
            vec!["println"]
        )
    );

    assert_eq!(
        Some(Statement::If {
            condition: Expression::Literal(Literal::Bool(true)),
            body: vec![Statement::Expression(Expression::FunctionCall {
                caller: Box::new(Expression::Variable(0.into())),
                arguments: vec![Expression::Literal(Literal::I32(1))],
            })],
            arm: Some(Box::new(Statement::If {
                condition: Expression::Comparison(
                    Box::new(Expression::Literal(Literal::I32(1))),
                    ComparisonOperand::Greater,
                    Box::new(Expression::Literal(Literal::I32(5))),
                ),
                body: vec![Statement::Expression(Expression::FunctionCall {
                    caller: Box::new(Expression::Variable(0.into())),
                    arguments: vec![Expression::Literal(Literal::I32(2))],
                })],
                arm: Some(Box::new(Statement::If {
                    condition: Expression::Literal(Literal::Bool(true)),
                    body: vec![Statement::Expression(Expression::FunctionCall {
                        caller: Box::new(Expression::Variable(0.into())),
                        arguments: vec![Expression::Literal(Literal::I32(3))],
                    })],
                    arm: None,
                }))
            })),
        }),
        parse_stmt(
            "if true\nprintln(1)\nelse if 1 > 5\nprintln(2)\nelse\nprintln(3)\nend",
            vec!["println"]
        )
    );
}

#[test]
fn function_call_stmt() {
    assert_eq!(
        Some(Statement::Expression(Expression::FunctionCall {
            caller: Box::new(Expression::Variable(0.into())),
            arguments: Vec::new(),
        })),
        parse_stmt("test_func()\n", vec!["test_func"])
    );
}

#[test]
fn decl_var() {
    assert_eq!(
        Some(Statement::VarDeclaration(
            0.into(),
            Expression::Literal(Literal::Bool(true))
        )),
        parse_stmt("let variable = true\n", vec!["variable"])
    );
}

#[test]
fn return_stmt() {
    assert_eq!(
        Some(Statement::Return(None)),
        parse_stmt("return\n", Vec::new())
    );

    assert_eq!(
        Some(Statement::Return(Some(Expression::BinaryOp(
            Box::new(Expression::Literal(Literal::I32(10))),
            BinaryOperand::Add,
            Box::new(Expression::Literal(Literal::I32(9)))
        )))),
        parse_stmt("return 10 + 9\n", Vec::new())
    );
}

#[test]
fn break_stmt() {
    assert_eq!(
        Some(Statement::Break(None)),
        parse_stmt("break\n", Vec::new())
    );

    assert_eq!(
        Some(Statement::Break(Some(Expression::BinaryOp(
            Box::new(Expression::Literal(Literal::I32(10))),
            BinaryOperand::Add,
            Box::new(Expression::Literal(Literal::I32(9)))
        )))),
        parse_stmt("break 10 + 9\n", Vec::new())
    );
}

#[test]
fn continue_stmt() {
    assert_eq!(
        Some(Statement::Continue),
        parse_stmt("continue\n", Vec::new())
    );
}

#[test]
fn while_stmt() {
    assert_eq!(
        Some(Statement::While {
            condition: Expression::Literal(Literal::Bool(true)),
            body: vec![Statement::Expression(Expression::FunctionCall {
                caller: Box::new(Expression::Variable(0.into())),
                arguments: vec![Expression::Literal(Literal::Bool(true))],
            })],
            then: None,
        }),
        parse_stmt("while true\nprintln(true)\nend", vec!["println"])
    );

    assert_eq!(
        Some(Statement::While {
            condition: Expression::Literal(Literal::Bool(true)),
            body: vec![Statement::Expression(Expression::FunctionCall {
                caller: Box::new(Expression::Variable(0.into())),
                arguments: vec![Expression::Literal(Literal::Bool(true))],
            })],
            then: Some(vec![Statement::Expression(Expression::FunctionCall {
                caller: Box::new(Expression::Variable(0.into())),
                arguments: vec![Expression::Literal(Literal::Bool(true))],
            })]),
        }),
        parse_stmt(
            "while true\nprintln(true)\nthen\nprintln(true)\nend",
            vec!["println"]
        )
    );
}

#[test]
fn loop_stmt() {
    assert_eq!(
        Some(Statement::Loop {
            body: vec![Statement::Expression(Expression::FunctionCall {
                caller: Box::new(Expression::Variable(0.into())),
                arguments: vec![Expression::Literal(Literal::Bool(true))],
            })],
            then: None,
        }),
        parse_stmt("loop\nprintln(true)\nend", vec!["println"])
    );

    assert_eq!(
        Some(Statement::Loop {
            body: vec![Statement::Expression(Expression::FunctionCall {
                caller: Box::new(Expression::Variable(0.into())),
                arguments: vec![Expression::Literal(Literal::Bool(true))],
            })],
            then: Some(vec![Statement::Expression(Expression::FunctionCall {
                caller: Box::new(Expression::Variable(0.into())),
                arguments: vec![Expression::Literal(Literal::Bool(true))],
            })]),
        }),
        parse_stmt(
            "loop\nprintln(true)\nthen\nprintln(true)\nend",
            vec!["println"]
        )
    );
}

#[test]
fn for_stmt() {
    assert_eq!(
        Some(Statement::For {
            var: Expression::Variable(0.into()),
            condition: Expression::Literal(Literal::I32(100)),
            body: vec![Statement::Expression(Expression::FunctionCall {
                caller: Box::new(Expression::Variable(1.into())),
                arguments: vec![Expression::Literal(Literal::Bool(true))],
            })],
            then: None,
        }),
        parse_stmt("for i in 100\nprintln(true)\nend", vec!["i", "println"])
    );

    assert_eq!(
        Some(Statement::For {
            var: Expression::Variable(0.into()),
            condition: Expression::Literal(Literal::I32(100)),
            body: vec![Statement::Expression(Expression::FunctionCall {
                caller: Box::new(Expression::Variable(1.into())),
                arguments: vec![Expression::Literal(Literal::Bool(true))],
            })],
            then: Some(vec![Statement::Expression(Expression::FunctionCall {
                caller: Box::new(Expression::Variable(1.into())),
                arguments: vec![Expression::Literal(Literal::Bool(true))],
            })]),
        }),
        parse_stmt(
            "for i in 100\nprintln(true)\nthen\nprintln(true)\nend",
            vec!["i", "println"]
        )
    );
}

#[test]
fn match_stmt() {
    simple_logger::init().unwrap();

    assert_eq!(
        Some(Statement::Match {
            var: Expression::Variable(0.into()),
            arms: vec![
                (
                    1.into(),
                    Some(Expression::Comparison(
                        Box::new(Expression::Variable(1.into())),
                        ComparisonOperand::Less,
                        Box::new(Expression::Literal(Literal::I32(6)))
                    )),
                    vec![Statement::Expression(Expression::FunctionCall {
                        caller: Box::new(Expression::Variable(2.into())),
                        arguments: vec![Expression::Literal(Literal::I32(6))]
                    })],
                ),
                (
                    1.into(),
                    None,
                    vec![Statement::Expression(Expression::FunctionCall {
                        caller: Box::new(Expression::Variable(2.into())),
                        arguments: vec![Expression::Variable(1.into())]
                    })],
                )
            ]
        }),
        parse_stmt(
            "match something\ni where i < 6 =>\nprintln(6)\nend\ni =>\nprintln(i)\nend\nend",
            vec!["something", "i", "println"]
        )
    );
}
