/*
TODO: Lord have mercy, this is all broken

use super::*;
use crate::files::FileId;

use core::convert::TryFrom;
use parking_lot::RwLock;
use stadium::Stadium;
use string_interner::StringInterner;

use alloc::{boxed::Box, sync::Arc, vec, vec::Vec};
use core::num::NonZeroUsize;

fn emit_diagnostics<'a>(
    source: &'a str,
    diagnostics: Vec<codespan_reporting::diagnostic::Diagnostic<FileId>>,
) {
    use crate::files::Files;

    let mut files = Files::new();
    files.add("<test file>", source);

    let writer = codespan_reporting::term::termcolor::StandardStream::stderr(
        codespan_reporting::term::termcolor::ColorChoice::Auto,
    );
    let config = codespan_reporting::term::Config::default();

    for diag in diagnostics {
        codespan_reporting::term::emit(&mut writer.lock(), &config, &files, &diag).unwrap();
    }
}

// TODO: Make this a result when Diagnostics are Debug
fn parse_expr<'src, 'expr>(source: &'src str, idents: Vec<&str>) -> ParseResult<Expr<'expr>> {
    let interner = Arc::new(RwLock::new(StringInterner::new()));
    for i in idents {
        interner.write().get_or_intern(i);
    }

    Parser::new(source, FileId::new(0), interner).expr()
}

fn parse_stmt<'src, 'expr, 'stmt>(
    source: &'src str,
    idents: Vec<&str>,
) -> ParseResult<Stmt<'expr, 'stmt>> {
    let interner = Arc::new(RwLock::new(StringInterner::new()));
    for i in idents {
        interner.write().get_or_intern(i);
    }

    Parser::new(source, FileId::new(0), interner)
        .stmt()
        .map(|s| s.unwrap())
}

fn parse_ast<'src, 'expr, 'stmt>(
    source: &'src str,
    idents: Vec<&str>,
) -> ParseResult<Ast<'expr, 'stmt>> {
    let interner = Arc::new(RwLock::new(StringInterner::new()));
    for i in idents {
        interner.write().get_or_intern(i);
    }

    Parser::new(source, FileId::new(0), interner)
        .ast()
        .map(|s| s.unwrap())
}

#[test]
fn expr() {
    let exprs = Stadium::with_capacity(NonZeroUsize::new(20).unwrap());

    assert_eq!(
        Ok(exprs.alloc(Expression::UnaryExpr(
            UnaryOperand::Not,
            exprs.alloc(Expression::BinaryOp(
                exprs.alloc(Expression::Literal(Literal::I32(5))),
                BinaryOperand::Add,
                exprs.alloc(Expression::BinaryOp(
                    exprs.alloc(Expression::Literal(Literal::I32(-10))),
                    BinaryOperand::Div,
                    exprs.alloc(Expression::BinaryOp(
                        exprs.alloc(Expression::Parenthesised(exprs.alloc(
                            Expression::BinaryOp(
                                exprs.alloc(Expression::FunctionCall {
                                    caller: exprs.alloc(Expression::Variable(0.into())),
                                    arguments: vec![
                                        exprs.alloc(Expression::Literal(Literal::I32(54))),
                                        exprs.alloc(Expression::Variable(1.into())),
                                    ]
                                }),
                                BinaryOperand::Mod,
                                exprs.alloc(Expression::InlineConditional {
                                    true_arm: exprs.alloc(Expression::Literal(Literal::I32(200))),
                                    condition: exprs.alloc(Expression::Literal(Literal::I32(10))),
                                    false_arm: exprs.alloc(Expression::Literal(Literal::I32(52))),
                                })
                            )
                        ))),
                        BinaryOperand::Sub,
                        exprs.alloc(Expression::Parenthesised(
                            exprs.alloc(Expression::Literal(Literal::Bool(true)))
                        ))
                    ))
                ))
            )),
        ))),
        parse_expr(
            "!5 + -10 / (test(54, test_again) % 200 if 10 else 52) - (true)",
            vec!["test", "test_again"]
        )
    );
}

#[test]
fn parse_signed_ints() {
    let exprs = Stadium::with_capacity(NonZeroUsize::new(20).unwrap());

    assert_eq!(
        Ok(exprs.alloc(Expression::Literal(Literal::I32(10)))),
        parse_expr("10", Vec::new())
    );

    assert_eq!(
        Ok(exprs.alloc(Expression::Literal(Literal::I32(-10)))),
        parse_expr("-10", Vec::new())
    );

    assert_eq!(
        Ok(exprs.alloc(Expression::Literal(Literal::I32(10)))),
        parse_expr("+10", Vec::new())
    );
}

#[test]
fn single_paren_expr() {
    let exprs = Stadium::with_capacity(NonZeroUsize::new(5).unwrap());

    assert_eq!(
        Ok(exprs.alloc(Expression::Comparison(
            exprs.alloc(Expression::Literal(Literal::I32(10))),
            ComparisonOperand::Equal,
            exprs.alloc(Expression::Parenthesised(
                exprs.alloc(Expression::Literal(Literal::I32(0)))
            ))
        ))),
        parse_expr("10 == (0)", Vec::new())
    );
}

#[test]
fn index_array_expr() {
    let exprs = Stadium::with_capacity(NonZeroUsize::new(5).unwrap());

    assert_eq!(
        Ok(exprs.alloc(Expression::IndexArray {
            array: exprs.alloc(Expression::Variable(0.into())),
            index: exprs.alloc(Expression::Literal(Literal::I32(0))),
        })),
        parse_expr("array[0]", vec!["array"])
    );
}

#[test]
fn array_literal_expr() {
    let exprs = Stadium::with_capacity(NonZeroUsize::new(5).unwrap());

    assert_eq!(
        Ok(exprs.alloc(Expression::Array(vec![
            exprs.alloc(Expression::Literal(Literal::Bool(true))),
            exprs.alloc(Expression::Variable(0.into())),
            exprs.alloc(Expression::BinaryOp(
                exprs.alloc(Expression::Literal(Literal::I32(100))),
                BinaryOperand::Div,
                exprs.alloc(Expression::Literal(Literal::I32(5)))
            ))
        ]))),
        parse_expr("[true, test, 100 / 5]", vec!["test"]),
    );
}

#[test]
fn comparisons_expr() {
    let exprs = Stadium::with_capacity(NonZeroUsize::new(10).unwrap());

    let ten = exprs.alloc(Expression::Literal(Literal::I32(10)));
    let nine = exprs.alloc(Expression::Literal(Literal::I32(9)));

    assert_eq!(
        Ok(exprs.alloc(Expression::Comparison(
            ten.clone(),
            ComparisonOperand::Greater,
            nine.clone()
        ))),
        parse_expr("10 > 9", Vec::new())
    );

    assert_eq!(
        Ok(exprs.alloc(Expression::Comparison(
            ten.clone(),
            ComparisonOperand::Less,
            nine.clone()
        ))),
        parse_expr("10 < 9", Vec::new())
    );

    assert_eq!(
        Ok(exprs.alloc(Expression::Comparison(
            ten.clone(),
            ComparisonOperand::GreaterEqual,
            nine.clone()
        ))),
        parse_expr("10 >= 9", Vec::new())
    );

    assert_eq!(
        Ok(exprs.alloc(Expression::Comparison(
            ten.clone(),
            ComparisonOperand::LessEqual,
            nine.clone()
        ))),
        parse_expr("10 <= 9", Vec::new())
    );

    assert_eq!(
        Ok(exprs.alloc(Expression::Comparison(
            ten.clone(),
            ComparisonOperand::Equal,
            nine.clone()
        ))),
        parse_expr("10 == 9", Vec::new())
    );

    assert_eq!(
        Ok(exprs.alloc(Expression::Comparison(
            ten.clone(),
            ComparisonOperand::NotEqual,
            nine.clone()
        ))),
        parse_expr("10 != 9", Vec::new())
    );
}

#[test]
fn literals_expr() {
    let exprs = Stadium::with_capacity(NonZeroUsize::new(5).unwrap());

    assert_eq!(
        Ok(exprs.alloc(Expression::Literal(Literal::I32(100)))),
        parse_expr("100", Vec::new())
    );

    assert_eq!(
        Ok(exprs.alloc(Expression::Literal(Literal::Bool(false)))),
        parse_expr("false", Vec::new())
    );

    assert_eq!(
        Ok(exprs.alloc(Expression::Literal(Literal::Bool(true)))),
        parse_expr("true", Vec::new())
    );
}

#[test]
fn func_call_expr() {
    let exprs = Stadium::with_capacity(NonZeroUsize::new(5).unwrap());

    assert_eq!(
        Ok(exprs.alloc(Expression::FunctionCall {
            caller: exprs.alloc(Expression::Variable(0.into())),
            arguments: vec![
                exprs.alloc(Expression::Variable(1.into())),
                exprs.alloc(Expression::Literal(Literal::I32(0))),
            ],
        })),
        parse_expr("test(test2, 0)", vec!["test", "test2"])
    );

    assert_eq!(
        Ok(exprs.alloc(Expression::FunctionCall {
            caller: exprs.alloc(Expression::Variable(0.into())),
            arguments: Vec::new(),
        })),
        parse_expr("test_func()", vec!["test_func"])
    );
}

#[test]
fn dotted_func_call_expr() {
    let exprs = Stadium::with_capacity(NonZeroUsize::new(5).unwrap());

    assert_eq!(
        Ok(exprs.alloc(Expression::MemberFunctionCall {
            member: exprs.alloc(Expression::Variable(0.into())),
            function: exprs.alloc(Expression::FunctionCall {
                caller: exprs.alloc(Expression::Variable(1.into())),
                arguments: Vec::new(),
            }),
        })),
        parse_expr("test_func.function()", vec!["test_func", "function"])
    );
}

#[test]
fn assignment_expr() {
    let exprs = Stadium::with_capacity(NonZeroUsize::new(20).unwrap());

    assert_eq!(
        Ok(exprs.alloc(Expression::Assignment(
            exprs.alloc(Expression::Variable(0.into())),
            AssignmentType::Normal,
            exprs.alloc(Expression::Literal(Literal::I32(100)))
        ))),
        parse_expr("i = 100", vec!["i"])
    );

    assert_eq!(
        Ok(exprs.alloc(Expression::Assignment(
            exprs.alloc(Expression::Variable(0.into())),
            AssignmentType::BinaryOp(BinaryOperand::Add),
            exprs.alloc(Expression::Literal(Literal::I32(100)))
        ))),
        parse_expr("i += 100", vec!["i"])
    );

    assert_eq!(
        Ok(exprs.alloc(Expression::Assignment(
            exprs.alloc(Expression::Variable(0.into())),
            AssignmentType::BinaryOp(BinaryOperand::Sub),
            exprs.alloc(Expression::Literal(Literal::I32(100)))
        ))),
        parse_expr("i -= 100", vec!["i"])
    );

    assert_eq!(
        Ok(exprs.alloc(Expression::Assignment(
            exprs.alloc(Expression::Variable(0.into())),
            AssignmentType::BinaryOp(BinaryOperand::Mult),
            exprs.alloc(Expression::Literal(Literal::I32(100)))
        ))),
        parse_expr("i *= 100", vec!["i"])
    );

    assert_eq!(
        Ok(exprs.alloc(Expression::Assignment(
            exprs.alloc(Expression::Variable(0.into())),
            AssignmentType::BinaryOp(BinaryOperand::Div),
            exprs.alloc(Expression::Literal(Literal::I32(100)))
        ))),
        parse_expr("i /= 100", vec!["i"])
    );

    assert_eq!(
        Ok(exprs.alloc(Expression::Assignment(
            exprs.alloc(Expression::Variable(0.into())),
            AssignmentType::BinaryOp(BinaryOperand::Mod),
            exprs.alloc(Expression::Literal(Literal::I32(100)))
        ))),
        parse_expr("i %= 100", vec!["i"])
    );

    assert_eq!(
        Ok(exprs.alloc(Expression::Assignment(
            exprs.alloc(Expression::Variable(0.into())),
            AssignmentType::BinaryOp(BinaryOperand::Pow),
            exprs.alloc(Expression::Literal(Literal::I32(100)))
        ))),
        parse_expr("i **= 100", vec!["i"])
    );

    assert_eq!(
        Ok(exprs.alloc(Expression::Assignment(
            exprs.alloc(Expression::Variable(0.into())),
            AssignmentType::BinaryOp(BinaryOperand::BitAnd),
            exprs.alloc(Expression::Literal(Literal::I32(100)))
        ))),
        parse_expr("i &= 100", vec!["i"])
    );

    assert_eq!(
        Ok(exprs.alloc(Expression::Assignment(
            exprs.alloc(Expression::Variable(0.into())),
            AssignmentType::BinaryOp(BinaryOperand::BitOr),
            exprs.alloc(Expression::Literal(Literal::I32(100)))
        ))),
        parse_expr("i |= 100", vec!["i"])
    );

    assert_eq!(
        Ok(exprs.alloc(Expression::Assignment(
            exprs.alloc(Expression::Variable(0.into())),
            AssignmentType::BinaryOp(BinaryOperand::BitXor),
            exprs.alloc(Expression::Literal(Literal::I32(100)))
        ))),
        parse_expr("i ^= 100", vec!["i"])
    );
}

#[test]
fn range_expr() {
    let exprs = Stadium::with_capacity(NonZeroUsize::new(20).unwrap());

    assert_eq!(
        Ok(exprs.alloc(Expression::Range(
            exprs.alloc(Expression::Literal(Literal::I32(1))),
            exprs.alloc(Expression::Literal(Literal::I32(100))),
        ))),
        parse_expr("1..100", Vec::new())
    );
}

#[test]
fn bin_ops_expr() {
    let exprs = Stadium::with_capacity(NonZeroUsize::new(20).unwrap());

    let ten = exprs.alloc(Expression::Literal(Literal::I32(10)));
    let nine = exprs.alloc(Expression::Literal(Literal::I32(9)));

    assert_eq!(
        Ok(exprs.alloc(Expression::BinaryOp(
            ten.clone(),
            BinaryOperand::Add,
            nine.clone()
        ))),
        parse_expr("10 + 9", Vec::new())
    );

    assert_eq!(
        Ok(exprs.alloc(Expression::BinaryOp(
            ten.clone(),
            BinaryOperand::Sub,
            nine.clone()
        ))),
        parse_expr("10 - 9", Vec::new())
    );

    assert_eq!(
        Ok(exprs.alloc(Expression::BinaryOp(
            ten.clone(),
            BinaryOperand::Mult,
            nine.clone()
        ))),
        parse_expr("10 * 9", Vec::new())
    );

    assert_eq!(
        Ok(exprs.alloc(Expression::BinaryOp(
            ten.clone(),
            BinaryOperand::Div,
            nine.clone()
        ))),
        parse_expr("10 / 9", Vec::new())
    );

    assert_eq!(
        Ok(exprs.alloc(Expression::BinaryOp(
            ten.clone(),
            BinaryOperand::Mod,
            nine.clone()
        ))),
        parse_expr("10 % 9", Vec::new())
    );

    assert_eq!(
        Ok(exprs.alloc(Expression::BinaryOp(
            ten.clone(),
            BinaryOperand::Pow,
            nine.clone()
        ))),
        parse_expr("10 ** 9", Vec::new())
    );

    assert_eq!(
        Ok(exprs.alloc(Expression::BinaryOp(
            ten.clone(),
            BinaryOperand::BitAnd,
            nine.clone()
        ))),
        parse_expr("10 & 9", Vec::new())
    );

    assert_eq!(
        Ok(exprs.alloc(Expression::BinaryOp(
            ten.clone(),
            BinaryOperand::BitOr,
            nine.clone()
        ))),
        parse_expr("10 | 9", Vec::new())
    );

    assert_eq!(
        Ok(exprs.alloc(Expression::BinaryOp(
            ten.clone(),
            BinaryOperand::BitXor,
            nine.clone()
        ))),
        parse_expr("10 ^ 9", Vec::new())
    );

    assert_eq!(
        Ok(exprs.alloc(Expression::BinaryOp(
            ten.clone(),
            BinaryOperand::Shl,
            nine.clone()
        ))),
        parse_expr("10 << 9", Vec::new())
    );

    assert_eq!(
        Ok(exprs.alloc(Expression::BinaryOp(
            ten.clone(),
            BinaryOperand::Shr,
            nine.clone()
        ))),
        parse_expr("10 >> 9", Vec::new())
    );
}

#[test]
fn prefix_ops_expr() {
    let exprs = Stadium::with_capacity(NonZeroUsize::new(20).unwrap());

    assert_eq!(
        Ok(exprs.alloc(Expression::UnaryExpr(
            UnaryOperand::Positive,
            exprs.alloc(Expression::Variable(0.into()))
        ))),
        parse_expr("+var", vec!["var"])
    );

    assert_eq!(
        Ok(exprs.alloc(Expression::UnaryExpr(
            UnaryOperand::Not,
            exprs.alloc(Expression::Literal(Literal::I32(0)))
        ))),
        parse_expr("!0", Vec::new())
    );

    assert_eq!(
        Ok(exprs.alloc(Expression::UnaryExpr(
            UnaryOperand::Negative,
            exprs.alloc(Expression::Variable(0.into()))
        ))),
        parse_expr("-var", vec!["var"])
    );
}

#[test]
fn inline_conditional_expr() {
    let exprs = Stadium::with_capacity(NonZeroUsize::new(20).unwrap());

    assert_eq!(
        Ok(exprs.alloc(Expression::InlineConditional {
            true_arm: exprs.alloc(Expression::Literal(Literal::I32(0))),
            condition: exprs.alloc(Expression::Literal(Literal::Bool(true))),
            false_arm: exprs.alloc(Expression::Literal(Literal::I32(1))),
        })),
        parse_expr("0 if true else 1", Vec::new())
    );
}

#[test]
fn if_stmt() {
    let stmts = Stadium::with_capacity(NonZeroUsize::new(20).unwrap());
    let exprs = Stadium::with_capacity(NonZeroUsize::new(20).unwrap());

    assert_eq!(
        Ok(stmts.alloc(Statement::If {
            condition: exprs.alloc(Expression::Literal(Literal::Bool(true))),
            body: vec![stmts.alloc(Statement::Expression(exprs.alloc(
                Expression::FunctionCall {
                    caller: exprs.alloc(Expression::Variable(0.into())),
                    arguments: vec![exprs.alloc(Expression::Literal(Literal::I32(1)))],
                }
            )))],
            arm: None,
        })),
        parse_stmt("if true\nprintln(1)\nend", vec!["println"])
    );

    assert_eq!(
        Ok(stmts.alloc(Statement::If {
            condition: exprs.alloc(Expression::Literal(Literal::Bool(true))),
            body: vec![stmts.alloc(Statement::Expression(exprs.alloc(
                Expression::FunctionCall {
                    caller: exprs.alloc(Expression::Variable(0.into())),
                    arguments: vec![exprs.alloc(Expression::Literal(Literal::I32(1)))],
                }
            )))],
            arm: Some(stmts.alloc(Statement::If {
                condition: exprs.alloc(Expression::Literal(Literal::Bool(true))),
                body: vec![stmts.alloc(Statement::Expression(exprs.alloc(
                    Expression::FunctionCall {
                        caller: exprs.alloc(Expression::Variable(0.into())),
                        arguments: vec![exprs.alloc(Expression::Literal(Literal::I32(2)))],
                    }
                )))],
                arm: None,
            })),
        })),
        parse_stmt(
            "if true\nprintln(1)\nelse\nprintln(2)\nend",
            vec!["println"]
        )
    );

    assert_eq!(
        Ok(stmts.alloc(Statement::If {
            condition: exprs.alloc(Expression::Literal(Literal::Bool(true))),
            body: vec![stmts.alloc(Statement::Expression(exprs.alloc(
                Expression::FunctionCall {
                    caller: exprs.alloc(Expression::Variable(0.into())),
                    arguments: vec![exprs.alloc(Expression::Literal(Literal::I32(1)))],
                }
            )))],
            arm: Some(stmts.alloc(Statement::If {
                condition: exprs.alloc(Expression::Comparison(
                    exprs.alloc(Expression::Literal(Literal::I32(1))),
                    ComparisonOperand::Greater,
                    exprs.alloc(Expression::Literal(Literal::I32(5))),
                )),
                body: vec![stmts.alloc(Statement::Expression(exprs.alloc(
                    Expression::FunctionCall {
                        caller: exprs.alloc(Expression::Variable(0.into())),
                        arguments: vec![exprs.alloc(Expression::Literal(Literal::I32(2)))],
                    }
                )))],
                arm: None,
            })),
        })),
        parse_stmt(
            "if true\nprintln(1)\nelse if 1 > 5\nprintln(2)\nend",
            vec!["println"]
        )
    );

    assert_eq!(
        Ok(stmts.alloc(Statement::If {
            condition: exprs.alloc(Expression::Literal(Literal::Bool(true))),
            body: vec![stmts.alloc(Statement::Expression(exprs.alloc(
                Expression::FunctionCall {
                    caller: exprs.alloc(Expression::Variable(0.into())),
                    arguments: vec![exprs.alloc(Expression::Literal(Literal::I32(1)))],
                }
            )))],
            arm: Some(stmts.alloc(Statement::If {
                condition: exprs.alloc(Expression::Comparison(
                    exprs.alloc(Expression::Literal(Literal::I32(1))),
                    ComparisonOperand::Greater,
                    exprs.alloc(Expression::Literal(Literal::I32(5))),
                )),
                body: vec![stmts.alloc(Statement::Expression(exprs.alloc(
                    Expression::FunctionCall {
                        caller: exprs.alloc(Expression::Variable(0.into())),
                        arguments: vec![exprs.alloc(Expression::Literal(Literal::I32(2)))],
                    }
                )))],
                arm: Some(stmts.alloc(Statement::If {
                    condition: exprs.alloc(Expression::Literal(Literal::Bool(true))),
                    body: vec![stmts.alloc(Statement::Expression(exprs.alloc(
                        Expression::FunctionCall {
                            caller: exprs.alloc(Expression::Variable(0.into())),
                            arguments: vec![exprs.alloc(Expression::Literal(Literal::I32(3)))],
                        }
                    )))],
                    arm: None,
                })),
            })),
        })),
        parse_stmt(
            "if true\nprintln(1)\nelse if 1 > 5\nprintln(2)\nelse\nprintln(3)\nend",
            vec!["println"]
        )
    );
}

#[test]
fn nested_if_stmt() {
    let stmts = Stadium::with_capacity(NonZeroUsize::new(20).unwrap());
    let exprs = Stadium::with_capacity(NonZeroUsize::new(20).unwrap());

    assert_eq!(
        Ok(stmts.alloc(Statement::If {
            condition: exprs.alloc(Expression::Literal(Literal::Bool(true))),
            body: vec![stmts.alloc(Statement::If {
                condition: exprs.alloc(Expression::Literal(Literal::Bool(false))),
                body: vec![stmts.alloc(Statement::Expression(exprs.alloc(
                    Expression::FunctionCall {
                        caller: exprs.alloc(Expression::Variable(0.into())),
                        arguments: vec![exprs.alloc(Expression::Literal(Literal::I32(1)))],
                    }
                )))],
                arm: None,
            })],
            arm: None,
        })),
        parse_stmt("if true\nif false\nprintln(1)\nend\nend", vec!["println"])
    );
}

#[test]
fn function_call_stmt() {
    let stmts = Stadium::with_capacity(NonZeroUsize::new(20).unwrap());
    let exprs = Stadium::with_capacity(NonZeroUsize::new(20).unwrap());

    assert_eq!(
        Ok(stmts.alloc(Statement::Expression(exprs.alloc(
            Expression::FunctionCall {
                caller: exprs.alloc(Expression::Variable(0.into())),
                arguments: Vec::new(),
            }
        )))),
        parse_stmt("test_func()\n", vec!["test_func"])
    );
}

#[test]
fn decl_var() {
    let stmts = Stadium::with_capacity(NonZeroUsize::new(20).unwrap());
    let exprs = Stadium::with_capacity(NonZeroUsize::new(20).unwrap());

    assert_eq!(
        Ok(stmts.alloc(Statement::VarDeclaration(
            0.into(),
            exprs.alloc(Expression::Literal(Literal::Bool(true))),
        ))),
        parse_stmt("let variable = true\n", vec!["variable"])
    );
}

#[test]
fn long_func_decl_stmt() {
    let stmts = Stadium::with_capacity(NonZeroUsize::new(20).unwrap());
    let exprs = Stadium::with_capacity(NonZeroUsize::new(20).unwrap());

    let assign = "let jkpwn = vssekgmbxoxshmhinx( jnldfzbd , kcqpq , gbuaqbax , argro , xhmfc , bredcp , pwlfywfkb , vgsjjcy , exomcmbf , cjsjpvgcl , omtlfpw , ssdrm , kxrtaun , xexzz , ejvmxj , ssmqkbqqi , )\n";
    let idents = vec![
        "jkpwn",
        "vssekgmbxoxshmhinx",
        "jnldfzbd",
        "kcqpq",
        "gbuaqbax",
        "argro",
        "xhmfc",
        "bredcp",
        "pwlfywfkb",
        "vgsjjcy",
        "exomcmbf",
        "cjsjpvgcl",
        "omtlfpw",
        "ssdrm",
        "kxrtaun",
        "xexzz",
        "ejvmxj",
        "ssmqkbqqi",
    ];

    assert_eq!(
        Ok(stmts.alloc(Statement::VarDeclaration(
            0.into(),
            exprs.alloc(Expression::FunctionCall {
                caller: exprs.alloc(Expression::Variable(1.into())),
                arguments: vec![
                    exprs.alloc(Expression::Variable(2.into())),
                    exprs.alloc(Expression::Variable(3.into())),
                    exprs.alloc(Expression::Variable(4.into())),
                    exprs.alloc(Expression::Variable(5.into())),
                    exprs.alloc(Expression::Variable(6.into())),
                    exprs.alloc(Expression::Variable(7.into())),
                    exprs.alloc(Expression::Variable(8.into())),
                    exprs.alloc(Expression::Variable(9.into())),
                    exprs.alloc(Expression::Variable(10.into())),
                    exprs.alloc(Expression::Variable(11.into())),
                    exprs.alloc(Expression::Variable(12.into())),
                    exprs.alloc(Expression::Variable(13.into())),
                    exprs.alloc(Expression::Variable(14.into())),
                    exprs.alloc(Expression::Variable(15.into())),
                    exprs.alloc(Expression::Variable(16.into())),
                ],
            }),
        ))),
        parse_stmt(assign, idents)
    );
}

#[test]
fn return_stmt() {
    let stmts = Stadium::with_capacity(NonZeroUsize::new(20).unwrap());
    let exprs = Stadium::with_capacity(NonZeroUsize::new(20).unwrap());

    assert_eq!(
        Ok(stmts.alloc(Statement::Return(None))),
        parse_stmt("return\n", Vec::new())
    );

    assert_eq!(
        Ok(
            stmts.alloc(Statement::Return(Some(exprs.alloc(Expression::BinaryOp(
                exprs.alloc(Expression::Literal(Literal::I32(10))),
                BinaryOperand::Add,
                exprs.alloc(Expression::Literal(Literal::I32(9)))
            )))))
        ),
        parse_stmt("return 10 + 9\n", Vec::new())
    );
}

#[test]
fn break_stmt() {
    let stmts = Stadium::with_capacity(NonZeroUsize::new(5).unwrap());
    let exprs = Stadium::with_capacity(NonZeroUsize::new(5).unwrap());

    assert_eq!(
        Ok(stmts.alloc(Statement::Break(None))),
        parse_stmt("break\n", Vec::new())
    );

    assert_eq!(
        Ok(
            stmts.alloc(Statement::Break(Some(exprs.alloc(Expression::BinaryOp(
                exprs.alloc(Expression::Literal(Literal::I32(10))),
                BinaryOperand::Add,
                exprs.alloc(Expression::Literal(Literal::I32(9)))
            )))))
        ),
        parse_stmt("break 10 + 9\n", Vec::new())
    );
}

#[test]
fn continue_stmt() {
    let stmts = Stadium::with_capacity(NonZeroUsize::new(1).unwrap());

    assert_eq!(
        Ok(stmts.alloc(Statement::Continue)),
        parse_stmt("continue\n", Vec::new())
    );
}

#[test]
fn while_stmt() {
    let stmts = Stadium::with_capacity(NonZeroUsize::new(5).unwrap());
    let exprs = Stadium::with_capacity(NonZeroUsize::new(5).unwrap());

    assert_eq!(
        Ok(stmts.alloc(Statement::While {
            condition: exprs.alloc(Expression::Literal(Literal::Bool(true))),
            body: vec![stmts.alloc(Statement::Expression(exprs.alloc(
                Expression::FunctionCall {
                    caller: exprs.alloc(Expression::Variable(0.into())),
                    arguments: vec![exprs.alloc(Expression::Literal(Literal::Bool(true)))],
                }
            )))],
            then: None,
        })),
        parse_stmt("while true\nprintln(true)\nend", vec!["println"])
    );

    assert_eq!(
        Ok(stmts.alloc(Statement::While {
            condition: exprs.alloc(Expression::Literal(Literal::Bool(true))),
            body: vec![stmts.alloc(Statement::Expression(exprs.alloc(
                Expression::FunctionCall {
                    caller: exprs.alloc(Expression::Variable(0.into())),
                    arguments: vec![exprs.alloc(Expression::Literal(Literal::Bool(true)))],
                }
            )))],
            then: Some(vec![stmts.alloc(Statement::Expression(exprs.alloc(
                Expression::FunctionCall {
                    caller: exprs.alloc(Expression::Variable(0.into())),
                    arguments: vec![exprs.alloc(Expression::Literal(Literal::Bool(true)))],
                }
            )))]),
        })),
        parse_stmt(
            "while true\nprintln(true)\nthen\nprintln(true)\nend",
            vec!["println"]
        )
    );
}

#[test]
fn loop_stmt() {
    assert_eq!(
        Ok(Statement::Loop {
            body: vec![Statement::Expression(Expression::FunctionCall {
                caller: Box::new(Expression::Variable(0.into())),
                arguments: vec![Expression::Literal(Literal::Bool(true))],
            })],
            then: None,
        }),
        parse_stmt("loop\nprintln(true)\nend", vec!["println"])
    );

    assert_eq!(
        Ok(Statement::Loop {
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
        Ok(Statement::For {
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
        Ok(Statement::For {
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
    assert_eq!(
        Ok(Statement::Match {
            var: Expression::Variable(0.into()),
            arms: vec![
                (
                    1.into(),
                    Ok(Expression::Comparison(
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

#[test]
fn strings() {
    let strings = [
        "'Test'", "\"Test\"",
        /* "'''Test'''", "\"\"\"Test\"\"\"" */
    ];
    for string in strings.iter() {
        assert_eq!(
            Ok(Expression::Literal(Literal::String("Test".into()))),
            parse_expr(string, Vec::new()),
        );
    }

    let strings = [
        "b'Test'",
        "b\"Test\"",
        /* "b'''Test'''", "b\"\"\"Test\"\"\"" */
    ];
    for string in strings.iter() {
        assert_eq!(
            Ok(Expression::Literal(Literal::ByteVec(b"Test".to_vec()))),
            parse_expr(string, Vec::new()),
        );
    }

    assert_eq!(
        Ok(Expression::Literal(Literal::String(
            " \0 \\ \n \r \t \" \u{2764} ".into()
        ))),
        parse_expr(r#"' \0 \\ \n \r \t \" \u{2764} '"#, Vec::new()),
    );

    assert_eq!(
        Ok(Expression::Literal(Literal::ByteVec(
            b" \0 \\ \n \r \t \" ".to_vec()
        ))),
        parse_expr(r#"b' \0 \\ \n \r \t \" '"#, Vec::new()),
    );

    assert_eq!(
        Ok(Expression::Literal(Literal::String(" \' ".into()))),
        parse_expr(r#"' \' '"#, Vec::new()),
    );

    assert_eq!(
        Ok(Expression::Literal(Literal::String(" \" ".into()))),
        parse_expr(r#"" \" ""#, Vec::new()),
    );

    assert_eq!(
        Ok(Expression::Literal(Literal::ByteVec(b" \' ".to_vec()))),
        parse_expr(r#"b' \' '"#, Vec::new()),
    );

    assert_eq!(
        Ok(Expression::Literal(Literal::ByteVec(b" \" ".to_vec()))),
        parse_expr(r#"b" \" ""#, Vec::new()),
    );

    // TODO: Fix
    // assert_eq!(
    //     Ok(Expression::Literal(Literal::String(" \'\'\' ".into()))),
    //     parse_expr(r#"''' \'\'\' '''"#, Vec::new()),
    // );
    //
    // assert_eq!(
    //     Ok(Expression::Literal(Literal::String(" \"\"\" ".into()))),
    //     parse_expr(r#""" \"\"\" """#, Vec::new()),
    // );
}

#[test]
fn integers() {
    let ints = ["100", "0x64", "+100", "+0x64"];
    for int in ints.iter() {
        assert_eq!(
            Ok(Expression::Literal(Literal::I32(100))),
            parse_expr(int, Vec::new()),
        );
    }

    let ints = ["-100", "-0x64"];
    for int in ints.iter() {
        assert_eq!(
            Ok(Expression::Literal(Literal::I32(-100))),
            parse_expr(int, Vec::new()),
        );
    }
}

// TODO: Test floats well
#[test]
fn floats() {
    if let Ok(Expression::Literal(Literal::F32(nan))) = parse_expr("NaN", Vec::new()) {
        assert!(nan.is_nan());
    } else {
        assert!(false);
    }

    assert_eq!(
        Ok(Expression::Literal(Literal::F32(core::f32::INFINITY))),
        parse_expr("inf", Vec::new()),
    );

    let floats = ["0.1", "0000000.1", "0000000.100000000"];
    for float in floats.iter() {
        assert_eq!(
            Ok(Expression::Literal(Literal::F32(0.1))),
            parse_expr(float, Vec::new()),
        );
    }
}

#[test]
fn functions_ast() {
    assert_eq!(
        Ok(Ast::Function {
            decorators: vec![Decorator {
                name: 1.into(),
                args: Vec::new(),
            }],
            attributes: vec![Attribute::Visibility(Visibility::FileLocal)],
            name: 0.into(),
            generics: Vec::new(),
            args: Vec::new(),
            returns: Type::Infer,
            body: Vec::new(),
        }),
        parse_ast("@inline\nfn test()\nend", vec!["test", "inline"]),
    );

    assert_eq!(
        Ok(Ast::Function {
            decorators: Vec::new(),
            attributes: vec![Attribute::Visibility(Visibility::Exposed)],
            name: 0.into(),
            generics: vec![3.into(), 4.into()],
            args: vec![(1.into(), Type::Infer), (1.into(), Type::Custom(2.into()))],
            returns: Type::Builtin(BuiltinType::Boolean),
            body: vec![Statement::Expression(Expression::FunctionCall {
                caller: Box::new(Expression::Variable(5.into())),
                arguments: vec![Expression::Literal(Literal::I32(1))]
            })],
        }),
        parse_ast(
            "exposed fn test<T, E>(a, a: b) -> bool\nprintln(1)\nend",
            vec!["test", "a", "b", "T", "E", "println"]
        ),
    );
}

#[test]
fn types_ast() {
    let interner = Arc::new(RwLock::new(StringInterner::new()));

    let builtins = [
        (
            Token::new(TokenType::Ident, "int", 0..2),
            Type::Builtin(BuiltinType::Integer),
        ),
        (
            Token::new(TokenType::Ident, "float", 0..4),
            Type::Builtin(BuiltinType::Float),
        ),
        (
            Token::new(TokenType::Ident, "bool", 0..3),
            Type::Builtin(BuiltinType::Boolean),
        ),
        (
            Token::new(TokenType::Ident, "str", 0..2),
            Type::Builtin(BuiltinType::String),
        ),
        (
            Token::new(TokenType::Ident, "[]", 0..1),
            Type::Builtin(BuiltinType::Vec(Box::new(Type::Infer))),
        ),
    ];

    // TODO: Use straight compares when Diagnostic is Debug
    for (token, ty) in builtins.iter() {
        assert_eq!(
            Type::try_from((*token, FileId::new(0usize), &interner)).ok(),
            Ok(ty.clone())
        );
    }

    let customs = [
        Token::new(TokenType::Ident, "sdgsagasfg", 0..9),
        Token::new(TokenType::Ident, "jkyu675", 0..6),
        Token::new(TokenType::Ident, "iyhgnfgbsfg", 0..9),
        Token::new(TokenType::Ident, "jdfsh75", 0..6),
        Token::new(TokenType::Ident, "sfhfgdjntyyb", 0..11),
        Token::new(TokenType::Ident, "randomletters", 0..12),
    ];

    // TODO: Use straight compares when Diagnostic is Debug
    for (idx, token) in customs.iter().enumerate() {
        interner.write().get_or_intern(token.source());

        assert_eq!(
            Type::try_from((*token, FileId::new(0usize), &interner)).ok(),
            Ok(Type::Custom(idx.into()))
        );
    }
}

#[test]
fn intern_string_doesnt_deadlock() {
    let interner = Arc::new(RwLock::new(StringInterner::new()));

    intern_string(&interner, "dfagsfgsfagff");
    intern_string(&interner, "dfa3465rwg345 by6v753 567Ugsfgsfyregwergagff");
    intern_string(&interner, "dfagsfsdfsafEhd sdfg246tBTEW GVWA yrjfgsfagff");
    intern_string(&interner, "dfagsfsdafsTCWERWEQTGERTANB fsbdagff");
}

#[test]
fn type_ast() {
    assert_eq!(
        Ok(Ast::Type {
            decorators: vec![
                Decorator {
                    name: 0.into(),
                    args: Vec::new(),
                },
                Decorator {
                    name: 1.into(),
                    args: Vec::new(),
                }
            ],
            attributes: vec![Attribute::Visibility(Visibility::Exposed)],
            name: 2.into(),
            generics: vec![3.into(), 4.into(), 5.into(), 6.into()],
            members: vec![
                TypeMember {
                    decorators: Vec::new(),
                    attributes:vec![Attribute::Visibility(Visibility::FileLocal)],
                    name: 7.into(),
                    ty: Type::Infer,
                }, TypeMember {
                    decorators: vec![Decorator {
                        name: 1.into(),
                        args: Vec::new(),
                    }],
                    attributes: vec![Attribute::Visibility(Visibility::Exposed)],
                    name: 8.into(),
                    ty: Type::Builtin(BuiltinType::Boolean),
                },
            ],
            methods: vec![Ast::Function {
                decorators: Vec::new(),
                attributes: vec![Attribute::Visibility(Visibility::FileLocal)],
                name: 2.into(),
                generics: Vec::new(),
                args: Vec::new(),
                returns: Type::Infer,
                body: Vec::new(),
            }]
        }),
        parse_ast(
            "@inline\n@builtin\nexposed type test<A, B, C, D>\nmember\n@builtin\nexposed another_member: bool\nfn test()\nend\nend",
            vec![
                "inline",
                "builtin",
                "test",
                "A",
                "B",
                "C",
                "D",
                "member",
                "another_member",
            ]
        ),
    );
}

#[test]
fn enum_ast() {
    assert_eq!(
        Ok(Ast::Enum {
            decorators: vec![Decorator {
                name: 0.into(),
                args: vec![Expression::Variable(1.into())],
            }],
            attributes: vec![Attribute::Visibility(Visibility::Package)],
            name: 2.into(),
            generics: vec![3.into()],
            variants: vec![
                EnumVariant::Unit {
                    name: 5.into(),
                    decorators: vec![Decorator {
                        name: 4.into(),
                        args: Vec::new()
                    }],
                },
                EnumVariant::Tuple {
                    name: 6.into(),
                    elements: vec![Type::Builtin(BuiltinType::Boolean), Type::Builtin(BuiltinType::Integer), Type::Custom(7.into())],
                    decorators: Vec::new(),
                },
            ],
        }),
        parse_ast(
            "@derive(Debug)\npkg enum testme<A>\n@no_construct\nUnitType\nTupleType(bool, int, tuplething)\nend",
            vec![
                "derive", "Debug", "testme", "A", "no_construct", "UnitType", "TupleType", "tuplething"
            ]
        ),
    );
}

#[test]
fn trait_ast() {
    assert_eq!(
        Ok(Ast::Trait {
            decorators: vec![Decorator {
                name: 0.into(),
                args: Vec::new(),
            }],
            attributes: vec![Attribute::Visibility(Visibility::Package)],
            name: 1.into(),
            generics: vec![2.into()],
            methods: vec![Ast::Function {
                decorators: vec![Decorator {
                    name: 3.into(),
                    args: vec![Expression::Literal(Literal::I32(10))]
                }],
                attributes: vec![Attribute::Visibility(Visibility::FileLocal)],
                name: 4.into(),
                generics: Vec::new(),
                args: Vec::new(),
                returns: Type::Builtin(BuiltinType::Boolean),
                body: Vec::new(),
            }]
        }),
        parse_ast(
            "@traits pkg trait Test<Classy>\n@something(10)\nfn be_classy() -> bool\nend\nend",
            vec!["traits", "Test", "Classy", "something", "be_classy"]
        ),
    );
}

#[test]
fn import_ast() {
    assert_eq!(
        Ok(Ast::Import {
            file: 0.into(),
            dest: ImportDest::Relative,
            exposes: ImportExposure::None(1.into()),
        }),
        parse_ast("import 'std.tests'\n", vec!["std.tests", "tests"]),
    );

    assert_eq!(
        Ok(Ast::Import {
            file: 0.into(),
            dest: ImportDest::Relative,
            exposes: ImportExposure::None(1.into()),
        }),
        parse_ast(
            "import 'std.tests' as tester\n",
            vec!["std.tests", "tester"]
        ),
    );

    assert_eq!(
        Ok(Ast::Import {
            file: 0.into(),
            dest: ImportDest::Relative,
            exposes: ImportExposure::All,
        }),
        parse_ast("import 'std.tests' exposing *\n", vec!["std.tests"]),
    );

    assert_eq!(
        Ok(Ast::Import {
            file: 0.into(),
            dest: ImportDest::Relative,
            exposes: ImportExposure::Members(vec![(1.into(), None), (2.into(), None)]),
        }),
        parse_ast(
            "import 'std.tests' exposing Okthing, OkthingElse\n",
            vec!["std.tests", "Okthing", "OkthingElse"]
        ),
    );

    assert_eq!(
        Ok(Ast::Import {
            file: 0.into(),
            dest: ImportDest::Relative,
            exposes: ImportExposure::Members(vec![
                (1.into(), Ok(2.into())),
                (3.into(), Ok(4.into()))
            ]),
        }),
        parse_ast(
            "import 'std.tests' exposing Okthing as Weird, OkthingElse as Weirder\n",
            vec!["std.tests", "Okthing", "Weird", "OkthingElse", "Weirder"]
        ),
    );
}
*/
