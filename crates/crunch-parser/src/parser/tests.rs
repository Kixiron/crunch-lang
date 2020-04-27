use super::*;
use crate::{files::FileId, parser::ast::Signedness, pretty_printer::PrettyPrinter};

#[cfg(feature = "no-std")]
use alloc::{boxed::Box, string::String};

fn format_expr(source: &str) -> String {
    let interner = Interner::default();
    let mut parser = Parser::new(
        source,
        CurrentFile::new(FileId::new(0), source.len()),
        interner,
    );
    let expr = parser.expr().unwrap();

    let interner = core::mem::take(&mut parser.string_interner);
    let mut string = String::new();
    PrettyPrinter::new(interner)
        .print_expr(&mut string, &expr)
        .unwrap();

    string
}

macro_rules! expr_eq {
    ($src:literal) => {
        assert_eq!($src, format_expr($src));
    };
}

fn format_stmt(source: &str) -> String {
    let interner = Interner::default();
    let mut parser = Parser::new(
        source,
        CurrentFile::new(FileId::new(0), source.len()),
        interner,
    );
    let stmt = parser.stmt().unwrap().unwrap();

    let interner = core::mem::take(&mut parser.string_interner);
    let mut string = String::new();
    PrettyPrinter::new(interner)
        .print_stmt(&mut string, &stmt)
        .unwrap();

    string
}

macro_rules! stmt_eq {
    ($src:literal) => {
        assert_eq!($src, format_stmt($src));
    };
}

fn format_ast(source: &str) -> String {
    let interner = Interner::default();
    let mut parser = Parser::new(
        source,
        CurrentFile::new(FileId::new(0), source.len()),
        interner,
    );
    let stmt = parser.ast().unwrap().unwrap();

    let interner = core::mem::take(&mut parser.string_interner);
    let mut string = String::new();
    PrettyPrinter::new(interner)
        .print_ast(&mut string, &stmt)
        .unwrap();

    string
}

macro_rules! ast_eq {
    ($src:literal) => {
        assert_eq!($src, format_ast($src));
    };
}

#[test]
fn expr() {
    expr_eq!("!5 + -10 / (test(54, test_again) % 200 if 10 else 52) - (true)");
}

#[test]
fn parse_signed_ints() {
    expr_eq!("10");
    expr_eq!("-10");
    assert_eq!("10", format_expr("+10"));
}

#[test]
fn single_paren_expr() {
    expr_eq!("10 == (0)");
}

#[test]
fn index_array_expr() {
    expr_eq!("array[0]");
}

#[test]
fn array_literal_expr() {
    expr_eq!("[true, test, 100 / 5]");
}

#[test]
fn comparisons_expr() {
    expr_eq!("10 > 9");
    expr_eq!("10 < 9");
    expr_eq!("10 >= 9");
    expr_eq!("10 <= 9");
    expr_eq!("10 == 9");
    expr_eq!("10 != 9");
}

// TODO: More literals
#[test]
fn literals_expr() {
    expr_eq!("100");
    expr_eq!("false");
    expr_eq!("true");
}

#[test]
fn func_call_expr() {
    expr_eq!("test(test2, 0)");
    expr_eq!("test_func()");
}

#[test]
fn dotted_func_call_expr() {
    expr_eq!("test_func.function()");
}

#[test]
fn assignment_expr() {
    expr_eq!("i := 100");
    expr_eq!("i += 100");
    expr_eq!("i -= 100");
    expr_eq!("i *= 100");
    expr_eq!("i /= 100");
    expr_eq!("i %= 100");
    expr_eq!("i **= 100");
    expr_eq!("i &= 100");
    expr_eq!("i |= 100");
    expr_eq!("i ^= 100");
    expr_eq!("i >>= 100");
    expr_eq!("i <<= 100");
}

#[test]
fn range_expr() {
    expr_eq!("1..100");
}

#[test]
fn bin_ops_expr() {
    expr_eq!("10 + 9");
    expr_eq!("10 - 9");
    expr_eq!("10 * 9");
    expr_eq!("10 / 9");
    expr_eq!("10 % 9");
    expr_eq!("10 ** 9");
    expr_eq!("10 & 9");
    expr_eq!("10 | 9");
    expr_eq!("10 ^ 9");
    expr_eq!("10 << 9");
    expr_eq!("10 >> 9");
}

#[test]
fn prefix_ops_expr() {
    expr_eq!("var");
    expr_eq!("!var");
    expr_eq!("-var");
}

#[test]
fn inline_conditional_expr() {
    expr_eq!("0 if true else 1");
}

#[test]
fn if_stmt() {
    stmt_eq!(
        "if true\n    \
            println(1)\n\
        end\n"
    );
    assert_eq!(
        "if true\n    println(1)\nelse if true\n    println(2)\nend\n",
        format_stmt("if true\n    println(1)\nelse\n    println(2)\nend\n"),
    );
    assert_eq!(
        "if true\n    println(1)\nelse if 1 > 5\n    println(2)\nend\n",
        format_stmt("if true\n    println(1)\nelse if 1 > 5\n    println(2)\nend\n"),
    );
    assert_eq!(
        "if true\n    println(1)\nelse if 1 > 5\n    println(2)\nelse if true\n    println(3)\nend\n",
        format_stmt(
            "if true\n    println(1)\nelse if 1 > 5\n    println(2)\nelse\n    println(3)\nend\n"
        ),
    );
}

#[test]
fn nested_if_stmt() {
    stmt_eq!(
        "if true\n    \
            if false\n        \
                println(1)\n    \
            end\n\
        end\n"
    );
}

#[test]
fn function_call_stmt() {
    stmt_eq!("test_func()\n");
}

#[test]
fn decl_var() {
    assert_eq!(
        "let variable: infer := true\n",
        format_stmt("let variable := true\n"),
    );
}

#[test]
fn long_func_decl_stmt() {
    stmt_eq!("let jkpwn: infer := vssekgmbxoxshmhinx(jnldfzbd, kcqpq, gbuaqbax, argro, xhmfc, bredcp, pwlfywfkb, vgsjjcy, exomcmbf, cjsjpvgcl, omtlfpw, ssdrm, kxrtaun, xexzz, ejvmxj, ssmqkbqqi)\n");
}

#[test]
fn return_stmt() {
    stmt_eq!("return\n");
    stmt_eq!("return 10 + 9\n");
}

#[test]
fn break_stmt() {
    stmt_eq!("break\n");
    stmt_eq!("break 10 + 9\n");
}

#[test]
fn continue_stmt() {
    stmt_eq!("continue\n");
}

#[test]
fn while_stmt() {
    stmt_eq!(
        "while true\n    \
            println(true)\n\
        end\n"
    );
    stmt_eq!(
        "while true\n    \
            println(true)\n\
        then\n    \
            println(false)\n\
        end\n"
    );
}

#[test]
fn loop_stmt() {
    stmt_eq!(
        "loop\n    \
            println(true)\n\
        end\n"
    );
    stmt_eq!(
        "loop\n    \
            println(true)\n\
        then\n    \
            println(false)\n\
        end\n"
    );
}

#[test]
fn for_stmt() {
    stmt_eq!(
        "for i in 100\n    \
            println(true)\n\
        end\n"
    );
    stmt_eq!(
        "for i in 100\n    \
            println(true)\n\
        then\n    \
            println(false)\n\
        end\n"
    );
}

#[test]
fn match_stmt() {
    stmt_eq!(
        "match something\n    \
            i where i < 6 =>\n        \
                println(6)\n    \
            end\n    \
            i =>\n        \
                println(i)\n    \
            end\n\
        end\n"
    );
}

#[test]
fn strings() {
    expr_eq!(r#""Test""#);
    assert_eq!(format!("{:?}", b"Test"), format_expr(r#"b"Test""#));
    assert_eq!(
        "\" \\\\ \\n \\r \\t \\\" \"",
        format_expr(r#"" \\ \n \r \t \" ""#)
    );
}

#[test]
fn integers() {
    expr_eq!("100");
    assert_eq!("100", format_expr("0x64"));
    assert_eq!("100", format_expr("+100"));
    assert_eq!("100", format_expr("+0x64"));
    expr_eq!("-100");
    assert_eq!("-100", format_expr("-0x64"));
}

// TODO: Test floats well
#[test]
fn floats() {
    expr_eq!("NaN");
    expr_eq!("inf");

    expr_eq!("0.1");
    assert_eq!("0.1", format_expr("0000000.1"));
    assert_eq!("0.1", format_expr("0000000.100000000"));
}

#[test]
fn functions_ast() {
    assert_eq!(
        "@inline\nfn test() -> infer\nend\n",
        format_ast("@inline\nfn test()\nend"),
    );
    assert_eq!(
        "exposed fn test(a: infer, a: b) -> bool\n    println(1)\nend\n",
        format_ast("exposed fn test(a, a: b) -> bool\n    println(1)\nend\n"),
    );
}

#[test]
fn generic_functions_ast() {
    assert_eq!(
        "@inline\nfn test(comptime T: type) -> infer\nend\n",
        format_ast("@inline\nfn test(comptime T: type)\nend"),
    );
    assert_eq!(
        "exposed fn test(comptime T: type[Clone, Debug], a: infer, a: b) -> bool\n    println(1)\nend\n",
        format_ast(
            "exposed fn test(comptime T: type[Clone, Debug], a, a: b) -> bool\n    println(1)\nend\n"
        ),
    );
}

#[test]
fn lots_generic_functions_ast() {
    assert_eq!(
        "exposed fn test(comptime T: type[Clone, Debug], a: infer, comptime E: type[PartialEq, Eq, Ord], comptime F: type[Copy]) -> bool\n    println(1)\nend\n",
        format_ast(
            "exposed fn test(comptime T: type[Clone, Debug], a: infer, comptime E: type[PartialEq, Eq, Ord], comptime F: type[Copy]) -> bool\n    println(1)\nend\n"
        ),
    );
}

#[test]
fn types_ast() {
    use lasso::Key;

    let builtins = [
        (
            "i128",
            Type::Builtin(BuiltinType::Integer {
                sign: Signedness::Signed,
                width: 128,
            }),
        ),
        (
            "u128",
            Type::Builtin(BuiltinType::Integer {
                sign: Signedness::Unsigned,
                width: 128,
            }),
        ),
        (
            "u1",
            Type::Builtin(BuiltinType::Integer {
                sign: Signedness::Unsigned,
                width: 1,
            }),
        ),
        (
            "i65535",
            Type::Builtin(BuiltinType::Integer {
                sign: Signedness::Signed,
                width: u16::max_value(),
            }),
        ),
        (
            "u65535",
            Type::Builtin(BuiltinType::Integer {
                sign: Signedness::Unsigned,
                width: u16::max_value(),
            }),
        ),
        (
            "f65535",
            Type::Builtin(BuiltinType::Float {
                width: u16::max_value(),
            }),
        ),
        (
            "i7453",
            Type::Builtin(BuiltinType::Integer {
                sign: Signedness::Signed,
                width: 7453,
            }),
        ),
        ("f32", Type::Builtin(BuiltinType::Float { width: 32 })),
        ("f1", Type::Builtin(BuiltinType::Float { width: 1 })),
        ("bool", Type::Builtin(BuiltinType::Boolean)),
        ("unit", Type::Builtin(BuiltinType::Unit)),
        ("str", Type::Builtin(BuiltinType::String)),
        ("absurd", Type::Builtin(BuiltinType::Absurd)),
        (
            "uptr",
            Type::Builtin(BuiltinType::IntPtr(Signedness::Unsigned)),
        ),
        (
            "iptr",
            Type::Builtin(BuiltinType::IntPtr(Signedness::Signed)),
        ),
        (
            "ureg",
            Type::Builtin(BuiltinType::IntReg(Signedness::Unsigned)),
        ),
        (
            "ireg",
            Type::Builtin(BuiltinType::IntReg(Signedness::Signed)),
        ),
        (
            "arr[]",
            Type::Builtin(BuiltinType::Array(Box::new(Locatable::new(
                Type::Infer,
                Location::implicit(0..3, FileId(0)),
            )))),
        ),
        (
            "arr[str]",
            Type::Builtin(BuiltinType::Array(Box::new(Locatable::new(
                Type::Builtin(BuiltinType::String),
                Location::concrete(4..7, FileId(0)),
            )))),
        ),
        (
            "tup[str, arr[i32]]",
            Type::Builtin(BuiltinType::Tuple(vec![
                Locatable::new(
                    Type::Builtin(BuiltinType::String),
                    Location::concrete(4..7, FileId(0)),
                ),
                Locatable::new(
                    Type::Builtin(BuiltinType::Array(Box::new(Locatable::new(
                        Type::Builtin(BuiltinType::Integer {
                            sign: Signedness::Signed,
                            width: 32,
                        }),
                        Location::concrete(13..16, FileId(0)),
                    )))),
                    Location::concrete(9..17, FileId(0)),
                ),
            ])),
        ),
        (
            "type[Trait1, Trait2]",
            Type::TraitObj(vec![
                Locatable::new(
                    Type::Custom(lasso::SmallSpur::try_from_usize(0).unwrap()),
                    Location::concrete(5..11, FileId(0)),
                ),
                Locatable::new(
                    Type::Custom(lasso::SmallSpur::try_from_usize(1).unwrap()),
                    Location::concrete(13..19, FileId(0)),
                ),
            ]),
        ),
        (
            "CustomThingy",
            Type::Custom(lasso::SmallSpur::try_from_usize(0).unwrap()),
        ),
    ];

    for (src, ty) in builtins.iter() {
        assert_eq!(
            Parser::new(src, CurrentFile::new(FileId(0), src.len()), Interner::new(),)
                .ascribed_type()
                .unwrap()
                .data(),
            &ty.clone(),
        );
    }
}

#[test]
fn type_ast() {
    assert_eq!(
        "@inline\n@builtin\nexposed type test[A, B, C, D]\n    member: infer\n    @builtin\n    exposed another_member: bool\n    fn test() -> infer\n    end\nend\n",
        format_ast("@inline\n@builtin\nexposed type test[A, B, C, D]\nmember\n@builtin\nexposed another_member: bool\nfn test()\nend\nend"),
    );
}

#[test]
fn enum_ast() {
    assert_eq!(
        "@derive(Debug)\npkg enum testme[A]\n    @no_construct\n    UnitType\n    TupleType(bool, int, tuplething)\nend\n",
        format_ast("@derive(Debug)\npkg enum testme[A]\n@no_construct\nUnitType\nTupleType(bool, int, tuplething)\nend"),
    );
}

#[test]
fn trait_ast() {
    assert_eq!(
        "@traits\npkg trait Test[Classy]\n    @something(10)\n    fn be_classy() -> bool\n    end\nend\n",
        format_ast(
            "@traits pkg trait Test[Classy]\n@something(10)\nfn be_classy() -> bool\nend\nend"
        ),
    );
}

#[test]
fn import_ast() {
    assert_eq!(
        "import \"std.tests\" as tests\n",
        format_ast("import \"std.tests\"\n")
    );

    assert_eq!(
        "import \"std.tests\" as tester\n",
        format_ast("import \"std.tests\" as tester\n"),
    );

    assert_eq!(
        "import \"std.tests\" exposing *\n",
        format_ast("import \"std.tests\" exposing *\n"),
    );

    assert_eq!(
        "import \"std.tests\" exposing Okthing, OkthingElse\n",
        format_ast("import \"std.tests\" exposing Okthing, OkthingElse\n"),
    );

    assert_eq!(
        "import \"std.tests\" exposing Okthing as Weird, OkthingElse as Weirder\n",
        format_ast("import \"std.tests\" exposing Okthing as Weird, OkthingElse as Weirder\n"),
    );
}

#[test]
fn non_ascii_idents() {
    ast_eq!("fn ȨʩӺޗઈဪ⏫㡘㢡䔹❉✅✨❗❓()\nend");
}

mod proptests {
    use super::*;
    use core::ops::Deref;
    use proptest::prelude::*;

    proptest! {
        #[test]
        fn strings(s in r#"b?"(\\.|[^\\"])*""#) {
            let mut parser = Parser::new(&s, CurrentFile::new(FileId(0), s.len()), Interner::default());

            let expr = parser.expr().map(|e| e.deref().clone());
            match expr {
                Ok(Expression::Literal(Literal::String(_))) | Ok(Expression::Literal(Literal::Array(_))) => {},

                Err(Locatable { data: _data @ Error::Syntax(SyntaxError::UnrecognizedEscapeSeq(_)), .. })
                | Err(Locatable { data: _data @ Error::Syntax(SyntaxError::MissingEscapeBraces), .. })
                | Err(Locatable { data: _data @ Error::Syntax(SyntaxError::MissingEscapeSpecifier), .. }) => {}

                _ => prop_assert!(false),
            }
        }

        #[test]
        fn runes(s in "b?'[^']*'") {
            let mut parser = Parser::new(&s, CurrentFile::new(FileId(0), s.len()), Interner::default());

            let expr = parser.expr().map(|e| e.deref().clone());
            match expr {
                Ok(Expression::Literal(Literal::Rune(_))) | Ok(Expression::Literal(Literal::Integer(_))) => {},

                Err(Locatable { data: _data @ Error::Syntax(SyntaxError::TooManyRunes), .. })
                | Err(Locatable { data: _data @ Error::Syntax(SyntaxError::UnrecognizedEscapeSeq(_)), .. })
                | Err(Locatable { data: _data @ Error::Syntax(SyntaxError::MissingEscapeBraces), .. })
                | Err(Locatable { data: _data @ Error::Syntax(SyntaxError::MissingEscapeSpecifier), .. }) => {}

                _ => prop_assert!(false),
            }
        }

        #[test]
        fn base10_int(s in "[+-]?[0-9][0-9_]*") {
            let mut parser = Parser::new(&s, CurrentFile::new(FileId(0), s.len()), Interner::default());

            let expr = parser.expr().map(|e| e.deref().clone());
            let cond = matches!(expr, Ok(Expression::Literal(Literal::Integer { .. })));
            prop_assert!(cond);
        }

        #[test]
        fn base16_int(s in "[+-]?0x[0-9a-fA-F][0-9a-fA-F_]*") {
            let mut parser = Parser::new(&s, CurrentFile::new(FileId(0), s.len()), Interner::default());

            let expr = parser.expr().map(|e| e.deref().clone());
            let cond = matches!(expr, Ok(Expression::Literal(Literal::Integer(Integer { .. }))));
            prop_assert!(cond);
        }

        #[test]
        fn base2_int(s in "[+-]?0b[0-1][0-1_]*") {
            let mut parser = Parser::new(&s, CurrentFile::new(FileId(0), s.len()), Interner::default());

            let expr = parser.expr().map(|e| e.deref().clone());
            let cond = matches!(expr, Ok(Expression::Literal(Literal::Integer(Integer { .. }))));
            prop_assert!(cond);
        }

        #[test]
        fn base10_float(s in "[+-]?[0-9][0-9_]*\\.[0-9][0-9_]*([eE][+-]?[0-9][0-9_]*)?") {
            let mut parser = Parser::new(&s, CurrentFile::new(FileId(0), s.len()), Interner::default());

            let expr = parser.expr().map(|e| e.deref().clone());
            prop_assert!(matches!(expr, Ok(Expression::Literal(Literal::Float(..)))));
        }

        #[test]
        fn base16_float(s in "[+-]?0x[0-9a-fA-F][0-9a-fA-F_]*\\.[0-9a-fA-F][0-9a-fA-F_]*([pP][+-]?[0-9][0-9_]?)?") {
            let mut parser = Parser::new(&s, CurrentFile::new(FileId(0), s.len()), Interner::default());

            let expr = parser.expr().map(|e| e.deref().clone());
            prop_assert!(matches!(expr, Ok(Expression::Literal(Literal::Float(..)))));
        }
    }
}
