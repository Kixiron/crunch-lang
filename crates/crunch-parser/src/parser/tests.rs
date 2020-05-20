use super::*;
use crate::{files::FileId, parser::types::Signedness, pretty_printer::PrettyPrinter};
#[cfg(feature = "no-std")]
use alloc::{boxed::Box, string::String};
use core::iter;

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

macro_rules! eval_expr {
    (@pretty $expr:expr) => {{
        std::panic::catch_unwind(|| {
            let interner = Interner::default();
            let mut parser = Parser::new(
                $expr,
                CurrentFile::new(FileId::new(0), $expr.len()),
                interner,
            );
            let expr = parser.expr().unwrap();

            ron::ser::to_string_pretty(
                &expr,
                ron::ser::PrettyConfig {
                    depth_limit: 15,
                    new_line: "\n".into(),
                    indentor: "    ".into(),
                    separate_tuple_members: true,
                    enumerate_arrays: false,
                },
            )
            .unwrap()
        })
        .unwrap_or_else(|err| panic!("Error on the input {}: {:?}", $expr, err))
    }};

    ($expr:expr) => {{
        std::panic::catch_unwind(|| {
            let interner = Interner::default();
            let mut parser = Parser::new(
                $expr,
                CurrentFile::new(FileId::new(0), $expr.len()),
                interner,
            );
            let expr = parser.expr().unwrap();

            ron::ser::to_string(&expr).unwrap()
        })
        .unwrap_or_else(|err| panic!("Error on the input {}: {:?}", $expr, err))
    }};
}

#[test]
fn expr() {
    let expr = eval_expr!(@pretty "!5 + -10 / (test(54, test_again) % 200 if 10 else 52) - (true)");
    let expected = r#"UnaryExpr(
    Not,
    BinaryOp(
        Literal(Integer((
            sign: Positive,
            bits: "5",
        ))),
        Add,
        BinaryOp(
            Literal(Integer((
                sign: Negative,
                bits: "10",
            ))),
            Div,
            BinaryOp(
                Parenthesised(BinaryOp(
                    FunctionCall(
                        caller: Variable((
                            key: 1,
                        )),
                        arguments: [
                            Literal(Integer((
                                sign: Positive,
                                bits: "54",
                            ))),
                            Variable((
                                key: 2,
                            )),
                        ],
                    ),
                    Mod,
                    InlineConditional(
                        true_arm: Literal(Integer((
                            sign: Positive,
                            bits: "200",
                        ))),
                        condition: Literal(Integer((
                            sign: Positive,
                            bits: "10",
                        ))),
                        false_arm: Literal(Integer((
                            sign: Positive,
                            bits: "52",
                        ))),
                    ),
                )),
                Sub,
                Parenthesised(Literal(Bool(true))),
            ),
        ),
    ),
)"#;

    assert_eq!(expr, expected);
}

#[test]
fn integer_literals() {
    let fmt = |bits, sign| format!("Literal(Integer((sign:{:?},bits:\"{}\",)))", sign, bits);

    #[rustfmt::skip]
    let integers: Vec<(String, (u128, Sign))> = vec![
        ("10".into(),   (10, Sign::Positive)),
        ("+10".into(),  (10, Sign::Positive)),
        ("-10".into(),  (10, Sign::Negative)),
        ("0".into(),    (0, Sign::Positive)),
        ("+0".into(),   (0, Sign::Positive)),
        ("-0".into(),   (0, Sign::Negative)),
        (format!("{}",  u128::max_value()), (u128::max_value(), Sign::Positive)),
        (format!("+{}", u128::max_value()), (u128::max_value(), Sign::Positive)),
        (format!("-{}", u128::max_value()), (u128::max_value(), Sign::Negative)),
    ];

    for (src, (bits, sign)) in integers {
        let expr = eval_expr!(&src);
        let expected = fmt(bits, sign);

        assert_eq!(expr, expected);
    }
}

#[test]
fn float_literals() {
    let fmt = |float| format!("Literal(Float(F64({})))", float);

    // TODO: More comprehensive testing
    #[rustfmt::skip]
    let f64s: Vec<(String, f64)> = vec![
        ("10.0".into(), 10.0),
        ("+10.0".into(), 10.0),
        ("-10.0".into(), -10.0),
        ("0.0".into(), 0.0),
        ("+0.0".into(), 0.0),
        ("-0.0".into(), -0.0),
        ("NaN".into(), f64::NAN),
        ("inf".into(), f64::INFINITY),
    ];

    for (src, float) in f64s {
        let expr = eval_expr!(&src);
        let expected = fmt(float);

        assert_eq!(expr, expected);
    }
}

#[test]
fn string_literals() {
    let expr = eval_expr!("\"Some string\"");
    let expected =
        "Literal(String(([(83),(111),(109),(101),(32),(115),(116),(114),(105),(110),(103),])))";

    assert_eq!(expr, expected);

    let expr = eval_expr!("b\"Some string\"");
    let expected =
        "Literal(Array([Integer((sign:Positive,bits:\"83\",)),Integer((sign:Positive,bits:\"111\",)),\
        Integer((sign:Positive,bits:\"109\",)),Integer((sign:Positive,bits:\"101\",)),Integer((sign:Positive,bits:\"32\",)),\
        Integer((sign:Positive,bits:\"115\",)),Integer((sign:Positive,bits:\"116\",)),Integer((sign:Positive,bits:\"114\",)),\
        Integer((sign:Positive,bits:\"105\",)),Integer((sign:Positive,bits:\"110\",)),Integer((sign:Positive,bits:\"103\",)),]))";

    assert_eq!(expr, expected);
}

#[test]
fn char_literals() {
    let expr = eval_expr!("'S'");
    let expected = "Literal(Rune((83)))";
    assert_eq!(expr, expected);

    let expr = eval_expr!("b'S'");
    let expected = "Literal(Integer((sign:Positive,bits:\"83\",)))";
    assert_eq!(expr, expected);
}

#[test]
fn boolean_literals() {
    let fmt = |boolean| format!("Literal(Bool({}))", boolean);

    let expr = eval_expr!("true");
    let expected = fmt(true);
    assert_eq!(expr, expected);

    let expr = eval_expr!("false");
    let expected = fmt(false);
    assert_eq!(expr, expected);
}

// TODO: More varied internal types
#[test]
fn array_literals() {
    for i in 0..64 {
        let expr = format!(
            "arr[{}]",
            iter::repeat("true").take(i).collect::<Vec<_>>().join(", ")
        );
        let expected = format!("Array([{}])", "Literal(Bool(true)),".repeat(i));

        assert_eq!(eval_expr!(&expr), expected);
    }
}

// TODO: More varied internal types
#[test]
fn tuple_literals() {
    for i in 0..64 {
        let expr = format!(
            "tup[{}]",
            iter::repeat("true").take(i).collect::<Vec<_>>().join(", ")
        );
        let expected = format!("Tuple([{}])", "Literal(Bool(true)),".repeat(i));

        assert_eq!(eval_expr!(&expr), expected);
    }
}

#[test]
fn comparison_operations() {
    let fmt = |lhs, op, rhs| format!("Comparison({},{:?},{},)", lhs, op, rhs);

    let ten = "Literal(Integer((sign:Positive,bits:\"10\",)))";
    let zero = "Literal(Integer((sign:Positive,bits:\"0\",)))";
    #[rustfmt::skip]
    let comparisons: Vec<(String, (String, ComparisonOperand, String))> = vec![
        ("10 == 0".into(), (ten.to_string(), ComparisonOperand::Equal,        zero.to_string())),
        ("10 != 0".into(), (ten.to_string(), ComparisonOperand::NotEqual,     zero.to_string())),
        ("10 < 0".into(),  (ten.to_string(), ComparisonOperand::Less,         zero.to_string())),
        ("10 > 0".into(),  (ten.to_string(), ComparisonOperand::Greater,      zero.to_string())),
        ("10 <= 0".into(), (ten.to_string(), ComparisonOperand::LessEqual,    zero.to_string())),
        ("10 >= 0".into(), (ten.to_string(), ComparisonOperand::GreaterEqual, zero.to_string())),
    ];

    for (src, (lhs, op, rhs)) in comparisons {
        let expr = eval_expr!(&src);
        let expected = fmt(lhs, op, rhs);

        assert_eq!(expr, expected);
    }
}

#[test]
fn assignments() {
    let fmt = |assign| {
        format!(
            "Assignment(Variable((key:1,)),{:?},Literal(Integer((sign:Positive,bits:\"100\",))),)",
            assign
        )
    };

    #[rustfmt::skip]
    let integers: Vec<(String, AssignmentType)> = vec![
        ("i := 100".into(),  AssignmentType::Normal),
        ("i += 100".into(),  AssignmentType::BinaryOp(BinaryOperand::Add)),
        ("i -= 100".into(),  AssignmentType::BinaryOp(BinaryOperand::Sub)),
        ("i *= 100".into(),  AssignmentType::BinaryOp(BinaryOperand::Mult)),
        ("i /= 100".into(),  AssignmentType::BinaryOp(BinaryOperand::Div)),
        ("i %= 100".into(),  AssignmentType::BinaryOp(BinaryOperand::Mod)),
        ("i **= 100".into(), AssignmentType::BinaryOp(BinaryOperand::Pow)),
        ("i &= 100".into(),  AssignmentType::BinaryOp(BinaryOperand::BitAnd)),
        ("i |= 100".into(),  AssignmentType::BinaryOp(BinaryOperand::BitOr)),
        ("i ^= 100".into(),  AssignmentType::BinaryOp(BinaryOperand::BitXor)),
        ("i >>= 100".into(), AssignmentType::BinaryOp(BinaryOperand::Shr)),
        ("i <<= 100".into(), AssignmentType::BinaryOp(BinaryOperand::Shl)),
    ];

    for (src, ty) in integers {
        let expr = eval_expr!(&src);
        let expected = fmt(ty);

        assert_eq!(expr, expected);
    }
}

// TODO: Range more things
//       - Functions
//       - More ranges
//       - Strings
//       - Chars
//       - Floats
#[test]
fn ranges() {
    let expr = eval_expr!("1..100");
    let expected = "Range(Literal(Integer((sign:Positive,bits:\"1\",))),Literal(Integer((sign:Positive,bits:\"100\",))),)";

    assert_eq!(expr, expected);
}

#[test]
fn binary_ops() {
    let fmt = |op| {
        format!(
            "BinaryOp(Variable((key:1,)),{:?},Literal(Integer((sign:Positive,bits:\"100\",))),)",
            op
        )
    };

    #[rustfmt::skip]
    let integers: Vec<(String, BinaryOperand)> = vec![
        ("i + 100".into(),  BinaryOperand::Add),
        ("i - 100".into(),  BinaryOperand::Sub),
        ("i * 100".into(),  BinaryOperand::Mult),
        ("i / 100".into(),  BinaryOperand::Div),
        ("i % 100".into(),  BinaryOperand::Mod),
        ("i ** 100".into(), BinaryOperand::Pow),
        ("i & 100".into(),  BinaryOperand::BitAnd),
        ("i | 100".into(),  BinaryOperand::BitOr),
        ("i ^ 100".into(),  BinaryOperand::BitXor),
        ("i >> 100".into(), BinaryOperand::Shr),
        ("i << 100".into(), BinaryOperand::Shl),
    ];

    for (src, ty) in integers {
        let expr = eval_expr!(&src);
        let expected = fmt(ty);

        assert_eq!(expr, expected);
    }
}

#[test]
fn index_array_expr() {
    expr_eq!("array[0]");
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
fn decl_arr() {
    assert_eq!(
        "let variable: infer := arr[true, 10, \"test\", 'c']\n",
        format_stmt("let variable := arr[true, 10, \"test\", 'c']\n"),
    );
}

#[test]
fn decl_mut_var() {
    assert_eq!(
        "let mut variable: infer := 10\n",
        format_stmt("let mut variable := 10\n"),
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
fn extend_block() {
    println!(
        "{:#?}",
        format_ast(
            "extend Struct
                fn test()
                    println()
                end
             end",
        )
    );
}

// TODO: Test generic functions

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
            "arr[100, infer]",
            Type::Builtin(BuiltinType::Array(
                100,
                Box::new(Locatable::new(
                    Type::Infer,
                    Location::implicit(9..14, FileId(0)),
                )),
            )),
        ),
        (
            "arr[1, str]",
            Type::Builtin(BuiltinType::Array(
                1,
                Box::new(Locatable::new(
                    Type::Builtin(BuiltinType::String),
                    Location::concrete(7..10, FileId(0)),
                )),
            )),
        ),
        (
            "tup[str, arr[5, i32]]",
            Type::Builtin(BuiltinType::Tuple(vec![
                Locatable::new(
                    Type::Builtin(BuiltinType::String),
                    Location::concrete(4..7, FileId(0)),
                ),
                Locatable::new(
                    Type::Builtin(BuiltinType::Array(
                        5,
                        Box::new(Locatable::new(
                            Type::Builtin(BuiltinType::Integer {
                                sign: Signedness::Signed,
                                width: 32,
                            }),
                            Location::concrete(16..19, FileId(0)),
                        )),
                    )),
                    Location::concrete(9..20, FileId(0)),
                ),
            ])),
        ),
        (
            "CustomThingy",
            Type::ItemPath(ItemPath::new(vec![lasso::Spur::try_from_usize(0).unwrap()])),
        ),
        (
            "Custom.Thingy.Pathed",
            Type::ItemPath(ItemPath::new(vec![
                lasso::Spur::try_from_usize(0).unwrap(),
                lasso::Spur::try_from_usize(1).unwrap(),
                lasso::Spur::try_from_usize(2).unwrap(),
            ])),
        ),
    ];

    for (src, ty) in builtins.iter() {
        assert_eq!(
            &*Parser::new(src, CurrentFile::new(FileId(0), src.len()), Interner::new(),)
                .ascribed_type()
                .unwrap(),
            &ty.clone(),
        );
    }
}

#[test]
fn type_ast() {
    assert_eq!(
        "@inline\n@builtin\nexposed type test[A, B, C, D]\n    member: infer\n    @builtin\n    exposed another_member: bool\nend\n",
        format_ast("@inline\n@builtin\nexposed type test[A, B, C, D]\nmember\n@builtin\nexposed another_member: bool\nend"),
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
    ast_eq!("fn ȨʩӺޗઈဪ⏫㡘㢡䔹✅✨❗❓() -> infer\n    empty\nend\n");
}

#[cfg(not(any(target_arch = "wasm32", miri)))]
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
                Ok(Expression::Literal(Literal::String(..))) | Ok(Expression::Literal(Literal::Array(..))) => {},

                Err(Locatable { data: _data @ Error::Syntax(SyntaxError::InvalidEscapeCharacters(..)), .. })
                | Err(Locatable { data: _data @ Error::Syntax(SyntaxError::UnrecognizedEscapeSeq(..)), .. })
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
                Ok(Expression::Literal(Literal::Rune(..))) | Ok(Expression::Literal(Literal::Integer(..))) => {},

                Err(Locatable { data: _data @ Error::Syntax(SyntaxError::TooManyRunes), .. })
                | Err(Locatable { data: _data @ Error::Syntax(SyntaxError::UnrecognizedEscapeSeq(..)), .. })
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
