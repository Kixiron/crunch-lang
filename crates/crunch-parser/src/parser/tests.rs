use super::*;
use crate::pretty_printer::PrettyPrinter;
#[cfg(feature = "no-std")]
use alloc::string::String;
use core::iter;
use crunch_shared::{
    error::{Error, Locatable, SyntaxError},
    files::FileId,
    strings::StrT,
    trees::{
        ast::{
            AssignKind, BinaryOp, CompOp, ExprKind, Integer, ItemPath, Literal, Sign, Signedness,
            Type,
        },
        Ref,
    },
};

fn format_expr(source: &str) -> String {
    let context = Context::default();
    let mut parser = Parser::new(
        source,
        CurrentFile::new(FileId::new(0), source.len()),
        context.clone(),
    );
    let expr = parser.expr().unwrap();

    let mut string = String::new();
    PrettyPrinter::new(context)
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
    let context = Context::default();
    let mut parser = Parser::new(
        source,
        CurrentFile::new(FileId::new(0), source.len()),
        context.clone(),
    );
    let stmt = parser.stmt().unwrap().unwrap();

    let mut string = String::new();
    PrettyPrinter::new(context)
        .print_stmt(&mut string, &stmt)
        .unwrap();

    string
}

macro_rules! stmt_eq {
    ($src:literal) => {
        assert_eq!($src, format_stmt($src));
    };
}

fn format_item(source: &str) -> String {
    let context = Context::default();
    let mut parser = Parser::new(
        source,
        CurrentFile::new(FileId::new(0), source.len()),
        context.clone(),
    );
    let item = parser.item().unwrap().unwrap();

    let mut string = String::new();
    PrettyPrinter::new(context)
        .print_item(&mut string, &item)
        .unwrap();

    string
}

macro_rules! ast_eq {
    ($src:literal) => {
        assert_eq!($src, format_item($src));
    };
}

macro_rules! assert_expr_eq {
    ($expr:expr, $expect:expr $(,)?) => {{
        let expr = std::panic::catch_unwind(|| {
            let context = Context::default();
            let mut parser = Parser::new(
                &$expr,
                CurrentFile::new(FileId::new(0), $expr.len()),
                context,
            );

            parser.expr().unwrap()
        })
        .unwrap_or_else(|err| panic!("Error on the input {}: {:?}", $expr, err));
        println!(
            "{}",
            ron::ser::to_string_pretty(&expr, ron::ser::PrettyConfig::default()).unwrap()
        );

        assert_eq!(expr, ron::from_str(&$expect).unwrap());
    }};
}

macro_rules! eval {
    (@expr $expr:expr) => {{
        std::panic::catch_unwind(|| {
            let context = Context::default();
            let mut parser = Parser::new(
                $expr,
                CurrentFile::new(FileId::new(0), $expr.len()),
                context,
            );
            let expr = parser.expr().unwrap();

            ron::ser::to_string(&expr).unwrap()
        })
        .unwrap_or_else(|err| panic!("Error on the input {}: {:?}", $expr, err))
    }};

    (@generics $generics:expr) => {{
        std::panic::catch_unwind(|| {
            let context = Context::default();
            let mut parser = Parser::new(
                $generics,
                CurrentFile::new(FileId::new(0), $generics.len()),
                context,
            );
            let generics = parser.generics().unwrap();

            ron::ser::to_string(&generics).unwrap()
        })
        .unwrap_or_else(|err| panic!("Error on the input {}: {:?}", $generics, err))
    }};

    (@item $item:expr) => {{
        std::panic::catch_unwind(|| {
            let context = Context::default();
            let mut parser = Parser::new(
                $item,
                CurrentFile::new(FileId::new(0), $item.len()),
                context,
            );
            let item = parser.item().unwrap().unwrap();

            ron::ser::to_string(&item).unwrap()
        })
        .unwrap_or_else(|err| panic!("Error on the input {}: {:?}", $item, err))
    }};
}

#[test]
fn expr() {
    // let expr = eval!(@expr "!5 + -10 / (test(54, test_again) % 200 if 10 else 52) - (true)");
    // let expected = "UnaryExpr(Not,BinaryOp(Literal(Integer((sign:Positive,bits:5))),Add,\
    //     BinaryOp(Literal(Integer((sign:Negative,bits:10))),Div,BinaryOp(Parenthesised(BinaryOp(\
    //     FunctionCall(caller:Variable((key:1)),arguments:[Literal(Integer((sign:Positive,bits:54\
    //     ))),Variable((key:2))]),Mod,InlineConditional(true_arm:Literal(Integer((sign:Positive,bits:200\
    //     ))),condition:Literal(Integer((sign:Positive,bits:10))),false_arm:Literal(Integer((sign:Positive,\
    //     bits:52)))))),Sub,Parenthesised(Literal(Bool(true)))))))";
    //
    // assert_eq!(expr, expected);

    let expr = "!5 + -10 / (test(54, test_again) % 200) - (true)";
    let expected = "
    (
        kind: UnaryOp(Not, ((
            kind: BinaryOp((
                lhs: ((
                    kind: Literal(Integer((
                        sign: Positive,
                        bits: 5,
                    ))),
                )),
                op: Add,
                rhs: ((
                    kind: BinaryOp((
                        lhs: ((
                            kind: Literal(Integer((
                                sign: Negative,
                                bits: 10,
                            ))),
                        )),
                        op: Div,
                        rhs: ((
                            kind: BinaryOp((
                                lhs: ((
                                    kind: Paren(((
                                        kind: BinaryOp((
                                            lhs: ((
                                                kind: FuncCall(
                                                    caller: ((
                                                        kind: Variable(((key: 1))),
                                                    )),
                                                    args: [

                                                        (
                                                            kind: Literal(Integer((
                                                                sign: Positive,
                                                                bits: 54,
                                                            ))),
                                                        ),
                                                        (
                                                            kind: Variable(((key: 2)))
                                                        ),
                                                    ],
                                                ),
                                            )),
                                            op: Mod,
                                            rhs: ((
                                                kind: Literal(Integer((
                                                    sign: Positive,
                                                    bits: 200,
                                                ))),
                                            )),
                                        )),
                                    ))),
                                )),
                                op: Sub,
                                rhs: ((
                                    kind: Paren(((
                                        kind: Literal(Bool(true)),
                                    ))),
                                )),
                            )),
                        )),
                    )),
                )),
            )),
        ))),
    )";

    assert_expr_eq!(expr, expected);
}

#[test]
fn integer_literals() {
    let fmt = |bits, sign| format!("(kind: Literal(Integer((sign:{:?},bits:{}))))", sign, bits);

    #[rustfmt::skip]
    let integers: Vec<(String, (u128, Sign))> = vec![
        ("10".into(),    (10, Sign::Positive)),
        ("+10".into(),   (10, Sign::Positive)),
        ("-10".into(),   (10, Sign::Negative)),
        ("0".into(),     (0,  Sign::Positive)),
        ("+0".into(),    (0,  Sign::Positive)),
        ("-0".into(),    (0,  Sign::Negative)),
        ("0x64".into(),  (100, Sign::Positive)),
        ("+0x64".into(), (100, Sign::Positive)),
        ("-0x64".into(), (100, Sign::Negative)),
        (format!("{}",   u128::max_value()), (u128::max_value(), Sign::Positive)),
        (format!("+{}",  u128::max_value()), (u128::max_value(), Sign::Positive)),
        (format!("-{}",  u128::max_value()), (u128::max_value(), Sign::Negative)),
    ];

    for (src, (bits, sign)) in integers {
        assert_expr_eq!(src, fmt(bits, sign));
    }
}

#[test]
fn float_literals() {
    let fmt = |float| format!("(kind: Literal(Float(F64({}))))", float);

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
        assert_expr_eq!(src, fmt(float));
    }
}

#[test]
fn string_literals() {
    let expected =
        "(kind: Literal(String(([(83),(111),(109),(101),(32),(115),(116),(114),(105),(110),(103)]))))";

    assert_expr_eq!(r#""Some string""#, expected);

    let expected = "
        (kind: Literal(Array([
            Integer((sign:Positive,bits:83)),
            Integer((sign:Positive,bits:111)),
            Integer((sign:Positive,bits:109)),
            Integer((sign:Positive,bits:101)),
            Integer((sign:Positive,bits:32)),
            Integer((sign:Positive,bits:115)),
            Integer((sign:Positive,bits:116)),
            Integer((sign:Positive,bits:114)),
            Integer((sign:Positive,bits:105)),
            Integer((sign:Positive,bits:110)),
            Integer((sign:Positive,bits:103)),
        ])))";

    assert_expr_eq!(r#"b"Some string""#, expected);
}

#[test]
fn char_literals() {
    let expected = "(kind: Literal(Rune((83))))";
    assert_expr_eq!("'S'", expected);

    let expected = "(kind: Literal(Integer((sign:Positive,bits:83))))";
    assert_expr_eq!("b'S'", expected);
}

#[test]
fn boolean_literals() {
    assert_expr_eq!("true", "(kind: Literal(Bool(true)))");
    assert_expr_eq!("false", "(kind: Literal(Bool(false)))");
}

// TODO: More varied internal types
#[test]
fn array_literals() {
    for i in 0..64 {
        let expr = format!(
            "arr[{}]",
            iter::repeat("true").take(i).collect::<Vec<_>>().join(", ")
        );
        let expected = format!(
            "(kind: Array([{}]))",
            iter::repeat("(kind: Literal(Bool(true)))")
                .take(i)
                .collect::<Vec<_>>()
                .join(",")
        );

        assert_expr_eq!(expr, expected);
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
        let expected = format!(
            "(kind: Tuple([{}]))",
            iter::repeat("(kind: Literal(Bool(true)))")
                .take(i)
                .collect::<Vec<&str>>()
                .join(",")
        );

        assert_expr_eq!(expr, expected);
    }
}

#[test]
fn comparison_operations() {
    let fmt = |op| {
        format!(
            "(kind:
                Comparison((
                    lhs: ((
                        kind: Literal(Integer((
                            sign: Positive,
                            bits: 10,
                        ))),
                    )),
                    op: {:?},
                    rhs: ((
                        kind: Literal(Integer((
                            sign: Positive,
                            bits: 0,
                        ))),
                    )),
                ))
            )",
            op
        )
    };

    #[rustfmt::skip]
    let comparisons: Vec<(&str, CompOp)> = vec![
        ("10 == 0", CompOp::Equal),
        ("10 != 0", CompOp::NotEqual),
        ("10 < 0",  CompOp::Less),
        ("10 > 0",  CompOp::Greater),
        ("10 <= 0", CompOp::LessEqual),
        ("10 >= 0", CompOp::GreaterEqual),
    ];

    for (src, op) in comparisons {
        assert_expr_eq!(src, fmt(op));
    }
}

#[test]
fn assignments() {
    let fmt = |assign| {
        format!(
            "(
                kind: Assign((
                    lhs: ((
                        kind: Variable(((
                            key: 1,
                        ))),
                    )),
                    op: {:?},
                    rhs: ((
                        kind: Literal(Integer((
                            sign: Positive,
                            bits: 100,
                        ))),
                    )),
                ))
            )",
            assign
        )
    };

    #[rustfmt::skip]
    let integers: Vec<(&str, AssignKind)> = vec![
        ("i := 100",  AssignKind::Normal),
        ("i += 100",  AssignKind::BinaryOp(BinaryOp::Add)),
        ("i -= 100",  AssignKind::BinaryOp(BinaryOp::Sub)),
        ("i *= 100",  AssignKind::BinaryOp(BinaryOp::Mult)),
        ("i /= 100",  AssignKind::BinaryOp(BinaryOp::Div)),
        ("i %= 100",  AssignKind::BinaryOp(BinaryOp::Mod)),
        ("i **= 100", AssignKind::BinaryOp(BinaryOp::Pow)),
        ("i &= 100",  AssignKind::BinaryOp(BinaryOp::BitAnd)),
        ("i |= 100",  AssignKind::BinaryOp(BinaryOp::BitOr)),
        ("i ^= 100",  AssignKind::BinaryOp(BinaryOp::BitXor)),
        ("i >>= 100", AssignKind::BinaryOp(BinaryOp::Shr)),
        ("i <<= 100", AssignKind::BinaryOp(BinaryOp::Shl)),
    ];

    for (src, ty) in integers {
        assert_expr_eq!(src, fmt(ty));
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
    let expected = "(
        kind: Range(
            ((
                kind: Literal(Integer((
                    sign: Positive,
                    bits: 1,
                ))),
            )),
            ((
                kind: Literal(Integer((
                    sign: Positive,
                    bits: 100,
                ))),
            )),
        ),
    )";

    assert_expr_eq!("1..100", expected);
}

#[test]
fn binary_ops() {
    let fmt = |op| {
        format!(
            "(
                kind: BinaryOp((
                    lhs: ((
                        kind: Variable(((
                            key: 1,
                        ))),
                    )),
                    op: {:?},
                    rhs: ((
                        kind: Literal(Integer((
                            sign: Positive,
                            bits: 100,
                        ))),
                    )),
                )),
            )",
            op
        )
    };

    #[rustfmt::skip]
    let integers: Vec<(&str, BinaryOp)> = vec![
        ("i + 100",  BinaryOp::Add),
        ("i - 100",  BinaryOp::Sub),
        ("i * 100",  BinaryOp::Mult),
        ("i / 100",  BinaryOp::Div),
        ("i % 100",  BinaryOp::Mod),
        ("i ** 100", BinaryOp::Pow),
        ("i & 100",  BinaryOp::BitAnd),
        ("i | 100",  BinaryOp::BitOr),
        ("i ^ 100",  BinaryOp::BitXor),
        ("i >> 100", BinaryOp::Shr),
        ("i << 100", BinaryOp::Shl),
    ];

    for (src, ty) in integers {
        assert_expr_eq!(src, fmt(ty));
    }
}

#[test]
fn generics() {
    let integers: Vec<(&str, &str)> = vec![
        ("[]", "[]"),
        (
            "[str]",
            "[(data:String,loc:Concrete(span:(start:1,end:4),file:(0)))]",
        ),
        (
            "[rune]",
            "[(data:Rune,loc:Concrete(span:(start:1,end:5),file:(0)))]",
        ),
        (
            "[rune, rune]",
            "[(data:Rune,loc:Concrete(span:(start:1,end:5),file:(0))),\
            (data:Rune,loc:Concrete(span:(start:7,end:11),file:(0)))]",
        ),
        (
            "[arr[10, bool]]",
            "[(data:Array(10,(data:Boolean,loc:Concrete(span:(start:9,end:13),\
            file:(0)))),loc:Concrete(span:(start:1,end:14),file:(0)))]",
        ),
        (
            "[arr[10, bool], arr[10, bool]]",
            "[(data:Array(10,(data:Boolean,loc:Concrete(span:(start:9,end:13),\
            file:(0)))),loc:Concrete(span:(start:1,end:14),file:(0))),\
            (data:Array(10,(data:Boolean,loc:Concrete(span:(start:24,end:28),\
            file:(0)))),loc:Concrete(span:(start:16,end:29),file:(0)))]",
        ),
    ];

    for (src, expected) in integers {
        let expr = eval!(@generics &src);

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

// TODO: All the ifs
#[test]
fn if_stmt() {
    stmt_eq!(
        "if true\n    \
            println(1)\n\
        end\n"
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
fn functions_ast() {
    let expected = "Function((data:(decorators:[],attrs:[(data:Visibility(FileLocal),loc:Concrete(span:(start:9,end:19),\
        file:(0)))],name:(key:1),args:[],returns:(data:Infer,loc:Concrete(span:(start:9,end:19),file:(0))),body:[]),loc:Concrete\
        (span:(start:9,end:30),file:(0))))";
    let src = "
        fn test()
        end";
    let func = eval!(@item src);
    assert_eq!(func, expected);

    let expected = "Function((data:(decorators:[(data:(name:(data:(key:1),loc:Concrete(span:(start:10,end:16),file:(0))),args:[]),\
        loc:Concrete(span:(start:9,end:16),file:(0))),(data:(name:(data:(key:1),loc:Concrete(span:(start:26,end:32),file:(0))),args:[]),\
        loc:Concrete(span:(start:25,end:32),file:(0)))],attrs:[(data:Visibility(FileLocal),loc:Concrete(span:(start:41,end:51),file:(0)))],\
        name:(key:2),args:[],returns:(data:Infer,loc:Concrete(span:(start:41,end:51),file:(0))),body:[]),loc:Concrete(span:(start:41,end:62),\
        file:(0))))";
    let src = "
        @inline
        @inline
        fn test()
        end";
    let func = eval!(@item src);
    assert_eq!(func, expected);

    let expected = "Function((data:(decorators:[],attrs:[(data:Visibility(Exposed),loc:Concrete(span:(start:9,end:16),file:(0)))],name:(key:1),\
        args:[(data:(name:(data:(key:2),loc:Concrete(span:(start:25,end:26),file:(0))),ty:(data:ItemPath(([(key:2)])),loc:Concrete(span:(start:28,end:29),\
        file:(0))),comptime:false),loc:Concrete(span:(start:25,end:29),file:(0))),(data:(name:(data:(key:3),loc:Concrete(span:(start:31,end:32),file:(0))),\
        ty:(data:ItemPath(([(key:3)])),loc:Concrete(span:(start:34,end:35),file:(0))),comptime:false),loc:Concrete(span:(start:31,end:35),file:(0)))],returns:\
        (data:Boolean,loc:Concrete(span:(start:40,end:44),file:(0))),body:[Expression(FunctionCall(caller:Variable((key:4)),arguments:[Literal(Integer((sign:\
        Positive,bits:1)))]))]),loc:Concrete(span:(start:17,end:79),file:(0))))";
    let src = "
        exposed fn test(a: a, b: b) -> bool
            println(1)
        end";
    let func = eval!(@item src);
    assert_eq!(func, expected);
}

#[test]
fn extend_block() {
    println!(
        "{:#?}",
        format_item(
            "extend Struct
                fn test()
                    println()
                end
             end",
        )
    );
}

#[test]
fn loop_follows_if() {
    let expected = "Function((data:(decorators:[],attrs:[(data:Visibility(FileLocal),loc:Concrete(span:(start:9,end:19,),\
        file:(0),),),],name:(key:1,),args:[],returns:(data:Infer,loc:Concrete(span:(start:9,end:19,),file:(0),),),body:[If(condition:\
        Comparison(Variable((key:2,)),Equal,Literal(String(([(72),(101),(108),(108),(111),]))),),body:[Expression(FunctionCall(caller:\
        Variable((key:3,)),arguments:[Literal(String(([(89),(111),(117),(32),(115),(97),(105),(100),(32),(104),(101),(108),(108),(111),\
        ]))),],)),],clauses:[],else_clause:Some([Expression(FunctionCall(caller:Variable((key:3,)),arguments:[Literal(String(([(89),(111),\
        (117),(32),(100),(105),(100),(110),(39),(116),(32),(115),(97),(121),(32),(104),(101),(108),(108),(111),(32),(58),(40),]))),],)),]),\
        ),Loop(body:[Expression(FunctionCall(caller:Variable((key:3,)),arguments:[Literal(String(([(79),(118),(101),(114),(32),(97),(110),(100),\
        (32),(111),(118),(101),(114),(32),(97),(103),(97),(105),(110),]))),],)),],then:None,),],),loc:Concrete(span:(start:9,end:272,),file:(0),),))";
    let src = "
        fn test()
            if greeting == \"Hello\"
                println(\"You said hello\")
            else
                println(\"You didn't say hello :(\")
            end

            loop
                println(\"Over and over again\")
            end
        end";

    let func = eval!(@item src);
    assert_eq!(func, expected);
}

// TODO: Test generic functions

#[test]
fn types_ast() {
    let context = Context::new();
    let builtins = [
        (
            "i128",
            Type::Integer {
                sign: Signedness::Signed,
                width: 128,
            },
        ),
        (
            "u128",
            Type::Integer {
                sign: Signedness::Unsigned,
                width: 128,
            },
        ),
        (
            "u1",
            Type::Integer {
                sign: Signedness::Unsigned,
                width: 1,
            },
        ),
        (
            "i65535",
            Type::Integer {
                sign: Signedness::Signed,
                width: u16::max_value(),
            },
        ),
        (
            "u65535",
            Type::Integer {
                sign: Signedness::Unsigned,
                width: u16::max_value(),
            },
        ),
        (
            "f65535",
            Type::Float {
                width: u16::max_value(),
            },
        ),
        (
            "i7453",
            Type::Integer {
                sign: Signedness::Signed,
                width: 7453,
            },
        ),
        ("f32", Type::Float { width: 32 }),
        ("f1", Type::Float { width: 1 }),
        ("bool", Type::Bool),
        ("unit", Type::Unit),
        ("str", Type::String),
        ("absurd", Type::Absurd),
        ("uptr", Type::IntPtr(Signedness::Unsigned)),
        ("iptr", Type::IntPtr(Signedness::Signed)),
        ("ureg", Type::IntReg(Signedness::Unsigned)),
        ("ireg", Type::IntReg(Signedness::Signed)),
        ("arr[100, infer]", Type::Array(100, Ref::new(Type::Infer))),
        ("arr[1, str]", Type::Array(1, Ref::new(Type::String))),
        (
            "tup[str, arr[5, i32]]",
            Type::Tuple(vec![
                Type::String,
                Type::Array(
                    5,
                    Ref::new(Type::Integer {
                        sign: Signedness::Signed,
                        width: 32,
                    }),
                ),
            ]),
        ),
        (
            "CustomThingy",
            Type::ItemPath(ItemPath::new(vec![StrT::new(0)])),
        ),
        (
            "Custom.Thingy.Pathed",
            Type::ItemPath(ItemPath::new(vec![
                StrT::new(0),
                StrT::new(1),
                StrT::new(2),
            ])),
        ),
    ];

    for (src, expected) in builtins.iter() {
        let ty = Parser::new(src, CurrentFile::new(FileId(0), src.len()), context.clone())
            .ascribed_type()
            .unwrap();

        assert_eq!(&ty, expected,);
    }
}

#[test]
fn type_ast() {
    assert_eq!(
        "@inline\n@builtin\nexposed type test[A, B, C, D]\n    member: infer\n    @builtin\n    exposed another_member: bool\nend\n",
        format_item("@inline\n@builtin\nexposed type test[A, B, C, D]\nmember\n@builtin\nexposed another_member: bool\nend"),
    );
}

#[test]
fn enum_ast() {
    assert_eq!(
        "@derive(Debug)\npkg enum testme[A]\n    @no_construct\n    UnitType\n    TupleType(bool, int, tuplething)\nend\n",
        format_item("@derive(Debug)\npkg enum testme[A]\n@no_construct\nUnitType\nTupleType(bool, int, tuplething)\nend"),
    );
}

#[test]
fn trait_ast() {
    assert_eq!(
        "@traits\npkg trait Test[Classy]\n    @something(10)\n    fn be_classy() -> bool\n    end\nend\n",
        format_item(
            "@traits pkg trait Test[Classy]\n@something(10)\nfn be_classy() -> bool\nend\nend"
        ),
    );
}

#[test]
fn import_ast() {
    assert_eq!(
        "import \"std.tests\" as tests\n",
        format_item("import \"std.tests\"\n")
    );

    assert_eq!(
        "import \"std.tests\" as tester\n",
        format_item("import \"std.tests\" as tester\n"),
    );

    assert_eq!(
        "import \"std.tests\" exposing *\n",
        format_item("import \"std.tests\" exposing *\n"),
    );

    assert_eq!(
        "import \"std.tests\" exposing Okthing, OkthingElse\n",
        format_item("import \"std.tests\" exposing Okthing, OkthingElse\n"),
    );

    assert_eq!(
        "import \"std.tests\" exposing Okthing as Weird, OkthingElse as Weirder\n",
        format_item("import \"std.tests\" exposing Okthing as Weird, OkthingElse as Weirder\n"),
    );
}

#[test]
fn non_ascii_idents() {
    ast_eq!("fn ȨʩӺޗઈဪ⏫㡘㢡䔹✅✨❗❓() -> infer\n    empty\nend\n");
}

#[cfg(not(any(target_arch = "wasm32", miri)))]
mod proptests {
    use super::*;
    use proptest::prelude::*;

    proptest! {
        #[test]
        fn strings(s in r#"b?"(\\.|[^\\"])*""#) {
            let context = Context::default();
            let mut parser = Parser::new(&s, CurrentFile::new(FileId(0), s.len()), context);

            let expr = parser.expr().map(|e| e.kind);
            match expr {
                Ok(ExprKind::Literal(Locatable { data: Literal::String(..), .. })) | Ok(ExprKind::Literal(Locatable { data: Literal::Array(..), .. })) => {},

                Err(Locatable { data: _data @ Error::Syntax(SyntaxError::InvalidEscapeCharacters(..)), .. })
                | Err(Locatable { data: _data @ Error::Syntax(SyntaxError::UnrecognizedEscapeSeq(..)), .. })
                | Err(Locatable { data: _data @ Error::Syntax(SyntaxError::MissingEscapeBraces), .. })
                | Err(Locatable { data: _data @ Error::Syntax(SyntaxError::MissingEscapeSpecifier), .. }) => {}

                _ => prop_assert!(false),
            }
        }

        #[test]
        fn runes(s in "b?'[^']*'") {
            let context = Context::default();
            let mut parser = Parser::new(&s, CurrentFile::new(FileId(0), s.len()), context);

            let expr = parser.expr().map(|e| e.kind);
            match expr {
                Ok(ExprKind::Literal(Locatable { data: Literal::Rune(..), .. })) | Ok(ExprKind::Literal(Locatable { data: Literal::Integer(..), .. })) => {},

                Err(Locatable { data: _data @ Error::Syntax(SyntaxError::TooManyRunes), .. })
                | Err(Locatable { data: _data @ Error::Syntax(SyntaxError::UnrecognizedEscapeSeq(..)), .. })
                | Err(Locatable { data: _data @ Error::Syntax(SyntaxError::MissingEscapeBraces), .. })
                | Err(Locatable { data: _data @ Error::Syntax(SyntaxError::MissingEscapeSpecifier), .. }) => {}

                _ => prop_assert!(false),
            }
        }

        #[test]
        fn base10_int(s in "[+-]?[0-9][0-9_]*") {
            let context = Context::default();
            let mut parser = Parser::new(&s, CurrentFile::new(FileId(0), s.len()), context);

            let expr = parser.expr().map(|e| e.kind);
            let cond = matches!(expr, Ok(ExprKind::Literal(Locatable { data: Literal::Integer { .. }, .. })));
            prop_assert!(cond);
        }

        #[test]
        fn base16_int(s in "[+-]?0x[0-9a-fA-F][0-9a-fA-F_]*") {
            let context = Context::default();
            let mut parser = Parser::new(&s, CurrentFile::new(FileId(0), s.len()), context);

            let expr = parser.expr().map(|e| e.kind);
            let cond = matches!(expr, Ok(ExprKind::Literal(Locatable { data: Literal::Integer(Integer { .. }), .. }))
                | Err(Locatable { data: Error::Syntax(SyntaxError::LiteralOverflow(..)), .. }));
            prop_assert!(cond);
        }

        #[test]
        fn base2_int(s in "[+-]?0b[0-1][0-1_]*") {
            let context = Context::default();
            let mut parser = Parser::new(&s, CurrentFile::new(FileId(0), s.len()), context);

            let expr = parser.expr().map(|e| e.kind);
            let cond = matches!(expr, Ok(ExprKind::Literal(Locatable { data: Literal::Integer(Integer { .. }), .. }))
                | Err(Locatable { data: Error::Syntax(SyntaxError::LiteralOverflow(..)), .. }));
            prop_assert!(cond);
        }

        #[test]
        fn base10_float(s in "[+-]?[0-9][0-9_]*\\.[0-9][0-9_]*([eE][+-]?[0-9][0-9_]*)?") {
            let context = Context::default();
            let mut parser = Parser::new(&s, CurrentFile::new(FileId(0), s.len()), context);

            let expr = parser.expr().map(|e| e.kind);
            let cond = matches!(expr, Ok(ExprKind::Literal(Locatable { data: Literal::Float(..), .. }))
                | Err(Locatable { data: Error::Syntax(SyntaxError::LiteralOverflow(..)), .. }));
            prop_assert!(cond);
        }

        #[test]
        fn base16_float(s in "[+-]?0x[0-9a-fA-F][0-9a-fA-F_]*\\.[0-9a-fA-F][0-9a-fA-F_]*([pP][+-]?[0-9][0-9_]?)?") {
            let context = Context::default();
            let mut parser = Parser::new(&s, CurrentFile::new(FileId(0), s.len()), context);

            let expr = parser.expr().map(|e| e.kind);
            let cond = matches!(expr, Ok(ExprKind::Literal(Locatable { data: Literal::Float(..), .. }))
                | Err(Locatable { data: Error::Syntax(SyntaxError::LiteralOverflow(..)), .. }));
            prop_assert!(cond);
        }
    }
}
