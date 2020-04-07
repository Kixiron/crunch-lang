use super::*;
use crate::{files::FileId, pretty_printer::PrettyPrinter};

use lasso::{Key, SmallSpur};

#[cfg(feature = "no-std")]
use alloc::{boxed::Box, string::String};

fn format_expr(source: &str) -> String {
    let interner = Interner::default();
    let mut parser = Parser::new(source, FileId::new(0), interner);
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
    let mut parser = Parser::new(source, FileId::new(0), interner);
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
    let mut parser = Parser::new(source, FileId::new(0), interner);
    let stmt = parser.ast().unwrap().unwrap();

    let interner = core::mem::take(&mut parser.string_interner);
    let mut string = String::new();
    PrettyPrinter::new(interner)
        .print_ast(&mut string, &stmt)
        .unwrap();

    string
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
    stmt_eq!("let variable = true\n");
}

#[test]
fn long_func_decl_stmt() {
    stmt_eq!("let jkpwn = vssekgmbxoxshmhinx(jnldfzbd, kcqpq, gbuaqbax, argro, xhmfc, bredcp, pwlfywfkb, vgsjjcy, exomcmbf, cjsjpvgcl, omtlfpw, ssdrm, kxrtaun, xexzz, ejvmxj, ssmqkbqqi)\n");
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

// TODO: '''Test''' and """Test"""
// TODO: Testing is real sketchy here
#[test]
fn strings() {
    assert_eq!(r#""Test""#, format_expr("'Test'"));
    expr_eq!(r#""Test""#);

    assert_eq!(format!("{:?}", b"Test"), format_expr("b'Test'"));
    assert_eq!(format!("{:?}", b"Test"), format_expr(r#"b"Test""#));

    assert_eq!(
        "\" \\ \n \r \t \" \u{2764} \"",
        format_expr(r#"' \\ \n \r \t \" \u{2764} '"#)
    );
    assert_eq!("\" \\ \n \r \t \" \"", format_expr(r#"" \\ \n \r \t \" ""#));

    assert_eq!(
        format!("{:?}", b" \\ \n \r \t \" "),
        format_expr(r#"b' \\ \n \r \t \" '"#)
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
        "exposed fn test<T, E>(a: infer, a: b) -> bool\n    println(1)\nend\n",
        format_ast("exposed fn test<T, E>(a, a: b) -> bool\n    println(1)\nend\n"),
    );
}

#[test]
fn types_ast() {
    let mut interner = Interner::default();

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
            Type::try_new((*token, FileId::new(0), &mut interner))
                .ok()
                .map(|(ty, _)| ty),
            Some(ty.clone()),
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
        interner.intern(token.source());

        assert_eq!(
            Type::try_new((*token, FileId::new(0), &mut interner))
                .ok()
                .map(|(ty, _)| ty),
            Some(Type::Custom(SmallSpur::try_from_usize(idx).unwrap())),
        );
    }
}

#[test]
fn type_ast() {
    assert_eq!(
        "@inline\n@builtin\nexposed type test<A, B, C, D>\n    member: infer\n    @builtin\n    exposed another_member: bool\n    fn test() -> infer\n    end\nend\n",
        format_ast("@inline\n@builtin\nexposed type test<A, B, C, D>\nmember\n@builtin\nexposed another_member: bool\nfn test()\nend\nend"),
    );
}

#[test]
fn enum_ast() {
    assert_eq!(
        "@derive(Debug)\npkg enum testme<A>\n    @no_construct\n    UnitType\n    TupleType(bool, int, tuplething)\nend\n",
        format_ast("@derive(Debug)\npkg enum testme<A>\n@no_construct\nUnitType\nTupleType(bool, int, tuplething)\nend"),
    );
}

#[test]
fn trait_ast() {
    assert_eq!(
        "@traits\npkg trait Test<Classy>\n    @something(10)\n    fn be_classy() -> bool\n    end\nend\n",
        format_ast(
            "@traits pkg trait Test<Classy>\n@something(10)\nfn be_classy() -> bool\nend\nend"
        ),
    );
}

#[test]
fn import_ast() {
    assert_eq!(
        "import \"std.tests\" as tests\n",
        format_ast("import 'std.tests'\n")
    );

    assert_eq!(
        "import \"std.tests\" as tester\n",
        format_ast("import 'std.tests' as tester\n"),
    );

    assert_eq!(
        "import \"std.tests\" exposing *\n",
        format_ast("import 'std.tests' exposing *\n"),
    );

    assert_eq!(
        "import \"std.tests\" exposing Okthing, OkthingElse\n",
        format_ast("import 'std.tests' exposing Okthing, OkthingElse\n"),
    );

    assert_eq!(
        "import \"std.tests\" exposing Okthing as Weird, OkthingElse as Weirder\n",
        format_ast("import 'std.tests' exposing Okthing as Weird, OkthingElse as Weirder\n"),
    );
}
