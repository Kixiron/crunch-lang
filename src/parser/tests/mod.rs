mod proptests;

use super::*;

// TODO: Actual parser testing

#[test]
#[ignore]
fn parse_test() {
    const CODE: &str = include_str!("../../../tests/parse_test.crunch");
    const FILENAME: &str = "parse_test.crunch";

    color_backtrace::install();
    // simple_logger::init().unwrap();

    let mut parser = Parser::new(Some(FILENAME), CODE);

    let ast = parser.parse().unwrap();
    println!("Ast: {:#?}", &ast);

    let bytecode = crate::interpreter::Interpreter::from_interner(
        &crate::OptionBuilder::new("./tests/parse_test.crunch").build(),
        parser.interner,
    )
    .interpret(ast.0.clone())
    .unwrap();
    println!("Bytecode: {:?}", &bytecode);

    crate::Vm::default().execute(&bytecode).unwrap();
}

#[test]
fn fibonacci_iterative_test() {
    const CODE: &str = include_str!("../../../tests/fibonacci_iterative.crunch");
    const FILENAME: &str = "fibonacci_iterative.crunch";

    // color_backtrace::install();
    // simple_logger::init().unwrap();

    let mut parser = Parser::new(Some(FILENAME), CODE);

    let ast = parser.parse().unwrap();

    let bytecode = crate::interpreter::Interpreter::from_interner(
        &crate::OptionBuilder::new("./tests/fibonacci_iterative.crunch").build(),
        parser.interner,
    )
    .interpret(ast.0.clone())
    .unwrap();

    crate::Vm::default().execute(&bytecode).unwrap();
}

#[test]
fn factorial_iterative_test() {
    const CODE: &str = include_str!("../../../tests/factorial_iterative.crunch");
    const FILENAME: &str = "factorial_iterative.crunch";

    // color_backtrace::install();
    simple_logger::init().unwrap();

    let mut parser = Parser::new(Some(FILENAME), CODE);

    let ast = parser.parse().unwrap();

    let bytecode = crate::interpreter::Interpreter::from_interner(
        &crate::OptionBuilder::new("./tests/factorial_iterative.crunch").build(),
        parser.interner,
    )
    .interpret(ast.0.clone())
    .unwrap();

    crate::Vm::default().execute(&bytecode).unwrap();
}

#[test]
fn fibonacci_recursive_test() {
    const CODE: &str = include_str!("../../../tests/fibonacci_recursive.crunch");
    const FILENAME: &str = "fibonacci_recursive.crunch";

    // color_backtrace::install();
    // simple_logger::init().unwrap();

    let mut parser = Parser::new(Some(FILENAME), CODE);

    let ast = parser.parse().unwrap();

    let bytecode = crate::interpreter::Interpreter::from_interner(
        &crate::OptionBuilder::new("./test/fibonacci_recursive.crunch").build(),
        parser.interner,
    )
    .interpret(ast.0.clone())
    .unwrap();

    crate::Vm::default().execute(&bytecode).unwrap();
}

#[test]
fn factorial_recursive_test() {
    const CODE: &str = include_str!("../../../tests/factorial_recursive.crunch");
    const FILENAME: &str = "factorial_recursive.crunch";

    // color_backtrace::install();
    // simple_logger::init().unwrap();

    let mut parser = Parser::new(Some(FILENAME), CODE);

    let ast = parser.parse().unwrap();

    let bytecode = crate::interpreter::Interpreter::from_interner(
        &crate::OptionBuilder::new("./tests/factorial_recursive.crunch").build(),
        parser.interner,
    )
    .interpret(ast.0.clone())
    .unwrap();

    crate::Vm::default().execute(&bytecode).unwrap();
}

#[test]
#[ignore]
fn ffi_test() {
    use crate::{Instruction::*, Value};

    color_backtrace::install();
    simple_logger::init().unwrap();

    let bytecode = vec![vec![
        Load(Value::Str("ffi_test.dll"), 0.into()),
        LoadLib(0.into(), 0.into()),
        Load(Value::Str("add"), 1.into()),
        Load(Value::I32(10), 2.into()),
        Load(Value::I32(20), 3.into()),
        Push(2.into()),
        Push(3.into()),
        ExecLibFunc(1.into(), 0.into(), 2),
        Pop(2.into()),
        Print(2.into()),
        Return,
    ]];

    crate::Vm::default().execute(&bytecode).unwrap();
}
