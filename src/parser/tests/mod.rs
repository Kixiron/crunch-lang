mod proptests;

use super::*;

// TODO: Actual parser testing

#[test]
#[ignore]
fn parse_test() {
    const CODE: &str = include_str!("../../../tests/parse_test.crunch");
    const FILENAME: &str = "parse_test.crunch";

    color_backtrace::install();
    simple_logger::init().unwrap();

    let mut parser = Parser::new(Some(FILENAME), CODE);

    let ast = parser.parse().unwrap();
    println!("Ast: {:#?}", &ast);

    let bytecode = crate::interpreter::Interpreter::from_interner(
        &crate::OptionBuilder::new("./examples/parse_test.crunch").build(),
        parser.interner,
    )
    .interpret(ast.0.clone())
    .unwrap();
    println!("Bytecode: {:?}", &bytecode);

    crate::Vm::default().execute(&bytecode).unwrap();
}

#[test]
fn fibonacci_test() {
    const CODE: &str = include_str!("../../../tests/fibonacci.crunch");
    const FILENAME: &str = "fibonacci.crunch";

    // color_backtrace::install();
    // simple_logger::init().unwrap();

    let mut parser = Parser::new(Some(FILENAME), CODE);

    let ast = parser.parse().unwrap();
    println!("Ast: {:#?}", &ast);

    let bytecode = crate::interpreter::Interpreter::from_interner(
        &crate::OptionBuilder::new("./examples/fibonacci.crunch").build(),
        parser.interner,
    )
    .interpret(ast.0.clone())
    .unwrap();
    println!("Bytecode: {:?}", &bytecode);

    crate::Vm::default().execute(&bytecode).unwrap();
}

#[test]
fn factorial_test() {
    const CODE: &str = include_str!("../../../tests/factorial.crunch");
    const FILENAME: &str = "factorial.crunch";

    color_backtrace::install();
    simple_logger::init().unwrap();

    let mut parser = Parser::new(Some(FILENAME), CODE);

    let ast = parser.parse().unwrap();
    println!("Ast: {:#?}", &ast);

    let bytecode = crate::interpreter::Interpreter::from_interner(
        &crate::OptionBuilder::new("./examples/factorial.crunch").build(),
        parser.interner,
    )
    .interpret(ast.0.clone())
    .unwrap();
    println!("Bytecode: {:?}", &bytecode);

    crate::Vm::default().execute(&bytecode).unwrap();
}

#[test]
#[ignore]
fn ffi_test() {
    use crate::{Instruction::*, RuntimeValue};

    color_backtrace::install();
    simple_logger::init().unwrap();

    let bytecode = vec![vec![
        Load(RuntimeValue::Str("ffi_test.dll"), 0.into()),
        LoadLib(0.into(), 0.into()),
        Load(RuntimeValue::Str("add"), 1.into()),
        Load(RuntimeValue::I32(10), 2.into()),
        Load(RuntimeValue::I32(20), 3.into()),
        Push(2.into()),
        Push(3.into()),
        ExecLibFunc(1.into(), 0.into(), 2),
        Pop(2.into()),
        Print(2.into()),
        Return,
    ]];

    crate::Vm::default().execute(&bytecode).unwrap();
}
