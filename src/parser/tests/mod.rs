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
        &crate::OptionBuilder::new("./examples/parse_test.crunch").build(),
        parser.interner,
    )
    .interpret(ast.0.clone())
    .unwrap();
    println!("Bytecode: {:?}", &bytecode);

    crate::Vm::default().execute(bytecode).unwrap();
}
