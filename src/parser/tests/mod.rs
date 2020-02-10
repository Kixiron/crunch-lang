mod proptests;

use super::*;

// TODO: Actual parser testing

#[test]
#[ignore]
fn parse_test() {
    const CODE: &str = include_str!("../../../tests/parse_test.crunch");
    const FILENAME: &str = "parse_test.crunch";

    // color_backtrace::install();
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
    simple_logger::init().unwrap();

    let mut parser = Parser::new(Some(FILENAME), CODE);

    let ast = parser.parse();
    if let Err(err) = ast {
        let writer = codespan_reporting::term::termcolor::StandardStream::stderr(
            codespan_reporting::term::termcolor::ColorChoice::Auto,
        );

        let config = codespan_reporting::term::Config::default();

        let mut files = codespan::Files::new();
        files.add(FILENAME, CODE);

        for e in err {
            if let Err(err) =
                codespan_reporting::term::emit(&mut writer.lock(), &config, &files, &e)
            {
                println!("Error Emitting Error: {:?}", err);
            }
        }

        panic!();
    }
    let ast = ast.unwrap();

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
    // simple_logger::init().unwrap();

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

#[allow(non_snake_case)]
mod fuzz_found {
    use super::*;

    // Note: All test names here are MD5 hashes of the panicking input preceded by an underscore

    #[test]
    fn _C6CBD54946E2A1D183EAA7D86241656F() {
        let input = "fn main()
            i +=
        end";

        let _ = Parser::new(None, input).parse();
    }

    #[test]
    fn _6B58087CB2578DD9C74F702A574A4C91() {
        let input = "fn main()
            let i = (1 + (100 / (1 * 10293207277133";

        let _ = Parser::new(None, input).parse();
    }

    #[test]
    fn _307818184C53B8C3412B778AF5B836F8() {
        let input = "fn main()
            println('Test\")
        end";

        let _ = Parser::new(None, input).parse();
    }

    #[test]
    fn _9D1BE77EA5DDA279F6679AD75A7213AF() {
        simple_logger::init().unwrap();

        let input = "fn main()
            println(factorial(1))
            println(factorial(10))
            println(factorial(20))
        end
        
        fn factorial(n: int) -> int
            let product = 1
        
            for i in 1..n
                product *= i
            end
        
            return product
        end";

        let _ = Parser::new(None, input).parse();
    }

    #[test]
    fn _FE605BD31796CB1A1A4A882487967B90() {
        let input = "fn main()614^8154291434688Ʀ \\ (1))
            pri -30076509";

        let _ = Parser::new(None, input).parse();
    }
    #[test]
    fn _46439F3F511989ABB1A794D5CD34F5C4() {
        let input = "\x66\x6E\x20\x6D\x61\x69\x6E\x28\x29\x0A\x20\x20\x13\x20\x65\x6D\x70\x74\x79\x0A\x65\x6E\x64";

        let _ = Parser::new(None, input).parse();
    }

    #[test]
    fn _A5DCA4A77C31DDDD5E03730B3D962718() {
        let input = "fn main()
            println(fibonacci-473222347563415634756341563475634156994onacci(10))
            println(fibonacci(20))
        end
        
        fn fibonacci(n: int) -> int
            if n == 0
                return 0
            else if n5311160814823956516n 1
            else
                let a = 0
                let b = 1
        
                for i in 0..n
                    let c = b
                    a = c
                    b = a + c
                end
        
                return a";

        let _ = Parser::new(None, input).parse();
    }

    #[test]
    fn _56C81E044729298317C09A2D7112A555() {
        let input = "fn majn()
            printnn(fibonacci(1))
            println(fibonacci(10))
            println(fibonacci(20))
        end

        fn fibonacci(n: int) -> int
            if n <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<4<<<<<<<<<<<<<<<<<onaccj)n - 2)
            end
        end";

        let _ = Parser::new(None, input).parse();
    }
}
