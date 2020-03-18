use crate::{Vice, ViceOptions};
use compactor::Compactor;
use crunch_parser::Parser;

use alloc::{boxed::Box, vec::Vec};

#[test]
fn hello_world() {
    let mut output = Vec::new();
    let mut compactor = Compactor::with_stdout(Box::new(&mut output));

    let hello_world = "
        fn main()
            println('Hello, world!')
        end
    ";
    let mut parser = Parser::new(None, hello_world);
    let ast = parser.parse().unwrap().0;
    let program = Vice::from_interner(ViceOptions::default(), parser.interner)
        .compile(ast)
        .unwrap();

    compactor.execute(&program).unwrap();

    assert_eq!(output, b"Hello, world!\n");
}

// FIXME: Panics at 'index out of bounds', codegen error
// Message: thread 'vice::tests::fibonacci_iterative' panicked at 'index out of bounds: the len is 38 but the index is 38'
#[test]
fn fibonacci_iterative() {
    let mut output = Vec::new();
    let mut compactor = Compactor::with_stdout(Box::new(&mut output));

    let fibonacci = "
        fn main()
            println(fibonacci(1))
            println(fibonacci(10))
            println(fibonacci(20))
        end

        fn fibonacci(n: int) -> int
            if n == 0
                return 0
            else if n == 1
                return 1
            else
                let a = 0
                let b = 1

                for i in 0..n
                    let c = b
                    a = c
                    b = a + c
                end

                return a
            end
        end
    ";
    let mut parser = Parser::new(None, fibonacci);
    let ast = parser.parse().unwrap().0;
    let program = Vice::from_interner(ViceOptions::default(), parser.interner)
        .compile(ast)
        .unwrap();

    compactor.execute(&program).unwrap();

    assert_eq!(output, b"Hello, world!\n");
}

#[test]
fn range_test() {
    simple_logger::init().unwrap();

    let mut output = Vec::new();
    let mut compactor = Compactor::with_stdout(Box::new(&mut output));

    let fibonacci = "
        fn main()
            for i in 0..10
                println(i)
            end
        end
    ";
    let mut parser = Parser::new(None, fibonacci);
    let ast = parser.parse().unwrap().0;
    let program = Vice::from_interner(ViceOptions::default(), parser.interner)
        .compile(ast)
        .unwrap();

    compactor.execute(&program).unwrap();

    println!("{}", std::str::from_utf8(&output).unwrap());
    assert_eq!(output, b"0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n");
}
