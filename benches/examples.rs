use criterion::{black_box, criterion_group, criterion_main, Criterion};
use crunch::*;

fn examples(c: &mut Criterion) {
    let mut group = c.benchmark_group("Fibonacci");
    group
        .bench_function("Crunch", |b| {
            let mut parser = Parser::new(
                Some("fibonacci.crunch"),
                "
    fn main()
        fibonacci(20)
    end
    
    fn fibonacci(n: int) -> int
        if n < 2
            return 1
        else
            return fibonacci(n - 1) + fibonacci(n - 2)
        end
    end
    ",
            );
            let ast = parser.parse().unwrap();
            let bytecode = Interpreter::from_interner(
                &OptionBuilder::new("./examples/fibonacci.crunch").build(),
                parser.interner,
            )
            .interpret(ast.0)
            .unwrap();
            let mut vm = Vm::default();

            b.iter(|| {
                black_box(vm.execute(&bytecode)).unwrap();
            });
        })
        .bench_function("Rust", |b| {
            fn fibonacci(n: u32) -> u32 {
                if n < 2 {
                    0
                } else {
                    fibonacci(n - 1) + fibonacci(n - 2)
                }
            }

            b.iter(|| black_box(fibonacci(black_box(20))));
        })
        .bench_function("C", |b| {
            extern "C" {
                fn fibonacci(n: i32) -> i32;
            }

            b.iter(|| unsafe { black_box(fibonacci(20)) });
        });
    group.finish();

    let mut group = c.benchmark_group("Factorial");
    group
        .bench_function("Crunch", |b| {
            let mut parser = Parser::new(
                Some("factorial.crunch"),
                "
    fn main()
        factorial(20)
    end
    
    fn factorial(n: int) -> int
        if n < 2
            return 1
        else
            return n * factorial(n - 1)
        end
    end
    ",
            );
            let ast = parser.parse().unwrap();
            let bytecode = Interpreter::from_interner(
                &OptionBuilder::new("./examples/factorial.crunch").build(),
                parser.interner,
            )
            .interpret(ast.0)
            .unwrap();
            let mut vm = Vm::default();

            b.iter(|| {
                black_box(vm.execute(&bytecode)).unwrap();
            });
        })
        .bench_function("Rust", |b| {
            fn factorial(n: u32) -> u32 {
                if n < 2 {
                    1
                } else {
                    n * factorial(n - 1)
                }
            }

            b.iter(|| black_box(factorial(black_box(20))));
        })
        .bench_function("C", |b| {
            extern "C" {
                fn factorial(n: i32) -> i32;
            }

            b.iter(|| unsafe { black_box(factorial(20)) });
        });
    group.finish();
}

criterion_group! {
    name = benches;
    config = Criterion::default();
    targets = examples
}
criterion_main!(benches);
