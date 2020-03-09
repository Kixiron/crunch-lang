use criterion::{black_box, criterion_group, criterion_main, Criterion};
use crunch_parser::Parser;
use vice::{Vice, ViceOptions};

fn compilation(c: &mut Criterion) {
    c.bench_function("Parse Fibonacci", |b| {
        b.iter(|| {
            black_box(Parser::new(
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
            ))
            .parse()
            .unwrap();
        });
    })
    .bench_function("Compile Fibonacci", |b| {
        let mut parser = black_box(Parser::new(
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
        ));
        let ast = parser.parse().unwrap();

        b.iter(|| {
            Vice::from_interner(ViceOptions::default(), parser.interner.clone())
                .compile(ast.0.clone())
        });
    })
    .bench_function("Parse Factorial", |b| {
        b.iter(|| {
            black_box(Parser::new(
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
            ))
            .parse()
            .unwrap();
        });
    })
    .bench_function("Compile Factorial", |b| {
        let mut parser = black_box(Parser::new(
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
        ));
        let ast = parser.parse().unwrap();

        b.iter(|| {
            Vice::from_interner(ViceOptions::default(), parser.interner.clone())
                .compile(ast.0.clone())
        });
    });
}

criterion_group! {
    name = benches;
    config = Criterion::default();
    targets = compilation
}
criterion_main!(benches);
