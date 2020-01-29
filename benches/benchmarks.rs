use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn garbage_collection(c: &mut Criterion) {
    use crunch::*;

    c.bench_function("GC Startup", |b| {
        let options = OptionBuilder::new("./gc_startup")
            .heap_size(1024 * 1024 * 50)
            .build();

        b.iter(|| {
            black_box(Gc::new(&options));
        });
    })
    .bench_function("GC Collect 10000 usizes", |b| {
        let mut gc = Gc::new(
            &OptionBuilder::new("./alloc_10000_usizes")
                .heap_size(std::mem::size_of::<usize>() * 10_000)
                .build(),
        );

        for i in 0..10_000usize {
            i.alloc(&mut gc).unwrap();
        }

        b.iter(|| {
            gc.collect();
        });
    })
    .bench_function("GC Allocate and Collect 10000 usizes exact heap", |b| {
        let mut gc = Gc::new(
            &OptionBuilder::new("./alloc_10000_usizes")
                .heap_size(std::mem::size_of::<usize>() * 10_000)
                .build(),
        );

        b.iter(|| {
            for i in 0..10_000usize {
                i.alloc(&mut gc).unwrap();
            }

            gc.collect();
        });
    })
    .bench_function("GC Allocate 10000 usizes exact heap", |b| {
        let mut gc = Gc::new(
            &OptionBuilder::new("./alloc_10000_usizes")
                .heap_size(std::mem::size_of::<usize>() * 10_000)
                .build(),
        );

        b.iter(|| {
            for i in 0..10_000usize {
                i.alloc(&mut gc).unwrap();
            }
        });

        gc.collect();
    })
    .bench_function(
        "GC Allocate and Collect 10000 usizes constrained heap",
        |b| {
            let mut gc = Gc::new(
                &OptionBuilder::new("./alloc_10000_usizes")
                    .heap_size(1024 * 1024 * 2)
                    .build(),
            );

            b.iter(|| {
                for i in 0..10_000usize {
                    i.alloc(&mut gc).unwrap();
                }

                gc.collect();
            });
        },
    )
    .bench_function("GC Allocate 10000 usizes constrained heap", |b| {
        let mut gc = Gc::new(
            &OptionBuilder::new("./alloc_10000_usizes")
                .heap_size(1024 * 1024 * 2)
                .build(),
        );

        b.iter(|| {
            for i in 0..10_000usize {
                i.alloc(&mut gc).unwrap();
            }
        });

        gc.collect();
    })
    .bench_function("GC fetch usize", |b| {
        let mut gc = Gc::new(
            &OptionBuilder::new("./alloc_10000_usizes")
                .heap_size(1024 * 1024 * 2)
                .build(),
        );

        let id = 100usize.alloc(&mut gc).unwrap();

        b.iter(|| {
            let _: usize = id.fetch(&gc).unwrap();
        });

        gc.collect();
    });
}

fn vm(c: &mut Criterion) {
    c.bench_function("VM startup", |b| {
        b.iter(|| black_box(crunch::Vm::default()));
    });
}

fn examples(c: &mut Criterion) {
    use crunch::*;

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
        });
    // .bench_function("C", |b| {
    //     cc::Build::new().file("fibonacci.c").compile("fibonacci");
    //
    //     #[link(name = "fibonacci")]
    //     extern "C" {
    //         fn fibonacci(n: i32) -> i32;
    //     }
    //
    //     b.iter(|| unsafe { black_box(fibonacci(20)) });
    // });
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
        });
    // .bench_function("C", |b| {
    //     cc::Build::new().file("factorial.c").compile("factorial");
    //
    //     #[link(name = "factorial")]
    //     extern "C" {
    //         fn factorial(n: i32) -> i32;
    //     }
    //
    //     b.iter(|| unsafe { black_box(factorial(20)) });
    // })
    group.finish();
}

criterion_group! {
    name = benches;
    config = Criterion::default();
    targets = garbage_collection, examples, vm
}
criterion_main!(benches);
