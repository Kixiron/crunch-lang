use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn instructions(c: &mut Criterion) {
    use crunch::*;

    c.bench_function("GC Collect 10000 usizes", |b| {
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
    })
    .bench_function("Fibonacci in Crunch", |b| {
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
        let bytecode = crunch::Interpreter::from_interner(
            &crunch::OptionBuilder::new("./examples/fibonacci.crunch").build(),
            parser.interner,
        )
        .interpret(ast.0)
        .unwrap();
        let mut vm = crunch::Vm::default();

        b.iter(|| {
            black_box(vm.execute(&bytecode)).unwrap();
        });
    })
    .bench_function("Fibonacci in Rust", |b| {
        fn fibonacci(n: u32) -> u32 {
            if n < 2 {
                0
            } else {
                fibonacci(n - 1) + fibonacci(n - 2)
            }
        }

        b.iter(|| black_box(fibonacci(black_box(20))));
    });
}

criterion_group!(benches, instructions);
criterion_main!(benches);
