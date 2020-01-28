use criterion::{criterion_group, criterion_main, Criterion};

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
        const CODE: &str = include_str!("../tests/fibonacci.crunch");
        const FILENAME: &str = "fibonacci.crunch";

        let mut parser = Parser::new(Some(FILENAME), CODE);
        let ast = parser.parse().unwrap();
        let bytecode = crunch::Interpreter::from_interner(
            &crunch::OptionBuilder::new("./examples/fibonacci.crunch").build(),
            parser.interner,
        )
        .interpret(ast.0)
        .unwrap();

        b.iter(|| {
            crunch::Vm::default().execute(bytecode.clone()).unwrap();
        });
    });
}

criterion_group!(benches, instructions);
criterion_main!(benches);
