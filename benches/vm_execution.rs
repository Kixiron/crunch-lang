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
    });
}

criterion_group!(benches, instructions);
criterion_main!(benches);
