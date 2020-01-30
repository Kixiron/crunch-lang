use criterion::{black_box, criterion_group, criterion_main, Criterion};
use crunch::*;
use std::alloc;

struct DeallocDrop(*mut u8, alloc::Layout);

impl Drop for DeallocDrop {
    fn drop(&mut self) {
        unsafe {
            alloc::dealloc(self.0, self.1);
        }
    }
}

fn garbage_collection(c: &mut Criterion) {
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

    let mut group = c.benchmark_group("Allocation Types");

    group
        .bench_function("Large Allocation", |b| {
            let heap_size = 1024 * 1024 * 2;
            let page_size = page_size();
            let layout = alloc::Layout::from_size_align(heap_size * 2, page_size)
                .expect("Failed to create GC memory block layout");

            b.iter_with_large_drop(|| {
                DeallocDrop(unsafe { black_box(alloc::alloc_zeroed(layout)) }, layout);
            });
        })
        .bench_function("Dual Allocation", |b| {
            let heap_size = 1024 * 1024 * 2;
            let page_size = page_size();
            let layout = alloc::Layout::from_size_align(heap_size, page_size)
                .expect("Failed to create GC memory block layout");

            b.iter_with_large_drop(|| {
                DeallocDrop(unsafe { black_box(alloc::alloc_zeroed(layout)) }, layout);
                DeallocDrop(unsafe { black_box(alloc::alloc_zeroed(layout)) }, layout);
            });
        });
    group.finish();
}

criterion_group! {
    name = benches;
    config = Criterion::default();
    targets = garbage_collection
}
criterion_main!(benches);
