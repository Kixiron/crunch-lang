use compactor::Compactor;
use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn vm(c: &mut Criterion) {
    c.bench_function("Compactor Startup", |b| {
        b.iter(|| {
            black_box({
                let mut stdout = std::io::stdout();
                let _ = Compactor::with_stdout(Box::new(&mut stdout));
            })
        });
    });
}

criterion_group! {
    name = benches;
    config = Criterion::default();
    targets = vm
}
criterion_main!(benches);
