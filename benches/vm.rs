use criterion::{black_box, criterion_group, criterion_main, Criterion};
use crunch::*;

fn vm(c: &mut Criterion) {
    c.bench_function("VM startup", |b| {
        b.iter(|| black_box(Vm::default()));
    });
}

criterion_group! {
    name = benches;
    config = Criterion::default();
    targets = vm
}
criterion_main!(benches);
