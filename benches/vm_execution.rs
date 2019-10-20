use criterion::{criterion_group, criterion_main, Criterion};

fn ten_plus_twenty(c: &mut Criterion) {
    use crunch::{Instruction::*, *};

    let inst = vec![
        Cache(0, Value::Int(10)),
        Load(0, 0.into()),
        Print(0.into()),
        Cache(1, Value::Int(20)),
        Load(1, 1.into()),
        Print(1.into()),
        Add(0.into(), 1.into()),
        OpToReg(2.into()),
        Print(2.into()),
        Halt,
    ];

    let mut crunch = Crunch::from((
        inst,
        Vec::new(),
        OptionBuilder::new("./ten_plus_twenty").build(),
    ));

    c.bench_function("Ten Plus Twenty", |b| b.iter(|| crunch.execute()));
}

criterion_group!(benches, ten_plus_twenty);
criterion_main!(benches);
