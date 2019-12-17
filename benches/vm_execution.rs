use criterion::{criterion_group, criterion_main, Criterion};

fn instructions(c: &mut Criterion) {
    use crunch::{Instruction::*, *};

    let inst = vec![
        Load(RuntimeValue::I32(10), 0.into()),
        Print(0.into()),
        Load(RuntimeValue::I32(20), 1.into()),
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

    c.bench_function("Ten Plus Twenty", |b| {
        b.iter(|| crunch.execute());
    });

    let inst = vec![
        Load(RuntimeValue::Str("Test"), 0.into()),
        Print(0.into()),
        Load(RuntimeValue::Str(" Me"), 1.into()),
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

    c.bench_function("Add Strings", |b| {
        b.iter(|| crunch.execute());
    });
}

criterion_group!(benches, instructions);
criterion_main!(benches);
