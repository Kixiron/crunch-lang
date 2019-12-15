use criterion::{criterion_group, criterion_main, Criterion};

fn instructions(c: &mut Criterion) {
    use crunch::{Instruction::*, *};

    let inst = vec![
        Cache(0, Value::Int(10), 0.into()),
        Print(0.into()),
        Cache(1, Value::Int(20), 1.into()),
        Print(1.into()),
        Add(0.into(), 1.into()),
        OpToReg(2.into()),
        Print(2.into()),
        Drop(0),
        Drop(1),
        Collect,
        Halt,
    ];

    let mut crunch = Crunch::from((
        inst,
        Vec::new(),
        OptionBuilder::new("./ten_plus_twenty").build(),
    ));

    c.bench_function("Ten Plus Twenty (GC Cached Values)", |b| {
        b.iter(|| crunch.execute());
    });

    let inst = vec![
        Load(Value::Int(10), 0.into()),
        Print(0.into()),
        Load(Value::Int(20), 1.into()),
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

    c.bench_function("Ten Plus Twenty (Register Values)", |b| {
        b.iter(|| crunch.execute());
    });

    let inst = vec![
        Cache(0, Value::String("Test".to_string()), 0.into()),
        Print(0.into()),
        Cache(1, Value::String(" Me".to_string()), 1.into()),
        Print(1.into()),
        Add(0.into(), 1.into()),
        OpToReg(2.into()),
        Print(2.into()),
        Drop(0),
        Drop(1),
        Collect,
        Halt,
    ];

    let mut crunch = Crunch::from((
        inst,
        Vec::new(),
        OptionBuilder::new("./ten_plus_twenty").build(),
    ));

    c.bench_function("Add strings (GC Cached Values)", |b| {
        b.iter(|| crunch.execute());
    });

    let inst = vec![
        Load(Value::String("Test".to_string()), 0.into()),
        Print(0.into()),
        Load(Value::String(" Me".to_string()), 1.into()),
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

    c.bench_function("Add strings (Register Values)", |b| {
        b.iter(|| crunch.execute());
    });
}

criterion_group!(benches, instructions);
criterion_main!(benches);
