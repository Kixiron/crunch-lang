use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn new_registers(c: &mut Criterion) {
    use crunch::Registers;

    c.bench_function("Register::new()", |b| {
        b.iter(|| {
            black_box(Registers::new());
        })
    });
}

fn load_str(c: &mut Criterion) {
    use crunch::{Instruction::LoadStr, LoadedString, Registers};

    c.bench_function("LoadStr", |b| {
        let mut registers = Registers::new();

        b.iter(|| {
            black_box(
                LoadStr(LoadedString::new_boxed("Test".to_owned(), 0, 0)).execute(&mut registers),
            );
        })
    });
}

fn load_bool(c: &mut Criterion) {
    use crunch::{Instruction::LoadBool, Registers};

    c.bench_function("LoadBool", |b| {
        let mut registers = Registers::new();

        b.iter(|| {
            black_box(LoadBool(true, 0).execute(&mut registers));
        })
    });
}

fn load_int(c: &mut Criterion) {
    use crunch::{Instruction::LoadInt, Registers};

    c.bench_function("LoadInt", |b| {
        let mut registers = Registers::new();

        b.iter(|| {
            black_box(LoadInt(0, 0).execute(&mut registers));
        })
    });
}

fn drop(c: &mut Criterion) {
    use crunch::{Instruction::Drop, Registers};

    c.bench_function("Drop", |b| {
        let mut registers = Registers::new();

        b.iter(|| {
            black_box(Drop(0).execute(&mut registers));
        })
    });
}

fn add_str(c: &mut Criterion) {
    use crunch::{Instruction::AddStr, Registers, Value};

    c.bench_function("AddStr", |b| {
        let mut registers = Registers::new();
        registers.load_str("Test".to_owned(), 0.into());
        registers.load_str("Test".to_owned(), 1.into());
        registers.load(Value::Str(0.into()), 0);
        registers.load(Value::Str(1.into()), 1);

        b.iter(|| {
            black_box(
                AddStr {
                    left: 0.into(),
                    right: 1.into(),
                }
                .execute(&mut registers),
            );
        })
    });
}

fn add_int(c: &mut Criterion) {
    use crunch::{Instruction::AddInt, Registers, Value};

    c.bench_function("AddInt", |b| {
        let mut registers = Registers::new();
        registers.load(Value::Int(1), 0);
        registers.load(Value::Int(1), 1);

        b.iter(|| {
            black_box(AddInt { left: 0, right: 1 }.execute(&mut registers));
        })
    });
}

fn sub_int(c: &mut Criterion) {
    use crunch::{Instruction::SubInt, Registers, Value};

    c.bench_function("SubInt", |b| {
        let mut registers = Registers::new();
        registers.load(Value::Int(1), 0);
        registers.load(Value::Int(1), 1);

        b.iter(|| {
            black_box(SubInt { left: 0, right: 1 }.execute(&mut registers));
        })
    });
}

fn print_int(c: &mut Criterion) {
    use crunch::{Instruction::Print, Registers, Value};

    c.bench_function("SubInt", |b| {
        let mut registers = Registers::new();
        registers.load(Value::Int(1), 0);

        b.iter(|| {
            black_box(Print(0).execute(&mut registers));
        })
    });
}

fn print_str(c: &mut Criterion) {
    use crunch::{Instruction::Print, Registers, Value};

    c.bench_function("SubInt", |b| {
        let mut registers = Registers::new();
        registers.load_str("Test".to_owned(), 0.into());
        registers.load(Value::Str(0.into()), 0);

        b.iter(|| {
            black_box(Print(0).execute(&mut registers));
        })
    });
}

criterion_group!(
    benches,
    load_str,
    new_registers,
    load_bool,
    load_int,
    drop,
    add_str,
    add_int,
    sub_int,
    // print_int,
    // print_str,
);
criterion_main!(benches);
