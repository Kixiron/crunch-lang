use compactor::Value;
use crunch_error::runtime_prelude::RuntimeResult;

#[no_mangle]
extern "C" fn add(values: &[Value]) -> RuntimeResult<Value> {
    values[0].clone().add_upflowing(&values[1])
}

// TODO: Write utility crate that allows easy plugin writing without letting users break things
