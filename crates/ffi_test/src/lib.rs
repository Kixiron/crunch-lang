use crunch::{Gc, RuntimeError, RuntimeValue};

#[no_mangle]
extern "C" fn add(gc: &mut Gc, values: &[RuntimeValue]) -> Result<RuntimeValue, RuntimeError> {
    values[0].clone().add_upflowing(values[1].clone(), gc)
}
