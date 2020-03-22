#[macro_use]
extern crate honggfuzz;

use parking_lot::RwLock;
use std::sync::Arc;

fn main() {
    let interner = Arc::new(Mutex::new(
        crunch_parser::string_interner::StringInterner::new(),
    ));

    loop {
        fuzz!(|bytes: &[u8]| {
            if let Ok(input_str) = std::str::from_utf8(bytes) {
                let _ = crunch_parser::Parser::new(&input_str, interner.clone(), 0).parse();
            }
        });
    }
}
