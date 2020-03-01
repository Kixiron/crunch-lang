#[macro_use]
extern crate honggfuzz;

fn main() {
    loop {
        fuzz!(|bytes: &[u8]| {
            if Ok(input_str) = std::str::from_utf8(bytes) {
                let _ = crunch::Parser::new(None, &input_str).parse();
            }
        });
    }
}
