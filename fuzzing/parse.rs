#[cfg(feature = "fuzz")]
use honggfuzz::fuzz;

#[cfg(feature = "fuzz")]
fn main() {
    loop {
        fuzz!(|bytes: &[u8]| {
            if Ok(input_str) = std::str::from_utf8(bytes) {
                let _ = Parser::new(None, &input_str).parse();
            }
        });
    }
}

#[cfg(not(feature = "fuzz"))]
fn main() {}
