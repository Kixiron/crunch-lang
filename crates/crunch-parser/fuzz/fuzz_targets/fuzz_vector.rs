#![no_main]

use crunch_parser::parsers::parse_vector;
use crunch_token::{Token, TokenData};
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    if let Ok(data) = std::str::from_utf8(data) {
        parse_vector(&TokenData {
            kind: Token::VectorLiteral,
            source: &data,
            range: (0, data.len()),
        });
    }
});
