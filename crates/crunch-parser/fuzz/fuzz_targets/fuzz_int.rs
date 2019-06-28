#![no_main]

use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    crunch_parser::Parser::new(&[TokenData {
        crunch_token::TokenData {
            kind: crunch_token::Token::IntLiteral,
            source: data.as_str(),
            range: (0, data.len()),
        }
    }]).parse()
});
