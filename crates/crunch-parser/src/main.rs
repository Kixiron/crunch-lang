use crunch_parser::{files::Files, Interner, SymbolTable};
use std::{fs::File, io::Read};

fn main() {
    let mut file = File::open("test.crunch").unwrap();
    let mut buf = String::with_capacity(10000);
    file.read_to_string(&mut buf).unwrap();

    let mut files = Files::new();
    let id = files.add("<test file>", &buf).unwrap();

    match crunch_parser::Parser::new(&buf, id, Interner::new(), SymbolTable::new()).parse() {
        Ok((ast, _interner, mut warn, _)) => {
            warn.emit(&files);
            println!("{:#?}", ast);
        }
        Err(mut err) => err.emit(&files),
    }
}
