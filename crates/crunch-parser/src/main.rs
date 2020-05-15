use crunch_parser::{files::Files, CurrentFile, Interner};
use std::{fs::File, io::Read};

fn main() {
    let mut file = File::open("crates/crunch-parser/test.crunch").unwrap();
    let mut buf = String::with_capacity(10000);
    file.read_to_string(&mut buf).unwrap();

    let mut files = Files::new();
    let id = files.add("<test file>", &buf).unwrap();

    match crunch_parser::Parser::new(&buf, CurrentFile::new(id, buf.len()), Interner::new()).parse()
    {
        Ok((ast, _interner, mut warn, ..)) => {
            warn.emit(&files);
            println!("{:#?}", ast);
        }
        Err(mut err) => err.emit(&files),
    }
}
