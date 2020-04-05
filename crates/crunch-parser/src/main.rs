use crunch_parser::{files::Files, Interner};
use std::{fs::File, io::Read};

fn main() {
    let mut file = File::open("test.crunch").unwrap();
    let mut buf = String::with_capacity(10000);
    file.read_to_string(&mut buf).unwrap();

    let interner = Interner::new();

    let mut files = Files::new();
    let id = files.add("<test file>", &buf).unwrap();

    match crunch_parser::Parser::new(&buf, id, interner.clone()).parse() {
        Ok((ast, warn)) => {
            warn.emit(&files);
            // let mut pp = PrettyPrinter::new(interner.clone());
            // let out = std::io::stdout();
            // pp.pretty_print(&mut out.lock(), &ast).unwrap();

            let _ = ast;
        }
        Err(err) => err.emit(&files),
    }
}
