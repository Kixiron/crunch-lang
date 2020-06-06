use crunch_parser::CurrentFile;
use crunch_shared::{context::Context, files::Files};
use std::{fs::File, io::Read};

fn main() {
    let mut file = File::open(
        std::env::args()
            .next()
            .expect("You must provide a file to be parsed"),
    )
    .unwrap();
    let mut buf = String::with_capacity(10000);
    file.read_to_string(&mut buf).unwrap();

    let mut files = Files::new();
    let id = files.add("<test file>", &buf).unwrap();

    let context = Context::new();
    match crunch_parser::Parser::new(&buf, CurrentFile::new(id, buf.len()), context.clone()).parse()
    {
        Ok((ast, mut warn, ..)) => {
            warn.emit(&files);
            println!("{:#?}", ast);
        }
        Err(mut warn) => warn.emit(&files),
    }
}
