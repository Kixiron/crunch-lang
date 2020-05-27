use crunch_parser::{files::Files, Context, CurrentFile};
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

    {
        let context = Context::new();
        {
            let mut warnings =
                crunch_parser::Parser::new(&buf, CurrentFile::new(id, buf.len()), &context)
                    .parse()
                    .map_or_else(|warn| warn, |(warn, ..)| warn);
            warnings.emit(&files);
        }

        println!("{:#?}", context);
    }
}
