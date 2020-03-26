use std::{fs::File, io::Read};

fn emit_diagnostics<'a>(
    source: &'a str,
    diagnostics: Vec<codespan_reporting::diagnostic::Diagnostic<usize>>,
) {
    let mut files = Files::new();
    files.add("<test file>", source);

    let writer = codespan_reporting::term::termcolor::StandardStream::stderr(
        codespan_reporting::term::termcolor::ColorChoice::Auto,
    );
    let config = codespan_reporting::term::Config::default();

    for diag in diagnostics {
        codespan_reporting::term::emit(&mut writer.lock(), &config, &files, &diag).unwrap();
    }
}

fn main() {
    let mut file = File::open("test.crunch").unwrap();
    let mut buf = String::with_capacity(10000);
    file.read_to_string(&mut buf).unwrap();

    let interner = std::sync::Arc::new(parking_lot::RwLock::new(
        string_interner::StringInterner::new(),
    ));

    match crunch_parser::Parser::new(&buf, files::FileId::new(0), interner.clone()).parse() {
        Ok((ast, warn)) => {
            warn.emit();
            println!("{:#?}", ast);
        }
        Err(err) => err.emit(),
    }
}
