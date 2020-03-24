use std::{fs::File, io::Read};

fn emit_diagnostics<'a>(
    source: &'a str,
    diagnostics: Vec<crunch_error::codespan_reporting::diagnostic::Diagnostic<usize>>,
) {
    use crunch_error::parse_prelude::{codespan_reporting, SimpleFiles};

    let mut files = SimpleFiles::new();
    files.add("<test file>", source);

    let writer = codespan_reporting::term::termcolor::StandardStream::stderr(
        codespan_reporting::term::termcolor::ColorChoice::Auto,
    );
    let config = codespan_reporting::term::Config::default();

    for diag in diagnostics {
        codespan_reporting::term::emit(&mut writer.lock(), &config, &files, &diag).unwrap();
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut file = File::open("test.crunch")?;
    let mut buf = String::new();
    file.read_to_string(&mut buf)?;

    let interner = std::sync::Arc::new(parking_lot::RwLock::new(
        string_interner::StringInterner::new(),
    ));

    match crunch_parser::Parser::new(&buf, 0, interner.clone()).parse() {
        Ok((ast, diag)) => {
            emit_diagnostics(&buf, diag);
            println!("{:?}", ast);
        }
        Err(err) => emit_diagnostics(&buf, err),
    }

    Ok(())
}
