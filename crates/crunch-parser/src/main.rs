fn main() {
    println!("Getting files");
    let files = std::fs::read_dir("../../tests/")
        .unwrap()
        .filter_map(|f| {
            if let Ok(f) = f {
                if let Some(ext) = f.path().extension() {
                    if &*ext == "crunch" {
                        use std::io::Read;

                        let mut file = std::fs::File::open(&f.path()).unwrap();
                        let mut contents = String::new();
                        file.read_to_string(&mut contents).unwrap();

                        return Some(contents);
                    }
                }
            }

            None
        })
        .collect::<Vec<String>>();

    let interner = std::sync::Arc::new(parking_lot::RwLock::new(
        string_interner::StringInterner::new(),
    ));

    println!("Starting parser loop");
    for (idx, file) in files.into_iter().enumerate() {
        println!("{}: {:?}", idx, file);

        if let Ok((ast, _)) = crunch_parser::Parser::new(&file, idx, interner.clone()).parse() {
            println!("{:?}", ast);
        } else {
            println!("error");
        }
    }
}
