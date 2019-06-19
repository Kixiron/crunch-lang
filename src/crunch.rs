use codespan::{CodeMap, FileName};
use std::path::Path;

pub struct Crunch {
    code_map: CodeMap,
}

/// All public interfaces to Crunch
impl Crunch {
    pub fn new() -> Self {
        Self { code_map: CodeMap::new() }
    }

    pub fn prompt(&mut self) {
        use std::{
            borrow::Cow,
            io::{stdin, stdout, Write},
        };

        Crunch::display_boxed("Welcome to the Crunch Interpreter");

        loop {
            print!("~> ");
            if let Err(err) = stdout().flush() {
                self.log(&format!("{:?}", err));
                continue;
            }

            let mut input = String::new();

            let mut new_input = String::new();
            if let Err(err) = stdin().read_line(&mut new_input) {
                self.log(&format!("{:?}", err));
                continue;
            }
            input.push_str(&new_input);

            // Empty lines across OS's
            let empty_line = if cfg!(target_os = "windows") {
                "\r\n"
            } else if cfg!(target_os = "linux") || cfg!(target_os = "macos") {
                "\n" // Untested
            } else {
                eprintln!("This OS is not supported!");
                "\n"
            };

            // While the input is not empty, continue taking code until it is
            while new_input != empty_line {
                print!("~  ");
                if let Err(err) = stdout().flush() {
                    self.log(&format!("{:?}", err));
                    continue;
                }

                new_input.clear();
                if let Err(err) = stdin().read_line(&mut new_input) {
                    self.log(&format!("{:?}", err));
                    continue;
                }

                input.push_str(&new_input);
            }

            self.code_map.add_filemap(
                FileName::Virtual(Cow::from("Crunch REPL")),
                input,
            );

            self.run_files();

            // TODO: Do better than this for clearing the REPL's code store, maybe store an index and only refresh like this when the
            // CodeMap gets too large
            self.code_map = CodeMap::new();
        }
    }

    pub fn run_file(&mut self, file_path: &Path) {
        self.code_map.add_filemap_from_disk(file_path).unwrap();

        self.run_files();
    }

    pub(crate) fn display_info(&self) {
        let version = "0.0.0";
        Crunch::display_boxed(&("Crunch v".to_owned() + version));
    }
}

/// Crunch's internal utility functions
impl Crunch {
    fn run_files(&mut self) {
        use crunch_error::EmittedError;
        use crunch_parser::Parser;
        use crunch_token::TokenStream;

        for file in self.code_map.iter() {
            let tokens = TokenStream::new(file.src());
            let tree = Parser::new(tokens).parse();

            let mut writer = EmittedError::new_writer();
            println!("{:#?}", tree);
            for node in tree.into_inner() {
                match node {
                    crunch_parser::Expr::Error(errors) => {
                        for error in errors {
                            error.emit(&mut writer, &self.code_map).unwrap();
                        }
                    }
                    _ => {}
                }
            }
        }
    }

    fn log(&self, error: &str) {
        use std::{fs::File, io::Write};

        let mut file = File::create("error.log").unwrap();

        file.write_all(
            &format!("\nCrunch ran into an error.\nPlease email this file with the conditions leading up to the error to <{}>\n{:#?}", crate::email(), error).as_bytes()
        ).unwrap();
    }

    fn display_boxed(content: &str) {
        println!("┌─{}─┐", "─".repeat(content.len()));
        println!("│ {} │", content);
        println!("└─{}─┘", "─".repeat(content.len()));
    }
}
