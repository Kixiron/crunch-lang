use std::path::Path;

pub struct Crunch {
    error_occurred: bool,
}

/// All public interfaces to Crunch
impl Crunch {
    pub fn new() -> Self {
        Self {
            error_occurred: false,
        }
    }

    pub fn prompt(&mut self) {
        use crunch_parser::Parser;
        use crunch_token::TokenStream;
        use std::io::{stdin, stdout, Write};

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

            let lexer = TokenStream::new(&input);
            println!(
                "{:#?}",
                lexer
                    .clone()
                    .filter(|token| token.kind() != crunch_token::Token::WhiteSpace)
                    .collect::<Vec<crunch_token::TokenData>>()
            );
            let tree = Parser::new(lexer).parse();
            println!("{:#?}", tree);
        }
    }

    pub fn run_file(&mut self, file_path: &Path) {
        use crunch_parser::Parser;
        use crunch_token::TokenStream;
        use std::io::ErrorKind;

        if let Some(name) = file_path.file_name() {
            if let Some(name) = name.to_str() {
                if match name.split(".").into_iter().last() {
                    Some(extension) => extension,
                    None => "",
                } != "crunch"
                {
                    println!("Only files ending in .crunch can be crunched!");
                    return;
                }

                Crunch::display_boxed(&("Crunching ".to_owned() + name));
            } else {
                Crunch::display_boxed("Crunching");
            }
        } else {
            Crunch::display_boxed("Crunching");
        }

        let contents = match Crunch::open_file(file_path) {
            Ok(contents) => contents,
            Err(err) => {
                match err.kind() {
                    ErrorKind::WriteZero => println!("The file is empty!"),
                    _ => println!("Something went wrong with reading the file"),
                }

                return;
            }
        };

        let lexer = TokenStream::new(contents.as_str());
        let tree = Parser::new(lexer).parse();
        println!("{:#?}", tree);
    }

    pub fn display_info(&self) {
        let version = "0.0.0";
        Crunch::display_boxed(&("Crunch v".to_owned() + version));
    }
}

/// Crunch's internal utility functions
impl Crunch {
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

    fn open_file(path: &Path) -> std::io::Result<String> {
        use std::{
            fs::File,
            io::{BufReader, Error, ErrorKind, Read},
        };

        let file = File::open(path)?;
        let mut reader = BufReader::new(file);

        let mut contents = String::new();
        let len = reader.read_to_string(&mut contents)?;

        if len == 0 {
            Err(Error::new(
                ErrorKind::WriteZero,
                "Nothing was read from the file",
            ))
        } else {
            Ok(contents)
        }
    }
}
