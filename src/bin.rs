use crunch::*;
use structopt::StructOpt;

fn main() {
    human_panic::setup_panic!(human_panic::Metadata {
        name: "Crunch".into(),
        version: env!("CARGO_PKG_VERSION").into(),
        authors: "Chase Wilson <chase.h.wilson3@gmail.com>".into(),
        homepage: "https://github.com/Kixiron/crunch-lang".into(),
    });

    color_backtrace::install();

    let opt = Opt::from_args();

    match opt {
        Opt::Run { options } => {
            if options.debug_log {
                simple_logger::init().unwrap();
            }

            match options.file.as_path().extension() {
                Some(ext) => match ext.to_os_string() {
                    ref a if &*a == "crunch" => Crunch::run_source_file(options),
                    ref b if &*b == "crunched" => Crunch::run_byte_file(options),
                    _ => {
                        println!("Please choose a valid .crunch or .crunched file");
                    }
                },
                None => {
                    println!("Please choose a valid .crunch or .crunched file");
                }
            }
        }
        Opt::Build { options } => {
            if options.debug_log {
                simple_logger::init().unwrap();
            }
        }
        Opt::Verify { options } => {
            if options.debug_log {
                simple_logger::init().unwrap();
            }

            if let Some(ext) = options.file.as_path().extension() {
                if &*ext.to_os_string() == "crunched" {
                    let source = {
                        use std::{fs::File, io::Read};

                        let mut buf = Vec::new();

                        let mut file = match File::open(&options.file) {
                            Ok(file) => file,
                            Err(err) => {
                                println!("Error Opening File: {:?}", err);
                                return;
                            }
                        };

                        if let Err(err) = file.read_to_end(&mut buf) {
                            println!("Error Reading File: {:?}", err);
                            return;
                        }

                        buf
                    };

                    if let Err(err) = Crunch::validate(&source) {
                        println!("Error Verifying Bytecode: {}", err);
                    } else {
                        println!("Bytecode is valid");
                    }
                }
            }
        }
    }
}

// #[derive(Debug, StructOpt)]
// #[structopt(name = "crunch", about = "The command-line interface for Crunch")]
// struct Opt {
//     #[structopt(parse(from_os_str))]
//     file: std::path::PathBuf,
// }

#[derive(Debug, StructOpt)]
#[structopt(
    name = "crunch",
    about = "The command-line interface for Crunch",
    rename_all = "kebab"
)]
enum Opt {
    /// Runs a source file or compiled bytecode
    Run {
        #[structopt(flatten)]
        options: Options,
    },

    /// Builds a source file into it's compiled version
    Build {
        #[structopt(flatten)]
        options: Options,
    },

    /// Verifies compiled bytecode
    Verify {
        #[structopt(flatten)]
        options: Options,
    },
}
