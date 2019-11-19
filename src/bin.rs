use crunch::*;
use structopt::StructOpt;

fn main() {
    human_panic::setup_panic!(human_panic::Metadata {
        name: "Crunch".into(),
        version: env!("CARGO_PKG_VERSION").into(),
        authors: "Chase Wilson <chase.h.wilson3@gmail.com>".into(),
        homepage: "https://github.com/Kixiron/crunch-lang".into(),
    });

    let opt = Opt::from_args();

    match opt {
        Opt::Run { options } => {
            if options.debug_log {
                set_debug_hooks();
            }

            match options.file.as_path().extension() {
                Some(ext) => match ext.to_os_string() {
                    ref a if &*a == "crunch" => Crunch::run_source_file(options),
                    ref b if &*b == "crunched" => Crunch::run_byte_file(options).unwrap(),
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
                set_debug_hooks();
            }
        }

        Opt::Verify { options } => {
            if options.debug_log {
                set_debug_hooks();
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

        Opt::Repl {
            repl_options,
            burn_gc,
            debug_log,
            fault_tolerant,
            overwrite_heap,
        } => {
            if debug_log {
                set_debug_hooks();
            }

            let options = Options {
                file: std::path::PathBuf::from("CrunchRepl"),
                burn_gc,
                debug_log,
                fault_tolerant,
                overwrite_heap,
            };

            Crunch::repl(options, repl_options);
        }
    }
}

fn set_debug_hooks() {
    simple_logger::init().unwrap();
    color_backtrace::install();
}

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

    /// Runs a repl
    Repl {
        #[structopt(long = "--output", parse(from_str))]
        repl_options: Vec<ReplOutput>,
        #[structopt(long = "--burn-gc")]
        burn_gc: bool,
        /// Activates detailed debug logging
        #[structopt(long = "--debug-log")]
        debug_log: bool,
        /// Allows some runtime errors to be ignored
        #[structopt(long = "--fault-tolerant")]
        fault_tolerant: bool,
        /// Overwrites the heap on a side swap
        #[structopt(long = "--overwrite-heap")]
        overwrite_heap: bool,
    },
}
