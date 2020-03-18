use crunch::{Command, Crunch, CrunchCli};
use std::io::{self, BufWriter};

fn main() {
    #[cfg(not(debug_assertions))]
    CrunchCli::set_panic_hook(crunch::CrashInfo {
        name: "Crunch".into(),
        version: env!("CARGO_PKG_VERSION").into(),
        authors: "Chase Wilson <contact@chasewilson.dev>".into(),
        homepage: "https://github.com/Kixiron/crunch-lang".into(),
        ..Default::default()
    });

    let options = CrunchCli::from_args();

    match &options.command {
        Command::Run { file } => {
            if options.debug_log {
                CrunchCli::set_debug_hooks();
            }

            let mut stdout = BufWriter::new(io::stdout());

            match file.as_path().extension() {
                Some(ext) => match ext.to_os_string() {
                    ref a if &*a == "crunch" => {
                        Crunch::run_source_file(options.clone(), &file, Box::new(&mut stdout))
                    }
                    ref b if &*b == "crunched" => {
                        Crunch::run_byte_file(options.clone(), &file, Box::new(&mut stdout))
                    }
                    _ => {
                        println!("Please choose a valid .crunch or .crunched file");
                    }
                },

                None => {
                    println!("Please choose a valid .crunch or .crunched file");
                }
            }
        }

        Command::Help => {
            CrunchCli::print_help();
        }
    }
}
