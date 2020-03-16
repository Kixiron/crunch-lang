use clap::{App, Arg, SubCommand};
use compactor::CompactorOptions;
use vice::ViceOptions;

use std::{
    panic::PanicInfo,
    path::{Path, PathBuf},
};

#[derive(Debug, Clone)]
pub struct CrunchCli {
    pub debug_log: bool,
    pub verbosity: Verbosity,
    pub command: Command,
}

impl CrunchCli {
    pub fn from_args() -> Self {
        let matches = Self::get_app().get_matches();

        let debug_log = matches.is_present("debug-log");

        let verbosity = match matches.occurrences_of("v") {
            0 => Verbosity::None,
            1 => Verbosity::Minimal,
            2 => Verbosity::Moderate,
            3 | _ => Verbosity::All,
        };

        let command = if let Some(matches) = matches.subcommand_matches("run") {
            Command::Run {
                file: PathBuf::from(matches.value_of("FILE").unwrap()),
            }
        } else {
            Command::Help
        };

        Self {
            debug_log,
            verbosity,
            command,
        }
    }

    pub fn print_help() {
        Self::get_app().print_long_help().unwrap();
    }

    pub fn set_debug_hooks() {
        simple_logger::init().unwrap();
        #[cfg(not(miri))]
        color_backtrace::install();
    }

    pub fn set_panic_hook(meta: CrashInfo) {
        use std::panic;

        panic::set_hook(Box::new(move |info: &PanicInfo| {
            let file_path = Self::handle_dump(&meta, info);

            Self::print_msg(file_path, &meta).expect("Printing error message to console failed");
        }));
    }

    fn print_msg<P: AsRef<Path>>(
        file_path: Option<P>,
        CrashInfo {
            name,
            authors,
            homepage,
            ..
        }: &CrashInfo,
    ) -> Result<(), std::io::Error> {
        use std::io::{self, BufWriter, Write};

        let mut buf = BufWriter::new(io::stdout());

        writeln!(&mut buf, "Well, this is embarrassing.\n")?;
        writeln!(
            &mut buf,
            "{} had a problem and crashed. To help us diagnose the \
           problem you can send us a crash report.\n",
            name
        )?;
        writeln!(
            &mut buf,
            "We have generated a report file at \"{}\". Submit an \
           issue or email with the subject of \"{} Crash Report\" and include the \
           report as an attachment.\n",
            match file_path {
                Some(fp) => format!("{}", fp.as_ref().display()),
                None => "<Failed to store file to disk>".to_string(),
            },
            name
        )?;

        if !homepage.is_empty() {
            writeln!(&mut buf, "- Homepage: {}", homepage)?;
        }

        if !authors.is_empty() {
            writeln!(&mut buf, "- Authors: {}", authors)?;
        }

        writeln!(
            &mut buf,
            "\nWe take privacy seriously, and do not perform any \
           automated error collection. In order to improve the software, we rely on \
           people to submit reports.\n"
        )?;
        writeln!(&mut buf, "Thank you kindly!")?;

        buf.flush()?;

        Ok(())
    }

    fn handle_dump(meta: &CrashInfo, panic_info: &PanicInfo) -> Option<PathBuf> {
        let mut explanation = String::new();

        let payload = panic_info.payload().downcast_ref::<&str>();
        if let Some(payload) = payload {
            explanation.push_str(&format!("Cause: {}. ", &payload));
        }

        match panic_info.location() {
            Some(location) => explanation.push_str(&format!(
                "Panic occurred in file '{}' at line {}\n",
                location.file(),
                location.line()
            )),
            None => explanation.push_str("Panic location unknown.\n"),
        }

        let report = CrashReport::new(
            meta.name.clone(),
            meta.version.clone(),
            CrashMethod::Panic,
            explanation,
        );

        match report.clone().store() {
            Ok(f) => Some(f),
            Err(_) => {
                eprintln!("{}", report.serialize());
                None
            }
        }
    }

    pub fn get_app() -> App<'static, 'static> {
        App::new("Crunch")
            .version(env!("CARGO_PKG_VERSION"))
            .author(env!("CARGO_PKG_AUTHORS"))
            .about(env!("CARGO_PKG_DESCRIPTION"))
            .arg(
                Arg::with_name("debug-log")
                    .short("d")
                    .long("debug-log")
                    .help("Enables debug logging"),
            )
            .arg(
                Arg::with_name("v")
                    .short("v")
                    .multiple(true)
                    .help("Sets the level of verbosity"),
            )
            .subcommand(
                SubCommand::with_name("run")
                    .about("Runs a source file or compiled bytecode")
                    .arg(
                        Arg::with_name("FILE")
                            .short("f")
                            .long("file")
                            .required(true)
                            .takes_value(true)
                            .help("Run a source or bytecode file"),
                    ),
            )
    }
}

#[derive(Debug, Clone)]
pub struct CrashInfo {
    name: String,
    version: String,
    authors: String,
    homepage: String,
}

#[derive(Debug, Clone)]
pub struct CrashReport {
    name: String,
    version: String,
    method: CrashMethod,
    explanation: String,
    backtrace: String,
}

impl CrashReport {
    pub fn new(name: String, version: String, method: CrashMethod, explanation: String) -> Self {
        Self {
            name,
            version,
            method,
            explanation,
            backtrace: Self::backtrace(),
        }
    }

    pub fn store(self) -> Result<PathBuf, std::io::Error> {
        use std::{env, fs::File, io::Write};
        use uuid::Uuid;

        let uuid = Uuid::new_v3(
            &Uuid::from_u128(0),
            format!("{}-{}-{:?}", self.name, self.version, self.method).as_bytes(),
        )
        .to_hyphenated()
        .to_string();

        let tmp_dir = env::temp_dir();
        let tmp_dir = match tmp_dir.to_str() {
            Some(dir) => dir,
            None => Err(std::io::Error::new(
                std::io::ErrorKind::Other,
                "Invalid temporary directory name",
            ))?,
        };

        let file_name = format!("report-{}.toml", &uuid);
        let file_path = Path::new(tmp_dir).join(file_name);

        let mut file = File::create(&file_path)?;
        let toml = self.serialize();
        file.write_all(toml.as_bytes())?;

        Ok(file_path)
    }

    pub fn serialize(&self) -> String {
        dbg!(format!(
            "# This is an automatically generated crash file created by {}\n\n\

            name = \"{}\"\n\
            version = \"{}\"\n\
            method = \"{:?}\"\n\
            explanation = \"{}\"\n\
            backtrace = \"{}\"\n",
            self.name, self.name, self.version, self.method, self.explanation, self.backtrace
        ))
    }

    pub fn backtrace() -> String {
        use backtrace::Backtrace;
        use std::fmt::Write;

        const SKIP_FRAMES_NUM: usize = 8;
        const HEX_WIDTH: usize = core::mem::size_of::<usize>() + 2;
        const NEXT_SYMBOL_PADDING: usize = HEX_WIDTH + 6;

        let mut backtrace = String::new();

        for (idx, frame) in Backtrace::new()
            .frames()
            .iter()
            .skip(SKIP_FRAMES_NUM)
            .enumerate()
        {
            let ip = frame.ip();
            let _ = write!(backtrace, "\n{:4}: {:2$?}", idx, ip, HEX_WIDTH);

            let symbols = frame.symbols();
            if symbols.is_empty() {
                let _ = write!(backtrace, " - <unresolved>");
                continue;
            }

            for (idx, symbol) in symbols.iter().enumerate() {
                if idx != 0 {
                    let _ = write!(backtrace, "\n{:1$}", "", NEXT_SYMBOL_PADDING);
                }

                if let Some(name) = symbol.name() {
                    let _ = write!(backtrace, " - {}", name);
                } else {
                    let _ = write!(backtrace, " - <unknown>");
                }

                if let (Some(file), Some(line)) = (symbol.filename(), symbol.lineno()) {
                    let _ = write!(
                        backtrace,
                        "\n{:3$}at {}:{}",
                        "",
                        file.display(),
                        line,
                        NEXT_SYMBOL_PADDING
                    );
                }
            }
        }

        backtrace
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum CrashMethod {
    Panic,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Command {
    Run { file: PathBuf },
    Help,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Verbosity {
    None,
    Minimal,
    Moderate,
    All,
}

impl Into<CompactorOptions> for CrunchCli {
    fn into(self) -> CompactorOptions {
        CompactorOptions {
            ..Default::default()
        }
    }
}

impl Into<ViceOptions> for CrunchCli {
    fn into(self) -> ViceOptions {
        ViceOptions {
            ..Default::default()
        }
    }
}
