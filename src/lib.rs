#![feature(external_doc)]
#![doc(include = "../README.md")]

mod cli;

pub use cli::{Command, CrunchCli, Verbosity};

use compactor::{Compactor, Instruction};
use crunch_bytecode::Bytecode;
use crunch_error::{
    codespan, codespan_reporting,
    compile_error::{CompileError, CompileErrorTy},
    log::{error, info, trace},
    runtime_prelude::RuntimeResult,
};
use std::path::Path;
use vice::Vice;

/// The main interface to the crunch language
#[allow(missing_debug_implementations)]
pub struct Crunch {
    /// The main contents of the VM
    vm: Compactor,
    /// Contained options
    _options: CrunchCli,
}

/// The main usage of Crunch
impl Crunch {
    #[inline]
    #[must_use]
    pub fn new(options: CrunchCli) -> Self {
        Self {
            vm: Compactor::new(options.clone().into(), Box::new(std::io::stdout())),
            _options: options,
        }
    }

    /// Execute the currently loaded program
    #[inline]
    pub fn execute(&mut self, instructions: &[Vec<Instruction>]) -> RuntimeResult<()> {
        trace!("Starting Crunch Execution");

        self.vm.execute(instructions)?;

        trace!("Finished Crunch Execution Successfully");
        Ok(())
    }

    /// Run a source file in the `.crunch` format
    #[inline]
    pub fn run_source_file(options: CrunchCli, path: &Path) {
        trace!("Running Source File: {}", path.display());

        let source = {
            use std::{fs::File, io::Read};

            let mut buf = String::new();

            let mut file = match File::open(path) {
                Ok(file) => file,
                Err(err) => {
                    println!("Error Opening File: {:?}", err);
                    return;
                }
            };

            if let Err(err) = file.read_to_string(&mut buf) {
                println!("Error Reading File: {:?}", err);
                return;
            }

            buf
        };

        let mut parser = crunch_parser::Parser::new(
            match path.file_name() {
                Some(name) => name.to_str(),
                None => None,
            },
            &source,
        );

        match parser.parse() {
            Ok(ast) => {
                match Vice::from_interner(options.clone().into(), parser.interner).compile(ast.0) {
                    Ok(functions) => {
                        info!("Executing Crunch Program");

                        if let Err(err) = Self::new(options).execute(&functions) {
                            println!("{}", err);
                        }
                    }
                    Err(err) => println!("{}", err),
                }
            }

            // Emit parsing errors
            Err(err) => {
                error!("Error Parsing Crunch Source File");

                let writer = codespan_reporting::term::termcolor::StandardStream::stderr(
                    codespan_reporting::term::termcolor::ColorChoice::Auto,
                );

                let config = codespan_reporting::term::Config::default();

                let mut files = codespan::Files::new();
                files.add(
                    match path.file_name() {
                        Some(name) => name.to_str().unwrap_or("Crunch Source File"),
                        None => "Crunch Source File",
                    },
                    &source,
                );

                for e in err {
                    if let Err(err) =
                        codespan_reporting::term::emit(&mut writer.lock(), &config, &files, &e)
                    {
                        println!("Error Emitting Error: {:?}", err);
                    }
                }
            }
        }
    }

    /// Run a byte file in the `.crunched` format
    #[inline]
    pub fn run_byte_file(options: CrunchCli, path: &Path) {
        trace!("Running Compiled File: {}", path.display());

        let source = {
            use std::{fs::File, io::Read};

            let mut buf = Vec::new();

            let mut file = match File::open(&path) {
                Ok(file) => file,
                Err(_err) => {
                    println!(
                        "{}",
                        CompileError::new(
                            CompileErrorTy::FileError,
                            format!("Error Opening {}", path.display()),
                        )
                    );
                    return;
                }
            };

            if let Err(_err) = file.read_to_end(&mut buf) {
                println!(
                    "{}",
                    CompileError::new(
                        CompileErrorTy::FileError,
                        format!("Error Reading {}", path.display()),
                    )
                );
                return;
            }

            buf
        };

        let bytes = match Self::validate(&source) {
            Ok(bytes) => bytes,
            Err(_err) => {
                println!(
                    "{}",
                    CompileError::new(CompileErrorTy::InvalidBytecode, "Invalid Bytecode",)
                );
                return;
            }
        };

        let (main, mut functions) = match Self::parse_bytecode(bytes) {
            Ok(parsed) => parsed,
            Err(err) => {
                println!("{}", err);
                return;
            }
        };
        functions.insert(0, main);

        if let Err(err) = Self::new(options).execute(&functions) {
            println!("{}", err);
        }
    }

    /*
    pub fn repl(options: CrunchCli, repl_outputs: Vec<ReplOutput>) {
        use std::io::{self, Write};

        info!("Starting Crunch REPL");

        'repl: loop {
            print!(">>> ");
            if let Err(err) = io::stdout().flush() {
                error!("Repl Flush Error: {:?}", err);
                println!("[Repl Flush Error]");
                continue;
            }

            let (mut input, mut curr_input, mut number_empty_lines) =
                (String::new(), String::new(), 0_u8);

            while number_empty_lines < 2 {
                input.push_str(&curr_input);
                curr_input = String::new();

                if let Err(err) = io::stdin().read_line(&mut curr_input) {
                    error!("Repl Read Error: {:?}", err);
                    println!("[Repl Read Error]");
                    break;
                }

                if curr_input.trim().is_empty() {
                    number_empty_lines += 1;
                } else {
                    number_empty_lines = 0;
                }

                if curr_input.trim() == "exit" {
                    break 'repl;
                }

                print!("... ");
                if let Err(err) = io::stdout().flush() {
                    error!("Repl Flush Error: {:?}", err);
                    println!("[Repl Flush Error]");
                    break;
                }
            }
            println!();

            trace!("REPL Input: {:?}", &input);

            let mut parser = crunch_parser::Parser::new(Some("CrunchRepl"), &input);

            match parser.parse() {
                Ok(ast) => {
                    trace!("Parsing successful");

                    if repl_outputs.contains(&ReplOutput::Ast) {
                        println!("[Program AST]: {:#?}", &ast);
                    }

                    match Vice::from_interner(options.clone().into(), parser.interner)
                        .compile(ast.0.clone())
                    {
                        Ok(functions) => {
                            if repl_outputs.contains(&ReplOutput::Bytecode) {
                                println!(
                                    "[Program Bytecode]:\n  [Functions]: {:#?}",
                                    functions
                                        .iter()
                                        .map(|f| format!("{:?}", f))
                                        .collect::<Vec<String>>()
                                );
                            }

                            println!("[Output]:");

                            if let Err(err) = Self::new(options.clone()).execute(&functions) {
                                println!("{}", err);
                            }
                        }
                        Err(err) => println!("{}", err),
                    }
                }

                // Emit parsing errors
                Err(err) => {
                    trace!("Parsing unsuccessful");

                    let writer = codespan_reporting::term::termcolor::StandardStream::stderr(
                        codespan_reporting::term::termcolor::ColorChoice::Auto,
                    );

                    let config = codespan_reporting::term::Config::default();

                    let mut files = codespan::Files::new();
                    files.add("CrunchRepl", &input);

                    for e in err {
                        if let Err(err) =
                            codespan_reporting::term::emit(&mut writer.lock(), &config, &files, &e)
                        {
                            println!("Error Emitting Error: {:?}", err);
                        }
                    }
                }
            }

            println!();
        }
    }
    */

    /// Parse validated bytecode into the Main Function and Function Table
    #[inline]
    fn parse_bytecode(
        bytes: Bytecode<'_>,
    ) -> RuntimeResult<(Vec<Instruction>, Vec<Vec<Instruction>>)> {
        crunch_bytecode::Decoder::new(&*bytes).decode()
    }

    /// Validate raw bytes as valid [`Bytecode`]
    #[inline]
    pub fn validate<'b>(bytes: &'b [u8]) -> RuntimeResult<Bytecode<'b>> {
        Bytecode::validate(bytes)
    }
}
