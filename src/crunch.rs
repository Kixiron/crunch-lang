use super::{
    disassemble, interpreter::Interpreter, Bytecode, Decoder, Instruction, Options, ReplOutput,
    Result, RuntimeError, RuntimeErrorTy, Vm,
};

/// The main interface to the crunch language
#[allow(missing_debug_implementations)]
pub struct Crunch {
    /// The main contents of the VM
    vm: Vm,
    /// Contained options
    _options: Options,
}

/// The main usage of Crunch
impl Crunch {
    #[inline]
    #[must_use]
    pub fn new(options: Options) -> Self {
        Self {
            vm: Vm::new(&options, Box::new(std::io::stdout())),
            _options: options,
        }
    }

    /// Execute the currently loaded program
    #[inline]
    pub fn execute(&mut self, instructions: &[Vec<Instruction>]) -> Result<()> {
        trace!("Starting Crunch Execution");

        self.vm.execute(instructions)?;

        trace!("Finished Crunch Execution Successfully");
        Ok(())
    }

    /// Run a source file in the `.crunch` format
    #[inline]
    pub fn run_source_file(options: Options) {
        trace!("Running Source File: {}", options.file.display());

        let source = {
            use std::{fs::File, io::Read};

            let mut buf = String::new();

            let mut file = match File::open(&options.file) {
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

        let mut parser = super::parser::Parser::new(
            match options.file.file_name() {
                Some(name) => name.to_str(),
                None => None,
            },
            &source,
        );

        match parser.parse() {
            Ok(ast) => match Interpreter::from_interner(&options, parser.interner).interpret(ast.0)
            {
                Ok(functions) => {
                    info!("Executing Crunch Program");

                    if let Err(err) = Self::new(options).execute(&functions) {
                        err.emit()
                    }
                }
                Err(err) => err.emit(),
            },

            // Emit parsing errors
            Err(err) => {
                error!("Error Parsing Crunch Source File");

                let writer = codespan_reporting::term::termcolor::StandardStream::stderr(
                    codespan_reporting::term::termcolor::ColorChoice::Auto,
                );

                let config = codespan_reporting::term::Config::default();

                let mut files = codespan::Files::new();
                files.add(
                    match options.file.file_name() {
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
    pub fn run_byte_file(options: Options) -> Result<()> {
        trace!("Running Compiled File: {}", options.file.display());

        let source = {
            use std::{fs::File, io::Read};

            let mut buf = Vec::new();

            let mut file = match File::open(&options.file) {
                Ok(file) => file,
                Err(_err) => {
                    return Err(RuntimeError {
                        ty: RuntimeErrorTy::FileError,
                        message: format!("Error Opening {}", options.file.display()),
                    });
                }
            };

            if let Err(_err) = file.read_to_end(&mut buf) {
                return Err(RuntimeError {
                    ty: RuntimeErrorTy::FileError,
                    message: format!("Error Reading {}", options.file.display()),
                });
            }

            buf
        };

        let bytes = match Self::validate(&source) {
            Ok(bytes) => bytes,
            Err(_err) => {
                return Err(RuntimeError {
                    ty: RuntimeErrorTy::BytecodeError,
                    message: "Invalid Bytecode".to_string(),
                });
            }
        };

        let (main, mut functions) = Self::parse_bytecode(bytes)?;
        functions.insert(0, main);

        Self::new(options).execute(&functions)?;

        Ok(())
    }

    pub fn repl(options: Options, repl_outputs: Vec<ReplOutput>) {
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

            let mut parser = super::parser::Parser::new(Some("CrunchRepl"), &input);

            match parser.parse() {
                Ok(ast) => {
                    trace!("Parsing successful");

                    if repl_outputs.contains(&ReplOutput::Ast) {
                        println!("[Program AST]: {:#?}", &ast);
                    }

                    match Interpreter::from_interner(&options, parser.interner)
                        .interpret(ast.0.clone())
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
                                err.emit()
                            }
                        }
                        Err(err) => err.emit(),
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

    /// Parse validated bytecode into the Main Function and Function Table
    #[inline]
    fn parse_bytecode(bytes: Bytecode<'_>) -> Result<(Vec<Instruction>, Vec<Vec<Instruction>>)> {
        Decoder::new(&*bytes).decode()
    }

    /// Validate raw bytes as valid [`Bytecode`]
    #[inline]
    pub fn validate<'b>(bytes: &'b [u8]) -> std::result::Result<Bytecode<'b>, &'static str> {
        Bytecode::validate(bytes)
    }

    /// Encode the currently loaded program as bytes
    // #[inline]
    // fn encode(self) -> Vec<u8> {
    //     Encoder::new({
    //         let mut funcs = self.vm.functions;
    //         funcs.insert(0, self.instructions);
    //         funcs
    //             .into_iter()
    //             .map(|f| match f.0 {
    //                 Either::Left(inst) => inst,
    //                 Either::Right((_jit, inst)) => inst,
    //             })
    //             .collect()
    //     })
    //     .encode()
    // }

    /// Disassemble bytecode in the `.crunched` format
    #[inline]
    #[must_use]
    pub fn disassemble(bytes: Bytecode<'_>) -> String {
        disassemble(&*bytes)
    }
}
