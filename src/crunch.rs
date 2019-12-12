use super::{
    disassemble, interpreter::Interpreter, Bytecode, Decoder, Encoder, Instruction, Options,
    ReplOutput, Result, RuntimeError, RuntimeErrorTy, Vm,
};

/// The main interface to the crunch language
#[allow(missing_debug_implementations)]
pub struct Crunch {
    /// The Main function of the program
    instructions: Vec<Instruction>,
    /// The main contents of the VM
    vm: Vm,
    /// Contained options
    _options: Options,
}

/// The main usage of Crunch
impl Crunch {
    /// Execute the currently loaded program
    #[inline]
    pub fn execute(&mut self) {
        trace!("Starting Crunch Execution");

        while !self.vm.finished_execution {
            trace!(
                "Executing instruction: {:?}",
                self.instructions[*self.vm.index as usize]
            );

            // If an error occurs during execution, emit the error and return
            if let Err(err) = self.instructions[*self.vm.index as usize].execute(&mut self.vm) {
                err.emit();
                trace!("Finished Crunch Execution with Error");
                return;
            }
        }

        trace!("Finished Crunch Execution");
    }

    /// Run a source file in the `.crunch` format
    #[inline]
    pub fn run_source_file<'a>(options: Options) {
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
            Ok(ast) => match Interpreter::new(ast.0.clone(), &options).interpret() {
                Ok((main, funcs)) => {
                    info!("Executing Crunch Program");
                    Self::from((main, funcs, options)).execute()
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
    pub fn run_byte_file<'a>(options: Options) -> Result<()> {
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

        let instructions = Self::parse_bytecode(bytes)?;

        Self::from((instructions.0, instructions.1, options)).execute();

        Ok(())
    }

    pub fn repl(options: Options, repl_outputs: Vec<ReplOutput>) {
        use std::io::{self, Write};

        info!("Starting Crunch REPL");

        'repl: loop {
            print!(">>> ");
            if let Err(_) = io::stdout().flush() {
                println!("[Repl Flush Error]");
                continue;
            }

            let (mut input, mut curr_input, mut number_empty_lines) =
                (String::new(), String::new(), 0_u8);

            while number_empty_lines < 2 {
                input.push_str(&curr_input);
                curr_input = String::new();

                if let Err(_) = io::stdin().read_line(&mut curr_input) {
                    println!("[Repl Read Error]");
                    break;
                }

                if !curr_input.trim().is_empty() {
                    number_empty_lines = 0;
                } else {
                    number_empty_lines += 1;
                }

                if curr_input.trim() == "exit" {
                    break 'repl;
                }

                print!("... ");
                if let Err(_) = io::stdout().flush() {
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

                    match Interpreter::new(ast.0.clone(), &options).interpret() {
                        Ok((main, funcs)) => {
                            if repl_outputs.contains(&ReplOutput::Bytecode) {
                                println!(
                                    "[Program Bytecode]:\n  [Main]: {:?}\n  [Functions]: {:#?}",
                                    &main,
                                    funcs
                                        .iter()
                                        .map(|f| format!("{:?}", f))
                                        .collect::<Vec<String>>()
                                );
                            }

                            println!("[Output]:");
                            Self::from((main, funcs, options.clone())).execute()
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
    fn parse_bytecode<'a>(
        bytes: Bytecode<'a>,
    ) -> Result<(Vec<Instruction>, Vec<Vec<Instruction>>)> {
        Decoder::new(&*bytes).decode()
    }

    /// Validate raw bytes as valid [`Bytecode`]
    #[inline]
    pub fn validate<'a>(bytes: &'a [u8]) -> std::result::Result<Bytecode<'a>, &'static str> {
        Bytecode::validate(bytes)
    }

    /// Encode the currently loaded program as bytes
    #[inline]
    fn encode(&self) -> Vec<u8> {
        Encoder::new({
            let mut funcs = self.vm.functions.clone();
            funcs.insert(0, self.instructions.clone());
            funcs
        })
        .encode()
    }

    /// Disassemble bytecode in the `.crunched` format
    #[inline]
    pub fn disassemble<'a>(bytes: Bytecode<'a>) -> String {
        disassemble(&*bytes)
    }
}

impl From<(Vec<Instruction>, Vec<Vec<Instruction>>, Options)> for Crunch {
    #[inline]
    fn from(
        (instructions, functions, options): (Vec<Instruction>, Vec<Vec<Instruction>>, Options),
    ) -> Self {
        Self {
            instructions,
            vm: Vm::new(functions, &options, Box::new(std::io::stdout())),
            _options: options,
        }
    }
}

impl Into<Vec<u8>> for Crunch {
    #[inline(always)]
    fn into(self) -> Vec<u8> {
        self.encode()
    }
}
