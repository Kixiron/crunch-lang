use super::{decode_program, disassemble, encode_program, Bytecode, Instruction, Vm};
use std::path::Path;

/// The main interface to the crunch language
#[allow(missing_debug_implementations)]
pub struct Crunch {
    /// The Main function of the program
    instructions: Vec<Instruction>,
    /// The main contents of the VM
    vm: Vm,
}

/// The main usage of Crunch
impl Crunch {
    /// Execute the currently loaded program
    #[inline]
    pub fn execute(&mut self) {
        trace!("Starting Crunch Execution");

        while !self.vm.environment.finished_execution {
            trace!(
                "Executing instruction: {:?}",
                self.instructions[*self.vm.environment.index as usize]
            );

            if let Err(err) =
                self.instructions[*self.vm.environment.index as usize].execute(&mut self.vm)
            {
                err.emit();
                trace!("Finished Crunch Execution with Error");
                return;
            }
        }

        trace!("Finished Crunch Execution");
    }

    #[inline]
    pub fn run_source_file<'a>(file: &Path) {
        trace!("Running Source File: {}", file.display());

        let source = {
            use std::{fs::File, io::Read};

            let mut buf = String::new();

            let mut file = match File::open(file) {
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
            match file.file_name() {
                Some(name) => name.to_str(),
                None => None,
            },
            &source,
        );

        match parser.parse() {
            Ok(ast) => {
                let instructions = super::parser::fast_interpret(ast.0.clone());

                Self::from(instructions).execute()
            }

            // Emit parsing errors
            Err(err) => {
                let writer = codespan_reporting::term::termcolor::StandardStream::stderr(
                    codespan_reporting::term::termcolor::ColorChoice::Auto,
                );

                let config = codespan_reporting::term::Config::default();

                let mut files = codespan::Files::new();
                files.add(
                    match file.file_name() {
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

    #[inline]
    pub fn run_byte_file<'a>(file: &Path) {
        trace!("Running Compiled File: {}", file.display());

        let source = {
            use std::{fs::File, io::Read};

            let mut buf = Vec::new();

            let mut file = match File::open(file) {
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

        let bytes = match Self::validate(&source) {
            Ok(bytes) => bytes,
            Err(err) => {
                println!("Invalid Bytecode: {:?}", err);
                return;
            }
        };

        let instructions = Self::parse_bytecode(bytes);

        Self::from(instructions).execute()
    }

    /// Parse validated bytecode into the Main Function and Function Table
    #[inline]
    fn parse_bytecode<'a>(bytes: Bytecode<'a>) -> (Vec<Instruction>, Vec<Vec<Instruction>>) {
        decode_program(*bytes)
    }

    /// Validate raw bytes as valid [`Bytecode`]
    #[inline]
    pub fn validate<'a>(bytes: &'a [u8]) -> Result<Bytecode<'a>, &'static str> {
        Bytecode::validate(bytes)
    }

    /// Encode the currently loaded program as bytes
    #[inline]
    fn encode(&self) -> Vec<u8> {
        encode_program(&self.instructions, &self.vm.functions)
    }

    #[inline]
    pub fn disassemble<'a>(bytes: Bytecode<'a>) -> String {
        disassemble(*bytes)
    }
}

impl From<(Vec<Instruction>, Vec<Vec<Instruction>>)> for Crunch {
    #[inline]
    fn from((instructions, functions): (Vec<Instruction>, Vec<Vec<Instruction>>)) -> Self {
        Self {
            instructions,
            vm: Vm::new(functions),
        }
    }
}

impl std::convert::TryFrom<&[u8]> for Crunch {
    type Error = &'static str;

    #[inline]
    fn try_from(bytes: &[u8]) -> Result<Self, Self::Error> {
        let bytecode = Self::validate(bytes)?;

        Ok(Self::from(Self::parse_bytecode(bytecode)))
    }
}

impl std::convert::TryFrom<&Vec<u8>> for Crunch {
    type Error = &'static str;

    #[inline]
    fn try_from(bytes: &Vec<u8>) -> Result<Self, Self::Error> {
        let bytecode = Self::validate(bytes)?;

        Ok(Self::from(Self::parse_bytecode(bytecode)))
    }
}

impl Into<Vec<u8>> for Crunch {
    fn into(self) -> Vec<u8> {
        self.encode()
    }
}
