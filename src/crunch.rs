use super::{decode_program, encode_program, Bytecode, Instruction, Registers};

/// The main interface to the crunch language
#[derive(Debug)]
pub struct Crunch {
    /// The Main function of the program
    instructions: Vec<Instruction>,
    /// The main contents of the VM
    registers: Registers,
}

/// The main usage of Crunch
impl Crunch {
    /// Execute the currently loaded program
    ///
    #[inline]
    pub fn execute(&mut self) {
        while !self.registers.environment().finished_execution() {
            log::trace!(
                "Executing Instruction {:?}",
                self.instructions[*self.registers.environment().index() as usize]
            );
            self.instructions[*self.registers.environment().index() as usize]
                .execute(&mut self.registers);
        }
    }

    /// Parse validated bytecode into the Main Function and Function Table
    #[cfg(feature = "bytecode")]
    #[inline]
    pub fn parse<'a>(bytes: Bytecode<'a>) -> (Vec<Instruction>, Vec<Vec<Instruction>>) {
        decode_program(*bytes)
    }

    /// Validate raw bytes as valid [`Bytecode`]
    #[cfg(feature = "bytecode")]
    #[inline]
    pub fn validate<'a>(bytes: &'a [u8]) -> Result<Bytecode<'a>, &'static str> {
        Bytecode::validate(bytes)
    }

    /// Encode the currently loaded program as bytes
    #[cfg(feature = "bytecode")]
    #[inline]
    pub fn encode(&self) -> Vec<u8> {
        encode_program(&self.instructions, self.registers.functions())
    }
}

impl From<(Vec<Instruction>, Vec<Vec<Instruction>>)> for Crunch {
    #[inline]
    fn from((instructions, functions): (Vec<Instruction>, Vec<Vec<Instruction>>)) -> Self {
        Self {
            instructions,
            registers: Registers::new(functions),
        }
    }
}

impl std::convert::TryFrom<&[u8]> for Crunch {
    type Error = &'static str;

    #[inline]
    fn try_from(bytes: &[u8]) -> Result<Self, Self::Error> {
        let bytecode = Self::validate(bytes)?;

        Ok(Self::from(Self::parse(bytecode)))
    }
}

impl std::convert::TryFrom<&Vec<u8>> for Crunch {
    type Error = &'static str;

    #[inline]
    fn try_from(bytes: &Vec<u8>) -> Result<Self, Self::Error> {
        let bytecode = Self::validate(bytes)?;

        Ok(Self::from(Self::parse(bytecode)))
    }
}

impl Into<Vec<u8>> for Crunch {
    fn into(self) -> Vec<u8> {
        self.encode()
    }
}
