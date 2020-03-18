use crate::{
    compactor_options::CompactorOptions, instruction::Instruction, return_frame::ReturnFrame,
    value::Value, write::CrunchWrite, NUMBER_REGISTERS,
};

use alloc::{boxed::Box, format, vec::Vec};
use core::fmt;
use crunch_error::runtime_prelude::*;

/// The Virtual Machine environment for Crunch
pub struct Compactor<'a> {
    /// The active VM Registers  
    /// `registers[len - 2]` is a read-only register containing 0  
    /// `registers[len - 1]` is a read-only register containing 1  
    pub(crate) registers: [Value; NUMBER_REGISTERS + 2],
    pub(crate) read_only_registers: u64,
    /// The register snapshots for function calls
    pub(crate) return_stack: Vec<ReturnFrame>,
    pub(crate) stack: Vec<Value>,
    /// The index of the current function
    pub(crate) current_func: u32,
    /// The current instruction index of the current function
    pub(crate) index: u32,
    /// Whether or not program execution is done
    pub(crate) finished_execution: bool,
    /// Whether or not the program is currently returning
    // TODO: Needed?
    pub(crate) returning: bool,
    /// The value of the previous operation
    pub(crate) prev_op: Value,
    /// The status of the previous comparison
    pub(crate) prev_comp: bool,
    /// The options initialized with the VM
    pub(crate) options: CompactorOptions,
    /// The stdout that the program will print to, recommended to be [`stdout`]
    ///
    /// [`stdout`]: std::io::stdout
    // TODO: no_std Write target
    pub(crate) stdout: Box<&'a mut dyn CrunchWrite>,
}

impl<'a> Compactor<'a> {
    /// Creates a new [`Compactor`] from [`options`] and the selected `stdout`
    /// target
    ///
    /// [`Compactor`]: crate::Compactor
    /// [`options`]: crate::Options
    #[inline]
    #[must_use]
    pub fn new(options: CompactorOptions, stdout: Box<&'a mut dyn CrunchWrite>) -> Self {
        let (registers, read_only_registers) = {
            let add_to_mask = |mask: &mut u64, reg: u64| {
                *mask |= 1 << reg;
            };

            let mut registers: [Value; NUMBER_REGISTERS + 2] =
                array_init::array_init(|_| Value::None);
            let mut mask = 0;

            // Set the last two registers to contain `0` and `1`, as well as set them read-only
            registers[NUMBER_REGISTERS + 1] = Value::I32(0);
            add_to_mask(&mut mask, NUMBER_REGISTERS as u64 + 1);
            registers[NUMBER_REGISTERS] = Value::I32(1);
            add_to_mask(&mut mask, NUMBER_REGISTERS as u64);

            (registers, mask)
        };

        Self {
            registers,
            read_only_registers,
            return_stack: Vec::with_capacity(10),
            stack: Vec::with_capacity(20),
            current_func: 0,
            index: 0,
            finished_execution: false,
            returning: false,
            prev_op: Value::None,
            prev_comp: false,
            options: CompactorOptions::from(options),
            stdout,
        }
    }

    #[inline]
    #[must_use]
    pub fn with_stdout(stdout: Box<&'a mut dyn CrunchWrite>) -> Self {
        Self::new(CompactorOptions::default(), stdout)
    }

    #[inline]
    #[must_use]
    pub fn into_stdout(self) -> Box<&'a mut dyn CrunchWrite> {
        self.stdout
    }

    #[inline]
    pub(crate) fn register_marked_read_only(&self, register: u8) -> bool {
        self.read_only_registers & (1 << register as u64) != 0
    }

    #[allow(dead_code)]
    pub(crate) fn register_set_read_only(&mut self, register: u8) {
        self.read_only_registers |= 1 << register as u64;
    }

    #[allow(dead_code)]
    pub(crate) fn register_unset_read_only(&mut self, register: u8) {
        self.read_only_registers &= !(1 << register as u64);
    }

    /// Uses the current [`Compactor`] to execute the given `functions`  
    /// Note: this means that any data left in the [`Gc`] will transfer to the new program's runtime
    ///
    /// # Errors
    ///
    /// Will return any [`RuntimeError`] thrown and not caught by the running program and program runtime
    ///
    /// [`Compactor`]: crate::Compactor
    /// [`Gc`]: crate::Gc
    /// [`RuntimeError`]: crate::RuntimeError
    #[inline]
    pub fn execute(&mut self, functions: &[Vec<Instruction>]) -> RuntimeResult<()> {
        while !self.finished_execution {
            functions[self.current_func as usize][self.index as usize].execute(self)?;

            #[cfg(features = "std")]
            {
                if self.options.stepping {
                    let mut t = String::new();
                    std::io::stdin().read_line(&mut t).unwrap();
                    match &*t.to_lowercase().trim() {
                        "exit" => break,
                        "inspect" => println!("{:#?}", self),
                        _ => {}
                    }
                }
            }
        }

        Ok(())
    }
}

impl<'a> fmt::Debug for Compactor<'a> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.debug_struct("Compactor")
            .field(
                "registers",
                &format!(
                    "[{}]",
                    self.registers
                        .iter()
                        .map(|r| format!("{:?}", r))
                        .collect::<Vec<_>>()
                        .join(", ")
                ),
            )
            .field("return_stack", &self.return_stack)
            .field("current_func", &self.current_func)
            .field("index", &self.index)
            .field("finished_execution", &self.finished_execution)
            .field("returning", &self.returning)
            .field("prev_op", &self.prev_op)
            .field("prev_comp", &self.prev_comp)
            .field("options", &self.options)
            .finish()
    }
}
