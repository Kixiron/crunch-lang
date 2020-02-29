use super::{Gc, Index, Instruction, Result, Value, NUMBER_REGISTERS};
use std::{fmt, time::Instant};

/// The initialized options for the VM
#[derive(Debug, Copy, Clone)]
pub(crate) struct VmOptions {
    pub fault_tolerant: bool,
}

impl Default for VmOptions {
    fn default() -> Self {
        Self {
            fault_tolerant: false,
        }
    }
}

impl From<&crate::Options> for VmOptions {
    fn from(&crate::Options { fault_tolerant, .. }: &crate::Options) -> Self {
        Self { fault_tolerant }
    }
}

/// A function's stack frame, holds enough information to continue execution where it
/// last left off
#[derive(Clone)]
pub struct ReturnFrame {
    /// The frame's registers
    pub registers: [Value; NUMBER_REGISTERS + 2],
    /// The current [`Instruction`] index of the frame
    ///
    /// [`Instruction`]: crate::Instruction
    pub index: Index,
    /// The index of the function to return to
    pub function_index: u32,
    pub yield_point: Option<Index>,
}

impl fmt::Debug for ReturnFrame {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.debug_struct("ReturnFrame")
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
            .field("Index", &self.index)
            .field("function_index", &self.function_index)
            .field("yield_point", &self.yield_point)
            .finish()
    }
}

/// The Virtual Machine environment for Crunch
pub struct Vm {
    /// The active VM Registers  
    /// `registers[len - 2]` is a read-only register containing 0  
    /// `registers[len - 1]` is a read-only register containing 1  
    pub(crate) registers: [Value; NUMBER_REGISTERS + 2],
    pub(crate) read_only_registers: u64,
    /// The register snapshots for function calls
    pub(crate) return_stack: Vec<ReturnFrame>,
    /// The index of the current function
    pub(crate) current_func: u32,
    /// The current instruction index of the current function
    pub(crate) index: Index,
    /// Whether or not program execution is done
    pub(crate) finished_execution: bool,
    /// Whether or not the program is currently returning
    // TODO: Needed?
    pub(crate) returning: bool,
    /// The value of the previous operation
    pub(crate) prev_op: Value,
    /// The status of the previous comparison
    pub(crate) prev_comp: bool,
    /// The Garbage Collector
    pub(crate) gc: Gc,
    /// The options initialized with the VM
    pub(crate) options: VmOptions,
    /// The stdout that the program will print to, recommended to be [`stdout`]
    ///
    /// [`stdout`]: std::io::stdout
    pub(crate) stdout: Box<dyn std::io::Write>,
    pub(crate) start_time: Option<Instant>,
    pub(crate) finish_time: Option<Instant>,
    pub(crate) stack: Vec<Value>,
}

impl Default for Vm {
    fn default() -> Self {
        Self::new(&crate::Options::default(), Box::new(std::io::stdout()))
    }
}

impl Vm {
    /// Creates a new [`Vm`] from [`options`] and the selected `stdout`
    /// target (Required to be [`Write`])
    ///
    /// [`Vm`]: crate::Vm
    /// [`options`]: crate::Options
    /// [`Write`]: std::io::Write
    #[inline]
    #[must_use]
    pub fn new(options: &crate::Options, stdout: Box<dyn std::io::Write>) -> Self {
        let (registers, read_only_registers) = {
            let add_to_mask = |mask: &mut u64, reg: u64| {
                *mask |= 1 << reg;
            };

            let mut registers: [Value; NUMBER_REGISTERS + 2] =
                array_init::array_init(|_| Value::None);
            let mut mask = 0;

            // +2 for the adding of the extra registers
            registers[NUMBER_REGISTERS + 1] = Value::U32(0);
            add_to_mask(&mut mask, NUMBER_REGISTERS as u64 + 1);
            registers[NUMBER_REGISTERS] = Value::U32(1);
            add_to_mask(&mut mask, NUMBER_REGISTERS as u64);

            (registers, mask)
        };

        Self {
            registers,
            read_only_registers,
            return_stack: Vec::with_capacity(5),
            current_func: 0,
            index: Index(0),
            finished_execution: false,
            returning: false,
            prev_op: Value::None,
            prev_comp: false,
            gc: Gc::new(options),
            options: VmOptions::from(options),
            stdout,
            start_time: None,
            finish_time: None,
            stack: Vec::with_capacity(20),
        }
    }

    pub(crate) fn register_read_only(&self, register: u8) -> bool {
        self.read_only_registers & (1 << register as u64) != 0
    }

    // pub(crate) fn register_set_read_only(&mut self, register: u8) {
    //     self.read_only_registers |= 1 << register as u64;
    // }

    // pub(crate) fn register_unset_read_only(&mut self, register: u8) {
    //     self.read_only_registers &= !(1 << register as u64);
    // }

    /// Uses the current [`Vm`] to execute the given `functions`  
    /// Note: this means that any data left in the [`Gc`] will transfer to the new program's runtime
    ///
    /// # Errors
    ///
    /// Will return any [`RuntimeError`] thrown and not caught by the running program and program runtime
    ///
    /// [`Vm`]: crate::Vm
    /// [`Gc`]: crate::Gc
    /// [`RuntimeError`]: crate::RuntimeError
    pub fn execute(&mut self, functions: &[Vec<Instruction>]) -> Result<()> {
        self.finish_time = None;
        self.start_time = Some(Instant::now());

        while !self.finished_execution {
            functions[self.current_func as usize][*self.index as usize].execute(self)?;

            #[cfg(feature = "stepping")]
            {
                let mut t = String::new();
                std::io::stdin().read_line(&mut t).unwrap();
                match &*t.to_lowercase().trim() {
                    "exit" => break,
                    "inspect" => println!("{:#?}", self),
                    _ => {}
                }
            }
        }

        self.finish_time = Some(Instant::now());

        Ok(())
    }

    /// Gets the time taken to execute the last program ran.
    ///
    /// # Returns
    ///
    /// Returns [`Some`]([`Duration`]) if a program was fully executed and timed,
    /// and [`None`] if a program has either not been executed or is in the midst of executing
    ///
    /// [`Some`]: std::option::Option
    /// [`Duration`]: std::time::Duration
    /// [`None`]: std::option::Option
    pub fn execution_time(&self) -> Option<std::time::Duration> {
        if let (Some(start), Some(finish)) = (self.start_time, self.finish_time) {
            Some(finish.duration_since(start))
        } else {
            None
        }
    }
}

impl fmt::Debug for Vm {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.debug_struct("Vm")
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
            .field("gc", &self.gc)
            .field("options", &self.options)
            .field("start_time", &self.start_time)
            .field("finish_time", &self.finish_time)
            .finish()
    }
}
