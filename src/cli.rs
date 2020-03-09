use compactor::CompactorOptions;
use vice::ViceOptions;

use std::path::PathBuf;

#[derive(Debug, structopt::StructOpt, Clone)]
#[structopt(rename_all = "kebab")]
pub struct CrunchCli {
    /// The target file
    #[structopt(parse(from_os_str))]
    pub file: PathBuf,
    /// Activates a GC Collection cycle at every opportunity
    #[structopt(long = "--burn-gc")]
    pub burn_gc: bool,
    /// Activates detailed debug logging
    #[structopt(long = "--debug-log")]
    pub debug_log: bool,
    /// Allows some runtime errors to be ignored
    #[structopt(long = "--fault-tolerant")]
    pub fault_tolerant: bool,
    /// Overwrites the heap on a side swap
    #[structopt(long = "--overwrite-heap")]
    pub overwrite_heap: bool,
    #[structopt(long = "--heap-size", default_value = "1024")]
    pub heap_size: usize,
}

impl Default for CrunchCli {
    fn default() -> Self {
        CrunchOptionBuilder::new("").build()
    }
}

impl Into<ViceOptions> for CrunchCli {
    fn into(self) -> ViceOptions {
        ViceOptions::new()
    }
}

impl Into<CompactorOptions> for CrunchCli {
    fn into(self) -> CompactorOptions {
        CompactorOptions::new(false)
    }
}

// TODO: Document the option builder

#[derive(Debug, Clone)]
pub struct CrunchOptionBuilder {
    file: PathBuf,
    burn_gc: bool,
    debug_log: bool,
    fault_tolerant: bool,
    overwrite_heap: bool,
    heap_size: usize,
}

impl CrunchOptionBuilder {
    pub fn new(file: impl Into<PathBuf>) -> Self {
        Self {
            file: file.into(),
            burn_gc: false,
            debug_log: false,
            fault_tolerant: false,
            overwrite_heap: false,
            heap_size: 1024,
        }
    }

    #[must_use]
    pub const fn burn_gc(mut self, b: bool) -> Self {
        self.burn_gc = b;
        self
    }

    #[must_use]
    pub const fn debug_log(mut self, b: bool) -> Self {
        self.debug_log = b;
        self
    }

    #[must_use]
    pub const fn fault_tolerant(mut self, b: bool) -> Self {
        self.fault_tolerant = b;
        self
    }

    #[must_use]
    pub const fn overwrite_heap(mut self, b: bool) -> Self {
        self.overwrite_heap = b;
        self
    }

    #[must_use]
    pub const fn heap_size(mut self, heap_size: usize) -> Self {
        self.heap_size = heap_size;
        self
    }

    // Cannot make destructors const fns
    #[must_use]
    pub fn build(self) -> CrunchCli {
        CrunchCli {
            file: self.file,
            burn_gc: self.burn_gc,
            debug_log: self.debug_log,
            fault_tolerant: self.fault_tolerant,
            overwrite_heap: self.overwrite_heap,
            heap_size: self.heap_size,
        }
    }
}
