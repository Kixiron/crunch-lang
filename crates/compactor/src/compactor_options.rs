/// The initialized options for the VM
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct CompactorOptions {
    pub stepping: bool,
}

impl CompactorOptions {
    pub fn new(stepping: bool) -> Self {
        Self { stepping }
    }
}

impl Default for CompactorOptions {
    fn default() -> Self {
        Self { stepping: false }
    }
}
