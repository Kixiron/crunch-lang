use crate::strings::StrInterner;

// TODO: Node interning

#[derive(Debug, Clone)]
pub struct Context {
    pub strings: StrInterner,
}

impl Context {
    #[inline]
    pub fn new() -> Self {
        Self {
            strings: StrInterner::new(),
        }
    }
}

impl Default for Context {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}
