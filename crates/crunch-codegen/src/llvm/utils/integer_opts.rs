#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Wrapping {
    NoSignedWrap,
    NoUnsignedWrap,
    None,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum DivideKind {
    Exact,
    Normal,
}
