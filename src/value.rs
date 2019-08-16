use super::StringPointer;
use derive_more::Display;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Display)]
pub enum Value {
    #[display(fmt = "{}", _0)]
    Int(i32),
    #[display(fmt = "StringPointer to StrReg {}", _0)]
    Str(StringPointer),
    #[display(fmt = "{}", _0)]
    Bool(bool),
    #[display(fmt = "{}", _0)]
    JumpLocation(u16),
    #[display(fmt = "Empty Register")]
    None,
}
