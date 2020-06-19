use crate::llvm::{Error, ErrorKind, Result};
use std::convert::TryFrom;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
#[rustfmt::skip]
pub enum AddressSpace {
    Generic = 0,
    Global  = 1,
    Shared  = 2,
    Const   = 3,
    Local   = 4,
}

impl TryFrom<u32> for AddressSpace {
    type Error = Error;

    fn try_from(val: u32) -> Result<Self> {
        match val {
            0 => Ok(AddressSpace::Generic),
            1 => Ok(AddressSpace::Global),
            3 => Ok(AddressSpace::Shared),
            4 => Ok(AddressSpace::Const),
            5 => Ok(AddressSpace::Local),

            _ => Err(Error::new(
                "LLVM returned an unrecognized AddressSpace",
                ErrorKind::LLVMError,
            )),
        }
    }
}
