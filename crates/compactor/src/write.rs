use alloc::fmt::Arguments;
use crunch_error::runtime_prelude::RuntimeError;
#[cfg(feature = "std")]
use crunch_error::runtime_prelude::RuntimeErrorTy;

pub trait CrunchWrite {
    fn write_fmt(&mut self, fmt: Arguments) -> Result<(), RuntimeError>;
}

#[cfg(feature = "std")]
impl<T> CrunchWrite for T
where
    T: std::io::Write,
{
    fn write_fmt(&mut self, fmt: Arguments) -> Result<(), RuntimeError> {
        <Self as std::io::Write>::write_fmt(self, fmt).map_err(|_err| {
            RuntimeError::new(
                RuntimeErrorTy::StdoutError,
                "Failed to write to the given write target",
            )
        })
    }
}
