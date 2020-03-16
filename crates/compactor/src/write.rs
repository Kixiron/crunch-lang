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
    #[inline]
    fn write_fmt(&mut self, fmt: Arguments) -> Result<(), RuntimeError> {
        <Self as std::io::Write>::write_fmt(self, fmt).map_err(|_err| {
            RuntimeError::new(
                RuntimeErrorTy::StdoutError,
                "Failed to write to the given write target",
            )
        })
    }
}

#[cfg(not(feature = "std"))]
impl CrunchWrite for &mut [u8] {
    #[inline]
    fn write_fmt(&mut self, fmt: Arguments) -> Result<usize, RuntimeError> {
        let bytes = format_args!(fmt).as_bytes();

        let amt = core::cmp::min(bytes.len(), self.len());
        let (a, b) = core::mem::replace(self, &mut []).split_at_mut(amt);

        a.copy_from_slice(&bytes[..amt]);
        *self = b;

        Ok(amt)
    }
}

#[cfg(not(feature = "std"))]
impl CrunchWrite for Vec<u8> {
    #[inline]
    fn write_fmt(&mut self, fmt: Arguments) -> Result<usize, RuntimeError> {
        self.extend_from_slice(format_args!(fmt).as_bytes());
        Ok(buf.len())
    }
}
