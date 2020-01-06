// TODO: Are these types really needed?

macro_rules! binary_ops {
    ($($ty:ty),*) => {
        $(
            impl std::ops::Add for $ty {
                type Output = Self;

                fn add(self, other: Self) -> Self::Output {
                    Self(self.0 + other.0)
                }
            }

            impl std::ops::AddAssign for $ty {
                fn add_assign(&mut self, other: Self) {
                    self.0 += other.0;
                }
            }

            impl std::ops::Sub for $ty {
                type Output = Self;

                fn sub(self, other: Self) -> Self::Output {
                    Self(self.0 - other.0)
                }
            }

            impl std::ops::SubAssign for $ty {
                fn sub_assign(&mut self, other: Self) {
                    self.0 -= other.0;
                }
            }
        )*
    };
}

macro_rules! util_ops {
    ($([$ty:ty, $target:ty, $display:literal]),*) => {
        $(
            impl std::ops::Deref for $ty {
                type Target = $target;

                fn deref(&self) -> &Self::Target {
                    &self.0
                }
            }

            impl From<$ty> for $target {
                fn from(ty: $ty) -> $target {
                    ty.0
                }
            }

            impl From<$target> for $ty {
                fn from(target: $target) -> Self {
                    Self(target)
                }
            }

            impl std::fmt::Display for $ty {
                fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    write!(fmt, $display, self.0)
                }
            }

            impl $ty {
                #[allow(dead_code)]
                #[must_use]
                pub const fn new(inner: $target) -> Self {
                    Self(inner)
                }
            }
        )*
    };
}

binary_ops! {
    Index, Register, AllocId
}

util_ops! {
    [Index, u32, "{}"],
    [Register, u8, "{}"],
    [AllocId, usize, "{}"],
    [HeapPointer, *mut u8, "{:p}"]
}

/// The id of a currently allocated object
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[repr(transparent)]
pub struct AllocId(pub usize);

/// A pointer into the Heap
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[repr(transparent)]
pub struct HeapPointer(pub *mut u8);

impl std::fmt::Pointer for HeapPointer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:p}", self.0)
    }
}

/// The instruction index for the VM
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[repr(transparent)]
pub struct Index(pub u32);

// Add, AddAssign, Sub, SubAssign

#[derive(Copy, Clone, PartialEq, Eq)]
#[repr(transparent)]
pub struct Register(pub u8);

impl std::fmt::Debug for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Bytecode<'a>(&'a [u8]);

impl<'a> Bytecode<'a> {
    #[inline]
    pub fn validate(bytes: &'a [u8]) -> Result<Self, &'static str> {
        // TODO: Write this

        Ok(Self(bytes))
    }
}

impl<'a> std::ops::Deref for Bytecode<'a> {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        self.0
    }
}
