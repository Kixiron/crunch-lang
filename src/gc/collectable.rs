use super::*;
use crate::{AllocId, Result, RuntimeError, RuntimeErrorTy};
use std::{marker::PhantomData, mem::ManuallyDrop};

#[derive(Debug, Clone, Copy)]
pub struct GcVec<T> {
    pub id: AllocId,
    pub len: usize,
    pub __inner__: std::marker::PhantomData<T>,
}

impl<T> GcVec<T>
where
    T: Clone + PartialEq + std::fmt::Debug,
{
    pub fn new(vec: Vec<T>, gc: &mut Gc) -> Result<Self> {
        use std::mem::size_of;

        let len = vec.len();

        let id = gc.alloc(size_of::<T>() * len)?;
        #[cfg(debug_assertions)]
        unsafe {
            gc.write(id, {
                let ptr = vec.as_ptr() as *const u8;
                std::slice::from_raw_parts(ptr, vec.len() * std::mem::size_of::<T>())
            })?;
        }
        #[cfg(not(debug_assertions))]
        unsafe {
            gc.write(id, &vec)?;
        }
        gc.add_root(id);

        #[cfg(debug_assertions)]
        {
            let raw = gc.fetch::<&[u8]>(id)?;
            let v = unsafe {
                Vec::from_raw_parts(
                    raw.as_ptr() as *mut T,
                    raw.len() / size_of::<T>(),
                    raw.len() / size_of::<T>(),
                )
            };

            debug_assert_eq!(vec, v);
            std::mem::forget(v);
        }

        Ok(Self {
            id,
            len,
            __inner__: std::marker::PhantomData,
        })
    }

    pub fn to_vec<'a>(&self, gc: &'a Gc) -> Result<BorrowedOwned<'a, Vec<T>>> {
        use std::mem::size_of;

        let raw = gc.fetch::<&[u8]>(self.id)?;
        Ok(unsafe {
            BorrowedOwned::new(Vec::from_raw_parts(
                raw.as_ptr() as *mut T,
                raw.len() / size_of::<T>(),
                raw.len() / size_of::<T>(),
            ))
        })
    }

    pub fn drop(self, gc: &mut Gc) -> Result<()> {
        gc.remove_root(self.id)?;

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct BorrowedOwned<'a, T>(ManuallyDrop<T>, PhantomData<&'a T>);

impl<'a, T> BorrowedOwned<'a, T> {
    pub fn new(val: T) -> Self {
        Self(ManuallyDrop::new(val), PhantomData)
    }
}

impl<'a, T> std::ops::Deref for BorrowedOwned<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}

/// Generates add, sub, etc. functions
macro_rules! ops {
    ($ty:ty, $to:tt, $([$name:tt, $op:tt]),*) => {
        impl $ty {
            $(
                pub fn $name(self, other: Self, gc: &mut Gc) -> Result<Self> {
                    let (left, right) = ((*self.$to(&gc)?).to_owned(), (*other.$to(&gc)?).to_owned());
                    let result = Self::new(left $op right, gc)?;

                    self.drop(gc)?;
                    other.drop(gc)?;

                    Ok(result)
                }
            )*
        }
    }
}

/// Generates new_adding, new_subbing, etc. functions
macro_rules! new_ops {
    // Signed
    (S$ty:ty, $to:tt, $internal:tt, $trait:path, $([$name:tt, $op:tt, $bounds:path]),*) => {
        impl $ty {
            $(
                pub fn $name<I, T>(int: I, right: T, gc: &mut Gc) -> Result<Self>
                where
                    I: $trait,
                    $internal: $bounds,
                {
                    let int = if let Some(mut int) = int.to_bigint() {
                        int $op right;
                        int.to_signed_bytes_be()
                    } else {
                        return Err(RuntimeError {
                            ty: RuntimeErrorTy::InvalidInt,
                            message: "Integer is not a valid integer".to_string(),
                        });
                    };
                    let len = int.len();

                    let id = gc.alloc(len)?;
                    #[cfg(debug_assertions)]
                    unsafe {
                        gc.write(id, &int)?;
                    }
                    #[cfg(not(debug_assertions))]
                    unsafe {
                        gc.write(id, &int)?;
                    }
                    gc.add_root(id);

                    #[cfg(debug_assertions)]
                    debug_assert_eq!(
                        $internal::from_signed_bytes_be(&int),
                        $internal::from_signed_bytes_be(gc.fetch::<&[u8]>(id).expect("Value does not exist")),
                    );

                    Ok(Self { id, len })
                }
            )*
        }
    };

    // Unsigned
    (U$ty:ty, $to:tt, $internal:tt, $trait:path, $([$name:tt, $op:tt, $bounds:path]),*) => {
        impl $ty {
            $(
                pub fn $name<I, T>(int: I, right: T, gc: &mut Gc) -> Result<Self>
                where
                    I: $trait,
                    $internal: $bounds,
                {
                    let int = if let Some(mut int) = int.to_biguint() {
                        int $op right;
                        int.to_bytes_be()
                    } else {
                        return Err(RuntimeError {
                            ty: RuntimeErrorTy::InvalidInt,
                            message: "Integer is not a valid integer".to_string(),
                        });
                    };
                    let len = int.len();

                    let id = gc.alloc(len)?;
                    #[cfg(debug_assertions)]
                    unsafe {
                        gc.write(id, &int)?;
                    }
                    #[cfg(not(debug_assertions))]
                    unsafe {
                        gc.write(id, &int)?;
                    }
                    gc.add_root(id);

                    #[cfg(debug_assertions)]
                    debug_assert_eq!(
                        $internal::from_bytes_be(&int),
                        $internal::from_bytes_be(gc.fetch::<&[u8]>(id).expect("Value does not exist")),
                    );

                    Ok(Self { id, len })
                }
            )*
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct GcBigInt {
    pub id: AllocId,
    pub len: usize,
}

impl GcBigInt {
    pub fn new(int: impl num_bigint::ToBigInt, gc: &mut Gc) -> Result<Self> {
        let int = if let Some(int) = int.to_bigint() {
            int.to_signed_bytes_be()
        } else {
            return Err(RuntimeError {
                ty: RuntimeErrorTy::InvalidInt,
                message: "Integer is not a valid integer".to_string(),
            });
        };
        let len = int.len();

        let id = gc.alloc(len)?;
        #[cfg(debug_assertions)]
        unsafe {
            gc.write(id, &int)?;
        }
        #[cfg(not(debug_assertions))]
        unsafe {
            gc.write(id, &int)?;
        }
        gc.add_root(id);

        #[cfg(debug_assertions)]
        debug_assert_eq!(
            num_bigint::BigInt::from_signed_bytes_be(&int),
            num_bigint::BigInt::from_signed_bytes_be(
                gc.fetch::<&[u8]>(id).expect("Value does not exist")
            ),
        );

        Ok(Self { id, len })
    }

    pub fn bit_not<'a>(self, gc: &'a mut Gc) -> Result<Self> {
        let int = self.to_int(gc)?;
        unsafe {
            gc.write(self.id, &(!(*int).clone()).to_signed_bytes_le())?;
        }

        Ok(self)
    }

    pub fn to_int<'a>(&self, gc: &'a Gc) -> Result<BorrowedOwned<'a, num_bigint::BigInt>> {
        Ok(BorrowedOwned::new(
            num_bigint::BigInt::from_signed_bytes_be(gc.fetch::<&[u8]>(self.id)?),
        ))
    }

    pub fn drop(self, gc: &mut Gc) -> Result<()> {
        gc.remove_root(self.id)?;

        Ok(())
    }
}

use num_bigint::BigInt;
new_ops!(S
    GcBigInt, to_int, BigInt, num_bigint::ToBigInt,
    [new_adding, +=, std::ops::AddAssign<T>],
    [new_subtracting, -=, std::ops::SubAssign<T>],
    [new_multiplying, *=, std::ops::MulAssign<T>],
    [new_dividing, /=, std::ops::DivAssign<T>],
    [new_shifting_right, >>=, std::ops::ShrAssign<T>],
    [new_shifting_left, <<=, std::ops::ShlAssign<T>]
);

ops!(
    GcBigInt, to_int,
    [add, +], [sub, -],
    [mult, *], [div, /],
    [bit_or, |], [bit_and, &],
    [bit_xor, ^]
);

#[derive(Debug, Clone, Copy)]
pub struct GcBigUint {
    pub id: AllocId,
    pub len: usize,
}

impl GcBigUint {
    pub fn new(int: impl num_bigint::ToBigUint, gc: &mut Gc) -> Result<Self> {
        let int = if let Some(int) = int.to_biguint() {
            int.to_bytes_be()
        } else {
            return Err(RuntimeError {
                ty: RuntimeErrorTy::InvalidInt,
                message: "Integer is not a valid integer".to_string(),
            });
        };
        let len = int.len();

        let id = gc.alloc(len)?;
        #[cfg(debug_assertions)]
        unsafe {
            gc.write(id, &int)?;
        }
        #[cfg(not(debug_assertions))]
        unsafe {
            gc.write(id, &int)?;
        }
        gc.add_root(id);

        #[cfg(debug_assertions)]
        debug_assert_eq!(
            num_bigint::BigUint::from_bytes_be(&int),
            num_bigint::BigUint::from_bytes_be(
                gc.fetch::<&[u8]>(id).expect("Value does not exist")
            ),
        );

        Ok(Self { id, len })
    }

    pub fn bit_not<'a>(self, _gc: &'a mut Gc) -> Result<Self> {
        // let int = self.to_uint(gc)?;
        // unsafe {
        //     gc.write(self.id, !*int, None)?;
        // }
        //
        // Ok(self)

        unimplemented!("`std::ops::Not` is not implemented for `num_bigint::BitUint`")
    }

    pub fn to_uint<'a>(&self, gc: &'a Gc) -> Result<BorrowedOwned<'a, num_bigint::BigUint>> {
        Ok(BorrowedOwned::new(num_bigint::BigUint::from_bytes_be(
            gc.fetch::<&[u8]>(self.id)?,
        )))
    }

    pub fn drop(self, gc: &mut Gc) -> Result<()> {
        gc.remove_root(self.id)?;

        Ok(())
    }
}

use num_bigint::BigUint;
new_ops!(U
    GcBigUint, to_uint, BigUint, num_bigint::ToBigUint,
    [new_adding, +=, std::ops::AddAssign<T>],
    [new_subtracting, -=, std::ops::SubAssign<T>],
    [new_multiplying, *=, std::ops::MulAssign<T>],
    [new_dividing, /=, std::ops::DivAssign<T>],
    [new_shifting_right, >>=, std::ops::ShrAssign<T>],
    [new_shifting_left, <<=, std::ops::ShlAssign<T>]
);

ops!(
    GcBigUint, to_uint,
    [add, +], [sub, -],
    [mult, *], [div, /],
    [bit_or, |], [bit_and, &],
    [bit_xor, ^]
);

#[derive(Debug, Clone, Copy)]
pub struct GcStr {
    pub id: AllocId,
    pub len: usize,
    pub capacity: usize,
}

impl GcStr {
    pub fn new(string: impl AsRef<str>, gc: &mut Gc) -> Result<Self> {
        let string = string.as_ref();

        let id = gc.alloc(string.as_bytes().len())?;
        // FIXME: If the normal `write` function is used, it does not work,
        // as the sizes are always mis-aligned
        unsafe {
            gc.write(id, string.as_bytes())?;
        }
        gc.add_root(id);

        debug_assert_eq!(
            string,
            std::str::from_utf8(gc.fetch::<&[u8]>(id).expect("Value does not exist"))
                .expect("Not valid utf-8")
        );

        Ok(Self {
            id,
            len: string.len(),
            capacity: string.len(),
        })
    }

    pub fn new_adding(left: impl AsRef<str>, right: impl AsRef<str>, gc: &mut Gc) -> Result<Self> {
        let (left, right) = (left.as_ref(), right.as_ref());

        let id = gc.alloc(left.len() + right.len())?;
        unsafe {
            gc.write(id, &{
                let mut vec = Vec::with_capacity(left.len() + right.len());
                vec.extend_from_slice(left.as_bytes());
                vec.extend_from_slice(right.as_bytes());
                vec
            })?;
        }
        gc.add_root(id);

        debug_assert_eq!(
            {
                let mut left = left.to_string();
                left.push_str(right);
                left
            },
            std::str::from_utf8(gc.fetch::<&[u8]>(id).expect("Value does not exist"))
                .expect("Not valid utf-8")
        );

        Ok(Self {
            id,
            len: left.len() + right.len(),
            capacity: left.len() + right.len(),
        })
    }

    pub fn add(self, other: Self, gc: &mut Gc) -> Result<Self> {
        // TODO: I don't like allocating, make it not
        let left = self.to_str(gc)?.to_string();
        let right = self.to_str(gc)?.to_string();

        let new = Self::new_adding(left, right, gc)?;

        self.drop(gc)?;
        other.drop(gc)?;

        Ok(new)
    }

    pub fn to_str<'a>(&self, gc: &'a Gc) -> Result<&'a str> {
        debug_assert!(std::str::from_utf8(
            gc.fetch::<&[u8]>(self.id).expect("Value does not exist")
        )
        .is_ok());

        unsafe { Ok(std::str::from_utf8_unchecked(gc.fetch::<&[u8]>(self.id)?)) }
    }

    pub fn drop(self, gc: &mut Gc) -> Result<()> {
        gc.remove_root(self.id)?;

        Ok(())
    }
}

#[derive(Debug)]
pub struct GcStruct {
    id: AllocId,
    len: usize,
}

#[derive(Debug)]
pub struct CrunchStruct {
    members: Vec<((AllocId, usize), (AllocId, usize))>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Stub {
    id: AllocId,
    size: usize,
}

pub trait Collectable: Sized {
    fn allocate(&self, gc: &mut Gc) -> Result<Stub>;
    fn fetch(stub: &Stub, gc: &Gc) -> Result<Self>;

    fn root(stub: &Stub, gc: &mut Gc) -> Result<()> {
        gc.add_root(stub.id);

        Ok(())
    }

    fn unroot(stub: &Stub, gc: &mut Gc) -> Result<()> {
        gc.remove_root(stub.id)
    }

    fn add_child(stub: &Stub, child: &Stub, gc: &mut Gc) -> Result<()> {
        gc.add_child(stub.id, child.id)
    }
}

impl Collectable for usize {
    fn allocate(&self, gc: &mut Gc) -> Result<Stub> {
        let bytes = &self.to_le_bytes();

        let id = gc.alloc(bytes.len())?;
        unsafe {
            gc.write(id, bytes)?;
        }

        Ok(Stub {
            id,
            size: bytes.len(),
        })
    }

    fn fetch(stub: &Stub, gc: &Gc) -> Result<Self> {
        use std::convert::TryInto;

        let bytes = gc.fetch::<usize>(stub.id)?;
        let int = usize::from_le_bytes(bytes.try_into().map_err(|_| RuntimeError {
            ty: RuntimeErrorTy::GcError,
            message: format!(
                "Attempted to make bytes of len {} into usize of len {}",
                bytes.len(),
                std::mem::size_of::<usize>()
            ),
        })?);

        Ok(int)
    }
}

#[test]
fn tadsfa() -> Result<()> {
    simple_logger::init().unwrap();

    let mut gc = super::Gc::new(&crate::OptionBuilder::new("./gc_test").build());

    let string = "Test";
    /*
    let stub = string.allocate(&mut gc).unwrap();
    println!("here");
    assert_eq!("Test", *<&str>::fetch(&stub, &mut gc)?);

    println!("here");
    <&str>::root(&stub, &mut gc).unwrap();
    println!("here");
    assert_eq!("Test", *<&str>::fetch(&stub, &mut gc)?);

    println!("here");
    gc.collect().unwrap();
    println!("here");
    assert_eq!("Test", *<&str>::fetch(&stub, &mut gc)?);

    println!("here");
    <&str>::unroot(&stub, &mut gc).unwrap();
    println!("here");
    assert_eq!("Test", *<&str>::fetch(&stub, &mut gc)?);

    println!("here");
    gc.collect().unwrap();
    println!("here");
    assert!(<&str>::fetch(&stub, &mut gc).is_err());
    */

    Ok(())
}
