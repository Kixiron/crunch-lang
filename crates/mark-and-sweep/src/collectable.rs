use super::*;
use crate::{AllocId, Result, RuntimeError, RuntimeErrorTy};
use num_bigint::{BigInt, BigUint};
use std::{marker::PhantomData, mem};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Heap<T: Collectable + Sized> {
    id: AllocId,
    __ty: PhantomData<T>,
}

impl<T: Collectable + Sized> Heap<T> {
    #[must_use]
    pub fn new(id: AllocId) -> Self {
        Heap {
            id,
            __ty: PhantomData,
        }
    }

    pub fn id(&self) -> AllocId {
        self.id
    }

    pub fn root(&self, gc: &mut Gc) -> Result<()> {
        gc.add_root(self.id);

        Ok(())
    }

    pub fn unroot(&self, gc: &mut Gc) -> Result<()> {
        gc.remove_root(self.id)
    }

    pub fn add_child<C: Collectable>(&self, child: &Heap<C>, gc: &mut Gc) -> Result<()> {
        gc.add_child(self.id, child.id)
    }

    pub fn fetch(&self, gc: &Gc) -> Result<<T as Collectable>::Owned> {
        T::fetch(self, gc)
    }

    pub fn drop(&self, gc: &Gc) -> Result<()> {
        T::drop(self, gc)
    }
}

pub trait Collectable: Sized {
    /// The owned version of the given type
    type Owned;
    /// Store an object on the heap
    fn alloc(self, gc: &mut Gc) -> Result<Heap<Self>>;
    /// Fetch the object from the heap
    fn fetch(stub: &Heap<Self>, gc: &Gc) -> Result<Self::Owned>;
    /// Called prior to the object being collected, can be re-implemented if needed
    fn drop(_stub: &Heap<Self>, _gc: &Gc) -> Result<()> {
        Ok(())
    }
}

macro_rules! collectable_int {
    ($($int:ty),*) => {
        $(
            impl Collectable for $int {
                type Owned = Self;

                #[inline]
                fn alloc(self, gc: &mut Gc) -> Result<Heap<Self>> {
                    let id = gc.allocate_heap(self)?;

                    Ok(Heap::new(id))
                }

                #[inline]
                fn fetch(stub: &Heap<Self>, gc: &Gc) -> Result<Self::Owned> {
                    use std::convert::TryInto;

                    let bytes = gc.fetch_bytes(stub.id)?;

                    let array: [u8; mem::size_of::<Self>()] =
                        bytes.try_into().map_err(|_| RuntimeError {
                            ty: RuntimeErrorTy::GcError,
                            message: format!(
                                "Attempted to make bytes of len {} into usize of len {}",
                                bytes.len(),
                                std::mem::size_of::<Self>()
                            ),
                        })?;

                    let int = Self::from_le_bytes(array);

                    Ok(int)
                }
            }
        )*
    };
}

collectable_int! {
    isize, i64, i32, i16, i8,
    usize, u64, u32, u16, u8
}

impl Collectable for &str {
    type Owned = String;

    #[inline]
    fn alloc(self, gc: &mut Gc) -> Result<Heap<Self>> {
        let bytes = self.as_bytes();
        let (ptr, id) = gc.allocate_zeroed(bytes.len())?;

        unsafe {
            let target = slice::from_raw_parts_mut(*ptr, bytes.len());
            target.copy_from_slice(bytes);
        }

        Ok(Heap::new(id))
    }

    #[inline]
    fn fetch(stub: &Heap<Self>, gc: &Gc) -> Result<Self::Owned> {
        let bytes = gc.fetch_bytes(stub.id)?.to_vec();
        let string = String::from_utf8(bytes).map_err(|_| RuntimeError {
            ty: RuntimeErrorTy::InvalidString,
            message: "The string retrieved from the GC was invalid UTF-8".to_string(),
        })?;

        Ok(string)
    }
}

impl Collectable for BigInt {
    type Owned = Self;

    fn alloc(self, gc: &mut Gc) -> Result<Heap<Self>> {
        let bytes = self.to_signed_bytes_le();
        let (ptr, id) = gc.allocate_zeroed(bytes.len())?;

        unsafe {
            let target = slice::from_raw_parts_mut(*ptr, bytes.len());
            target.copy_from_slice(&bytes);
        }

        Ok(Heap::new(id))
    }

    fn fetch(stub: &Heap<Self>, gc: &Gc) -> Result<Self::Owned> {
        Ok(BigInt::from_signed_bytes_be(gc.fetch_bytes(stub.id)?))
    }
}

impl Collectable for BigUint {
    type Owned = Self;

    fn alloc(self, gc: &mut Gc) -> Result<Heap<Self>> {
        let bytes = self.to_bytes_le();
        let (ptr, id) = gc.allocate_zeroed(bytes.len())?;

        unsafe {
            let target = slice::from_raw_parts_mut(*ptr, bytes.len());
            target.copy_from_slice(&bytes);
        }

        Ok(Heap::new(id))
    }

    fn fetch(stub: &Heap<Self>, gc: &Gc) -> Result<Self::Owned> {
        Ok(BigUint::from_bytes_be(gc.fetch_bytes(stub.id)?))
    }
}

/*
impl<T: Collectable> Collectable for Vec<T> {
    type Owned = Self;

    fn alloc(self, gc: &mut Gc) -> Result<Heap<Self>> {
        let size = mem::size_of::<T>() * self.len();
        let (ptr, id) = gc.allocate(size)?;
        let ptr = *ptr as *mut T;

        // Safety: Doing the same thing as `Gc::allocate_zeroed`
        unsafe { ptr.write_bytes(0x00, self.len()) };

        let mut offset = 0;
        for val in self {
            ptr.wrapping_offset(offset).write(val);
        }

        Ok(Heap::new(id, size))
    }

    fn fetch(stub: &Heap<Self>, gc: &Gc) -> Result<Self::Owned> {
        let ptr = unsafe { *gc.get_ptr(stub.id)? as *mut T };
        let number_elements = stub.size / mem::size_of::<T>();
        let vec = Vec::from_raw_parts(ptr, number_elements, number_elements); // Points into the GC, could be invalidated at any time

        Ok(vec)
    }
}
*/

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn collectable() -> Result<()> {
        let mut gc = super::Gc::new(&crate::OptionBuilder::new("./gc_test").build());

        let int = 1000_usize;
        let stub = int.alloc(&mut gc)?;
        assert_eq!(int, stub.fetch(&mut gc)?);

        stub.root(&mut gc)?;
        assert_eq!(int, stub.fetch(&mut gc)?);

        gc.collect();
        assert_eq!(int, stub.fetch(&mut gc)?);

        stub.unroot(&mut gc)?;
        assert_eq!(int, stub.fetch(&mut gc)?);

        gc.collect();
        assert!(stub.fetch(&mut gc).is_err());

        Ok(())
    }
}
