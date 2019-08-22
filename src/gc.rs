use std::{
    alloc, mem,
    ptr::{self, NonNull},
};

#[cfg(target_family = "unix")]
pub(crate) fn page_size() -> usize {
    let val = unsafe { libc::sysconf(libc::_SC_PAGESIZE) };

    if val <= 0 {
        panic!("could not determine page size.");
    }

    val as usize
}

#[cfg(target_family = "windows")]
pub(crate) fn page_size() -> usize {
    use std::mem::MaybeUninit;
    use winapi::um::sysinfoapi::{GetSystemInfo, SYSTEM_INFO};

    unsafe {
        let mut system_info: SYSTEM_INFO = MaybeUninit::zeroed();
        GetSystemInfo(system_info.get_mut());

        system_info.assume_init().dwPageSize
    }
}

#[derive(Debug)]
struct Root {
    base_address: NonNull<u8>,
    objects: Vec<(usize, NonNull<u8>)>,
}

struct GcAlloc {
    length: usize,
    ptr: NonNull<u8>,
}

impl Root {
    pub fn new() -> Self {
        let base_address = unsafe {
            let block = alloc::alloc(
                alloc::Layout::from_size_align(512_000_000, page_size() * 2)
                    .expect("Failed to specify memory layout"),
            );

            NonNull::new(block).expect("Newly allocated memory is null")
        };

        Self {
            base_address,
            objects: Vec::new(),
        }
    }

    pub unsafe fn alloc<T: Mark>(&mut self, obj: T) -> Result<GcHandle<T>, &'static str> {
        let offset = {
            if self.objects.len() > 0 {
                let (len, ptr) = self.objects[self.objects.len() - 1];
                if let Some(offset) = NonNull::new(
                    ptr.as_ptr()
                        .wrapping_sub(self.base_address.as_ptr() as usize)
                        .wrapping_add(len),
                ) {
                    offset
                } else {
                    return Err("Failed to allocate");
                }
            } else {
                self.base_address
            }
        };

        ptr::write(offset.as_ptr() as *mut _, obj);

        self.objects.push((mem::size_of::<T>(), offset.cast()));

        Ok(GcHandle(offset.cast()))
    }
}

#[derive(Debug)]
struct GcHandle<T: Mark>(NonNull<T>);

impl<T: Mark> GcHandle<T> {
    pub fn as_ptr(self) -> *mut T {
        self.0.as_ptr()
    }
}

pub trait Mark {
    fn mark(&mut self);
}

impl<T> Mark for T {
    fn mark(&mut self) {}
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        let mut root = Root::new();
        println!("{:?}", root);

        unsafe {
            let int = root.alloc::<u8>(10).unwrap();
            println!("{:?}", int);
            println!("{:?}", *int.as_ptr());
        }

        println!("{:?}", root);

        unsafe {
            let big_int = root.alloc::<u32>(10000).unwrap();
            println!("{:?}", big_int);
            println!("{:?}", *big_int.as_ptr());
        }
    }
}
