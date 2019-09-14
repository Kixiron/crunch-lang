use require_unsafe_in_body::require_unsafe_in_bodies;
use std::{
    alloc,
    cmp::Ordering,
    collections::HashMap,
    fmt,
    marker::PhantomData,
    mem,
    ptr::{self, NonNull},
};

#[inline]
#[cfg(target_family = "unix")]
pub(crate) fn page_size() -> usize {
    let val = unsafe { libc::sysconf(libc::_SC_PAGESIZE) };

    if val <= 0 {
        panic!("could not determine page size.");
    }

    val as usize
}

#[inline]
#[cfg(target_family = "windows")]
pub(crate) fn page_size() -> usize {
    use std::mem::MaybeUninit;
    use winapi::um::sysinfoapi::{GetSystemInfo, SYSTEM_INFO};

    unsafe {
        let mut system_info: MaybeUninit<SYSTEM_INFO> = MaybeUninit::zeroed();
        GetSystemInfo(system_info.as_mut_ptr());

        system_info.assume_init().dwPageSize as usize
    }
}

pub trait Mark {
    fn marked(&self) -> bool;
    fn mark(&mut self);
    fn unmark(&mut self);
}

#[derive(Debug)]
pub struct GcValue<T: Mark + ?Sized> {
    ptr: *mut T,
}

#[derive(Debug)]
pub struct Gc {
    left_heap: Region,
    right_heap: Region,
    allocations: HashMap<usize, (Region, GcValue<dyn Mark>)>,
    roots: Vec<usize>,
}

impl Gc {
    #[inline]
    pub fn new() -> Self {
        /// 64 megabytes per heap side
        const HEAP_SIZE: usize = 1000 * 1000 * 64;

        let (left_heap, right_heap) = {
            let page_size = {
                let size = page_size();
                assert!(size > 0);

                size
            };

            let layout = alloc::Layout::from_size_align(HEAP_SIZE, page_size)
                .expect("Failed to create GC memory block layout");

            let left = {
                let alloc = unsafe { alloc::alloc_zeroed(layout) } as usize;

                Region::new(alloc, alloc + HEAP_SIZE)
            };

            let right = {
                let alloc = unsafe { alloc::alloc_zeroed(layout) } as usize;

                Region::new(alloc, alloc + HEAP_SIZE)
            };

            (left, right)
        };

        Self {
            left_heap,
            right_heap,
            allocations: HashMap::new(),
            roots: Vec::new(),
        }
    }

    #[inline]
    pub fn alloc_num<T: Mark>(&mut self, count: usize, root: bool) -> GcHandle<T> {
        let region = {
            let len = if self.allocations.len() > 0 {
                self.allocations.len() - 1
            } else {
                0
            };

            Region::new(len, len + mem::size_of::<T>() * count)
        };

        let mut key = region.start.to_usize();
        loop {
            if !self.allocations.contains_key(&key) {
                self.allocations.insert(key, (region, 0));

                break GcHandle::new(key);
            }
            key += 1;
        }
    }

    #[inline]
    pub fn mark(&mut self) {
        for root in &self.roots {
            if let Some(root) = self.allocations.get(&root) {
                unsafe {
                    (*(root.0.start.to_mut_ptr())).mark();
                }
            }
        }
    }
}

#[derive(Debug)]
pub struct GcHandle<T>(Address, PhantomData<T>);

impl<T> GcHandle<T> {
    pub fn new<U: Into<Address>>(addr: U) -> Self {
        Self(addr.into(), PhantomData)
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Region {
    start: Address,
    end: Address,
}

impl Region {
    pub fn new<T: Into<Address>>(start: T, end: T) -> Region {
        let (start, end) = (start.into(), end.into());
        debug_assert!(start <= end);

        Region { start, end }
    }

    #[inline(always)]
    pub fn contains(&self, addr: Address) -> bool {
        self.start <= addr && addr < self.end
    }

    #[inline(always)]
    pub fn valid_top(&self, addr: Address) -> bool {
        self.start <= addr && addr <= self.end
    }

    #[inline(always)]
    pub fn size(&self) -> usize {
        self.end.to_usize() - self.start.to_usize()
    }

    #[inline(always)]
    pub fn empty(&self) -> bool {
        self.start == self.end
    }

    #[inline(always)]
    pub fn disjunct(&self, other: &Region) -> bool {
        self.end <= other.start || self.start >= other.end
    }

    #[inline(always)]
    pub fn overlaps(&self, other: &Region) -> bool {
        !self.disjunct(other)
    }

    #[inline(always)]
    pub fn fully_contains(&self, other: &Region) -> bool {
        self.contains(other.start) && self.valid_top(other.end)
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct Address(usize);

impl Address {
    #[inline(always)]
    pub fn from(val: usize) -> Address {
        Address(val)
    }

    #[inline(always)]
    pub fn region_start(self, size: usize) -> Region {
        Region::new(self, self.offset(size))
    }

    #[inline(always)]
    pub fn offset_from(self, base: Address) -> usize {
        debug_assert!(self >= base);

        self.to_usize() - base.to_usize()
    }

    #[inline(always)]
    pub fn offset(self, offset: usize) -> Address {
        Address(self.0 + offset)
    }

    #[inline(always)]
    pub fn sub(self, offset: usize) -> Address {
        Address(self.0 - offset)
    }

    #[inline(always)]
    pub fn add_ptr(self, words: usize) -> Address {
        Address(self.0 + words * std::mem::size_of::<usize>())
    }

    #[inline(always)]
    pub fn sub_ptr(self, words: usize) -> Address {
        Address(self.0 - words * std::mem::size_of::<usize>())
    }

    #[inline(always)]
    pub fn to_usize(self) -> usize {
        self.0
    }

    #[inline(always)]
    pub fn from_ptr<T>(ptr: *const T) -> Address {
        Address(ptr as usize)
    }

    #[inline(always)]
    pub fn to_ptr<T>(&self) -> *const T {
        self.0 as *const T
    }

    #[inline(always)]
    pub fn to_mut_ptr<T>(&self) -> *mut T {
        self.0 as *const T as *mut T
    }

    #[inline(always)]
    pub fn null() -> Address {
        Address(0)
    }

    #[inline(always)]
    pub fn is_null(self) -> bool {
        self.0 == 0
    }

    #[inline(always)]
    pub fn is_non_null(self) -> bool {
        self.0 != 0
    }
}

impl fmt::Display for Address {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:p}", self.to_ptr::<usize>())
    }
}

impl fmt::Debug for Address {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:p}", self.to_ptr::<usize>())
    }
}

impl PartialOrd for Address {
    fn partial_cmp(&self, other: &Address) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Address {
    fn cmp(&self, other: &Address) -> Ordering {
        self.to_usize().cmp(&other.to_usize())
    }
}

impl From<usize> for Address {
    fn from(val: usize) -> Address {
        Address(val)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn gc() {
        let mut gc = Gc::new();
        println!("{:?}", gc);

        let handle = gc.alloc_num::<usize>(10, true);
        println!("{:?}", handle);
        println!("{:?}", gc);
    }
}
