use derive_more::{Add, AddAssign, Constructor, From, Into, Mul, MulAssign, Sub, SubAssign};
use shrinkwraprs::Shrinkwrap;
use std::{alloc, mem, ptr, slice};

/// 64mb per half of heap
const HEAP_HALF_SIZE: usize = 1000;

/// Gets the memory page size
#[inline]
#[cfg(target_family = "unix")]
pub(crate) fn page_size() -> usize {
    let val = unsafe { libc::sysconf(libc::_SC_PAGESIZE) };

    if val <= 0 {
        panic!("could not determine page size.");
    }

    trace!("Memory Page Size: {}", val);

    val as usize
}

/// Gets the memory page size
#[inline]
#[cfg(target_family = "windows")]
pub(crate) fn page_size() -> usize {
    use std::mem::MaybeUninit;
    use winapi::um::sysinfoapi::{GetSystemInfo, SYSTEM_INFO};

    let val = unsafe {
        let mut system_info: MaybeUninit<SYSTEM_INFO> = MaybeUninit::zeroed();
        GetSystemInfo(system_info.as_mut_ptr());

        system_info.assume_init().dwPageSize as usize
    };

    if val == 0 {
        panic!("could not determine page size.");
    }

    trace!("Memory Page Size: {}", val);

    val
}

/// The options for an initialized GC
#[derive(Debug, Copy, Clone)]
pub struct GcOptions {
    /// Activates a GC collect at every opportunity
    pub burn_gc: bool,
    /// Overwrites the heap on a side swap
    pub overwrite_heap: bool,
}

impl From<&crate::Options> for GcOptions {
    fn from(options: &crate::Options) -> Self {
        Self {
            burn_gc: options.burn_gc,
            overwrite_heap: options.overwrite_heap,
        }
    }
}

/// The Crunch Garbage Collector
#[derive(Debug, Clone)]
pub struct Gc {
    /// The current root objects
    roots: Vec<GcValue>,
    /// The left heap
    left: HeapPointer,
    /// The right heap
    right: HeapPointer,
    /// The start of free memory
    latest: HeapPointer,
    /// The heap side currently in use
    current_side: Side,
    /// A vector of each allocation's id and current pointer
    allocations: Vec<(AllocId, HeapPointer)>,
    options: GcOptions,
}

impl Gc {
    /// Create a new GC instance
    pub fn new(options: &crate::Options) -> Self {
        trace!("Initializing GC");

        let (left, right) = {
            // Get the memory page size
            let page_size = {
                let size = page_size();
                assert!(size > 0);
                size
            };

            // Create the layout for a heap half
            let layout = alloc::Layout::from_size_align(HEAP_HALF_SIZE, page_size)
                .expect("Failed to create GC memory block layout");

            // Left heap
            let left = HeapPointer::new(unsafe { alloc::alloc_zeroed(layout) });
            trace!("Left Heap: {:p}", left);
            // Right heap
            let right = HeapPointer::new(unsafe { alloc::alloc_zeroed(layout) });
            trace!("Right Heap: {:p}", right);

            (left, right)
        };

        Self {
            left,
            right,
            allocations: Vec::new(),
            roots: Vec::new(),
            current_side: Side::Left,
            latest: left,
            options: GcOptions::from(options),
        }
    }

    /// Allocate the space for an object
    pub fn allocate(&mut self, size: usize) -> Option<(GcValue, AllocId)> {
        trace!("Allocating size {}", size);

        if self.options.burn_gc {
            self.collect();
        }

        let (block_end, block_start) = (
            *self.latest as usize + size + 1,
            (*self.latest as usize + 1) as *mut u8,
        );
        let heap = self.get_side();

        // If the object is too large return None
        if block_end - *heap as usize > HEAP_HALF_SIZE {
            self.collect(); // Collect garbage
            return None;
        }

        // Generate the Id of the new allocation based off of its pointer
        let mut new_id: AllocId = AllocId::new(block_start as usize >> 2);
        loop {
            if !self.allocations.iter().any(|(id, _)| *id == new_id) {
                self.allocations
                    .push((new_id, HeapPointer::new(block_start)));
                break;
            }
            new_id += 1.into();
        }

        // Create the GcValue
        let value = GcValue {
            id: new_id,
            size,
            children: Vec::new(),
            marked: false,
        };

        // end = self.latest + size
        self.latest = HeapPointer::new(block_end as *mut u8);

        Some((value, new_id))
    }

    /// Allocate the space for an object
    pub fn allocate_id<Id: Into<AllocId>>(
        &mut self,
        size: usize,
        id: Id,
    ) -> Option<(GcValue, AllocId)> {
        let id = id.into();

        trace!("Allocating for size {} and id {}", size, id.0);

        if self.options.burn_gc {
            self.collect();
        }

        let (block_end, block_start) = (
            *self.latest as usize + size + 1,
            (*self.latest as usize + 1) as *mut u8,
        );
        let heap = self.get_side();

        // If the object is too large return None
        if block_end - *heap as usize > HEAP_HALF_SIZE {
            trace!("Object too large, failing allocation");
            self.collect(); // Collect garbage
            return None;
        }

        if !self.allocations.iter().any(|(i, _)| *i == id) {
            self.allocations.push((id, HeapPointer::new(block_start)));
        } else {
            trace!("Requested Id {:?} already exists, failing allocation", id);
            trace!("Gc Dump: {:?}", self);
            return None;
        }

        // Create the GcValue
        let value = GcValue {
            id,
            size,
            children: Vec::new(),
            marked: false,
        };

        // end = self.latest + size
        self.latest = HeapPointer::new(block_end as *mut u8);

        Some((value, id))
    }

    /// Collect all unused objects and shift to the other heap half
    pub fn collect(&mut self) {
        trace!("GC Collecting");

        // The allocations to be transferred over to the new heap
        let mut keep = Vec::new();
        // Get the valid allocations of roots and all children
        for root in &mut self.roots {
            if !root.marked {
                keep.extend(root.collect());
            }
        }

        let heap = {
            match !self.current_side {
                Side::Left => self.left,
                Side::Right => self.right,
            }
        };
        self.latest = heap;

        trace!("Allocations before collect: {}", self.allocations.len());

        let mut new_allocations = Vec::new();
        // Iterate over allocations to keep to move them onto the new heap
        for (id, size) in keep {
            // Get the pointer to the location on the old heap
            let ptr = self.get_ptr(id).unwrap();

            // Unsafe Usage: Get the bytes from the object on the old heap and copy them onto the new heap
            unsafe {
                let target: &mut [u8] = slice::from_raw_parts_mut(*self.latest as *mut _, size);
                target.copy_from_slice(slice::from_raw_parts(*ptr, size));
            }

            // Push the new allocation to new_allocations
            new_allocations.push((id, self.latest));

            trace!("Saving allocation {:?}", id);

            // Increment by the size of the moved object
            self.latest = self.latest.wrapping_add(size).into();
        }
        self.allocations = new_allocations;

        trace!("Allocations after collect: {}", self.allocations.len());

        if self.options.overwrite_heap {
            trace!("Overwriting old heap side: {:?}", self.current_side);

            // Overwrite old heap
            unsafe {
                ptr::write_bytes(*self.get_side(), 0, HEAP_HALF_SIZE);
            }
        }

        // Change the current side
        self.current_side = !self.current_side;

        // There has to be a better way to unmark all allocations
        for root in &mut self.roots {
            if root.marked {
                root.unmark();
            }
        }
    }

    /// Fetch a currently allocated value
    fn fetch_value<Id: Into<AllocId> + Copy>(&self, id: Id) -> Option<&GcValue> {
        for root in &self.roots {
            if root.id == id.into() {
                return Some(root);
            } else {
                if let Some(value) = root.fetch_child(id) {
                    return Some(value);
                }
            }
        }

        None
    }

    /// Fetches the current pointer associated with an id
    #[inline]
    pub(crate) fn get_ptr<Id: Into<AllocId> + Copy>(&self, id: Id) -> Option<HeapPointer> {
        self.allocations
            .iter()
            .find(|alloc| alloc.0 == id.into())
            .map(|alloc| alloc.1)
    }

    /// Add a root object
    #[inline]
    pub fn add_root(&mut self, root: GcValue) {
        trace!("Adding GC Root: {:?}", root);
        self.roots.push(root);
    }

    /// Remove a root object
    pub fn remove_root(&mut self, id: impl Into<AllocId> + Copy) -> Result<(), ()> {
        let id = id.into();

        trace!("Removing GC Root: {:?}", id);

        if let Some(index) = self.roots.iter().position(|value| value.id == id) {
            self.roots.remove(index);

            if self.options.burn_gc {
                self.collect();
            }

            return Ok(());
        }

        Err(())
    }

    /// Write to an object
    /// WARNING: Any object passed as `data` MUST BE OWNED
    pub unsafe fn write<T>(
        &self,
        id: impl Into<AllocId> + Copy,
        data: T,
        concrete_value: Option<&GcValue>,
    ) -> Result<(), String> {
        match (self.get_ptr(id), self.fetch_value(id), concrete_value) {
            (Some(ptr), Some(value), _) => {
                if !mem::size_of::<T>() > value.size {
                    ptr::write(*ptr as *mut T, data);

                    Ok(())
                } else {
                    Err(format!(
                        "Size Misalign: {} != {}",
                        value.size,
                        mem::size_of::<T>()
                    ))
                }
            }

            (Some(ptr), None, Some(value)) => {
                if !mem::size_of::<T>() > value.size {
                    ptr::write(*ptr as *mut T, data);

                    Ok(())
                } else {
                    Err(format!(
                        "Size Misalign: {} != {}",
                        value.size,
                        mem::size_of::<T>()
                    ))
                }
            }

            _ => Err(format!(
                "Id {:?} could not be fetched.\nSelf: {:?}",
                id.into(),
                self
            )),
        }
    }

    /// Fetch an object's value
    pub fn fetch<T, Id: Into<AllocId> + Copy>(&self, id: Id) -> Option<T> {
        if let Some((_, ptr)) = self.allocations.iter().find(|(i, _)| *i == id.into()) {
            Some(unsafe { ptr::read(**ptr as *const T) })
        } else {
            None
        }

        // TODO: Investigate this
        // Strange bug, the above code works fine, but replacing it with
        // if let Some(ptr) = self.get_ptr(id) {
        //     Some(unsafe { ptr::read(*ptr as *const T) })
        // } else {
        //     None
        // }
        // causes an immediate and silent crash
        // Definitely the sign of a deeper problem
    }

    /// Gets the current heap side
    pub fn get_side(&self) -> HeapPointer {
        match self.current_side {
            Side::Left => self.left,
            Side::Right => self.right,
        }
    }

    /// Information about the state of the GC
    pub fn data(&self) -> GcData {
        trace!(
            "Latest: {:?}, Start: {:?}, Diff {}",
            self.latest,
            self.get_side(),
            *self.latest as usize - *self.get_side() as usize
        );

        GcData {
            heap_size: HEAP_HALF_SIZE,
            heap_usage: *self.latest as usize - *self.get_side() as usize,
            num_roots: self.roots.len(),
            num_allocations: self.allocations.len(),
        }
    }

    /// See if the GC contains an Id
    #[inline]
    pub fn contains<Id: Into<AllocId> + Copy>(&self, id: Id) -> bool {
        self.allocations.iter().any(|(__id, _)| *__id == id.into())
    }
}

impl Drop for Gc {
    fn drop(&mut self) {
        // TODO: This should do things
    }
}

/// The status of the GC
#[derive(Debug, Copy, Clone)]
pub struct GcData {
    /// Size of the heap
    heap_size: usize,
    /// Amount of the heap currently used
    heap_usage: usize,
    /// Number of Root objects
    num_roots: usize,
    /// Total number of allocated objects
    num_allocations: usize,
}

impl std::fmt::Display for GcData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f,
            "Total Heap Size: {} bytes\nTotal Heap Usage: {} bytes\nPercent Heap Usage: {:.2}%\nTotal Root Objects: {}\nTotal Allocations: {}",
            self.heap_size / 8,
            self.heap_usage,
            (self.heap_usage as f64 / (self.heap_size / 8) as f64) * 100.0,
            self.num_roots,
            self.num_allocations
        )
    }
}

/// Represents the heap side currently used
#[derive(Debug, Copy, Clone)]
enum Side {
    Left,
    Right,
}

impl std::ops::Not for Side {
    type Output = Self;

    fn not(self) -> Self::Output {
        match self {
            Self::Left => Self::Right,
            Self::Right => Self::Left,
        }
    }
}

/// The id of a currently allocated object
#[derive(
    Debug,
    Copy,
    Clone,
    PartialEq,
    Eq,
    From,
    Into,
    Add,
    AddAssign,
    Constructor,
    Shrinkwrap,
    Mul,
    MulAssign,
    Sub,
    SubAssign,
)]
#[repr(transparent)]
pub struct AllocId(usize);

/// A pointer into the Heap
#[derive(Debug, Copy, Clone, PartialEq, Eq, From, Into, Constructor, Shrinkwrap, Mul, MulAssign)]
#[repr(transparent)]
pub struct HeapPointer(*mut u8);

impl std::fmt::Pointer for HeapPointer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:p}", self.0)
    }
}

/// A value contained in the GC
#[derive(Debug, Clone)]
pub struct GcValue {
    /// The id of the object, points into an hashmap containing the true pointer of the object
    id: AllocId,
    /// The size of the object, in bytes
    size: usize,
    /// The children of the value, will all be collected when it itself is collected
    children: Vec<GcValue>,
    /// Whether or not the object is marked, for collection purposes
    marked: bool,
}

impl GcValue {
    /// Fetches The id and size of all children
    #[inline]
    pub fn collect(&mut self) -> Vec<(AllocId, usize)> {
        let mut keep = Vec::new();

        self.marked = true;
        keep.push((self.id, self.size));

        for child in &mut self.children {
            if !child.marked {
                keep.extend(child.collect());
            }
        }

        keep
    }

    /// Fetches a child of the Value
    #[inline]
    pub fn fetch_child<Id: Into<AllocId> + Copy>(&self, id: Id) -> Option<&GcValue> {
        for child in &self.children {
            if child.id == id.into() {
                return Some(child);
            } else {
                if let Some(value) = child.fetch_child(id) {
                    return Some(value);
                }
            }
        }

        None
    }

    /// Adds a child
    #[inline]
    pub fn add_child(&mut self, child: GcValue) {
        self.children.push(child);
    }

    /// Unmarks self and all children
    #[inline]
    pub fn unmark(&mut self) {
        self.marked = false;

        for child in &mut self.children {
            if child.marked {
                child.unmark();
            }
        }
    }
}

pub trait CrunchGc {
    type Ident;
    type Error;

    fn alloc(&mut self, size: usize) -> Result<Self::Ident, Self::Error>;
    fn alloc_id(
        &mut self,
        size: usize,
        id: impl Into<Self::Ident>,
    ) -> Result<Self::Ident, Self::Error>;
    fn contains(&self, id: impl Into<Self::Ident>) -> Result<bool, Self::Error>;
    fn add_root(&mut self, id: impl Into<Self::Ident>) -> Result<(), Self::Error>;
    fn remove_root(&mut self, id: impl Into<Self::Ident>) -> Result<(), Self::Error>;
    fn add_child(
        &mut self,
        parent: impl Into<Self::Ident>,
        child: impl Into<Self::Ident>,
    ) -> Result<(), Self::Error>;
    fn remove_child(
        &mut self,
        parent: impl Into<Self::Ident>,
        child: impl Into<Self::Ident>,
    ) -> Result<(), Self::Error>;
    fn collect(&mut self) -> Result<(), Self::Error>;
    fn write<T>(&mut self, id: impl Into<Self::Ident>, data: &T) -> Result<(), Self::Error>;
    fn read<T>(&self, id: impl Into<Self::Ident>) -> Result<T, Self::Error>;
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::mem::size_of;

    #[test]
    fn alloc_value() {
        use crate::Value;

        let mut gc = Gc::new(&crate::OptionBuilder::new("./alloc_value").build());

        let (int, int_id) = gc.allocate(size_of::<Value>()).unwrap();
        unsafe {
            gc.write(int_id, Value::Int(10), Some(&int)).unwrap();
        }
        gc.add_root(int);
        assert!(gc.contains(int_id));
        assert!(gc.fetch(int_id) == Some(Value::Int(10)));

        let (int_2, int_2_id) = dbg!(gc.allocate_id(size_of::<Value>(), 0).unwrap());
        unsafe {
            gc.write(int_2_id, Value::Int(20), Some(&int_2)).unwrap();
        }
        gc.add_root(int_2);
        assert_eq!(AllocId(0), int_2_id);
        assert!(gc.contains(int_2_id));
        assert!(gc.fetch(int_2_id) == Some(Value::Int(20)));
    }

    #[test]
    fn alloc_usizes() {
        let mut gc = Gc::new(&crate::OptionBuilder::new("./alloc_usizes").build());

        let (keep, keep_id) = gc.allocate(size_of::<usize>()).unwrap();
        unsafe {
            gc.write(keep_id, usize::max_value(), Some(&keep)).unwrap();
        }
        gc.add_root(keep);
        assert!(gc.contains(keep_id));
        assert!(gc.fetch(keep_id) == Some(usize::max_value()));

        let (discard, discard_id) = gc.allocate(size_of::<usize>()).unwrap();
        unsafe {
            gc.write(discard_id, usize::max_value() - 1, Some(&discard))
                .unwrap();
        }
        assert!(gc.contains(discard_id));
        assert!(gc.fetch(discard_id) == Some(usize::max_value() - 1));

        gc.collect();

        assert!(gc.contains(keep_id));
        assert!(gc.fetch(keep_id) == Some(usize::max_value()));

        assert!(!gc.contains(discard_id));
        assert!(gc.fetch(discard_id) == <Option<usize>>::None);

        gc.collect();

        assert!(gc.contains(keep_id));
        assert!(gc.fetch(keep_id) == Some(usize::max_value()));

        assert!(!gc.contains(discard_id));
        assert!(gc.fetch(discard_id) == <Option<usize>>::None);

        assert!(gc.remove_root(keep_id).is_ok());

        gc.collect();

        assert!(!gc.contains(keep_id));
        assert!(gc.fetch(keep_id) == <Option<usize>>::None);

        gc.collect();

        assert!(!gc.contains(keep_id));
        assert!(gc.fetch(keep_id) == <Option<usize>>::None);
    }

    #[test]
    fn gc_test() {
        let mut gc = Gc::new(&crate::OptionBuilder::new("./gc_test").build());

        let (mut ten, ten_id) = gc.allocate(size_of::<usize>()).unwrap();
        println!("Allocated usize: Ptr: {:p}", gc.get_ptr(ten_id).unwrap());
        unsafe {
            gc.write(ten_id, 10, Some(&ten)).unwrap();
        }

        let (eleven, eleven_id) = gc.allocate(size_of::<usize>()).unwrap();
        println!("Allocated usize: Ptr: {:p}", gc.get_ptr(eleven_id).unwrap());
        unsafe {
            gc.write(eleven_id, 11, Some(&eleven)).unwrap();
        }

        let (twelve, twelve_id) = gc.allocate(size_of::<usize>()).unwrap();
        println!("Allocated usize: Ptr: {:p}", gc.get_ptr(twelve_id).unwrap());
        unsafe {
            gc.write(twelve_id, 12, Some(&twelve)).unwrap();
        }

        ten.add_child(eleven);
        println!("Added Child to ten: {:?}", ten);

        gc.add_root(ten);
        println!("Added root to GC: {:?}", gc.roots);

        println!("\n=> Before Collect\n{}\n", gc.data());

        {
            let ten: Option<usize> = gc.fetch(ten_id);
            let eleven: Option<usize> = gc.fetch(eleven_id);
            let twelve: Option<usize> = gc.fetch(twelve_id);

            println!("Ten: {:?}, Eleven: {:?}, Twelve: {:?}", ten, eleven, twelve);
        }

        gc.collect();
        println!("\n=> After Collect\n{}\n", gc.data());

        {
            let ten: Option<usize> = gc.fetch(ten_id);
            let eleven: Option<usize> = gc.fetch(eleven_id);
            let twelve: Option<usize> = gc.fetch(twelve_id);

            println!("Ten: {:?}, Eleven: {:?}, Twelve: {:?}", ten, eleven, twelve);
        }

        gc.collect();
        println!("\n=> After Second Collect\n{}\n", gc.data());

        {
            let ten: Option<usize> = gc.fetch(ten_id);
            let eleven: Option<usize> = gc.fetch(eleven_id);
            let twelve: Option<usize> = gc.fetch(twelve_id);

            println!("Ten: {:?}, Eleven: {:?}, Twelve: {:?}", ten, eleven, twelve);
        }
    }
}
