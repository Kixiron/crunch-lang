use crate::{AllocId, HeapPointer, Result, RuntimeError, RuntimeErrorTy};
use std::{alloc, collections::HashMap, mem, ptr, slice};

mod collectable;
pub use collectable::*;

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
    pub heap_size: usize,
}

impl From<&crate::Options> for GcOptions {
    fn from(options: &crate::Options) -> Self {
        Self {
            burn_gc: options.burn_gc,
            overwrite_heap: options.overwrite_heap,
            heap_size: options.heap_size,
        }
    }
}

/// The Crunch Garbage Collector
#[derive(Debug, Clone)]
pub struct Gc {
    /// The current root objects
    roots: Vec<AllocId>,
    /// The left heap
    left: HeapPointer,
    /// The right heap
    right: HeapPointer,
    /// The start of free memory
    latest: HeapPointer,
    /// The heap side currently in use
    current_side: Side,
    /// A vector of each allocation's id and current pointer
    allocations: HashMap<AllocId, (HeapPointer, GcValue)>,
    options: GcOptions,
}

impl Gc {
    /// Create a new GC instance
    #[must_use]
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
            let layout = alloc::Layout::from_size_align(options.heap_size, page_size)
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
            allocations: HashMap::new(),
            roots: Vec::new(),
            current_side: Side::Left,
            latest: left,
            options: GcOptions::from(options),
        }
    }

    /// Allocate the space for an object
    pub fn allocate(&mut self, size: usize) -> Result<AllocId> {
        trace!("Allocating size {}", size);

        if self.options.burn_gc {
            self.collect()?;
        }

        let (block_end, block_start) = (
            *self.latest as usize + size + 1,
            (*self.latest as usize + 1) as *mut u8,
        );
        let heap = self.get_side();

        // If the object is too large return None
        if block_end - *heap as usize > self.options.heap_size {
            self.collect()?; // Collect garbage
            return Err(RuntimeError {
                ty: RuntimeErrorTy::GcError,
                message: "The heap is full".to_string(),
            });
        }

        // Generate the Id of the new allocation based off of its pointer
        let mut new_id: AllocId = AllocId::new(block_start as usize >> 2);
        loop {
            if !self.allocations.iter().any(|(id, _)| *id == new_id) {
                // Create the GcValue
                let value = GcValue {
                    id: new_id,
                    size,
                    children: Vec::new(),
                    marked: false,
                };

                self.allocations
                    .insert(new_id, (HeapPointer::new(block_start), value));

                break;
            }
            new_id += 1.into();
        }

        // end = self.latest + size
        self.latest = HeapPointer::new(block_end as *mut u8);

        Ok(new_id)
    }

    /// Allocate the space for an object
    pub fn allocate_id<Id: Into<AllocId>>(&mut self, size: usize, id: Id) -> Result<AllocId> {
        let id = id.into();

        trace!("Allocating for size {} and id {}", size, id.0);

        if self.options.burn_gc {
            self.collect()?;
        }

        let (block_end, block_start) = (
            *self.latest as usize + size + 1,
            (*self.latest as usize + 1) as *mut u8,
        );
        let heap = self.get_side();

        // If the object is too large return None
        if block_end - *heap as usize > self.options.heap_size {
            trace!("Object too large, failing allocation");
            self.collect()?; // Collect garbage
            return Err(RuntimeError {
                ty: RuntimeErrorTy::GcError,
                message: "The heap is full".to_string(),
            });
        }

        if self.allocations.iter().any(|(i, _)| *i != id) {
            // Create the GcValue
            let value = GcValue {
                id,
                size,
                children: Vec::new(),
                marked: false,
            };

            self.allocations
                .insert(id, (HeapPointer::new(block_start), value));
        } else {
            trace!("Requested Id {:?} already exists, failing allocation", id);
            trace!("Gc Dump: {:?}", self);
            return Err(RuntimeError {
                ty: RuntimeErrorTy::GcError,
                message: "The requested ID already exists".to_string(),
            });
        }

        // end = self.latest + size
        self.latest = HeapPointer::new(block_end as *mut u8);

        Ok(id)
    }

    /// Collect all unused objects and shift to the other heap half
    pub fn collect(&mut self) -> Result<()> {
        trace!("GC Collecting");

        // The allocations to be transferred over to the new heap
        let mut keep = HashMap::with_capacity(self.roots.len());

        // Get the valid allocations of roots and all children
        for root in &self.roots {
            if let Some((ptr, root)) = self.allocations.get(root) {
                if !root.marked {
                    root.collect(*ptr, &self.allocations, &mut keep);
                }
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

        let mut new_allocations = HashMap::with_capacity(keep.len());
        // Iterate over allocations to keep to move them onto the new heap
        for (id, (old_ptr, val)) in keep.iter() {
            // Unsafe Usage: Get the bytes from the object on the old heap and copy them onto the new heap
            unsafe {
                let target: &mut [u8] = slice::from_raw_parts_mut(*self.latest as *mut _, val.size);
                target.copy_from_slice(slice::from_raw_parts(**old_ptr, val.size));
            }

            // Push the new allocation to new_allocations
            new_allocations.insert(*id, (self.latest, *val));

            trace!("Saving allocation {:?}", id);

            // Increment by the size of the moved object
            self.latest = self.latest.wrapping_add(val.size).into();
        }
        self.allocations = new_allocations;

        trace!("Allocations after collect: {}", self.allocations.len());

        if self.options.overwrite_heap {
            trace!("Overwriting old heap side: {:?}", self.current_side);

            // Overwrite old heap
            unsafe {
                ptr::write_bytes(*self.get_side(), 0x00, self.options.heap_size);
            }
        }

        // Change the current side
        self.current_side = !self.current_side;

        // There has to be a better way to unmark all allocations
        for root in &self.roots {
            if let Some((ptr, root)) = self.allocations.get_mut(root) {
                if root.marked {
                    root.unmark(&mut self.allocations);
                }
            }
        }

        Ok(())
    }

    /// Fetch a currently allocated value
    fn fetch_value<Id: Into<AllocId>>(&self, id: Id) -> Result<&GcValue> {
        let id = id.into();

        for root in &self.roots {
            if let Some(target) = self.__search_children(root, id) {
                return Ok(target);
            }
        }

        Err(RuntimeError {
            ty: RuntimeErrorTy::GcError,
            message: "Requested value does not exist".to_string(),
        })
    }

    fn __search_children(&self, parent: &AllocId, target: AllocId) -> Option<&GcValue> {
        if let Some((_ptr, parent)) = self.allocations.get(parent) {
            if parent.id == target {
                return Some(parent);
            } else {
                for child in parent.children {
                    if let Some(target) = self.__search_children(&child, target) {
                        return Some(target);
                    }
                }
            }
        }

        None
    }

    /// Fetch a currently allocated value
    fn fetch_value_mut<Id: Into<AllocId>>(&self, id: Id) -> Result<&mut GcValue> {
        let id = id.into();

        for root in &mut self.roots {
            if let Some(target) = self.__search_children_mut(root, id) {
                return Ok(target);
            }
        }

        Err(RuntimeError {
            ty: RuntimeErrorTy::GcError,
            message: "Requested value does not exist".to_string(),
        })
    }

    fn __search_children_mut(&self, parent: &AllocId, target: AllocId) -> Option<&mut GcValue> {
        if let Some((_ptr, parent)) = self.allocations.get_mut(parent) {
            if parent.id == target {
                return Some(parent);
            } else {
                for child in parent.children {
                    if let Some(target) = self.__search_children_mut(&child, target) {
                        return Some(target);
                    }
                }
            }
        }

        None
    }

    /// Add a root object
    #[inline]
    pub fn add_root(&mut self, id: AllocId) {
        trace!("Adding GC Root: {:?}", id);
        self.roots.push(id);
    }

    /// Remove a root object
    pub fn remove_root(&mut self, id: impl Into<AllocId> + Copy) -> Result<()> {
        let id = id.into();

        trace!("Removing GC Root: {:?}", id);

        if let Some(index) = self.roots.iter().position(|root_id| *root_id == id) {
            self.roots.remove(index);

            if self.options.burn_gc {
                self.collect()?;
            }

            return Ok(());
        }

        Err(RuntimeError {
            ty: RuntimeErrorTy::GcError,
            message: "The object to be unrooted does not exist".to_string(),
        })
    }

    /// Write to an object
    ///
    /// # Safety
    /// Any object passed as `data` **must** be owned
    pub unsafe fn write<Id>(
        &self,
        id: Id,
        data: &[u8],
        concrete_value: Option<&GcValue>,
    ) -> Result<()>
    where
        Id: Into<AllocId> + Copy,
    {
        match (self.get_ptr(id), self.fetch_value(id), concrete_value) {
            (Ok(ptr), Ok(value), _) | (Ok(ptr), Err(_), Some(value)) => {
                if data.len() == value.size {
                    ptr::copy(data.as_ptr(), *ptr as *mut u8, data.len());

                    Ok(())
                } else {
                    Err(RuntimeError {
                        ty: RuntimeErrorTy::GcError,
                        message: format!("Size Misalign: {} != {}", value.size, data.len(),),
                    })
                }
            }

            _ => Err(RuntimeError {
                ty: RuntimeErrorTy::GcError,
                message: "Object to be written to does not exist".to_string(),
            }),
        }
    }

    /// Write to an object
    ///
    /// # Safety
    /// Any object passed as `data` **must** be owned  
    /// With great power comes great danger, just don't use this unless you have to  
    /// Currently only used inside of `GcStr` because it's broken with the normal `write`  
    pub unsafe fn write_unchecked<T, Id>(
        &self,
        id: Id,
        data: T,
        concrete_value: Option<&GcValue>,
    ) -> Result<()>
    where
        Id: Into<AllocId> + Copy,
    {
        match (self.get_ptr(id), self.fetch_value(id), concrete_value) {
            (Ok(ptr), Ok(value), _) | (Ok(ptr), Err(_), Some(value)) => {
                if mem::size_of::<T>() != value.size {
                    warn!(
                        "Non-fatal Size Misalign: {} != {}",
                        value.size,
                        mem::size_of::<T>(),
                    );
                }

                ptr::write(*ptr as *mut T, data);
                Ok(())
            }

            _ => Err(RuntimeError {
                ty: RuntimeErrorTy::GcError,
                message: "Object to be written to does not exist".to_string(),
            }),
        }
    }

    /// Fetch an object's value
    pub fn fetch<'gc, Id: Into<AllocId> + Copy>(
        &'gc self,
        size: usize,
        id: Id,
    ) -> Result<&'gc [u8]> {
        if let Some((_, (ptr, val))) = self.allocations.iter().find(|(i, _)| *i == id.into()) {
            Ok(unsafe { std::slice::from_raw_parts(**ptr, size) })
        } else {
            Err(RuntimeError {
                ty: RuntimeErrorTy::GcError,
                message: "Requested value does not exist".to_string(),
            })
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
    #[must_use]
    pub fn get_side(&self) -> HeapPointer {
        match self.current_side {
            Side::Left => self.left,
            Side::Right => self.right,
        }
    }

    /// Information about the state of the GC
    #[must_use]
    pub fn data(&self) -> GcData {
        trace!(
            "Latest: {:?}, Start: {:?}, Diff {}",
            self.latest,
            self.get_side(),
            *self.latest as usize - *self.get_side() as usize
        );

        GcData {
            heap_size: self.options.heap_size,
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

/// A value contained in the GC
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GcValue {
    /// The id of the object, points into an hashmap containing the true pointer of the object
    id: AllocId,
    /// The size of the object, in bytes
    size: usize,
    /// The children of the value, will all be collected when it itself is collected
    children: Vec<AllocId>,
    /// Whether or not the object is marked, for collection purposes
    marked: bool,
}

impl GcValue {
    /// Fetches The id and size of all children
    #[inline]
    pub fn collect(
        &self,
        ptr: HeapPointer,
        allocations: &HashMap<AllocId, (HeapPointer, Self)>,
        map: &mut HashMap<AllocId, (HeapPointer, Self)>,
    ) {
        self.marked = true;
        map.insert(self.id, (ptr, self.clone())); // Avoid clone

        for child in &self.children {
            if let Some((ptr, child)) = allocations.get(child) {
                if !child.marked {
                    child.collect(*ptr, allocations, &mut map);
                }
            }
        }
    }

    /// Fetches a child of the Value
    #[inline]
    pub fn fetch_child<Id: Into<AllocId> + Copy>(&self, id: Id) -> Option<&Self> {
        for child in &self.children {
            if child.id == id.into() {
                return Some(child);
            } else if let Some(value) = child.fetch_child(id) {
                return Some(value);
            }
        }

        None
    }

    #[inline]
    pub fn fetch_child_mut<Id: Into<AllocId> + Copy>(&mut self, id: Id) -> Option<&mut Self> {
        for child in &mut self.children {
            if child.id == id.into() {
                return Some(child);
            } else if let Some(value) = child.fetch_child_mut(id) {
                return Some(value);
            }
        }

        None
    }

    /// Adds a child
    #[inline]
    pub fn add_child(&mut self, child: Self) {
        self.children.push(child);
    }

    pub fn remove_child(&mut self, child: &Self) {
        self.children.remove_item(child);
    }

    /// Unmarks self and all children
    #[inline]
    pub fn unmark(&mut self, allocations: &mut HashMap<AllocId, (HeapPointer, GcValue)>) {
        self.marked = false;

        for child in &mut self.children {
            if let Some((_ptr, child)) = allocations.get_mut(child) {
                if child.marked {
                    child.unmark(allocations);
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::mem::size_of;

    #[test]
    fn alloc_value() {
        use crate::RuntimeValue;

        let mut gc = Gc::new(&crate::OptionBuilder::new("./alloc_value").build());

        let (int, int_id) = gc.allocate(size_of::<RuntimeValue>()).unwrap();
        unsafe {
            gc.write(
                int_id,
                &<RuntimeValue as Into<Vec<u8>>>::into(RuntimeValue::I32(10)),
                Some(&int),
            )
            .unwrap();
        }
        gc.add_root(int);
        assert!(gc.contains(int_id));
        assert!(gc.fetch(int_id) == Ok(RuntimeValue::I32(10)));

        let (int_2, int_2_id) = gc.allocate_id(size_of::<RuntimeValue>(), 0).unwrap();
        unsafe {
            gc.write(
                int_2_id,
                &<RuntimeValue as Into<Vec<u8>>>::into(RuntimeValue::I32(20)),
                Some(&int_2),
            )
            .unwrap();
        }
        gc.add_root(int_2);
        assert_eq!(AllocId(0), int_2_id);
        assert!(gc.contains(int_2_id));
        assert!(gc.fetch(int_2_id) == Ok(RuntimeValue::I32(20)));
    }

    #[test]
    fn alloc_usizes() {
        let mut gc = Gc::new(&crate::OptionBuilder::new("./alloc_usizes").build());

        let (keep, keep_id) = gc.allocate(size_of::<usize>()).unwrap();
        unsafe {
            gc.write(keep_id, &usize::max_value().to_le_bytes(), Some(&keep))
                .unwrap();
        }
        gc.add_root(keep);
        assert!(gc.contains(keep_id));
        assert!(gc.fetch(keep_id) == Ok(usize::max_value()));

        let (discard, discard_id) = gc.allocate(size_of::<usize>()).unwrap();
        unsafe {
            gc.write(
                discard_id,
                &(usize::max_value() - 1).to_le_bytes(),
                Some(&discard),
            )
            .unwrap();
        }
        assert!(gc.contains(discard_id));
        assert!(gc.fetch(discard_id) == Ok(usize::max_value() - 1));

        gc.collect().unwrap();

        assert!(gc.contains(keep_id));
        assert!(gc.fetch(keep_id) == Ok(usize::max_value()));

        assert!(!gc.contains(discard_id));
        assert!(gc.fetch::<usize, AllocId>(discard_id).is_err());

        gc.collect().unwrap();

        assert!(gc.contains(keep_id));
        assert!(gc.fetch(keep_id) == Ok(usize::max_value()));

        assert!(!gc.contains(discard_id));
        assert!(gc.fetch::<usize, AllocId>(discard_id).is_err());

        assert!(gc.remove_root(keep_id).is_ok());

        gc.collect().unwrap();

        assert!(!gc.contains(keep_id));
        assert!(gc.fetch::<usize, AllocId>(keep_id).is_err());

        gc.collect().unwrap();

        assert!(!gc.contains(keep_id));
        assert!(gc.fetch::<usize, AllocId>(keep_id).is_err());
    }

    #[test]
    fn gc_test() {
        let mut gc = Gc::new(&crate::OptionBuilder::new("./gc_test").build());

        let (mut ten, ten_id) = gc.allocate(size_of::<usize>()).unwrap();
        println!("Allocated usize: Ptr: {:p}", gc.get_ptr(ten_id).unwrap());
        unsafe {
            gc.write(ten_id, &10_usize.to_le_bytes(), Some(&ten))
                .unwrap();
        }

        let (eleven, eleven_id) = gc.allocate(size_of::<usize>()).unwrap();
        println!("Allocated usize: Ptr: {:p}", gc.get_ptr(eleven_id).unwrap());
        unsafe {
            gc.write(eleven_id, &11_usize.to_le_bytes(), Some(&eleven))
                .unwrap();
        }

        let (twelve, twelve_id) = gc.allocate(size_of::<usize>()).unwrap();
        println!("Allocated usize: Ptr: {:p}", gc.get_ptr(twelve_id).unwrap());
        unsafe {
            gc.write(twelve_id, &12_usize.to_le_bytes(), Some(&twelve))
                .unwrap();
        }

        ten.add_child(eleven);
        println!("Added Child to ten: {:?}", ten);

        gc.add_root(ten);
        println!("Added root to GC: {:?}", gc.roots);

        println!("\n=> Before Collect\n{}\n", gc.data());

        {
            let ten = gc.fetch::<usize, AllocId>(ten_id);
            let eleven = gc.fetch::<usize, AllocId>(eleven_id);
            let twelve = gc.fetch::<usize, AllocId>(twelve_id);

            println!("Ten: {:?}, Eleven: {:?}, Twelve: {:?}", ten, eleven, twelve);
        }

        gc.collect().unwrap();
        println!("\n=> After Collect\n{}\n", gc.data());

        {
            let ten = gc.fetch::<usize, AllocId>(ten_id);
            let eleven = gc.fetch::<usize, AllocId>(eleven_id);
            let twelve = gc.fetch::<usize, AllocId>(twelve_id);

            println!("Ten: {:?}, Eleven: {:?}, Twelve: {:?}", ten, eleven, twelve);
        }

        gc.collect().unwrap();
        println!("\n=> After Second Collect\n{}\n", gc.data());

        {
            let ten = gc.fetch::<usize, AllocId>(ten_id);
            let eleven = gc.fetch::<usize, AllocId>(eleven_id);
            let twelve = gc.fetch::<usize, AllocId>(twelve_id);

            println!("Ten: {:?}, Eleven: {:?}, Twelve: {:?}", ten, eleven, twelve);
        }
    }
}
