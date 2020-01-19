use crate::{AllocId, HeapPointer, Result, RuntimeError, RuntimeErrorTy};
use std::{alloc, collections::HashMap, mem, pin::Pin, ptr, slice};

mod collectable;
pub use collectable::*;

/// Gets the memory page size
#[inline(always)]
#[cfg(target_family = "unix")]
pub(crate) fn page_size() -> usize {
    let val = unsafe { libc::sysconf(libc::_SC_PAGESIZE) } as usize;

    trace!("Memory Page Size: {}", val);
    assert!(size != 0);

    size
}

/// Gets the memory page size
#[inline(always)]
#[cfg(target_family = "windows")]
pub(crate) fn page_size() -> usize {
    use std::mem::MaybeUninit;
    use winapi::um::sysinfoapi::{GetSystemInfo, SYSTEM_INFO};

    let size = unsafe {
        let mut system_info: MaybeUninit<SYSTEM_INFO> = MaybeUninit::zeroed();
        GetSystemInfo(system_info.as_mut_ptr());

        system_info.assume_init().dwPageSize as usize
    };

    trace!("Memory Page Size: {}", size);
    assert!(size != 0);

    size
}

/// The options for an initialized GC
#[derive(Debug, Copy, Clone)]
pub struct GcOptions {
    /// Activates a GC collect at every opportunity
    pub burn_gc: bool,
    /// Overwrites the heap on a side swap
    pub overwrite_heap: bool,
    pub heap_size: usize,
    pub debug: bool,
}

impl From<&crate::Options> for GcOptions {
    fn from(options: &crate::Options) -> Self {
        Self {
            burn_gc: options.burn_gc,
            overwrite_heap: options.overwrite_heap,
            heap_size: options.heap_size,
            debug: options.debug_log,
        }
    }
}

/// The Crunch Garbage Collector
#[derive(Debug)]
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
            let page_size = page_size();

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
    pub fn allocate(&mut self, size: usize) -> Result<(HeapPointer, AllocId)> {
        trace!("Allocating size {}", size);

        if self.options.burn_gc {
            self.collect()?;
        }

        let (block_end, block_start) = (
            *self.latest as usize + size,
            (*self.latest as usize) as *mut u8,
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

        self.latest = HeapPointer::new(block_end as *mut u8);

        Ok((HeapPointer::new(block_start), new_id))
    }

    pub fn allocate_heap<T: Collectable>(&mut self, item: T) -> Result<AllocId> {
        trace!("Allocating an item to the heap");

        let (ptr, id) = self.allocate(mem::size_of::<T>())?;
        unsafe { (*ptr as *mut T).write(item) };

        Ok(id)
    }

    pub fn allocate_zeroed(&mut self, size: usize) -> Result<AllocId> {
        trace!("Allocating the zeroed for size {}", size);

        let (ptr, id) = self.allocate(size)?;
        unsafe { ptr.write_bytes(0x00, size) };

        Ok(id)
    }

    /// Collect all unused objects and shift to the other heap half
    pub fn collect(&mut self) -> Result<()> {
        trace!("GC Collecting");

        // The allocations to be transferred over to the new heap
        let mut keep = HashMap::with_capacity(self.roots.len());
        let mut queue = Vec::with_capacity(self.allocations.len());
        queue.extend_from_slice(&self.roots);

        while let Some(val) = queue.pop() {
            if let Some((ptr, root)) = self.allocations.get_mut(&val) {
                if !root.marked {
                    root.collect(*ptr, &mut queue, &mut keep);
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
        for (id, (old_ptr, val)) in keep.into_iter() {
            // Unsafe Usage: Get the bytes from the object on the old heap and copy them onto the new heap
            unsafe {
                let target: &mut [u8] = slice::from_raw_parts_mut(*self.latest as *mut _, val.size);
                target.copy_from_slice(slice::from_raw_parts(*old_ptr, val.size));
            }

            // Increment by the size of the moved object
            self.latest = self.latest.wrapping_add(val.size).into();

            // Push the new allocation to new_allocations
            new_allocations.insert(id, (self.latest, val));

            trace!("Saving allocation {:?}", id);
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

        let mut queue = Vec::with_capacity(self.allocations.len());
        queue.extend_from_slice(&self.roots);

        while let Some(val) = queue.pop() {
            if let Some((_ptr, root)) = self.allocations.get_mut(&val) {
                if root.marked {
                    root.unmark(&mut queue);
                }
            }
        }

        Ok(())
    }

    pub unsafe fn get_ptr(&self, id: AllocId) -> Result<HeapPointer> {
        let (ptr, _val) = self.allocations.get(&id).ok_or(RuntimeError {
            ty: RuntimeErrorTy::GcError,
            message: "Requested value does not exist".to_string(),
        })?;

        Ok(*ptr)
    }

    fn dump_heap(&self, side: Side) -> std::result::Result<(), Box<dyn std::error::Error>> {
        use std::io::Write;

        let mut f = std::fs::File::create("right.dump")?;
        f.write_all(unsafe {
            std::slice::from_raw_parts(
                match side {
                    Side::Left => *self.left,
                    Side::Right => *self.right,
                },
                self.options.heap_size,
            )
        })?;

        Ok(())
    }

    /// Fetch an object's raw bytes
    pub fn fetch_bytes<'gc>(&'gc self, id: AllocId) -> Result<&[u8]> {
        trace!("Fetching {}", id);

        if let Some((_, (ptr, val))) = self.allocations.iter().find(|(i, _)| **i == id.into()) {
            if self.options.debug {
                self.dump_heap(Side::Right).unwrap();
                self.dump_heap(Side::Left).unwrap();
            }

            println!(
                "Ptr: {:p}, Heap: {:p}, Heap - Ptr: {:p}",
                **ptr,
                *self.get_side(),
                (**ptr as usize - *self.get_side() as usize) as *const u8
            );

            Ok(unsafe { std::slice::from_raw_parts(**ptr, val.size) })
        } else {
            Err(RuntimeError {
                ty: RuntimeErrorTy::GcError,
                message: "Requested value does not exist".to_string(),
            })
        }
    }

    /// Fetch a currently allocated value
    fn fetch_value(&self, id: AllocId) -> Result<&GcValue> {
        trace!("Fetching allocation {}", id);

        let mut queue = Vec::with_capacity(self.allocations.len());
        queue.extend_from_slice(&self.roots);

        while let Some(val) = queue.pop() {
            if let Some((_ptr, root)) = self.allocations.get(&val) {
                if root.id == id {
                    return Ok(root);
                } else {
                    queue.extend_from_slice(&root.children);
                }
            }
        }

        Err(RuntimeError {
            ty: RuntimeErrorTy::GcError,
            message: "Requested value does not exist".to_string(),
        })
    }

    /// Fetch a currently allocated value
    fn fetch_value_mut(&mut self, id: AllocId) -> Result<&mut GcValue> {
        trace!("Fetching allocation {} mutably", id);

        let mut queue = Vec::with_capacity(self.allocations.len());
        queue.extend_from_slice(&self.roots);

        let allocs = &mut self.allocations;

        while let Some(val) = queue.pop() {
            if allocs.get(&val).map(|v| v.1.id == id).unwrap_or(false) {
                return allocs
                    .get_mut(&val)
                    .map(|(_, root)| &mut *root)
                    .ok_or(RuntimeError {
                        ty: RuntimeErrorTy::GcError,
                        message: "Requested value does not exist".to_string(),
                    });
            } else if let Some((_, root)) = allocs.get(&val) {
                queue.extend_from_slice(&root.children);
            }
        }

        Err(RuntimeError {
            ty: RuntimeErrorTy::GcError,
            message: "Requested value does not exist".to_string(),
        })
    }
    /*
    TODO: When polonius lands, replace current implementation
    fn fetch_value_mut(&mut self, id: AllocId) -> Result<&mut GcValue> {
        let mut queue = Vec::with_capacity(self.allocations.len());
        queue.extend_from_slice(&self.roots);

        let mut value;
        while let Some(val) = queue.pop() {
            value = self.allocations.get_mut(&val);
            if let Some((_ptr, root)) = value {
                if root.id == id {
                    return Ok(root);
                } else {
                    queue.extend_from_slice(&root.children);
                }
            }
        }

        Err(RuntimeError {
            ty: RuntimeErrorTy::GcError,
            message: "Requested value does not exist".to_string(),
        })
    }
    */

    pub fn add_child(&mut self, parent: AllocId, child: AllocId) -> Result<()> {
        self.fetch_value_mut(parent)?.add_child(child);

        Ok(())
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

    pub unsafe fn write<Id, T>(&self, id: Id, data: T) -> Result<()>
    where
        Id: Into<AllocId> + Copy,
    {
        let id = id.into();
        trace!("Writing to allocation {}", id);

        if let Some((ptr, val)) = self.allocations.get(&id) {
            if mem::size_of::<T>() != val.size {
                (**ptr as *mut T).write(data);
                trace!("Wrote to allocation {}, ptr {:p}", id, *ptr);

                Ok(())
            } else {
                Err(RuntimeError {
                    ty: RuntimeErrorTy::GcError,
                    message: format!("Size Misalign: {} != {}", val.size, mem::size_of::<T>()),
                })
            }
        } else {
            Err(RuntimeError {
                ty: RuntimeErrorTy::GcError,
                message: "Object to be written to does not exist".to_string(),
            })
        }
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
        &mut self,
        ptr: HeapPointer,
        queue: &mut Vec<AllocId>,
        map: &mut HashMap<AllocId, (HeapPointer, Self)>,
    ) {
        self.marked = true;
        map.insert(self.id, (ptr, self.clone())); // Avoid clone
        queue.extend_from_slice(&self.children);
    }

    /// Adds a child
    #[inline]
    pub fn add_child(&mut self, child: AllocId) {
        self.children.push(child);
    }

    pub fn remove_child(&mut self, child: AllocId) {
        self.children.remove_item(&child);
    }

    /// Unmarks self and all children
    #[inline]
    pub fn unmark(&mut self, queue: &mut Vec<AllocId>) {
        self.marked = false;
        queue.extend_from_slice(&self.children);
    }
}
