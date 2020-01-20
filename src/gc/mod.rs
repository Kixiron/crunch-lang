use crate::{AllocId, HeapPointer, Result, RuntimeError, RuntimeErrorTy};
use fxhash::FxBuildHasher;
use std::{alloc, collections::HashMap, mem, ptr, slice};

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
    /// Overwrites the heap on a side swap and on the `Gc` drop
    ///
    /// [`Gc`]: crate.Gc
    pub overwrite_heap: bool,
    /// Sets the size of one half of the heap  
    /// Note: This means that the total allocated memory is `heap_size * 2`, while the avaliable
    /// memory is only `heap_size`
    pub heap_size: usize,
    /// Enables additional debug output
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
    allocations: HashMap<AllocId, (HeapPointer, GcValue), FxBuildHasher>,
    /// The next `AllocId` to be used for an `allocated` value
    ///
    /// [`AllocId`]: crate.AllocId
    /// [`allocated`]: #allocate.crate.Gc
    next_id: usize,
    /// The options configured for the Gc
    options: GcOptions,
}

impl Gc {
    /// Create a new GC instance, using `options.heap_size` as the starting heap size.  
    /// Note: This means that the total allocated memory is `heap_size * 2`, while the avaliable
    /// memory is only `heap_size`
    ///
    /// # Panics
    ///
    /// Will panic if `options.heap_size` or the memory `page_size` are `0`
    ///
    /// [`options.heap_size`]: #heap_size.crate.GcOptions
    #[must_use]
    pub fn new(options: &crate::Options) -> Self {
        trace!("Initializing GC");

        assert!(options.heap_size != 0);

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
            allocations: HashMap::with_hasher(FxBuildHasher::default()),
            roots: Vec::new(),
            current_side: Side::Left,
            latest: left,
            next_id: 0,
            options: GcOptions::from(options),
        }
    }

    /// Allocates a region of `size` bytes and returns the `HeapPointer` and `AllocId` of said allocation
    ///
    /// # Safety
    /// The `HeapPointer` returned can be invalidated by a `Gc` collection cycle (See `collect`)
    ///
    /// [`HeapPointer`]: crate.HeapPointer
    /// [`AllocId`]: crate.AllocId
    /// [`Gc`]: crate.Gc
    /// [`collect`]: #collect.crate.Gc
    #[must_use]
    pub fn allocate(&mut self, size: usize) -> Result<(HeapPointer, AllocId)> {
        self.allocate_inner(size, 0)
    }

    /// The inner workings of `allocate`, used to allow retrying allocation when the heap is out of memory.  
    /// See `allocate` for more information
    ///
    /// [`allocate`]: #allocate.crate.Gc
    #[inline]
    #[must_use]
    fn allocate_inner(&mut self, size: usize, attempt: u8) -> Result<(HeapPointer, AllocId)> {
        trace!("Allocating size {}", size);

        if self.options.burn_gc {
            self.collect();
        }

        let (block_start, block_end) = (
            (*self.latest as usize) as *mut u8,
            *self.latest as usize + size,
        );

        // If the object is too large return an error
        if !(block_start as usize >= *self.get_side() as usize
            && block_end < *self.get_side() as usize + self.options.heap_size)
        {
            self.collect(); // Collect garbage

            if attempt < 1 {
                return self.allocate_inner(size, attempt + 1);
            } else {
                return Err(RuntimeError {
                    ty: RuntimeErrorTy::GcError,
                    message: "The heap is full".to_string(),
                });
            }
        }

        // Generate the Id of the new allocation based off of its pointer
        let new_id: AllocId = AllocId::new(self.next_id);
        self.next_id += 1;

        let value = GcValue {
            id: new_id,
            size,
            children: Vec::new(),
            marked: false,
        };

        self.allocations
            .insert(new_id, (HeapPointer::new(block_start), value));

        self.latest = HeapPointer::new(block_end as *mut u8);

        Ok((HeapPointer::new(block_start), new_id))
    }

    /// Allocates and writes `T` directly to the heap, returning its `AllocId`.  
    ///
    /// [`AllocId`]: crate.AllocId
    #[must_use]
    pub fn allocate_heap<T: Collectable>(&mut self, item: T) -> Result<AllocId> {
        trace!("Allocating an item to the heap");

        let (ptr, id) = self.allocate(mem::size_of::<T>())?;
        unsafe { (*ptr as *mut T).write(item) };

        Ok(id)
    }

    /// Allocates a region of `size` bytes, zeroes it and returns the `HeapPointer` and `AllocId` of said allocation
    ///
    /// # Safety
    /// The `HeapPointer` returned can be invalidated by a `Gc` collection cycle (See `collect`)
    ///
    /// [`HeapPointer`]: crate.HeapPointer
    /// [`AllocId`]: crate.AllocId
    /// [`Gc`]: crate.Gc
    /// [`collect`]: #collect.crate.Gc
    #[must_use]
    pub fn allocate_zeroed(&mut self, size: usize) -> Result<(HeapPointer, AllocId)> {
        trace!("Allocating the zeroed for size {}", size);

        let (ptr, id) = self.allocate(size)?;
        unsafe { ptr.write_bytes(0x00, size) };

        Ok((ptr, id))
    }

    /// Collect all unused objects and shifts to the other heap half
    ///
    /// # Collection
    ///
    /// All reachable allocations (Decided by the gc's current `roots`) are marked, extending the
    /// allocations to be marked by all of the allocation's children.  
    /// All marked allocations are then moved to the opposite heap side.
    ///
    /// [`roots`]: #roots.crate.Gc
    pub fn collect(&mut self) {
        trace!("GC Collecting");

        // The allocations to be transferred over to the new heap
        let mut keep =
            HashMap::with_capacity_and_hasher(self.roots.len(), FxBuildHasher::default());
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

        let mut new_allocations =
            HashMap::with_capacity_and_hasher(keep.len(), FxBuildHasher::default());

        // Iterate over allocations to keep to move them onto the new heap
        for (id, (old_ptr, val)) in keep {
            let size = val.size;

            // Safety: Copying bytes from one heap to the other
            unsafe {
                let target: &mut [u8] = slice::from_raw_parts_mut(*self.latest as *mut _, size);
                target.copy_from_slice(slice::from_raw_parts(*old_ptr, size));
            }

            // Push the new allocation to new_allocations
            new_allocations.insert(id, (self.latest, val));

            // Increment by the size of the moved object
            self.latest = unsafe { self.latest.offset(size as isize) }.into();

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

        // Use the pre-existing queue
        queue.extend_from_slice(&self.roots);

        while let Some(val) = queue.pop() {
            if let Some((_ptr, root)) = self.allocations.get_mut(&val) {
                if root.marked {
                    root.unmark(&mut queue);
                }
            }
        }
    }

    /// Get the concrete `HeapPointer` to an object stored in the `Gc`
    ///
    /// # Safety
    ///
    /// This returns a pointer to the `AllocId`'s *current* location in memory.  
    /// Said pointer can be invalidated at any time by a collection cycle, so use immediately.  
    /// Additionally, the pointer has no related type info, so the user is trusted to not overwrite
    /// their allocated space.
    ///
    /// # Errors
    ///
    /// Throws a `RuntimeError` with the type of `GcError` if the requested `AllocId` does not exist
    ///
    /// [`HeapPointer`]: crate.HeapPointer
    /// [`Gc`]: crate.Gc
    /// [`AllocId`]: crate.AllocId
    /// [`RuntimeError`]: crate.RuntimeError
    /// [`GcError`]: crate.RuntimeErrorTy
    #[must_use]
    pub unsafe fn get_ptr(&self, id: AllocId) -> Result<HeapPointer> {
        let (ptr, _val) = self.allocations.get(&id).ok_or(RuntimeError {
            ty: RuntimeErrorTy::GcError,
            message: "Requested value does not exist".to_string(),
        })?;

        Ok(*ptr)
    }

    /// Creates two dump files, one for each heap half, containing the raw bytes of each
    fn dump_heap(&self, side: Side) -> std::result::Result<(), std::io::Error> {
        use std::io::Write;

        let mut f = std::fs::File::create(match side {
            Side::Left => "left.dump",
            Side::Right => "right.dump",
        })?;

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
    #[must_use]
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
    #[allow(dead_code)]
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

    /// Fetch a currently allocated value mutably
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

    /// Adds a child to the requested parent
    ///
    /// # Errors
    ///
    /// Returns an `RuntimeError` if the parent doesn't exist, but does not check if the
    /// child exists.
    ///
    /// [`RuntimeError`]: crate.RuntimeError
    pub fn add_child(&mut self, parent: AllocId, child: AllocId) -> Result<()> {
        self.fetch_value_mut(parent)?.add_child(child);

        Ok(())
    }

    /// Add a root object to the `Gc`.  
    /// Note: No checks are preformed to see if the `AllocId` exists
    ///
    /// [`Gc`]: crate.Gc
    /// [`AllocId`]: crate.AllocId
    #[inline]
    pub fn add_root(&mut self, id: AllocId) {
        trace!("Adding GC Root: {:?}", id);
        self.roots.push(id);
    }

    /// Remove a root object
    ///
    /// # Errors
    /// Returns a `RuntimeError` if the requested `AllocId` is not rooted. This
    /// error can be safely ignored if that behavior is desired.
    ///
    /// [`RuntimeError`]: crate.RuntimeError
    /// [`AllocId`]: crate.AllocId
    pub fn remove_root(&mut self, id: AllocId) -> Result<()> {
        let id = id.into();

        trace!("Removing GC Root: {:?}", id);

        if let Some(index) = self.roots.iter().position(|root_id| *root_id == id) {
            self.roots.remove(index);

            if self.options.burn_gc {
                self.collect();
            }

            return Ok(());
        }

        Err(RuntimeError {
            ty: RuntimeErrorTy::GcError,
            message: "The object to be unrooted does not exist".to_string(),
        })
    }

    /// Write the data `T` to the specified `AllocId`.
    ///
    /// # Errors
    ///
    /// Returns a `RuntimeError` if any of the following conditions are met:  
    /// * The requested `AllocId` does not exist  
    /// * The size of `T` and the allocated space are not equal  
    ///
    /// [`AllocId`]: crate.AllocId
    /// [`RuntimeError`]: crate.RuntimeError
    pub unsafe fn write<Id, T>(&self, id: AllocId, data: T) -> Result<()> {
        trace!("Writing to allocation {}", id);

        if let Some((ptr, val)) = self.allocations.get(&id) {
            if mem::size_of::<T>() == val.size {
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

    /// Gets the current heap side as a `HeapPointer`
    ///
    /// [`HeapPointer`]: crate.HeapPointer
    #[must_use]
    fn get_side(&self) -> HeapPointer {
        match self.current_side {
            Side::Left => self.left,
            Side::Right => self.right,
        }
    }

    /// Information about the state of the `Gc`. See `GcData` for details on the
    /// returned information
    ///
    /// [`Gc`]: crate.Gc
    /// [`GcData`]: crate.GcData
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

    /// See if the `Gc` contains an `AllocId`
    ///
    /// [`Gc`]: crate.Gc
    /// [`HeapPointer`]: crate.HeapPointer
    #[inline]
    pub fn contains(&self, id: AllocId) -> bool {
        self.allocations.iter().any(|(__id, _)| *__id == id)
    }
}

impl Drop for Gc {
    fn drop(&mut self) {
        // Get the memory page size
        let page_size = page_size();

        // Create the layout for a heap half
        let layout = alloc::Layout::from_size_align(self.options.heap_size, page_size)
            .expect("Failed to create GC memory block layout");

        if self.options.overwrite_heap {
            unsafe {
                ptr::write_bytes(*self.left, 0x00, self.options.heap_size);
                ptr::write_bytes(*self.right, 0x00, self.options.heap_size);
            }
        }

        // Deallocate the left and right heaps
        unsafe {
            alloc::dealloc(*self.left, layout);
            alloc::dealloc(*self.right, layout);
        }
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
        map: &mut HashMap<AllocId, (HeapPointer, Self), FxBuildHasher>,
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
