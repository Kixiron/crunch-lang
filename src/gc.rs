use std::{alloc, mem, ptr, slice};

/// 64mb per half of heap
const HEAP_HALF_SIZE: usize = 1000 * 1000 * 64;

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

#[derive(Debug)]
pub struct Gc {
    /// The current root objects
    roots: Vec<GcValue>,
    /// The left heap
    left: *mut u8,
    /// The right heap
    right: *mut u8,
    /// The start of free memory
    latest: usize,
    /// The heap side currently in use
    current_side: Side,
    /// A vector of each allocation's id and current pointer
    allocations: Vec<(usize, *mut u8)>,
}

impl Gc {
    /// Create a new GC instance
    pub fn new() -> Self {
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
            let left = unsafe { alloc::alloc_zeroed(layout) };
            // Right heap
            let right = unsafe { alloc::alloc_zeroed(layout) };

            (left, right)
        };

        Self {
            left,
            right,
            allocations: Vec::new(),
            roots: Vec::new(),
            current_side: Side::Left,
            latest: left as usize,
        }
    }

    /// Allocate the space for an object
    pub fn allocate<T>(&mut self, size: usize) -> Option<(GcValue, usize)> {
        let (end, ptr) = (self.latest + size, self.latest as *mut u8);
        let heap = self.get_side();

        // If the object is too large return None
        if end - heap as usize > HEAP_HALF_SIZE {
            return None;
        }

        // Generate the Id of the new allocation based off of it's pointer
        let mut new_id = ptr as usize;
        loop {
            if !self
                .allocations
                .iter()
                .any(|(id, _)| *id == new_id as usize)
            {
                self.allocations.push((new_id, ptr));
                break;
            }
            new_id += 1;
        }

        // Create the GcValue
        let value = GcValue {
            id: new_id,
            size: mem::size_of::<T>(),
            children: Vec::new(),
            marked: false,
        };

        // end = self.latest + size
        self.latest = end;

        Some((value, new_id))
    }

    /// Collect all unused objects and shift to the other heap half
    pub fn collect(&mut self) {
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
        self.latest = heap as usize;

        let mut new_allocations = Vec::new();
        // Iterate over allocations to keep to move them onto the new heap
        for (id, size) in keep {
            // Get the pointer to the location on the old heap
            let ptr = self.get_ptr(id).unwrap();

            // Unsafe Usage: Get the bytes from the object on the old heap and copy them onto the new heap
            unsafe {
                let target: &mut [u8] = slice::from_raw_parts_mut(self.latest as *mut _, size);
                target.copy_from_slice(slice::from_raw_parts(ptr, size));
            }

            // Push the new allocation to new_allocations
            new_allocations.push((id, self.latest as *mut u8));

            // Increment by the size of the moved object
            self.latest += size;
        }
        self.allocations = new_allocations;

        // Overwrite old heap
        unsafe {
            ptr::write_bytes(self.get_side(), 0, HEAP_HALF_SIZE);
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

    /// Fetches the current pointer associated with an id
    pub(crate) fn get_ptr(&self, id: usize) -> Option<*mut u8> {
        self.allocations
            .iter()
            .find(|alloc| alloc.0 == id)
            .map(|alloc| alloc.1)
    }

    /// Add a root object
    pub fn add_root(&mut self, root: GcValue) {
        self.roots.push(root);
    }

    /// Remove a root object
    pub fn remove_root(&mut self, id: usize) -> Result<(), ()> {
        if let Some(index) = self.roots.iter().position(|value| value.id == id) {
            self.roots.remove(index);
            return Ok(());
        }

        Err(())
    }

    /// Write to an object
    /// Note: No current way to verify that `data` does not overflow onto other allocations
    /// Function really should be unsafe
    pub fn write<T>(&self, id: usize, data: T) {
        let ptr = self.get_ptr(id).unwrap();

        unsafe {
            ptr::write(ptr as *mut T, data);
        }
    }

    /// Fetch an object's value
    pub fn fetch<T>(&self, id: usize) -> Option<T> {
        if let Some((_, ptr)) = self.allocations.iter().find(|(i, _)| *i == id) {
            Some(unsafe { ptr::read(*ptr as *const T) })
        } else {
            None
        }

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
    pub fn get_side(&self) -> *mut u8 {
        match self.current_side {
            Side::Left => self.left,
            Side::Right => self.right,
        }
    }

    /// Information about the state of the GC
    pub fn data(&self) -> GcData {
        GcData {
            heap_size: HEAP_HALF_SIZE,
            heap_usage: self.latest - self.get_side() as usize,
            num_roots: self.roots.len(),
            num_allocations: self.allocations.len(),
        }
    }

    /// See if the GC contains an Id
    pub fn contains(&self, id: usize) -> bool {
        self.allocations.iter().any(|(__id, _)| *__id == id)
    }
}

#[derive(Debug, Copy, Clone)]
pub struct GcData {
    heap_size: usize,
    heap_usage: usize,
    num_roots: usize,
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

#[derive(Debug, Clone)]
pub struct GcValue {
    id: usize,
    size: usize,
    children: Vec<GcValue>,
    marked: bool,
}

impl GcValue {
    /// Fetches The id and size of all children
    pub fn collect(&mut self) -> Vec<(usize, usize)> {
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

    /// Adds a child
    pub fn add_child(&mut self, child: GcValue) {
        self.children.push(child);
    }

    /// Unmarks self and all children
    pub fn unmark(&mut self) {
        self.marked = false;

        for child in &mut self.children {
            if child.marked {
                child.unmark();
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::mem::size_of;

    #[test]
    fn alloc_usizes() {
        let mut gc = Gc::new();

        let (keep, keep_id) = gc.allocate::<usize>(size_of::<usize>()).unwrap();
        gc.write(keep_id, usize::max_value());
        gc.add_root(keep);
        assert!(gc.contains(keep_id));
        assert!(gc.fetch::<usize>(keep_id) == Some(usize::max_value()));

        let (_discard, discard_id) = gc.allocate::<usize>(size_of::<usize>()).unwrap();
        gc.write(discard_id, usize::max_value() - 1);
        assert!(gc.contains(discard_id));
        assert!(gc.fetch::<usize>(discard_id) == Some(usize::max_value() - 1));

        gc.collect();

        assert!(gc.contains(keep_id));
        assert!(gc.fetch::<usize>(keep_id) == Some(usize::max_value()));

        assert!(!gc.contains(discard_id));
        assert!(gc.fetch::<usize>(discard_id).is_none());

        gc.collect();

        assert!(gc.contains(keep_id));
        assert!(gc.fetch::<usize>(keep_id) == Some(usize::max_value()));

        assert!(!gc.contains(discard_id));
        assert!(gc.fetch::<usize>(discard_id).is_none());

        assert!(gc.remove_root(keep_id).is_ok());

        gc.collect();

        assert!(!gc.contains(keep_id));
        assert!(gc.fetch::<usize>(keep_id).is_none());

        gc.collect();

        assert!(!gc.contains(keep_id));
        assert!(gc.fetch::<usize>(keep_id).is_none());
    }

    #[test]
    fn gc_test() {
        let mut gc = Gc::new();

        let (mut ten, ten_id) = gc.allocate::<usize>(size_of::<usize>()).unwrap();
        println!("Allocated usize: Ptr: {:p}", gc.get_ptr(ten_id).unwrap());
        gc.write(ten_id, 10);

        let (eleven, eleven_id) = gc.allocate::<usize>(size_of::<usize>()).unwrap();
        println!("Allocated usize: Ptr: {:p}", gc.get_ptr(eleven_id).unwrap());
        gc.write(eleven_id, 11);

        let (_twelve, twelve_id) = gc.allocate::<usize>(size_of::<usize>()).unwrap();
        println!("Allocated usize: Ptr: {:p}", gc.get_ptr(twelve_id).unwrap());
        gc.write(twelve_id, 12);

        ten.add_child(eleven);
        println!("Added Child to ten: {:?}", ten);

        gc.add_root(ten);
        println!("Added root to GC: {:?}", gc.roots);

        println!("=> Before Collect\n{}", gc.data());

        println!(
            "Ten: {:?}, Eleven: {:?}",
            gc.fetch::<usize>(ten_id),
            gc.fetch::<usize>(eleven_id)
        );

        gc.collect();
        println!("=> After Collect\n{}", gc.data());

        println!(
            "Ten: {:?}, Eleven: {:?}",
            gc.fetch::<usize>(ten_id),
            gc.fetch::<usize>(eleven_id)
        );

        println!("{:#?}", gc);
        gc.collect();
        println!("{:#?}", gc);

        println!(
            "Ten: {:?}, Eleven: {:?}",
            gc.fetch::<usize>(ten_id),
            gc.fetch::<usize>(eleven_id)
        );

        println!("=> After Second Collect\n{}", gc.data());
    }
}
