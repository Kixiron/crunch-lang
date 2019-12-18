use crate::{AllocId, HeapPointer, Result, RuntimeError, RuntimeErrorTy};
use std::{alloc, marker::PhantomData, mem, ptr, slice};

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
            allocations: Vec::new(),
            roots: Vec::new(),
            current_side: Side::Left,
            latest: left,
            options: GcOptions::from(options),
        }
    }

    /// Allocate the space for an object
    pub fn allocate(&mut self, size: usize) -> Result<(GcValue, AllocId)> {
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

        Ok((value, new_id))
    }

    /// Allocate the space for an object
    pub fn allocate_id<Id: Into<AllocId>>(
        &mut self,
        size: usize,
        id: Id,
    ) -> Result<(GcValue, AllocId)> {
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
            self.allocations.push((id, HeapPointer::new(block_start)));
        } else {
            trace!("Requested Id {:?} already exists, failing allocation", id);
            trace!("Gc Dump: {:?}", self);
            return Err(RuntimeError {
                ty: RuntimeErrorTy::GcError,
                message: "The requested ID already exists".to_string(),
            });
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

        Ok((value, id))
    }

    /// Collect all unused objects and shift to the other heap half
    pub fn collect(&mut self) -> Result<()> {
        trace!("GC Collecting");

        // The allocations to be transferred over to the new heap
        let mut keep = Vec::with_capacity(self.roots.len());
        // Get the valid allocations of roots and all children
        for root in &mut self.roots {
            if !root.marked {
                root.collect(&mut keep);
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

        let mut new_allocations = Vec::with_capacity(keep.len());
        // Iterate over allocations to keep to move them onto the new heap
        for (id, size) in keep {
            // Get the pointer to the location on the old heap
            let ptr = self.get_ptr(id)?;

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
                ptr::write_bytes(*self.get_side(), 0x00, self.options.heap_size);
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

        Ok(())
    }

    /// Fetch a currently allocated value
    fn fetch_value<Id: Into<AllocId> + Copy>(&self, id: Id) -> Result<&GcValue> {
        for root in &self.roots {
            if root.id == id.into() {
                return Ok(root);
            } else if let Some(value) = root.fetch_child(id) {
                return Ok(value);
            }
        }

        Err(RuntimeError {
            ty: RuntimeErrorTy::GcError,
            message: "Requested value does not exist".to_string(),
        })
    }

    /// Fetches the current pointer associated with an id
    #[inline]
    pub(crate) fn get_ptr<Id: Into<AllocId> + Copy>(&self, id: Id) -> Result<HeapPointer> {
        if let Some(ptr) = self.allocations.iter().find_map(|alloc| {
            if alloc.0 == id.into() {
                Some(alloc.1)
            } else {
                None
            }
        }) {
            Ok(ptr)
        } else {
            Err(RuntimeError {
                ty: RuntimeErrorTy::GcError,
                message: "Requested heap pointer does not exist".to_string(),
            })
        }
    }

    /// Add a root object
    #[inline]
    pub fn add_root(&mut self, root: GcValue) {
        trace!("Adding GC Root: {:?}", root);
        self.roots.push(root);
    }

    /// Remove a root object
    pub fn remove_root(&mut self, id: impl Into<AllocId> + Copy) -> Result<()> {
        let id = id.into();

        trace!("Removing GC Root: {:?}", id);

        if let Some(index) = self.roots.iter().position(|value| value.id == id) {
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
    /// Any object passed as `data` **must** be owned,
    pub unsafe fn write<T>(
        &self,
        id: impl Into<AllocId> + Copy,
        data: T,
        concrete_value: Option<&GcValue>,
    ) -> Result<()> {
        match (self.get_ptr(id), self.fetch_value(id), concrete_value) {
            (Ok(ptr), Ok(value), _) | (Ok(ptr), Err(_), Some(value)) => {
                if mem::size_of::<T>() == value.size {
                    ptr::write(*ptr as *mut T, data);

                    Ok(())
                } else {
                    Err(RuntimeError {
                        ty: RuntimeErrorTy::GcError,
                        message: format!(
                            "Size Misalign: {} != {}",
                            value.size,
                            mem::size_of::<T>()
                        ),
                    })
                }
            }

            _ => Err(RuntimeError {
                ty: RuntimeErrorTy::GcError,
                message: "Object to be written to does not exist".to_string(),
            }),
        }
    }

    /// Fetch an object's value
    pub fn fetch<T, Id: Into<AllocId> + Copy>(&self, id: Id) -> Result<T> {
        if let Some((_, ptr)) = self.allocations.iter().find(|(i, _)| *i == id.into()) {
            Ok(unsafe { ptr::read(**ptr as *const T) })
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
    pub fn collect(&mut self, vec: &mut Vec<(AllocId, usize)>) {
        self.marked = true;
        vec.push((self.id, self.size));

        for child in &mut self.children {
            if !child.marked {
                child.collect(vec);
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

    /// Adds a child
    #[inline]
    pub fn add_child(&mut self, child: Self) {
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

        let (obj, id) = gc.allocate(size_of::<T>() * len)?;
        #[cfg(debug_assertions)]
        unsafe {
            gc.write(id, vec.clone(), Some(&obj))?;
        }
        #[cfg(not(debug_assertions))]
        unsafe {
            gc.write(id, vec, Some(&obj))?;
        }
        gc.add_root(obj);

        #[cfg(debug_assertions)]
        {
            let raw = gc.fetch::<&[u8], AllocId>(id)?;
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

    pub fn new_id(vec: Vec<T>, id: impl Into<AllocId>, gc: &mut Gc) -> Result<Self> {
        use std::mem::size_of;

        let len = vec.len();

        let (obj, id) = gc.allocate_id(size_of::<T>() * len, id.into())?;
        #[cfg(debug_assertions)]
        unsafe {
            gc.write(id, vec.clone(), Some(&obj))?;
        }
        #[cfg(not(debug_assertions))]
        unsafe {
            gc.write(id, vec, Some(&obj))?;
        }
        gc.add_root(obj);

        #[cfg(debug_assertions)]
        {
            let raw = gc.fetch::<&[u8], AllocId>(id)?;
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

    pub fn to_vec<'a>(&self, gc: &'a Gc) -> Result<ForgetDrop<'a, Vec<T>>> {
        use std::mem::size_of;

        let raw = gc.fetch::<&[u8], AllocId>(self.id)?;
        Ok(unsafe {
            ForgetDrop(
                Vec::from_raw_parts(
                    raw.as_ptr() as *mut T,
                    raw.len() / size_of::<T>(),
                    raw.len() / size_of::<T>(),
                ),
                PhantomData,
            )
        })
    }

    pub fn drop(self, gc: &mut Gc) -> Result<()> {
        gc.remove_root(self.id)?;

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct ForgetDrop<'a, T>(T, PhantomData<&'a T>);

impl<'a, T> std::ops::Deref for ForgetDrop<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'a, T> Drop for ForgetDrop<'a, T> {
    fn drop(&mut self) {
        let mut tmp = unsafe { mem::MaybeUninit::zeroed().assume_init() };
        mem::swap(&mut self.0, &mut tmp);
        mem::forget(tmp);
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

                    let (obj, id) = gc.allocate(len)?;
                    #[cfg(debug_assertions)]
                    unsafe {
                        gc.write(id, int.clone(), Some(&obj))?;
                    }
                    #[cfg(not(debug_assertions))]
                    unsafe {
                        gc.write(id, int, Some(&obj))?;
                    }
                    gc.add_root(obj);

                    #[cfg(debug_assertions)]
                    debug_assert_eq!(
                        $internal::from_signed_bytes_be(&int),
                        $internal::from_signed_bytes_be(gc.fetch(id).expect("Value does not exist")),
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

                    let (obj, id) = gc.allocate(len)?;
                    #[cfg(debug_assertions)]
                    unsafe {
                        gc.write(id, int.clone(), Some(&obj))?;
                    }
                    #[cfg(not(debug_assertions))]
                    unsafe {
                        gc.write(id, int, Some(&obj))?;
                    }
                    gc.add_root(obj);

                    #[cfg(debug_assertions)]
                    debug_assert_eq!(
                        $internal::from_bytes_be(&int),
                        $internal::from_bytes_be(gc.fetch(id).expect("Value does not exist")),
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

        let (obj, id) = gc.allocate(len)?;
        #[cfg(debug_assertions)]
        unsafe {
            gc.write(id, int.clone(), Some(&obj))?;
        }
        #[cfg(not(debug_assertions))]
        unsafe {
            gc.write(id, int, Some(&obj))?;
        }
        gc.add_root(obj);

        #[cfg(debug_assertions)]
        debug_assert_eq!(
            num_bigint::BigInt::from_signed_bytes_be(&int),
            num_bigint::BigInt::from_signed_bytes_be(gc.fetch(id).expect("Value does not exist")),
        );

        Ok(Self { id, len })
    }

    pub fn new_id(
        int: impl num_bigint::ToBigInt,
        id: impl Into<AllocId>,
        gc: &mut Gc,
    ) -> Result<Self> {
        let int = if let Some(int) = int.to_bigint() {
            int.to_signed_bytes_be()
        } else {
            return Err(RuntimeError {
                ty: RuntimeErrorTy::InvalidInt,
                message: "Integer is not a valid integer".to_string(),
            });
        };
        let len = int.len();

        let (obj, id) = gc.allocate_id(len, id.into())?;
        #[cfg(debug_assertions)]
        unsafe {
            gc.write(id, int.clone(), Some(&obj))?;
        }
        #[cfg(not(debug_assertions))]
        unsafe {
            gc.write(id, int, Some(&obj))?;
        }
        gc.add_root(obj);

        #[cfg(debug_assertions)]
        debug_assert_eq!(
            num_bigint::BigInt::from_signed_bytes_be(&int),
            num_bigint::BigInt::from_signed_bytes_be(gc.fetch(id).expect("Value does not exist")),
        );

        Ok(Self { id, len })
    }

    pub fn bit_not<'a>(self, gc: &'a mut Gc) -> Result<Self> {
        let int = self.to_int(gc)?;
        unsafe {
            gc.write(self.id, !(*int).clone(), None)?;
        }

        Ok(self)
    }

    pub fn to_int<'a>(&self, gc: &'a Gc) -> Result<ForgetDrop<'a, num_bigint::BigInt>> {
        Ok(ForgetDrop(
            num_bigint::BigInt::from_signed_bytes_be(gc.fetch(self.id)?),
            PhantomData,
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

        let (obj, id) = gc.allocate(len)?;
        #[cfg(debug_assertions)]
        unsafe {
            gc.write(id, int.clone(), Some(&obj))?;
        }
        #[cfg(not(debug_assertions))]
        unsafe {
            gc.write(id, int, Some(&obj))?;
        }
        gc.add_root(obj);

        #[cfg(debug_assertions)]
        debug_assert_eq!(
            num_bigint::BigUint::from_bytes_be(&int),
            num_bigint::BigUint::from_bytes_be(gc.fetch(id).expect("Value does not exist")),
        );

        Ok(Self { id, len })
    }

    pub fn new_id(
        int: impl num_bigint::ToBigUint,
        id: impl Into<AllocId>,
        gc: &mut Gc,
    ) -> Result<Self> {
        let int = if let Some(int) = int.to_biguint() {
            int.to_bytes_be()
        } else {
            return Err(RuntimeError {
                ty: RuntimeErrorTy::InvalidInt,
                message: "Integer is not a valid integer".to_string(),
            });
        };
        let len = int.len();

        let (obj, id) = gc.allocate_id(len, id.into())?;
        #[cfg(debug_assertions)]
        unsafe {
            gc.write(id, int.clone(), Some(&obj))?;
        }
        #[cfg(not(debug_assertions))]
        unsafe {
            gc.write(id, int, Some(&obj))?;
        }
        gc.add_root(obj);

        #[cfg(debug_assertions)]
        debug_assert_eq!(
            num_bigint::BigUint::from_bytes_be(&int),
            num_bigint::BigUint::from_bytes_be(gc.fetch(id).expect("Value does not exist")),
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

    pub fn to_uint<'a>(&self, gc: &'a Gc) -> Result<ForgetDrop<'a, num_bigint::BigUint>> {
        Ok(ForgetDrop(
            num_bigint::BigUint::from_bytes_be(gc.fetch(self.id)?),
            PhantomData,
        ))
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

        let (obj, id) = gc.allocate(string.len())?;
        unsafe {
            gc.write(id, string.as_bytes().to_vec(), Some(&obj))?;
        }
        gc.add_root(obj);

        debug_assert_eq!(
            string,
            std::str::from_utf8(gc.fetch(id).expect("Value does not exist"))
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

        let (obj, id) = gc.allocate(left.len() + right.len())?;
        unsafe {
            gc.write(
                id,
                {
                    let mut vec = Vec::with_capacity(left.len() + right.len());
                    vec.extend_from_slice(left.as_bytes());
                    vec.extend_from_slice(right.as_bytes());
                    left
                },
                Some(&obj),
            )?;
        }
        gc.add_root(obj);

        debug_assert_eq!(
            {
                let mut left = left.to_string();
                left.push_str(right);
                left
            },
            std::str::from_utf8(gc.fetch(id).expect("Value does not exist"))
                .expect("Not valid utf-8")
        );

        Ok(Self {
            id,
            len: left.len() + right.len(),
            capacity: left.len() + right.len(),
        })
    }

    pub fn new_id(string: impl AsRef<str>, id: impl Into<AllocId>, gc: &mut Gc) -> Result<Self> {
        let string = string.as_ref();

        let (obj, id) = gc.allocate_id(string.len(), id)?;
        unsafe {
            gc.write(id, string.as_bytes(), Some(&obj))?;
        }
        gc.add_root(obj);

        Ok(Self {
            id,
            len: string.len(),
            capacity: string.len(),
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
        debug_assert!(
            std::str::from_utf8(gc.fetch(self.id).expect("Value does not exist")).is_ok()
        );

        unsafe { Ok(std::str::from_utf8_unchecked(gc.fetch(self.id)?)) }
    }

    pub fn drop(self, gc: &mut Gc) -> Result<()> {
        gc.remove_root(self.id)?;

        Ok(())
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
            gc.write(int_id, RuntimeValue::I32(10), Some(&int)).unwrap();
        }
        gc.add_root(int);
        assert!(gc.contains(int_id));
        assert!(gc.fetch(int_id) == Ok(RuntimeValue::I32(10)));

        let (int_2, int_2_id) = gc.allocate_id(size_of::<RuntimeValue>(), 0).unwrap();
        unsafe {
            gc.write(int_2_id, RuntimeValue::I32(20), Some(&int_2))
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
            gc.write(keep_id, usize::max_value(), Some(&keep)).unwrap();
        }
        gc.add_root(keep);
        assert!(gc.contains(keep_id));
        assert!(gc.fetch(keep_id) == Ok(usize::max_value()));

        let (discard, discard_id) = gc.allocate(size_of::<usize>()).unwrap();
        unsafe {
            gc.write(discard_id, usize::max_value() - 1, Some(&discard))
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
            gc.write(ten_id, 10_usize, Some(&ten)).unwrap();
        }

        let (eleven, eleven_id) = gc.allocate(size_of::<usize>()).unwrap();
        println!("Allocated usize: Ptr: {:p}", gc.get_ptr(eleven_id).unwrap());
        unsafe {
            gc.write(eleven_id, 11_usize, Some(&eleven)).unwrap();
        }

        let (twelve, twelve_id) = gc.allocate(size_of::<usize>()).unwrap();
        println!("Allocated usize: Ptr: {:p}", gc.get_ptr(twelve_id).unwrap());
        unsafe {
            gc.write(twelve_id, 12_usize, Some(&twelve)).unwrap();
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
