use alloc::alloc::{GlobalAlloc, Layout};
use stats_alloc::{Region, Stats, StatsAlloc, INSTRUMENTED_SYSTEM};
use std::{alloc::System, time::Instant};

pub static CRUNCHC_ALLOCATOR: CrunchcAllocator = CrunchcAllocator {
    alloc: &INSTRUMENTED_SYSTEM,
};

#[derive(Debug, Copy, Clone)]
#[repr(transparent)]
pub struct CrunchcAllocator {
    alloc: &'static StatsAlloc<System>,
}

impl CrunchcAllocator {
    pub fn record_region<S, F, T>(&self, region_name: S, with: F) -> T
    where
        S: AsRef<str>,
        F: FnOnce() -> T,
    {
        let region = Region::new(self.alloc);
        let start = Instant::now();
        let ret = with();
        let elapsed = start.elapsed();

        let Stats {
            allocations,
            deallocations,
            reallocations,
            bytes_allocated,
            bytes_deallocated,
            bytes_reallocated,
        } = region.change();

        crate::info!("• Region '{}' finished", region_name.as_ref());
        crate::info!(
            "    • Finished in {}sec, {}ms and {}μs",
            elapsed.as_secs(),
            elapsed.subsec_millis(),
            elapsed.subsec_micros() % 1000,
        );
        crate::info!(
            "    • Allocated {} bytes in {} events",
            bytes_allocated,
            allocations,
        );
        crate::info!(
            "    • Deallocated {} bytes in {} events",
            bytes_deallocated,
            deallocations,
        );
        crate::info!(
            "    • Reallocated {} bytes in {} events",
            bytes_reallocated,
            reallocations,
        );

        ret
    }
}

unsafe impl GlobalAlloc for CrunchcAllocator {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        self.alloc.alloc(layout)
    }

    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        self.alloc.dealloc(ptr, layout)
    }
}
