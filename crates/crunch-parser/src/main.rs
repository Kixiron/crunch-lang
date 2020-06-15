use crunch_parser::{ParseConfig, Parser};
use crunch_shared::{
    context::Context,
    files::{CurrentFile, Files},
};
use std::{
    alloc::{self, GlobalAlloc, Layout},
    fs::File,
    io::Read,
    sync::atomic::{AtomicUsize, Ordering},
};

#[global_allocator]
static ALLOCATOR: Record<alloc::System> = Record::new(alloc::System);

struct Record<A> {
    alloc: A,
    max: AtomicUsize,
    current: AtomicUsize,
}

impl<A> Record<A> {
    pub const fn new(alloc: A) -> Self {
        Self {
            alloc,
            max: AtomicUsize::new(0),
            current: AtomicUsize::new(0),
        }
    }

    pub fn max(&self) -> usize {
        self.max.load(Ordering::SeqCst)
    }
}

unsafe impl<A: GlobalAlloc> GlobalAlloc for Record<A>
where
    A: GlobalAlloc,
{
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        let size = layout.size();

        if self.current.fetch_add(size, Ordering::SeqCst) > self.max.load(Ordering::SeqCst) {
            // this is a race, but what you gonna do
            let current = self.current.load(Ordering::SeqCst);
            self.max.store(current, Ordering::SeqCst);
        }

        self.alloc.alloc(layout)
    }

    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        self.alloc.dealloc(ptr, layout);
        self.current.fetch_sub(layout.size(), Ordering::Release);
    }
}

fn main() {
    let mut file = File::open(
        std::env::args()
            .nth(1)
            .expect("You must provide a file to be parsed"),
    )
    .unwrap();
    let mut buf = String::with_capacity(10000);
    file.read_to_string(&mut buf).unwrap();

    let mut files = Files::new();
    let id = files.add("<test file>", &buf).unwrap();

    let context = Context::new();
    let start = std::time::Instant::now();
    match Parser::new(&buf, CurrentFile::new(id, buf.len()), context.clone())
        .with_config(ParseConfig {
            max_errors: 1,
            ..Default::default()
        })
        .parse()
    {
        Ok((ast, mut warn, ..)) => {
            warn.emit(&files);

            let _ast = ast;
        }

        Err(mut warn) => warn.emit(&files),
    }
    let elapsed = start.elapsed();

    println!(
        "Max memory usage: {:.2}MiB",
        ALLOCATOR.max() as f64 / 1024.0 / 1024.0,
    );
    println!("Time: {}ms", elapsed.as_millis());
}
