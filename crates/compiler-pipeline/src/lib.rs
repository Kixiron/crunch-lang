#![cfg_attr(feature = "no_std", no_std)]

use cfg_if::cfg_if;
use log::{info, trace};
use pipeline_job::PipelineJob;

cfg_if! {
    if #[cfg(feature = "no_std")] {
        extern crate alloc;

        use alloc::collections::VecDeque;

        pub struct Pipeline {
            queue: VecDeque<PipelineJob>,
        }
    } else {
        use std::thread::JoinHandle;

        pub struct Pipeline {
            workers: Vec<JoinHandle<()>>,
            data: worker::Data,
        }
    }
}

#[cfg(feature = "no_std")]
impl Pipeline {
    compile_error!("Unimplemented");
}

#[cfg(not(feature = "no_std"))]
impl Pipeline {
    pub fn new(file: impl AsRef<std::path::Path>) -> Self {
        use crunch_parser::{Files, Interner};
        use parking_lot::{Mutex, RwLock};
        use std::{
            collections::VecDeque,
            sync::{atomic::AtomicBool, Arc, Barrier},
            thread::Builder,
        };

        let cpus = num_cpus::get();

        let data = worker::Data {
            queue: Arc::new(Mutex::new(VecDeque::with_capacity(cpus * 4))),
            barrier: Arc::new(Barrier::new(cpus)),
            failed_compile: Arc::new(AtomicBool::new(false)),
            interner: Interner::new(),
            files: Arc::new(RwLock::new(Files::new())),
        };

        let mut workers = Vec::with_capacity(cpus - 1);
        for i in 0..cpus - 1 {
            let data = data.clone();

            let worker = Builder::new()
                .name(format!("<crunch worker {:02}>", i))
                .spawn(move || worker::PipelineWorker::new(data).run())
                .unwrap();

            workers.push(worker);
        }

        data.queue
            .lock()
            .push_back(PipelineJob::ReadFile(file.as_ref().to_owned()));

        Self { workers, data }
    }

    pub fn run(self) {
        info!("Starting pipeline");

        worker::PipelineWorker::new(self.data).run();
    }
}

#[cfg(not(feature = "no_std"))]
mod worker {
    use crunch_parser::{Files, Interner};
    use log::trace;
    use parking_lot::{Mutex, RwLock};
    use pipeline_job::PipelineJob;
    use std::{
        collections::VecDeque,
        fs::File,
        io::Read,
        sync::{
            atomic::{AtomicBool, Ordering},
            Arc, Barrier,
        },
        thread::{self, JoinHandle},
    };

    #[derive(Debug, Clone)]
    pub(crate) struct Data {
        pub(crate) queue: Arc<Mutex<VecDeque<PipelineJob>>>,
        pub(crate) barrier: Arc<Barrier>,
        pub(crate) interner: Interner,
        pub(crate) failed_compile: Arc<AtomicBool>,
        pub(crate) files: Arc<RwLock<Files>>,
    }

    pub(crate) struct PipelineWorker {
        data: Data,
    }

    impl PipelineWorker {
        pub(crate) fn new(data: Data) -> Self {
            Self { data }
        }

        pub(crate) fn run(self) {
            // Wait to run
            self.data.barrier.wait();
            trace!(
                "{} starting execution",
                thread::current().name().unwrap_or("<unnamed thread>")
            );

            loop {
                let job = self.data.queue.lock().pop_front();

                if let Some(job) = job {
                    if self.exec_job(job) {
                        break;
                    }
                }
            }
        }

        pub fn exec_job(&self, job: PipelineJob) -> bool {
            trace!(
                "{} is executing {}",
                thread::current().name().unwrap_or("<unnamed thread>"),
                job.ty()
            );

            match job {
                PipelineJob::ReadFile(path) => {
                    let mut file = match File::open(&path) {
                        Ok(file) => file,
                        Err(err) => {
                            println!("Failed to open file: {:?}", err);
                            self.data.failed_compile.store(true, Ordering::Relaxed);
                            return false;
                        }
                    };

                    let mut source = String::new();
                    file.read_to_string(&mut source).unwrap();
                    let file = self
                        .data
                        .files
                        .write()
                        .add(path.to_string_lossy(), source.clone())
                        .unwrap()
                        .0;

                    self.data
                        .queue
                        .lock()
                        .push_back(PipelineJob::ParseFile { source, file });
                }

                PipelineJob::ParseFile { source, file } => {}

                PipelineJob::Terminate => {
                    return true;
                }
            }

            false
        }
    }
}
