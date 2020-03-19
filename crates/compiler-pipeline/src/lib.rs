use crunch_error::{
    codespan_reporting,
    parse_prelude::{Diagnostic, SimpleFiles},
};
use crunch_parser::{
    string_interner::{StringInterner, Sym},
    SymbolTable,
};
use pipeline_job::PipelineJob;

use std::{
    collections::VecDeque,
    path::{Path, PathBuf},
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc, Barrier, Mutex, RwLock,
    },
    thread,
};

pub struct CompilerPipeline;

impl CompilerPipeline {
    pub fn run(self, file: impl AsRef<Path>) {
        let job_queue = {
            let mut queue = VecDeque::with_capacity(50);
            queue.push_back(PipelineJob::ReadFile(file.as_ref().to_owned()));

            Arc::new(Mutex::new(queue))
        };

        let (threads_to_spawn, mut threads, barrier) = {
            let num_threads = num_cpus::get();
            let threads = Vec::with_capacity(num_threads - 1);
            let barrier = Arc::new(Barrier::new(num_threads));

            (num_threads - 1, threads, barrier)
        };

        let data = PipelineData {
            symbol_table: Arc::new(RwLock::new(crunch_parser::SymbolTable::new())),
            string_interner: Arc::new(RwLock::new(StringInterner::with_capacity(100))),
            diagnostics: Arc::new(Mutex::new(Vec::with_capacity(50))),
            codespan: Arc::new(RwLock::new(SimpleFiles::new())),
            compilation_failed: Arc::new(AtomicBool::new(false)),
        };

        for i in 0..threads_to_spawn {
            let barrier = Arc::clone(&barrier);
            let job_queue = Arc::clone(&job_queue);
            let data = data.clone();

            let worker = thread::Builder::new()
                .name(format!("Worker thread {}", i))
                .spawn(move || {
                    println!("[Worker {:0>2}] Started", i);

                    worker_runtime(job_queue, barrier, data, i);
                })
                .unwrap();

            threads.push(worker);
        }

        worker_runtime(job_queue, barrier, data.clone(), threads_to_spawn + 1);

        emit_diagnostics(
            &data.codespan.read().unwrap(),
            &data.diagnostics.lock().unwrap(),
        );

        if data.compilation_failed.load(Ordering::SeqCst) {
            println!("[Compilation failed]");
        } else {
            println!("[Compilation Succeeded]");

            // TODO: Emit or Execute
        }
    }
}

// Waits for all threads to terminate on an `Err` output
fn worker_runtime(
    jobs: Arc<Mutex<VecDeque<PipelineJob>>>,
    barrier: Arc<Barrier>,
    data: PipelineData,
    worker_number: usize,
) {
    loop {
        let job = {
            if let Ok(mut jobs) = (*jobs).try_lock() {
                jobs.pop_front()
            } else {
                continue;
            }
        };

        if let Some(job) = job {
            println!(
                "[Worker {:0>2}] Received a job of type {}",
                worker_number,
                job.ty()
            );

            let result = match job {
                PipelineJob::ReadFile(path) => ReadPhase::run(&jobs, &data, path),

                PipelineJob::ParseFile { source, file } => {
                    ParsePhase::run(&jobs, &data, (source, file))
                }

                PipelineJob::Terminate => {
                    println!("[Worker {:0>2}] Shutting Down", worker_number);
                    jobs.lock().unwrap().push_back(PipelineJob::Terminate);
                    break;
                }
            };

            match result {
                Ok(tasks) => jobs.lock().unwrap().extend(tasks),
                Err(task) => {
                    data.compilation_failed.store(true, Ordering::SeqCst);
                    jobs.lock().unwrap().push_back(task);
                }
            }
        }
    }
}

fn emit_diagnostics(files: &SimpleFiles<String, String>, diagnostics: &Vec<Diagnostic<usize>>) {
    let writer = codespan_reporting::term::termcolor::StandardStream::stderr(
        codespan_reporting::term::termcolor::ColorChoice::Auto,
    );
    let config = codespan_reporting::term::Config::default();

    for diagnostic in diagnostics {
        codespan_reporting::term::emit(&mut writer.lock(), &config, files, diagnostic).unwrap();
    }
}

#[derive(Clone)]
pub struct PipelineData {
    symbol_table: Arc<RwLock<SymbolTable>>,
    string_interner: Arc<RwLock<StringInterner<Sym>>>,
    diagnostics: Arc<Mutex<Vec<Diagnostic<usize>>>>,
    codespan: Arc<RwLock<SimpleFiles<String, String>>>,
    compilation_failed: Arc<AtomicBool>,
}

pub trait Pipelined {
    type Job;

    fn run(
        job_queue: &Arc<Mutex<VecDeque<PipelineJob>>>,
        data: &PipelineData,
        job: Self::Job,
    ) -> Result<Vec<PipelineJob>, PipelineJob>;
}

struct ReadPhase;

impl Pipelined for ReadPhase {
    type Job = PathBuf;

    fn run(
        _job_queue: &Arc<Mutex<VecDeque<PipelineJob>>>,
        data: &PipelineData,
        path: Self::Job,
    ) -> Result<Vec<PipelineJob>, PipelineJob> {
        use std::{fs::File, io::Read};

        let mut source = String::with_capacity(1000);
        let mut file = File::open(&path).map_err(|_| PipelineJob::Terminate)?;

        file.read_to_string(&mut source)
            .map_err(|_| PipelineJob::Terminate)?;

        // TODO: Less shitty way to handle the file
        let path = path.to_str().unwrap().to_string();
        let file = data.codespan.write().unwrap().add(path, source.clone());

        Ok(vec![PipelineJob::ParseFile { source, file }])
    }
}

struct ParsePhase;

impl Pipelined for ParsePhase {
    type Job = (String, usize);

    fn run(
        job_queue: &Arc<Mutex<VecDeque<PipelineJob>>>,
        data: &PipelineData,
        (source, file): Self::Job,
    ) -> Result<Vec<PipelineJob>, PipelineJob> {
        let parsed = crunch_parser::Parser::new(
            &source,
            file,
            data.string_interner.clone(),
            job_queue.clone(),
        )
        .parse();

        match parsed {
            Ok((_ast, nonfatal)) => {
                data.diagnostics
                    .lock()
                    .unwrap()
                    .extend_from_slice(&nonfatal);

                // TODO: Next phase of compilation
                Ok(vec![PipelineJob::Terminate])
            }

            Err(fatal) => {
                data.diagnostics.lock().unwrap().extend_from_slice(&fatal);

                Err(PipelineJob::Terminate)
            }
        }
    }
}
