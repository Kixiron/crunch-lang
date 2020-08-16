use crunch_codegen::llvm::target_machine::{CodegenFileKind, Target, TargetConf, TargetMachine};
use crunch_database::{CodegenDatabase, ConfigDatabase, CrunchDatabase, SourceDatabase};
use crunch_shared::{
    allocator::{CrunchcAllocator, CRUNCHC_ALLOCATOR},
    codespan_reporting::term::{termcolor::StandardStream, Config as TermConfig},
    config::{BuildOptions, CrunchcOpts, EmissionKind, TermColor},
    context::{Arenas, Context, ContextDatabase, OwnedArenas},
    files::FileCache,
    utils::DbgWrap,
};
use std::{
    borrow::Cow,
    fmt, fs,
    io::{self, Write},
    sync::Arc,
    time::Instant,
};

#[global_allocator]
static GLOBAL_ALLOCATOR: CrunchcAllocator = CRUNCHC_ALLOCATOR;

fn main() {
    let code = {
        let args = CrunchcOpts::from_args();
        let options = args.build_options();
        let mut stderr = Stderr::new(&options);

        // Users can't enable both the verbose and quiet flags
        if options.is_verbose() && options.quiet {
            todo!("error here")

        // If verbose is enabled, enable logging
        // TODO: Switch to env_logger to allow the user to better configure logging
        //       via environmental variables
        // TODO: Make different levels of verbosity actually do something
        } else if options.is_verbose() {
            use crunch_shared::tracing;
            use tracing_subscriber::{layer::SubscriberExt, registry::Registry, EnvFilter};
            use tracing_tree::HierarchicalLayer;

            let env_layer = EnvFilter::try_from_env("CRUNCHC_LOG")
                .unwrap_or_else(|_| EnvFilter::new("trace,salsa=off"));
            let tree_layer = HierarchicalLayer::new(2)
                .with_ansi(match options.color {
                    TermColor::Always | TermColor::Auto => true,
                    TermColor::None => false,
                })
                .with_wraparound(80);

            let registry = Registry::default().with(env_layer).with(tree_layer);
            tracing::subscriber::set_global_default(registry)
                .unwrap_or_else(|err| eprintln!("failed to initialize logging: {:?}", err));
        }

        GLOBAL_ALLOCATOR.record_region("driver", || {
            let owned_arenas = OwnedArenas::default();
            let arenas = Arenas::from(&owned_arenas);
            let context = Context::new(arenas);

            match run(&mut stderr, args, options, &context) {
                Ok(ExitStatus { message, exit_code }) => {
                    if let Some(message) = message {
                        stderr.write(|| format!("{}\n", message));
                    }

                    exit_code.unwrap_or(0)
                }

                Err(ExitStatus { message, exit_code }) => {
                    if let Some(message) = message {
                        stderr.write(|| format!("crunchc failed to compile: {}\n", message));
                    }

                    exit_code.unwrap_or(101)
                }
            }
        })
    };

    // exit immediately terminates the program, so make sure everything is cleaned
    // up before that so that we don't leak anything
    std::process::exit(code);
}

fn run<'ctx>(
    stderr: &mut Stderr,
    args: CrunchcOpts,
    options: BuildOptions,
    context: &'ctx Context<'ctx>,
) -> Result<ExitStatus, ExitStatus> {
    let start_time = Instant::now();

    let writer = StandardStream::stderr(options.color.into());
    let stdout_conf = TermConfig::default();

    // Get the source file's name without an extension
    let source_file = options
        .target_file
        .file_stem()
        .ok_or_else(|| {
            ExitStatus::message(format!(
                "the given target file must be a file, {} is not",
                options.target_file.display(),
            ))
        })?
        .to_string_lossy();
    let out_file = options.out_dir.join(source_file.as_ref());
    stderr.write(|| format!("Compiling '{}.crunch'\n", &source_file));

    // Check that the given file has the `.crunch` extension
    {
        let source_file_extension = options
            .target_file
            .extension()
            .ok_or_else(|| {
                ExitStatus::message(format!(
                    "Crunch files must have the '.crunch' extension, and '{}' has no extension",
                    options.target_file.display()
                ))
            })?
            .to_string_lossy();

        if source_file_extension != "crunch" {
            return Err(ExitStatus::message(format!(
                "Crunch files must have the '.crunch' extension, and '{}' has the '.{}' extension",
                options.target_file.display(),
                source_file_extension,
            )));
        }
    }

    // Create the build directory
    fs::create_dir_all(&options.out_dir).map_err(|err| {
        ExitStatus::message(format!(
            "failed to create build directory {}: {:?}",
            options.out_dir.display(),
            err,
        ))
    })?;

    let file_id = context.next_file_id();
    let mut database = CrunchDatabase::default();
    // Nothing in this function should ever escape it, right?
    // Also, I fucking hate this
    database.set_config(Arc::new(options.clone()));
    database.set_writer(Arc::new(DbgWrap::new(StandardStream::stderr(
        database.config().color.into(),
    ))));
    database.set_stdout_config(Arc::new(DbgWrap::new(TermConfig::default())));
    database.set_context(unsafe {
        core::mem::transmute::<&'ctx Context<'ctx>, &'static Context<'static>>(context)
    });
    database.set_file_path(file_id, Arc::new(options.target_file.clone()));

    // Check types and update the hir with concrete types
    let module = match database.generate_module(file_id) {
        Ok(ok) => ok,
        Err(errors) => {
            (&*errors)
                .clone()
                .emit(&FileCache::upcast(&database), &writer, &stdout_conf);

            return Err(ExitStatus::default());
        }
    };

    // TODO: User input for all of this
    // FIXME: This is really funky with initializing and may not even work correctly

    Target::init_native(TargetConf::all()).unwrap();
    let target_machine = TargetMachine::default();

    // Emit to an object file so we can link it
    let object_file = out_file.with_extension("o");
    GLOBAL_ALLOCATOR.record_region("write object file", || {
        target_machine
            .emit_to_file(module.get(), &object_file, CodegenFileKind::Object)
            .map_err(|err| {
                ExitStatus::message(format!(
                    "encountered an error while emitting object file to '{}': {:?}",
                    object_file.display(),
                    err
                ))
            })
    })?;

    if options.emit.contains(&EmissionKind::Object) {
        // TODO: Print object file to stdout?
        println!("Printing object files to stdout is currently unsupported");
    }

    if options.emit.contains(&EmissionKind::Assembly) {
        let path = out_file.with_extension("s");

        target_machine
            .emit_to_file(module.get(), &out_file, CodegenFileKind::Assembly)
            .map_err(|err| {
                ExitStatus::message(format!(
                    "encountered an error while emitting assembly to '{}': {:?}",
                    path.display(),
                    err
                ))
            })?;
    }

    if options.emit.contains(&EmissionKind::Assembly) {
        // TODO: Print assembly to stdout
        println!("Printing assembly to stdout is currently unsupported");
    }

    let exe_path = if let Some(ref out) = options.out_file {
        options.out_dir.join(out)
    // TODO: Replace with seeing if the *target* is windows
    } else if cfg!(windows) {
        out_file.with_extension("exe")
    } else {
        out_file
    };

    // TODO: Use `cc` to get the relevant linkers
    GLOBAL_ALLOCATOR.record_region("linking", || {
        std::process::Command::new("clang")
            .arg(&object_file)
            .arg("-o")
            .arg(&exe_path)
            .spawn()
            .and_then(|mut linker| linker.wait())
            .map_err(|err| ExitStatus::message(format!("failed to link: {:?}", err)))
    })?;

    let build_time = start_time.elapsed();
    stderr.write(|| {
        format!(
            "Finished building in {:.2} seconds\n",
            build_time.as_secs_f64(),
        )
    });

    if let CrunchcOpts::Run { .. } = args {
        let status = std::process::Command::new(&exe_path)
            .spawn()
            .and_then(|mut target| target.wait())
            .map_err(|err| ExitStatus::message(format!("failed to run child process: {:?}", err)))?
            .code();

        if let Some(code) = status {
            return Ok(ExitStatus::new(
                format!(
                    "running '{}' exited with the status code {}",
                    exe_path.display(),
                    code,
                ),
                code,
            ));
        }
    }

    Ok(ExitStatus::default())
}

struct Stderr {
    stderr: Option<io::Stderr>,
}

impl Stderr {
    pub fn new(options: &BuildOptions) -> Self {
        let stderr = if options.quiet {
            None
        } else {
            Some(io::stderr())
        };

        Self { stderr }
    }

    pub fn write<F, D>(&mut self, message: F)
    where
        F: FnOnce() -> D,
        D: fmt::Display,
    {
        if let Some(ref mut stderr) = self.stderr {
            write!(stderr, "{}", message()).expect("Encountered an error printing to stderr");
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
struct ExitStatus {
    message: Option<Cow<'static, str>>,
    exit_code: Option<i32>,
}

impl ExitStatus {
    pub fn new<M>(message: M, code: i32) -> Self
    where
        M: Into<Cow<'static, str>>,
    {
        Self {
            message: Some(message.into()),
            exit_code: Some(code),
        }
    }

    pub fn message<M>(message: M) -> Self
    where
        M: Into<Cow<'static, str>>,
    {
        Self {
            message: Some(message.into()),
            exit_code: None,
        }
    }
}
