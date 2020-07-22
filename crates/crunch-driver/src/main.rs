use crunch_codegen::{
    llvm::{
        target_machine::{CodegenFileKind, Target, TargetConf, TargetMachine},
        Context,
    },
    CodeGenerator,
};
use crunch_database::{ContextDatabase, CrunchDatabase, ParseDatabase, SourceDatabase};
use crunch_mir::MirBuilder;
use crunch_shared::{
    allocator::{CrunchcAllocator, CRUNCHC_ALLOCATOR},
    context::Context as ParseContext,
    files::{FileId, Files},
    symbol_table::Resolver,
    visitors::ast::ItemVisitor,
};
use crunch_typecheck::Engine;
use ladder::Ladder;
use std::{
    borrow::Cow,
    fmt, fs,
    io::{self, Write},
    path::PathBuf,
    str::FromStr,
    time::Instant,
};
use structopt::StructOpt;

#[global_allocator]
static GLOBAL_ALLOCATOR: CrunchcAllocator = CRUNCHC_ALLOCATOR;

fn main() {
    let code = {
        let args = CrunchcOpts::from_args();
        let options = args.build_options();
        let mut stderr = Stderr::new(&options);

        match GLOBAL_ALLOCATOR.record_region("driver", || run(&mut stderr, args, options)) {
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
    };

    // exit immediately terminates the program, so make sure everything is cleaned
    // up before that so that we don't leak anything
    std::process::exit(code);
}

fn run(
    stderr: &mut Stderr,
    args: CrunchcOpts,
    options: BuildOptions,
) -> Result<ExitStatus, ExitStatus> {
    let start_time = Instant::now();

    // Users can't enable both the verbose and quiet flags
    if options.is_verbose() && options.quiet {
        return Err(ExitStatus::message(
            "The `--verbose` and `--quiet` options are exclusive",
        ));

    // If verbose is enabled, enable logging
    // TODO: Switch to env_logger to allow the user to better configure logging
    //       via environmental variables
    // TODO: Make different levels of verbosity actually do something
    } else if options.is_verbose() {
        use simplelog::{ConfigBuilder, LevelFilter, TermLogger, TerminalMode};

        let log_level = LevelFilter::Trace;
        let config = ConfigBuilder::new().add_filter_ignore_str("salsa").build();
        let terminal_mode = TerminalMode::Stderr;

        TermLogger::init(log_level, config, terminal_mode).ok();
    }

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

    // Read the source of the given file into a string
    let source = fs::read_to_string(&options.target_file).map_err(|err| {
        ExitStatus::message(format!(
            "failed to read the source file '{}': {:?}",
            options.target_file.display(),
            err,
        ))
    })?;

    // Create the parsing context
    let context = ParseContext::default();

    let mut database = CrunchDatabase::default();
    database.set_context(context.clone());
    database.set_source_text(FileId::new(0), std::sync::Arc::new(source.clone()));
    database.set_file_name(FileId::new(0), source_file.clone().to_string());

    // Create the files for error reporting
    let mut files = Files::new();
    files.add(source_file.clone(), source.clone());

    // Parse the source file into an ast
    let ast = match database.parse(FileId::new(0)) {
        Ok(ok) => {
            // TODO: Stop cloning things
            let (ast, mut warnings) = ok.as_ref().clone();
            warnings.emit(&files);

            if options.emit.contains(&EmissionKind::Ast) {
                let path = out_file.with_extension("ast");

                fs::write(&path, format!("{:#?}", &ast)).map_err(|err| {
                    ExitStatus::message(format!(
                        "failed to write ast to '{}': {:?}",
                        path.display(),
                        err,
                    ))
                })?;
            }

            if options.print.contains(&EmissionKind::Ast) {
                println!("{:#?}", &ast);
            }

            ast
        }

        Err(errors) => {
            errors.as_ref().clone().emit(&files);
            return Err(ExitStatus::default());
        }
    };

    // Resolve all names in the ast
    let _resolver = GLOBAL_ALLOCATOR.record_region("symbol table building", || {
        use crunch_shared::trees::ItemPath;

        let mut resolver = Resolver::new(ItemPath::new(context.strings().intern(&source_file)));
        for node in ast.iter() {
            resolver.visit_item(node);
        }
        resolver.finalize();

        resolver
    });

    // Lower the ast to hir
    let hir =
        GLOBAL_ALLOCATOR.record_region("hir lowering", || Ladder::new(context.clone()).lower(&ast));
    if options.emit.contains(&EmissionKind::Hir) {
        let path = out_file.with_extension("hir");

        fs::write(&path, format!("{:#?}", &hir)).map_err(|err| {
            ExitStatus::message(format!(
                "failed to write hir to '{}': {:?}",
                path.display(),
                err
            ))
        })?;
    }

    if options.print.contains(&EmissionKind::Hir) {
        println!("{:#?}", &hir);
    }

    // Check types and update the hir with concrete types
    let mut engine = Engine::new(context.clone());
    match GLOBAL_ALLOCATOR.record_region("type checking", || engine.walk(&hir)) {
        Ok(mut warnings) => warnings.emit(&files),
        Err(mut errors) => {
            errors.emit(&files);

            return Err(ExitStatus::default());
        }
    }

    // Lower hir to mir
    let mir = GLOBAL_ALLOCATOR.record_region("mir lowering", || {
        MirBuilder::new(context.clone(), engine)
            .lower(&hir)
            .unwrap()
    });

    if options.emit.contains(&EmissionKind::Mir) {
        let path = out_file.with_extension("mir");

        fs::write(&path, format!("{}", mir.write_pretty(&context.strings))).map_err(|err| {
            ExitStatus::message(format!(
                "failed to write mir to '{}': {:?}",
                path.display(),
                err
            ))
        })?;
    }

    if options.print.contains(&EmissionKind::Mir) {
        println!("{}", mir.write_pretty(&context.strings));
    }

    // Create LLVM context
    let llvm_context = Context::new().map_err(|err| {
        ExitStatus::message(format!(
            "Failed to create LLVM context for codegen: {:?}",
            err
        ))
    })?;
    let module = llvm_context
        .module(&format!("{}.crunch", source_file))
        .map_err(|err| ExitStatus::message(format!("error creating LLVM module: {:?}", err)))?;

    // Generate LLVM ir
    GLOBAL_ALLOCATOR.record_region("code generation", || {
        CodeGenerator::new(&module, &context.strings)
            .generate(mir)
            .map_err(|err| {
                ExitStatus::message(format!("encountered an error during codegen: {:?}", err))
            })
    })?;

    // Verify the generated module
    GLOBAL_ALLOCATOR.record_region("module verification", || {
        module
            .verify()
            .map_err(|err| ExitStatus::message(format!("generated invalid LLVM module: {:?}", err)))
    })?;

    if options.emit.contains(&EmissionKind::LlvmIr) {
        let path = out_file.with_extension("ll");

        module.emit_ir_to_file(&path).map_err(|err| {
            ExitStatus::message(format!(
                "encountered an error while emitting LLVM IR to '{}': {:?}",
                path.display(),
                err,
            ))
        })?;
    }

    // TODO: Use native LLVM function since it's probably more efficient
    if options.print.contains(&EmissionKind::LlvmIr) {
        crunch_shared::warn!("Using an inefficient method of printing LLVM IR to stdout");
        println!("{:?}", module);
    }

    if options.emit.contains(&EmissionKind::LlvmBc) {
        let path = out_file.with_extension("bc");

        module
            .emit_ir_to_file(options.out_dir.join(&path))
            .map_err(|err| {
                ExitStatus::message(format!(
                    "encountered an error while emitting LLVM bitcode to '{}': {:?}",
                    path.display(),
                    err,
                ))
            })?;
    }

    if options.emit.contains(&EmissionKind::LlvmBc) {
        // TODO: Print object file to stdout?
        println!("Printing LLVM Bitcode to stdout is currently unsupported");
    }

    // TODO: User input for all of this
    // FIXME: This is really funky with initializing and may not even work correctly
    const TARGET_TRIPLE: &str = "x86_64-pc-windows-msvc";
    let llvm_config = TargetConf::all();
    Target::init_native(llvm_config).map_err(|err| {
        ExitStatus::message(format!("failed to initialize native target: {:?}", err))
    })?;

    let target = Target::from_triple(TARGET_TRIPLE)
        .map_err(|err| ExitStatus::message(format!("failed to create LLVM target: {:?}", err)))?;

    let target_machine = TargetMachine::new(&target, TARGET_TRIPLE, None, None, None, None, None)
        .map_err(|err| {
        ExitStatus::message(format!("failed to create LLVM target machine: {:?}", err))
    })?;

    // Emit to an object file so we can link it
    let object_file = out_file.with_extension("o");
    GLOBAL_ALLOCATOR.record_region("write object file", || {
        target_machine
            .emit_to_file(&module, &object_file, CodegenFileKind::Object)
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
            .emit_to_file(&module, &out_file, CodegenFileKind::Assembly)
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
        out_file.clone()
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

#[derive(Debug, Clone, StructOpt)]
#[structopt(about = "The Crunch compiler", rename_all = "kebab-case")]
enum CrunchcOpts {
    /// Builds a source file, producing an executable
    Build {
        #[structopt(flatten)]
        options: BuildOptions,
    },

    /// Builds a source file, producing an executable and running it
    Run {
        #[structopt(flatten)]
        options: BuildOptions,
    },
}

impl CrunchcOpts {
    pub fn build_options(&self) -> BuildOptions {
        match self {
            Self::Build { options, .. } | Self::Run { options, .. } => options.clone(),
        }
    }
}

#[derive(Debug, Clone, StructOpt)]
#[structopt(rename_all = "kebab-case")]
struct BuildOptions {
    /// The file to be compiled
    #[structopt(name = "FILE")]
    pub target_file: PathBuf,

    /// The output file's name
    #[structopt(short = "o", long = "output")]
    pub out_file: Option<PathBuf>,

    /// Enable verbose output
    #[structopt(short = "v", long = "verbose", parse(from_occurrences))]
    pub verbose: u8,

    /// A list of types for the compiler to emit
    #[structopt(long = "emit", possible_values = &EmissionKind::VALUES)]
    pub emit: Vec<EmissionKind>,

    /// A list of types for the compiler to print
    #[structopt(long = "print", possible_values = &EmissionKind::VALUES)]
    pub print: Vec<EmissionKind>,

    /// The output directory
    #[structopt(default_value = "build")]
    pub out_dir: PathBuf,

    /// Silence all compiler output
    #[structopt(short = "q", long = "quiet")]
    pub quiet: bool,
}

impl BuildOptions {
    pub fn is_verbose(&self) -> bool {
        self.verbose != 0
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum EmissionKind {
    Ast,
    Hir,
    Mir,
    LlvmIr,
    LlvmBc,
    Object,
    Assembly,
}

impl EmissionKind {
    pub const VALUES: [&'static str; 7] = ["ast", "hir", "mir", "llvm-ir", "llvm-bc", "obj", "asm"];
}

impl FromStr for EmissionKind {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let emit = match s.to_lowercase().as_ref() {
            "ast" => Self::Ast,
            "hir" => Self::Hir,
            "mir" => Self::Mir,
            "llvm-ir" => Self::LlvmIr,
            "llvm-bc" => Self::LlvmBc,
            "obj" => Self::Object,
            "asm" => Self::Assembly,

            _ => return Err("Unrecognized emission kind"),
        };

        Ok(emit)
    }
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
