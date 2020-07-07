use crunch_codegen::{
    llvm::{
        target_machine::{CodegenFileKind, Target, TargetConf, TargetMachine},
        Context,
    },
    CodeGenerator,
};
use crunch_parser::{ExternUnnester, Parser};
use crunch_shared::{
    context::Context as ParseContext,
    files::{CurrentFile, FileId, Files},
    symbol_table::Resolver,
    trees::mir::MirBuilder,
    visitors::ast::ItemVisitor,
};
use ladder::Ladder;
use std::{error::Error, fs, path::PathBuf, str::FromStr};
use structopt::StructOpt;

fn main() -> Result<(), Box<dyn Error>> {
    let args = CrunchcOpts::from_args();
    fs::create_dir_all(&args.out_dir)?;

    if args.verbose {
        simple_logger::init().ok();
    }

    let file_name = args
        .target_file
        .file_name()
        .ok_or("Files must be a file")?
        .to_str()
        .ok_or("File names must be valid utf-8")?;
    let file_name_ext = file_name
        .splitn(2, ".")
        .next()
        .ok_or("Files must end in the .crunch extension")?;
    let source = fs::read_to_string(&args.target_file)?;

    let parse_ctx = ParseContext::default();
    let mut files = Files::new();
    files.add(file_name, source.clone());

    let parser = Parser::new(
        &source,
        CurrentFile::new(FileId::new(0), source.len()),
        parse_ctx.clone(),
    );

    let ast = match parser.parse() {
        Ok((ast, mut warnings)) => {
            warnings.emit(&files);

            let ast = ExternUnnester::new().unnest(ast);
            if args.emit.contains(&EmissionKind::Ast) {
                fs::write(
                    &args.out_dir.join(format!("{}.ast", file_name_ext)),
                    format!("{:#?}", &ast),
                )?;
            }

            ast
        }

        Err(mut errors) => {
            errors.emit(&files);
            return Ok(());
        }
    };

    let _resolver = {
        let mut resolver = Resolver::new(vec![parse_ctx.strings().intern(file_name)].into());
        for node in ast.iter() {
            resolver.visit_item(node);
        }
        resolver.finalize();

        resolver
    };

    let hir = Ladder::new().lower(&ast);
    if args.emit.contains(&EmissionKind::Hir) {
        fs::write(
            args.out_dir.join(format!("{}.hir", file_name_ext)),
            format!("{:#?}", &hir),
        )?;
    }

    // match Engine::new(parse_ctx.strings.clone()).walk(&mut hir) {
    //     Ok(mut warnings) => warnings.emit(&files),
    //     Err(mut errors) => {
    //         errors.emit(&files);
    //         return Ok(());
    //     }
    // }

    let mir = MirBuilder::new().lower(&hir).unwrap();
    if args.emit.contains(&EmissionKind::Mir) {
        fs::write(
            &args.out_dir.join(format!("{}.mir", file_name_ext)),
            format!("{:#?}", &mir),
        )?;
    }

    let ctx = Context::new()?;
    let module = ctx.module(file_name)?;
    CodeGenerator::new(&module, &parse_ctx.strings).generate(mir)?;
    module.verify()?;

    if args.emit.contains(&EmissionKind::LlvmIr) {
        module.emit_ir_to_file(args.out_dir.join(format!("{}.ll", file_name_ext)))?;
    }

    if args.emit.contains(&EmissionKind::LlvmBc) {
        module.emit_ir_to_file(args.out_dir.join(format!("{}.bc", file_name_ext)))?;
    }

    Target::init_native(TargetConf::all())?;
    let target = Target::from_triple("x86_64-pc-windows-msvc")?;
    let target_machine = TargetMachine::new(
        &target,
        "x86_64-pc-windows-msvc",
        None,
        None,
        None,
        None,
        None,
    )?;

    target_machine.emit_to_file(
        &module,
        args.out_dir.join(format!("{}.o", file_name_ext)),
        CodegenFileKind::Object,
    )?;

    if args.emit.contains(&EmissionKind::Assembly) {
        target_machine.emit_to_file(
            &module,
            args.out_dir.join(format!("{}.s", file_name_ext)),
            CodegenFileKind::Assembly,
        )?;
    }

    let exe_path = if let Some(ref out) = args.out_file {
        args.out_dir.join(out)
    } else {
        args.out_dir.join(&format!("{}.exe", file_name_ext))
    };
    // TODO: Use `cc` to get the relevant linkers
    std::process::Command::new("clang")
        .arg(args.out_dir.join(&format!("{}.o", file_name_ext)))
        .arg("-o")
        .arg(&exe_path)
        .spawn()?
        .wait()?;

    if args.mode == CompilerMode::Run {
        std::process::Command::new(exe_path).spawn()?.wait()?;
    }

    Ok(())
}

#[derive(Debug, Clone, StructOpt)]
#[structopt(about = "The Crunch compiler", rename_all = "kebab-case")]
struct CrunchcOpts {
    /// The mode to run the compiler in
    #[structopt(name = "MODE")]
    pub mode: CompilerMode,

    /// The file to be compiled
    #[structopt(name = "FILE")]
    pub target_file: PathBuf,

    /// The output file's name
    #[structopt(short = "o", long = "output")]
    pub out_file: Option<PathBuf>,

    /// Enable verbose output
    #[structopt(short = "v", long = "verbose")]
    pub verbose: bool,

    /// A list of types for the compiler to emit
    #[structopt(long = "emit")]
    pub emit: Vec<EmissionKind>,

    /// The output directory
    #[structopt(default_value = "build")]
    pub out_dir: PathBuf,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum CompilerMode {
    Build,
    Run,
}

impl FromStr for CompilerMode {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let emit = match s.to_lowercase().as_ref() {
            "build" => Self::Build,
            "run" => Self::Run,

            _ => return Err("Unrecognized run mode"),
        };

        Ok(emit)
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

/*
use crunch_parser::{ExternUnnester, Parser};
use crunch_shared::{
    context::Context as ParseContext,
    files::{CurrentFile, FileId, Files},
    symbol_table::Resolver,
    trees::mir::MirBuilder,
    utils::Timer,
    visitors::ast::ItemVisitor,
};
use ladder::Ladder;
use llvm::{
    target_machine::{CodegenFileKind, Target, TargetConf, TargetMachine},
    Context,
};
use std::fs::File;

simple_logger::init().ok();

let source = r#"
extern
    @callconv("C")
    fn puts(string: *const i8) -> i32;
end

fn main() -> i32
    return puts("Hello, world!")
end
"#;

let compilation = Timer::start("compilation");

let parse_ctx = ParseContext::default();
let mut files = Files::new();
files.add("<test>", source);

match Parser::new(
    source,
    CurrentFile::new(FileId::new(0), source.len()),
    parse_ctx.clone(),
)
.parse()
{
    Ok((items, mut warnings)) => {
        warnings.emit(&files);

        let items = ExternUnnester::new().unnest(items);

        let mut resolver = Resolver::new(vec![parse_ctx.strings().intern("<test>")].into());
        for item in items.iter() {
            resolver.visit_item(item);
        }
        resolver.finalize();

        let ladder = Ladder::new().lower(&items);
        let mir = MirBuilder::new().lower(&ladder).unwrap();

        let ctx = Context::new().unwrap();
        let module = ctx.module("crunch-module").unwrap();

        CodeGenerator::new(&module, &parse_ctx.strings)
            .generate(mir)
            .unwrap();

        module.verify().unwrap();

        let object_emission = Timer::start("object file emission");

        Target::init_native(TargetConf::all()).unwrap();
        let target = Target::from_triple("x86_64-pc-windows-msvc").unwrap();
        let target_machine = TargetMachine::new(
            &target,
            "x86_64-pc-windows-msvc",
            None,
            None,
            None,
            None,
            None,
        )
        .unwrap();

        target_machine
            .emit_to_file(&module, "crunch.o", CodegenFileKind::Object)
            .unwrap();

        object_emission.end();

        let linking = Timer::start("linking");

        // TODO: Use `cc` to get the relevant linkers
        std::process::Command::new("clang")
            .args(&["crunch.o", "-o", "crunch.exe"])
            .spawn()
            .unwrap()
            .wait()
            .unwrap();

        linking.end();
        compilation.end();

        let runtime = Timer::start("running executable");
        let command = std::process::Command::new("crunch.exe")
            .spawn()
            .unwrap()
            .wait_with_output()
            .unwrap();
        runtime.end();

        target_machine
            .emit_to_file(&module, "crunch.s", CodegenFileKind::Assembly)
            .unwrap();

        let llvm_ir = format!("{:?}", module)
            .trim()
            .lines()
            .map(|l| "    ".to_string() + l)
            .collect::<Vec<String>>()
            .join("\n");
        let assembly = std::fs::read_to_string("crunch.s")
            .unwrap()
            .trim()
            .lines()
            .map(|l| "    ".to_string() + l)
            .collect::<Vec<String>>()
            .join("\n");

        let source_len = source.as_bytes().len();
        let object_file_len = File::open("crunch.o").unwrap().metadata().unwrap().len();
        let executable_len = File::open("crunch.exe").unwrap().metadata().unwrap().len();

        println!(
            "Source Code:{}\n\n\
             LLVM IR:\n{}\n\n\
             Assembly:\n{}\n\n\
             File Sizes:\n    \
                 Source: {:>4} bytes\n    \
                 Object: {:>4} bytes\n    \
                 Binary: {:>4} bytes\n\n\
             Exited with code {:?}",
            source,
            llvm_ir,
            assembly,
            source_len,
            object_file_len,
            executable_len,
            command.status.code(),
        );
    }

    Err(mut err) => {
        err.emit(&files);
    }
}
*/
