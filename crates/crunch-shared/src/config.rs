use crate::{
    distance::{self, WordMode},
    utils::{DbgWrap, HashSet},
};
use alloc::sync::Arc;
use codespan_reporting::term::{
    termcolor::{ColorChoice, StandardStream},
    Config as TermConfig,
};
use core::str::FromStr;
use salsa::Database;
use std::path::PathBuf;
use structopt::StructOpt;

#[salsa::query_group(ConfigDatabaseStorage)]
pub trait ConfigDatabase: Database {
    #[salsa::input]
    fn config(&self) -> Arc<BuildOptions>;

    #[salsa::input]
    fn writer(&self) -> Arc<DbgWrap<StandardStream>>;

    #[salsa::input]
    fn stdout_config(&self) -> Arc<DbgWrap<TermConfig>>;
}

#[derive(Debug, Clone, StructOpt)]
#[structopt(about = "The Crunch compiler", rename_all = "kebab-case")]
pub enum CrunchcOpts {
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
    pub fn from_args() -> Self {
        <Self as StructOpt>::from_args()
    }

    pub fn build_options(&self) -> BuildOptions {
        match self {
            Self::Build { options, .. } | Self::Run { options, .. } => options.clone(),
        }
    }
}

#[derive(Debug, Clone, StructOpt)]
#[structopt(rename_all = "kebab-case")]
pub struct BuildOptions {
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

    /// Set the colors of terminal output
    #[structopt(long = "color", default_value = "auto", possible_values = &TermColor::VALUES)]
    pub color: TermColor,

    /// Set the maximum number of errors the compiler will collect before halting
    #[structopt(default_value = "50")]
    pub max_errors: usize,

    /// Experimental (unstable) flags to the compiler
    #[structopt(
        name = "flags",
        short = "Z",
        default_value = "",
        multiple = true,
        parse(try_from_str = parse_experimental_flag),
    )]
    pub experimental_flags: HashSet<&'static str>,
}

impl BuildOptions {
    pub fn new(target_file: impl Into<PathBuf>) -> Self {
        Self {
            target_file: target_file.into(),
            out_file: None,
            verbose: 0,
            emit: Vec::new(),
            print: Vec::new(),
            out_dir: PathBuf::from("build"),
            quiet: false,
            color: TermColor::Auto,
            max_errors: 50,
            experimental_flags: HashSet::default(),
        }
    }

    pub fn is_verbose(&self) -> bool {
        self.verbose != 0
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct ExperimentalFlag {
    pub flag: &'static str,
    pub description: Option<&'static str>,
}

impl ExperimentalFlag {
    pub fn new<D: Into<Option<&'static str>>>(flag: &'static str, description: D) -> Self {
        Self {
            flag,
            description: description.into(),
        }
    }
}

inventory::collect!(ExperimentalFlag);

fn parse_experimental_flag(user_flag: &str) -> Result<HashSet<&'static str>, String> {
    // Weird hack so that we can provide a default for `BuildOptions.experimental_flags` and
    // therefore not require the `-Z` flag to always be passed
    if user_flag.is_empty() {
        return Ok(HashSet::default());
    }

    for &ExperimentalFlag { flag, .. } in inventory::iter() {
        if flag == user_flag {
            let mut flags = HashSet::with_capacity_and_hasher(1, Default::default());
            flags.insert(flag);

            return Ok(flags);
        }
    }

    let suggestion = distance::find_best_match(
        user_flag,
        inventory::iter::<ExperimentalFlag>().map(|flag| flag.flag),
        None,
        WordMode::KebabCase,
    );

    if let Some(suggestion) = suggestion {
        Err(format!(
            "an experimental flag by the name '{}' does not exist, maybe you meant '-Z {}'?",
            user_flag, suggestion,
        ))
    } else {
        Err(format!(
            "an experimental flag by the name '{}' does not exist",
            user_flag,
        ))
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TermColor {
    Always,
    Auto,
    None,
}

impl TermColor {
    pub const VALUES: [&'static str; 4] = ["always", "auto", "never", "none"];
}

impl Into<ColorChoice> for TermColor {
    fn into(self) -> ColorChoice {
        match self {
            Self::Always => ColorChoice::Always,
            Self::Auto => ColorChoice::Auto,
            Self::None => ColorChoice::Never,
        }
    }
}

impl FromStr for TermColor {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let emit = match s.to_lowercase().as_ref() {
            "always" => Self::Always,
            "auto" => Self::Auto,
            "none" | "never" => Self::None,

            _ => return Err("Unrecognized terminal color"),
        };

        Ok(emit)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum EmissionKind {
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
