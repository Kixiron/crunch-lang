use std::path::PathBuf;
use structopt::StructOpt;

#[derive(Debug, StructOpt)]
#[structopt(
    name = "Crunch",
    about = "The Crunch interpreter",
    rename_all = "kebab-case"
)]
pub struct Cli {
    /// The path of the file to interpret. If left empty, an interactive prompt will open
    ///
    /// Opens the requested file and runs using the Crunch interpreter. If no file is specified,
    /// then an interactive Crunch prompt will be opened.
    #[structopt(parse(from_os_str))]
    pub file_path: Option<PathBuf>,

    /// Displays info about Crunch
    #[structopt(long = "info")]
    pub display_info: bool,
}

#[derive(StructOpt)]
#[structopt(
    name = "Crunch",
    about = "The Crunch Language",
    rename_all = "kebab-case"
)]
pub enum CrunchCli {
    /// Run the crunch interpreter
    ///
    /// Runs a REPL if no file is specified, a file if specified and a project
    /// if in a project directory
    #[structopt(name = "run")]
    Run {
        /// The path of the file to interpret. If left empty, an interactive prompt will open
        ///
        /// Opens the requested file and runs using the Crunch interpreter.
        #[structopt(parse(from_os_str))]
        file_path: Option<PathBuf>,
    },

    /// Build a crunch file or project
    #[structopt(name = "build")]
    Build {},

    /// Test a crunch file or project
    #[structopt(name = "test")]
    Test {},

    /// Document a crunch file or project
    #[structopt(name = "doc")]
    Doc {},

    /// Update your installation of Crunch
    #[structopt(name = "update")]
    Update {},

    /// Get help
    #[structopt(name = "help")]
    Help {},
}
