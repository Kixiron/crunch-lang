use std::path::PathBuf;
use structopt::StructOpt;

#[derive(StructOpt)]
#[structopt(
    name = "Crunch",
    about = "The Crunch Language",
    rename_all = "kebab-case"
)]
pub struct CrunchCli {
    #[structopt(subcommand)]
    pub(crate) cmd: Option<CrunchCmd>,
}

#[derive(StructOpt)]
pub enum CrunchCmd {
    /// Run a file in the crunch interpreter
    ///
    /// Runs a file if specified and a project if in a project directory
    #[structopt(name = "run")]
    Run {
        /// The path of the file to interpret.
        ///
        /// Opens the requested file and runs using the Crunch interpreter.
        #[structopt(parse(from_os_str))]
        file_path: Option<PathBuf>,
    },

    /// Run the Crunch interpreter
    #[structopt(name = "prompt")]
    Prompt {},

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

    /// Get help with crunch
    #[structopt(name = "help")]
    Help {},
}
