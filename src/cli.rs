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
