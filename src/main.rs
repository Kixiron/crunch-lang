mod cli;
mod crunch;
mod parser;

#[doc(hidden)]
const fn email() -> &'static str {
    "kixiron.contact@gmail.com"
}

fn main() {
    use cli::Cli;
    use crunch::Crunch;
    use structopt::StructOpt;

    let cli = Cli::from_args();
    let mut crunch = Crunch::new();

    if !cli.display_info {
        if let Some(file_path) = cli.file_path {
            crunch.run_file(&file_path);
        } else {
            crunch.prompt();
        }
    } else {
        crunch.display_info();
    }
}
