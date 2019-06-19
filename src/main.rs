#![deny(
    clippy::all,
    clippy::correctness,
    clippy::style,
    clippy::complexity,
    clippy::perf,
    clippy::pedantic,
    clippy::nursery,
    clippy::cargo
)]
#![allow(non_snake_case)]

mod cli;
mod crunch;

#[doc(hidden)]
const fn email() -> &'static str {
    "kixiron.contact@gmail.com"
}

fn main() {
    use cli::Cli;
    use crunch::Crunch;
    use structopt::StructOpt;

    // Activate human_panic
    human_panic::setup_panic!(human_panic::Metadata {
        version: env!("CARGO_PKG_VERSION").into(),
        name: "Crunch".into(),
        authors: env!("CARGO_PKG_AUTHORS").replace(":", ", ").into(),
        homepage: env!("CARGO_PKG_HOMEPAGE").into(),
    });

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
