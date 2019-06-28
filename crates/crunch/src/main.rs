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

// Faster allocator for the unix family
// Custom allocators are not supported by windows
#[cfg(target_family = "unix")]
#[global_allocator]
static ALLOC: mimallocator::Mimalloc = mimallocator::Mimalloc;

mod cli;
mod crunch;

#[doc(hidden)]
const fn email() -> &'static str {
    "kixiron.contact@gmail.com"
}

fn main() {
    use cli::{CrunchCli, CrunchCmd};
    use crunch::Crunch;
    use structopt::StructOpt;

    // Activate human_panic
    human_panic::setup_panic!(human_panic::Metadata {
        version: env!("CARGO_PKG_VERSION").into(),
        name: "Crunch".into(),
        authors: env!("CARGO_PKG_AUTHORS").replace(":", ", ").into(),
        homepage: env!("CARGO_PKG_HOMEPAGE").into(),
    });

    let cli = CrunchCli::from_args();
    let mut crunch = Crunch::new();

    if let Some(cmd) = cli.cmd {
        match cmd {
            CrunchCmd::Run { file_path } => {
                if let Some(file_path) = file_path {
                    crunch.run_file(&file_path);
                } else {
                    unimplemented!();
                }
            }
            CrunchCmd::Prompt {} => {
                crunch.prompt();
            }
            CrunchCmd::Build {} => {}
            CrunchCmd::Test {} => {}
            CrunchCmd::Doc {} => {}
            CrunchCmd::Update {} => {}
            CrunchCmd::Help {} => {
                CrunchCli::clap()
                    .print_long_help()
                    .expect("Something went wrong! Please try again later.");
            }
        }
    } else {
        CrunchCli::clap()
            .print_help()
            .expect("Something went wrong! Please try again later.");
    }
}
