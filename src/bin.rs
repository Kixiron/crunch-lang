use crunch::*;
use structopt::StructOpt;

fn main() {
    let opt = Opt::from_args();

    let bytecode = {
        use std::{fs::File, io::Read};

        let mut bytecode = Vec::new();
        let mut file = File::open(&opt.file).expect("File does not exist");
        file.read_to_end(&mut bytecode)
            .expect("Could not read file");

        bytecode
    };

    let mut crunch = {
        let instructions = Crunch::parse(Crunch::validate(&bytecode).unwrap());
        Crunch::from(instructions)
    };
    crunch.execute();
}

#[derive(Debug, StructOpt)]
#[structopt(name = "crunch", about = "The command-line interface for Crunch")]
struct Opt {
    #[structopt(parse(from_os_str))]
    file: std::path::PathBuf,
}
