use crunch::*;
use structopt::StructOpt;

fn main() {
    color_backtrace::install();

    let opt = Opt::from_args();

    match opt.file.as_path().extension() {
        Some(ext) => match ext.to_os_string() {
            ref a if &*a == "crunch" => Crunch::run_source_file(&opt.file),
            ref b if &*b == "crunched" => Crunch::run_byte_file(&opt.file),
            _ => {
                println!("Please choose a valid .crunch or .crunched file");
            }
        },
        None => {
            println!("Please choose a valid .crunch or .crunched file");
        }
    }
}

#[derive(Debug, StructOpt)]
#[structopt(name = "crunch", about = "The command-line interface for Crunch")]
struct Opt {
    #[structopt(parse(from_os_str))]
    file: std::path::PathBuf,
}
