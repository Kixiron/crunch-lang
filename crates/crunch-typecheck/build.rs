use std::{
    env,
    error::Error,
    fs::{self, DirEntry},
    io,
    path::Path,
    process::Command,
};

fn main() -> Result<(), Box<dyn Error>> {
    // Traverse ddlog/ and rerun the DDlog build if any files have changed
    traverse("ddlog", &|entry| {
        let path = dbg!(entry.path());
        if matches!(
            path.extension(),
            Some(ext) if ext == "dl" || ext == "dat"
        ) {
            println!("cargo:rerun-if-changed={}", path.display());
        }
    })?;

    build_ddlog()?;

    Ok(())
}

fn traverse<P: AsRef<Path>, F: Fn(&DirEntry)>(dir: P, func: &F) -> io::Result<()> {
    for entry in fs::read_dir(dir.as_ref())? {
        let entry = dbg!(entry?);
        let path = entry.path();

        if path.is_dir() {
            traverse(&path, func)?;
        } else {
            func(&entry);
        }
    }

    Ok(())
}

fn build_ddlog() -> Result<(), Box<dyn Error>> {
    if env::var("DDLOG_HOME").is_err() {
        println!("cargo:warning=`DDLOG_HOME` is not set, building may not work properly");
    }

    Command::new("ddlog")
        .args(&["-i", "ddlog/typecheck.dl", "-L", "."])
        .spawn()?
        .wait()?;

    Ok(())
}
