use chrono::Utc;
use std::{error::Error, process::Command};

fn main() -> Result<(), Box<dyn Error>> {
    crunchc_git_hash()?;
    build_date();

    Ok(())
}

fn crunchc_git_hash() -> Result<(), Box<dyn Error>> {
    let hash = String::from_utf8(
        Command::new("git")
            .args(&["rev-parse", "--short", "HEAD"])
            .output()?
            .stdout,
    )?;
    println!("cargo:rustc-env=CRUNCHC_GIT_HASH={}", hash);

    Ok(())
}

fn build_date() {
    let date = Utc::now().format("%Y-%m-%d");

    println!("cargo:rustc-env=CRUNCHC_BUILD_DATE={}", date);
}
