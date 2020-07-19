#[no_mangle]
pub static CRUNCHC_VERSION: &str = concat!(
    "crunchc v",
    env!("CARGO_PKG_VERSION"),
    " (",
    env!("CRUNCHC_GIT_HASH"),
    " ",
    env!("CRUNCHC_BUILD_DATE"),
    ")",
);
