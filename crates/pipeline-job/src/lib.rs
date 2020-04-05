#[cfg(not(feature = "no_std"))]
use std::path::PathBuf;

#[derive(Debug)]
pub enum PipelineJob {
    #[cfg(not(feature = "no_std"))]
    ReadFile(PathBuf),

    ParseFile {
        source: String,
        file: u32,
    },

    Terminate,
}

impl PipelineJob {
    pub fn ty(&self) -> &'static str {
        match self {
            #[cfg(not(feature = "no_std"))]
            Self::ReadFile(_) => "ReadFile",

            Self::ParseFile { .. } => "ParseFile",

            Self::Terminate => "Terminate",
        }
    }
}
