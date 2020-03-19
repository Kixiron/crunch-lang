use std::path::PathBuf;

pub enum PipelineJob {
    ReadFile(PathBuf),
    ParseFile { source: String, file: usize },
    Terminate,
}

impl PipelineJob {
    pub fn ty(&self) -> &'static str {
        match self {
            Self::ReadFile(_) => "ReadFile",
            Self::ParseFile { .. } => "ParseFile",
            Self::Terminate => "Terminate",
        }
    }
}
