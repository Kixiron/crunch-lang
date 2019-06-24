use super::Error;
use codespan::{ByteIndex, CodeMap, Span};
use codespan_reporting::{
    emit, termcolor::StandardStream, ColorArg, Diagnostic, Label, LabelStyle,
};
use std::{fmt, ops::Range, str::FromStr};

#[derive(Debug, Clone)]
pub struct EmittedError {
    message: String,
    code: Option<ErrorCode>,
    severity: Severity,
    ranges: Vec<Range<usize>>,
}

impl EmittedError {
    // TODO: Add more functionality for easier error creation and streamlining of error messages
    pub fn new(
        message: &str,
        code: Option<ErrorCode>,
        severity: Severity,
        ranges: &[Range<usize>],
    ) -> Self {
        Self {
            message: message.to_owned(),
            code,
            severity,
            ranges: Vec::from(ranges),
        }
    }

    pub fn new_bug(
        message: &str,
        code: Option<ErrorCode>,
        ranges: &[Range<usize>],
    ) -> Self {
        Self {
            message: message.to_owned(),
            code,
            severity: Severity::Bug,
            ranges: Vec::from(ranges),
        }
    }

    pub fn new_error(
        message: &str,
        code: Option<ErrorCode>,
        ranges: &[Range<usize>],
    ) -> Self {
        Self {
            message: message.to_owned(),
            code,
            severity: Severity::Error,
            ranges: Vec::from(ranges),
        }
    }

    pub fn new_warning(
        message: &str,
        code: Option<ErrorCode>,
        ranges: &[Range<usize>],
    ) -> Self {
        Self {
            message: message.to_owned(),
            code,
            severity: Severity::Warning,
            ranges: Vec::from(ranges),
        }
    }

    pub fn new_note(
        message: &str,
        code: Option<ErrorCode>,
        ranges: &[Range<usize>],
    ) -> Self {
        Self {
            message: message.to_owned(),
            code,
            severity: Severity::Note,
            ranges: Vec::from(ranges),
        }
    }

    pub fn new_help(
        message: &str,
        code: Option<ErrorCode>,
        ranges: &[Range<usize>],
    ) -> Self {
        Self {
            message: message.to_owned(),
            code,
            severity: Severity::Help,
            ranges: Vec::from(ranges),
        }
    }

    pub fn new_writer() -> StandardStream {
        StandardStream::stderr(ColorArg::from_str("always").unwrap().into())
    }

    pub fn emit(
        self,
        writer: &mut StandardStream,
        code_map: &CodeMap,
    ) -> Result<(), Error> {
        let diag: Diagnostic = self.into();
        emit(&mut writer.lock(), &code_map, &diag)?;
        Ok(())
    }
}

impl Into<Diagnostic> for EmittedError {
    fn into(self) -> Diagnostic {
        let mut diag = Diagnostic::new(self.severity.into(), self.message);

        if let Some(code) = self.code {
            diag = diag.with_code(code.to_string());
        }

        for range in self.ranges {
            diag = diag.with_label(Label::new(
                // ByteIndex has an offset of one
                Span::new(
                    ByteIndex::from((range.start + 1) as u32),
                    ByteIndex::from((range.end + 1) as u32),
                ),
                LabelStyle::Primary,
            ));
        }

        diag
    }
}

#[derive(Debug, Clone)]
pub enum Severity {
    Bug,
    Error,
    Warning,
    Note,
    Help,
}

impl Into<codespan_reporting::Severity> for Severity {
    fn into(self) -> codespan_reporting::Severity {
        match self {
            Severity::Bug => codespan_reporting::Severity::Bug,
            Severity::Error => codespan_reporting::Severity::Error,
            Severity::Warning => codespan_reporting::Severity::Warning,
            Severity::Note => codespan_reporting::Severity::Note,
            Severity::Help => codespan_reporting::Severity::Help,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ErrorCode {
    E001,
    E002,
    E003,
    E004,
    E005,
}

impl fmt::Display for ErrorCode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}
