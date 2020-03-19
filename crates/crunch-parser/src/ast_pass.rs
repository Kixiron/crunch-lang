use crate::{
    ast::{FunctionDecl, Import, TypeDecl},
    symbol_table::SymbolTable,
};

use crunch_error::parse_prelude::*;
use string_interner::{StringInterner, Sym};

pub trait AstPass<'a> {
    fn requires() -> Vec<AstPassRequires> {
        vec![]
    }

    fn visit_function(
        &mut self,
        func: &'a FunctionDecl,
        interner: &StringInterner<Sym>,
        errors: &'a mut Vec<ParserDiagnostic>,
        extras: Vec<AstPassExtra<'a>>,
    );

    fn visit_type(
        &mut self,
        ty: &'a TypeDecl,
        interner: &StringInterner<Sym>,
        errors: &'a mut Vec<ParserDiagnostic>,
        extras: Vec<AstPassExtra<'a>>,
    );

    fn visit_import(
        &mut self,
        import: &'a Import,
        interner: &StringInterner<Sym>,
        errors: &'a mut Vec<ParserDiagnostic>,
        extras: Vec<AstPassExtra<'a>>,
    );
}

#[derive(Debug, Clone)]
pub enum AstPassExtra<'a> {
    FilePath(&'a [&'a str]),
    SymbolTable(&'a SymbolTable),
}

impl<'a> AstPassExtra<'a> {
    pub fn as_file_path(self) -> Option<&'a [&'a str]> {
        if let Self::FilePath(path) = self {
            Some(path)
        } else {
            None
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum AstPassRequires {
    FilePaths,
}
