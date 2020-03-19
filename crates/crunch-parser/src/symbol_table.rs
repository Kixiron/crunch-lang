use crate::{ast::*, AstPass, AstPassExtra, AstPassRequires};

use crunch_error::parse_prelude::*;
use hashbrown::HashMap;
use string_interner::{StringInterner, Sym};

use alloc::vec::Vec;

#[derive(Debug, Clone)]
pub struct SymbolTable {
    functions: HashMap<Mangled, FunctionEntry>,
    types: HashMap<Mangled, TypeEntry>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            functions: HashMap::with_capacity(50),
            types: HashMap::with_capacity(50),
        }
    }
}

#[derive(Debug, Clone)]
pub struct SymbolTableBuilder {
    function_table: HashMap<Mangled, FunctionEntry>,
    type_table: HashMap<Mangled, TypeEntry>,
}

impl SymbolTableBuilder {
    pub fn new() -> Self {
        Self {
            function_table: HashMap::with_capacity(50),
            type_table: HashMap::with_capacity(50),
        }
    }

    pub fn into_inner(self) -> SymbolTable {
        SymbolTable {
            functions: self.function_table,
            types: self.type_table,
        }
    }

    pub fn mangle(path: &[&str]) -> Mangled {
        path.join(".")
    }

    pub fn demangle(path: &Mangled) -> Vec<String> {
        path.split(".").map(str::to_string).collect()
    }
}

impl<'a> AstPass<'a> for SymbolTableBuilder {
    fn requires() -> Vec<AstPassRequires> {
        vec![AstPassRequires::FilePaths]
    }

    fn visit_function(
        &mut self,
        func: &'a FunctionDecl,
        interner: &StringInterner<Sym>,
        errors: &'a mut Vec<ParserDiagnostic>,
        mut extras: Vec<AstPassExtra<'a>>,
    ) {
        let path = Self::mangle(extras.remove(0).as_file_path().unwrap());

        let entry = FunctionEntry::from(func);
        if let Some(prev) = self.function_table.insert(path.clone(), entry.clone()) {
            self.function_table
                .get_mut(&path)
                .unwrap()
                .conflicting_decls = true;

            errors.push(
                Diagnostic::new(Severity::Error)
                    .with_message(format!(
                        "The function `{}` has already been declared",
                        interner.resolve(entry.name).unwrap(),
                    ))
                    .with_code("E000")
                    .with_labels(vec![
                        Label::primary(prev.decl_loc.file, prev.decl_loc.range())
                            .with_message("First declared here"),
                        Label::secondary(entry.decl_loc.file, entry.decl_loc.range())
                            .with_message("Redeclared here"),
                    ])
                    .with_notes(vec![
                        "A function can only be declared once, try renaming it".to_string(),
                    ]),
            );
        }
    }

    fn visit_type(
        &mut self,
        func: &'a TypeDecl,
        interner: &StringInterner<Sym>,
        errors: &'a mut Vec<ParserDiagnostic>,
        mut extras: Vec<AstPassExtra<'a>>,
    ) {
        let path = Self::mangle(extras.remove(0).as_file_path().unwrap());

        let entry = TypeEntry::from(func);
        if let Some(prev) = self.type_table.insert(path.clone(), entry.clone()) {
            self.type_table.get_mut(&path).unwrap().conflicting_decls = true;

            errors.push(
                Diagnostic::new(Severity::Error)
                    .with_message(format!(
                        "The type `{}` has already been declared",
                        interner.resolve(entry.name).unwrap(),
                    ))
                    .with_code("E000")
                    .with_labels(vec![
                        Label::primary(prev.decl_loc.file, prev.decl_loc.range())
                            .with_message("First declared here"),
                        Label::secondary(entry.decl_loc.file, entry.decl_loc.range())
                            .with_message("Redeclared here"),
                    ])
                    .with_notes(vec![
                        "A type can only be declared once, try renaming it".to_string()
                    ]),
            );
        }
    }

    fn visit_import(
        &mut self,
        _import: &'a Import,
        _interner: &StringInterner<Sym>,
        _errors: &'a mut Vec<ParserDiagnostic>,
        _extras: Vec<AstPassExtra<'a>>,
    ) {
    }
}

pub type Mangled = String;

#[derive(Debug, Clone)]
pub struct FunctionEntry {
    name: Sym,
    exposure: Exposure,
    num_param: usize,
    decl_loc: Location,
    conflicting_decls: bool,
}

impl<'a> From<&'a FunctionDecl> for FunctionEntry {
    fn from(decl: &'a FunctionDecl) -> Self {
        Self {
            name: decl.name,
            exposure: decl
                .attributes
                .iter()
                .find(|attr| attr.is_exposure())
                .map(|a| a.clone().as_exposure().unwrap_or(Exposure::CurrentFile))
                .unwrap_or(Exposure::CurrentFile),
            num_param: decl.arguments.len(),
            decl_loc: decl.decl_loc.clone(),
            conflicting_decls: false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct MemberEntry {
    exposure: Exposure,
}

#[derive(Debug, Clone)]
pub struct TypeEntry {
    name: Sym,
    exposure: Exposure,
    members: HashMap<Sym, MemberEntry>,
    decl_loc: Location,
    conflicting_decls: bool,
}

impl<'a> From<&'a TypeDecl> for TypeEntry {
    fn from(decl: &'a TypeDecl) -> Self {
        Self {
            name: decl.name,
            exposure: decl
                .attributes
                .iter()
                .find(|attr| attr.is_exposure())
                .map(|a| a.clone().as_exposure().unwrap_or(Exposure::CurrentFile))
                .unwrap_or(Exposure::CurrentFile),
            members: decl
                .members
                .iter()
                .map(|(exp, name, _)| {
                    (
                        *name,
                        MemberEntry {
                            exposure: exp.clone(),
                        },
                    )
                })
                .collect(),
            decl_loc: decl.decl_loc.clone(),
            conflicting_decls: false,
        }
    }
}
