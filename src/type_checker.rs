#![allow(dead_code)]

use crate::parser::ast::*;
use codespan::{FileId, Files};
use codespan_reporting::diagnostic::{Diagnostic, Label, Severity};
use std::collections::HashMap;
use string_interner::{StringInterner, Sym};

pub(crate) struct TypeChecker {
    types: HashMap<Sym, TypeDecl>,
    functions: HashMap<Sym, FunctionDecl>,
    codespan: Files,
    files: Vec<FileId>,
    current_file: FileId,
    diagnostics: Vec<Diagnostic>,
    errored: bool,
    interner: StringInterner<Sym>,
}

impl TypeChecker {
    pub fn new(
        interner: StringInterner<Sym>,
        types: usize,
        functions: usize,
        current_file: FileId,
        files: Vec<FileId>,
        codespan: Files,
    ) -> Self {
        Self {
            types: HashMap::with_capacity(types),
            functions: HashMap::with_capacity(functions),
            diagnostics: Vec::with_capacity(10),
            errored: false,
            current_file,
            files,
            codespan,
            interner,
        }
    }

    pub fn check(
        mut self,
        program: Vec<Program>,
    ) -> std::result::Result<Vec<Diagnostic>, Vec<Diagnostic>> {
        for def in program {
            match def {
                Program::TypeDecl(ty) => {
                    if let Some(old_def) = self.types.insert(ty.name, ty.clone()) {
                        self.errored = true;
                        self.diagnostics.push(Diagnostic::new(
                        Severity::Error,
                        "Redefined function",
                        Label::new(
                            self.current_file,
                            0..0,
                            format!(
                                "Already defined `{}type {}{}`, attempted to redefine with `{}type {}{}`",
                                {
                                    let mut vis = old_def.visibility.to_string();
                                    if vis.is_empty() {
                                        vis
                                    } else {
                                        vis.push(' ');
                                        vis
                                    }
                                },
                                self.interner.resolve(old_def.name).expect("The requested symbol doesn't exist"),
                                if old_def.generics.is_empty() {
                                    String::new()
                                } else {
                                    format!("<{}>", old_def.generics.iter().map(|ty| ty.to_string(&self.interner)).collect::<Vec<String>>().join(", "))
                                },
                                {
                                    let mut vis = ty.visibility.to_string();
                                    if vis.is_empty() {
                                        vis
                                    } else {
                                        vis.push(' ');
                                        vis
                                    }
                                },
                                self.interner.resolve(ty.name).expect("The requested symbol doesn't exist"),
                                if old_def.generics.is_empty() {
                                    String::new()
                                } else {
                                    format!("<{}>", old_def.generics.iter().map(|ty| ty.to_string(&self.interner)).collect::<Vec<String>>().join(", "))
                                },
                            ),
                        ),
                    ));
                    }
                }
                Program::FunctionDecl(func) => {
                    if let Some(old_def) = self.functions.insert(func.name, func.clone()) {
                        self.errored = true;
                        self.diagnostics.push(Diagnostic::new(
                            Severity::Error,
                            "Redefined function",
                            Label::new(
                                self.current_file,
                                0..0,
                                format!(
                                    "Already defined `{}fn {}{}({}) -> {}`, attempted to redefine with `{}fn {}{}({}) -> {}`",
                                    {
                                        let mut vis = old_def.visibility.to_string();
                                        if vis.is_empty() {
                                            vis
                                        } else {
                                            vis.push(' ');
                                            vis
                                        }
                                    },
                                    self.interner.resolve(old_def.name).expect("The requested symbol doesn't exist"),
                                    if old_def.generics.is_empty() {
                                        String::new()
                                    } else {
                                        format!("<{}>", old_def.generics.iter().map(|ty| ty.to_string(&self.interner)).collect::<Vec<String>>().join(", "))
                                    },
                                    old_def.arguments.iter().map(|(name, ty)| format!("{}: {}", self.interner.resolve(*name).unwrap(), ty.to_string(&self.interner))).collect::<Vec<String>>().join(", "),
                                    old_def.returns.to_string(&self.interner),
                                    {
                                        let mut vis = func.visibility.to_string();
                                        if vis.is_empty() {
                                            vis
                                        } else {
                                            vis.push(' ');
                                            vis
                                        }
                                    },
                                    self.interner.resolve(func.name).expect("The requested symbol doesn't exist"),
                                    if func.generics.is_empty() {
                                        String::new()
                                    } else {
                                        format!("<{}>", func.generics.iter().map(|ty| ty.to_string(&self.interner)).collect::<Vec<String>>().join(", "))
                                    },
                                    func.arguments.iter().map(|(name, ty)| format!("{}: {}", self.interner.resolve(*name).unwrap(), ty.to_string(&self.interner))).collect::<Vec<String>>().join(", "),
                                    func.returns.to_string(&self.interner),
                                ),
                            ),
                        ));
                    }
                }
                Program::Import(_import) => todo!("Typecheck imports"),
            }
        }

        if self.errored {
            Err(self.diagnostics)
        } else {
            Ok(self.diagnostics)
        }
    }
}
