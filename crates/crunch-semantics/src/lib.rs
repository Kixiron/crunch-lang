#![cfg_attr(feature = "no-std", no_std)]

extern crate alloc;

use alloc::{boxed::Box, vec::Vec};
use core::fmt;
use crunch_parser::{
    error::{Error, ErrorHandler, Locatable, Location, SemanticError, Warning},
    parser::{
        Ast, Attribute, Enum, EnumVariant, Expression, FuncArg, Function, Import, Statement, Trait,
        TypeDecl, TypeMember,
    },
    symbol_table::{Module, Symbol},
    GlobalSymbolTable, Interner,
};

cfg_if::cfg_if! {
    if #[cfg(feature = "no-std")] {
        use hashbrown::HashMap;
    } else {
        use std::collections::HashMap;
    }
}

pub struct SemanticAnalyzer {
    passes: Vec<Box<dyn SemanticPass>>,
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        Self { passes: Vec::new() }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            passes: Vec::with_capacity(capacity),
        }
    }

    pub fn pass<T: SemanticPass + 'static>(mut self, pass: T) -> Self {
        self.passes.push(Box::new(pass));
        self
    }

    pub fn passes<I>(mut self, passes: I) -> Self
    where
        I: Iterator<Item = Box<dyn SemanticPass>>,
    {
        self.passes.extend(passes);
        self
    }

    pub fn analyze<'stmt, 'expr>(
        &mut self,
        node: &Ast<'stmt, 'expr>,
        symbol_table: &GlobalSymbolTable,
        interner: &Interner,
        error_handler: &mut ErrorHandler,
    ) {
        for pass in self.passes.iter_mut() {
            match node {
                Ast::Function(func) => pass.analyze_function(
                    func.data(),
                    func.loc(),
                    symbol_table,
                    interner,
                    error_handler,
                ),

                Ast::Type(ty) => {
                    pass.analyze_type(ty.data(), ty.loc(), symbol_table, interner, error_handler)
                }

                Ast::Enum(en) => {
                    pass.analyze_enum(en.data(), en.loc(), symbol_table, interner, error_handler)
                }

                Ast::Trait(tr) => {
                    pass.analyze_trait(tr.data(), tr.loc(), symbol_table, interner, error_handler)
                }

                Ast::Import(import) => pass.analyze_import(
                    import.data(),
                    import.loc(),
                    symbol_table,
                    interner,
                    error_handler,
                ),
            }
        }
    }

    pub fn analyze_all<'stmt, 'expr>(
        &mut self,
        module: &Module<'stmt, 'expr>,
        symbol_table: &GlobalSymbolTable,
        interner: &Interner,
        error_handler: &mut ErrorHandler,
    ) {
        for node in module.symbols.values().map(|s| {
            if let Symbol::Unresolved(_, node) = s {
                node
            } else {
                panic!()
            }
        }) {
            self.analyze(node, symbol_table, interner, error_handler);
        }
    }
}

impl Default for SemanticAnalyzer {
    fn default() -> Self {
        Self::with_capacity(1).pass(Correctness::new())
    }
}

impl fmt::Debug for SemanticAnalyzer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("SemanticAnalyzer")
            .field(
                "passes",
                &self
                    .passes
                    .iter()
                    .map(|pass| pass.name())
                    .collect::<Vec<_>>(),
            )
            .finish()
    }
}

// TODO: Give access to other file's symbol tables
pub trait SemanticPass {
    fn name(&self) -> &'static str;

    fn analyze_function<'stmt, 'expr>(
        &mut self,
        _func: &Function<'stmt, 'expr>,
        _loc: Location,
        _local_symbol_table: &GlobalSymbolTable,
        _interner: &Interner,
        _error_handler: &mut ErrorHandler,
    ) {
    }

    fn analyze_type<'stmt, 'expr>(
        &mut self,
        _type: &TypeDecl<'stmt, 'expr>,
        _loc: Location,
        _local_symbol_table: &GlobalSymbolTable,
        _interner: &Interner,
        _error_handler: &mut ErrorHandler,
    ) {
    }

    fn analyze_enum<'stmt>(
        &mut self,
        _enum: &Enum<'stmt>,
        _loc: Location,
        _local_symbol_table: &GlobalSymbolTable,
        _interner: &Interner,
        _error_handler: &mut ErrorHandler,
    ) {
    }

    fn analyze_trait<'stmt, 'expr>(
        &mut self,
        _trait: &Trait<'stmt, 'expr>,
        _loc: Location,
        _local_symbol_table: &GlobalSymbolTable,
        _interner: &Interner,
        _error_handler: &mut ErrorHandler,
    ) {
    }

    fn analyze_import(
        &mut self,
        _import: &Import,
        _loc: Location,
        _local_symbol_table: &GlobalSymbolTable,
        _interner: &Interner,
        _error_handler: &mut ErrorHandler,
    ) {
    }

    fn analyze_stmt<'stmt, 'expr>(
        &mut self,
        _stmt: &Statement<'stmt, 'expr>,
        _local_symbol_table: &GlobalSymbolTable,
        _interner: &Interner,
        _error_handler: &mut ErrorHandler,
    ) {
    }

    fn analyze_expr<'expr>(
        &mut self,
        _expr: &Expression<'expr>,
        _local_symbol_table: &GlobalSymbolTable,
        _interner: &Interner,
        _error_handler: &mut ErrorHandler,
    ) {
    }
}

struct Correctness {}

impl Correctness {
    pub fn new() -> Self {
        Self {}
    }

    // TODO: Contradictory attributes
    /// Check for duplicate attributes
    fn duplicated_attrs(
        &mut self,
        error_handler: &mut ErrorHandler,
        attrs: &[Locatable<Attribute>],
    ) {
        #[derive(PartialEq, Eq, PartialOrd, Ord)]
        enum Attr {
            Vis = 0,
            Misc = 1,
        }

        let (mut seen, mut stage, mut vis) = (
            HashMap::with_capacity(attrs.len()),
            Attr::Vis,
            Vec::with_capacity(1),
        );
        for attr in attrs.iter() {
            let curr = match attr.data() {
                Attribute::Visibility(_) => {
                    vis.push(attr);
                    Attr::Vis
                }
                Attribute::Comptime => Attr::Misc,
            };

            // If the attributes are incorrectly ordered, emit an error
            if curr < stage {
                error_handler.push_err(Locatable::new(
                    Error::Semantic(SemanticError::UnorderedAttrs),
                    attr.loc(),
                ));
            } else {
                stage = curr;
            }

            // If the attribute has already been seen, emit an error
            if let Some(loc) = seen.insert(attr.data(), attr.loc()) {
                error_handler.push_err(Locatable::new(
                    Error::Semantic(SemanticError::DuplicatedAttributes {
                        attr: attr.data().as_str().to_owned(),
                        first: loc,
                        second: attr.loc(),
                    }),
                    attr.loc(),
                ));
            }
        }

        // Emit errors for conflicting attributes
        if vis.len() > 1 {
            let mut windows = vis.windows(2);
            while let Some([first, second]) = windows.next() {
                error_handler.push_err(Locatable::new(
                    Error::Semantic(SemanticError::ConflictingAttributes {
                        attr1: first.data().as_str().to_owned(),
                        attr2: second.data().as_str().to_owned(),
                        first: first.loc(),
                        second: second.loc(),
                    }),
                    second.loc(),
                ));
            }
        }
    }
}

impl SemanticPass for Correctness {
    fn name(&self) -> &'static str {
        "Correctness"
    }

    fn analyze_function<'stmt, 'expr>(
        &mut self,
        func: &Function<'stmt, 'expr>,
        func_loc: Location,
        local_symbol_table: &GlobalSymbolTable,
        interner: &Interner,
        error_handler: &mut ErrorHandler,
    ) {
        // Errors for empty function bodies
        if func.body.is_empty() {
            error_handler.push_err(Locatable::new(
                Error::Semantic(SemanticError::EmptyFuncBody),
                func_loc,
            ));
        }

        self.duplicated_attrs(error_handler, &func.attrs);

        // Check for duplicated function arguments
        let mut args = HashMap::with_capacity(func.args.len());
        for FuncArg { name, .. } in func.args.iter().map(|arg| arg.data()) {
            if let Some(loc) = args.insert(name.data(), name.loc()) {
                error_handler.push_err(Locatable::new(
                    Error::Semantic(SemanticError::Redefinition {
                        name: interner.resolve(name.data()).to_owned(),
                        first: loc,
                        second: name.loc(),
                    }),
                    name.loc(),
                ));
            }
        }

        for stmt in func.body.iter() {
            self.analyze_stmt(&*stmt, local_symbol_table, interner, error_handler);
        }
    }

    fn analyze_type<'stmt, 'expr>(
        &mut self,
        ty: &TypeDecl<'stmt, 'expr>,
        ty_loc: Location,
        local_symbol_table: &GlobalSymbolTable,
        interner: &Interner,
        error_handler: &mut ErrorHandler,
    ) {
        // Errors for empty type bodies
        if ty.members.is_empty() && ty.methods.is_empty() {
            error_handler.push_err(Locatable::new(
                Error::Semantic(SemanticError::EmptyTypeBody),
                ty_loc,
            ));
        }

        self.duplicated_attrs(error_handler, &ty.attrs);

        // Check for duplicated members, unused generics and unused attributes
        let mut members = HashMap::with_capacity(ty.members.len());
        let mut generics: Vec<_> = ty.generics.iter().map(|g| (g, false)).collect();
        for member in ty.members.iter() {
            let TypeMember {
                name,
                ty: member,
                attrs,
                ..
            } = member.data();

            // If there's already a member by the same name, emit an error
            if let Some(loc) = members.insert(*name, member.loc()) {
                error_handler.push_err(Locatable::new(
                    Error::Semantic(SemanticError::Redefinition {
                        name: interner.resolve(name).to_owned(),
                        first: loc,
                        second: member.loc(),
                    }),
                    member.loc(),
                ));
            }

            // Mark the generic as used
            if let Some((_, used)) = generics.iter_mut().find(|(g, _)| g.data() == member.data()) {
                *used = true;
            }

            // Also check for unused attributes while we're here
            self.duplicated_attrs(error_handler, attrs);
        }

        // Do the actual check that all generics are used
        for generic in generics
            .into_iter()
            .filter_map(|(g, used)| if used { Some(g) } else { None })
        {
            error_handler.push_warning(Locatable::new(
                Warning::UnusedGeneric(generic.data().to_string(&interner)),
                generic.loc(),
            ));
        }

        // Check for duplicated methods, member/method overlap and analyze methods
        let mut methods = HashMap::with_capacity(ty.methods.len());
        for method in ty.methods.iter() {
            let Function { name, .. } = method.data();

            // If there's already a method by the same name, emit an error
            if let Some(loc) = methods.insert(*name, method.loc()) {
                error_handler.push_err(Locatable::new(
                    Error::Semantic(SemanticError::Redefinition {
                        name: interner.resolve(name).to_owned(),
                        first: loc,
                        second: method.loc(),
                    }),
                    method.loc(),
                ));
            }

            // If there's a member name that overlaps, emit an error
            if let Some(member) = members.get(name) {
                error_handler.push_err(Locatable::new(
                    Error::Semantic(SemanticError::Redefinition {
                        name: interner.resolve(name).to_owned(),
                        first: *member,
                        second: method.loc(),
                    }),
                    method.loc(),
                ));
            }

            // Analyze the function while we're here
            self.analyze_function(
                method.data(),
                method.loc(),
                local_symbol_table,
                interner,
                error_handler,
            );
        }
    }

    fn analyze_enum<'stmt>(
        &mut self,
        en: &Enum<'stmt>,
        _loc: Location,
        _local_symbol_table: &GlobalSymbolTable,
        interner: &Interner,
        error_handler: &mut ErrorHandler,
    ) {
        self.duplicated_attrs(error_handler, &en.attrs);

        // Check for duplicated variants and unused generics
        let mut variants = HashMap::with_capacity(en.variants.len());
        let mut generics: Vec<_> = en.generics.iter().map(|g| (g, false)).collect();
        for variant in en.variants.iter() {
            let (name, elements) = match variant.data() {
                EnumVariant::Unit { name, .. } => (*name, None),
                EnumVariant::Tuple { name, elements, .. } => (*name, Some(elements)),
            };

            // If there's already a variant by the same name, emit an error
            if let Some(loc) = variants.insert(name, variant.loc()) {
                error_handler.push_err(Locatable::new(
                    Error::Semantic(SemanticError::Redefinition {
                        name: interner.resolve(&name).to_owned(),
                        first: loc,
                        second: variant.loc(),
                    }),
                    variant.loc(),
                ));
            }

            // Mark all used generics as used
            if let Some(elements) = elements {
                for elm in elements {
                    if let Some((_, used)) =
                        generics.iter_mut().find(|(g, _)| g.data() == elm.data())
                    {
                        *used = true;
                    }
                }
            }
        }

        // Do the actual check that all generics are used
        for generic in generics
            .into_iter()
            .filter_map(|(g, used)| if used { Some(g) } else { None })
        {
            error_handler.push_warning(Locatable::new(
                Warning::UnusedGeneric(generic.data().to_string(&interner)),
                generic.loc(),
            ));
        }
    }

    // TODO: How do generics work here?
    fn analyze_trait<'stmt, 'expr>(
        &mut self,
        tr: &Trait<'stmt, 'expr>,
        _loc: Location,
        local_symbol_table: &GlobalSymbolTable,
        interner: &Interner,
        error_handler: &mut ErrorHandler,
    ) {
        self.duplicated_attrs(error_handler, &tr.attrs);

        // Analyze all the methods
        for method in tr.methods.iter() {
            self.analyze_function(
                method.data(),
                method.loc(),
                local_symbol_table,
                interner,
                error_handler,
            );
        }
    }

    fn analyze_import(
        &mut self,
        _import: &Import,
        _loc: Location,
        _local_symbol_table: &GlobalSymbolTable,
        _interner: &Interner,
        _error_handler: &mut ErrorHandler,
    ) {
        // ???
    }

    fn analyze_stmt<'stmt, 'expr>(
        &mut self,
        stmt: &Statement<'stmt, 'expr>,
        local_symbol_table: &GlobalSymbolTable,
        interner: &Interner,
        error_handler: &mut ErrorHandler,
    ) {
        match stmt {
            Statement::Expression(expr) => {
                self.analyze_expr(&*expr, local_symbol_table, interner, error_handler);
            }

            _ => {}
        }
    }

    fn analyze_expr<'expr>(
        &mut self,
        expr: &Expression<'expr>,
        _local_symbol_table: &GlobalSymbolTable,
        _interner: &Interner,
        _error_handler: &mut ErrorHandler,
    ) {
        match expr {
            _ => {}
        }
    }
}
