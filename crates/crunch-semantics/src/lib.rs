#![cfg_attr(feature = "no-std", no_std)]
#![warn(
    missing_copy_implementations,
    missing_debug_implementations,
    clippy::dbg_macro,
    clippy::missing_safety_doc,
    clippy::wildcard_imports,
    clippy::shadow_unrelated
)]

extern crate alloc;

use alloc::{borrow::ToOwned, boxed::Box, format, vec::Vec};
use core::fmt;
use crunch_shared::{
    context::Context,
    end_timer,
    error::{Error, ErrorHandler, Locatable, Location, SemanticError, Warning},
    start_timer,
    strings::{StrInterner, StrT},
    trees::ast::{
        AssignKind, BinaryOp, Block, CompOp, Dest, Exposure, Expr, For, FuncArg, If, Item,
        ItemPath, Literal, Loop, Match, Stmt, Type, TypeMember, UnaryOp, VarDecl, Variant, While,
    },
    utils::HashMap,
    visitors::ast::{ExprVisitor, ItemVisitor, StmtVisitor},
};

// FIXME: Actual errors here
pub trait Analyzer: ItemVisitor {
    fn name(&self) -> &str;
    fn load(&mut self, error_handler: ErrorHandler, context: Context);
    fn unload(&mut self) -> ErrorHandler;
}

pub struct SemanticAnalyzer {
    passes: Vec<Box<dyn Analyzer<Output = ()>>>,
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

    pub fn pass<T: Analyzer<Output = ()> + 'static>(mut self, pass: T) -> Self {
        self.passes.push(Box::new(pass));
        self
    }

    pub fn passes<I>(mut self, passes: I) -> Self
    where
        I: Iterator<Item = Box<dyn Analyzer<Output = ()>>>,
    {
        self.passes.extend(passes);
        self
    }

    pub fn analyze(&mut self, items: &[Item], context: &Context, errors: &mut ErrorHandler) {
        let timer = start_timer!("semantic analysis");

        for pass in self.passes.iter_mut() {
            let pass_name = format!("semantic analysis pass {}", pass.name());
            let pass_timer = start_timer!(pass_name);

            pass.load(ErrorHandler::new(), context.clone());

            for item in items {
                pass.visit_item(item);
            }

            let err = pass.unload();
            errors.extend(err);
            end_timer!(pass_name, pass_timer);
        }

        end_timer!("semantic analysis", timer);
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

#[derive(Debug, Default)]
pub struct Correctness {
    errors: Option<ErrorHandler>,
    context: Option<Context>,
}

impl<'err> Correctness {
    pub fn new() -> Self {
        Self {
            errors: None,
            context: None,
        }
    }

    fn errors(&mut self) -> &mut ErrorHandler {
        self.errors.as_mut().unwrap()
    }

    fn strings(&self) -> &StrInterner {
        &self.context.as_ref().unwrap().strings
    }
}

impl Analyzer for Correctness {
    fn name(&self) -> &str {
        "Correctness"
    }

    fn load(&mut self, errors: ErrorHandler, context: Context) {
        self.errors = Some(errors);
        self.context = Some(context);
    }

    fn unload(&mut self) -> ErrorHandler {
        self.errors.as_mut().unwrap().take()
    }
}

// TODO: So much information skipped here
impl ItemVisitor for Correctness {
    type Output = ();

    fn visit_func(
        &mut self,
        item: &Item,
        _generics: &[Type],
        args: &[FuncArg],
        body: &Block,
        _ret: Locatable<&Type>,
        _loc: Location,
    ) {
        // Errors for empty function bodies
        if body.is_empty() {
            self.errors().push_err(Locatable::new(
                Error::Semantic(SemanticError::EmptyFuncBody),
                item.loc,
            ));
        }

        // Check for duplicated function arguments
        let mut arg_map = HashMap::with_capacity(args.len());
        for FuncArg { name, .. } in args.iter() {
            if let Some(..) = arg_map.insert(name, name) {
                let name = self.strings().resolve(*name).as_ref().to_owned();
                // FIXME: More locations
                self.errors().push_err(Locatable::new(
                    Error::Semantic(SemanticError::Redefinition {
                        name,
                        first: item.loc,
                        second: item.loc,
                    }),
                    item.loc,
                ));
            }
        }

        for stmt in body.iter() {
            self.visit_stmt(stmt);
        }
    }

    fn visit_type(&mut self, item: &Item, generics: &[Type], members: &[TypeMember]) {
        // Errors for empty type bodies
        if members.is_empty() {
            self.errors().push_err(Locatable::new(
                Error::Semantic(SemanticError::EmptyTypeBody),
                item.loc,
            ));
        }

        // Check for duplicated members, unused generics and unused attributes
        let mut member_map = HashMap::with_capacity(members.len());
        let mut generics: Vec<_> = generics.iter().map(|g| (g, false)).collect();
        for TypeMember {
            name, ty: member, ..
        } in members.iter()
        {
            // If there's already a member by the same name, emit an error
            if let Some(loc) = member_map.insert(name, item.loc) {
                let name = self.strings().resolve(*name).as_ref().to_owned();
                // FIXME: Location information
                self.errors().push_err(Locatable::new(
                    Error::Semantic(SemanticError::Redefinition {
                        name,
                        first: loc,
                        second: item.loc,
                    }),
                    item.loc,
                ));
            }

            // Mark the generic as used
            if let Some((_, used)) = generics.iter_mut().find(|(g, _)| *g == &**member) {
                *used = true;
            }
        }

        // Do the actual check that all generics are used
        for generic in generics
            .into_iter()
            .filter_map(|(g, used)| if used { Some(g) } else { None })
        {
            let msg = generic.to_string(&self.strings());
            // FIXME: Err location
            self.errors()
                .push_warning(Locatable::new(Warning::UnusedGeneric(msg), item.loc));
        }

        // TODO: Re-add this once methods are resolved
        // // Check for duplicated methods, member/method overlap and analyze methods
        // let mut methods = HashMap::with_capacity(ty.methods.len());
        // for method in ty.methods.iter() {
        //     let Function { name, .. } = &**method;
        //
        //     // If there's already a method by the same name, emit an error
        //     if let Some(loc) = methods.insert(*name, method.loc()) {
        //         error_handler.push_err(Locatable::new(
        //             Error::Semantic(SemanticError::Redefinition {
        //                 name: interner.resolve(name).to_owned(),
        //                 first: loc,
        //                 second: method.loc(),
        //             }),
        //             method.loc(),
        //         ));
        //     }
        //
        //     // If there's a member name that overlaps, emit an error
        //     if let Some(member) = members.get(name) {
        //         error_handler.push_err(Locatable::new(
        //             Error::Semantic(SemanticError::Redefinition {
        //                 name: interner.resolve(name).to_owned(),
        //                 first: *member,
        //                 second: method.loc(),
        //             }),
        //             method.loc(),
        //         ));
        //     }
        //
        //     // Analyze the function while we're here
        //     self.analyze_function(
        //         &*method,
        //         method.loc(),
        //         local_symbol_table,
        //         ctx,
        //         error_handler,
        //     );
        // }
    }

    fn visit_enum(&mut self, item: &Item, generics: &[Type], variants: &[Variant]) {
        // Check for duplicated variants and unused generics
        let mut variant_map = HashMap::with_capacity(variants.len());
        let mut generics: Vec<_> = generics.iter().map(|g| (g, false)).collect();
        for variant in variants.iter() {
            let (name, elements) = match variant {
                Variant::Unit { name, .. } => (*name, None),
                Variant::Tuple { name, elms, .. } => (*name, Some(elms)),
            };

            // If there's already a variant by the same name, emit an error
            // FIXME: Error locations
            if let Some(loc) = variant_map.insert(name, item.loc) {
                let name = self.strings().resolve(name).as_ref().to_owned();
                self.errors().push_err(Locatable::new(
                    Error::Semantic(SemanticError::Redefinition {
                        name,
                        first: loc,
                        second: item.loc,
                    }),
                    item.loc,
                ));
            }

            // Mark all used generics as used
            if let Some(elements) = elements {
                for elm in elements {
                    if let Some((_, used)) = generics.iter_mut().find(|(g, _)| *g == elm) {
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
            let msg = generic.to_string(self.strings());
            // FIXME: Error location
            self.errors()
                .push_warning(Locatable::new(Warning::UnusedGeneric(msg), item.loc));
        }
    }

    fn visit_trait(&mut self, _item: &Item, _generics: &[Type], methods: &[Item]) {
        // Analyze all the methods
        for method in methods {
            self.visit_item(method);
        }
    }

    fn visit_import(&mut self, _item: &Item, _file: &ItemPath, _dest: &Dest, _exposes: &Exposure) {}

    fn visit_extend_block(
        &mut self,
        _item: &Item,
        _target: &Type,
        _extender: Option<&Type>,
        _items: &[Item],
    ) {
    }

    fn visit_alias(&mut self, _item: &Item, _alias: &Type, _actual: &Type) {}
}

impl StmtVisitor for Correctness {
    type Output = <Self as ItemVisitor>::Output;

    fn visit_var_decl(&mut self, _stmt: &Stmt, _var: &VarDecl) {}
}

impl ExprVisitor for Correctness {
    type Output = <Self as ItemVisitor>::Output;

    fn visit_if(&mut self, _expr: &Expr, _if_: &If) {}
    fn visit_return(&mut self, _expr: &Expr, _value: Option<&Expr>) {}
    fn visit_break(&mut self, _expr: &Expr, _value: Option<&Expr>) {}
    fn visit_continue(&mut self, _expr: &Expr) {}
    fn visit_while(&mut self, _expr: &Expr, _while_: &While) {}
    fn visit_loop(&mut self, _expr: &Expr, _loop_: &Loop) {}
    fn visit_for(&mut self, _expr: &Expr, _for_: &For) {}
    fn visit_match(&mut self, _expr: &Expr, _match_: &Match) {}
    fn visit_variable(&mut self, _expr: &Expr, _var: StrT) {}
    fn visit_literal(&mut self, _expr: &Expr, _literal: &Literal) {}
    fn visit_unary(&mut self, _expr: &Expr, _op: UnaryOp, _inner: &Expr) {}
    fn visit_binary_op(&mut self, _expr: &Expr, _lhs: &Expr, _op: BinaryOp, _rhs: &Expr) {}
    fn visit_comparison(&mut self, _expr: &Expr, _lhs: &Expr, _op: CompOp, _rhs: &Expr) {}
    fn visit_assign(&mut self, _expr: &Expr, _lhs: &Expr, _op: AssignKind, _rhs: &Expr) {}
    fn visit_paren(&mut self, _expr: &Expr, _inner: &Expr) {}
    fn visit_array(&mut self, _expr: &Expr, _elements: &[Expr]) {}
    fn visit_tuple(&mut self, _expr: &Expr, _elements: &[Expr]) {}
    fn visit_range(&mut self, _expr: &Expr, _start: &Expr, _end: &Expr) {}
    fn visit_index(&mut self, _expr: &Expr, _var: &Expr, _index: &Expr) {}
    fn visit_func_call(&mut self, _expr: &Expr, _caller: &Expr, _args: &[Expr]) {}
    fn visit_member_func_call(&mut self, _expr: &Expr, _member: &Expr, _func: &Expr) {}
}
