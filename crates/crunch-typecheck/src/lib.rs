#![warn(
    missing_copy_implementations,
    missing_debug_implementations,
    clippy::dbg_macro,
    clippy::missing_safety_doc,
    clippy::wildcard_imports,
    clippy::shadow_unrelated
)]

extern crate alloc;

mod ddlog;

use alloc::sync::Arc;
use core::fmt::{self, Result as FmtResult, Write};
use crunch_shared::{
    context::ContextDatabase,
    error::{ErrorHandler, Locatable, Location, Span, TypeError, TypeResult},
    files::{FileCache, FileId},
    salsa, tracing,
    trees::{
        hir::{
            BinaryOp, Block, Break, Cast, CompOp, Expr, ExternFunc, FuncArg, FuncCall, Function,
            Item, Literal, LiteralVal, Match, Pattern, Reference, Return, Stmt, Type, TypeId,
            TypeKind, Var, VarDecl,
        },
        ItemPath,
    },
    utils::{HashMap, Hasher},
    visitors::hir::{ExprVisitor, ItemVisitor, StmtVisitor},
};
use ladder::HirDatabase;

type ArcError = Arc<ErrorHandler>;

#[salsa::query_group(TypecheckDatabaseStorage)]
pub trait TypecheckDatabase: salsa::Database + ContextDatabase + HirDatabase {
    fn typecheck(&self, file: FileId) -> Result<(), ArcError>;
}

#[crunch_shared::instrument(name = "type checking", skip(db))]
fn typecheck(db: &dyn TypecheckDatabase, file: FileId) -> Result<(), ArcError> {
    let hir = db.lower_hir(file)?;

    crunch_shared::allocator::CRUNCHC_ALLOCATOR
        .record_region("typechecking", || Engine::new(db).walk(&*hir))
        .map(|mut ok| {
            ok.emit(
                &FileCache::upcast(db),
                &**db.writer(),
                &**db.stdout_config(),
            )
        })
        .map_err(Arc::new)
}

#[derive(Debug, Clone)]
struct Func {
    ret: TypeId,
    args: Vec<TypeId>,
    arg_span: Location,
    sig: Location,
}

// TODO: Find a better arch than this
#[derive(Clone)]
pub struct Engine<'ctx> {
    errors: ErrorHandler,
    current_func: Option<Func>,
    functions: HashMap<ItemPath, Func>,
    variables: Vec<HashMap<Var, TypeId>>,
    check: Option<TypeId>,
    db: &'ctx dyn TypecheckDatabase,
}

impl<'ctx> Engine<'ctx> {
    pub fn new(db: &'ctx dyn TypecheckDatabase) -> Self {
        crunch_shared::trace!("creating a new type engine");

        Self {
            errors: ErrorHandler::default(),
            current_func: None,
            functions: HashMap::with_hasher(Hasher::default()),
            variables: Vec::new(),
            check: None,
            db,
        }
    }

    // TODO: Caching
    fn var_type(&self, var: &Var, loc: Location) -> TypeResult<TypeId> {
        crunch_shared::trace!("getting the type of the variable {:?}", var);

        self.variables
            .iter()
            .rev()
            .find_map(|vars| vars.get(var))
            .copied()
            .ok_or_else(|| {
                Locatable::new(
                    TypeError::VarNotInScope(var.to_string(self.db.context().strings())).into(),
                    loc,
                )
            })
    }

    fn insert_variable(&mut self, var: Var, type_id: TypeId) {
        crunch_shared::trace!("inserting a variable {:?} with the type {:?}", var, type_id);

        if let Some(old_type) = self.variables.last_mut().unwrap().insert(var, type_id) {
            crunch_shared::warn!(
                "The variable {:?} previously had the type {:?} but it was overwritten with {:?}",
                var,
                type_id,
                old_type,
            );
        }
    }

    fn push_scope(&mut self) {
        crunch_shared::trace!("pushing a variable scope");

        self.variables.push(HashMap::with_hasher(Hasher::default()));
    }

    fn pop_scope(&mut self) {
        crunch_shared::trace!("popping a variable scope");

        self.variables.pop().unwrap();
    }

    fn with_scope<F, T>(&mut self, func: F) -> T
    where
        F: FnOnce(&mut Self) -> T,
    {
        crunch_shared::trace_span!("variable_scope").in_scope(|| {
            crunch_shared::trace!("preforming a function with a new variable scope");

            self.push_scope();
            let result = func(self);
            self.pop_scope();

            result
        })
    }

    /// Make the types of two type terms equivalent (or produce an error if
    /// there is a conflict between them)
    ///
    /// The log spam can be silenced by using `type_unification=info`
    // TODO: Caching
    #[crunch_shared::instrument(
        target = "type_unification",
        level = "trace",
        name = "type unification",
        skip(self)
    )]
    fn unify(&mut self, left: TypeId, right: TypeId) -> TypeResult<()> {
        crunch_shared::trace!(
            target: "type_unification",
            "unifying types {:?} and {:?}",
            left,
            right,
        );

        let (mut left_ty, mut right_ty) = (
            self.db.context().get_hir_type(left).unwrap(),
            self.db.context().get_hir_type(right).unwrap(),
        );

        // Drill through type references to the underlying types
        // FIXME: Cycle detection
        loop {
            if left == right {
                crunch_shared::trace!(
                    target: "type_unification",
                    "the unified types are equal, returning immediately",
                );

                return Ok(());
            }

            match (left_ty.kind, right_ty.kind) {
                (TypeKind::Variable(l), TypeKind::Variable(r)) => {
                    left_ty = self.db.context().get_hir_type(l).unwrap();
                    right_ty = self.db.context().get_hir_type(r).unwrap();
                }

                // FIXME: Add back once cycles are detected
                // (TypeKind::Variable(l), _) => {
                //     left_ty = self.db.context().get_hir_type(l).unwrap();
                // }
                // (_, TypeKind::Variable(r)) => {
                //     right_ty = self.db.context().get_hir_type(r).unwrap();
                // }
                (_, _) => break,
            }
        }

        match (left_ty.kind, right_ty.kind) {
            // FIXME: Remove this once cycle detection works
            (TypeKind::Variable(left), _) => self.unify(left, right),
            (_, TypeKind::Variable(right)) => self.unify(left, right),

            (TypeKind::Unknown, _) => {
                let ty = self
                    .db
                    .context()
                    .hir_type(Type::new(TypeKind::Variable(right), left_ty.location()));
                self.db.context().overwrite_hir_type(left, ty);

                Ok(())
            }
            (_, TypeKind::Unknown) => {
                let ty = self
                    .db
                    .context()
                    .hir_type(Type::new(TypeKind::Variable(left), right_ty.location()));
                self.db.context().overwrite_hir_type(right, ty);

                Ok(())
            }

            (TypeKind::Absurd, _) | (_, TypeKind::Absurd) => {
                crunch_shared::trace!(
                    target: "type_unification",
                    "one of the sides is absurd, unifying",
                );
                Ok(())
            }
            (TypeKind::String, TypeKind::String)
            | (TypeKind::Bool, TypeKind::Bool)
            | (TypeKind::Unit, TypeKind::Unit) => {
                crunch_shared::trace!(
                    target: "type_unification",
                    "identical primitives, unifying",
                );
                Ok(())
            }

            (
                TypeKind::Integer {
                    signed: signed_a,
                    width: width_a,
                },
                TypeKind::Integer {
                    signed: signed_b,
                    width: width_b,
                },
            ) => {
                crunch_shared::trace!(
                    target: "type_unification",
                    "unifying {:?} and {:?}",
                    left_ty.kind,
                    right_ty.kind,
                );

                match (signed_a, signed_b) {
                    (Some(signed_a), Some(signed_b)) if signed_a != signed_b => {
                        crunch_shared::error!(
                            target: "type_unification",
                            "the signedness of {:?} and {:?} are not equal",
                            left_ty.kind,
                            right_ty.kind,
                        );

                        return Err(Locatable::new(
                            TypeError::TypeConflict {
                                call_type: self.display_type(&left_ty.kind),
                                def_type: self.display_type(&right_ty.kind),
                                def_site: right_ty.location(),
                            }
                            .into(),
                            left_ty.location(),
                        ));
                    }
                    _ => {}
                }

                match (width_a, width_b) {
                    (Some(width_a), Some(width_b)) if width_a != width_b => {
                        crunch_shared::error!(
                            target: "type_unification",
                            "the bit width of {:?} and {:?} are not equal",
                            left_ty.kind,
                            right_ty.kind,
                        );

                        return Err(Locatable::new(
                            TypeError::TypeConflict {
                                call_type: self.display_type(&left_ty.kind),
                                def_type: self.display_type(&right_ty.kind),
                                def_site: right_ty.location(),
                            }
                            .into(),
                            left_ty.location(),
                        ));
                    }
                    _ => {}
                }

                let ty = self
                    .db
                    .context()
                    .hir_type(Type::new(TypeKind::Variable(right), right_ty.location()));
                self.db.context().overwrite_hir_type(left, ty);

                Ok(())
            }

            (
                TypeKind::Array {
                    element: left_elem,
                    length: left_len,
                },
                TypeKind::Array {
                    element: right_elem,
                    length: right_len,
                },
            ) if left_len == right_len => {
                crunch_shared::error!(
                    target: "type_unification",
                    "array lengths are equal, unifying element types",
                );
                self.unify(left_elem, right_elem)?;

                Ok(())
            }

            (
                TypeKind::Reference {
                    referee: left,
                    mutable: left_mut,
                },
                TypeKind::Reference {
                    referee: right,
                    mutable: right_mut,
                },
            )
            | (
                TypeKind::Pointer {
                    pointee: left,
                    mutable: left_mut,
                },
                TypeKind::Pointer {
                    pointee: right,
                    mutable: right_mut,
                },
            ) if left_mut == right_mut => {
                crunch_shared::error!(
                    target: "type_unification",
                    "reference/pointer mutability are equal, unifying pointee types",
                );
                self.unify(left, right)?;

                Ok(())
            }

            // If no previous attempts to unify were successful, raise an error
            (call_type, def_type) => {
                crunch_shared::error!(
                    target: "type_unification",
                    "{:?} and {:?} are not unifiable, returning an error",
                    call_type,
                    def_type,
                );

                Err(Locatable::new(
                    TypeError::TypeConflict {
                        call_type: self.display_type(&call_type),
                        def_type: self.display_type(&def_type),
                        def_site: right_ty.location(),
                    }
                    .into(),
                    left_ty.location(),
                ))
            }
        }
    }

    pub fn walk(&mut self, items: &[&'ctx Item<'ctx>]) -> Result<ErrorHandler, ErrorHandler> {
        crunch_shared::trace!("walking a tree for type checking");

        self.with_scope(|builder| {
            for item in items.iter() {
                match item {
                    &&Item::Function(Function {
                        ref name,
                        ref args,
                        ret,
                        sig,
                        ..
                    })
                    | &&Item::ExternFunc(ExternFunc {
                        ref name,
                        ref args,
                        ret,
                        loc: sig,
                        ..
                    }) => {
                        // TODO: Use error types as fillers here if they're unknown
                        for arg in args.iter() {
                            let is_unknown = builder
                                .db
                                .context()
                                .get_hir_type(arg.kind)
                                .unwrap()
                                .is_unknown();

                            if is_unknown {
                                crunch_shared::error!(
                                    "the function {:?} is missing a function argument type",
                                    name.to_string(builder.db.context().strings()),
                                );

                                builder.errors.push_err(Locatable::new(
                                    TypeError::MissingType(
                                        "Types for function arguments".to_owned(),
                                    )
                                    .into(),
                                    arg.location(),
                                ));
                            }
                        }

                        // TODO: Use error types as fillers here if they're unknown
                        let ret_ty = builder.db.context().get_hir_type(ret).unwrap();
                        if ret_ty.kind.is_unknown() {
                            crunch_shared::error!(
                                "the function {:?} is missing a return type",
                                name.to_string(builder.db.context().strings()),
                            );

                            builder.errors.push_err(Locatable::new(
                                TypeError::MissingType("Return types for functions".to_owned())
                                    .into(),
                                ret_ty.location(),
                            ));
                        }

                        // TODO: Use error types as fillers here if they're unknown
                        let arg_span = args.location();
                        let args: Vec<TypeId> = args
                            .iter()
                            .map(|&FuncArg { name, kind, .. }| {
                                builder.insert_variable(name, kind);
                                kind
                            })
                            .collect();

                        let func = Func {
                            ret,
                            args,
                            arg_span,
                            sig,
                        };

                        crunch_shared::trace!(
                            "inserting a function into the builder: {:?}",
                            name.to_string(builder.db.context().strings()),
                        );

                        builder.functions.insert(name.clone(), func);
                    }
                }
            }

            for item in items {
                if let Err(err) = builder.visit_item(item) {
                    crunch_shared::error!("item encountered an error while type checking");

                    builder.errors.push_err(err);
                }
            }

            if builder.errors.is_fatal() {
                crunch_shared::error!(
                    "fatal errors encountered when type checking, returning an error",
                );

                Err(builder.errors.take())
            } else {
                crunch_shared::info!("type checking succeeded");

                Ok(builder.errors.take())
            }
        })
    }

    // TODO: Caching
    #[crunch_shared::instrument(name = "intern literal", skip(self, val, ty, loc, _loc))]
    fn intern_literal(
        &mut self,
        &Literal { ref val, ty, loc }: &Literal,
        _loc: Location,
    ) -> TypeResult<TypeId> {
        match val {
            LiteralVal::Array { elements } => {
                let element = self.db.hir_type(Type::new(TypeKind::Unknown, loc));

                self.db.hir_type(Type::new(
                    TypeKind::Array {
                        element,
                        length: elements.len() as u64,
                    },
                    loc,
                ));

                crunch_shared::trace_span!("check_array_elements").in_scope(|| {
                    crunch_shared::trace!("checking array element types");
                    for elem in elements {
                        let elem = self.intern_literal(elem, elem.location())?;
                        self.unify(element, elem)?;
                    }

                    Ok(())
                })?;
            }

            ignored => crunch_shared::debug!("Ignoring {:?} in intern_literal", ignored),
        }

        if let Some(check) = self.check {
            self.unify(ty, check)?;
        }

        // TODO: Check inner types of stuff
        Ok(ty)
    }

    // TODO: Caching
    fn display_type(&self, ty: &TypeKind) -> String {
        let mut string = String::new();
        self.display_type_inner(ty, &mut string)
            .expect("Failed to format type");

        string
    }

    fn display_type_inner<W: Write>(&self, ty: &TypeKind, f: &mut W) -> FmtResult {
        match ty {
            &TypeKind::Variable(inner) => {
                self.display_type_inner(&self.db.context().get_hir_type(inner).unwrap().kind, f)
            }
            TypeKind::Unknown => f.write_str("infer"),
            &TypeKind::Integer { signed, width } => match (signed, width) {
                (Some(signed), Some(width)) => {
                    write!(f, "{}{}", if signed { "i" } else { "u" }, width)
                }
                (_, _) => f.write_str("{{integer}}"),
            },
            TypeKind::String => f.write_str("str"),
            TypeKind::Bool => f.write_str("bool"),
            TypeKind::Unit => f.write_str("unit"),
            TypeKind::Absurd => f.write_str("absurd"),

            &TypeKind::Array { element, length } => {
                f.write_str("arr[")?;
                self.display_type_inner(&self.db.context().get_hir_type(element).unwrap().kind, f)?;
                f.write_str("; ")?;
                write!(f, "{}", length)?;
                f.write_char(']')
            }

            &TypeKind::Slice { element } => {
                f.write_str("arr[")?;
                self.display_type_inner(&self.db.context().get_hir_type(element).unwrap().kind, f)?;
                f.write_char(']')
            }

            &TypeKind::Reference { mutable, referee } => {
                f.write_char('&')?;
                if mutable {
                    f.write_str("mut ")?;
                }

                self.display_type_inner(&self.db.context().get_hir_type(referee).unwrap().kind, f)
            }

            &TypeKind::Pointer { mutable, pointee } => {
                if mutable {
                    f.write_str("*const ")?;
                } else {
                    f.write_str("*mut ")?;
                }

                self.display_type_inner(&self.db.context().get_hir_type(pointee).unwrap().kind, f)
            }
        }
    }
}

impl<'ctx> ItemVisitor<'ctx> for Engine<'ctx> {
    type Output = TypeResult<()>;

    #[crunch_shared::instrument(name = "item", skip(self, item))]
    fn visit_item(&mut self, item: &Item<'ctx>) -> Self::Output {
        match item {
            Item::Function(func) => {
                crunch_shared::trace!("item is a function, visiting");
                self.visit_func(func)
            }
            Item::ExternFunc(func) => {
                crunch_shared::trace!("item is an external function, visiting");
                self.visit_extern_func(func)
            }
        }
    }

    #[crunch_shared::instrument(
        name = "function",
        skip(self, name, body, args),
        fields(name = ?name.to_string(self.db.context().strings())),
    )]
    fn visit_func(
        &mut self,
        Function {
            name, body, args, ..
        }: &Function<'ctx>,
    ) -> Self::Output {
        self.with_scope(|builder| {
            builder.current_func = Some(builder.functions.get(name).unwrap().clone());

            for arg in args.iter() {
                builder.insert_variable(arg.name, arg.kind);
            }

            for stmt in body.iter() {
                builder.visit_stmt(stmt)?;
            }

            builder.current_func = None;

            Ok(())
        })
    }

    #[crunch_shared::instrument(
        name = "external function",
        skip(self, name, ret, args),
        fields(name = ?name.to_string(self.db.context().strings())),
    )]
    fn visit_extern_func(
        &mut self,
        &ExternFunc {
            ref args,
            ret,
            ref name,
            ..
        }: &ExternFunc,
    ) -> Self::Output {
        let mut missing_arg_ty = None;

        for arg in args.iter() {
            let is_unknown = self
                .db
                .context()
                .get_hir_type(arg.kind)
                .unwrap()
                .is_unknown();

            if is_unknown {
                if let Some(err) = missing_arg_ty.take() {
                    self.errors.push_err(err);
                }

                missing_arg_ty = Some(Locatable::new(
                    TypeError::MissingType("Types for external function arguments".to_owned())
                        .into(),
                    arg.loc,
                ));
            }
        }

        let ret = self.db.context().get_hir_type(ret).unwrap();
        if ret.is_unknown() {
            if let Some(err) = missing_arg_ty {
                self.errors.push_err(err);
            }

            return Err(Locatable::new(
                TypeError::MissingType("Return types for external functions".to_owned()).into(),
                ret.location(),
            ));
        } else if let Some(err) = missing_arg_ty {
            return Err(err);
        }

        Ok(())
    }
}

impl<'ctx> StmtVisitor<'ctx> for Engine<'ctx> {
    type Output = TypeResult<Option<TypeId>>;
    #[crunch_shared::instrument(name = "statement", skip(self, stmt))]
    fn visit_stmt(&mut self, stmt: &'ctx Stmt<'ctx>) -> <Self as StmtVisitor<'ctx>>::Output {
        match stmt {
            Stmt::VarDecl(decl) => self.visit_var_decl(decl),

            Stmt::Item(item) => {
                self.visit_item(item)?;

                // FIXME: This is bad, very bad
                Ok(None)
            }

            Stmt::Expr(expr) => self.visit_expr(expr).map(Some),
        }
    }
    #[crunch_shared::instrument(
        name = "variable declaration",
        skip(self, name, value, ty, loc),
        fields(name = ?name.to_string(self.db.context().strings()), ty = ?ty),
    )]
    fn visit_var_decl(
        &mut self,
        &VarDecl {
            name,
            value,
            ty,
            loc,
            ..
        }: &VarDecl<'ctx>,
    ) -> <Self as StmtVisitor<'ctx>>::Output {
        let expr = self.visit_expr(value)?;
        self.insert_variable(name, ty);
        self.unify(expr, ty)?;

        Ok(Some(self.db.hir_type(Type::new(TypeKind::Unit, loc))))
    }
}

impl<'ctx> ExprVisitor<'ctx> for Engine<'ctx> {
    type Output = TypeResult<TypeId>;

    #[crunch_shared::instrument(name = "return", skip(self, loc, ret))]
    fn visit_return(&mut self, loc: Location, ret: &Return<'ctx>) -> Self::Output {
        let func_ret = self.current_func.as_ref().unwrap().ret;
        self.check = Some(func_ret);

        if let Some(ret) = ret.val {
            let ret = self.visit_expr(ret)?;
            self.unify(ret, func_ret)?;
        } else {
            let unit = self.db.hir_type(Type::new(TypeKind::Unit, loc));
            self.unify(unit, func_ret)?;
        }
        self.check.take();

        Ok(self.db.hir_type(Type::new(TypeKind::Absurd, loc)))
    }

    #[crunch_shared::instrument(name = "break", skip(self, _loc, _value))]
    fn visit_break(&mut self, _loc: Location, _value: &Break<'ctx>) -> Self::Output {
        todo!()
    }

    #[crunch_shared::instrument(name = "continue", skip(self, _loc))]
    fn visit_continue(&mut self, _loc: Location) -> Self::Output {
        todo!()
    }

    #[crunch_shared::instrument(name = "loop", skip(self, loc, body))]
    fn visit_loop(&mut self, loc: Location, body: &Block<&'ctx Stmt<'ctx>>) -> Self::Output {
        crunch_shared::trace!(
            "visiting an unconditional loop with {} body statements",
            body.len(),
        );

        for stmt in body.iter() {
            self.visit_stmt(stmt)?;
        }

        Ok(self.db.context().hir_type(Type::new(TypeKind::Absurd, loc)))
    }

    #[crunch_shared::instrument(name = "match", skip(self, loc, cond, arms, ty))]
    fn visit_match(
        &mut self,
        loc: Location,
        &Match { cond, ref arms, ty }: &Match<'ctx>,
    ) -> Self::Output {
        let check = self.check;
        let condition_type = self.visit_expr(cond)?;

        crunch_shared::trace_span!("match_arms").in_scope(|| {
            for arm in arms.iter() {
                crunch_shared::trace_span!("match_arm").in_scope(|| {
                    crunch_shared::trace!("checking match arm pattern");
                    match &arm.bind.pattern {
                        Pattern::Literal(literal) => {
                            crunch_shared::trace!("pattern was a literal");

                            self.check = Some(condition_type);
                            let literal_type = self.visit_literal(loc, literal)?;
                            self.unify(condition_type, literal_type)?;

                            self.check.take();
                        }

                        &Pattern::Ident(variable) => {
                            crunch_shared::trace!("pattern was an ident");

                            self.check = Some(condition_type);
                            let variable_type = self
                                .db
                                .hir_type(Type::new(TypeKind::Variable(condition_type), loc));

                            self.insert_variable(Var::User(variable), variable_type);
                            self.unify(condition_type, variable_type)?;

                            self.check.take();
                        }

                        // TODO: Is this correct?
                        Pattern::Wildcard => {
                            crunch_shared::trace!("pattern was a wildcard");
                            crunch_shared::warn!("Match pattern wildcards are currently ignored");
                        }
                        Pattern::ItemPath(..) => todo!(),
                    }

                    crunch_shared::trace!("checking match arm guard");
                    if let Some(guard) = arm.guard {
                        let guard_ty = self.visit_expr(guard)?;
                        let boolean = self
                            .db
                            .hir_type(Type::new(TypeKind::Bool, guard.location()));

                        self.unify(guard_ty, boolean)?;
                    }

                    self.check = Some(ty);
                    crunch_shared::trace!("checking match arm body");
                    let arm_type = self.with_scope(|builder| {
                        arm.body
                            .iter()
                            .filter_map(|s| builder.visit_stmt(s).transpose())
                            .last()
                            .unwrap_or_else(|| {
                                Ok(builder
                                    .db
                                    .hir_type(Type::new(TypeKind::Unit, arm.body.location())))
                            })
                    })?;

                    crunch_shared::trace!("unifying match arm type");
                    self.unify(ty, arm_type)?;
                    self.check.take();

                    Ok(())
                })?;
            }

            Ok(())
        })?;

        if let Some(check) = check {
            self.unify(ty, check)?;
        }

        Ok(ty)
    }

    #[crunch_shared::instrument(name = "variable", skip(self, loc))]
    fn visit_variable(&mut self, loc: Location, var: Var, _ty: TypeId) -> Self::Output {
        self.var_type(&var, loc)
    }

    #[crunch_shared::instrument(name = "literal", skip(self, loc, literal))]
    fn visit_literal(&mut self, loc: Location, literal: &Literal) -> Self::Output {
        self.intern_literal(literal, loc)
    }

    #[crunch_shared::instrument(name = "scope", skip(self, loc, body))]
    fn visit_scope(&mut self, loc: Location, body: &Block<&'ctx Stmt<'ctx>>) -> Self::Output {
        crunch_shared::trace!("visiting a scope with {} body statements", body.len());

        self.with_scope(|builder| {
            body.iter()
                .filter_map(|s| builder.visit_stmt(s).transpose())
                .last()
                .unwrap_or_else(|| Ok(builder.db.hir_type(Type::new(TypeKind::Unit, loc))))
        })
    }

    #[crunch_shared::instrument(name = "function call", skip(self, loc, call))]
    fn visit_func_call(&mut self, loc: Location, call: &FuncCall<'ctx>) -> Self::Output {
        let func = self
            .functions
            .get(&call.func)
            .ok_or_else(|| {
                crunch_shared::error!(
                    "the function {:?} does not exist",
                    call.func.to_string(self.db.context().strings()),
                );

                Locatable::new(
                    TypeError::FuncNotInScope(call.func.to_string(self.db.context().strings()))
                        .into(),
                    loc,
                )
            })?
            .clone();

        if func.args.len() != call.args.len() {
            crunch_shared::error!(
                "the function {:?} takes {} args but {} were supplied",
                call.func.to_string(self.db.context().strings()),
                func.args.len(),
                call.args.len(),
            );

            let def_site = if func.args.is_empty() {
                func.arg_span
            } else {
                // If there's 1 or more args trim off the parentheses
                func.arg_span
                    .map_span(|span| Span::new(span.start() + 1, span.end() - 1))
            };

            return Err(Locatable::new(
                TypeError::NotEnoughArgs {
                    expected: func.args.len(),
                    received: call.args.len(),
                    def_site,
                }
                .into(),
                loc,
            ));
        }

        for (expr, check) in call.args.iter().zip(func.args.iter().cloned()) {
            self.check = Some(check);
            let expr = self.visit_expr(expr)?;
            self.check.take();

            self.unify(expr, check)?;
        }

        Ok(func.ret)
    }

    #[crunch_shared::instrument(
        name = "comparison",
        skip(self, loc, lhs, op, rhs),
        fields(comparison_type = ?op),
    )]
    fn visit_comparison(
        &mut self,
        loc: Location,
        lhs: &'ctx Expr<'ctx>,
        op: CompOp,
        rhs: &'ctx Expr<'ctx>,
    ) -> Self::Output {
        // FIXME: Make sure the types are comparable
        crunch_shared::warn!("Comparisons are not checked to be comparable");
        // FIXME: The type of comparison is ignored, could be important
        crunch_shared::warn!("Comparison operands are ignored");

        let (left, right) = (self.visit_expr(lhs)?, self.visit_expr(rhs)?);
        self.unify(left, right)?;

        Ok(self.db.hir_type(Type::new(TypeKind::Bool, loc)))
    }

    #[crunch_shared::instrument(name = "assignment", skip(self, loc, value))]
    fn visit_assign(&mut self, loc: Location, var: Var, value: &'ctx Expr<'ctx>) -> Self::Output {
        self.check.take();
        let expected = self.var_type(&var, loc)?;

        self.check = Some(expected);
        let value = self.visit_expr(value)?;

        self.check.take();
        self.unify(expected, value)?;

        Ok(self.db.hir_type(Type::new(TypeKind::Unit, loc)))
    }

    // TODO: This is sketchy and doesn't check if they're bin-op-able
    #[crunch_shared::instrument(
        name = "binary operation",
        skip(self, _loc, lhs, op, rhs),
        fields(operand_type = ?op),
    )]
    fn visit_binop(
        &mut self,
        _loc: Location,
        lhs: &'ctx Expr<'ctx>,
        op: BinaryOp,
        rhs: &'ctx Expr<'ctx>,
    ) -> Self::Output {
        let check = self.check;

        crunch_shared::trace!("visiting left hand side");
        let lhs = self.visit_expr(lhs)?;
        crunch_shared::trace!("visiting right hand side");
        let rhs = self.visit_expr(rhs)?;

        crunch_shared::trace!("unifying binary operation types");
        self.unify(lhs, rhs)?;

        if let Some(check) = check {
            crunch_shared::trace_span!("bin_op_check").in_scope(|| {
                self.unify(lhs, check)?;
                self.unify(rhs, check)
            })?;
        }

        Ok(lhs)
    }

    #[crunch_shared::instrument(name = "type cast", skip(self, _loc, casted, ty))]
    fn visit_cast(&mut self, _loc: Location, &Cast { casted, ty }: &Cast<'ctx>) -> Self::Output {
        crunch_shared::warn!("type casts are not verified in any way");
        // FIXME: Verify that the types are castable
        let _casted = self.visit_expr(casted)?;

        Ok(ty)
    }

    #[crunch_shared::instrument(name = "reference", skip(self, loc, mutable, reference))]
    fn visit_reference(
        &mut self,
        loc: Location,
        &Reference { mutable, reference }: &Reference<'ctx>,
    ) -> Self::Output {
        let referee = self.visit_expr(reference)?;

        Ok(self
            .db
            .hir_type(Type::new(TypeKind::Reference { referee, mutable }, loc)))
    }

    #[crunch_shared::instrument(name = "index", skip(self, loc, var, index))]
    fn visit_index(&mut self, loc: Location, var: Var, index: &'ctx Expr<'ctx>) -> Self::Output {
        let you_size = self.db.hir_type(Type::new(
            TypeKind::Integer {
                signed: None,
                width: None,
            },
            index.location(),
        ));
        let index = self.visit_expr(index)?;
        self.unify(you_size, index)?;

        let var = self.var_type(&var, loc)?;
        let arr_ty = self.db.context().get_hir_type(var).unwrap();

        let mut kind = arr_ty.kind;
        loop {
            match kind {
                TypeKind::Array { element, .. } | TypeKind::Slice { element } => {
                    crunch_shared::trace!(
                        "indexee type was a slice or array, returning the element type {:?}",
                        element,
                    );

                    return Ok(element);
                }

                TypeKind::Variable(ty) => {
                    crunch_shared::trace!("indexee type was a variable, iterating");

                    kind = self.db.context().get_hir_type(ty).unwrap().kind
                }

                _ => {
                    crunch_shared::error!("invalid indexee type: {:?}", kind);

                    return Err(Locatable::new(
                        TypeError::TypeConflict {
                            call_type: self.display_type(&arr_ty.kind),
                            def_type: "slice or arr".to_owned(),
                            def_site: loc,
                        }
                        .into(),
                        loc,
                    ));
                }
            }
        }
    }
}

impl fmt::Debug for Engine<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Engine")
            .field("errors", &self.errors)
            .field("current_func", &self.current_func)
            .field("functions", &self.functions)
            .field("variables", &self.variables)
            .field("check", &self.check)
            .finish()
    }
}
