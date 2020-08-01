#![warn(
    missing_copy_implementations,
    missing_debug_implementations,
    clippy::dbg_macro,
    clippy::missing_safety_doc,
    clippy::wildcard_imports,
    clippy::shadow_unrelated
)]

extern crate alloc;

mod bidirectional;

use alloc::sync::Arc;
use core::fmt::{self, Result as FmtResult, Write};
use crunch_shared::{
    context::ContextDatabase,
    error::{ErrorHandler, Locatable, Location, Span, TypeError, TypeResult},
    files::{FileCache, FileId},
    salsa,
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
        Self {
            errors: ErrorHandler::default(),
            current_func: None,
            functions: HashMap::with_hasher(Hasher::default()),
            variables: Vec::new(),
            check: None,
            db,
        }
    }

    /// Create a new type term with whatever we have about its type
    fn insert(&mut self, var: Var, type_id: TypeId) {
        if let Some(old_type) = self.variables.last_mut().unwrap().insert(var, type_id) {
            crunch_shared::warn!(
                "The variable {:?} previously had the type {:?} but it was overwritten with {:?}",
                var,
                type_id,
                old_type,
            );
        }
    }

    // TODO: Caching
    fn var_type(&self, var: &Var, loc: Location) -> TypeResult<TypeId> {
        self.variables
            .iter()
            .rev()
            .find_map(|vars| vars.get(var))
            .copied()
            .ok_or_else(|| {
                Locatable::new(
                    TypeError::VarNotInScope(var.to_string(&self.db.context().strings())).into(),
                    loc,
                )
            })
    }

    fn insert_variable(&mut self, var: Var, ty: TypeId) {
        self.variables.last_mut().unwrap().insert(var, ty);
    }

    fn push_scope(&mut self) {
        self.variables.push(HashMap::with_hasher(Hasher::default()));
    }

    fn pop_scope(&mut self) {
        self.variables.pop().unwrap();
    }

    fn with_scope<F, T>(&mut self, func: F) -> T
    where
        F: FnOnce(&mut Self) -> T,
    {
        self.push_scope();
        let result = func(self);
        self.pop_scope();

        result
    }

    /// Make the types of two type terms equivalent (or produce an error if
    /// there is a conflict between them)
    // TODO: Caching
    fn unify(&mut self, left: TypeId, right: TypeId) -> TypeResult<()> {
        if left == right {
            return Ok(());
        }

        const EXPECT_MSG: &str = "Attempted to get a type that does not exist";
        let (left_ty, right_ty) = (
            self.db.context().get_hir_type(left).expect(EXPECT_MSG),
            self.db.context().get_hir_type(right).expect(EXPECT_MSG),
        );

        // TODO: Cycle detection
        match (left_ty.kind, right_ty.kind) {
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

            (TypeKind::Absurd, _) | (_, TypeKind::Absurd) => Ok(()),
            (TypeKind::String, TypeKind::String)
            | (TypeKind::Bool, TypeKind::Bool)
            | (TypeKind::Unit, TypeKind::Unit) => Ok(()),

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
                match (signed_a, signed_b) {
                    (Some(signed_a), Some(signed_b)) if signed_a != signed_b => {
                        todo!("Conflicting sign, return error")
                    }
                    _ => {}
                }
                match (width_a, width_b) {
                    (Some(width_a), Some(width_b)) if width_a != width_b => {
                        todo!("Conflicting width, return error")
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
                self.unify(left, right)?;

                Ok(())
            }

            // If no previous attempts to unify were successful, raise an error
            (call_type, def_type) => Err(Locatable::new(
                TypeError::TypeConflict {
                    call_type: self.display_type(&call_type),
                    def_type: self.display_type(&def_type),
                    def_site: right_ty.location(),
                }
                .into(),
                left_ty.location(),
            )),
        }
    }

    pub fn walk(&mut self, items: &[&'ctx Item<'ctx>]) -> Result<ErrorHandler, ErrorHandler> {
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
                                builder.insert(name, kind);
                                kind
                            })
                            .collect();

                        let func = Func {
                            ret,
                            args,
                            arg_span,
                            sig,
                        };

                        builder.functions.insert(name.clone(), func);
                    }
                }
            }

            for item in items {
                if let Err(err) = builder.visit_item(item) {
                    builder.errors.push_err(err);
                }
            }

            if builder.errors.is_fatal() {
                Err(builder.errors.take())
            } else {
                Ok(builder.errors.take())
            }
        })
    }

    // TODO: Caching
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

                for elem in elements {
                    let elem = self.intern_literal(elem, elem.location())?;
                    self.unify(element, elem)?;
                }
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

    fn visit_item(&mut self, item: &Item<'ctx>) -> Self::Output {
        match item {
            Item::Function(func) => self.visit_func(func),
            Item::ExternFunc(func) => self.visit_extern_func(func),
        }
    }

    fn visit_func(
        &mut self,
        Function {
            name, body, args, ..
        }: &Function<'ctx>,
    ) -> Self::Output {
        self.with_scope(|builder| {
            builder.current_func = Some(builder.functions.get(name).unwrap().clone());

            for arg in args.iter() {
                builder.insert(arg.name, arg.kind);
            }

            for stmt in body.iter() {
                builder.visit_stmt(stmt)?;
            }

            builder.current_func = None;

            Ok(())
        })
    }

    fn visit_extern_func(
        &mut self,
        &ExternFunc { ref args, ret, .. }: &ExternFunc,
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

    #[inline]
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
        self.insert(name, ty);
        self.unify(expr, ty)?;

        Ok(Some(self.db.hir_type(Type::new(TypeKind::Unit, loc))))
    }
}

impl<'ctx> ExprVisitor<'ctx> for Engine<'ctx> {
    type Output = TypeResult<TypeId>;

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

    fn visit_break(&mut self, _loc: Location, _value: &Break<'ctx>) -> Self::Output {
        todo!()
    }

    fn visit_continue(&mut self, _loc: Location) -> Self::Output {
        todo!()
    }

    fn visit_loop(&mut self, _loc: Location, _body: &Block<&'ctx Stmt<'ctx>>) -> Self::Output {
        todo!()
    }

    fn visit_match(
        &mut self,
        loc: Location,
        &Match { cond, ref arms, ty }: &Match<'ctx>,
    ) -> Self::Output {
        let check = self.check;
        let condition_type = self.visit_expr(cond)?;

        for arm in arms.iter() {
            match &arm.bind.pattern {
                Pattern::Literal(literal) => {
                    self.check = Some(condition_type);
                    let literal_type = self.visit_literal(loc, literal)?;
                    self.unify(condition_type, literal_type)?;

                    self.check.take();
                }

                &Pattern::Ident(variable) => {
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
                    crunch_shared::warn!("Match pattern wildcards are currently ignored");
                }
                Pattern::ItemPath(..) => todo!(),
            }

            if let Some(guard) = arm.guard {
                let guard_ty = self.visit_expr(guard)?;
                let boolean = self
                    .db
                    .hir_type(Type::new(TypeKind::Bool, guard.location()));

                self.unify(guard_ty, boolean)?;
            }

            self.check = Some(ty);
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

            self.unify(ty, arm_type)?;
            self.check.take();
        }

        if let Some(check) = check {
            self.unify(ty, check)?;
        }

        Ok(ty)
    }

    fn visit_variable(&mut self, loc: Location, var: Var, _ty: TypeId) -> Self::Output {
        self.var_type(&var, loc)
    }

    fn visit_literal(&mut self, loc: Location, literal: &Literal) -> Self::Output {
        self.intern_literal(literal, loc)
    }

    fn visit_scope(&mut self, loc: Location, body: &Block<&'ctx Stmt<'ctx>>) -> Self::Output {
        self.with_scope(|builder| {
            body.iter()
                .filter_map(|s| builder.visit_stmt(s).transpose())
                .last()
                .unwrap_or_else(|| Ok(builder.db.hir_type(Type::new(TypeKind::Unit, loc))))
        })
    }

    fn visit_func_call(&mut self, loc: Location, call: &FuncCall<'ctx>) -> Self::Output {
        let func = self
            .functions
            .get(&call.func)
            .ok_or_else(|| {
                Locatable::new(
                    TypeError::FuncNotInScope(call.func.to_string(&self.db.context().strings()))
                        .into(),
                    loc,
                )
            })?
            .clone();

        if func.args.len() != call.args.len() {
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

    fn visit_comparison(
        &mut self,
        loc: Location,
        lhs: &'ctx Expr<'ctx>,
        _op: CompOp,
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
    fn visit_binop(
        &mut self,
        _loc: Location,
        lhs: &'ctx Expr<'ctx>,
        _op: BinaryOp,
        rhs: &'ctx Expr<'ctx>,
    ) -> Self::Output {
        let check = self.check;

        let lhs = self.visit_expr(lhs)?;
        let rhs = self.visit_expr(rhs)?;
        self.unify(lhs, rhs)?;

        if let Some(check) = check {
            self.unify(lhs, check)?;
            self.unify(rhs, check)?;
        }

        Ok(lhs)
    }

    fn visit_cast(&mut self, _loc: Location, &Cast { casted, ty }: &Cast<'ctx>) -> Self::Output {
        crunch_shared::warn!("Type casts are not verified in any way");
        // FIXME: Verify that the types are castable
        let _casted = self.visit_expr(casted)?;

        Ok(ty)
    }

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
