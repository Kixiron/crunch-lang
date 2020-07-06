#![warn(
    missing_copy_implementations,
    missing_debug_implementations,
    clippy::dbg_macro,
    clippy::missing_safety_doc,
    clippy::wildcard_imports,
    clippy::shadow_unrelated
)]

use crunch_shared::{
    crunch_proc::instrument,
    error::{ErrorHandler, Locatable, Location, Span, TypeError, TypeResult},
    strings::StrInterner,
    trees::hir::{
        Block, Break, CompOp, Expr, ExternFunc, FuncArg, FuncCall, Function, Item, Literal, Match,
        MatchArm, Return, Stmt, TypeKind, Var, VarDecl,
    },
    utils::HashMap,
    visitors::hir::{MutExprVisitor, MutItemVisitor, MutStmtVisitor},
};

type TypeId = usize;

#[derive(Debug, Clone)]
pub struct Engine {
    id_counter: TypeId,
    types: HashMap<TypeId, (TypeInfo, Location)>,
    ids: HashMap<Var, TypeId>,
    errors: ErrorHandler,
    interner: StrInterner,
}

impl Engine {
    pub fn new(interner: StrInterner) -> Self {
        Self {
            id_counter: 0,
            types: HashMap::new(),
            ids: HashMap::new(),
            errors: ErrorHandler::default(),
            interner,
        }
    }

    /// Create a new type term with whatever we have about its type
    fn insert(&mut self, variable: Var, kind: &TypeKind, loc: Location) -> TypeId {
        if let Some(&id) = self.ids.get(&variable) {
            self.types.insert(id, (kind.into(), loc));

            id
        } else {
            let id = self.id_counter;
            self.id_counter += 1;

            self.types.insert(id, (kind.into(), loc));
            self.ids.insert(variable.clone(), id);

            id
        }
    }

    fn get(&self, var: &Var) -> TypeResult<TypeId> {
        self.ids.get(var).copied().ok_or_else(|| {
            Locatable::new(
                TypeError::VarNotInScope(var.to_string(&self.interner)).into(),
                Location::implicit(Span::new(0, 0), crunch_shared::files::FileId::new(0)),
            )
        })
    }

    fn insert_bare(&mut self, info: TypeInfo, loc: Location) -> TypeId {
        let id = self.id_counter;
        self.id_counter += 1;

        self.types.insert(id, (info, loc));

        id
    }

    /// Make the types of two type terms equivalent (or produce an error if
    /// there is a conflict between them)
    fn unify(&mut self, a: TypeId, b: TypeId) -> TypeResult<()> {
        match (self.types[&a].clone(), self.types[&b].clone()) {
            // Follow any references
            ((TypeInfo::Ref(a), _), _) => self.unify(a, b),
            (_, (TypeInfo::Ref(b), _)) => self.unify(a, b),

            // When we don't know anything about either term, assume that
            // they match and make the one we know nothing about reference the
            // one we may know something about
            ((TypeInfo::Infer, loc), _) => {
                self.types.insert(a, (TypeInfo::Ref(b), loc));

                Ok(())
            }
            (_, (TypeInfo::Infer, loc)) => {
                self.types.insert(b, (TypeInfo::Ref(a), loc));

                Ok(())
            }

            // Primitives are trivial to unify
            ((TypeInfo::Integer, _), (TypeInfo::Integer, _))
            | ((TypeInfo::String, _), (TypeInfo::String, _))
            | ((TypeInfo::Bool, _), (TypeInfo::Bool, _))
            | ((TypeInfo::Unit, _), (TypeInfo::Unit, _)) => Ok(()),

            // If no previous attempts to unify were successful, raise an error
            ((a_ty, a_loc), (b_ty, b_loc)) => {
                let a = self.ids.iter().find_map(|(name, id)| {
                    if *id == a {
                        Some(name.to_string(&self.interner))
                    } else {
                        None
                    }
                });

                let b = self.ids.iter().find_map(|(name, id)| {
                    if *id == b {
                        Some(name.to_string(&self.interner))
                    } else {
                        None
                    }
                });

                Err(Locatable::new(
                    TypeError::TypeConflict([
                        // TODO: Pretty print the types
                        (a, format!("{:?}", a_ty), a_loc),
                        (b, format!("{:?}", b_ty), b_loc),
                    ])
                    .into(),
                    b_loc,
                ))
            }
        }
    }

    /// Attempt to reconstruct a concrete type from the given type term ID. This
    /// may fail if we don't yet have enough information to figure out what the
    /// type is.
    fn reconstruct(&self, id: TypeId) -> TypeResult<TypeKind> {
        match self.types[&id].clone() {
            (TypeInfo::Infer, loc) => Err(Locatable::new(
                TypeError::FailedInfer(
                    self.ids
                        .iter()
                        .find_map(|(name, _id)| {
                            if *_id == id {
                                Some(name.to_string(&self.interner))
                            } else {
                                None
                            }
                        })
                        .unwrap_or("<anonymous type>".to_owned()),
                )
                .into(),
                loc,
            )),
            (TypeInfo::Ref(id), _) => self.reconstruct(id),
            (TypeInfo::Integer, _) => Ok(TypeKind::Integer),
            (TypeInfo::Bool, _) => Ok(TypeKind::Bool),
            (TypeInfo::Unit, _) => Ok(TypeKind::Unit),
            (TypeInfo::String, _) => Ok(TypeKind::String),
        }
    }

    #[instrument(name = "type checking")]
    pub fn walk(&mut self, items: &mut [Item]) -> Result<ErrorHandler, ErrorHandler> {
        for item in items {
            if let Err(err) = self.visit_item(item) {
                self.errors.push_err(err);
            }
        }

        if self.errors.is_fatal() {
            Err(self.errors.take())
        } else {
            Ok(self.errors.take())
        }
    }

    pub fn type_of(&self, var: &Var) -> TypeResult<TypeKind> {
        if let Some(&id) = self.ids.get(var) {
            self.reconstruct(id)
        } else {
            Err(Locatable::new(
                TypeError::VarNotInScope(var.to_string(&self.interner)).into(),
                Location::implicit(Span::new(0, 0), crunch_shared::files::FileId::new(0)),
            ))
        }
    }
}

impl MutItemVisitor for Engine {
    type Output = TypeResult<()>;

    fn visit_item(&mut self, item: &mut Item) -> Self::Output {
        match item {
            Item::Function(func) => self.visit_func(func),
            Item::ExternFunc(func) => self.visit_extern_func(func),
        }
    }

    fn visit_func(
        &mut self,
        Function {
            args,
            body,
            ret,
            sig,
            ..
        }: &mut Function,
    ) -> Self::Output {
        let mut missing_arg_ty = None;
        for arg in args.iter().filter(|a| !a.kind.is_infer()) {
            if let Some(err) = missing_arg_ty.take() {
                self.errors.push_err(err);
            }

            missing_arg_ty = Some(Locatable::new(
                TypeError::MissingType("Types for function arguments".to_owned()).into(),
                arg.loc,
            ));
        }

        if ret.kind.is_infer() {
            if let Some(err) = missing_arg_ty {
                self.errors.push_err(err);
            }

            return Err(Locatable::new(
                TypeError::MissingType("Return types for functions".to_owned()).into(),
                ret.loc,
            ));
        } else if let Some(err) = missing_arg_ty {
            return Err(err);
        }

        let func_args: Vec<_> = args
            .iter()
            .map(|FuncArg { name, kind, loc }| self.insert(*name, kind, *loc))
            .collect();

        let ret_type = self.insert_bare(TypeInfo::from(&ret.kind), *sig);
        for stmt in body.iter_mut() {
            let ty = self.visit_stmt(stmt)?;
            self.unify(ty, ret_type)?;
        }

        ret.kind = self.reconstruct(ret_type)?;

        for (i, arg) in func_args.into_iter().enumerate() {
            args[i].kind = self.reconstruct(arg)?;
        }

        Ok(())
    }

    fn visit_extern_func(&mut self, ExternFunc { args, ret, .. }: &mut ExternFunc) -> Self::Output {
        let mut missing_arg_ty = None;
        for arg in args.iter().filter(|a| !a.kind.is_infer()) {
            if let Some(err) = missing_arg_ty.take() {
                self.errors.push_err(err);
            }

            missing_arg_ty = Some(Locatable::new(
                TypeError::MissingType("Types for external function arguments".to_owned()).into(),
                arg.loc,
            ));
        }

        if ret.kind.is_infer() {
            if let Some(err) = missing_arg_ty {
                self.errors.push_err(err);
            }

            return Err(Locatable::new(
                TypeError::MissingType("Return types for external functions".to_owned()).into(),
                ret.loc,
            ));
        } else if let Some(err) = missing_arg_ty {
            return Err(err);
        }

        Ok(())
    }
}

impl MutStmtVisitor for Engine {
    type Output = TypeResult<TypeId>;

    #[inline]
    fn visit_stmt(&mut self, stmt: &mut Stmt) -> <Self as MutStmtVisitor>::Output {
        match stmt {
            Stmt::VarDecl(decl) => self.visit_var_decl(decl),

            Stmt::Item(item) => {
                self.visit_item(item)?;

                // FIXME: This is bad, very bad
                Ok(0)
            }

            Stmt::Expr(expr) => self.visit_expr(expr),
        }
    }

    fn visit_var_decl(
        &mut self,
        VarDecl {
            name,
            value,
            ty,
            loc,
            ..
        }: &mut VarDecl,
    ) -> <Self as MutStmtVisitor>::Output {
        let var = self.insert(*name, &ty.kind, *loc);
        let expr = self.visit_expr(value)?;

        self.unify(var, expr)?;
        ty.kind = self.reconstruct(var)?;

        Ok(self.insert_bare(TypeInfo::Unit, *loc))
    }
}

impl MutExprVisitor for Engine {
    type Output = TypeResult<TypeId>;

    fn visit_return(&mut self, _loc: Location, _value: &mut Return) -> Self::Output {
        todo!()
    }

    fn visit_break(&mut self, _loc: Location, _value: &mut Break) -> Self::Output {
        todo!()
    }

    fn visit_continue(&mut self, _loc: Location) -> Self::Output {
        todo!()
    }

    fn visit_loop(&mut self, _loc: Location, _body: &mut Block<Stmt>) -> Self::Output {
        todo!()
    }

    fn visit_match(&mut self, loc: Location, Match { cond, arms, ty }: &mut Match) -> Self::Output {
        let match_cond = self.visit_expr(cond)?;
        let b = self.insert_bare(TypeInfo::Bool, cond.location());
        if self.unify(match_cond, b).is_err() {
            return Err(Locatable::new(
                TypeError::IncorrectType("If conditions must be booleans".to_owned()).into(),
                cond.location(),
            ));
        }

        let mut arm_types = Vec::new();
        for MatchArm {
            bind: _,
            guard,
            body,
            ty,
            ..
        } in arms.iter_mut()
        {
            let arm_ty = self.insert_bare(TypeInfo::from(&*ty), cond.location());

            // FIXME: Bindings

            if let Some(guard) = guard {
                let guard_ty = self.visit_expr(guard)?;
                let boolean = self.insert_bare(TypeInfo::Bool, guard.location());
                self.unify(guard_ty, boolean)?;
            }

            let arm_ret = body
                .iter_mut()
                .map(|s| self.visit_stmt(s))
                .collect::<TypeResult<Vec<TypeId>>>()?
                .get(0)
                .copied()
                .unwrap_or_else(|| self.insert_bare(TypeInfo::Unit, body.location()));

            self.unify(arm_ty, arm_ret)?;
            *ty = self.reconstruct(arm_ty)?;

            arm_types.push(arm_ty);
        }

        let match_ty = self.insert_bare(TypeInfo::from(&*ty), loc);
        for arm in arm_types {
            self.unify(match_ty, arm)?;
        }

        *ty = self.reconstruct(match_ty)?;

        Ok(match_ty)
    }

    fn visit_variable(&mut self, _loc: Location, var: Var, _ty: &mut TypeKind) -> Self::Output {
        self.get(&var)
    }

    fn visit_literal(&mut self, loc: Location, literal: &mut Literal) -> Self::Output {
        let info = TypeInfo::from(&*literal);
        let id = self.insert_bare(info, loc);

        Ok(id)
    }

    fn visit_scope(&mut self, _loc: Location, _body: &mut Block<Stmt>) -> Self::Output {
        todo!()
    }

    fn visit_func_call(&mut self, _loc: Location, _call: &mut FuncCall) -> Self::Output {
        todo!()
    }

    fn visit_comparison(
        &mut self,
        loc: Location,
        lhs: &mut Expr,
        _op: CompOp,
        rhs: &mut Expr,
    ) -> Self::Output {
        let (left, right) = (self.visit_expr(lhs)?, self.visit_expr(rhs)?);
        self.unify(left, right)?;

        Ok(self.insert_bare(TypeInfo::Bool, loc))
    }

    fn visit_assign(&mut self, _loc: Location, _var: Var, _value: &mut Expr) -> Self::Output {
        todo!()
    }

    fn visit_binop(
        &mut self,
        _loc: Location,
        _lhs: &mut Expr,
        _op: crunch_shared::trees::hir::BinaryOp,
        _rhs: &mut Expr,
    ) -> Self::Output {
        todo!()
    }
}

#[derive(Debug, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
enum TypeInfo {
    Ref(TypeId),
    Infer,
    Integer,
    String,
    Bool,
    Unit,
}

impl From<&Literal> for TypeInfo {
    fn from(literal: &Literal) -> Self {
        match literal {
            Literal::Integer(..) => Self::Integer,
            Literal::Bool(..) => Self::Bool,
            Literal::String(..) => Self::String,

            _ => todo!(),
        }
    }
}

impl From<&TypeKind> for TypeInfo {
    fn from(kind: &TypeKind) -> Self {
        match kind {
            TypeKind::Infer => Self::Infer,
            TypeKind::Integer => Self::Integer,
            TypeKind::String => Self::String,
            TypeKind::Bool => Self::Bool,
            TypeKind::Unit => Self::Unit,
            TypeKind::Pointer(..) => todo!(),
        }
    }
}

#[test]
fn test() {
    use crunch_parser::Parser;
    use crunch_shared::{
        context::Context,
        files::{CurrentFile, FileId, Files},
    };
    use ladder::Ladder;

    simple_logger::init().ok();

    let source = r#"
    fn main()
        let mut greeting := "Hello from Crunch!"
        :: println(greeting)

        if greeting == "Hello"
            "test"
        else
            "test2"
        end

        :: match greeting
        ::     string where string == "some string" =>
        ::         :: println("this can't happen")
        ::     end
        :: 
        ::     greeting =>
        ::         :: println("{}", greeting)
        ::     end
        :: end
    end
    "#;

    let ctx = Context::default();
    let mut files = Files::new();
    files.add("<test>", source);

    match Parser::new(
        source,
        CurrentFile::new(FileId::new(0), source.len()),
        ctx.clone(),
    )
    .parse()
    {
        Ok((ast, mut warnings)) => {
            warnings.emit(&files);

            // println!("Nodes: {:#?}", &ast);
            // println!("Symbols: {:#?}", &module_scope);

            let mut ladder = Ladder::new();

            let mut hir = ladder.lower(&ast);
            println!("HIR: {:#?}", hir);

            let mut engine = Engine::new(ctx.strings.clone());

            match engine.walk(&mut hir) {
                Ok(mut warnings) => {
                    println!(
                        "Type checking completed successfully with {} warnings",
                        warnings.warn_len(),
                    );
                    warnings.emit(&files);

                    println!(
                        "Type of `greeting`: {:?}",
                        engine
                            .type_of(&Var::User(ctx.strings.intern("greeting")))
                            .unwrap(),
                    );
                }

                Err(mut errors) => {
                    println!(
                        "Type checking failed with {} warnings and {} errors",
                        errors.warn_len(),
                        errors.err_len(),
                    );
                    errors.emit(&files);
                }
            }
        }

        Err(mut err) => {
            err.emit(&files);
        }
    }
}
