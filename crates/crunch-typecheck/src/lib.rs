#![warn(
    missing_copy_implementations,
    missing_debug_implementations,
    clippy::dbg_macro,
    clippy::missing_safety_doc,
    clippy::wildcard_imports,
    clippy::shadow_unrelated
)]

use core::fmt::{Result as FmtResult, Write};
use crunch_shared::{
    crunch_proc::instrument,
    error::{ErrorHandler, Locatable, Location, Span, TypeError, TypeResult},
    strings::StrInterner,
    trees::{
        hir::{
            BinaryOp, Block, Break, Cast, CompOp, Expr, ExternFunc, FuncArg, FuncCall, Function,
            Item, Literal, Match, MatchArm, Pattern, Reference, Return, Stmt, TypeKind, Var,
            VarDecl,
        },
        ItemPath, Ref, Signedness,
    },
    utils::HashMap,
    visitors::hir::{MutExprVisitor, MutItemVisitor, MutStmtVisitor},
};

type TypeId = usize;

#[derive(Debug, Clone)]
struct Func {
    ret: Locatable<TypeId>,
    args: Locatable<Vec<TypeId>>,
    sig: Location,
}

#[derive(Debug, Clone)]
pub struct Engine {
    id_counter: TypeId,
    types: HashMap<TypeId, (TypeInfo, Location)>,
    ids: HashMap<Var, TypeId>,
    errors: ErrorHandler,
    interner: StrInterner,
    current_func: Option<Func>,
    functions: HashMap<ItemPath, Func>,
}

impl Engine {
    pub fn new(interner: StrInterner) -> Self {
        Self {
            id_counter: 0,
            types: HashMap::new(),
            ids: HashMap::new(),
            errors: ErrorHandler::default(),
            interner,
            current_func: None,
            functions: HashMap::new(),
        }
    }

    /// Create a new type term with whatever we have about its type
    fn insert(&mut self, variable: Var, kind: &TypeKind, loc: Location) -> TypeResult<TypeId> {
        if let Some(&id) = self.ids.get(&variable) {
            let ty = self.intern_type(kind, loc)?;
            self.types.insert(id, (ty, loc));

            Ok(id)
        } else {
            let id = self.id_counter;
            self.id_counter += 1;

            let ty = self.intern_type(kind, loc)?;
            self.types.insert(id, (ty, loc));
            self.ids.insert(variable.clone(), id);

            Ok(id)
        }
    }

    fn get(&self, var: &Var, loc: Location) -> TypeResult<TypeId> {
        self.ids.get(var).copied().ok_or_else(|| {
            Locatable::new(
                TypeError::VarNotInScope(var.to_string(&self.interner)).into(),
                loc,
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

            ((TypeInfo::Absurd, loc), _) => {
                self.types.insert(a, (TypeInfo::Ref(b), loc));

                Ok(())
            }
            (_, (TypeInfo::Absurd, loc)) => {
                self.types.insert(b, (TypeInfo::Ref(a), loc));

                Ok(())
            }

            // Primitives are trivial to unify
            ((TypeInfo::String, _), (TypeInfo::String, _))
            | ((TypeInfo::Bool, _), (TypeInfo::Bool, _))
            | ((TypeInfo::Unit, _), (TypeInfo::Unit, _)) => Ok(()),

            #[rustfmt::skip]
            ((TypeInfo::Integer { sign: Some(left_sign), width: Some(left_width) }, _),
                (TypeInfo::Integer { sign: Some(right_sign), width: Some(right_width) }, _))
                if left_sign == right_sign && left_width == right_width => Ok(()),

            #[rustfmt::skip]
            (
                (TypeInfo::Integer { sign: None, width: None }, loc),
                (TypeInfo::Integer { sign: Some(sign), width: Some(width) }, _),
            ) => {
                self.types.insert(
                    a,
                    (
                        TypeInfo::Integer {
                            sign: Some(sign),
                            width: Some(width),
                        },
                        loc,
                    ),
                );

                Ok(())
            }
            #[rustfmt::skip]
            (
                (TypeInfo::Integer { sign: Some(sign), width: Some(width) }, _),
                (TypeInfo::Integer { sign: None, width: None }, loc),
            ) => {
                self.types.insert(
                    b,
                    (
                        TypeInfo::Integer {
                            sign: Some(sign),
                            width: Some(width),
                        },
                        loc,
                    ),
                );

                Ok(())
            }
            #[rustfmt::skip]
            (
                (TypeInfo::Integer { sign: None, width: None }, _),
                (TypeInfo::Integer { sign: None, width: None }, loc),
            ) => {
                self.types.insert(
                    b,
                    (
                        TypeInfo::Ref(a),
                        loc,
                    ),
                );

                Ok(())
            }

            (
                (TypeInfo::Array(left_elem, left_len), _),
                (TypeInfo::Array(right_elem, right_len), _),
            ) if left_len == right_len => {
                self.unify(left_elem, right_elem)?;

                Ok(())
            }

            (
                (TypeInfo::Reference(left_mut, left), _),
                (TypeInfo::Reference(right_mut, right), _),
            ) if left_mut == right_mut => {
                self.unify(left, right)?;

                Ok(())
            }

            ((TypeInfo::Pointer(left), _), (TypeInfo::Pointer(right), _)) => {
                self.unify(left, right)?;

                Ok(())
            }

            // If no previous attempts to unify were successful, raise an error
            ((call_type, call_loc), (def_type, def_loc)) => Err(Locatable::new(
                TypeError::TypeConflict {
                    call_type: self.display_type(&call_type),
                    def_type: self.display_type(&def_type),
                    def_site: def_loc,
                }
                .into(),
                call_loc,
            )),
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
            // TODO: This seems wrong
            (TypeInfo::Integer { sign, width }, _) => Ok(TypeKind::Integer {
                sign: sign.unwrap_or(Signedness::Signed),
                width: width.unwrap_or(32),
            }),
            (TypeInfo::Bool, _) => Ok(TypeKind::Bool),
            (TypeInfo::Unit, _) => Ok(TypeKind::Unit),
            (TypeInfo::String, _) => Ok(TypeKind::String),
            (TypeInfo::Absurd, _) => Ok(TypeKind::Absurd),
            (TypeInfo::Pointer(pointee), _) => {
                Ok(TypeKind::Pointer(Ref::new(self.reconstruct(pointee)?)))
            }
            (TypeInfo::Array(elem, len), _) => {
                Ok(TypeKind::Array(Ref::new(self.reconstruct(elem)?), len))
            }
            (TypeInfo::Slice(elem), _) => Ok(TypeKind::Slice(Ref::new(self.reconstruct(elem)?))),
            (TypeInfo::Reference(mutable, reference), _) => Ok(TypeKind::Reference(
                mutable,
                Ref::new(self.reconstruct(reference)?),
            )),
        }
    }

    #[instrument(name = "type checking")]
    #[allow(irrefutable_let_patterns)]
    pub fn walk(&mut self, items: &mut [Item]) -> Result<ErrorHandler, ErrorHandler> {
        for item in items.iter() {
            if let Item::Function(Function {
                name,
                args,
                ret,
                sig,
                ..
            })
            | Item::ExternFunc(ExternFunc {
                name,
                args,
                ret,
                loc: sig,
                ..
            }) = item
            {
                // TODO: Use error types here
                for arg in args.iter().filter(|a| a.kind.is_infer()) {
                    self.errors.push_err(Locatable::new(
                        TypeError::MissingType("Types for function arguments".to_owned()).into(),
                        arg.location(),
                    ));
                }

                // TODO: Use error types here
                if ret.kind.is_infer() {
                    self.errors.push_err(Locatable::new(
                        TypeError::MissingType("Return types for functions".to_owned()).into(),
                        ret.location(),
                    ));
                }

                // TODO: Use error types here
                let ret_type = match self.intern_type(&ret.kind, ret.location()) {
                    Ok(ty) => ty,
                    Err(err) => {
                        self.errors.push_err(err);
                        todo!()
                    }
                };

                // TODO: Use error types here
                let func_args = args
                    .iter()
                    .map(|FuncArg { name, kind, loc }| self.insert(*name, kind, *loc))
                    .collect::<TypeResult<Vec<_>>>()?;

                let func = Func {
                    ret: Locatable::new(self.insert_bare(ret_type, ret.location()), ret.location()),
                    args: Locatable::new(func_args, args.location()),
                    sig: *sig,
                };

                self.functions.insert(name.clone(), func);
            }
        }

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

    pub fn type_of(&self, var: &Var) -> Option<TypeKind> {
        self.ids.get(var).and_then(|id| self.reconstruct(*id).ok())
    }

    fn intern_type(&mut self, kind: &TypeKind, loc: Location) -> TypeResult<TypeInfo> {
        let ty = match kind {
            TypeKind::Infer => TypeInfo::Infer,
            TypeKind::Integer { sign, width } => TypeInfo::Integer {
                sign: Some(*sign),
                width: Some(*width),
            },
            TypeKind::String => TypeInfo::String,
            TypeKind::Bool => TypeInfo::Bool,
            TypeKind::Unit => TypeInfo::Unit,
            TypeKind::Absurd => TypeInfo::Absurd,
            TypeKind::Pointer(pointee) => {
                if pointee.is_infer() {
                    return Err(Locatable::new(
                        TypeError::MissingType("pointer types".to_owned()).into(),
                        loc,
                    ));
                } else {
                    let pointee = self.intern_type(&**pointee, loc)?;

                    TypeInfo::Pointer(self.insert_bare(pointee, loc))
                }
            }
            TypeKind::Array(elem, len) => {
                let elem = self.intern_type(elem, loc)?;
                TypeInfo::Array(self.insert_bare(elem, loc), *len)
            }
            TypeKind::Slice(elem) => {
                let elem = self.intern_type(elem, loc)?;
                TypeInfo::Slice(self.insert_bare(elem, loc))
            }
            TypeKind::Reference(mutable, ty) => {
                let ty = self.intern_type(ty, loc)?;
                TypeInfo::Reference(*mutable, self.insert_bare(ty, loc))
            }
        };

        Ok(ty)
    }

    fn intern_literal(&mut self, literal: &Literal, loc: Location) -> TypeResult<TypeInfo> {
        let ty = match literal {
            Literal::Integer(..) => TypeInfo::Integer {
                sign: None,
                width: None,
            },
            Literal::Bool(..) => TypeInfo::Bool,
            Literal::String(..) => TypeInfo::String,
            Literal::Array(array) => {
                // FIXME: Make sure all literals in the array are the same
                let elements = array
                    .iter()
                    .map(|elem| {
                        let elem = self.intern_literal(elem, loc)?;
                        Ok(self.insert_bare(elem, loc))
                    })
                    .collect::<TypeResult<Vec<TypeId>>>()?;

                let elem_type = self.insert_bare(TypeInfo::Infer, loc);
                for elem in elements.iter() {
                    self.unify(*elem, elem_type)?;
                }

                TypeInfo::Array(elem_type, elements.len() as u32)
            }

            _ => todo!(),
        };

        Ok(ty)
    }

    fn display_type(&self, ty: &TypeInfo) -> String {
        let mut string = String::new();
        self.display_type_inner(ty, &mut string)
            .expect("Failed to format type");

        string
    }

    fn display_type_inner<W: Write>(&self, ty: &TypeInfo, f: &mut W) -> FmtResult {
        match ty {
            TypeInfo::Ref(inner) => self.display_type_inner(
                &self
                    .types
                    .get(inner)
                    .expect("Tried to display type that doesn't exist")
                    .0,
                f,
            ),
            TypeInfo::Infer => f.write_str("infer"),
            TypeInfo::Integer { sign, width } => match (sign, width) {
                (Some(sign), Some(width)) => write!(f, "{}{}", sign, width),
                (_, _) => f.write_str("{{integer}}"),
            },
            TypeInfo::String => f.write_str("str"),
            TypeInfo::Bool => f.write_str("bool"),
            TypeInfo::Unit => f.write_str("unit"),
            TypeInfo::Absurd => f.write_str("absurd"),
            TypeInfo::Pointer(pointee) => {
                f.write_str("*const ")?;
                self.display_type_inner(
                    &self
                        .types
                        .get(pointee)
                        .expect("Tried to display type that doesn't exist")
                        .0,
                    f,
                )
            }
            TypeInfo::Array(elem, len) => {
                f.write_str("arr[")?;
                self.display_type_inner(
                    &self
                        .types
                        .get(elem)
                        .expect("Tried to display type that doesn't exist")
                        .0,
                    f,
                )?;
                f.write_str("; ")?;
                write!(f, "{}", len)?;
                f.write_char(']')
            }
            TypeInfo::Slice(elem) => {
                f.write_str("arr[")?;
                self.display_type_inner(
                    &self
                        .types
                        .get(elem)
                        .expect("Tried to display type that doesn't exist")
                        .0,
                    f,
                )?;
                f.write_char(']')
            }
            TypeInfo::Reference(mutable, reference) => {
                f.write_char('&')?;
                if *mutable {
                    f.write_str("mut ")?;
                }

                self.display_type_inner(
                    &self
                        .types
                        .get(reference)
                        .expect("Tried to display type that doesn't exist")
                        .0,
                    f,
                )
            }
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

    fn visit_func(&mut self, Function { name, body, .. }: &mut Function) -> Self::Output {
        self.current_func = Some(self.functions.get(name).unwrap().clone());
        for stmt in body.iter_mut() {
            self.visit_stmt(stmt)?;
        }
        self.current_func = None;

        Ok(())
    }

    fn visit_extern_func(&mut self, ExternFunc { args, ret, .. }: &mut ExternFunc) -> Self::Output {
        let mut missing_arg_ty = None;
        for arg in args.iter().filter(|a| a.kind.is_infer()) {
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
        let var = self.insert(*name, &ty.kind, *loc)?;
        let expr = self.visit_expr(value)?;

        self.unify(var, expr)?;
        ty.kind = self.reconstruct(var)?;

        Ok(self.insert_bare(TypeInfo::Unit, *loc))
    }
}

impl MutExprVisitor for Engine {
    type Output = TypeResult<TypeId>;

    fn visit_return(&mut self, loc: Location, ret: &mut Return) -> Self::Output {
        let func_ret = *self.current_func.as_ref().unwrap().ret;

        if let Some(ref mut ret) = ret.val {
            let ret = self.visit_expr(ret)?;
            self.unify(ret, func_ret)?;
        } else {
            let unit = self.insert_bare(TypeInfo::Unit, loc);
            self.unify(unit, func_ret)?;
        }

        Ok(self.insert_bare(TypeInfo::Absurd, loc))
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

        let mut arm_types = Vec::new();
        for MatchArm {
            bind,
            guard,
            body,
            ty,
            ..
        } in arms.iter_mut()
        {
            match &mut bind.pattern {
                Pattern::Literal(lit) => {
                    let lit = self.intern_literal(&*lit, loc)?;
                    let bind_ty = self.insert_bare(lit, loc);
                    self.unify(match_cond, bind_ty)?;
                }
                Pattern::Ident(var) => {
                    self.insert(Var::User(*var), &self.reconstruct(match_cond)?, loc)?;
                }
                Pattern::ItemPath(..) => todo!(),
                // TODO: Is this correct?
                Pattern::Wildcard => {}
            }

            let arm_ty = self.intern_type(&*ty, loc)?;
            let arm_ty = self.insert_bare(arm_ty, cond.location());

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

        let match_ty = self.intern_type(&*ty, loc)?;
        let match_ty = self.insert_bare(match_ty, loc);
        for arm in arm_types {
            self.unify(match_ty, arm)?;
        }

        *ty = self.reconstruct(match_ty)?;

        Ok(match_ty)
    }

    fn visit_variable(&mut self, loc: Location, var: Var, _ty: &mut TypeKind) -> Self::Output {
        self.get(&var, loc)
    }

    fn visit_literal(&mut self, loc: Location, literal: &mut Literal) -> Self::Output {
        let info = self.intern_literal(&*literal, loc)?;
        let id = self.insert_bare(info, loc);

        Ok(id)
    }

    fn visit_scope(&mut self, _loc: Location, _body: &mut Block<Stmt>) -> Self::Output {
        todo!()
    }

    fn visit_func_call(&mut self, loc: Location, call: &mut FuncCall) -> Self::Output {
        let func = self
            .functions
            .get(&call.func)
            .ok_or_else(|| {
                Locatable::new(
                    TypeError::FuncNotInScope(call.func.to_string(&self.interner)).into(),
                    loc,
                )
            })?
            .clone();

        let args = call
            .args
            .iter_mut()
            .map(|e| self.visit_expr(e))
            .collect::<TypeResult<Vec<TypeId>>>()?;

        if func.args.len() != args.len() {
            let def_site = if func.args.is_empty() {
                func.args.location()
            } else {
                // If there's 1 or more args trim off the parentheses
                func.args
                    .location()
                    .map_span(|span| Span::new(span.start() + 1, span.end() - 1))
            };

            return Err(Locatable::new(
                TypeError::NotEnoughArgs {
                    expected: func.args.len(),
                    received: args.len(),
                    def_site,
                }
                .into(),
                loc,
            ));
        }

        for (expected, given) in func.args.iter().zip(args.iter()) {
            self.unify(*given, *expected)?;
        }

        Ok(*func.ret)
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

    // TODO: This is sketchy and doesn't check if they're bin-op-able
    fn visit_binop(
        &mut self,
        _loc: Location,
        lhs: &mut Expr,
        _op: BinaryOp,
        rhs: &mut Expr,
    ) -> Self::Output {
        let lhs = self.visit_expr(lhs)?;
        let rhs = self.visit_expr(rhs)?;
        self.unify(lhs, rhs)?;

        Ok(lhs)
    }

    fn visit_cast(&mut self, loc: Location, Cast { casted, ty }: &mut Cast) -> Self::Output {
        let _casted = self.visit_expr(casted)?;
        let ty = self.intern_type(&ty.kind, loc)?;

        Ok(self.insert_bare(ty, loc))
    }

    fn visit_reference(
        &mut self,
        loc: Location,
        Reference { mutable, reference }: &mut Reference,
    ) -> Self::Output {
        let expr = self.visit_expr(reference)?;

        Ok(self.insert_bare(TypeInfo::Reference(*mutable, expr), loc))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum TypeInfo {
    Ref(TypeId),
    Infer,
    Integer {
        /// Will be `None` for unknown
        sign: Option<Signedness>,
        /// Will be `None` for unknown
        width: Option<u16>,
    },
    String,
    Bool,
    Unit,
    Absurd,
    Pointer(TypeId),
    Array(TypeId, u32),
    Slice(TypeId),
    Reference(bool, TypeId),
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
                            .type_of(&Var::User(ctx.strings.intern("greeting")),)
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
