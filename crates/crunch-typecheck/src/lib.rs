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
            Item, Literal, LiteralVal, Match, Pattern, Reference, Return, Stmt, Type, TypeKind,
            Var, VarDecl,
        },
        ItemPath, Ref,
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
    fn insert(&mut self, variable: Var, ty: &Type) -> TypeResult<TypeId> {
        let location = ty.location();
        let info = self.intern_type(ty)?;

        if let Some(&id) = self.ids.get(&variable) {
            self.types.insert(id, (info, location));

            Ok(id)
        } else {
            let id = self.id_counter;
            self.id_counter += 1;

            self.types.insert(id, (info, location));
            self.ids.insert(variable, id);

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
        let ((lhs, lhs_loc), (rhs, rhs_loc)) = (self.types[&a].clone(), self.types[&b].clone());

        match (lhs, rhs) {
            (TypeInfo::Ref(a), _) => self.unify(a, b),
            (_, TypeInfo::Ref(b)) => self.unify(a, b),

            (TypeInfo::Unknown, _) => {
                self.types.insert(a, (TypeInfo::Ref(b), lhs_loc));

                Ok(())
            }
            (_, TypeInfo::Unknown) => {
                self.types.insert(b, (TypeInfo::Ref(a), rhs_loc));

                Ok(())
            }

            (TypeInfo::Absurd, _) | (_, TypeInfo::Absurd) => Ok(()),
            (TypeInfo::String, TypeInfo::String)
            | (TypeInfo::Bool, TypeInfo::Bool)
            | (TypeInfo::Unit, TypeInfo::Unit) => Ok(()),

            (
                TypeInfo::Integer {
                    signed: Some(left_sign),
                    width: Some(left_width),
                },
                TypeInfo::Integer {
                    signed: Some(right_sign),
                    width: Some(right_width),
                },
            ) if left_sign == right_sign && left_width == right_width => Ok(()),

            (
                TypeInfo::Integer {
                    signed: None,
                    width: None,
                },
                TypeInfo::Integer {
                    signed: Some(..),
                    width: Some(..),
                },
            ) => {
                self.types.insert(a, (TypeInfo::Ref(b), rhs_loc));

                Ok(())
            }
            (
                TypeInfo::Integer {
                    signed: Some(..),
                    width: Some(..),
                },
                TypeInfo::Integer {
                    signed: None,
                    width: None,
                },
            ) => {
                self.types.insert(b, (TypeInfo::Ref(a), lhs_loc));

                Ok(())
            }
            (
                TypeInfo::Integer {
                    signed: None,
                    width: None,
                },
                TypeInfo::Integer {
                    signed: None,
                    width: None,
                },
            ) => {
                self.types.insert(b, (TypeInfo::Ref(a), lhs_loc));

                Ok(())
            }

            (
                TypeInfo::Array {
                    element: left_elem,
                    length: left_len,
                },
                TypeInfo::Array {
                    element: right_elem,
                    length: right_len,
                },
            ) if left_len == right_len => {
                self.unify(left_elem, right_elem)?;

                Ok(())
            }

            (
                TypeInfo::Reference {
                    referee: left,
                    mutable: left_mut,
                },
                TypeInfo::Reference {
                    referee: right,
                    mutable: right_mut,
                },
            )
            | (
                TypeInfo::Pointer {
                    pointee: left,
                    mutable: left_mut,
                },
                TypeInfo::Pointer {
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
                    def_site: rhs_loc,
                }
                .into(),
                lhs_loc,
            )),
        }
    }

    /// Attempt to reconstruct a concrete type from the given type term ID. This
    /// may fail if we don't yet have enough information to figure out what the
    /// type is.
    fn reconstruct(&self, id: TypeId) -> TypeResult<Type> {
        let &(ref info, loc) = &self.types[&id];

        let kind = match info {
            TypeInfo::Unknown => {
                return Err(Locatable::new(
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
                            .unwrap_or("{{unknown}}".to_owned()),
                    )
                    .into(),
                    loc,
                ));
            }
            &TypeInfo::Ref(id) => return self.reconstruct(id),
            &TypeInfo::Integer { signed, width } => TypeKind::Integer { signed, width },
            TypeInfo::Bool => TypeKind::Bool,
            TypeInfo::Unit => TypeKind::Unit,
            TypeInfo::String => TypeKind::String,
            TypeInfo::Absurd => TypeKind::Absurd,
            &TypeInfo::Pointer { pointee, mutable } => TypeKind::Pointer {
                pointee: Ref::new(self.reconstruct(pointee)?),
                mutable,
            },
            &TypeInfo::Array { element, length } => TypeKind::Array {
                element: Ref::new(self.reconstruct(element)?),
                length,
            },
            &TypeInfo::Slice { element } => TypeKind::Slice {
                element: Ref::new(self.reconstruct(element)?),
            },
            &TypeInfo::Reference { referee, mutable } => TypeKind::Reference {
                referee: Ref::new(self.reconstruct(referee)?),
                mutable,
            },
        };

        Ok(Type { kind, loc })
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
                for arg in args.iter().filter(|a| a.kind.is_unknown()) {
                    self.errors.push_err(Locatable::new(
                        TypeError::MissingType("Types for function arguments".to_owned()).into(),
                        arg.location(),
                    ));
                }

                // TODO: Use error types here
                if ret.kind.is_unknown() {
                    self.errors.push_err(Locatable::new(
                        TypeError::MissingType("Return types for functions".to_owned()).into(),
                        ret.location(),
                    ));
                }

                // TODO: Use error types here
                let ret_type = match self.intern_type(&ret) {
                    Ok(ty) => ty,
                    Err(err) => {
                        self.errors.push_err(err);
                        todo!()
                    }
                };

                // TODO: Use error types here
                let func_args = args
                    .iter()
                    .map(|FuncArg { name, kind, .. }| self.insert(*name, kind))
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

    pub fn type_of(&self, var: &Var) -> Option<Type> {
        self.ids.get(var).and_then(|id| self.reconstruct(*id).ok())
    }

    fn intern_type(&mut self, ty: &Type) -> TypeResult<TypeInfo> {
        let ty = match &ty.kind {
            TypeKind::Unknown => TypeInfo::Unknown,
            &TypeKind::Integer { signed, width } => TypeInfo::Integer { signed, width },
            TypeKind::String => TypeInfo::String,
            TypeKind::Bool => TypeInfo::Bool,
            TypeKind::Unit => TypeInfo::Unit,
            TypeKind::Absurd => TypeInfo::Absurd,

            &TypeKind::Pointer {
                ref pointee,
                mutable,
            } => {
                if pointee.is_unknown() {
                    return Err(Locatable::new(
                        TypeError::MissingType("pointer types".to_owned()).into(),
                        ty.location(),
                    ));
                } else {
                    let pointee = self.intern_type(pointee.as_ref())?;

                    TypeInfo::Pointer {
                        pointee: self.insert_bare(pointee, ty.location()),
                        mutable,
                    }
                }
            }

            &TypeKind::Array {
                ref element,
                length,
            } => {
                let elem = self.intern_type(element.as_ref())?;

                TypeInfo::Array {
                    element: self.insert_bare(elem, ty.location()),
                    length,
                }
            }

            TypeKind::Slice { element } => {
                let element = self.intern_type(element.as_ref())?;

                TypeInfo::Slice {
                    element: self.insert_bare(element, ty.location()),
                }
            }

            TypeKind::Reference { referee, mutable } => {
                let referee = self.intern_type(referee.as_ref())?;

                TypeInfo::Reference {
                    referee: self.insert_bare(referee, ty.location()),
                    mutable: *mutable,
                }
            }
        };

        Ok(ty)
    }

    fn intern_literal(
        &mut self,
        Literal { val, ty, loc }: &mut Literal,
        _loc: Location,
    ) -> TypeResult<TypeId> {
        let info = self.intern_type(ty)?;
        let info = self.insert_bare(info, *loc);

        match val {
            LiteralVal::Integer(_) => {
                let int = self.insert_bare(
                    TypeInfo::Integer {
                        signed: None,
                        width: None,
                    },
                    *loc,
                );
                self.unify(int, info)?;
            }

            LiteralVal::Bool(_) => {
                let boolean = self.insert_bare(TypeInfo::Bool, *loc);
                self.unify(boolean, info)?;
            }

            LiteralVal::String(_) => {
                let string = self.insert_bare(TypeInfo::String, *loc);
                self.unify(string, info)?;
            }

            LiteralVal::Array { elements } => {
                let element = self.insert_bare(TypeInfo::Unknown, *loc);

                self.insert_bare(
                    TypeInfo::Array {
                        element,
                        length: elements.len() as u64,
                    },
                    *loc,
                );

                for elem in elements {
                    let elem = self.intern_literal(elem, elem.location())?;
                    self.unify(element, elem)?;
                }
            }

            lit => todo!("{:?}", lit),
        }

        // TODO: Check inner types of stuff
        Ok(info)
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
            TypeInfo::Unknown => f.write_str("infer"),
            &TypeInfo::Integer { signed, width } => match (signed, width) {
                (Some(signed), Some(width)) => {
                    write!(f, "{}{}", if signed { "i" } else { "u" }, width)
                }
                (_, _) => f.write_str("{{integer}}"),
            },
            TypeInfo::String => f.write_str("str"),
            TypeInfo::Bool => f.write_str("bool"),
            TypeInfo::Unit => f.write_str("unit"),
            TypeInfo::Absurd => f.write_str("absurd"),

            TypeInfo::Array { element, length } => {
                f.write_str("arr[")?;
                self.display_type_inner(
                    &self
                        .types
                        .get(element)
                        .expect("Tried to display type that doesn't exist")
                        .0,
                    f,
                )?;
                f.write_str("; ")?;
                write!(f, "{}", length)?;
                f.write_char(']')
            }

            TypeInfo::Slice { element } => {
                f.write_str("arr[")?;
                self.display_type_inner(
                    &self
                        .types
                        .get(element)
                        .expect("Tried to display type that doesn't exist")
                        .0,
                    f,
                )?;
                f.write_char(']')
            }

            &TypeInfo::Reference { mutable, referee } => {
                f.write_char('&')?;
                if mutable {
                    f.write_str("mut ")?;
                }

                self.display_type_inner(
                    &self
                        .types
                        .get(&referee)
                        .expect("Tried to display type that doesn't exist")
                        .0,
                    f,
                )
            }

            &TypeInfo::Pointer { mutable, pointee } => {
                if mutable {
                    f.write_str("*const ")?;
                } else {
                    f.write_str("*mut ")?;
                }

                self.display_type_inner(
                    &self
                        .types
                        .get(&pointee)
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
        for arg in args.iter().filter(|a| a.kind.is_unknown()) {
            if let Some(err) = missing_arg_ty.take() {
                self.errors.push_err(err);
            }

            missing_arg_ty = Some(Locatable::new(
                TypeError::MissingType("Types for external function arguments".to_owned()).into(),
                arg.loc,
            ));
        }

        if ret.kind.is_unknown() {
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
        let var = self.insert(*name, ty)?;
        let expr = self.visit_expr(value)?;
        self.unify(expr, var)?;

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
        let mut arm_types = Vec::with_capacity(arms.len());
        let condition_type = self.visit_expr(cond)?;

        for arm in arms.iter_mut() {
            match &mut arm.bind.pattern {
                Pattern::Literal(literal) => {
                    let literal_type = self.visit_literal(loc, literal)?;
                    self.unify(condition_type, literal_type)?;
                }

                &mut Pattern::Ident(variable) => {
                    let variable_type = self.insert_bare(TypeInfo::Ref(condition_type), loc);
                    self.ids.insert(Var::User(variable), variable_type);
                    self.unify(condition_type, variable_type)?;
                }

                // TODO: Is this correct?
                Pattern::Wildcard => {
                    crunch_shared::warn!("Match pattern wildcards are currently ignored");
                }
                Pattern::ItemPath(..) => todo!(),
            }

            if let Some(guard) = &mut arm.guard {
                let guard_ty = self.visit_expr(guard)?;
                let boolean = self.insert_bare(TypeInfo::Bool, guard.location());

                self.unify(guard_ty, boolean)?;
            }

            let arm_type = arm
                .body
                .iter_mut()
                .map(|s| self.visit_stmt(s))
                .collect::<TypeResult<Vec<TypeId>>>()?
                .last()
                .copied()
                .unwrap_or_else(|| self.insert_bare(TypeInfo::Unit, arm.body.location()));

            arm_types.push(arm_type);
        }

        let match_ty = self.intern_type(ty)?;
        let match_ty = self.insert_bare(match_ty, loc);
        for arm in arm_types {
            self.unify(match_ty, arm)?;
        }

        Ok(match_ty)
    }

    fn visit_variable(&mut self, loc: Location, var: Var, _ty: &mut Type) -> Self::Output {
        self.get(&var, loc)
    }

    fn visit_literal(&mut self, loc: Location, literal: &mut Literal) -> Self::Output {
        self.intern_literal(&mut *literal, loc)
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
        let ty = self.intern_type(ty)?;

        Ok(self.insert_bare(ty, loc))
    }

    fn visit_reference(
        &mut self,
        loc: Location,
        &mut Reference {
            mutable,
            ref mut reference,
        }: &mut Reference,
    ) -> Self::Output {
        let referee = self.visit_expr(reference)?;

        Ok(self.insert_bare(TypeInfo::Reference { referee, mutable }, loc))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum TypeInfo {
    Ref(TypeId),
    Unknown,
    Integer {
        signed: Option<bool>,
        width: Option<u16>,
    },
    String,
    Bool,
    Unit,
    Absurd,
    Pointer {
        pointee: TypeId,
        mutable: bool,
    },
    Array {
        element: TypeId,
        length: u64,
    },
    Slice {
        element: TypeId,
    },
    Reference {
        referee: TypeId,
        mutable: bool,
    },
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
