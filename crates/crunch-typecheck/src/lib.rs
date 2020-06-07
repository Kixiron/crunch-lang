#![warn(
    missing_copy_implementations,
    missing_debug_implementations,
    clippy::dbg_macro,
    clippy::missing_safety_doc,
    clippy::wildcard_imports,
    clippy::shadow_unrelated
)]

use crunch_shared::{
    strings::StrT,
    trees::hir::{
        Block, Break, CompOp, Expr, FuncArg, FuncCall, Function, Item, ItemPath, Literal, Match,
        MatchArm, Return, Stmt, TypeKind, VarDecl,
    },
    utils::HashMap,
    visitors::hir::{ExprVisitor, ItemVisitor, StmtVisitor},
};

type TypeId = usize;

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Engine {
    id_counter: TypeId,
    types: HashMap<TypeId, TypeInfo>,
    ids: HashMap<ItemPath, TypeId>,
}

impl Engine {
    /// Create a new type term with whatever we have about its type
    fn insert(&mut self, variable: &ItemPath, kind: &TypeKind) -> TypeId {
        if let Some(&id) = self.ids.get(&variable) {
            self.types.insert(id, kind.into());

            id
        } else {
            let id = self.id_counter;
            self.id_counter += 1;

            self.types.insert(id, kind.into());
            self.ids.insert(variable.clone(), id);

            id
        }
    }

    fn get(&self, path: &ItemPath) -> Result<TypeId, String> {
        self.ids
            .get(path)
            .copied()
            .ok_or_else(|| "No variable by that name".to_string())
    }

    fn insert_bare(&mut self, info: TypeInfo) -> TypeId {
        let id = self.id_counter;
        self.id_counter += 1;

        self.types.insert(id, info);

        id
    }

    /// Make the types of two type terms equivalent (or produce an error if
    /// there is a conflict between them)
    fn unify(&mut self, a: TypeId, b: TypeId) -> Result<(), String> {
        match (self.types[&a].clone(), self.types[&b].clone()) {
            // Follow any references
            (TypeInfo::Ref(a), _) => self.unify(a, b),
            (_, TypeInfo::Ref(b)) => self.unify(a, b),

            // When we don't know anything about either term, assume that
            // they match and make the one we know nothing about reference the
            // one we may know something about
            (TypeInfo::Infer, _) => {
                self.types.insert(a, TypeInfo::Ref(b));

                Ok(())
            }
            (_, TypeInfo::Infer) => {
                self.types.insert(b, TypeInfo::Ref(a));

                Ok(())
            }

            // Primitives are trivial to unify
            (TypeInfo::Integer, TypeInfo::Integer)
            | (TypeInfo::String, TypeInfo::String)
            | (TypeInfo::Bool, TypeInfo::Bool)
            | (TypeInfo::Unit, TypeInfo::Unit) => Ok(()),

            // If no previous attempts to unify were successful, raise an error
            (a, b) => Err(format!("Conflict between {:?} and {:?}", a, b)),
        }
    }

    /// Attempt to reconstruct a concrete type from the given type term ID. This
    /// may fail if we don't yet have enough information to figure out what the
    /// type is.
    fn reconstruct(&self, id: TypeId) -> Result<TypeKind, String> {
        match self.types[&id].clone() {
            TypeInfo::Infer => Err(format!("Cannot infer")),
            TypeInfo::Ref(id) => self.reconstruct(id),
            TypeInfo::Integer => Ok(TypeKind::Integer),
            TypeInfo::Bool => Ok(TypeKind::Bool),
            TypeInfo::Unit => Ok(TypeKind::Unit),
            TypeInfo::String => Ok(TypeKind::String),
        }
    }

    pub fn walk(&mut self, hir: &mut [Item]) -> Result<(), String> {
        for node in hir {
            match node {
                Item::Function(func) => self.visit_func(func)?,
            }
        }

        Ok(())
    }

    pub fn type_of(&self, var: &ItemPath) -> Option<TypeKind> {
        self.ids.get(var).map(|&id| self.reconstruct(id).unwrap())
    }
}

impl ItemVisitor for Engine {
    type Output = Result<(), String>;

    fn visit_func(
        &mut self,
        Function {
            args, body, ret, ..
        }: &mut Function,
    ) -> Self::Output {
        let func_args: Vec<_> = args
            .iter()
            .map(|FuncArg { name, kind }| self.insert(name, kind))
            .collect();

        let mut ty = self.insert_bare(TypeInfo::Infer);
        for stmt in body.iter_mut() {
            ty = self.visit_stmt(stmt)?;
        }

        let ret_type = self.insert(&ItemPath::default(), &ret);
        self.unify(ty, ret_type)?;
        *ret = self.reconstruct(ty)?;

        for (i, arg) in func_args.into_iter().enumerate() {
            args[i].kind = self.reconstruct(arg)?;
        }

        Ok(())
    }
}

impl StmtVisitor for Engine {
    type Output = Result<TypeId, String>;

    #[inline]
    fn visit_stmt(&mut self, stmt: &mut Stmt) -> <Self as StmtVisitor>::Output {
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
        VarDecl { name, value, ty }: &mut VarDecl,
    ) -> <Self as StmtVisitor>::Output {
        let var = self.insert(name, &ty.kind);
        let expr = self.visit_expr(value)?;

        self.unify(var, expr)?;
        ty.kind = self.reconstruct(var)?;

        Ok(self.insert_bare(TypeInfo::Unit))
    }
}

impl ExprVisitor for Engine {
    type Output = Result<TypeId, String>;

    fn visit_return(&mut self, _value: &mut Return) -> Self::Output {
        todo!()
    }

    fn visit_break(&mut self, _value: &mut Break) -> Self::Output {
        todo!()
    }

    fn visit_continue(&mut self) -> Self::Output {
        todo!()
    }

    fn visit_loop(&mut self, _body: &mut Block<Stmt>) -> Self::Output {
        todo!()
    }

    fn visit_match(&mut self, Match { cond, arms, ty }: &mut Match) -> Self::Output {
        let match_cond = self.visit_expr(cond)?;

        let mut arm_types = Vec::new();
        for MatchArm { cond, body, ty } in arms.iter_mut() {
            let arm_ty = self.insert_bare(TypeInfo::from(&*ty));
            let arm_cond = self.visit_expr(cond)?;
            self.unify(match_cond, arm_cond)?;

            let arm_ret = body
                .iter_mut()
                .map(|s| self.visit_stmt(s))
                .collect::<Result<Vec<TypeId>, String>>()?
                .get(0)
                .copied()
                .unwrap_or_else(|| self.insert_bare(TypeInfo::Unit));

            self.unify(arm_ty, arm_ret)?;
            *ty = self.reconstruct(arm_ty)?;

            arm_types.push(arm_ty);
        }

        let match_ty = self.insert_bare(TypeInfo::from(&*ty));
        for arm in arm_types {
            self.unify(match_ty, arm)?;
        }
        *ty = self.reconstruct(match_ty)?;

        Ok(match_ty)
    }

    fn visit_variable(&mut self, var: StrT, _ty: &mut TypeKind) -> Self::Output {
        self.get(&ItemPath::new(var))
    }

    fn visit_literal(&mut self, literal: &mut Literal) -> Self::Output {
        let info = TypeInfo::from(&*literal);
        let id = self.insert_bare(info);

        Ok(id)
    }

    fn visit_scope(&mut self, _body: &mut Block<Stmt>) -> Self::Output {
        todo!()
    }

    fn visit_func_call(&mut self, _call: &mut FuncCall) -> Self::Output {
        todo!()
    }

    fn visit_comparison(&mut self, lhs: &mut Expr, _op: CompOp, rhs: &mut Expr) -> Self::Output {
        let (left, right) = (self.visit_expr(lhs)?, self.visit_expr(rhs)?);
        self.unify(left, right)?;

        Ok(self.insert_bare(TypeInfo::Bool))
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
        Ok((ast, mut warnings, module_table, module_scope)) => {
            warnings.emit(&files);

            println!("Nodes: {:#?}", &ast);
            println!("Symbols: {:#?}", &module_scope);

            let mut ladder = Ladder::new(
                module_table,
                module_scope,
                ItemPath::new(ctx.strings.intern("package")),
            );

            let mut hir = ladder.lower(&ast);
            println!("HIR: {:#?}", hir);

            let mut engine = Engine::default();
            println!("{:?}", engine.walk(&mut hir));

            println!("HIR: {:#?}", hir);
            println!(
                "Type of `greeting`: {:?}",
                engine
                    .type_of(&ItemPath::new(vec![ctx.strings.intern("greeting")]))
                    .unwrap(),
            );
        }

        Err(mut err) => {
            err.emit(&files);
        }
    }
}
