#![warn(
    missing_copy_implementations,
    missing_debug_implementations,
    clippy::dbg_macro,
    clippy::missing_safety_doc,
    clippy::wildcard_imports,
    clippy::shadow_unrelated
)]

use crunch_shared::{
    trees::hir::{
        Expr, Function, Hir, ItemPath, Literal, Match, MatchArm, Stmt, Type, TypeKind, VarDecl,
    },
    utils::HashMap,
};

type TypeId = usize;

#[derive(Debug, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
enum TypeInfo {
    Ref(TypeId),
    Infer,
    Integer,
    String,
    Bool,
    Unit,
}

impl From<TypeKind> for TypeInfo {
    fn from(kind: TypeKind) -> Self {
        match kind {
            TypeKind::Infer => TypeInfo::Infer,
            TypeKind::Integer => TypeInfo::Integer,
            TypeKind::String => TypeInfo::String,
            TypeKind::Bool => TypeInfo::Bool,
            TypeKind::Unit => TypeInfo::Unit,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
struct Engine {
    id_counter: TypeId,
    types: HashMap<TypeId, TypeInfo>,
    ids: HashMap<ItemPath, TypeId>,
}

impl Engine {
    /// Create a new type term with whatever we have about its type
    pub fn insert(&mut self, info: &Type) -> TypeId {
        if let Some(&id) = self.ids.get(&info.name) {
            self.types.insert(id, info.kind.into());

            id
        } else {
            let id = self.id_counter;
            self.id_counter += 1;

            self.types.insert(id, info.kind.into());
            self.ids.insert(info.name.clone(), id);

            id
        }
    }

    pub fn get(&self, path: &ItemPath) -> Result<TypeId, String> {
        self.ids
            .get(path)
            .copied()
            .ok_or_else(|| "No variable by that name".to_string())
    }

    pub fn insert_bare(&mut self, info: TypeInfo) -> TypeId {
        let id = self.id_counter;
        self.id_counter += 1;

        self.types.insert(id, info);

        id
    }

    /// Make the types of two type terms equivalent (or produce an error if
    /// there is a conflict between them)
    pub fn unify(&mut self, a: TypeId, b: TypeId) -> Result<(), String> {
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
    pub fn reconstruct(&self, id: TypeId) -> Result<TypeKind, String> {
        match self.types[&id].clone() {
            TypeInfo::Infer => Err(format!("Cannot infer")),
            TypeInfo::Ref(id) => self.reconstruct(id),
            TypeInfo::Integer => Ok(TypeKind::Integer),
            TypeInfo::Bool => Ok(TypeKind::Bool),
            TypeInfo::Unit => Ok(TypeKind::Unit),
            TypeInfo::String => Ok(TypeKind::String),
        }
    }

    pub fn walk(&mut self, hir: &mut [Hir]) -> Result<(), String> {
        for node in hir {
            match node {
                Hir::Function(func) => self.func(func)?,
            }
        }

        Ok(())
    }

    pub fn func(&mut self, func: &mut Function) -> Result<(), String> {
        let params: Vec<_> = func.args.iter().map(|param| self.insert(param)).collect();

        let mut ty = self.insert_bare(TypeInfo::Infer);
        for stmt in func.body.iter_mut() {
            ty = self.stmt(stmt)?;
        }

        let ret = self.insert(&ladder::TypeInfo {
            name: ItemPath::default(),
            kind: func.return_ty,
        });
        self.unify(ty, ret)?;
        func.return_ty = self.reconstruct(ty)?;

        for (i, param) in params.into_iter().enumerate() {
            func.params[i].kind = self.reconstruct(param)?;
        }

        Ok(())
    }

    pub fn stmt(&mut self, stmt: &mut Stmt) -> Result<TypeId, String> {
        Ok(match stmt {
            Stmt::VarDecl(VarDecl { name: _, value, ty }) => {
                let var = self.insert(ty);
                let expr = self.expr(value)?;

                self.unify(var, expr)?;
                ty.kind = self.reconstruct(var)?;

                self.insert_bare(TypeInfo::Unit)
            }
            Stmt::Expr(expr) => self.expr(expr)?,
            Stmt::Match(Match {
                condition,
                arms,
                ty,
            }) => {
                let cond = self.expr(condition)?;

                let mut arm_types = Vec::new();
                for MatchArm {
                    condition,
                    body,
                    ty,
                } in arms.iter_mut()
                {
                    let arm_ty = self.insert_bare(TypeInfo::from(*ty));
                    let arm_cond = self.expr(condition)?;
                    self.unify(cond, arm_cond)?;

                    let arm_ret = body
                        .iter_mut()
                        .map(|s| self.stmt(s))
                        .collect::<Result<Vec<TypeId>, String>>()?
                        .get(0)
                        .copied()
                        .unwrap_or_else(|| self.insert_bare(TypeInfo::Unit));

                    self.unify(arm_ty, arm_ret)?;
                    *ty = self.reconstruct(arm_ty)?;

                    arm_types.push(arm_ty);
                }

                let match_ty = self.insert_bare(TypeInfo::from(*ty));
                for arm in arm_types {
                    self.unify(match_ty, arm)?;
                }
                *ty = self.reconstruct(match_ty)?;

                match_ty
            }

            _ => todo!(),
        })
    }

    pub fn expr(&mut self, expr: &mut Expr) -> Result<TypeId, String> {
        Ok(match expr {
            Expr::Literal(lit) => match lit {
                Literal::Integer(..) => self.insert_bare(TypeInfo::Integer),
                Literal::Bool(..) => self.insert_bare(TypeInfo::Bool),
                Literal::String(..) => self.insert_bare(TypeInfo::String),

                _ => todo!(),
            },
            Expr::Var(path) => self.get(path)?,
            Expr::Comparison(left, _op, right) => {
                let (left, right) = (self.expr(left)?, self.expr(right)?);
                self.unify(left, right)?;

                self.insert_bare(TypeInfo::Bool)
            }

            _ => todo!(),
        })
    }

    pub fn type_of(&self, var: &ItemPath) -> Option<TypeKind> {
        self.ids.get(var).map(|&id| self.reconstruct(id).unwrap())
    }
}

#[test]
fn test() {
    use crunch_parser::{Context, CurrentFile, Parser};
    use crunch_shared::files::FileId;

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
    let mut files = crunch_shared::files::Files::new();
    files.add("<test>", source);

    match Parser::new(source, CurrentFile::new(FileId::new(0), source.len()), &ctx).parse() {
        Ok((ast, mut warnings, module_table, module_scope)) => {
            warnings.emit(&files);

            println!("Nodes: {:#?}", &ast);
            println!("Symbols: {:#?}", &module_scope);

            let ladder = Ladder::new(
                module_table,
                module_scope,
                ItemPath::new(ctx.intern("package")),
            );

            let mut hir = ladder.lower(&ast);
            println!("HIR: {:#?}", hir);

            let mut engine = Engine::default();
            println!("{:?}", engine.walk(&mut hir));

            println!("HIR: {:#?}", hir);
            println!(
                "Type of `greeting`: {:?}",
                engine
                    .type_of(&ItemPath::new(vec![ctx.intern("greeting")]))
                    .unwrap(),
            );
        }

        Err(mut err) => {
            err.emit(&files);
        }
    }
}
