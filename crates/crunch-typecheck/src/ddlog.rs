use super::TypecheckDatabase;
use core::hash::Hash;
use crunch_shared::{
    tracing,
    trees::hir::{
        Expr as HirExpr, ExprKind as HirExprKind, FuncArg as HirFuncArg, Function as HirFunction,
        Integer, Item as HirItem, ItemPath as HirItemPath, Literal as HirLiteral,
        LiteralVal as HirLiteralVal, Stmt as HirStmt, Type as HirType, TypeId,
        TypeKind as HirTypeKind, Var as HirVar, VarDecl as HirVarDecl, Vis as HirVis,
    },
    utils::HashMap,
};
use ddlog_types::{
    hir_ExprId as ExprId, hir_ExprKind as ExprKind, hir_FuncArg as FuncArg,
    hir_Function as Function, hir_Item as Item, hir_ItemPath as ItemPath, hir_Literal as Literal,
    hir_Stmt as Stmt, hir_TypeKind as TypeKind, hir_VarDecl as VarDecl, hir_Vis as Vis,
    internment_Intern as Interned, internment_intern as ddlog_intern, option2std,
    std_Vec as Vector, Expr, InputItems,
};
use ddlog_values::{relid2name, Relations, Value};
use differential_datalog::{
    ddval::{DDValConvert, DDValue},
    program::{RelId, Update},
    record::Record,
    DDlog, DeltaMap,
};
use typecheck_ddlog::api::HDDlog;

trait Visit<T> {
    type Output;

    fn visit(&mut self, data: &T) -> Self::Output;
}

pub struct DDlogEngine<'ctx> {
    variable_id: u64,
    variables: HashMap<HirVar, u64>,
    expression_id: u64,
    expressions: Vec<Expr>,
    db: &'ctx dyn TypecheckDatabase,
}

impl<'ctx> DDlogEngine<'ctx> {
    pub fn new(db: &'ctx dyn TypecheckDatabase) -> Self {
        crunch_shared::trace!("creating a new ddlog type engine");

        Self {
            variable_id: 0,
            variables: HashMap::default(),
            expression_id: 0,
            expressions: Vec::new(),
            db,
        }
    }

    #[crunch_shared::instrument(name = "type checking", skip(self, program, items))]
    pub fn walk(&mut self, program: &mut HDDlog, items: &[&HirItem<'_>]) -> Result<(), String> {
        crunch_shared::trace!("starting transaction");
        program.transaction_start()?;

        program.apply_valupdates(items.iter().copied().map(|item| {
            Update::Insert {
                relid: Relations::InputItems as RelId,
                v: Value::InputItems(InputItems {
                    item: self.visit(item),
                })
                .into_ddvalue(),
            }
        }))?;
        program.apply_valupdates(self.expressions.drain(..).map(|expr| Update::Insert {
            relid: Relations::Expr as RelId,
            v: Value::Expr(expr).into_ddvalue(),
        }))?;

        crunch_shared::trace!("committing transaction");
        let delta = program.transaction_commit_dump_changes()?;
        dump_delta(&delta);

        Ok(())
    }

    fn next_expression(&mut self) -> u64 {
        let expr = self.expression_id;
        self.expression_id += 1;
        expr
    }

    fn next_variable(&mut self) -> u64 {
        let variable = self.variable_id;
        self.variable_id += 1;
        variable
    }

    fn get_or_create_var(&mut self, var: HirVar) -> u64 {
        if let Some(&var) = self.variables.get(&var) {
            var
        } else {
            let id = self.next_variable();
            self.variables.insert(var, id);

            id
        }
    }

    fn intern<T>(&mut self, data: T) -> Interned<T>
    where
        T: Eq + Hash + Send + Sync + Clone + 'static,
    {
        ddlog_intern(&data)
    }
}

impl<'ctx> Visit<HirItem<'_>> for DDlogEngine<'ctx> {
    type Output = Item;

    #[crunch_shared::instrument(name = "item", skip(self, item))]
    fn visit(&mut self, item: &HirItem<'_>) -> Self::Output {
        match item {
            HirItem::Function(function) => {
                crunch_shared::trace!("item is a function, visiting");
                Item::hir_ItemFunc {
                    func: self.visit(function),
                }
            }

            HirItem::ExternFunc(_external_function) => {
                crunch_shared::trace!("item is an external function, visiting");
                // self.visit(external_function)
                todo!()
            }
        }
    }
}

impl<'ctx> Visit<HirFunction<'_>> for DDlogEngine<'ctx> {
    type Output = Function;

    #[crunch_shared::instrument(
        name = "function",
        skip(self, function),
        fields(name = ?function.name.to_string(self.db.context().strings())),
    )]
    fn visit(&mut self, function: &HirFunction<'_>) -> Self::Output {
        crunch_shared::trace!("visiting {} function arguments", function.args.len());
        let args = function.args.iter().map(|arg| self.visit(arg)).collect();

        crunch_shared::trace!("visiting {} function body statements", function.body.len());
        let body = function
            .body
            .iter()
            .copied()
            .rev()
            .fold(None, |prev, next| {
                let next = {
                    let next = self.visit(next);
                    self.intern(next)
                };
                match prev {
                    Some(prev) => Some(self.intern(Stmt::hir_StmtSeq {
                        first: prev,
                        second: next,
                    })),
                    None => Some(next),
                }
            })
            .unwrap_or_else(|| self.intern(Stmt::hir_Empty));

        Function {
            name: self.visit(&function.name),
            vis: self.visit(&function.vis),
            args: Vector { x: args },
            body,
            ret: {
                let ret = self.visit(&function.ret);
                self.intern(ret)
            },
        }
    }
}

impl<'ctx> Visit<HirFuncArg> for DDlogEngine<'ctx> {
    type Output = FuncArg;

    #[crunch_shared::instrument(
        name = "function argument",
        skip(self, arg),
        fields(name = ?arg.name.to_string(self.db.context().strings())),
    )]
    fn visit(&mut self, arg: &HirFuncArg) -> Self::Output {
        FuncArg {
            name: self.next_variable(),
            kind: {
                let kind = self.visit(&arg.kind);
                self.intern(kind)
            },
        }
    }
}

impl<'ctx> Visit<TypeId> for DDlogEngine<'ctx> {
    type Output = TypeKind;

    #[crunch_shared::instrument(name = "type id", skip(self, id))]
    fn visit(&mut self, id: &TypeId) -> Self::Output {
        let ty = self
            .db
            .context()
            .get_hir_type(*id)
            .expect("got a hir type id that doesn't exist");

        self.visit(ty)
    }
}

impl<'ctx> Visit<HirType> for DDlogEngine<'ctx> {
    type Output = TypeKind;

    #[crunch_shared::instrument(name = "type", skip(self, ty))]
    fn visit(&mut self, ty: &HirType) -> Self::Output {
        match ty.kind {
            HirTypeKind::Unknown => TypeKind::hir_Unknown,
            HirTypeKind::Integer { signed, width } => TypeKind::hir_Int {
                is_signed: option2std(signed),
                width: option2std(width),
            },
            HirTypeKind::Bool => TypeKind::hir_Bool,
            HirTypeKind::Unit => TypeKind::hir_Unit,
            HirTypeKind::Absurd => TypeKind::hir_Absurd,

            kind => {
                crunch_shared::warn!("unhandled type in ddlog: {:?}", kind);
                TypeKind::hir_Error
            }
        }
    }
}

impl<'ctx> Visit<HirItemPath> for DDlogEngine<'ctx> {
    type Output = ItemPath;

    #[crunch_shared::instrument(name = "item path", skip(self, path))]
    fn visit(&mut self, path: &HirItemPath) -> Self::Output {
        self.intern(Vector {
            x: path.iter().map(|s| s.as_u32()).collect(),
        })
    }
}

impl<'ctx> Visit<HirVis> for DDlogEngine<'ctx> {
    type Output = Vis;

    #[crunch_shared::instrument(name = "visibility", skip(self, vis))]
    fn visit(&mut self, vis: &HirVis) -> Self::Output {
        match vis {
            HirVis::FileLocal => Vis::hir_FileLocal,
            HirVis::Package => Vis::hir_Package,
            HirVis::Exposed => Vis::hir_Exposed,
        }
    }
}

impl<'ctx> Visit<HirStmt<'_>> for DDlogEngine<'ctx> {
    type Output = Stmt;

    #[crunch_shared::instrument(name = "statement", skip(self, stmt))]
    fn visit(&mut self, stmt: &HirStmt<'_>) -> Self::Output {
        match stmt {
            &HirStmt::Item(item) => Stmt::hir_StmtItem {
                item: self.visit(item),
            },

            &HirStmt::Expr(expr) => Stmt::hir_StmtExpr {
                expr: self.visit(expr),
            },

            HirStmt::VarDecl(var_decl) => Stmt::hir_StmtDecl {
                decl: self.visit(var_decl),
            },
        }
    }
}

impl<'ctx> Visit<HirVarDecl<'_>> for DDlogEngine<'ctx> {
    type Output = VarDecl;

    #[crunch_shared::instrument(name = "variable declaration", skip(self, var_decl))]
    fn visit(&mut self, var_decl: &HirVarDecl<'_>) -> Self::Output {
        VarDecl {
            name: self.get_or_create_var(var_decl.name),
            value: self.visit(var_decl.value),
            mutable: var_decl.mutable,
            ty: {
                let ty = self.visit(&var_decl.ty);
                self.intern(ty)
            },
        }
    }
}

impl<'ctx> Visit<HirExpr<'_>> for DDlogEngine<'ctx> {
    type Output = ExprId;

    #[crunch_shared::instrument(name = "expression", skip(self, expr))]
    fn visit(&mut self, expr: &HirExpr<'_>) -> Self::Output {
        let id = self.next_expression();

        let (kind, ty) = match &expr.kind {
            HirExprKind::Literal(literal) => {
                let (literal, ty) = self.visit(literal);
                (
                    ExprKind::hir_ExprLit {
                        lit: self.intern(literal),
                    },
                    Some(self.intern(ty)),
                )
            }

            &HirExprKind::Variable(var, ty) => {
                let expr = ExprKind::hir_ExprVar {
                    variable: self.get_or_create_var(var),
                };
                let ty = {
                    let ty = self
                        .db
                        .context()
                        .get_hir_type(ty)
                        .expect("attempted to get a type that doesn't exist");
                    self.visit(ty)
                };

                (expr, Some(self.intern(ty)))
            }

            &HirExprKind::Assign(var, rhs) => {
                let rhs = self.visit(rhs);
                let variable = self
                    .variables
                    .get(&var)
                    .copied()
                    .expect("got a variable that doesn't exist");

                let expr = ExprKind::hir_ExprAssign {
                    variable,
                    expr_id: rhs,
                };

                (expr, Some(self.intern(TypeKind::hir_Unit)))
            }

            HirExprKind::Match(_) => todo!(),
            HirExprKind::Scope(_) => todo!(),
            HirExprKind::Loop(_) => todo!(),
            HirExprKind::Return(_) => todo!(),
            HirExprKind::Continue => todo!(),
            HirExprKind::Break(_) => todo!(),
            HirExprKind::FnCall(_) => todo!(),
            HirExprKind::Comparison(_) => todo!(),
            HirExprKind::BinOp(_) => todo!(),
            HirExprKind::Cast(_) => todo!(),
            HirExprKind::Reference(_) => todo!(),
            HirExprKind::Index { var: _, index: _ } => todo!(),
        };
        let kind = self.intern(kind);
        let ty = ty.unwrap_or_else(|| self.intern(TypeKind::hir_Unknown));

        self.expressions.push(Expr { id, kind, ty });
        id
    }
}

impl<'ctx> Visit<HirLiteral> for DDlogEngine<'ctx> {
    type Output = (Literal, TypeKind);

    #[crunch_shared::instrument(name = "literal", skip(self, lit))]
    fn visit(&mut self, lit: &HirLiteral) -> Self::Output {
        let val = self.visit(&lit.val);
        let ty = self
            .db
            .context()
            .get_hir_type(lit.ty)
            .expect("attempted to get a type that doesn't exist");

        (val, self.visit(ty))
    }
}

impl<'ctx> Visit<HirLiteralVal> for DDlogEngine<'ctx> {
    type Output = Literal;

    #[crunch_shared::instrument(name = "literal value", skip(self, val))]
    fn visit(&mut self, val: &HirLiteralVal) -> Self::Output {
        match val {
            &HirLiteralVal::Integer(Integer { bits: int, .. }) => {
                Literal::hir_Integer { int: int as u64 }
            }
            &HirLiteralVal::Bool(boolean) => Literal::hir_Boolean { boolean },
            HirLiteralVal::String(string) => Literal::hir_String {
                r#str: string.to_string(),
            },
            HirLiteralVal::Rune(_) => todo!(),
            HirLiteralVal::Float(_) => todo!(),
            HirLiteralVal::Array { elements } => todo!(),
        }
    }
}

#[test]
fn ddlog_test() {
    run_ddlog().unwrap();
}

pub fn ddlog_callback(_rel: usize, _rec: &Record, _w: isize) {
    /* Obsolete, will be removed in later ddlog releases */
}
pub const DDLOG_WORKER_THREADS: usize = 12;
// Used for `HDDlog::dump_table()`
pub const DDLOG_TRACK_SNAPSHOTS: bool = false;

#[cfg(test)]
fn run_ddlog() -> Result<(), String> {
    let (mut program, init_state) =
        HDDlog::run(DDLOG_WORKER_THREADS, DDLOG_TRACK_SNAPSHOTS, ddlog_callback)?;

    println!("Initial ddlog state:");
    dump_delta(&init_state);

    program.transaction_start()?;
    let updates = vec![
        Update::Insert {
            relid: Relations::Expr as RelId,
            v: Value::Expr(Expr {
                id: 0,
                kind: ddlog_intern(&ExprKind::hir_ExprLit {
                    lit: ddlog_intern(&Literal::hir_Integer { int: 10 }),
                }),
                ty: ddlog_intern(&TypeKind::hir_Unknown),
            })
            .into_ddvalue(),
        },
        Update::Insert {
            relid: Relations::Expr as RelId,
            v: Value::Expr(Expr {
                id: 1,
                kind: ddlog_intern(&ExprKind::hir_ExprLit {
                    lit: ddlog_intern(&Literal::hir_Integer { int: 10 }),
                }),
                ty: ddlog_intern(&TypeKind::hir_Unknown),
            })
            .into_ddvalue(),
        },
        Update::Insert {
            relid: Relations::Expr as RelId,
            v: Value::Expr(Expr {
                id: 2,
                kind: ddlog_intern(&ExprKind::hir_ExprVar { variable: 0 }),
                ty: ddlog_intern(&TypeKind::hir_Unknown),
            })
            .into_ddvalue(),
        },
        Update::Insert {
            relid: Relations::InputItems as RelId,
            v: Value::InputItems(InputItems {
                item: Item::hir_ItemFunc {
                    func: Function {
                        name: ddlog_intern(&Vector { x: vec![0u32] }),
                        vis: Vis::hir_FileLocal,
                        args: Vector {
                            x: vec![FuncArg {
                                name: 0,
                                kind: ddlog_intern(&TypeKind::hir_Unknown),
                            }],
                        },
                        body: ddlog_intern(&Stmt::hir_StmtSeq {
                            first: ddlog_intern(&Stmt::hir_StmtExpr { expr: 0 }),
                            second: ddlog_intern(&Stmt::hir_StmtExpr { expr: 2 }),
                        }),
                        ret: ddlog_intern(&TypeKind::hir_Unknown),
                    },
                },
            })
            .into_ddvalue(),
        },
    ];
    program.apply_valupdates(updates.into_iter())?;

    let delta = program.transaction_commit_dump_changes()?;
    println!("\nState after transaction 1");
    dump_delta(&delta);

    program.stop()
}

fn dump_delta(delta: &DeltaMap<DDValue>) {
    for (rel, changes) in delta.iter() {
        crunch_shared::debug!("Changes to relation {}", relid2name(*rel).unwrap());
        for (val, weight) in changes.iter() {
            crunch_shared::debug!("{} {:+}", val, weight);
        }
    }
}
