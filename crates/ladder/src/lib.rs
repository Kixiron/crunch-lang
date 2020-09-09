extern crate alloc;

use alloc::sync::Arc;
use crunch_parser::database::ParseDatabase;
use crunch_shared::{
    config::EmissionKind,
    context::{Context, ContextDatabase},
    error::{ErrorHandler, Locatable, Location},
    files::FileId,
    salsa,
    strings::StrT,
    tracing,
    trees::{
        ast::{
            Arm as AstMatchArm, AssignKind, BinaryOp, Binding as AstBinding, Block as AstBlock,
            BlockExpr, CompOp, Dest as AstDest, Exposure as AstExposure, Expr as AstExpr,
            ExprKind as AstExprKind, ExternFunc as AstExternFunc, For as AstFor,
            FuncArg as AstFuncArg, If as AstIf, IfCond as AstIfCond, Item as AstItem,
            ItemKind as AstItemKind, Literal as AstLiteral, LiteralVal as AstLiteralVal,
            Loop as AstLoop, Match as AstMatch, Pattern as AstPattern, Stmt as AstStmt,
            StmtKind as AstStmtKind, StructField as AstStructField,
            StructLiteral as AstStructLiteral, Type as AstType, TypeDecl as AstTypeDecl,
            TypeMember as AstTypeMember, UnaryOp, VarDecl as AstVarDecl, Variant as AstVariant,
            While as AstWhile,
        },
        hir::{
            Binding, Block, Break, Cast, Expr, ExprKind, ExternFunc, FuncArg, FuncCall, Function,
            Item, Literal, LiteralVal, Match, MatchArm, Pattern, Reference, Return, Stmt,
            StructField, StructLiteral, Type, TypeDecl, TypeId, TypeKind, TypeMember, Var, VarDecl,
        },
        CallConv, ItemPath, Sided,
    },
    utils::Upcast,
    visitors::{
        ast::{ExprVisitor, ItemVisitor},
        Visit,
    },
};

#[salsa::query_group(HirDatabaseStorage)]
pub trait HirDatabase:
    salsa::Database
    + ContextDatabase
    + ParseDatabase
    + Upcast<dyn ContextDatabase>
    + Upcast<dyn ParseDatabase>
{
    // FIXME: Actual lifetimes when salsa allows
    fn lower_hir(
        &self,
        file: FileId,
    ) -> Result<Arc<Vec<&'static Item<'static>>>, Arc<ErrorHandler>>;
}

#[crunch_shared::instrument(name = "hir lowering", skip(db))]
fn lower_hir(
    db: &dyn HirDatabase,
    file: FileId,
) -> Result<Arc<Vec<&'static Item<'static>>>, Arc<ErrorHandler>> {
    let config = db.config();
    let ast = db.parse(file)?;

    // FIXME: I hate this
    let hir = unsafe {
        core::mem::transmute::<Vec<&'_ Item<'_>>, Vec<&'static Item<'static>>>(
            crunch_shared::allocator::CRUNCHC_ALLOCATOR
                .record_region("hir lowering", || Ladder::new(db).lower(&*ast)),
        )
    };

    if config.emit.contains(&EmissionKind::Hir) {
        let path = db
            .config()
            .out_dir
            .join(&*db.file_name(file))
            .with_extension("hir");

        std::fs::write(&path, format!("{:#?}", &hir)).unwrap();
    }

    if config.print.contains(&EmissionKind::Hir) {
        println!("{:#?}", &hir);
    }

    Ok(Arc::new(hir))
}

pub struct Ladder<'ctx> {
    db: &'ctx dyn HirDatabase,
    variable_counter: usize,
}

impl<'ctx> Ladder<'ctx> {
    pub fn new(db: &'ctx dyn HirDatabase) -> Self {
        Self {
            db,
            variable_counter: 0,
        }
    }

    pub fn lower(&mut self, items: &[&AstItem<'_>]) -> Vec<&'ctx Item<'ctx>> {
        items.iter().filter_map(|item| self.visit(item)).collect()
    }

    fn next_var(&mut self) -> Var {
        let var = Var::Auto(self.variable_counter);
        self.variable_counter += 1;

        var
    }

    // FIXME: I hate this
    fn context(&self) -> &'ctx Context<'ctx> {
        unsafe {
            core::mem::transmute::<&'static Context<'static>, &'ctx Context<'ctx>>(
                self.db.context(),
            )
        }
    }

    fn block_statement(
        &mut self,
        then_loc: Location,
        else_loc: Location,
        loop_broken: Var,
        then_block: Block<&'ctx Stmt<'ctx>>,
        else_block: Block<&'ctx Stmt<'ctx>>,
    ) -> &'ctx Stmt<'ctx> {
        crunch_shared::warn!("Returning values from loops is currently ignored");
        let location = Location::merge(then_loc, else_loc);

        self.context()
            .hir_stmt(Stmt::Expr(self.context().hir_expr(Expr {
                kind: ExprKind::Match(Match {
                    cond: self.context().hir_expr(Expr {
                        kind: ExprKind::Comparison(Sided {
                            lhs: self.context().hir_expr(Expr {
                                kind: ExprKind::Variable(
                                    loop_broken,
                                    self.db.hir_type(Type {
                                        kind: TypeKind::Bool,
                                        loc: location,
                                    }),
                                ),
                                loc: location,
                            }),
                            op: CompOp::Equal,
                            rhs: self.context().hir_expr(Expr {
                                kind: ExprKind::Literal(Literal {
                                    val: LiteralVal::Bool(true),
                                    ty: self.db.hir_type(Type {
                                        kind: TypeKind::Bool,
                                        loc: location,
                                    }),
                                    loc: location,
                                }),
                                loc: location,
                            }),
                        }),
                        loc: location,
                    }),
                    arms: vec![
                        // If the loop was broken, execute the `else` block
                        MatchArm {
                            bind: Binding {
                                reference: false,
                                mutable: false,
                                pattern: Pattern::Literal(Literal {
                                    val: LiteralVal::Bool(true),
                                    ty: self.db.hir_type(Type {
                                        kind: TypeKind::Bool,
                                        loc: location,
                                    }),
                                    loc: location,
                                }),
                                ty: None,
                            },
                            guard: None,
                            body: else_block,
                            // TODO: Allow returning values from loops
                            ty: self.db.hir_type(Type {
                                kind: TypeKind::Unknown,
                                loc: location,
                            }),
                        },
                        // If the loop was unbroken, execute the `then` block
                        MatchArm {
                            bind: Binding {
                                reference: false,
                                mutable: false,
                                pattern: Pattern::Literal(Literal {
                                    val: LiteralVal::Bool(false),
                                    ty: self.db.hir_type(Type {
                                        kind: TypeKind::Bool,
                                        loc: location,
                                    }),
                                    loc: location,
                                }),
                                ty: None,
                            },
                            guard: None,
                            body: then_block,
                            // TODO: Allow returning values from loops
                            ty: self.db.hir_type(Type {
                                kind: TypeKind::Unknown,
                                loc: location,
                            }),
                        },
                    ],
                    // TODO: Allow returning values from loops
                    ty: self.db.hir_type(Type {
                        kind: TypeKind::Unknown,
                        loc: location,
                    }),
                }),
                loc: location,
            })))
    }

    fn visit_then_and_else(
        &mut self,
        scope: &mut Block<&'ctx Stmt<'ctx>>,
        loop_broken: Var,
        then: &Option<AstBlock<'_>>,
        else_: &Option<AstBlock<'_>>,
    ) {
        // TODO: Handle these blocks for *any* breaks, not just condition ones
        match (then, else_) {
            (Some(then), Some(else_)) => {
                let then_block =
                    Block::from_iter(then.location(), then.iter().filter_map(|s| self.visit(s)));
                let else_block =
                    Block::from_iter(else_.location(), else_.iter().filter_map(|s| self.visit(s)));

                scope.push(self.block_statement(
                    then.location(),
                    else_.location(),
                    loop_broken,
                    then_block,
                    else_block,
                ));
            }

            (Some(then), None) => {
                let then_block =
                    Block::from_iter(then.location(), then.iter().filter_map(|s| self.visit(s)));

                scope.push(self.block_statement(
                    then.location(),
                    then.location(),
                    loop_broken,
                    then_block,
                    Block::empty(then.location()),
                ));
            }

            (None, Some(else_)) => {
                let else_block =
                    Block::from_iter(else_.location(), else_.iter().filter_map(|s| self.visit(s)));

                scope.push(self.block_statement(
                    else_.location(),
                    else_.location(),
                    loop_broken,
                    Block::empty(else_.location()),
                    else_block,
                ));
            }

            (None, None) => {}
        }
    }
}

impl<'ctx> Visit<AstItem<'_>> for Ladder<'ctx> {
    type Output = Option<&'ctx Item<'ctx>>;

    #[crunch_shared::instrument(name = "item", skip(self, item))]
    fn visit(&mut self, item: &AstItem<'_>) -> Self::Output {
        match &item.kind {
            AstItemKind::Func {
                generics,
                args,
                body,
                ret,
                sig,
            } => self.visit_func(
                item,
                generics.as_ref().map(|g| g.as_deref()),
                args.as_deref(),
                body,
                *ret,
                *sig,
            ),
            AstItemKind::Type(ty) => {
                let ty = self.visit(ty);
                Some(self.context().hir_item(Item::Type(ty)))
            }
            AstItemKind::Enum { generics, variants } => {
                self.visit_enum(item, generics.as_ref().map(|g| g.as_deref()), variants)
            }
            AstItemKind::Trait { generics, methods } => {
                self.visit_trait(item, generics.as_ref().map(|g| g.as_deref()), methods)
            }
            AstItemKind::Import {
                file,
                dest,
                exposes,
            } => self.visit_import(item, file, dest, exposes),
            AstItemKind::ExtendBlock(_) => {
                crunch_shared::error!("TODO: lower extend blocks to hir");
                None
            }
            AstItemKind::Alias { alias, actual } => self.visit_alias(item, *alias, *actual),
            AstItemKind::ExternBlock(_) => {
                // TODO: Maybe un-nest here?
                crunch_shared::error!(
                    "The external block un-nester pass should be run before HIR lowering",
                );
                None
            }
            AstItemKind::ExternFunc(func) => {
                let func = self.visit(&(item, func));
                Some(self.context().hir_item(Item::ExternFunc(func)))
            }
        }
    }
}

impl<'ctx> Visit<(&AstItem<'_>, &AstExternFunc<'_>)> for Ladder<'ctx> {
    type Output = ExternFunc;

    fn visit(&mut self, (item, func): &(&AstItem<'_>, &AstExternFunc<'_>)) -> Self::Output {
        let name = ItemPath::from(item.name.unwrap());
        let args = func.args.as_ref().map(|args| self.visit(&args));
        let vis = item
            .vis
            .expect("External functions should have a visibility");

        ExternFunc {
            name,
            vis,
            args,
            ret: self.visit(&func.ret),
            callconv: func.callconv,
            loc: item.location(),
        }
    }
}

impl<'ctx> Visit<AstFuncArg<'_>> for Ladder<'ctx> {
    type Output = FuncArg;

    #[crunch_shared::instrument(name = "function argument", skip(self, arg))]
    fn visit(&mut self, arg: &AstFuncArg<'_>) -> Self::Output {
        let kind = self.visit(&arg.ty);

        FuncArg {
            name: Var::User(arg.name),
            kind,
            loc: arg.location(),
        }
    }
}

impl<'ctx> ItemVisitor<'_> for Ladder<'ctx> {
    type Output = Option<&'ctx Item<'ctx>>;

    fn visit_func(
        &mut self,
        item: &AstItem<'_>,
        _generics: Option<Locatable<&[Locatable<&'_ AstType<'_>>]>>,
        args: Locatable<&[AstFuncArg<'_>]>,
        body: &AstBlock<'_>,
        ret: Locatable<&'_ AstType<'_>>,
        sig: Location,
    ) -> Self::Output {
        let name = ItemPath::from(vec![item.name.unwrap()]);
        let args = args.map(|args| args.iter().map(|arg| self.visit(arg)).collect());

        let body = Block::from_iter(
            body.location(),
            body.iter().filter_map(|stmt| self.visit(stmt)),
        );

        let func = Function {
            name,
            vis: item.vis.expect("Functions should have a visibility"),
            args,
            body,
            ret: self.visit(&ret),
            loc: item.location(),
            sig,
        };

        Some(self.context().hir_item(Item::Function(func)))
    }

    fn visit_type_decl(&mut self, _: &AstItem<'_>, _: &AstTypeDecl<'_>) -> Self::Output {
        unreachable!()
    }

    fn visit_enum(
        &mut self,
        _item: &AstItem<'_>,
        _generics: Option<Locatable<&[Locatable<&AstType<'_>>]>>,
        _variants: &[AstVariant<'_>],
    ) -> Self::Output {
        todo!()
    }

    fn visit_trait(
        &mut self,
        _item: &AstItem<'_>,
        _generics: Option<Locatable<&[Locatable<&AstType<'_>>]>>,
        _methods: &[&AstItem<'_>],
    ) -> Self::Output {
        todo!()
    }

    fn visit_import(
        &mut self,
        _item: &AstItem<'_>,
        _file: &ItemPath,
        _dest: &AstDest,
        _exposes: &AstExposure,
    ) -> Self::Output {
        todo!()
    }

    fn visit_extend_block(
        &mut self,
        _item: &AstItem<'_>,
        _target: Locatable<&AstType<'_>>,
        _extender: Option<Locatable<&AstType<'_>>>,
        _items: &[&AstItem<'_>],
    ) -> Self::Output {
        todo!()
    }

    fn visit_alias(
        &mut self,
        _item: &AstItem<'_>,
        _alias: Locatable<&AstType<'_>>,
        _actual: Locatable<&AstType<'_>>,
    ) -> Self::Output {
        todo!()
    }

    fn visit_extern_block(&mut self, _: &AstItem<'_>, _: &[&AstItem<'_>]) -> Self::Output {
        unreachable!()
    }

    fn visit_extern_func(
        &mut self,
        _: &AstItem<'_>,
        _: Option<Locatable<&[Locatable<&AstType<'_>>]>>,
        _: Locatable<&[AstFuncArg<'_>]>,
        _: Locatable<&AstType<'_>>,
        _: CallConv,
    ) -> Self::Output {
        unreachable!()
    }
}

impl<'ctx> Visit<AstTypeDecl<'_>> for Ladder<'ctx> {
    type Output = TypeDecl;

    #[crunch_shared::instrument(name = "type declaration", skip(self, ty))]
    fn visit(&mut self, ty: &AstTypeDecl<'_>) -> Self::Output {
        TypeDecl {
            generics: self.visit(&ty.generics),
            members: self.visit(&ty.members),
        }
    }
}

impl<'ctx> Visit<Locatable<Vec<Locatable<&AstType<'_>>>>> for Ladder<'ctx> {
    type Output = Vec<TypeId>;

    #[crunch_shared::instrument(name = "generics", skip(self, generics))]
    fn visit(&mut self, generics: &Locatable<Vec<Locatable<&AstType<'_>>>>) -> Self::Output {
        let mut gen = Vec::with_capacity(generics.len());
        for generic in generics.iter() {
            gen.push(self.visit(generic));
        }

        gen
    }
}

impl<'ctx> Visit<AstTypeMember<'_>> for Ladder<'ctx> {
    type Output = TypeMember;

    #[crunch_shared::instrument(name = "type member", skip(self, member))]
    fn visit(&mut self, member: &AstTypeMember<'_>) -> Self::Output {
        TypeMember {
            name: member.name,
            ty: self.visit(&member.ty),
            attrs: member.attrs.clone(),
            loc: member.loc,
        }
    }
}

impl<'ctx> Visit<AstStmt<'_>> for Ladder<'ctx> {
    type Output = Option<&'ctx Stmt<'ctx>>;

    #[crunch_shared::instrument(name = "statement", skip(self, stmt))]
    fn visit(&mut self, stmt: &AstStmt<'_>) -> Self::Output {
        match &stmt.kind {
            AstStmtKind::VarDecl(decl) => {
                let var = self.visit(decl);
                Some(self.context().hir_stmt(Stmt::VarDecl(var)))
            }

            &AstStmtKind::Item(item) => self
                .visit(item)
                .map(|item| self.context().hir_stmt(Stmt::Item(item))),

            AstStmtKind::Expr(expr) => {
                let expr = self.visit(expr);
                Some(self.context().hir_stmt(Stmt::Expr(expr)))
            }
        }
    }
}

impl<'ctx> Visit<AstVarDecl<'_>> for Ladder<'ctx> {
    type Output = VarDecl<'ctx>;

    #[crunch_shared::instrument(name = "type member", skip(self, var))]
    fn visit(&mut self, var: &AstVarDecl<'_>) -> Self::Output {
        let value = self.visit(&*var.val);
        let ty = self.visit(&var.ty);

        VarDecl {
            name: Var::User(var.name),
            value,
            mutable: var.mutable,
            ty,
            loc: var.loc,
        }
    }
}

impl<'ctx> Visit<AstExpr<'_>> for Ladder<'ctx> {
    type Output = &'ctx Expr<'ctx>;

    #[crunch_shared::instrument(name = "expression", skip(self, expr))]
    fn visit(&mut self, expr: &AstExpr<'_>) -> Self::Output {
        match &expr.kind {
            AstExprKind::If(if_) => self.visit_if(expr, if_),
            AstExprKind::Return(value) => self.visit_return(expr, value.as_deref()),
            AstExprKind::Break(value) => self.visit_break(expr, value.as_deref()),
            AstExprKind::Continue => self.visit_continue(expr),
            AstExprKind::While(while_) => self.visit_while(expr, while_),
            AstExprKind::Loop(loop_) => self.visit_loop(expr, loop_),
            AstExprKind::For(for_) => self.visit_for(expr, for_),
            AstExprKind::Match(match_) => self.visit_match(expr, match_),
            AstExprKind::Variable(var) => self.visit_variable(expr, *var),
            AstExprKind::Literal(literal) => {
                let kind = ExprKind::Literal(self.visit(literal));

                self.context().hir_expr(Expr {
                    kind,
                    loc: expr.location(),
                })
            }
            AstExprKind::UnaryOp(op, inner) => self.visit_unary(expr, *op, inner),
            AstExprKind::BinaryOp(Sided { lhs, op, rhs }) => {
                self.visit_binary_op(expr, lhs, *op, rhs)
            }
            AstExprKind::Comparison(Sided { lhs, op, rhs }) => {
                self.visit_comparison(expr, lhs, *op, rhs)
            }
            AstExprKind::Assign(Sided { lhs, op, rhs }) => self.visit_assign(expr, lhs, *op, rhs),
            AstExprKind::Paren(inner) => self.visit_paren(expr, inner),
            AstExprKind::Array(elements) => self.visit_array(expr, elements),
            AstExprKind::Tuple(elements) => self.visit_tuple(expr, elements),
            AstExprKind::Range(start, end) => self.visit_range(expr, start, end),
            AstExprKind::Index { var, index } => self.visit_index(expr, var, index),
            AstExprKind::FuncCall { caller, args } => self.visit_func_call(expr, caller, args),
            AstExprKind::MemberFuncCall { member, func } => {
                self.visit_member_func_call(expr, member, func)
            }
            &AstExprKind::Reference {
                mutable,
                expr: reference,
            } => self.visit_reference(expr, mutable, reference),
            &AstExprKind::Cast { expr: cast, ty } => self.visit_cast(expr, cast, ty),
            AstExprKind::Block(block) => {
                let block = self.visit(block);
                let loc = block.location();

                self.context().hir_expr(Expr {
                    kind: ExprKind::Scope(block),
                    loc,
                })
            }
        }
    }
}

impl<'ctx> Visit<BlockExpr<'_>> for Ladder<'ctx> {
    type Output = Block<&'ctx Stmt<'ctx>>;

    #[crunch_shared::instrument(name = "block", skip(self, block))]
    fn visit(&mut self, block: &BlockExpr<'_>) -> Self::Output {
        let mut new_block = Block::with_capacity_and_colors(
            block.contents.location(),
            block.contents.len(),
            block.colors.len(),
        );
        new_block.extend_colors(block.colors.iter().copied());
        new_block.extend(block.contents.iter().filter_map(|stmt| self.visit(stmt)));

        new_block
    }
}

impl<'ctx> ExprVisitor<'_> for Ladder<'ctx> {
    type Output = &'ctx Expr<'ctx>;

    fn visit_expr(&mut self, _: &AstExpr<'_>) -> Self::Output {
        unreachable!()
    }

    fn visit_if(
        &mut self,
        expr: &AstExpr<'_>,
        AstIf { clauses, else_ }: &AstIf<'_>,
    ) -> Self::Output {
        let mut clauses = clauses.iter();
        let mut arms = Vec::with_capacity(clauses.len() + else_.is_some() as usize);

        // If the if is `else if`-less, then generate a true/false match
        if clauses.len() == 1 {
            let AstIfCond { cond, body } = clauses.next().expect("There's at least 1 clause");
            debug_assert_eq!(clauses.count(), 0);

            arms.extend_from_slice(&[
                MatchArm {
                    bind: Binding {
                        reference: false,
                        mutable: false,
                        pattern: Pattern::Literal(Literal {
                            val: LiteralVal::Bool(true),
                            ty: self.db.hir_type(Type {
                                kind: TypeKind::Bool,
                                loc: cond.location(),
                            }),
                            loc: cond.location(),
                        }),
                        ty: None,
                    },
                    guard: None,
                    body: Block::from_iter(
                        body.location(),
                        body.iter().filter_map(|stmt| self.visit(stmt)),
                    ),
                    ty: self.db.hir_type(Type {
                        kind: TypeKind::Unknown,
                        loc: cond.location(),
                    }),
                },
                MatchArm {
                    bind: Binding {
                        reference: false,
                        mutable: false,
                        pattern: Pattern::Literal(Literal {
                            val: LiteralVal::Bool(false),
                            ty: self.db.hir_type(Type {
                                kind: TypeKind::Bool,
                                loc: cond.location(),
                            }),
                            loc: cond.location(),
                        }),
                        ty: None,
                    },
                    guard: None,
                    body: if let Some(else_) = else_ {
                        Block::from_iter(
                            else_.location(),
                            else_.iter().filter_map(|s| self.visit(s)),
                        )
                    } else {
                        Block::empty(cond.location())
                    },
                    ty: self.db.hir_type(Type {
                        kind: TypeKind::Unknown,
                        loc: cond.location(),
                    }),
                },
            ]);

            let cond = self.visit(&*cond);
            self.context().hir_expr(Expr {
                kind: ExprKind::Match(Match {
                    cond,
                    arms,
                    ty: self.db.hir_type(Type {
                        kind: TypeKind::Unknown,
                        loc: expr.location(),
                    }),
                }),
                loc: expr.location(),
            })
        } else {
            for AstIfCond { cond, body } in clauses {
                arms.push(MatchArm {
                    bind: Binding {
                        reference: false,
                        mutable: false,
                        pattern: Pattern::Wildcard,
                        ty: None,
                    },
                    guard: Some(self.visit(cond)),
                    body: Block::from_iter(
                        body.location(),
                        body.iter().filter_map(|stmt| self.visit(stmt)),
                    ),
                    ty: self.db.hir_type(Type {
                        kind: TypeKind::Unknown,
                        loc: body.location(),
                    }),
                });
            }

            if let Some(body) = else_ {
                arms.push(MatchArm {
                    bind: Binding {
                        reference: false,
                        mutable: false,
                        pattern: Pattern::Wildcard,
                        ty: None,
                    },
                    guard: None,
                    body: Block::from_iter(
                        body.location(),
                        body.iter().filter_map(|s| self.visit(s)),
                    ),
                    ty: self.db.hir_type(Type {
                        kind: TypeKind::Unknown,
                        loc: body.location(),
                    }),
                });
            }

            self.context().hir_expr(Expr {
                kind: ExprKind::Match(Match {
                    cond: self.context().hir_expr(Expr {
                        kind: ExprKind::Literal(Literal {
                            val: LiteralVal::Bool(true),
                            ty: self.db.hir_type(Type {
                                kind: TypeKind::Bool,
                                loc: expr.location(),
                            }),
                            loc: expr.location(),
                        }),
                        loc: expr.location(),
                    }),
                    arms,
                    ty: self.db.hir_type(Type {
                        kind: TypeKind::Unknown,
                        loc: expr.location(),
                    }),
                }),
                loc: expr.location(),
            })
        }
    }

    fn visit_return(&mut self, expr: &AstExpr<'_>, value: Option<&AstExpr<'_>>) -> Self::Output {
        if let Some(value) = value {
            let value = self.visit(value);
            let name = self.next_var();
            let ty = self
                .db
                .context()
                .hir_type(Type::new(TypeKind::Unknown, expr.location()));

            let assign = self.context().hir_stmt(Stmt::VarDecl(VarDecl {
                name,
                value,
                mutable: false,
                ty,
                loc: expr.location(),
            }));

            let val = Some(self.context().hir_expr(Expr {
                kind: ExprKind::Variable(name, ty),
                loc: expr.location(),
            }));

            let ret = {
                let ret = self.context().hir_expr(Expr {
                    kind: ExprKind::Return(Return { val }),
                    loc: expr.location(),
                });

                self.context().hir_stmt(Stmt::Expr(ret))
            };

            self.context().hir_expr(Expr {
                kind: ExprKind::Scope(Block::new(vec![assign, ret], expr.location())),
                loc: expr.location(),
            })
        } else {
            let kind = ExprKind::Return(Return { val: None });
            self.context().hir_expr(Expr {
                kind,
                loc: expr.location(),
            })
        }
    }

    fn visit_break(&mut self, expr: &AstExpr<'_>, value: Option<&AstExpr<'_>>) -> Self::Output {
        let kind = ExprKind::Break(Break {
            val: value.map(|expr| self.visit(expr)),
        });

        self.context().hir_expr(Expr {
            kind,
            loc: expr.location(),
        })
    }

    fn visit_continue(&mut self, expr: &AstExpr<'_>) -> Self::Output {
        self.context().hir_expr(Expr {
            kind: ExprKind::Continue,
            loc: expr.location(),
        })
    }

    fn visit_while(
        &mut self,
        expr: &AstExpr<'_>,
        AstWhile {
            cond,
            body: ast_body,
            then,
            else_,
        }: &AstWhile<'_>,
    ) -> Self::Output {
        let mut scope: Block<&'ctx Stmt<'ctx>> = Block::with_capacity(
            expr.location(),
            2 + (then.is_some() as usize * 2) + (else_.is_some() as usize * 2),
        );

        let loop_broken = self.next_var();
        scope.push(self.context().hir_stmt(Stmt::VarDecl(VarDecl {
            name: loop_broken,
            value: self.context().hir_expr(Expr {
                kind: ExprKind::Literal(Literal {
                    val: LiteralVal::Bool(false),
                    ty: self.db.hir_type(Type {
                        kind: TypeKind::Bool,
                        loc: cond.location(),
                    }),
                    loc: cond.location(),
                }),
                loc: cond.location(),
            }),
            mutable: true,
            ty: self.db.hir_type(Type {
                kind: TypeKind::Bool,
                loc: expr.location(),
            }),
            loc: cond.location(),
        })));

        let mut body: Block<&'ctx Stmt<'ctx>> =
            Block::with_capacity(ast_body.location(), ast_body.len() + 1);

        let cond = self.visit(cond);
        body.push(
            self.context()
                .hir_stmt(Stmt::Expr(self.context().hir_expr(Expr {
                    kind: ExprKind::Match(Match {
                        cond,
                        arms: vec![
                            // If the `while` condition is true, do nothing
                            // TODO: `likely` annotation?
                            // TODO: `generated` annotation?
                            MatchArm {
                                bind: Binding {
                                    reference: false,
                                    mutable: false,
                                    pattern: Pattern::Literal(Literal {
                                        val: LiteralVal::Bool(true),
                                        ty: self.db.hir_type(Type {
                                            kind: TypeKind::Bool,
                                            loc: cond.location(),
                                        }),
                                        loc: cond.location(),
                                    }),
                                    ty: None,
                                },
                                guard: None,
                                body: Block::empty(cond.location()),
                                ty: self.db.hir_type(Type {
                                    kind: TypeKind::Unknown,
                                    loc: cond.location(),
                                }),
                            },
                            // If the `while` condition returns false, set the status and break
                            MatchArm {
                                bind: Binding {
                                    reference: false,
                                    mutable: false,
                                    pattern: Pattern::Literal(Literal {
                                        val: LiteralVal::Bool(false),
                                        ty: self.db.hir_type(Type {
                                            kind: TypeKind::Bool,
                                            loc: cond.location(),
                                        }),
                                        loc: cond.location(),
                                    }),
                                    ty: None,
                                },
                                guard: None,
                                body: Block::from_iter(
                                    cond.location(),
                                    vec![
                                        // Set the loop status to true since we've broken it
                                        self.context().hir_stmt(Stmt::Expr(
                                            self.context().hir_expr(Expr {
                                                kind: ExprKind::Assign(
                                                    loop_broken,
                                                    self.context().hir_expr(Expr {
                                                        kind: ExprKind::Literal(Literal {
                                                            val: LiteralVal::Bool(true),
                                                            ty: self.db.hir_type(Type {
                                                                kind: TypeKind::Bool,
                                                                loc: cond.location(),
                                                            }),
                                                            loc: cond.location(),
                                                        }),
                                                        loc: cond.location(),
                                                    }),
                                                ),
                                                loc: cond.location(),
                                            }),
                                        )),
                                        // Break from the loop
                                        self.context().hir_stmt(Stmt::Expr(
                                            self.context().hir_expr(Expr {
                                                kind: ExprKind::Break(Break { val: None }),
                                                loc: cond.location(),
                                            }),
                                        )),
                                    ],
                                ),
                                ty: self.db.hir_type(Type {
                                    kind: TypeKind::Unknown,
                                    loc: cond.location(),
                                }),
                            },
                        ],
                        ty: self.db.hir_type(Type {
                            kind: TypeKind::Unknown,
                            loc: cond.location(),
                        }),
                    }),
                    loc: cond.location(),
                }))),
        );

        body.extend(ast_body.iter().filter_map(|s| self.visit(s)));

        scope.push(
            self.context()
                .hir_stmt(Stmt::Expr(self.context().hir_expr(Expr {
                    kind: ExprKind::Loop(body),
                    loc: expr.location(),
                }))),
        );

        self.visit_then_and_else(&mut scope, loop_broken, then, else_);

        self.context().hir_expr(Expr {
            kind: ExprKind::Scope(scope),
            loc: expr.location(),
        })
    }

    fn visit_loop(
        &mut self,
        expr: &AstExpr<'_>,
        AstLoop { body, else_: _ }: &AstLoop<'_>,
    ) -> Self::Output {
        let kind = ExprKind::Loop(Block::from_iter(
            body.location(),
            body.iter().filter_map(|stmt| self.visit(stmt)),
        ));

        self.context().hir_expr(Expr {
            kind,
            loc: expr.location(),
        })
    }

    fn visit_for(&mut self, _expr: &AstExpr<'_>, _for: &AstFor<'_>) -> Self::Output {
        todo!("Desugar `for` conditions to iterators + loops")
    }

    fn visit_match(
        &mut self,
        expr: &AstExpr<'_>,
        AstMatch { var, arms }: &AstMatch<'_>,
    ) -> Self::Output {
        let cond = self.visit(&*var);
        let arms = arms
            .iter()
            .map(|AstMatchArm { bind, guard, body }| MatchArm {
                bind: self.visit_binding(bind),
                guard: guard.as_ref().map(|guard| self.visit(&**guard)),
                body: Block::from_iter(
                    body.location(),
                    body.iter().filter_map(|stmt| self.visit(stmt)),
                ),
                ty: self.db.hir_type(Type {
                    kind: TypeKind::Unknown,
                    loc: expr.location(),
                }),
            })
            .collect();

        self.context().hir_expr(Expr {
            kind: ExprKind::Match(Match {
                cond,
                arms,
                ty: self.db.hir_type(Type {
                    kind: TypeKind::Unknown,
                    loc: expr.location(),
                }),
            }),
            loc: expr.location(),
        })
    }

    fn visit_variable(&mut self, expr: &AstExpr<'_>, var: Locatable<StrT>) -> Self::Output {
        self.context().hir_expr(Expr {
            kind: ExprKind::Variable(
                Var::User(*var),
                self.db.hir_type(Type {
                    kind: TypeKind::Unknown,
                    loc: expr.location(),
                }),
            ),
            loc: expr.location(),
        })
    }

    type LiteralOutput = Literal<'ctx>;
    fn visit_literal(&mut self, _: &AstLiteral<'_>) -> Self::LiteralOutput {
        unreachable!()
    }

    type LiteralValOutput = LiteralVal<'ctx>;
    fn visit_literal_val(&mut self, _: &AstLiteralVal<'_>) -> Self::LiteralValOutput {
        unreachable!()
    }

    fn visit_unary(
        &mut self,
        _expr: &AstExpr<'_>,
        _op: UnaryOp,
        _inner: &AstExpr<'_>,
    ) -> Self::Output {
        todo!()
    }

    fn visit_binary_op(
        &mut self,
        expr: &AstExpr<'_>,
        lhs: &AstExpr<'_>,
        op: BinaryOp,
        rhs: &AstExpr<'_>,
    ) -> Self::Output {
        let lhs = self.visit(lhs);
        let rhs = self.visit(rhs);

        self.context().hir_expr(Expr {
            kind: ExprKind::BinOp(Sided { lhs, op, rhs }),
            loc: expr.location(),
        })
    }

    fn visit_comparison(
        &mut self,
        expr: &AstExpr<'_>,
        lhs: &AstExpr<'_>,
        op: CompOp,
        rhs: &AstExpr<'_>,
    ) -> Self::Output {
        let lhs = self.visit(lhs);
        let rhs = self.visit(rhs);

        self.context().hir_expr(Expr {
            kind: ExprKind::Comparison(Sided { lhs, op, rhs }),
            loc: expr.location(),
        })
    }

    fn visit_assign(
        &mut self,
        expr: &AstExpr<'_>,
        lhs: &AstExpr<'_>,
        op: AssignKind,
        rhs: &AstExpr<'_>,
    ) -> Self::Output {
        let var = if let AstExpr {
            kind: AstExprKind::Variable(var),
            ..
        } = lhs
        {
            Var::User(**var)
        } else {
            self.next_var()
        };

        if let AssignKind::BinaryOp(op) = op {
            let lhs = self.visit(lhs);
            let rhs = self.visit(rhs);

            self.context().hir_expr(Expr {
                kind: ExprKind::Assign(
                    var,
                    self.context().hir_expr(Expr {
                        kind: ExprKind::BinOp(Sided { lhs, op, rhs }),
                        loc: expr.location(),
                    }),
                ),
                loc: expr.location(),
            })
        } else {
            // FIXME: This is wrong lol
            let kind = ExprKind::Assign(var, self.visit(rhs));

            self.context().hir_expr(Expr {
                kind,
                loc: expr.location(),
            })
        }
    }

    fn visit_paren(&mut self, _expr: &AstExpr<'_>, inner: &AstExpr<'_>) -> Self::Output {
        self.visit(inner)
    }

    fn visit_array(&mut self, _expr: &AstExpr<'_>, _elements: &[&AstExpr<'_>]) -> Self::Output {
        todo!()
    }

    fn visit_tuple(&mut self, _expr: &AstExpr<'_>, _elements: &[&AstExpr<'_>]) -> Self::Output {
        todo!()
    }

    fn visit_range(
        &mut self,
        _expr: &AstExpr<'_>,
        _start: &AstExpr<'_>,
        _end: &AstExpr<'_>,
    ) -> Self::Output {
        todo!()
    }

    fn visit_index(
        &mut self,
        expr: &AstExpr<'_>,
        var: &AstExpr<'_>,
        index: &AstExpr<'_>,
    ) -> Self::Output {
        let mut scope = Vec::with_capacity(2);
        let indexee_name = if let AstExpr {
            kind: AstExprKind::Variable(var),
            ..
        } = var
        {
            Var::User(**var)
        } else {
            let value = self.visit(var);
            let name = self.next_var();
            let ty = self
                .db
                .context()
                .hir_type(Type::new(TypeKind::Unknown, expr.location()));

            scope.push(self.context().hir_stmt(Stmt::VarDecl(VarDecl {
                name,
                value,
                mutable: false,
                ty,
                loc: expr.location(),
            })));

            name
        };

        let (index_name, index_ty) = if let AstExpr {
            kind: AstExprKind::Variable(var),
            ..
        } = index
        {
            (
                Var::User(**var),
                self.db
                    .context()
                    .hir_type(Type::new(TypeKind::Unknown, expr.location())),
            )
        } else {
            let value = self.visit(index);
            let name = self.next_var();
            let ty = self
                .db
                .context()
                .hir_type(Type::new(TypeKind::Unknown, expr.location()));

            scope.push(self.context().hir_stmt(Stmt::VarDecl(VarDecl {
                name,
                value,
                mutable: false,
                ty,
                loc: expr.location(),
            })));

            (name, ty)
        };

        let index = self.context().hir_expr(Expr {
            kind: ExprKind::Variable(index_name, index_ty),
            loc: expr.location(),
        });
        let index = self.context().hir_expr(Expr {
            kind: ExprKind::Index {
                var: indexee_name,
                index,
            },
            loc: expr.location(),
        });
        scope.push(self.context().hir_stmt(Stmt::Expr(index)));

        self.context().hir_expr(Expr {
            kind: ExprKind::Scope(Block::new(scope, expr.location())),
            loc: expr.location(),
        })
    }

    fn visit_func_call(
        &mut self,
        expr: &AstExpr<'_>,
        caller: &AstExpr<'_>,
        args: &[&AstExpr<'_>],
    ) -> Self::Output {
        let args = args.iter().map(|a| self.visit(a)).collect();

        self.context().hir_expr(Expr {
            kind: ExprKind::FnCall(FuncCall {
                func: if let AstExprKind::Variable(path) = caller.kind {
                    ItemPath::new(*path)
                } else {
                    todo!()
                },
                args,
            }),
            loc: expr.location(),
        })
    }

    fn visit_member_func_call(
        &mut self,
        _expr: &AstExpr<'_>,
        _member: &AstExpr<'_>,
        _func: &AstExpr<'_>,
    ) -> Self::Output {
        todo!()
    }

    fn visit_reference(
        &mut self,
        expr: &AstExpr<'_>,
        mutable: bool,
        reference: &AstExpr<'_>,
    ) -> Self::Output {
        let reference = self.visit(reference);

        self.context().hir_expr(Expr {
            kind: ExprKind::Reference(Reference { mutable, reference }),
            loc: expr.location(),
        })
    }

    fn visit_cast(
        &mut self,
        expr: &AstExpr<'_>,
        cast: &AstExpr<'_>,
        ty: Locatable<&AstType<'_>>,
    ) -> Self::Output {
        let casted = self.visit(cast);
        let ty = self.visit(&ty);

        self.context().hir_expr(Expr {
            kind: ExprKind::Cast(Cast { casted, ty }),
            loc: expr.location(),
        })
    }

    type BindingOutput = Binding<'ctx>;
    fn visit_binding(&mut self, binding: &AstBinding<'_>) -> Self::BindingOutput {
        self.visit(binding)
    }

    type PatternOutput = Pattern<'ctx>;
    fn visit_pattern(&mut self, pattern: &AstPattern<'_>) -> Self::PatternOutput {
        self.visit(pattern)
    }
}

impl<'ctx> Visit<AstBinding<'_>> for Ladder<'ctx> {
    type Output = Binding<'ctx>;

    #[crunch_shared::instrument(name = "binding", skip(self, binding))]
    fn visit(&mut self, binding: &AstBinding<'_>) -> Self::Output {
        Binding {
            reference: binding.reference,
            mutable: binding.mutable,
            pattern: self.visit_pattern(&binding.pattern),
            ty: self.visit(&binding.ty),
        }
    }
}

impl<'ctx> Visit<AstPattern<'_>> for Ladder<'ctx> {
    type Output = Pattern<'ctx>;

    #[crunch_shared::instrument(name = "pattern", skip(self, pattern))]
    fn visit(&mut self, pattern: &AstPattern<'_>) -> Self::Output {
        match pattern {
            AstPattern::Literal(literal) => Pattern::Literal(self.visit(literal)),
            &AstPattern::Ident(ident) => Pattern::Ident(ident),
            AstPattern::ItemPath(path) => Pattern::ItemPath(path.clone()),
            AstPattern::Wildcard => Pattern::Wildcard,
        }
    }
}

impl<'ctx> Visit<AstLiteral<'_>> for Ladder<'ctx> {
    type Output = Literal<'ctx>;

    #[crunch_shared::instrument(name = "literal", skip(self, literal))]
    fn visit(&mut self, literal: &AstLiteral<'_>) -> Self::Output {
        Literal {
            val: self.visit(&literal.val),
            ty: self.visit(&Locatable::new(literal.ty, literal.location())),
            loc: literal.location(),
        }
    }
}

impl<'ctx> Visit<AstLiteralVal<'_>> for Ladder<'ctx> {
    type Output = LiteralVal<'ctx>;

    #[crunch_shared::instrument(name = "literal value", skip(self, val))]
    fn visit(&mut self, val: &AstLiteralVal<'_>) -> Self::Output {
        match val {
            &AstLiteralVal::Integer(int) => LiteralVal::Integer(int),
            &AstLiteralVal::Bool(boolean) => LiteralVal::Bool(boolean),
            AstLiteralVal::String(text) => LiteralVal::String(text.clone()),
            &AstLiteralVal::Rune(rune) => LiteralVal::Rune(rune),
            &AstLiteralVal::Float(float) => LiteralVal::Float(float),
            AstLiteralVal::Array(array) => {
                let elements = array.iter().map(|literal| self.visit(literal)).collect();

                LiteralVal::Array { elements }
            }
            AstLiteralVal::Struct(struct_val) => LiteralVal::Struct(self.visit(struct_val)),
        }
    }
}

impl<'ctx> Visit<AstStructLiteral<'_>> for Ladder<'ctx> {
    type Output = StructLiteral<'ctx>;

    #[crunch_shared::instrument(
        name = "struct literal",
        skip(self, struct_lit),
        fields(struct_name = %self.db.context().strings().resolve(struct_lit.name)),
    )]
    fn visit(&mut self, struct_lit: &AstStructLiteral<'_>) -> Self::Output {
        StructLiteral {
            name: struct_lit.name,
            fields: struct_lit
                .fields
                .iter()
                .map(|field| self.visit(field))
                .collect(),
        }
    }
}

impl<'ctx> Visit<AstStructField<'_>> for Ladder<'ctx> {
    type Output = StructField<'ctx>;

    #[crunch_shared::instrument(
        name = "struct literal field",
        skip(self, field),
        fields(field_name = %self.db.context().strings().resolve(field.name)),
    )]
    fn visit(&mut self, field: &AstStructField<'_>) -> Self::Output {
        StructField {
            name: field.name,
            value: self.visit(field.value),
            loc: field.loc,
        }
    }
}

impl<'ctx> Visit<Locatable<&AstType<'_>>> for Ladder<'ctx> {
    type Output = TypeId;

    #[crunch_shared::instrument(name = "type", skip(self, ty))]
    fn visit(&mut self, ty: &Locatable<&AstType<'_>>) -> Self::Output {
        let kind = self.visit(**ty);

        self.db.hir_type(Type {
            kind,
            loc: ty.location(),
        })
    }
}

impl<'ctx> Visit<AstType<'_>> for Ladder<'ctx> {
    type Output = TypeKind;

    #[crunch_shared::instrument(name = "type inner", skip(self, ty))]
    fn visit(&mut self, ty: &AstType<'_>) -> Self::Output {
        match ty {
            AstType::Unknown => TypeKind::Unknown,
            AstType::Unit => TypeKind::Unit,
            AstType::Bool => TypeKind::Bool,
            AstType::String => TypeKind::String,
            &AstType::Integer { signed, width } => TypeKind::Integer { signed, width },

            &AstType::Array {
                ref element,
                length,
            } => {
                let element = self.visit(element);
                TypeKind::Array { element, length }
            }

            AstType::Slice { element } => {
                let element = self.visit(element);
                TypeKind::Slice { element }
            }

            &AstType::Pointer {
                ref pointee,
                mutable,
            } => {
                let pointee = self.visit(pointee);
                TypeKind::Pointer { pointee, mutable }
            }

            &AstType::Reference {
                ref referee,
                mutable,
            } => {
                let referee = self.visit(referee);
                TypeKind::Reference { referee, mutable }
            }

            // FIXME: This
            &AstType::IntPtr { signed } => TypeKind::Integer {
                signed: Some(signed),
                width: Some(64),
            },

            ty => todo!("{:?}", ty),
        }
    }
}
