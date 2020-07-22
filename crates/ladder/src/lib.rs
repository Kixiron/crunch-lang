use crunch_shared::{
    context::Context,
    error::{Locatable, Location},
    strings::StrT,
    trees::{
        ast::{
            Arm as AstMatchArm, AssignKind, BinaryOp, Binding as AstBinding, Block as AstBlock,
            CompOp, Dest as AstDest, Exposure as AstExposure, Expr as AstExpr,
            ExprKind as AstExprKind, For as AstFor, FuncArg as AstFuncArg, If as AstIf,
            IfCond as AstIfCond, Item as AstItem, Literal as AstLiteral,
            LiteralVal as AstLiteralVal, Loop as AstLoop, Match as AstMatch, Pattern as AstPattern,
            Stmt as AstStmt, StmtKind as AstStmtKind, Type as AstType, TypeMember as AstTypeMember,
            UnaryOp, VarDecl as AstVarDecl, Variant as AstVariant, While as AstWhile,
        },
        hir::{
            Binding, Block, Break, Cast, Expr, ExprKind, ExternFunc, FuncArg, FuncCall, Function,
            Item, Literal, LiteralVal, Match, MatchArm, Pattern, Reference, Return, Stmt, Type,
            TypeId, TypeKind, Var, VarDecl,
        },
        CallConv, ItemPath, Sided,
    },
    visitors::ast::{ExprVisitor, ItemVisitor, StmtVisitor, TypeVisitor},
};

pub struct Ladder<'ctx> {
    context: &'ctx Context<'ctx>,
}

impl<'ctx> Ladder<'ctx> {
    pub const fn new(context: &'ctx Context<'ctx>) -> Self {
        Self { context }
    }

    #[inline]
    pub fn lower(&mut self, items: &[&'ctx AstItem<'ctx>]) -> Vec<&'ctx Item<'ctx>> {
        items
            .iter()
            .filter_map(|item| self.visit_item(item))
            .collect()
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

        self.context
            .hir_stmt(Stmt::Expr(self.context.hir_expr(Expr {
                kind: ExprKind::Match(Match {
                    cond: self.context.hir_expr(Expr {
                        kind: ExprKind::Comparison(Sided {
                            lhs: self.context.hir_expr(Expr {
                                kind: ExprKind::Variable(
                                    loop_broken,
                                    self.context.hir_type(Type {
                                        kind: TypeKind::Bool,
                                        loc: location,
                                    }),
                                ),
                                loc: location,
                            }),
                            op: CompOp::Equal,
                            rhs: self.context.hir_expr(Expr {
                                kind: ExprKind::Literal(Literal {
                                    val: LiteralVal::Bool(true),
                                    ty: self.context.hir_type(Type {
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
                                    ty: self.context.hir_type(Type {
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
                            ty: self.context.hir_type(Type {
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
                                    ty: self.context.hir_type(Type {
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
                            ty: self.context.hir_type(Type {
                                kind: TypeKind::Unknown,
                                loc: location,
                            }),
                        },
                    ],
                    // TODO: Allow returning values from loops
                    ty: self.context.hir_type(Type {
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
        then: &Option<AstBlock<'ctx>>,
        else_: &Option<AstBlock<'ctx>>,
    ) {
        // TODO: Handle these blocks for *any* breaks, not just condition ones
        match (then, else_) {
            (Some(then), Some(else_)) => {
                let then_block = Block::from_iter(
                    then.location(),
                    then.iter().filter_map(|s| self.visit_stmt(s)),
                );
                let else_block = Block::from_iter(
                    else_.location(),
                    else_.iter().filter_map(|s| self.visit_stmt(s)),
                );

                scope.push(self.block_statement(
                    then.location(),
                    else_.location(),
                    loop_broken,
                    then_block,
                    else_block,
                ));
            }

            (Some(then), None) => {
                let then_block = Block::from_iter(
                    then.location(),
                    then.iter().filter_map(|s| self.visit_stmt(s)),
                );

                scope.push(self.block_statement(
                    then.location(),
                    then.location(),
                    loop_broken,
                    then_block,
                    Block::new(then.location()),
                ));
            }

            (None, Some(else_)) => {
                let else_block = Block::from_iter(
                    else_.location(),
                    else_.iter().filter_map(|s| self.visit_stmt(s)),
                );

                scope.push(self.block_statement(
                    else_.location(),
                    else_.location(),
                    loop_broken,
                    Block::new(else_.location()),
                    else_block,
                ));
            }

            (None, None) => {}
        }
    }
}

impl<'ctx> ItemVisitor<'ctx> for Ladder<'ctx> {
    type Output = Option<&'ctx Item<'ctx>>;

    fn visit_func(
        &mut self,
        item: &'ctx AstItem<'ctx>,
        _generics: Option<Locatable<&[Locatable<&'ctx AstType<'ctx>>]>>,
        args: Locatable<&[AstFuncArg<'ctx>]>,
        body: &AstBlock<'ctx>,
        ret: Locatable<&'ctx AstType<'ctx>>,
        sig: Location,
    ) -> Self::Output {
        let name = ItemPath::from(vec![item.name.unwrap()]);
        let args = args.map(|args| {
            args.iter()
                .map(|&AstFuncArg { name, ty, loc }| FuncArg {
                    name: Var::User(name),
                    kind: self.visit_type(ty),
                    loc,
                })
                .collect()
        });

        let body = Block::from_iter(
            body.location(),
            body.iter().filter_map(|stmt| self.visit_stmt(stmt)),
        );

        let func = Function {
            name,
            vis: item.vis.expect("Functions should have a visibility"),
            args,
            body,
            ret: self.visit_type(ret),
            loc: item.location(),
            sig,
        };

        Some(self.context.hir_item(Item::Function(func)))
    }

    fn visit_type_decl(
        &mut self,
        _item: &'ctx AstItem<'ctx>,
        _generics: Option<Locatable<&[Locatable<&'ctx AstType<'ctx>>]>>,
        _members: &[AstTypeMember],
    ) -> Self::Output {
        todo!()
    }

    fn visit_enum(
        &mut self,
        _item: &'ctx AstItem<'ctx>,
        _generics: Option<Locatable<&[Locatable<&'ctx AstType<'ctx>>]>>,
        _variants: &[AstVariant],
    ) -> Self::Output {
        todo!()
    }

    fn visit_trait(
        &mut self,
        _item: &'ctx AstItem<'ctx>,
        _generics: Option<Locatable<&[Locatable<&'ctx AstType<'ctx>>]>>,
        _methods: &[&'ctx AstItem<'ctx>],
    ) -> Self::Output {
        todo!()
    }

    fn visit_import(
        &mut self,
        _item: &'ctx AstItem<'ctx>,
        _file: &ItemPath,
        _dest: &AstDest,
        _exposes: &AstExposure,
    ) -> Self::Output {
        todo!()
    }

    fn visit_extend_block(
        &mut self,
        _item: &'ctx AstItem<'ctx>,
        _target: Locatable<&'ctx AstType<'ctx>>,
        _extender: Option<Locatable<&'ctx AstType<'ctx>>>,
        _items: &[&'ctx AstItem<'ctx>],
    ) -> Self::Output {
        todo!()
    }

    fn visit_alias(
        &mut self,
        _item: &'ctx AstItem<'ctx>,
        _alias: Locatable<&'ctx AstType<'ctx>>,
        _actual: Locatable<&'ctx AstType<'ctx>>,
    ) -> Self::Output {
        todo!()
    }

    fn visit_extern_block(
        &mut self,
        _item: &'ctx AstItem<'ctx>,
        _items: &[&'ctx AstItem<'ctx>],
    ) -> Self::Output {
        crunch_shared::error!("The external block unnester pass should be run before HIR lowering");

        None
    }

    fn visit_extern_func(
        &mut self,
        item: &'ctx AstItem<'ctx>,
        _generics: Option<Locatable<&[Locatable<&'ctx AstType<'ctx>>]>>,
        args: Locatable<&[AstFuncArg<'ctx>]>,
        ret: Locatable<&'ctx AstType<'ctx>>,
        callconv: CallConv,
    ) -> Self::Output {
        let name = ItemPath::from(vec![item.name.unwrap()]);
        let args = args.map(|args| {
            args.iter()
                .map(|&AstFuncArg { name, ty, loc }| {
                    let kind = self.visit_type(ty);

                    FuncArg {
                        name: Var::User(name),
                        kind,
                        loc,
                    }
                })
                .collect()
        });

        let func = ExternFunc {
            name,
            vis: item
                .vis
                .expect("External functions should have a visibility"),
            args,
            ret: self.visit_type(ret),
            callconv,
            loc: item.location(),
        };

        Some(self.context.hir_item(Item::ExternFunc(func)))
    }
}

impl<'ctx> StmtVisitor<'ctx> for Ladder<'ctx> {
    type Output = Option<&'ctx Stmt<'ctx>>;

    fn visit_stmt(&mut self, stmt: &'ctx AstStmt<'ctx>) -> Self::Output {
        match &stmt.kind {
            AstStmtKind::VarDecl(decl) => self.visit_var_decl(stmt, decl),

            AstStmtKind::Item(item) => self
                .visit_item(item)
                .map(|i| self.context.hir_stmt(Stmt::Item(i))),

            AstStmtKind::Expr(expr) => {
                let expr = self.visit_expr(expr);

                Some(self.context.hir_stmt(Stmt::Expr(expr)))
            }
        }
    }

    fn visit_var_decl(
        &mut self,
        stmt: &'ctx AstStmt<'ctx>,
        var: &AstVarDecl<'ctx>,
    ) -> Self::Output {
        let value = self.visit_expr(&*var.val);
        let ty = self.visit_type(var.ty);

        Some(self.context.hir_stmt(Stmt::VarDecl(VarDecl {
            name: Var::User(var.name),
            value,
            mutable: var.mutable,
            ty,
            loc: stmt.location(),
        })))
    }
}

impl<'ctx> ExprVisitor<'ctx> for Ladder<'ctx> {
    type Output = &'ctx Expr<'ctx>;

    fn visit_expr(&mut self, expr: &'ctx AstExpr<'ctx>) -> Self::Output {
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
                let kind = ExprKind::Literal(self.visit_literal(literal));

                self.context.hir_expr(Expr {
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
        }
    }

    fn visit_if(
        &mut self,
        expr: &'ctx AstExpr<'ctx>,
        AstIf { clauses, else_ }: &AstIf<'ctx>,
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
                            ty: self.context.hir_type(Type {
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
                        body.iter().filter_map(|stmt| self.visit_stmt(stmt)),
                    ),
                    ty: self.context.hir_type(Type {
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
                            ty: self.context.hir_type(Type {
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
                            else_.iter().filter_map(|s| self.visit_stmt(s)),
                        )
                    } else {
                        Block::new(cond.location())
                    },
                    ty: self.context.hir_type(Type {
                        kind: TypeKind::Unknown,
                        loc: cond.location(),
                    }),
                },
            ]);

            let cond = self.visit_expr(&*cond);
            self.context.hir_expr(Expr {
                kind: ExprKind::Match(Match {
                    cond,
                    arms,
                    ty: self.context.hir_type(Type {
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
                    guard: Some(self.visit_expr(cond)),
                    body: Block::from_iter(
                        body.location(),
                        body.iter().filter_map(|stmt| self.visit_stmt(stmt)),
                    ),
                    ty: self.context.hir_type(Type {
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
                        body.iter().filter_map(|s| self.visit_stmt(s)),
                    ),
                    ty: self.context.hir_type(Type {
                        kind: TypeKind::Unknown,
                        loc: body.location(),
                    }),
                });
            }

            self.context.hir_expr(Expr {
                kind: ExprKind::Match(Match {
                    cond: self.context.hir_expr(Expr {
                        kind: ExprKind::Literal(Literal {
                            val: LiteralVal::Bool(true),
                            ty: self.context.hir_type(Type {
                                kind: TypeKind::Bool,
                                loc: expr.location(),
                            }),
                            loc: expr.location(),
                        }),
                        loc: expr.location(),
                    }),
                    arms,
                    ty: self.context.hir_type(Type {
                        kind: TypeKind::Unknown,
                        loc: expr.location(),
                    }),
                }),
                loc: expr.location(),
            })
        }
    }

    fn visit_return(
        &mut self,
        expr: &'ctx AstExpr<'ctx>,
        value: Option<&'ctx AstExpr<'ctx>>,
    ) -> Self::Output {
        let kind = ExprKind::Return(Return {
            val: value.map(|expr| self.visit_expr(expr)),
        });

        self.context.hir_expr(Expr {
            kind,
            loc: expr.location(),
        })
    }

    fn visit_break(
        &mut self,
        expr: &'ctx AstExpr<'ctx>,
        value: Option<&'ctx AstExpr<'ctx>>,
    ) -> Self::Output {
        let kind = ExprKind::Break(Break {
            val: value.map(|expr| self.visit_expr(expr)),
        });

        self.context.hir_expr(Expr {
            kind,
            loc: expr.location(),
        })
    }

    fn visit_continue(&mut self, expr: &'ctx AstExpr<'ctx>) -> Self::Output {
        self.context.hir_expr(Expr {
            kind: ExprKind::Continue,
            loc: expr.location(),
        })
    }

    fn visit_while(
        &mut self,
        expr: &'ctx AstExpr<'ctx>,
        AstWhile {
            cond,
            body: ast_body,
            then,
            else_,
        }: &AstWhile<'ctx>,
    ) -> Self::Output {
        let mut scope: Block<&'ctx Stmt<'ctx>> = Block::with_capacity(
            expr.location(),
            2 + (then.is_some() as usize * 2) + (else_.is_some() as usize * 2),
        );

        let loop_broken = Var::Auto(0);
        scope.push(self.context.hir_stmt(Stmt::VarDecl(VarDecl {
            name: loop_broken,
            value: self.context.hir_expr(Expr {
                kind: ExprKind::Literal(Literal {
                    val: LiteralVal::Bool(false),
                    ty: self.context.hir_type(Type {
                        kind: TypeKind::Bool,
                        loc: cond.location(),
                    }),
                    loc: cond.location(),
                }),
                loc: cond.location(),
            }),
            mutable: true,
            ty: self.context.hir_type(Type {
                kind: TypeKind::Bool,
                loc: expr.location(),
            }),
            loc: cond.location(),
        })));

        let mut body: Block<&'ctx Stmt<'ctx>> =
            Block::with_capacity(ast_body.location(), ast_body.len() + 1);

        let cond = self.visit_expr(cond);
        body.push(
            self.context
                .hir_stmt(Stmt::Expr(self.context.hir_expr(Expr {
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
                                        ty: self.context.hir_type(Type {
                                            kind: TypeKind::Bool,
                                            loc: cond.location(),
                                        }),
                                        loc: cond.location(),
                                    }),
                                    ty: None,
                                },
                                guard: None,
                                body: Block::new(cond.location()),
                                ty: self.context.hir_type(Type {
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
                                        ty: self.context.hir_type(Type {
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
                                        self.context.hir_stmt(Stmt::Expr(self.context.hir_expr(
                                            Expr {
                                                kind: ExprKind::Assign(
                                                    loop_broken,
                                                    self.context.hir_expr(Expr {
                                                        kind: ExprKind::Literal(Literal {
                                                            val: LiteralVal::Bool(true),
                                                            ty: self.context.hir_type(Type {
                                                                kind: TypeKind::Bool,
                                                                loc: cond.location(),
                                                            }),
                                                            loc: cond.location(),
                                                        }),
                                                        loc: cond.location(),
                                                    }),
                                                ),
                                                loc: cond.location(),
                                            },
                                        ))),
                                        // Break from the loop
                                        self.context.hir_stmt(Stmt::Expr(self.context.hir_expr(
                                            Expr {
                                                kind: ExprKind::Break(Break { val: None }),
                                                loc: cond.location(),
                                            },
                                        ))),
                                    ],
                                ),
                                ty: self.context.hir_type(Type {
                                    kind: TypeKind::Unknown,
                                    loc: cond.location(),
                                }),
                            },
                        ],
                        ty: self.context.hir_type(Type {
                            kind: TypeKind::Unknown,
                            loc: cond.location(),
                        }),
                    }),
                    loc: cond.location(),
                }))),
        );

        body.extend(ast_body.iter().filter_map(|s| self.visit_stmt(s)));

        scope.push(
            self.context
                .hir_stmt(Stmt::Expr(self.context.hir_expr(Expr {
                    kind: ExprKind::Loop(body),
                    loc: expr.location(),
                }))),
        );

        self.visit_then_and_else(&mut scope, loop_broken, then, else_);

        self.context.hir_expr(Expr {
            kind: ExprKind::Scope(scope),
            loc: expr.location(),
        })
    }

    fn visit_loop(
        &mut self,
        expr: &'ctx AstExpr<'ctx>,
        AstLoop { body, else_: _ }: &AstLoop<'ctx>,
    ) -> Self::Output {
        let kind = ExprKind::Loop(Block::from_iter(
            body.location(),
            body.iter().filter_map(|stmt| self.visit_stmt(stmt)),
        ));

        self.context.hir_expr(Expr {
            kind,
            loc: expr.location(),
        })
    }

    fn visit_for(&mut self, _expr: &'ctx AstExpr<'ctx>, _for: &AstFor) -> Self::Output {
        todo!("Desugar `for` conditions to iterators")
    }

    fn visit_match(
        &mut self,
        expr: &'ctx AstExpr<'ctx>,
        AstMatch { var, arms }: &AstMatch<'ctx>,
    ) -> Self::Output {
        let cond = self.visit_expr(&*var);
        let arms = arms
            .iter()
            .map(|AstMatchArm { bind, guard, body }| MatchArm {
                bind: self.visit_binding(bind),
                guard: guard.as_ref().map(|guard| self.visit_expr(&**guard)),
                body: Block::from_iter(
                    body.location(),
                    body.iter().filter_map(|stmt| self.visit_stmt(stmt)),
                ),
                ty: self.context.hir_type(Type {
                    kind: TypeKind::Unknown,
                    loc: expr.location(),
                }),
            })
            .collect();

        self.context.hir_expr(Expr {
            kind: ExprKind::Match(Match {
                cond,
                arms,
                ty: self.context.hir_type(Type {
                    kind: TypeKind::Unknown,
                    loc: expr.location(),
                }),
            }),
            loc: expr.location(),
        })
    }

    fn visit_variable(&mut self, expr: &'ctx AstExpr<'ctx>, var: Locatable<StrT>) -> Self::Output {
        self.context.hir_expr(Expr {
            kind: ExprKind::Variable(
                Var::User(*var),
                self.context.hir_type(Type {
                    kind: TypeKind::Unknown,
                    loc: expr.location(),
                }),
            ),
            loc: expr.location(),
        })
    }

    type LiteralOutput = Literal;
    fn visit_literal(&mut self, literal: &AstLiteral<'ctx>) -> Self::LiteralOutput {
        Literal {
            val: self.visit_literal_val(&literal.val),
            ty: self.visit_type(Locatable::new(&literal.ty, literal.location())),
            loc: literal.location(),
        }
    }

    type LiteralValOutput = LiteralVal;
    fn visit_literal_val(&mut self, val: &AstLiteralVal<'ctx>) -> Self::LiteralValOutput {
        match val {
            &AstLiteralVal::Integer(int) => LiteralVal::Integer(int),
            &AstLiteralVal::Bool(boolean) => LiteralVal::Bool(boolean),
            AstLiteralVal::String(text) => LiteralVal::String(text.clone()),
            &AstLiteralVal::Rune(rune) => LiteralVal::Rune(rune),
            &AstLiteralVal::Float(float) => LiteralVal::Float(float),
            AstLiteralVal::Array(array) => {
                let elements = array
                    .into_iter()
                    .map(|literal| self.visit_literal(literal))
                    .collect();

                LiteralVal::Array { elements }
            }
        }
    }

    fn visit_unary(
        &mut self,
        _expr: &'ctx AstExpr<'ctx>,
        _op: UnaryOp,
        _inner: &'ctx AstExpr<'ctx>,
    ) -> Self::Output {
        todo!()
    }

    fn visit_binary_op(
        &mut self,
        expr: &'ctx AstExpr<'ctx>,
        lhs: &'ctx AstExpr<'ctx>,
        op: BinaryOp,
        rhs: &'ctx AstExpr<'ctx>,
    ) -> Self::Output {
        let lhs = self.visit_expr(lhs);
        let rhs = self.visit_expr(rhs);

        self.context.hir_expr(Expr {
            kind: ExprKind::BinOp(Sided { lhs, op, rhs }),
            loc: expr.location(),
        })
    }

    fn visit_comparison(
        &mut self,
        expr: &'ctx AstExpr<'ctx>,
        lhs: &'ctx AstExpr<'ctx>,
        op: CompOp,
        rhs: &'ctx AstExpr<'ctx>,
    ) -> Self::Output {
        let lhs = self.visit_expr(lhs);
        let rhs = self.visit_expr(rhs);

        self.context.hir_expr(Expr {
            kind: ExprKind::Comparison(Sided { lhs, op, rhs }),
            loc: expr.location(),
        })
    }

    fn visit_assign(
        &mut self,
        expr: &'ctx AstExpr<'ctx>,
        lhs: &'ctx AstExpr<'ctx>,
        op: AssignKind,
        rhs: &'ctx AstExpr<'ctx>,
    ) -> Self::Output {
        let var = if let AstExpr {
            kind: AstExprKind::Variable(var),
            ..
        } = lhs
        {
            Var::User(**var)
        } else {
            todo!()
        };

        if let AssignKind::BinaryOp(op) = op {
            let lhs = self.visit_expr(lhs);
            let rhs = self.visit_expr(rhs);

            self.context.hir_expr(Expr {
                kind: ExprKind::Assign(
                    var,
                    self.context.hir_expr(Expr {
                        kind: ExprKind::BinOp(Sided { lhs, op, rhs }),
                        loc: expr.location(),
                    }),
                ),
                loc: expr.location(),
            })
        } else {
            let kind = ExprKind::Assign(var, self.visit_expr(rhs));

            self.context.hir_expr(Expr {
                kind,
                loc: expr.location(),
            })
        }
    }

    fn visit_paren(
        &mut self,
        _expr: &'ctx AstExpr<'ctx>,
        _inner: &'ctx AstExpr<'ctx>,
    ) -> Self::Output {
        todo!()
    }

    fn visit_array(
        &mut self,
        _expr: &'ctx AstExpr<'ctx>,
        _elements: &[&'ctx AstExpr<'ctx>],
    ) -> Self::Output {
        todo!()
    }

    fn visit_tuple(
        &mut self,
        _expr: &'ctx AstExpr<'ctx>,
        _elements: &[&'ctx AstExpr<'ctx>],
    ) -> Self::Output {
        todo!()
    }

    fn visit_range(
        &mut self,
        _expr: &'ctx AstExpr<'ctx>,
        _start: &'ctx AstExpr<'ctx>,
        _end: &'ctx AstExpr<'ctx>,
    ) -> Self::Output {
        todo!()
    }

    fn visit_index(
        &mut self,
        _expr: &'ctx AstExpr<'ctx>,
        _var: &'ctx AstExpr<'ctx>,
        _index: &'ctx AstExpr<'ctx>,
    ) -> Self::Output {
        todo!()
    }

    fn visit_func_call(
        &mut self,
        expr: &'ctx AstExpr<'ctx>,
        caller: &'ctx AstExpr<'ctx>,
        args: &[&'ctx AstExpr<'ctx>],
    ) -> Self::Output {
        let args = args.iter().map(|a| self.visit_expr(a)).collect();

        self.context.hir_expr(Expr {
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
        _expr: &'ctx AstExpr<'ctx>,
        _member: &'ctx AstExpr<'ctx>,
        _func: &'ctx AstExpr<'ctx>,
    ) -> Self::Output {
        todo!()
    }

    fn visit_reference(
        &mut self,
        expr: &'ctx AstExpr<'ctx>,
        mutable: bool,
        reference: &'ctx AstExpr<'ctx>,
    ) -> Self::Output {
        let reference = self.visit_expr(reference);

        self.context.hir_expr(Expr {
            kind: ExprKind::Reference(Reference { mutable, reference }),
            loc: expr.location(),
        })
    }

    fn visit_cast(
        &mut self,
        expr: &'ctx AstExpr<'ctx>,
        cast: &'ctx AstExpr<'ctx>,
        ty: Locatable<&'ctx AstType<'ctx>>,
    ) -> Self::Output {
        let casted = self.visit_expr(cast);
        let ty = self.visit_type(ty);

        self.context.hir_expr(Expr {
            kind: ExprKind::Cast(Cast { casted, ty }),
            loc: expr.location(),
        })
    }

    type BindingOutput = Binding;
    fn visit_binding(
        &mut self,
        &AstBinding {
            reference,
            mutable,
            ref pattern,
            ty,
        }: &AstBinding<'ctx>,
    ) -> Self::BindingOutput {
        Binding {
            reference,
            mutable,
            pattern: self.visit_pattern(pattern),
            ty: ty.map(|ty| self.visit_type(ty)),
        }
    }

    type PatternOutput = Pattern;
    fn visit_pattern(&mut self, pattern: &AstPattern<'ctx>) -> Self::PatternOutput {
        match pattern {
            AstPattern::Literal(lit) => Pattern::Literal(self.visit_literal(lit)),
            &AstPattern::Ident(ident) => Pattern::Ident(ident),
            AstPattern::ItemPath(path) => Pattern::ItemPath(path.clone()),
            AstPattern::Wildcard => Pattern::Wildcard,
        }
    }
}

impl<'ctx> TypeVisitor<'ctx> for Ladder<'ctx> {
    type Output = TypeId;

    fn visit_type(&mut self, r#type: Locatable<&'ctx AstType<'ctx>>) -> TypeId {
        let kind = self.visit_type_kind(*r#type);

        self.context.hir_type(Type {
            kind,
            loc: r#type.location(),
        })
    }
}

impl<'ctx> Ladder<'ctx> {
    fn visit_type_kind(&mut self, r#type: &'ctx AstType<'ctx>) -> TypeKind {
        match r#type {
            AstType::Unknown => TypeKind::Unknown,
            AstType::Unit => TypeKind::Unit,
            AstType::Bool => TypeKind::Bool,
            AstType::String => TypeKind::String,
            &AstType::Integer { signed, width } => TypeKind::Integer { signed, width },

            &AstType::Array { element, length } => {
                let element = self.visit_type(element);

                TypeKind::Array { element, length }
            }

            &AstType::Slice { element } => {
                let element = self.visit_type(element);

                TypeKind::Slice { element }
            }

            &AstType::Pointer { pointee, mutable } => {
                let pointee = self.visit_type(pointee);

                TypeKind::Pointer { pointee, mutable }
            }

            &AstType::Reference { referee, mutable } => {
                let referee = self.visit_type(referee);

                TypeKind::Reference { referee, mutable }
            }

            ty => todo!("{:?}", ty),
        }
    }
}
