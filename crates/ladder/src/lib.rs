use crunch_shared::{
    end_timer,
    error::{Locatable, Location},
    start_timer,
    strings::StrT,
    trees::{
        ast::{
            Arm as AstMatchArm, AssignKind, BinaryOp, Block as AstBlock, CompOp, Dest as AstDest,
            Exposure as AstExposure, Expr as AstExpr, ExprKind as AstExprKind, For as AstFor,
            FuncArg as AstFuncArg, If as AstIf, IfCond as AstIfCond, Item as AstItem, ItemPath,
            Literal, Loop as AstLoop, Match as AstMatch, Stmt as AstStmt, Type as AstType,
            TypeMember as AstTypeMember, UnaryOp, VarDecl as AstVarDecl, Variant as AstVariant,
            While as AstWhile,
        },
        hir::{
            Binding, Block, Break, Expr, ExprKind, FuncArg, FuncCall, Function, Item, Match,
            MatchArm, Pattern, Return, Stmt, Type, TypeKind, Var, VarDecl,
        },
        Ref, Sided,
    },
    visitors::ast::{ExprVisitor, ItemVisitor, StmtVisitor},
};

pub struct Ladder {}

impl Ladder {
    pub fn new() -> Self {
        Self {}
    }

    pub fn lower(&mut self, items: &[AstItem]) -> Vec<Item> {
        let timer = start_timer!("hir lowering");

        let lowered = items.iter().map(|item| self.visit_item(item)).collect();

        end_timer!("hir lowering", timer);
        lowered
    }

    fn visit_then_and_else(
        &mut self,
        scope: &mut Block<Stmt>,
        loop_broken: Var,
        then: &Option<AstBlock>,
        else_: &Option<AstBlock>,
    ) {
        fn block_statement(
            then_loc: Location,
            else_loc: Location,
            loop_broken: Var,
            then_block: Block<Stmt>,
            else_block: Block<Stmt>,
        ) -> Stmt {
            let location = Location::merge(then_loc, else_loc);

            Stmt::Expr(Expr {
                kind: ExprKind::Match(Match {
                    cond: Ref::new(Expr {
                        kind: ExprKind::Comparison(Sided {
                            lhs: Ref::new(Expr {
                                kind: ExprKind::Variable(loop_broken, TypeKind::Bool),
                                loc: location,
                            }),
                            op: CompOp::Equal,
                            rhs: Ref::new(Expr {
                                kind: ExprKind::Literal(Literal::Bool(true)),
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
                                pattern: Pattern::Literal(Literal::Bool(true)),
                                ty: None,
                            },
                            guard: None,
                            body: else_block,
                            ty: TypeKind::Unit, // TODO: Allow returning values from loops
                        },
                        // If the loop was unbroken, execute the `then` block
                        MatchArm {
                            bind: Binding {
                                reference: false,
                                mutable: false,
                                pattern: Pattern::Literal(Literal::Bool(false)),
                                ty: None,
                            },
                            guard: None,
                            body: then_block,
                            ty: TypeKind::Unit, // TODO: Allow returning values from loops
                        },
                    ],
                    ty: TypeKind::Unit, // TODO: Allow returning values from loops
                }),
                loc: location,
            })
        }

        // TODO: Handle these blocks for *any* breaks, not just condition ones
        match (then, else_) {
            (Some(then), Some(else_)) => {
                scope.push(block_statement(
                    then.location(),
                    else_.location(),
                    loop_broken,
                    Block::from_iter(then.location(), then.iter().map(|s| self.visit_stmt(s))),
                    Block::from_iter(else_.location(), else_.iter().map(|s| self.visit_stmt(s))),
                ));
            }

            (Some(then), None) => {
                scope.push(block_statement(
                    then.location(),
                    then.location(),
                    loop_broken,
                    Block::from_iter(then.location(), then.iter().map(|s| self.visit_stmt(s))),
                    Block::new(then.location()),
                ));
            }

            (None, Some(else_)) => {
                scope.push(block_statement(
                    else_.location(),
                    else_.location(),
                    loop_broken,
                    Block::new(else_.location()),
                    Block::from_iter(else_.location(), else_.iter().map(|s| self.visit_stmt(s))),
                ));
            }

            (None, None) => {}
        }
    }
}

impl ItemVisitor for Ladder {
    type Output = Item;

    fn visit_func(
        &mut self,
        item: &AstItem,
        _generics: &[AstType],
        args: &[AstFuncArg],
        body: &AstBlock,
        ret: Locatable<&AstType>,
        sig: Location,
    ) -> Self::Output {
        let name = ItemPath::from(vec![item.name.unwrap()]);
        let args = args
            .iter()
            .map(|AstFuncArg { name, ty, loc }| {
                let kind = TypeKind::from(&**ty);

                FuncArg {
                    name: Var::User(*name),
                    kind,
                    loc: *loc,
                }
            })
            .collect();

        let body = Block::from_iter(
            body.location(),
            body.iter().map(|stmt| self.visit_stmt(stmt)),
        );

        let func = Function {
            name,
            vis: item.vis.expect("Functions should have a visibility"),
            args,
            body,
            ret: Type {
                name: ItemPath::default(), // FIXME: ???
                kind: TypeKind::from(*ret.data()),
                loc: ret.loc(),
            },
            loc: item.location(),
            sig,
        };

        Item::Function(func)
    }

    fn visit_type(
        &mut self,
        _item: &AstItem,
        _generics: &[AstType],
        _members: &[AstTypeMember],
    ) -> Self::Output {
        todo!()
    }

    fn visit_enum(
        &mut self,
        _item: &AstItem,
        _generics: &[AstType],
        _variants: &[AstVariant],
    ) -> Self::Output {
        todo!()
    }

    fn visit_trait(
        &mut self,
        _item: &AstItem,
        _generics: &[AstType],
        _methods: &[AstItem],
    ) -> Self::Output {
        todo!()
    }

    fn visit_import(
        &mut self,
        _item: &AstItem,
        _file: &ItemPath,
        _dest: &AstDest,
        _exposes: &AstExposure,
    ) -> Self::Output {
        todo!()
    }

    fn visit_extend_block(
        &mut self,
        _item: &AstItem,
        _target: &AstType,
        _extender: Option<&AstType>,
        _items: &[AstItem],
    ) -> Self::Output {
        todo!()
    }

    fn visit_alias(
        &mut self,
        _item: &AstItem,
        _alias: &AstType,
        _actual: &AstType,
    ) -> Self::Output {
        todo!()
    }
}

impl StmtVisitor for Ladder {
    type Output = Stmt;

    fn visit_var_decl(&mut self, stmt: &AstStmt, var: &AstVarDecl) -> Stmt {
        Stmt::VarDecl(VarDecl {
            name: Var::User(var.name),
            value: Ref::new(self.visit_expr(&*var.val)),
            ty: Type {
                name: ItemPath::new(var.name),
                kind: TypeKind::from(&*var.ty),
                loc: stmt.location(),
            },
            loc: stmt.location(),
        })
    }
}

impl ExprVisitor for Ladder {
    type Output = Expr;

    fn visit_if(&mut self, expr: &AstExpr, AstIf { clauses, else_ }: &AstIf) -> Self::Output {
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
                        pattern: Pattern::Literal(Literal::Bool(true)),
                        ty: None,
                    },
                    guard: None,
                    body: Block::from_iter(
                        body.location(),
                        body.iter().map(|stmt| self.visit_stmt(stmt)),
                    ),
                    ty: TypeKind::Infer,
                },
                MatchArm {
                    bind: Binding {
                        reference: false,
                        mutable: false,
                        pattern: Pattern::Literal(Literal::Bool(false)),
                        ty: None,
                    },
                    guard: None,
                    body: if let Some(else_) = else_ {
                        Block::from_iter(else_.location(), else_.iter().map(|s| self.visit_stmt(s)))
                    } else {
                        Block::new(cond.location())
                    },
                    ty: TypeKind::Infer,
                },
            ]);

            Expr {
                kind: ExprKind::Match(Match {
                    cond: Ref::new(self.visit_expr(&*cond)),
                    arms,
                    ty: TypeKind::Infer,
                }),
                loc: dbg!(expr.location()),
            }
        } else {
            for AstIfCond { cond, body } in clauses {
                arms.push(MatchArm {
                    bind: Binding {
                        reference: false,
                        mutable: false,
                        pattern: Pattern::Wildcard,
                        ty: None,
                    },
                    guard: Some(Ref::new(self.visit_expr(cond))),
                    body: Block::from_iter(
                        body.location(),
                        body.iter().map(|stmt| self.visit_stmt(stmt)),
                    ),
                    ty: TypeKind::Infer,
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
                        body.iter().map(|s| self.visit_stmt(s)),
                    ),
                    ty: TypeKind::Infer,
                });
            }

            Expr {
                kind: ExprKind::Match(Match {
                    cond: Ref::new(Expr {
                        kind: ExprKind::Literal(Literal::Bool(true)),
                        loc: expr.location(),
                    }),
                    arms,
                    ty: TypeKind::Infer,
                }),
                loc: expr.location(),
            }
        }
    }

    fn visit_return(&mut self, expr: &AstExpr, value: Option<&AstExpr>) -> Self::Output {
        Expr {
            kind: ExprKind::Return(Return {
                val: value.map(|expr| Ref::new(self.visit_expr(expr))),
            }),
            loc: expr.location(),
        }
    }

    fn visit_break(&mut self, expr: &AstExpr, value: Option<&AstExpr>) -> Self::Output {
        Expr {
            kind: ExprKind::Break(Break {
                val: value.map(|expr| Ref::new(self.visit_expr(expr))),
            }),
            loc: expr.location(),
        }
    }

    fn visit_continue(&mut self, expr: &AstExpr) -> Self::Output {
        Expr {
            kind: ExprKind::Continue,
            loc: expr.location(),
        }
    }

    fn visit_while(
        &mut self,
        expr: &AstExpr,
        AstWhile {
            cond,
            body: ast_body,
            then,
            else_,
        }: &AstWhile,
    ) -> Self::Output {
        let mut scope: Block<Stmt> = Block::with_capacity(
            expr.location(),
            2 + (then.is_some() as usize * 2) + (else_.is_some() as usize * 2),
        );

        let loop_broken = Var::Auto(0);
        scope.push(Stmt::VarDecl(VarDecl {
            name: loop_broken,
            value: Ref::new(Expr {
                kind: ExprKind::Literal(Literal::Bool(false)),
                loc: cond.location(),
            }),
            ty: Type {
                name: ItemPath::default(), // TODO: ????
                kind: TypeKind::Bool,
                loc: expr.location(),
            },
            loc: cond.location(),
        }));

        let mut body: Block<Stmt> = Block::with_capacity(ast_body.location(), ast_body.len() + 1);
        body.push(Stmt::Expr(Expr {
            kind: ExprKind::Match(Match {
                cond: Ref::new(self.visit_expr(cond)),
                arms: vec![
                    // If the `while` condition is true, do nothing
                    // TODO: `likely` annotation?
                    // TODO: `generated` annotation?
                    MatchArm {
                        bind: Binding {
                            reference: false,
                            mutable: false,
                            pattern: Pattern::Literal(Literal::Bool(true)),
                            ty: None,
                        },
                        guard: None,
                        body: Block::new(cond.location()),
                        ty: TypeKind::Infer,
                    },
                    // If the `while` condition returns false, set the status and break
                    MatchArm {
                        bind: Binding {
                            reference: false,
                            mutable: false,
                            pattern: Pattern::Literal(Literal::Bool(false)),
                            ty: None,
                        },
                        guard: None,
                        body: Block::from_iter(
                            cond.location(),
                            vec![
                                // Set the loop status to true since we've broken it
                                Stmt::Expr(Expr {
                                    kind: ExprKind::Assign(
                                        loop_broken,
                                        Ref::new(Expr {
                                            kind: ExprKind::Literal(Literal::Bool(true)),
                                            loc: cond.location(),
                                        }),
                                    ),
                                    loc: cond.location(),
                                }),
                                // Break from the loop
                                Stmt::Expr(Expr {
                                    kind: ExprKind::Break(Break { val: None }),
                                    loc: cond.location(),
                                }),
                            ],
                        ),
                        ty: TypeKind::Infer,
                    },
                ],
                ty: TypeKind::Infer,
            }),
            loc: cond.location(),
        }));

        body.extend(ast_body.iter().map(|s| self.visit_stmt(s)));

        scope.push(Stmt::Expr(Expr {
            kind: ExprKind::Loop(body),
            loc: expr.location(),
        }));

        self.visit_then_and_else(&mut scope, loop_broken, then, else_);

        Expr {
            kind: ExprKind::Scope(scope),
            loc: expr.location(),
        }
    }

    fn visit_loop(&mut self, expr: &AstExpr, AstLoop { body, else_: _ }: &AstLoop) -> Self::Output {
        Expr {
            kind: ExprKind::Loop(Block::from_iter(
                body.location(),
                body.iter().map(|stmt| self.visit_stmt(stmt)),
            )),
            loc: expr.location(),
        }
    }

    fn visit_for(&mut self, _expr: &AstExpr, _for: &AstFor) -> Self::Output {
        todo!("Desugar `for` conditions to iterators")
    }

    fn visit_match(&mut self, expr: &AstExpr, AstMatch { var, arms }: &AstMatch) -> Self::Output {
        Expr {
            kind: ExprKind::Match(Match {
                cond: Ref::new(self.visit_expr(&*var)),

                arms: arms
                    .iter()
                    .map(|AstMatchArm { bind, guard, body }| MatchArm {
                        bind: Binding {
                            reference: bind.reference,
                            mutable: bind.mutable,
                            pattern: Pattern::from(bind.pattern.clone()),
                            ty: bind.ty.as_ref().map(|ty| {
                                Ref::new(Type {
                                    name: ItemPath::default(), // FIXME: ???
                                    kind: TypeKind::from(&**ty),
                                    loc: expr.location(),
                                })
                            }),
                        },
                        guard: guard
                            .as_ref()
                            .map(|guard| Ref::new(self.visit_expr(&**guard))),
                        body: Block::from_iter(
                            body.location(),
                            body.iter().map(|stmt| self.visit_stmt(stmt)),
                        ),
                        ty: TypeKind::Infer,
                    })
                    .collect(),

                // arms
                // .iter()
                // // TODO: Patterns with matches
                // .map(|(var, _clause, body)| MatchArm {
                //     condition: self.lower_expr(&AstExpr::Variable(*var)),
                //     body: body
                //         .iter()
                //         .filter_map(|stmt| self.lower_statement(stmt))
                //         .collect(),
                // })
                // .collect(),
                ty: TypeKind::Infer,
            }),
            loc: expr.location(),
        }
    }

    fn visit_variable(&mut self, expr: &AstExpr, var: StrT) -> Self::Output {
        Expr {
            kind: ExprKind::Variable(Var::User(var), TypeKind::Infer),
            loc: expr.location(),
        }
    }

    fn visit_literal(&mut self, expr: &AstExpr, literal: &Literal) -> Self::Output {
        Expr {
            kind: ExprKind::Literal(literal.clone()),
            loc: expr.location(),
        }
    }

    fn visit_unary(&mut self, _expr: &AstExpr, _op: UnaryOp, _inner: &AstExpr) -> Self::Output {
        todo!()
    }

    fn visit_binary_op(
        &mut self,
        _expr: &AstExpr,
        _lhs: &AstExpr,
        _op: BinaryOp,
        _rhs: &AstExpr,
    ) -> Self::Output {
        todo!()
    }

    fn visit_comparison(
        &mut self,
        expr: &AstExpr,
        lhs: &AstExpr,
        op: CompOp,
        rhs: &AstExpr,
    ) -> Self::Output {
        Expr {
            kind: ExprKind::Comparison(Sided {
                lhs: Ref::new(self.visit_expr(lhs)),
                op,
                rhs: Ref::new(self.visit_expr(rhs)),
            }),
            loc: expr.location(),
        }
    }

    fn visit_assign(
        &mut self,
        expr: &AstExpr,
        lhs: &AstExpr,
        op: AssignKind,
        rhs: &AstExpr,
    ) -> Self::Output {
        let var = if let AstExpr {
            kind: AstExprKind::Variable(var),
            ..
        } = lhs
        {
            Var::User(*var)
        } else {
            todo!()
        };

        if let AssignKind::BinaryOp(op) = op {
            Expr {
                kind: ExprKind::Assign(
                    var,
                    Ref::new(Expr {
                        kind: ExprKind::BinOp(Sided {
                            lhs: Ref::new(self.visit_expr(lhs)),
                            op: op,
                            rhs: Ref::new(self.visit_expr(rhs)),
                        }),
                        loc: expr.location(),
                    }),
                ),
                loc: expr.location(),
            }
        } else {
            Expr {
                kind: ExprKind::Assign(var, Ref::new(self.visit_expr(rhs))),
                loc: expr.location(),
            }
        }
    }

    fn visit_paren(&mut self, _expr: &AstExpr, _inner: &AstExpr) -> Self::Output {
        todo!()
    }

    fn visit_array(&mut self, _expr: &AstExpr, _elements: &[AstExpr]) -> Self::Output {
        todo!()
    }

    fn visit_tuple(&mut self, _expr: &AstExpr, _elements: &[AstExpr]) -> Self::Output {
        todo!()
    }

    fn visit_range(&mut self, _expr: &AstExpr, _start: &AstExpr, _end: &AstExpr) -> Self::Output {
        todo!()
    }

    fn visit_index(&mut self, _expr: &AstExpr, _var: &AstExpr, _index: &AstExpr) -> Self::Output {
        todo!()
    }

    fn visit_func_call(
        &mut self,
        expr: &AstExpr,
        caller: &AstExpr,
        args: &[AstExpr],
    ) -> Self::Output {
        Expr {
            kind: ExprKind::FnCall(FuncCall {
                func: if let AstExprKind::Variable(path) = caller.kind {
                    ItemPath::new(path)
                } else {
                    todo!()
                },
                args: args.iter().map(|a| self.visit_expr(a)).collect(),
            }),
            loc: expr.location(),
        }
    }

    fn visit_member_func_call(
        &mut self,
        _expr: &AstExpr,
        _member: &AstExpr,
        _func: &AstExpr,
    ) -> Self::Output {
        todo!()
    }
}

#[test]
fn test() {
    use crunch_parser::Parser;
    use crunch_shared::{
        context::Context,
        files::{CurrentFile, FileId},
        symbol_table::Resolver,
    };

    let source = r#"
    fn main()
        let greeting := 10
        printf(greeting)
    end
    "#;

    let ctx = Context::default();
    let mut files = crunch_shared::files::Files::new();
    files.add("<test>", source);

    match Parser::new(
        source,
        CurrentFile::new(FileId::new(0), source.len()),
        ctx.clone(),
    )
    .parse()
    {
        Ok((items, mut warnings)) => {
            warnings.emit(&files);

            let mut resolver = Resolver::new(vec![ctx.strings.intern("<test>")].into());
            for item in items.iter() {
                resolver.visit_item(item);
            }
            resolver.finalize();
            println!("{:#?}", resolver);

            // println!("Nodes: {:#?}", &items);
            // println!("Symbols: {:#?}", &module_scope);

            let mut _ladder = Ladder::new();

            // println!("HIR: {:#?}", ladder.lower(&items));
        }

        Err(mut err) => {
            err.emit(&files);
        }
    }
}
