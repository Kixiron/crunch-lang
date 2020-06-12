use crunch_shared::{
    end_timer, start_timer,
    strings::StrT,
    symbol_table::{Graph, MaybeSym, NodeId, Scope},
    trees::{
        ast::{
            AssignKind, BinaryOp, Block as AstBlock, CompOp, Dest as AstDest,
            Exposure as AstExposure, Expr as AstExpr, ExprKind as AstExprKind, For as AstFor,
            FuncArg as AstFuncArg, If as AstIf, IfCond as AstIfCond, Item as AstItem, ItemPath,
            Literal, Loop as AstLoop, Match as AstMatch, Stmt as AstStmt, Type as AstType,
            TypeMember as AstTypeMember, UnaryOp, VarDecl as AstVarDecl, Variant as AstVariant,
            While as AstWhile,
        },
        hir::{
            Block, Break, Expr, ExprKind, FuncArg, FuncCall, Function, Item, Match, MatchArm,
            Return, Stmt, Type, TypeKind, VarDecl, Vis,
        },
        Ref, Sided,
    },
    visitors::ast::{ExprVisitor, ItemVisitor, StmtVisitor},
};

pub struct Ladder {
    module_table: Graph<Scope, MaybeSym>,
    module_scope: NodeId,
    module_path: ItemPath,
}

impl Ladder {
    pub fn new(
        module_table: Graph<Scope, MaybeSym>,
        module_scope: NodeId,
        module_path: ItemPath,
    ) -> Self {
        Self {
            module_table,
            module_scope,
            module_path,
        }
    }

    pub fn lower(&mut self, items: &[AstItem]) -> Vec<Item> {
        let timer = start_timer!("hir lowering");

        let lowered = items.iter().map(|item| self.visit_item(item)).collect();

        end_timer!("hir lowering", timer);
        lowered
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
        ret: &AstType,
    ) -> Self::Output {
        let name = self.module_path.join(item.name.unwrap());
        let args = args
            .iter()
            .map(|arg| {
                let name = self
                    .module_table
                    .node(self.module_scope)
                    .unwrap()
                    .resolve(&self.module_table, arg.name)
                    .unwrap();

                let kind = TypeKind::from(&*arg.ty);

                FuncArg {
                    name,
                    kind,
                    loc: arg.location(),
                }
            })
            .collect();

        let body = Block::from_iter(
            body.location(),
            body.iter().map(|stmt| self.visit_stmt(stmt)),
        );

        let func = Function {
            name,
            // TODO: Parse this out
            vis: Vis::FileLocal,
            args,
            body,
            ret: TypeKind::from(ret),
            loc: item.location(),
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
            name: ItemPath::new(var.name),
            value: Ref::new(self.visit_expr(&*var.val)),
            ty: Type {
                name: ItemPath::new(var.name),
                kind: TypeKind::from(&*var.ty),
            },
            loc: stmt.location(),
        })
    }
}

impl ExprVisitor for Ladder {
    type Output = Expr;

    fn visit_if(
        &mut self,
        expr: &AstExpr,
        AstIf {
            cond: AstIfCond { cond, body },
            clauses,
            else_,
        }: &AstIf,
    ) -> Self::Output {
        let mut arms = Vec::with_capacity(1 + clauses.len() + else_.is_some() as usize);

        if clauses.is_empty() {
            arms.push(MatchArm {
                cond: Ref::new(Expr {
                    kind: ExprKind::Literal(Literal::Bool(true)),
                    loc: expr.location(),
                }),
                body: Block::from_iter(
                    body.location(),
                    body.iter().map(|stmt| self.visit_stmt(stmt)),
                ),
                ty: TypeKind::Infer,
            });

            if let Some(body) = else_ {
                arms.push(MatchArm {
                    cond: Ref::new(Expr {
                        kind: ExprKind::Literal(Literal::Bool(false)),
                        loc: expr.location(),
                    }),
                    body: Block::from_iter(
                        body.location(),
                        body.iter().map(|s| self.visit_stmt(s)),
                    ),
                    ty: TypeKind::Infer,
                });
            }

            Expr {
                kind: ExprKind::Match(Match {
                    cond: Ref::new(self.visit_expr(cond)),
                    arms,
                    ty: TypeKind::Infer,
                }),
                loc: expr.location(),
            }
        } else {
            todo!()
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

    fn visit_while(&mut self, _expr: &AstExpr, _while_: &AstWhile) -> Self::Output {
        todo!()
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

    fn visit_for(&mut self, _expr: &AstExpr, _for_: &AstFor) -> Self::Output {
        todo!()
    }

    fn visit_match(&mut self, expr: &AstExpr, match_: &AstMatch) -> Self::Output {
        Expr {
            kind: ExprKind::Match(Match {
                cond: Ref::new(self.visit_expr(&*match_.var)),
                arms: Vec::new(),
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
            kind: ExprKind::Variable(var, TypeKind::Infer),
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
        _expr: &AstExpr,
        _lhs: &AstExpr,
        _op: AssignKind,
        _rhs: &AstExpr,
    ) -> Self::Output {
        todo!()
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
    };

    let source = r#"
    fn main()
        let mut greeting := "Hello from Crunch!"
        println(greeting)

        if greeting == "Hello"
            println("You said hello")
        else
            println("You didn't say hello :(")
        end

        loop
            println("Over and over again")
        end

        match greeting
            string where string == "some string" =>
                println("this can't happen")
            end

            greeting =>
                println("{}", greeting)
            end
        end
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
        Ok((ast, mut warnings, module_table, module_scope)) => {
            warnings.emit(&files);

            println!("Nodes: {:#?}", &ast);
            println!("Symbols: {:#?}", &module_scope);

            let mut ladder = Ladder::new(
                module_table,
                module_scope,
                ItemPath::new(ctx.strings.intern("package")),
            );

            println!("HIR: {:#?}", ladder.lower(&ast));
        }

        Err(mut err) => {
            err.emit(&files);
        }
    }
}
