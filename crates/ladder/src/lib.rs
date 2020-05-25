use crunch_parser::{
    parser::{
        Ast, ComparisonOperand, Expression as AstExpr, Function as AstFunction, ItemPath, Literal,
        Statement as AstStmt,
    },
    symbol_table::{Graph, MaybeSym, NodeId, Scope},
};

#[test]
fn test() {
    use crunch_parser::{CurrentFile, FileId, Interner, Parser};

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

    let interner = Interner::default();
    let mut files = crunch_parser::Files::new();
    files.add("<test>", source);

    match Parser::new(
        source,
        CurrentFile::new(FileId::new(0), source.len()),
        interner,
    )
    .parse()
    {
        Ok((tree, mut interner, mut warnings, module_table, module_scope)) => {
            warnings.emit(&files);

            println!("{:#?}", &module_scope);

            let ladder = Ladder::new(
                module_table,
                module_scope,
                ItemPath::new(interner.intern("package")),
            );

            println!("{:#?}", ladder.lower(&*tree));
        }

        Err(mut err) => {
            err.emit(&files);
        }
    }
}

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

    pub fn lower(&self, nodes: &[Ast<'_, '_>]) -> Vec<Hir> {
        nodes
            .iter()
            .filter_map(|node| self.lower_node(node))
            .collect()
    }

    pub fn lower_node(&self, node: &Ast<'_, '_>) -> Option<Hir> {
        match node {
            Ast::Function(function) => Some(self.lower_function(function)),
            Ast::Type(_type_decl) => todo!(),
            Ast::Enum(_enumeration) => todo!(),
            Ast::Trait(_trait_decl) => todo!(),
            Ast::ExtendBlock(_extend_block) => todo!(),

            Ast::Alias(..) | Ast::Import(..) => None,
        }
    }

    fn lower_function(&self, function: &AstFunction<'_, '_>) -> Hir {
        let func = Function {
            name: self.module_path.join(function.name),
            // TODO: Parse this out
            visibility: ItemVis::Local,
            params: function
                .args
                .iter()
                .map(|arg| {
                    self.module_table
                        .node(self.module_scope)
                        .unwrap()
                        .resolve(&self.module_table, *arg.name)
                        .unwrap()
                })
                .collect(),
            body: function
                .body
                .iter()
                .filter_map(|stmt| self.lower_statement(stmt))
                .collect(),
        };

        Hir::Function(func)
    }

    fn lower_statement(&self, stmt: &AstStmt<'_, '_>) -> Option<Stmt> {
        let stmt = match stmt {
            AstStmt::Expression(expr) => Stmt::Expr(self.lower_expr(expr)),

            AstStmt::VarDeclaration { name, val, .. } => Stmt::VarDecl(VarDecl {
                name: ItemPath::new(*name),
                value: self.lower_expr(val),
            }),

            AstStmt::Match { var, arms: _ } => Stmt::Match(Match {
                condition: self.lower_expr(var),
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
            }),

            AstStmt::If {
                condition,
                body,
                clauses,
                else_clause,
            } => {
                let mut arms =
                    Vec::with_capacity(1 + clauses.len() + else_clause.is_some() as usize);

                if clauses.is_empty() {
                    arms.push(MatchArm {
                        condition: Expr::Literal(Literal::Bool(true)),
                        body: body
                            .iter()
                            .filter_map(|stmt| self.lower_statement(stmt))
                            .collect(),
                    });

                    if let Some(body) = else_clause {
                        arms.push(MatchArm {
                            condition: Expr::Literal(Literal::Bool(false)),
                            body: body
                                .iter()
                                .map(|s| self.lower_statement(s).unwrap())
                                .collect(),
                        });
                    }

                    Stmt::Match(Match {
                        condition: self.lower_expr(condition),
                        arms,
                    })
                } else {
                    todo!()
                }
            }

            AstStmt::Loop { body, _else_clause } => Stmt::Loop(
                body.iter()
                    .filter_map(|stmt| self.lower_statement(stmt))
                    .collect(),
            ),

            AstStmt::Empty => return None,

            AstStmt::Return(val) => Stmt::Return(Return {
                val: val.as_ref().map(|expr| self.lower_expr(expr)),
            }),

            AstStmt::Break(val) => Stmt::Break(Break {
                val: val.as_ref().map(|expr| self.lower_expr(expr)),
            }),

            AstStmt::Continue => Stmt::Continue,

            _ => todo!(),
        };

        Some(stmt)
    }

    fn lower_expr(&self, expr: &AstExpr<'_>) -> Expr {
        match expr {
            AstExpr::Literal(lit) => Expr::Literal(lit.clone()),
            AstExpr::FunctionCall { caller, arguments } => Expr::FnCall(FuncCall {
                func: if let AstExpr::Variable(path) = **caller {
                    ItemPath::new(path)
                } else {
                    todo!()
                },
                params: arguments.iter().map(|a| self.lower_expr(a)).collect(),
            }),
            AstExpr::Variable(var) => Expr::Var(ItemPath::new(*var)),
            AstExpr::Comparison(lhs, op, rhs) => Expr::Comparison(
                Box::new(self.lower_expr(lhs)),
                *op,
                Box::new(self.lower_expr(rhs)),
            ),

            e => todo!("{:#?}", e),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Hir {
    Function(Function),
}

#[derive(Debug, Clone, PartialEq)]
pub enum ItemVis {
    Module,
    Package,
    Local,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    name: ItemPath,
    visibility: ItemVis,
    params: Vec<ItemPath>,
    body: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    VarDecl(VarDecl),
    Expr(Expr),
    Match(Match),
    Scope(Vec<Stmt>),
    Loop(Vec<Stmt>),
    Return(Return),
    Continue,
    Break(Break),
}

#[derive(Debug, Clone, PartialEq)]
pub struct VarDecl {
    name: ItemPath,
    value: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    FnCall(FuncCall),
    Literal(Literal),
    Var(ItemPath),
    Comparison(Box<Expr>, ComparisonOperand, Box<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncCall {
    func: ItemPath,
    params: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Match {
    condition: Expr,
    arms: Vec<MatchArm>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm {
    condition: Expr,
    body: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Return {
    val: Option<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Break {
    val: Option<Expr>,
}
