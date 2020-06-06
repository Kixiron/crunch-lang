use core::ops::Deref;
use crunch_parser::symbol_table::{Graph, MaybeSym, NodeId, Scope};
use crunch_shared::ast::{CompOp, Expr as ItemExpr, Item, ItemPath, Literal, Stmt as ItemStmt};

#[test]
fn test() {
    use crunch_parser::{Context, CurrentFile, Parser};
    use crunch_shared::files::FileId;

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

            println!("HIR: {:#?}", ladder.lower(&ast));
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

    pub fn lower(&self, nodes: &[impl Deref<Target = Item>]) -> Vec<Hir> {
        nodes
            .iter()
            .filter_map(|node| self.lower_node(&*node))
            .collect()
    }

    pub fn lower_node(&self, node: &Item) -> Option<Hir> {
        match node {
            ItemKind::Function(function) => Some(self.lower_function(function)),
            _ => todo!(),
        }
    }

    fn lower_function(&self, function: &ItemFunction) -> Hir {
        let func = Function {
            name: self.module_path.join(function.name),
            // TODO: Parse this out
            visibility: ItemVis::Local,
            params: function
                .args
                .iter()
                .map(|arg| {
                    let name = self
                        .module_table
                        .node(self.module_scope)
                        .unwrap()
                        .resolve(&self.module_table, *arg.name)
                        .unwrap();
                    let kind = TypeKind::from(&**arg.ty);

                    TypeInfo { name, kind }
                })
                .collect(),
            body: function
                .body
                .iter()
                .filter_map(|stmt| self.lower_statement(stmt))
                .collect(),
            return_ty: TypeKind::from(&**function.returns),
        };

        Hir::Function(func)
    }

    fn lower_statement(&self, stmt: &ItemStmt) -> Option<Stmt> {
        let stmt = match stmt {
            ItemStmt::Expr(expr) => Stmt::Expr(self.lower_expr(expr)),

            ItemStmt::VarDeclaration { name, val, .. } => Stmt::VarDecl(VarDecl {
                name: ItemPath::new(*name),
                value: self.lower_expr(val),
                ty: TypeInfo {
                    name: ItemPath::new(*name),
                    kind: TypeKind::Infer,
                },
            }),

            ItemStmt::Match { var, arms: _ } => Stmt::Match(Match {
                condition: self.lower_expr(var),
                arms: Vec::new(),
                // arms
                // .iter()
                // // TODO: Patterns with matches
                // .map(|(var, _clause, body)| MatchArm {
                //     condition: self.lower_expr(&ItemExpr::Variable(*var)),
                //     body: body
                //         .iter()
                //         .filter_map(|stmt| self.lower_statement(stmt))
                //         .collect(),
                // })
                // .collect(),
                ty: TypeKind::Infer,
            }),

            ItemStmt::If {
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
                        ty: TypeKind::Infer,
                    });

                    if let Some(body) = else_clause {
                        arms.push(MatchArm {
                            condition: Expr::Literal(Literal::Bool(false)),
                            body: body
                                .iter()
                                .filter_map(|s| self.lower_statement(s))
                                .collect(),
                            ty: TypeKind::Infer,
                        });
                    }

                    Stmt::Match(Match {
                        condition: self.lower_expr(condition),
                        arms,
                        ty: TypeKind::Infer,
                    })
                } else {
                    todo!()
                }
            }

            ItemStmt::Loop {
                body,
                else_clause: _else,
            } => Stmt::Loop(
                body.iter()
                    .filter_map(|stmt| self.lower_statement(stmt))
                    .collect(),
            ),

            ItemStmt::Empty => return None,

            ItemStmt::Return(val) => Stmt::Return(Return {
                val: val.as_ref().map(|expr| self.lower_expr(expr)),
            }),

            ItemStmt::Break(val) => Stmt::Break(Break {
                val: val.as_ref().map(|expr| self.lower_expr(expr)),
            }),

            ItemStmt::Continue => Stmt::Continue,

            _ => todo!(),
        };

        Some(stmt)
    }

    fn lower_expr(&self, expr: &ItemExpr) -> Expr {
        match expr {
            ItemExpr::Literal(lit) => Expr::Literal(lit.clone()),
            ItemExpr::FunctionCall { caller, arguments } => Expr::FnCall(FuncCall {
                func: if let ItemExpr::Variable(path) = **caller {
                    ItemPath::new(path)
                } else {
                    todo!()
                },
                params: arguments.iter().map(|a| self.lower_expr(a)).collect(),
            }),
            ItemExpr::Variable(var) => Expr::Var(ItemPath::new(*var)),
            ItemExpr::Comparison(lhs, op, rhs) => Expr::Comparison(
                Box::new(self.lower_expr(lhs)),
                *op,
                Box::new(self.lower_expr(rhs)),
            ),

            e => todo!("{:#?}", e),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeInfo {
    pub name: ItemPath,
    pub kind: TypeKind,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TypeKind {
    Infer,
    Integer,
    String,
    Bool,
    Unit,
}

impl From<&crunch_shared::ast::Type> for TypeKind {
    fn from(ty: &crunch_shared::ast::Type) -> Self {
        use crunch_shared::ast::{Signedness, Type};

        match ty {
            Type::Infer => Self::Infer,
            Type::Unit => Self::Unit,
            Type::Bool => Self::Bool,
            Type::String => Self::String,
            Type::Integer {
                sign: Signedness::Signed,
                width: 32,
            } => Self::Integer,

            _ => todo!(),
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
    pub name: ItemPath,
    pub visibility: ItemVis,
    pub params: Vec<TypeInfo>,
    pub body: Vec<Stmt>,
    pub return_ty: TypeKind,
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
    pub name: ItemPath,
    pub value: Expr,
    pub ty: TypeInfo,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    FnCall(FuncCall),
    Literal(Literal),
    Var(ItemPath),
    Comparison(Box<Expr>, CompOp, Box<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncCall {
    pub func: ItemPath,
    pub params: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Match {
    pub condition: Expr,
    pub arms: Vec<MatchArm>,
    pub ty: TypeKind,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm {
    pub condition: Expr,
    pub body: Vec<Stmt>,
    pub ty: TypeKind,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Return {
    pub val: Option<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Break {
    pub val: Option<Expr>,
}
