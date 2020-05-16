use crunch_parser::{
    parser::{
        Ast, Expression as AstExpr, Function as AstFunction, Integer, ItemPath, Literal, Sign,
        Statement as AstStmt,
    },
    symbol_table::{Graph, MaybeSym, NodeId, Scope},
};

use std::collections::HashMap;

#[test]
fn test() {
    use crunch_parser::{CurrentFile, FileId, Interner, Parser};

    let source = r#"
    fn main()
        let greeting := "Hello from Crunch!"
        println(greeting)
    end
    "#;

    let interner = Interner::default();
    let (tree, mut interner, _warnings, module_table, module_scope) = Parser::new(
        source,
        CurrentFile::new(FileId::new(0), source.len()),
        interner,
    )
    .parse()
    .unwrap();

    let ladder = Ladder::new(
        module_table,
        module_scope,
        ItemPath::new(interner.intern("package")),
    );

    let hir = ladder.lower(&*tree);

    let mut builtins: HashMap<ItemPath, fn(&[Literal])> = HashMap::new();
    builtins.insert(ItemPath::new(interner.intern("println")), |args| {
        for arg in args {
            println!("{}", arg);
        }
    });

    let mut interp = Interpreter {
        vars: HashMap::new(),
        funcs: HashMap::new(),
        builtins,
    };
    interp.eval_func(if let Hir::Function(f) = hir.first().unwrap() {
        f
    } else {
        todo!()
    });
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
                .map(|stmt| self.lower_statement(stmt))
                .collect(),
        };

        Hir::Function(func)
    }

    fn lower_statement(&self, stmt: &AstStmt<'_, '_>) -> Stmt {
        match stmt {
            AstStmt::Expression(expr) => Stmt::Expr(self.lower_expr(expr)),
            AstStmt::VarDeclaration { name, val, .. } => Stmt::VarDecl(VarDecl {
                name: ItemPath::new(*name),
                value: self.lower_expr(val),
            }),

            _ => todo!(),
        }
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

            e => todo!("{:#?}", e),
        }
    }
}

pub struct Interpreter {
    vars: HashMap<ItemPath, Literal>,
    funcs: HashMap<ItemPath, Function>,
    builtins: HashMap<ItemPath, fn(&[Literal])>,
}

impl Interpreter {
    fn eval_func(&mut self, func: &Function) {
        for stmt in func.body.iter() {
            self.eval_stmt(stmt);
        }
    }

    fn eval_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::VarDecl(decl) => {
                let val = self.eval_expr(&decl.value);
                self.vars.insert(decl.name.clone(), val);
            }
            Stmt::Expr(expr) => {
                self.eval_expr(expr);
            }
        }
    }

    fn eval_expr(&mut self, expr: &Expr) -> Literal {
        match expr {
            Expr::FnCall(func) => {
                let args: Vec<Literal> = func.params.iter().map(|e| self.eval_expr(e)).collect();

                if let Some(f) = self.builtins.get(&func.func) {
                    f(&args);

                    return Literal::Integer(Integer {
                        sign: Sign::Positive,
                        bits: 0,
                    });
                }

                todo!()
            }
            Expr::Literal(lit) => lit.clone(),
            Expr::Var(path) => self.vars.get(path).unwrap().clone(),
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
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncCall {
    func: ItemPath,
    params: Vec<Expr>,
}
