use crate::{
    strings::StrT,
    trees::{
        hir::{
            Block, Break, CompOp, Expr, FuncCall, Function, Item, Literal, Match, Return, Stmt,
            TypeKind, VarDecl,
        },
        Sided,
    },
};

#[allow(unused_variables)]
pub trait ItemVisitor {
    type Output;

    #[inline]
    fn visit_item(&mut self, item: &mut Item) -> Self::Output {
        match item {
            Item::Function(func) => self.visit_func(func),
        }
    }

    fn visit_func(&mut self, func: &mut Function) -> Self::Output;
}

pub trait StmtVisitor: ItemVisitor + ExprVisitor {
    type Output;

    fn visit_stmt(&mut self, stmt: &mut Stmt) -> <Self as StmtVisitor>::Output;
    fn visit_var_decl(&mut self, var: &mut VarDecl) -> <Self as StmtVisitor>::Output;
}

pub trait ExprVisitor {
    type Output;

    #[inline]
    fn visit_expr(&mut self, expr: &mut Expr) -> Self::Output {
        match expr {
            Expr::Return(value) => self.visit_return(value),
            Expr::Break(value) => self.visit_break(value),
            Expr::Continue => self.visit_continue(),
            Expr::Loop(body) => self.visit_loop(body),
            Expr::Match(match_) => self.visit_match(match_),
            Expr::Variable(var, ty) => self.visit_variable(*var, ty),
            Expr::Literal(literal) => self.visit_literal(literal),
            Expr::Scope(body) => self.visit_scope(body),
            Expr::FnCall(call) => self.visit_func_call(call),
            Expr::Comparison(Sided { lhs, op, rhs }) => self.visit_comparison(lhs, *op, rhs),
        }
    }

    fn visit_return(&mut self, value: &mut Return) -> Self::Output;
    fn visit_break(&mut self, value: &mut Break) -> Self::Output;
    fn visit_continue(&mut self) -> Self::Output;
    fn visit_loop(&mut self, body: &mut Block<Stmt>) -> Self::Output;
    fn visit_match(&mut self, match_: &mut Match) -> Self::Output;
    fn visit_variable(&mut self, var: StrT, ty: &mut TypeKind) -> Self::Output;
    fn visit_literal(&mut self, literal: &mut Literal) -> Self::Output;
    fn visit_scope(&mut self, body: &mut Block<Stmt>) -> Self::Output;
    fn visit_func_call(&mut self, call: &mut FuncCall) -> Self::Output;
    fn visit_comparison(&mut self, lhs: &mut Expr, op: CompOp, rhs: &mut Expr) -> Self::Output;
}
