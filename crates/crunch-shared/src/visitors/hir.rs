use crate::{
    error::Location,
    trees::{
        ast::BinaryOp,
        hir::{
            Block, Break, CompOp, Expr, ExprKind, ExternFunc, FuncCall, Function, Item, Literal,
            Match, Return, Stmt, TypeKind, Var, VarDecl,
        },
        Sided,
    },
};

#[allow(unused_variables)]
pub trait MutItemVisitor {
    type Output;

    #[inline]
    fn visit_item(&mut self, item: &mut Item) -> Self::Output {
        match item {
            Item::Function(func) => self.visit_func(func),
            Item::ExternFunc(func) => self.visit_extern_func(func),
        }
    }

    fn visit_func(&mut self, func: &mut Function) -> Self::Output;
    fn visit_extern_func(&mut self, func: &mut ExternFunc) -> Self::Output;
}

pub trait MutStmtVisitor: MutItemVisitor + MutExprVisitor {
    type Output;

    fn visit_stmt(&mut self, stmt: &mut Stmt) -> <Self as MutStmtVisitor>::Output;
    fn visit_var_decl(&mut self, var: &mut VarDecl) -> <Self as MutStmtVisitor>::Output;
}

pub trait MutExprVisitor {
    type Output;

    #[inline]
    fn visit_expr(&mut self, expr: &mut Expr) -> Self::Output {
        let loc = expr.loc;

        match &mut expr.kind {
            ExprKind::Return(value) => self.visit_return(loc, value),
            ExprKind::Break(value) => self.visit_break(loc, value),
            ExprKind::Continue => self.visit_continue(loc),
            ExprKind::Loop(body) => self.visit_loop(loc, body),
            ExprKind::Match(match_) => self.visit_match(loc, match_),
            ExprKind::Variable(var, ty) => self.visit_variable(loc, *var, ty),
            ExprKind::Literal(literal) => self.visit_literal(loc, literal),
            ExprKind::Scope(body) => self.visit_scope(loc, body),
            ExprKind::FnCall(call) => self.visit_func_call(loc, call),
            ExprKind::Comparison(Sided { lhs, op, rhs }) => {
                self.visit_comparison(loc, lhs, *op, rhs)
            }
            ExprKind::Assign(var, value) => self.visit_assign(loc, *var, value),
            ExprKind::BinOp(Sided { lhs, op, rhs }) => self.visit_binop(loc, lhs, *op, rhs),
        }
    }

    fn visit_return(&mut self, loc: Location, value: &mut Return) -> Self::Output;
    fn visit_break(&mut self, loc: Location, value: &mut Break) -> Self::Output;
    fn visit_continue(&mut self, loc: Location) -> Self::Output;
    fn visit_loop(&mut self, loc: Location, body: &mut Block<Stmt>) -> Self::Output;
    fn visit_match(&mut self, loc: Location, match_: &mut Match) -> Self::Output;
    fn visit_variable(&mut self, loc: Location, var: Var, ty: &mut TypeKind) -> Self::Output;
    fn visit_literal(&mut self, loc: Location, literal: &mut Literal) -> Self::Output;
    fn visit_scope(&mut self, loc: Location, body: &mut Block<Stmt>) -> Self::Output;
    fn visit_func_call(&mut self, loc: Location, call: &mut FuncCall) -> Self::Output;
    fn visit_comparison(
        &mut self,
        loc: Location,
        lhs: &mut Expr,
        op: CompOp,
        rhs: &mut Expr,
    ) -> Self::Output;
    fn visit_assign(&mut self, loc: Location, var: Var, value: &mut Expr) -> Self::Output;
    fn visit_binop(
        &mut self,
        loc: Location,
        lhs: &mut Expr,
        op: BinaryOp,
        rhs: &mut Expr,
    ) -> Self::Output;
}

#[allow(unused_variables)]
pub trait ItemVisitor {
    type Output;

    #[inline]
    fn visit_item(&mut self, item: &Item) -> Self::Output {
        match item {
            Item::Function(func) => self.visit_func(func),
            Item::ExternFunc(func) => self.visit_extern_func(func),
        }
    }

    fn visit_func(&mut self, func: &Function) -> Self::Output;
    fn visit_extern_func(&mut self, func: &ExternFunc) -> Self::Output;
}

pub trait StmtVisitor: ItemVisitor + ExprVisitor {
    type Output;

    fn visit_stmt(&mut self, stmt: &Stmt) -> <Self as StmtVisitor>::Output;
    fn visit_var_decl(&mut self, var: &VarDecl) -> <Self as StmtVisitor>::Output;
}

pub trait ExprVisitor {
    type Output;

    #[inline]
    fn visit_expr(&mut self, expr: &Expr) -> Self::Output {
        let loc = expr.loc;

        match &expr.kind {
            ExprKind::Return(value) => self.visit_return(loc, value),
            ExprKind::Break(value) => self.visit_break(loc, value),
            ExprKind::Continue => self.visit_continue(loc),
            ExprKind::Loop(body) => self.visit_loop(loc, body),
            ExprKind::Match(match_) => self.visit_match(loc, match_),
            ExprKind::Variable(var, ty) => self.visit_variable(loc, *var, ty),
            ExprKind::Literal(literal) => self.visit_literal(loc, literal),
            ExprKind::Scope(body) => self.visit_scope(loc, body),
            ExprKind::FnCall(call) => self.visit_func_call(loc, call),
            ExprKind::Comparison(Sided { lhs, op, rhs }) => {
                self.visit_comparison(loc, lhs, *op, rhs)
            }
            ExprKind::Assign(var, value) => self.visit_assign(loc, *var, value),
            ExprKind::BinOp(Sided { lhs, op, rhs }) => self.visit_binop(loc, lhs, *op, rhs),
        }
    }

    fn visit_return(&mut self, loc: Location, value: &Return) -> Self::Output;
    fn visit_break(&mut self, loc: Location, value: &Break) -> Self::Output;
    fn visit_continue(&mut self, loc: Location) -> Self::Output;
    fn visit_loop(&mut self, loc: Location, body: &Block<Stmt>) -> Self::Output;
    fn visit_match(&mut self, loc: Location, match_: &Match) -> Self::Output;
    fn visit_variable(&mut self, loc: Location, var: Var, ty: &TypeKind) -> Self::Output;
    fn visit_literal(&mut self, loc: Location, literal: &Literal) -> Self::Output;
    fn visit_scope(&mut self, loc: Location, body: &Block<Stmt>) -> Self::Output;
    fn visit_func_call(&mut self, loc: Location, call: &FuncCall) -> Self::Output;
    fn visit_comparison(
        &mut self,
        loc: Location,
        lhs: &Expr,
        op: CompOp,
        rhs: &Expr,
    ) -> Self::Output;
    fn visit_assign(&mut self, loc: Location, var: Var, value: &Expr) -> Self::Output;
    fn visit_binop(&mut self, loc: Location, lhs: &Expr, op: BinaryOp, rhs: &Expr) -> Self::Output;
}
