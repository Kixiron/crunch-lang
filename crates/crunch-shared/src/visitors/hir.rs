use crate::{
    error::Location,
    trees::{
        ast::BinaryOp,
        hir::{
            Block, Break, Cast, CompOp, Expr, ExprKind, ExternFunc, FuncCall, Function, Item,
            Literal, Match, Reference, Return, Stmt, TypeId, Var, VarDecl,
        },
        Sided,
    },
};

pub trait ItemVisitor<'ctx> {
    type Output;

    #[inline]
    fn visit_item(&mut self, item: &Item<'ctx>) -> Self::Output {
        match item {
            Item::Function(func) => self.visit_func(func),
            Item::ExternFunc(func) => self.visit_extern_func(func),
        }
    }

    fn visit_func(&mut self, func: &Function<'ctx>) -> Self::Output;
    fn visit_extern_func(&mut self, func: &ExternFunc) -> Self::Output;
}

pub trait StmtVisitor<'ctx>: ItemVisitor<'ctx> + ExprVisitor<'ctx> {
    type Output;

    fn visit_stmt(&mut self, stmt: &'ctx Stmt<'ctx>) -> <Self as StmtVisitor<'ctx>>::Output;
    fn visit_var_decl(&mut self, var: &'ctx VarDecl<'ctx>) -> <Self as StmtVisitor<'ctx>>::Output;
}

pub trait ExprVisitor<'ctx> {
    type Output;

    #[inline]
    fn visit_expr(&mut self, expr: &'ctx Expr<'ctx>) -> Self::Output {
        let loc = expr.loc;

        match &expr.kind {
            ExprKind::Return(value) => self.visit_return(loc, value),
            ExprKind::Break(value) => self.visit_break(loc, value),
            ExprKind::Continue => self.visit_continue(loc),
            ExprKind::Loop(body) => self.visit_loop(loc, body),
            ExprKind::Match(match_) => self.visit_match(loc, match_),
            ExprKind::Variable(var, ty) => self.visit_variable(loc, *var, *ty),
            ExprKind::Literal(literal) => self.visit_literal(loc, literal),
            ExprKind::Scope(body) => self.visit_scope(loc, body),
            ExprKind::FnCall(call) => self.visit_func_call(loc, call),
            ExprKind::Comparison(Sided { lhs, op, rhs }) => {
                self.visit_comparison(loc, lhs, *op, rhs)
            }
            ExprKind::Assign(var, value) => self.visit_assign(loc, *var, value),
            ExprKind::BinOp(Sided { lhs, op, rhs }) => self.visit_binop(loc, lhs, *op, rhs),
            ExprKind::Cast(cast) => self.visit_cast(loc, cast),
            ExprKind::Reference(reference) => self.visit_reference(loc, reference),
            ExprKind::Index { var, index } => self.visit_index(loc, *var, index),
        }
    }

    fn visit_return(&mut self, loc: Location, value: &Return<'ctx>) -> Self::Output;
    fn visit_break(&mut self, loc: Location, value: &Break<'ctx>) -> Self::Output;
    fn visit_continue(&mut self, loc: Location) -> Self::Output;
    fn visit_loop(&mut self, loc: Location, body: &Block<&'ctx Stmt<'ctx>>) -> Self::Output;
    fn visit_match(&mut self, loc: Location, match_: &Match<'ctx>) -> Self::Output;
    fn visit_variable(&mut self, loc: Location, var: Var, ty: TypeId) -> Self::Output;
    fn visit_literal(&mut self, loc: Location, literal: &Literal) -> Self::Output;
    fn visit_scope(&mut self, loc: Location, body: &Block<&'ctx Stmt<'ctx>>) -> Self::Output;
    fn visit_func_call(&mut self, loc: Location, call: &FuncCall<'ctx>) -> Self::Output;
    fn visit_comparison(
        &mut self,
        loc: Location,
        lhs: &'ctx Expr<'ctx>,
        op: CompOp,
        rhs: &'ctx Expr<'ctx>,
    ) -> Self::Output;
    fn visit_assign(&mut self, loc: Location, var: Var, value: &'ctx Expr<'ctx>) -> Self::Output;
    fn visit_binop(
        &mut self,
        loc: Location,
        lhs: &'ctx Expr<'ctx>,
        op: BinaryOp,
        rhs: &'ctx Expr<'ctx>,
    ) -> Self::Output;
    fn visit_cast(&mut self, loc: Location, cast: &Cast<'ctx>) -> Self::Output;
    fn visit_reference(&mut self, loc: Location, reference: &Reference<'ctx>) -> Self::Output;
    fn visit_index(&mut self, loc: Location, var: Var, index: &'ctx Expr<'ctx>) -> Self::Output;
}

pub trait TypeVisitor<'ctx> {
    type Output;

    fn visit_type(&mut self, r#type: TypeId) -> Self::Output;
}
