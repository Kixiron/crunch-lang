use crate::{
    ast::{
        AssignKind, BinaryOp, Block, CompOp, Dest, Exposure, Expr, ExprKind, For, FuncArg, If,
        Item, ItemKind, ItemPath, Literal, Loop, Match, Sided, Stmt, StmtKind, Type, TypeMember,
        UnaryOp, VarDecl, Variant, While,
    },
    strings::StrT,
};

#[allow(unused_variables)]
pub trait AstVisitor {
    #[inline]
    fn visit_item(&mut self, item: &Item) {
        match &item.kind {
            ItemKind::Func {
                generics,
                args,
                body,
                ret,
            } => self.visit_func(item, generics, args, body, ret),
            ItemKind::Type { generics, members } => self.visit_type(item, generics, members),
            ItemKind::Enum { generics, variants } => self.visit_enum(item, generics, variants),
            ItemKind::Trait { generics, methods } => self.visit_trait(item, generics, methods),
            ItemKind::Import {
                file,
                dest,
                exposes,
            } => self.visit_import(item, file, dest, exposes),
            ItemKind::ExtendBlock {
                target,
                extender,
                items,
            } => {
                self.visit_extend_block(item, target, extender.as_ref().map(|t| t.as_ref()), items)
            }
            ItemKind::Alias { alias, actual } => self.visit_alias(item, alias, actual),
        }
    }

    fn visit_func(
        &mut self,
        item: &Item,
        generics: &[Type],
        args: &[FuncArg],
        body: &Block,
        ret: &Type,
    ) {
    }
    fn visit_type(&mut self, item: &Item, generics: &[Type], members: &[TypeMember]) {}
    fn visit_enum(&mut self, item: &Item, generics: &[Type], variants: &[Variant]) {}
    fn visit_trait(&mut self, item: &Item, generics: &[Type], methods: &[Item]) {}
    fn visit_import(&mut self, item: &Item, file: &ItemPath, dest: &Dest, exposes: &Exposure) {}
    fn visit_extend_block(
        &mut self,
        item: &Item,
        target: &Type,
        extender: Option<&Type>,
        items: &[Item],
    ) {
    }
    fn visit_alias(&mut self, item: &Item, alias: &Type, actual: &Type) {}

    #[inline]
    fn visit_stmt(&mut self, stmt: &Stmt) {
        match &stmt.kind {
            StmtKind::VarDecl(decl) => self.visit_var_decl(stmt, decl),
            StmtKind::Item(item) => self.visit_item(item),
            StmtKind::Expr(expr) => self.visit_expr(expr),
        }
    }

    fn visit_var_decl(&mut self, stmt: &Stmt, var: &VarDecl) {}

    #[inline]
    fn visit_expr(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::If(if_) => self.visit_if(expr, if_),
            ExprKind::Return(value) => self.visit_return(expr, value.as_ref().map(|e| e.as_ref())),
            ExprKind::Break(value) => self.visit_break(expr, value.as_ref().map(|e| e.as_ref())),
            ExprKind::Continue => self.visit_continue(expr),
            ExprKind::While(while_) => self.visit_while(expr, while_),
            ExprKind::Loop(loop_) => self.visit_loop(expr, loop_),
            ExprKind::For(for_) => self.visit_for(expr, for_),
            ExprKind::Match(match_) => self.visit_match(expr, match_),
            ExprKind::Variable(var) => self.visit_variable(expr, *var),
            ExprKind::Literal(literal) => self.visit_literal(expr, literal),
            ExprKind::UnaryOp(op, inner) => self.visit_unary(expr, *op, inner),
            ExprKind::BinaryOp(Sided { lhs, op, rhs }) => self.visit_binary_op(expr, lhs, *op, rhs),
            ExprKind::Comparison(Sided { lhs, op, rhs }) => {
                self.visit_comparison(expr, lhs, *op, rhs)
            }
            ExprKind::Assign(Sided { lhs, op, rhs }) => self.visit_assign(expr, lhs, *op, rhs),
            ExprKind::Paren(inner) => self.visit_paren(expr, inner),
            ExprKind::Array(elements) => self.visit_array(expr, elements),
            ExprKind::Tuple(elements) => self.visit_tuple(expr, elements),
            ExprKind::Range(start, end) => self.visit_range(expr, start, end),
            ExprKind::Index { var, index } => self.visit_index(expr, var, index),
            ExprKind::FuncCall { caller, args } => self.visit_func_call(expr, caller, args),
            ExprKind::MemberFuncCall { member, func } => {
                self.visit_member_func_call(expr, member, func)
            }
        }
    }

    fn visit_if(&mut self, expr: &Expr, if_: &If) {}
    fn visit_return(&mut self, expr: &Expr, value: Option<&Expr>) {}
    fn visit_break(&mut self, expr: &Expr, value: Option<&Expr>) {}
    fn visit_continue(&mut self, expr: &Expr) {}
    fn visit_while(&mut self, expr: &Expr, while_: &While) {}
    fn visit_loop(&mut self, expr: &Expr, loop_: &Loop) {}
    fn visit_for(&mut self, expr: &Expr, for_: &For) {}
    fn visit_match(&mut self, expr: &Expr, match_: &Match) {}
    fn visit_variable(&mut self, expr: &Expr, var: StrT) {}
    fn visit_literal(&mut self, expr: &Expr, literal: &Literal) {}
    fn visit_unary(&mut self, expr: &Expr, op: UnaryOp, inner: &Expr) {}
    fn visit_binary_op(&mut self, expr: &Expr, lhs: &Expr, op: BinaryOp, rhs: &Expr) {}
    fn visit_comparison(&mut self, expr: &Expr, lhs: &Expr, op: CompOp, rhs: &Expr) {}
    fn visit_assign(&mut self, expr: &Expr, lhs: &Expr, op: AssignKind, rhs: &Expr) {}
    fn visit_paren(&mut self, expr: &Expr, inner: &Expr) {}
    fn visit_array(&mut self, expr: &Expr, elements: &[Expr]) {}
    fn visit_tuple(&mut self, expr: &Expr, elements: &[Expr]) {}
    fn visit_range(&mut self, expr: &Expr, start: &Expr, end: &Expr) {}
    fn visit_index(&mut self, expr: &Expr, var: &Expr, index: &Expr) {}
    fn visit_func_call(&mut self, expr: &Expr, caller: &Expr, args: &[Expr]) {}
    fn visit_member_func_call(&mut self, expr: &Expr, member: &Expr, func: &Expr) {}
}
