use crate::{
    error::{Locatable, Location},
    strings::StrT,
    trees::{
        ast::{
            AssignKind, BinaryOp, Block, CompOp, Dest, Exposure, Expr, ExprKind, For, FuncArg, If,
            Item, ItemKind, Literal, Loop, Match, Stmt, Type, TypeMember, UnaryOp, VarDecl,
            Variant, While,
        },
        CallConv, ItemPath, Sided,
    },
};
use alloc::vec::Vec;

pub trait ItemVisitor {
    type Output;

    #[inline]
    fn visit_item(&mut self, item: &Item) -> Self::Output {
        match &item.kind {
            ItemKind::Func {
                generics,
                args,
                body,
                ret,
                sig,
            } => self.visit_func(
                item,
                generics.as_ref().map(|g| g.as_deref()),
                args.as_deref(),
                body,
                (**ret).as_ref(),
                *sig,
            ),
            ItemKind::Type { generics, members } => {
                self.visit_type(item, generics.as_ref().map(|g| g.as_deref()), members)
            }
            ItemKind::Enum { generics, variants } => {
                self.visit_enum(item, generics.as_ref().map(|g| g.as_deref()), variants)
            }
            ItemKind::Trait { generics, methods } => {
                self.visit_trait(item, generics.as_ref().map(|g| g.as_deref()), methods)
            }
            ItemKind::Import {
                file,
                dest,
                exposes,
            } => self.visit_import(item, file, dest, exposes),
            ItemKind::ExtendBlock {
                target,
                extender,
                items,
            } => self.visit_extend_block(
                item,
                (**target).as_ref(),
                extender.as_ref().map(|t| (**t).as_ref()),
                items,
            ),
            ItemKind::Alias { alias, actual } => {
                self.visit_alias(item, (**alias).as_ref(), (**actual).as_ref())
            }
            ItemKind::ExternBlock { items } => self.visit_extern_block(item, items),
            ItemKind::ExternFunc {
                generics,
                args,
                ret,
                callconv,
            } => self.visit_extern_func(
                item,
                generics.as_ref().map(|g| g.as_deref()),
                args.as_deref(),
                (**ret).as_ref(),
                *callconv,
            ),
        }
    }

    fn visit_func(
        &mut self,
        item: &Item,
        generics: Option<Locatable<&[Locatable<Type>]>>,
        args: Locatable<&[FuncArg]>,
        body: &Block,
        ret: Locatable<&Type>,
        sig: Location,
    ) -> Self::Output;
    fn visit_type(
        &mut self,
        item: &Item,
        generics: Option<Locatable<&[Locatable<Type>]>>,
        members: &[TypeMember],
    ) -> Self::Output;
    fn visit_enum(
        &mut self,
        item: &Item,
        generics: Option<Locatable<&[Locatable<Type>]>>,
        variants: &[Variant],
    ) -> Self::Output;
    fn visit_trait(
        &mut self,
        item: &Item,
        generics: Option<Locatable<&[Locatable<Type>]>>,
        methods: &[Item],
    ) -> Self::Output;
    fn visit_import(
        &mut self,
        item: &Item,
        file: &ItemPath,
        dest: &Dest,
        exposes: &Exposure,
    ) -> Self::Output;
    fn visit_extend_block(
        &mut self,
        item: &Item,
        target: Locatable<&Type>,
        extender: Option<Locatable<&Type>>,
        items: &[Item],
    ) -> Self::Output;
    fn visit_alias(
        &mut self,
        item: &Item,
        alias: Locatable<&Type>,
        actual: Locatable<&Type>,
    ) -> Self::Output;
    fn visit_extern_block(&mut self, item: &Item, items: &[Item]) -> Self::Output;
    fn visit_extern_func(
        &mut self,
        item: &Item,
        generics: Option<Locatable<&[Locatable<Type>]>>,
        args: Locatable<&[FuncArg]>,
        ret: Locatable<&Type>,
        callconv: CallConv,
    ) -> Self::Output;
}

pub trait StmtVisitor {
    type Output;

    fn visit_stmt(&mut self, stmt: &Stmt) -> Self::Output;
    fn visit_var_decl(&mut self, stmt: &Stmt, var: &VarDecl) -> Self::Output;
}

pub trait ExprVisitor {
    type Output;

    #[inline]
    fn visit_expr(&mut self, expr: &Expr) -> Self::Output {
        match &expr.kind {
            ExprKind::If(if_) => self.visit_if(expr, if_),
            ExprKind::Return(value) => self.visit_return(expr, value.as_deref()),
            ExprKind::Break(value) => self.visit_break(expr, value.as_deref()),
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
            ExprKind::Reference {
                mutable,
                expr: reference,
            } => self.visit_reference(expr, *mutable, reference.as_ref()),
            ExprKind::Cast { expr: cast, ty } => self.visit_cast(expr, cast, ty.as_ref().as_ref()),
        }
    }

    fn visit_if(&mut self, expr: &Expr, if_: &If) -> Self::Output;
    fn visit_return(&mut self, expr: &Expr, value: Option<&Expr>) -> Self::Output;
    fn visit_break(&mut self, expr: &Expr, value: Option<&Expr>) -> Self::Output;
    fn visit_continue(&mut self, expr: &Expr) -> Self::Output;
    fn visit_while(&mut self, expr: &Expr, while_: &While) -> Self::Output;
    fn visit_loop(&mut self, expr: &Expr, loop_: &Loop) -> Self::Output;
    fn visit_for(&mut self, expr: &Expr, for_: &For) -> Self::Output;
    fn visit_match(&mut self, expr: &Expr, match_: &Match) -> Self::Output;
    fn visit_variable(&mut self, expr: &Expr, var: Locatable<StrT>) -> Self::Output;
    fn visit_literal(&mut self, expr: &Expr, literal: &Locatable<Literal>) -> Self::Output;
    fn visit_unary(&mut self, expr: &Expr, op: UnaryOp, inner: &Expr) -> Self::Output;
    fn visit_binary_op(
        &mut self,
        expr: &Expr,
        lhs: &Expr,
        op: BinaryOp,
        rhs: &Expr,
    ) -> Self::Output;
    fn visit_comparison(&mut self, expr: &Expr, lhs: &Expr, op: CompOp, rhs: &Expr)
        -> Self::Output;
    fn visit_assign(&mut self, expr: &Expr, lhs: &Expr, op: AssignKind, rhs: &Expr)
        -> Self::Output;
    fn visit_paren(&mut self, expr: &Expr, inner: &Expr) -> Self::Output;
    fn visit_array(&mut self, expr: &Expr, elements: &[Expr]) -> Self::Output;
    fn visit_tuple(&mut self, expr: &Expr, elements: &[Expr]) -> Self::Output;
    fn visit_range(&mut self, expr: &Expr, start: &Expr, end: &Expr) -> Self::Output;
    fn visit_index(&mut self, expr: &Expr, var: &Expr, index: &Expr) -> Self::Output;
    fn visit_func_call(&mut self, expr: &Expr, caller: &Expr, args: &[Expr]) -> Self::Output;
    fn visit_member_func_call(&mut self, expr: &Expr, member: &Expr, func: &Expr) -> Self::Output;
    fn visit_reference(&mut self, expr: &Expr, mutable: bool, reference: &Expr) -> Self::Output;
    fn visit_cast(&mut self, expr: &Expr, cast: &Expr, ty: Locatable<&Type>) -> Self::Output;
}

#[allow(unused_variables)]
pub trait ItemVisitorMut {
    type Output;

    #[inline]
    fn visit_item(&mut self, items: &mut Vec<Item>, item: &mut Item) -> Self::Output {
        match &item.kind {
            ItemKind::Func { .. } => self.visit_func(items, item),
            ItemKind::Type { .. } => self.visit_type(items, item),
            ItemKind::Enum { .. } => self.visit_enum(items, item),
            ItemKind::Trait { .. } => self.visit_trait(items, item),
            ItemKind::Import { .. } => self.visit_import(items, item),
            ItemKind::ExtendBlock { .. } => self.visit_extend_block(items, item),
            ItemKind::Alias { .. } => self.visit_alias(items, item),
            ItemKind::ExternBlock { .. } => self.visit_extern_block(items, item),
            ItemKind::ExternFunc { .. } => self.visit_extern_func(items, item),
        }
    }

    fn visit_func(&mut self, items: &mut Vec<Item>, item: &mut Item) -> Self::Output;
    fn visit_type(&mut self, items: &mut Vec<Item>, item: &mut Item) -> Self::Output;
    fn visit_enum(&mut self, items: &mut Vec<Item>, item: &mut Item) -> Self::Output;
    fn visit_trait(&mut self, items: &mut Vec<Item>, item: &mut Item) -> Self::Output;
    fn visit_import(&mut self, items: &mut Vec<Item>, item: &mut Item) -> Self::Output;
    fn visit_extend_block(&mut self, items: &mut Vec<Item>, item: &mut Item) -> Self::Output;
    fn visit_alias(&mut self, items: &mut Vec<Item>, item: &mut Item) -> Self::Output;
    fn visit_extern_block(&mut self, items: &mut Vec<Item>, item: &mut Item) -> Self::Output;
    fn visit_extern_func(&mut self, items: &mut Vec<Item>, item: &mut Item) -> Self::Output;
}
