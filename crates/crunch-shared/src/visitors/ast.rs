use crate::{
    error::{Locatable, Location},
    strings::StrT,
    trees::{
        ast::{
            AssignKind, BinaryOp, Binding, Block, CompOp, Dest, Exposure, Expr, For, FuncArg, If,
            Item, ItemKind, Literal, LiteralVal, Loop, Match, Pattern, Stmt, Type, TypeMember,
            UnaryOp, VarDecl, Variant, While,
        },
        CallConv, ItemPath,
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
                self.visit_type_decl(item, generics.as_ref().map(|g| g.as_deref()), members)
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
    fn visit_type_decl(
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

    fn visit_expr(&mut self, expr: &Expr) -> Self::Output;
    fn visit_if(&mut self, expr: &Expr, if_: &If) -> Self::Output;
    fn visit_return(&mut self, expr: &Expr, value: Option<&Expr>) -> Self::Output;
    fn visit_break(&mut self, expr: &Expr, value: Option<&Expr>) -> Self::Output;
    fn visit_continue(&mut self, expr: &Expr) -> Self::Output;
    fn visit_while(&mut self, expr: &Expr, while_: &While) -> Self::Output;
    fn visit_loop(&mut self, expr: &Expr, loop_: &Loop) -> Self::Output;
    fn visit_for(&mut self, expr: &Expr, for_: &For) -> Self::Output;
    fn visit_match(&mut self, expr: &Expr, match_: &Match) -> Self::Output;
    fn visit_variable(&mut self, expr: &Expr, var: Locatable<StrT>) -> Self::Output;

    type LiteralOutput;
    fn visit_literal(&mut self, literal: &Literal) -> Self::LiteralOutput;

    type LiteralValOutput;
    fn visit_literal_val(&mut self, val: &LiteralVal) -> Self::LiteralValOutput;

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

    type BindingOutput;
    fn visit_binding(&mut self, binding: &Binding) -> Self::BindingOutput;
    type PatternOutput;
    fn visit_pattern(&mut self, pattern: &Pattern) -> Self::PatternOutput;
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

pub trait TypeVisitor {
    type Output;

    fn visit_type(&mut self, r#type: Locatable<&Type>) -> Self::Output;
}
