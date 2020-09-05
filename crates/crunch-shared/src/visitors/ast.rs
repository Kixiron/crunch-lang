use crate::{
    error::{Locatable, Location},
    strings::StrT,
    trees::{
        ast::{
            AssignKind, BinaryOp, Binding, Block, CompOp, Dest, Exposure, Expr, ExtendBlock,
            ExternBlock, ExternFunc, For, FuncArg, If, Item, ItemKind, Literal, LiteralVal, Loop,
            Match, Pattern, Stmt, Type, TypeDecl, UnaryOp, VarDecl, Variant, While,
        },
        CallConv, ItemPath,
    },
};

pub trait ItemVisitor<'ctx> {
    type Output;

    fn visit_item(&mut self, item: &'ctx Item<'ctx>) -> Self::Output {
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
                *ret,
                *sig,
            ),
            ItemKind::Type(ty) => self.visit_type_decl(item, ty),
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
            ItemKind::ExtendBlock(ExtendBlock {
                target,
                extender,
                items,
            }) => self.visit_extend_block(item, *target, extender.as_ref().copied(), items),
            ItemKind::Alias { alias, actual } => self.visit_alias(item, *alias, *actual),
            ItemKind::ExternBlock(ExternBlock { items }) => self.visit_extern_block(item, items),
            ItemKind::ExternFunc(ExternFunc {
                generics,
                args,
                ret,
                callconv,
            }) => self.visit_extern_func(
                item,
                generics.as_ref().map(|g| g.as_deref()),
                args.as_deref(),
                *ret,
                *callconv,
            ),
        }
    }

    fn visit_func(
        &mut self,
        item: &'ctx Item<'ctx>,
        generics: Option<Locatable<&[Locatable<&'ctx Type<'ctx>>]>>,
        args: Locatable<&[FuncArg<'ctx>]>,
        body: &Block<'ctx>,
        ret: Locatable<&'ctx Type<'ctx>>,
        sig: Location,
    ) -> Self::Output;
    fn visit_type_decl(&mut self, item: &'ctx Item<'ctx>, ty: &TypeDecl<'ctx>) -> Self::Output;
    fn visit_enum(
        &mut self,
        item: &'ctx Item<'ctx>,
        generics: Option<Locatable<&[Locatable<&'ctx Type<'ctx>>]>>,
        variants: &[Variant<'ctx>],
    ) -> Self::Output;
    fn visit_trait(
        &mut self,
        item: &'ctx Item<'ctx>,
        generics: Option<Locatable<&[Locatable<&'ctx Type<'ctx>>]>>,
        methods: &[&'ctx Item<'ctx>],
    ) -> Self::Output;
    fn visit_import(
        &mut self,
        item: &'ctx Item<'ctx>,
        file: &ItemPath,
        dest: &Dest,
        exposes: &Exposure,
    ) -> Self::Output;
    fn visit_extend_block(
        &mut self,
        item: &'ctx Item<'ctx>,
        target: Locatable<&'ctx Type<'ctx>>,
        extender: Option<Locatable<&'ctx Type<'ctx>>>,
        items: &[&'ctx Item<'ctx>],
    ) -> Self::Output;
    fn visit_alias(
        &mut self,
        item: &'ctx Item<'ctx>,
        alias: Locatable<&'ctx Type<'ctx>>,
        actual: Locatable<&'ctx Type<'ctx>>,
    ) -> Self::Output;
    fn visit_extern_block(
        &mut self,
        item: &'ctx Item<'ctx>,
        items: &[&'ctx Item<'ctx>],
    ) -> Self::Output;
    fn visit_extern_func(
        &mut self,
        item: &'ctx Item<'ctx>,
        generics: Option<Locatable<&[Locatable<&'ctx Type<'ctx>>]>>,
        args: Locatable<&[FuncArg<'ctx>]>,
        ret: Locatable<&'ctx Type<'ctx>>,
        callconv: CallConv,
    ) -> Self::Output;
}

pub trait StmtVisitor<'ctx> {
    type Output;

    fn visit_stmt(&mut self, stmt: &'ctx Stmt<'ctx>) -> Self::Output;
    fn visit_var_decl(&mut self, stmt: &'ctx Stmt<'ctx>, var: &VarDecl<'ctx>) -> Self::Output;
}

pub trait ExprVisitor<'ctx> {
    type Output;

    fn visit_expr(&mut self, expr: &'ctx Expr<'ctx>) -> Self::Output;
    fn visit_if(&mut self, expr: &'ctx Expr<'ctx>, if_: &If<'ctx>) -> Self::Output;
    fn visit_return(
        &mut self,
        expr: &'ctx Expr<'ctx>,
        value: Option<&'ctx Expr<'ctx>>,
    ) -> Self::Output;
    fn visit_break(
        &mut self,
        expr: &'ctx Expr<'ctx>,
        value: Option<&'ctx Expr<'ctx>>,
    ) -> Self::Output;
    fn visit_continue(&mut self, expr: &'ctx Expr<'ctx>) -> Self::Output;
    fn visit_while(&mut self, expr: &'ctx Expr<'ctx>, while_: &While<'ctx>) -> Self::Output;
    fn visit_loop(&mut self, expr: &'ctx Expr<'ctx>, loop_: &Loop<'ctx>) -> Self::Output;
    fn visit_for(&mut self, expr: &'ctx Expr<'ctx>, for_: &For<'ctx>) -> Self::Output;
    fn visit_match(&mut self, expr: &'ctx Expr<'ctx>, match_: &Match<'ctx>) -> Self::Output;
    fn visit_variable(&mut self, expr: &'ctx Expr<'ctx>, var: Locatable<StrT>) -> Self::Output;

    type LiteralOutput;
    fn visit_literal(&mut self, literal: &Literal<'ctx>) -> Self::LiteralOutput;

    type LiteralValOutput;
    fn visit_literal_val(&mut self, val: &LiteralVal<'ctx>) -> Self::LiteralValOutput;

    fn visit_unary(
        &mut self,
        expr: &'ctx Expr<'ctx>,
        op: UnaryOp,
        inner: &'ctx Expr<'ctx>,
    ) -> Self::Output;
    fn visit_binary_op(
        &mut self,
        expr: &'ctx Expr<'ctx>,
        lhs: &'ctx Expr<'ctx>,
        op: BinaryOp,
        rhs: &'ctx Expr<'ctx>,
    ) -> Self::Output;
    fn visit_comparison(
        &mut self,
        expr: &'ctx Expr<'ctx>,
        lhs: &'ctx Expr<'ctx>,
        op: CompOp,
        rhs: &'ctx Expr<'ctx>,
    ) -> Self::Output;
    fn visit_assign(
        &mut self,
        expr: &'ctx Expr<'ctx>,
        lhs: &'ctx Expr<'ctx>,
        op: AssignKind,
        rhs: &'ctx Expr<'ctx>,
    ) -> Self::Output;
    fn visit_paren(&mut self, expr: &'ctx Expr<'ctx>, inner: &'ctx Expr<'ctx>) -> Self::Output;
    fn visit_array(
        &mut self,
        expr: &'ctx Expr<'ctx>,
        elements: &[&'ctx Expr<'ctx>],
    ) -> Self::Output;
    fn visit_tuple(
        &mut self,
        expr: &'ctx Expr<'ctx>,
        elements: &[&'ctx Expr<'ctx>],
    ) -> Self::Output;
    fn visit_range(
        &mut self,
        expr: &'ctx Expr<'ctx>,
        start: &'ctx Expr<'ctx>,
        end: &'ctx Expr<'ctx>,
    ) -> Self::Output;
    fn visit_index(
        &mut self,
        expr: &'ctx Expr<'ctx>,
        var: &'ctx Expr<'ctx>,
        index: &'ctx Expr<'ctx>,
    ) -> Self::Output;
    fn visit_func_call(
        &mut self,
        expr: &'ctx Expr<'ctx>,
        caller: &'ctx Expr<'ctx>,
        args: &[&'ctx Expr<'ctx>],
    ) -> Self::Output;
    fn visit_member_func_call(
        &mut self,
        expr: &'ctx Expr<'ctx>,
        member: &'ctx Expr<'ctx>,
        func: &'ctx Expr<'ctx>,
    ) -> Self::Output;
    fn visit_reference(
        &mut self,
        expr: &'ctx Expr<'ctx>,
        mutable: bool,
        reference: &'ctx Expr<'ctx>,
    ) -> Self::Output;
    fn visit_cast(
        &mut self,
        expr: &'ctx Expr<'ctx>,
        cast: &'ctx Expr<'ctx>,
        ty: Locatable<&'ctx Type<'ctx>>,
    ) -> Self::Output;

    type BindingOutput;
    fn visit_binding(&mut self, binding: &Binding<'ctx>) -> Self::BindingOutput;

    type PatternOutput;
    fn visit_pattern(&mut self, pattern: &Pattern<'ctx>) -> Self::PatternOutput;
}

#[allow(unused_variables)]
pub trait ItemVisitorMut<'ctx> {
    type Output;

    fn visit_item(
        &mut self,
        items: &mut Vec<&'ctx Item<'ctx>>,
        item: &'ctx Item<'ctx>,
    ) -> Self::Output {
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

    fn visit_func(
        &mut self,
        items: &mut Vec<&'ctx Item<'ctx>>,
        item: &'ctx Item<'ctx>,
    ) -> Self::Output;
    fn visit_type(
        &mut self,
        items: &mut Vec<&'ctx Item<'ctx>>,
        item: &'ctx Item<'ctx>,
    ) -> Self::Output;
    fn visit_enum(
        &mut self,
        items: &mut Vec<&'ctx Item<'ctx>>,
        item: &'ctx Item<'ctx>,
    ) -> Self::Output;
    fn visit_trait(
        &mut self,
        items: &mut Vec<&'ctx Item<'ctx>>,
        item: &'ctx Item<'ctx>,
    ) -> Self::Output;
    fn visit_import(
        &mut self,
        items: &mut Vec<&'ctx Item<'ctx>>,
        item: &'ctx Item<'ctx>,
    ) -> Self::Output;
    fn visit_extend_block(
        &mut self,
        items: &mut Vec<&'ctx Item<'ctx>>,
        item: &'ctx Item<'ctx>,
    ) -> Self::Output;
    fn visit_alias(
        &mut self,
        items: &mut Vec<&'ctx Item<'ctx>>,
        item: &'ctx Item<'ctx>,
    ) -> Self::Output;
    fn visit_extern_block(
        &mut self,
        items: &mut Vec<&'ctx Item<'ctx>>,
        item: &'ctx Item<'ctx>,
    ) -> Self::Output;
    fn visit_extern_func(
        &mut self,
        items: &mut Vec<&'ctx Item<'ctx>>,
        item: &'ctx Item<'ctx>,
    ) -> Self::Output;
}

pub trait TypeVisitor<'ctx> {
    type Output;

    fn visit_type(&mut self, r#type: Locatable<&'ctx Type<'ctx>>) -> Self::Output;
}
