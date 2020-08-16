use alloc::vec::Vec;
use crunch_shared::{
    trees::ast::{ExternBlock, Item, ItemKind},
    visitors::ast::ItemVisitorMut,
};

#[derive(Debug, Clone)]
#[allow(missing_copy_implementations)]
pub struct FlattenExternals {
    __private: (),
}

impl FlattenExternals {
    pub const fn new() -> Self {
        Self { __private: () }
    }

    pub fn flatten<'ctx>(mut self, mut items: Vec<&'ctx Item<'ctx>>) -> Vec<&'ctx Item<'ctx>> {
        let mut out_items = Vec::with_capacity(items.len());
        while !items.is_empty() {
            let item = items.remove(0);
            if self.visit_item(&mut items, &item) {
                out_items.push(item);
            }
        }

        out_items
    }
}

impl<'ctx> ItemVisitorMut<'ctx> for FlattenExternals {
    type Output = bool;

    fn visit_extern_block(
        &mut self,
        items: &mut Vec<&'ctx Item<'ctx>>,
        item: &'ctx Item<'ctx>,
    ) -> Self::Output {
        if let ItemKind::ExternBlock(ExternBlock { items: block_items }) = &item.kind {
            items.extend(block_items.iter().cloned());
            false
        } else {
            unreachable!()
        }
    }

    fn visit_func(
        &mut self,
        _items: &mut Vec<&'ctx Item<'ctx>>,
        _item: &'ctx Item<'ctx>,
    ) -> Self::Output {
        true
    }

    fn visit_type(
        &mut self,
        _items: &mut Vec<&'ctx Item<'ctx>>,
        _item: &'ctx Item<'ctx>,
    ) -> Self::Output {
        true
    }

    fn visit_enum(
        &mut self,
        _items: &mut Vec<&'ctx Item<'ctx>>,
        _item: &'ctx Item<'ctx>,
    ) -> Self::Output {
        true
    }

    fn visit_trait(
        &mut self,
        _items: &mut Vec<&'ctx Item<'ctx>>,
        _item: &'ctx Item<'ctx>,
    ) -> Self::Output {
        true
    }

    fn visit_import(
        &mut self,
        _items: &mut Vec<&'ctx Item<'ctx>>,
        _item: &'ctx Item<'ctx>,
    ) -> Self::Output {
        true
    }

    fn visit_extend_block(
        &mut self,
        _items: &mut Vec<&'ctx Item<'ctx>>,
        _item: &'ctx Item<'ctx>,
    ) -> Self::Output {
        true
    }

    fn visit_alias(
        &mut self,
        _items: &mut Vec<&'ctx Item<'ctx>>,
        _item: &'ctx Item<'ctx>,
    ) -> Self::Output {
        true
    }

    fn visit_extern_func(
        &mut self,
        _items: &mut Vec<&'ctx Item<'ctx>>,
        _item: &'ctx Item<'ctx>,
    ) -> Self::Output {
        true
    }
}
