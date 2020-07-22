use alloc::vec::Vec;
use crunch_shared::{
    trees::ast::{ExternBlock, Item, ItemKind},
    visitors::ast::ItemVisitorMut,
};

#[derive(Debug, Clone)]
#[allow(missing_copy_implementations)]
pub struct ExternUnnester {
    __private: (),
}

impl ExternUnnester {
    pub const fn new() -> Self {
        Self { __private: () }
    }

    pub fn unnest(mut self, mut items: Vec<Item>) -> Vec<Item> {
        let mut out_items = Vec::with_capacity(items.len());
        while !items.is_empty() {
            let mut item = items.remove(0);
            if self.visit_item(&mut items, &mut item) {
                out_items.push(item);
            }
        }

        out_items
    }
}

impl ItemVisitorMut for ExternUnnester {
    type Output = bool;

    fn visit_extern_block(&mut self, items: &mut Vec<Item>, item: &mut Item) -> Self::Output {
        if let ItemKind::ExternBlock(ExternBlock { items: block_items }) = &mut item.kind {
            items.extend(block_items.drain(..));
            false
        } else {
            unreachable!()
        }
    }

    fn visit_func(&mut self, _items: &mut Vec<Item>, _item: &mut Item) -> Self::Output {
        true
    }
    fn visit_type(&mut self, _items: &mut Vec<Item>, _item: &mut Item) -> Self::Output {
        true
    }
    fn visit_enum(&mut self, _items: &mut Vec<Item>, _item: &mut Item) -> Self::Output {
        true
    }
    fn visit_trait(&mut self, _items: &mut Vec<Item>, _item: &mut Item) -> Self::Output {
        true
    }
    fn visit_import(&mut self, _items: &mut Vec<Item>, _item: &mut Item) -> Self::Output {
        true
    }
    fn visit_extend_block(&mut self, _items: &mut Vec<Item>, _item: &mut Item) -> Self::Output {
        true
    }
    fn visit_alias(&mut self, _items: &mut Vec<Item>, _item: &mut Item) -> Self::Output {
        true
    }
    fn visit_extern_func(&mut self, _items: &mut Vec<Item>, _item: &mut Item) -> Self::Output {
        true
    }
}
