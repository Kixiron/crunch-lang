use crate::trees::ast::{ExternBlock, Item};

pub fn flatten_external_blocks(items: &mut Vec<Item>, mut block: ExternBlock) {
    items.extend(block.items.drain(..));
}
