use crate::{
    error::Locatable,
    interner::Spur,
    parser::{Ast, EnumVariant, ItemPath, Type},
};

use alloc::vec::Vec;
use core::{
    convert::TryInto,
    iter::FromIterator,
    num::NonZeroU32,
    ops::{self, Deref},
};

cfg_if::cfg_if! {
    if #[cfg(feature = "no-std")] {
        use hashbrown::HashMap;
    }  else {
        use std::collections::HashMap;
    }
}

// TODO: Is `Type` actually resolvable?
#[derive(Debug, Clone)]
pub enum Scope {
    Type {
        name: Spur,
        members: HashMap<Spur, Type>,
    },
    Enum {
        name: Spur,
        variants: HashMap<Spur, Variant>,
    },
    Alias {
        alias: Type,
        actual: Type,
    },
    Variable {
        name: Spur,
        ty: Type,
    },
    Function {
        name: Spur,
        scope: NodeId,
        params: Vec<(Spur, Type)>,
        returns: Type,
    },
    Trait {
        name: Spur,
        methods: HashMap<Spur, MaybeSym>,
    },
    ExtendBlock {
        target: Type,
        extender: Option<Type>,
    },
    LocalScope(Vec<Scope>),
}

impl Scope {
    pub const fn new() -> Self {
        Self::LocalScope(Vec::new())
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self::LocalScope(Vec::with_capacity(capacity))
    }

    pub fn name(&self) -> Spur {
        match self {
            Self::Function { name, .. } => *name,
            _ => todo!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Variant {
    Unit,
    Tuple(Vec<Type>),
}

impl From<&EnumVariant<'_>> for (Spur, Variant) {
    fn from(variant: &EnumVariant) -> Self {
        match variant {
            EnumVariant::Unit { name, .. } => (*name, Variant::Unit),
            EnumVariant::Tuple { name, elements, .. } => (
                *name,
                Variant::Tuple(elements.iter().map(|e| e.deref().clone()).collect()),
            ),
        }
    }
}

impl From<&Locatable<EnumVariant<'_>>> for (Spur, Variant) {
    fn from(variant: &Locatable<EnumVariant<'_>>) -> Self {
        variant.deref().into()
    }
}

// TODO: Think this through a bit more
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum MaybeSym {
    Unresolved(Spur),
    Resolved(NodeId),
}

impl From<NodeId> for MaybeSym {
    fn from(id: NodeId) -> Self {
        Self::Resolved(id)
    }
}

impl From<Spur> for MaybeSym {
    fn from(name: Spur) -> Self {
        Self::Unresolved(name)
    }
}

#[derive(Debug, Clone)]
pub struct Graph<T, L> {
    nodes: Vec<Node<T, L>>,
}

impl<T, L> Graph<T, L> {
    pub const fn new() -> Self {
        Self { nodes: Vec::new() }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            nodes: Vec::with_capacity(capacity),
        }
    }

    pub fn push(&mut self, data: T) -> NodeId {
        self.push_with_capacity(data, 0)
    }

    pub fn push_with_capacity(&mut self, data: T, capacity: usize) -> NodeId {
        let id = NodeId(
            NonZeroU32::new(
                (self.nodes.len() + 1)
                    .try_into()
                    .expect("Ran out of node capacity, should probably handle this"),
            )
            .expect("This shouldn't happen because of the +1"),
        );
        self.nodes.push(Node {
            data,
            parent: None,
            links: Vec::new(),
            children: Vec::with_capacity(capacity),
        });

        id
    }

    pub fn node(&self, id: NodeId) -> Option<&Node<T, L>> {
        self.nodes.get(id.to_usize() - 1)
    }

    pub fn node_mut(&mut self, id: NodeId) -> Option<&mut Node<T, L>> {
        self.nodes.get_mut(id.to_usize() - 1)
    }

    pub fn contains_node(&self, id: NodeId) -> bool {
        id.to_usize() < self.nodes.len()
    }
}

impl Graph<Scope, MaybeSym> {
    pub fn push_ast(&mut self, id: NodeId, node: &Ast<'_, '_>) {
        match node {
            Ast::Function(func) => {
                // TODO: All fields
                let function_scope = Scope::Function {
                    name: func.name,
                    scope: self.push(Scope::new()),
                    params: func
                        .args
                        .iter()
                        .map(|a| (*a.name, a.ty.deref().clone()))
                        .collect(),
                    returns: func.returns.deref().clone(),
                };

                // TODO: Build the inner scope

                let node = self.push(function_scope);
                self.node_mut(id).unwrap().children.push(node);
            }

            Ast::Type(ty) => {
                // TODO: All fields
                let type_scope = Scope::Type {
                    name: ty.name,
                    members: HashMap::from_iter(
                        ty.members.iter().map(|m| (m.name, m.ty.deref().clone())),
                    ),
                };

                let node = self.push(type_scope);
                self.node_mut(id).unwrap().children.push(node);
            }

            Ast::Enum(enu) => {
                // TODO: All fields
                let enum_scope = Scope::Enum {
                    name: enu.name,
                    variants: HashMap::from_iter(enu.variants.iter().map(<(Spur, Variant)>::from)),
                };

                let node = self.push(enum_scope);
                self.node_mut(id).unwrap().children.push(node);
            }

            Ast::Trait(tr) => {
                // TODO: All fields
                let trait_scope = Scope::Trait {
                    name: tr.name,
                    methods: HashMap::from_iter(
                        tr.methods
                            .iter()
                            .map(|f| (f.name, MaybeSym::Unresolved(f.name))),
                    ),
                };

                // TODO: Process members, indicating if they're implemented by default or not
                let node = self.push(trait_scope);
                self.node_mut(id).unwrap().children.push(node);
            }

            Ast::Import(import) => {
                self.node_mut(id)
                    .unwrap()
                    .links
                    .push(MaybeSym::Unresolved(*import.file));
            }

            // TODO: How do I connect these to their implementors?
            Ast::ExtendBlock(block) => {
                let block_scope = Scope::ExtendBlock {
                    target: block.target.deref().clone(),
                    extender: block.extender.as_ref().map(|t| t.deref().clone()),
                };

                let node = self.push(block_scope);
                self.node_mut(id).unwrap().children.push(node);
            }

            Ast::Alias(alias) => {
                let alias_scope = Scope::Alias {
                    alias: alias.alias.deref().clone(),
                    actual: alias.actual.deref().clone(),
                };

                let node = self.push(alias_scope);
                self.node_mut(id).unwrap().children.push(node);
            }
        }
    }
}

impl<T, L> ops::Index<NodeId> for Graph<T, L> {
    type Output = Node<T, L>;

    fn index(&self, id: NodeId) -> &Self::Output {
        &self.nodes[id.to_usize()]
    }
}

impl<T, L> ops::IndexMut<NodeId> for Graph<T, L> {
    fn index_mut(&mut self, id: NodeId) -> &mut Self::Output {
        &mut self.nodes[id.to_usize()]
    }
}

#[derive(Debug, Clone)]
pub struct Node<T, L> {
    data: T,
    parent: Option<NodeId>,
    links: Vec<L>,
    children: Vec<NodeId>,
}

impl<T, L> Node<T, L> {
    pub fn data(&self) -> &T {
        &self.data
    }

    pub fn data_mut(&mut self) -> &mut T {
        &mut self.data
    }

    pub fn parent(&self) -> Option<NodeId> {
        self.parent
    }

    pub fn parent_mut(&mut self) -> &mut Option<NodeId> {
        &mut self.parent
    }

    pub fn children(&self) -> &Vec<NodeId> {
        &self.children
    }

    pub fn children_mut(&mut self) -> &mut Vec<NodeId> {
        &mut self.children
    }

    pub fn links(&self) -> &Vec<L> {
        &self.links
    }

    pub fn links_mut(&mut self) -> &mut Vec<L> {
        &mut self.links
    }
}

impl Node<Scope, MaybeSym> {
    pub fn resolve(&self, graph: &Graph<Scope, MaybeSym>, name: Spur) -> Option<ItemPath> {
        if let Some(item) = self
            .children
            .iter()
            .filter_map(|c| graph.node(*c))
            .find(|c| c.data.name() == name)
        {
            Some(ItemPath::new(item.data.name()))
        } else {
            todo!("Do this later or something")
        }
    }
}

impl<T, L> ops::Deref for Node<T, L> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

impl<T, L> ops::DerefMut for Node<T, L> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.data
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct NodeId(NonZeroU32);

impl NodeId {
    pub fn to_usize(self) -> usize {
        self.0.get() as usize
    }
}
