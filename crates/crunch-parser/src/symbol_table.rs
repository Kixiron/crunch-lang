use crate::{
    context::StrT,
    error::Locatable,
    parser::{Ast, EnumVariant, Expr, ItemPath, Stmt, Type},
};

use alloc::vec::Vec;
use core::{
    convert::TryInto,
    fmt,
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
pub enum Scope<'ctx> {
    Type {
        name: StrT,
        members: HashMap<StrT, &'ctx Type<'ctx>>,
    },
    Enum {
        name: StrT,
        variants: HashMap<StrT, Variant<'ctx>>,
    },
    Alias {
        alias: &'ctx Type<'ctx>,
        actual: &'ctx Type<'ctx>,
    },
    Variable {
        name: StrT,
        ty: &'ctx Type<'ctx>,
    },
    Function {
        name: StrT,
        scope: NodeId,
        params: Vec<(StrT, &'ctx Type<'ctx>)>,
        returns: &'ctx Type<'ctx>,
    },
    Trait {
        name: StrT,
        methods: HashMap<StrT, MaybeSym>,
    },
    ExtendBlock {
        target: &'ctx Type<'ctx>,
        extender: Option<&'ctx Type<'ctx>>,
    },
    LocalScope(Vec<(StrT, &'ctx Type<'ctx>)>, Vec<Scope<'ctx>>),
}

impl<'ctx> Scope<'ctx> {
    pub const fn new() -> Self {
        Self::LocalScope(Vec::new(), Vec::new())
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self::LocalScope(Vec::new(), Vec::with_capacity(capacity))
    }

    pub fn name(&self) -> StrT {
        match self {
            Self::Function { name, .. } => *name,
            _ => todo!(),
        }
    }

    pub fn vars_mut<'a>(&'a mut self) -> &'a mut Vec<(StrT, &'ctx Type<'ctx>)> {
        if let Self::LocalScope(vars, ..) = self {
            vars
        } else {
            todo!("Probably should return an option")
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Variant<'ctx> {
    Unit,
    Tuple(Vec<&'ctx Type<'ctx>>),
}

impl<'ctx> From<&EnumVariant<'ctx>> for (StrT, Variant<'ctx>) {
    fn from(variant: &EnumVariant<'ctx>) -> Self {
        match variant {
            EnumVariant::Unit { name, .. } => (*name, Variant::Unit),
            EnumVariant::Tuple { name, elements, .. } => (
                *name,
                Variant::Tuple(elements.iter().map(|e| e.deref().clone()).collect()),
            ),
        }
    }
}

impl<'ctx> From<&Locatable<EnumVariant<'ctx>>> for (StrT, Variant<'ctx>) {
    fn from(variant: &Locatable<EnumVariant<'ctx>>) -> Self {
        variant.deref().into()
    }
}

// TODO: Think this through a bit more
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum MaybeSym {
    Unresolved(StrT),
    Resolved(NodeId),
}

impl From<NodeId> for MaybeSym {
    fn from(id: NodeId) -> Self {
        Self::Resolved(id)
    }
}

impl From<StrT> for MaybeSym {
    fn from(name: StrT) -> Self {
        Self::Unresolved(name)
    }
}

#[derive(Clone)]
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

    pub fn add_child(&mut self, parent: NodeId, child: NodeId) -> Option<()> {
        self.node_mut(parent)?.children.push(child);
        self.node_mut(child)?.parent = Some(parent);
        Some(())
    }
}

impl<'ctx> Graph<Scope<'ctx>, MaybeSym> {
    pub fn push_ast(&mut self, id: NodeId, node: &'ctx Ast<'ctx>) {
        match node {
            Ast::Function(func) => {
                // TODO: All fields
                let scope = self.push(Scope::new());
                let function_scope = Scope::Function {
                    name: func.name,
                    scope,
                    params: func
                        .args
                        .iter()
                        .map(|a| (*a.name, a.ty.deref().clone()))
                        .collect(),
                    returns: func.returns.deref().clone(),
                };

                for stmt in func.body.iter() {
                    self.push_stmt(scope, &*stmt);
                }

                let node = self.push(function_scope);
                self.add_child(id, node).unwrap();
                self.add_child(node, scope).unwrap();
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
                self.add_child(id, node).unwrap();
            }

            Ast::Enum(enu) => {
                // TODO: All fields
                let enum_scope = Scope::Enum {
                    name: enu.name,
                    variants: HashMap::from_iter(enu.variants.iter().map(<(StrT, Variant)>::from)),
                };

                let node = self.push(enum_scope);
                self.add_child(id, node).unwrap();
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
                self.add_child(id, node).unwrap();
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
                    target: block.target.deref(),
                    extender: block.extender.map(|t| *t.deref()),
                };

                let node = self.push(block_scope);
                self.add_child(id, node).unwrap();
            }

            Ast::Alias(alias) => {
                let alias_scope = Scope::Alias {
                    alias: alias.alias.deref(),
                    actual: alias.actual.deref(),
                };

                let node = self.push(alias_scope);
                self.add_child(id, node).unwrap();
            }
        }
    }

    fn push_stmt(&mut self, parent: NodeId, stmt: &'ctx Stmt<'ctx>) {
        match stmt {
            Stmt::If {
                condition,
                body,
                clauses,
                else_clause,
            } => {
                self.push_expr(parent, condition);

                let body_scope = self.push(Scope::new());
                self.add_child(parent, body_scope).unwrap();

                for stmt in body {
                    self.push_stmt(parent, stmt);
                }

                for (condition, body) in clauses {
                    self.push_expr(parent, condition);

                    let else_scope = self.push(Scope::new());
                    self.add_child(parent, else_scope).unwrap();

                    for stmt in body {
                        self.push_stmt(parent, stmt);
                    }
                }

                if let Some(ref else_clause) = else_clause {
                    let else_scope = self.push(Scope::new());
                    self.add_child(parent, else_scope).unwrap();

                    for stmt in else_clause {
                        self.push_stmt(parent, stmt);
                    }
                }
            }

            Stmt::Expr(expr) => self.push_expr(parent, expr),

            Stmt::VarDeclaration {
                name,
                ty,
                val,
                constant: _,
                mutable: _,
            } => {
                let scope = self.node_mut(parent).unwrap();
                scope.vars_mut().push((*name, ty.deref().clone()));

                self.push_expr(parent, val);
            }

            Stmt::Return(ret) => {
                if let Some(ret) = ret {
                    self.push_expr(parent, &*ret);
                }
            }
            Stmt::Break(brk) => {
                if let Some(brk) = brk {
                    self.push_expr(parent, &brk);
                }
            }
            Stmt::Continue => {}
            Stmt::Empty => {}

            Stmt::While {
                condition,
                body,
                then,
                else_clause,
            } => {
                self.push_expr(parent, condition);

                let body_scope = self.push(Scope::new());
                self.add_child(parent, body_scope).unwrap();

                for stmt in body {
                    self.push_stmt(parent, stmt);
                }

                if let Some(body) = then {
                    let then_scope = self.push(Scope::new());
                    self.add_child(parent, then_scope).unwrap();

                    for stmt in body {
                        self.push_stmt(parent, &*stmt);
                    }
                }

                if let Some(else_clause) = else_clause {
                    let else_scope = self.push(Scope::new());
                    self.add_child(parent, else_scope).unwrap();

                    for stmt in else_clause {
                        self.push_stmt(parent, stmt);
                    }
                }
            }

            Stmt::Loop { body, else_clause } => {
                let body_scope = self.push(Scope::new());
                self.add_child(parent, body_scope).unwrap();

                for stmt in body {
                    self.push_stmt(parent, stmt);
                }

                if let Some(else_clause) = else_clause {
                    let else_scope = self.push(Scope::new());
                    self.add_child(parent, else_scope).unwrap();

                    for stmt in else_clause {
                        self.push_stmt(parent, stmt);
                    }
                }
            }

            Stmt::For {
                var,
                condition,
                body,
                then,
                else_clause,
            } => {
                self.push_expr(parent, var);
                self.push_expr(parent, condition);

                let body_scope = self.push(Scope::new());
                self.add_child(parent, body_scope).unwrap();

                for stmt in body {
                    self.push_stmt(body_scope, stmt);
                }

                if let Some(body) = then {
                    let then_scope = self.push(Scope::new());
                    self.add_child(parent, then_scope).unwrap();

                    for stmt in body {
                        self.push_stmt(parent, &*stmt);
                    }
                }

                if let Some(else_clause) = else_clause {
                    let else_scope = self.push(Scope::new());
                    self.add_child(parent, else_scope).unwrap();

                    for stmt in else_clause {
                        self.push_stmt(parent, stmt);
                    }
                }
            }

            Stmt::Match { var, arms } => {
                self.push_expr(parent, var);

                for (_bind, clause, body) in arms {
                    let arm = self.push(Scope::new());
                    self.add_child(parent, arm).unwrap();

                    // FIXME: Figure out Binding/Pattern's repr in HIR
                    // self.node_mut(arm)
                    //     .unwrap()
                    //     .vars_mut()
                    //     .push((*bind, Type::Infer));

                    if let Some(clause) = clause {
                        self.push_expr(arm, &*clause);
                    }

                    for stmt in body {
                        self.push_stmt(arm, stmt);
                    }
                }
            }
        }
    }

    fn push_expr(&mut self, _parent: NodeId, _expr: &'ctx Expr<'ctx>) {}
}

impl<T: fmt::Debug, L: fmt::Debug> fmt::Debug for Graph<T, L> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Graph")
            .field(
                "nodes",
                &HashMap::<usize, &Node<T, L>>::from_iter(self.nodes.iter().enumerate()),
            )
            .finish()
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

impl<'ctx> Node<Scope<'ctx>, MaybeSym> {
    pub fn resolve(&self, graph: &Graph<Scope, MaybeSym>, name: StrT) -> Option<ItemPath> {
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

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct NodeId(NonZeroU32);

impl NodeId {
    pub fn to_usize(self) -> usize {
        self.0.get() as usize
    }
}

impl fmt::Debug for NodeId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "NodeId({})", self.0.get() - 1)
    }
}
