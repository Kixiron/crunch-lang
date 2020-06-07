use crate::{
    strings::StrT,
    trees::ast::{
        Expr, Item, ItemKind, ItemPath, Stmt, StmtKind, Type, VarDecl, Variant as AstVariant,
    },
    utils::HashMap,
};
use alloc::vec::Vec;
use core::{convert::TryInto, fmt, iter::FromIterator, num::NonZeroU32, ops};

// TODO: Is `Type` actually resolvable?
#[derive(Debug, Clone)]
pub enum Scope {
    Type {
        name: StrT,
        members: HashMap<StrT, Type>,
    },
    Enum {
        name: StrT,
        variants: HashMap<StrT, Variant>,
    },
    Alias {
        alias: Type,
        actual: Type,
    },
    Variable {
        name: StrT,
        ty: Type,
    },
    Function {
        name: StrT,
        scope: NodeId,
        params: Vec<(StrT, Type)>,
        returns: Type,
    },
    Trait {
        name: StrT,
        methods: HashMap<StrT, MaybeSym>,
    },
    ExtendBlock {
        target: Type,
        extender: Option<Type>,
    },
    LocalScope(Vec<(StrT, Type)>, Vec<Scope>),
}

impl Scope {
    #[inline]
    pub const fn new() -> Self {
        Self::LocalScope(Vec::new(), Vec::new())
    }

    #[inline]
    pub fn with_capacity(capacity: usize) -> Self {
        Self::LocalScope(Vec::new(), Vec::with_capacity(capacity))
    }

    #[inline]
    pub fn name(&self) -> StrT {
        match self {
            Self::Function { name, .. } => *name,
            _ => todo!(),
        }
    }

    #[inline]
    pub fn vars_mut<'a>(&'a mut self) -> &'a mut Vec<(StrT, Type)> {
        if let Self::LocalScope(vars, ..) = self {
            vars
        } else {
            todo!("Probably should return an option")
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Variant {
    Unit,
    Tuple(Vec<Type>),
}

#[inline]
fn split_variant(variant: &AstVariant) -> (StrT, Variant) {
    match variant {
        AstVariant::Unit { name, .. } => (*name, Variant::Unit),
        AstVariant::Tuple { name, elms, .. } => (*name, Variant::Tuple(elms.clone())),
    }
}

// TODO: Think this through a bit more
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum MaybeSym {
    Unresolved(StrT),
    Resolved(NodeId),
}

impl From<NodeId> for MaybeSym {
    #[inline]
    fn from(id: NodeId) -> Self {
        Self::Resolved(id)
    }
}

impl From<StrT> for MaybeSym {
    #[inline]
    fn from(name: StrT) -> Self {
        Self::Unresolved(name)
    }
}

#[derive(Clone)]
pub struct Graph<T, L> {
    nodes: Vec<Node<T, L>>,
}

impl<T, L> Graph<T, L> {
    #[inline]
    pub const fn new() -> Self {
        Self { nodes: Vec::new() }
    }

    #[inline]
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            nodes: Vec::with_capacity(capacity),
        }
    }

    #[inline]
    pub fn push(&mut self, data: T) -> NodeId {
        self.push_with_capacity(data, 0)
    }

    #[inline]
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

    #[inline]
    pub fn node(&self, id: NodeId) -> Option<&Node<T, L>> {
        self.nodes.get(id.to_usize() - 1)
    }

    #[inline]
    pub fn node_mut(&mut self, id: NodeId) -> Option<&mut Node<T, L>> {
        self.nodes.get_mut(id.to_usize() - 1)
    }

    #[inline]
    pub fn contains_node(&self, id: NodeId) -> bool {
        id.to_usize() < self.nodes.len()
    }

    #[inline]
    pub fn add_child(&mut self, parent: NodeId, child: NodeId) -> Option<()> {
        self.node_mut(parent)?.children.push(child);
        self.node_mut(child)?.parent = Some(parent);

        Some(())
    }
}

impl Graph<Scope, MaybeSym> {
    // TODO: General metadata & generics
    #[inline]
    pub fn push_item(&mut self, id: NodeId, item: &Item) {
        match &item.kind {
            ItemKind::Func {
                generics: _,
                args,
                body,
                ret,
            } => {
                // TODO: All fields
                let scope = self.push(Scope::new());
                let function_scope = Scope::Function {
                    name: item.name.unwrap(),
                    scope,
                    params: args
                        .iter()
                        .map(|arg| (arg.name, arg.ty.as_ref().clone()))
                        .collect(),
                    returns: ret.as_ref().clone(),
                };

                for stmt in body.iter() {
                    self.push_stmt(scope, stmt);
                }

                let node = self.push(function_scope);
                self.add_child(id, node).unwrap();
                self.add_child(node, scope).unwrap();
            }

            ItemKind::Type {
                generics: _,
                members,
            } => {
                // TODO: All fields
                let type_scope = Scope::Type {
                    name: item.name.unwrap(),
                    members: HashMap::from_iter(
                        members.iter().map(|m| (m.name, m.ty.as_ref().clone())),
                    ),
                };

                let node = self.push(type_scope);
                self.add_child(id, node).unwrap();
            }

            ItemKind::Enum {
                generics: _,
                variants,
            } => {
                // TODO: All fields
                let enum_scope = Scope::Enum {
                    name: item.name.unwrap(),
                    variants: HashMap::from_iter(variants.iter().map(|v| split_variant(v))),
                };

                let node = self.push(enum_scope);
                self.add_child(id, node).unwrap();
            }

            ItemKind::Trait {
                generics: _,
                methods,
            } => {
                // TODO: All fields
                let trait_scope = Scope::Trait {
                    name: item.name.unwrap(),
                    methods: HashMap::from_iter(
                        methods
                            .iter()
                            .map(|f| (f.name.unwrap(), MaybeSym::Unresolved(f.name.unwrap()))),
                    ),
                };

                // TODO: Process members, indicating if they're implemented by default or not
                let node = self.push(trait_scope);
                self.add_child(id, node).unwrap();
            }

            ItemKind::Import {
                file: _,
                dest: _,
                exposes: _,
            } => todo!(),

            // TODO: How do I connect these to their implementors?
            ItemKind::ExtendBlock {
                target,
                extender,
                items: _,
            } => {
                let block_scope = Scope::ExtendBlock {
                    target: target.as_ref().clone(),
                    extender: extender.as_ref().map(|t| t.as_ref().clone()),
                };

                let node = self.push(block_scope);
                self.add_child(id, node).unwrap();
            }

            ItemKind::Alias { alias, actual } => {
                let alias_scope = Scope::Alias {
                    alias: alias.as_ref().clone(),
                    actual: actual.as_ref().clone(),
                };

                let node = self.push(alias_scope);
                self.add_child(id, node).unwrap();
            }
        }
    }

    #[inline]
    fn push_stmt(&mut self, parent: NodeId, stmt: &Stmt) {
        match &stmt.kind {
            StmtKind::Item(item) => self.push_item(parent, item),
            StmtKind::Expr(expr) => self.push_expr(parent, expr),
            StmtKind::VarDecl(decl) => {
                let VarDecl {
                    name,
                    ty,
                    val,
                    constant: _,
                    mutable: _,
                } = &**decl;

                let scope = self.node_mut(parent).unwrap();
                scope.vars_mut().push((*name, ty.as_ref().clone()));

                self.push_expr(parent, val);
            }
        }
    }

    // TODO: Expressions
    #[inline]
    fn push_expr(&mut self, _parent: NodeId, _expr: &Expr) {
        /*
            StmtKind::If {
                condition,
                body,
                clauses,
                else_clause,
            } => {
                self.push_expr(parent, *condition);

                let body_scope = self.push(Scope::new());
                self.add_child(parent, body_scope).unwrap();

                for stmt in body {
                    self.push_stmt(parent, *stmt);
                }

                for (condition, body) in clauses {
                    self.push_expr(parent, *condition);

                    let else_scope = self.push(Scope::new());
                    self.add_child(parent, else_scope).unwrap();

                    for stmt in body {
                        self.push_stmt(parent, *stmt);
                    }
                }

                if let Some(ref else_clause) = else_clause {
                    let else_scope = self.push(Scope::new());
                    self.add_child(parent, else_scope).unwrap();

                    for stmt in else_clause {
                        self.push_stmt(parent, *stmt);
                    }
                }
            }

            Stmt::Return(ret) => {
                if let Some(ret) = ret {
                    self.push_expr(parent, *ret);
                }
            }

            Stmt::Break(brk) => {
                if let Some(brk) = brk {
                    self.push_expr(parent, *brk);
                }
            }

            Stmt::Continue => {}

            Stmt::While {
                condition,
                body,
                then,
                else_clause,
            } => {
                self.push_expr(parent, *condition);

                let body_scope = self.push(Scope::new());
                self.add_child(parent, body_scope).unwrap();

                for stmt in body {
                    self.push_stmt(parent, *stmt);
                }

                if let Some(body) = then {
                    let then_scope = self.push(Scope::new());
                    self.add_child(parent, then_scope).unwrap();

                    for stmt in body {
                        self.push_stmt(parent, *stmt);
                    }
                }

                if let Some(else_clause) = else_clause {
                    let else_scope = self.push(Scope::new());
                    self.add_child(parent, else_scope).unwrap();

                    for stmt in else_clause {
                        self.push_stmt(parent, *stmt);
                    }
                }
            }

            Stmt::Loop { body, else_clause } => {
                let body_scope = self.push(Scope::new());
                self.add_child(parent, body_scope).unwrap();

                for stmt in body {
                    self.push_stmt(parent, *stmt);
                }

                if let Some(else_clause) = else_clause {
                    let else_scope = self.push(Scope::new());
                    self.add_child(parent, else_scope).unwrap();

                    for stmt in else_clause {
                        self.push_stmt(parent, *stmt);
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
                self.push_expr(parent, *var);
                self.push_expr(parent, *condition);

                let body_scope = self.push(Scope::new());
                self.add_child(parent, body_scope).unwrap();

                for stmt in body {
                    self.push_stmt(body_scope, *stmt);
                }

                if let Some(body) = then {
                    let then_scope = self.push(Scope::new());
                    self.add_child(parent, then_scope).unwrap();

                    for stmt in body {
                        self.push_stmt(parent, *stmt);
                    }
                }

                if let Some(else_clause) = else_clause {
                    let else_scope = self.push(Scope::new());
                    self.add_child(parent, else_scope).unwrap();

                    for stmt in else_clause {
                        self.push_stmt(parent, *stmt);
                    }
                }
            }

            Stmt::Match { var, arms } => {
                self.push_expr(parent, *var);

                for (_bind, clause, body) in arms {
                    let arm = self.push(Scope::new());
                    self.add_child(parent, arm).unwrap();

                    // FIXME: Figure out Binding/Pattern's repr in HIR
                    // self.node_mut(arm)
                    //     .unwrap()
                    //     .vars_mut()
                    //     .push((*bind, Type::Infer));

                    if let Some(clause) = clause {
                        self.push_expr(arm, *clause);
                    }

                    for stmt in body {
                        self.push_stmt(arm, *stmt);
                    }
                }
            }
        */
    }
}

impl<T: fmt::Debug, L: fmt::Debug> fmt::Debug for Graph<T, L> {
    #[inline]
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

    #[inline]
    fn index(&self, id: NodeId) -> &Self::Output {
        &self.nodes[id.to_usize()]
    }
}

impl<T, L> ops::IndexMut<NodeId> for Graph<T, L> {
    #[inline]
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
    #[inline]
    pub fn data(&self) -> &T {
        &self.data
    }

    #[inline]
    pub fn data_mut(&mut self) -> &mut T {
        &mut self.data
    }

    #[inline]
    pub fn parent(&self) -> Option<NodeId> {
        self.parent
    }

    #[inline]
    pub fn parent_mut(&mut self) -> &mut Option<NodeId> {
        &mut self.parent
    }

    #[inline]
    pub fn children(&self) -> &Vec<NodeId> {
        &self.children
    }

    #[inline]
    pub fn children_mut(&mut self) -> &mut Vec<NodeId> {
        &mut self.children
    }

    #[inline]
    pub fn links(&self) -> &Vec<L> {
        &self.links
    }

    #[inline]
    pub fn links_mut(&mut self) -> &mut Vec<L> {
        &mut self.links
    }
}

impl Node<Scope, MaybeSym> {
    #[inline]
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

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

impl<T, L> ops::DerefMut for Node<T, L> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.data
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct NodeId(NonZeroU32);

impl NodeId {
    #[inline]
    pub fn to_usize(self) -> usize {
        self.0.get() as usize
    }
}

impl fmt::Debug for NodeId {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "NodeId({})", self.0.get() - 1)
    }
}
