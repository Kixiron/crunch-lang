use crate::interner::SmallSpur;

use alloc::{
    rc::{Rc, Weak},
    vec::Vec,
};

trait Scopelike {
    fn get_parent(&self) -> Option<Rc<Scope>>;
    fn parent_mut<'a>(&'a mut self) -> Option<&'a mut Option<Weak<Scope>>>;
    fn children<'a>(&'a self) -> &'a Vec<Rc<Scope>>;
    fn children_mut<'a>(&'a mut self) -> &'a mut Vec<Rc<Scope>>;
    fn members<'a>(&'a self) -> &'a Vec<Symbol>;
    fn members_mut<'a>(&'a mut self) -> &'a mut Vec<Symbol>;
    fn define(&mut self, symbol: Symbol);
    fn resolve<'a>(&'a self, name: SmallSpur) -> Option<&'a Symbol>;
    fn push(&mut self, scope: Rc<Scope>);
    fn pop(&mut self) -> Option<Rc<Scope>>;
}

#[derive(Debug, Clone)]
enum Scope {
    Global {
        members: Vec<Symbol>,
        children: Vec<Rc<Scope>>,
    },
    Local {
        parent: Option<Weak<Scope>>,
        members: Vec<Symbol>,
        children: Vec<Rc<Scope>>,
    },
}

impl Scopelike for Scope {
    #[inline]
    fn get_parent(&self) -> Option<Rc<Scope>> {
        if let Self::Local { parent, .. } = self {
            parent.map(|p| p.upgrade().expect("Parent should be valid"))
        } else {
            None
        }
    }

    #[inline]
    fn parent_mut<'a>(&'a mut self) -> Option<&'a mut Option<Weak<Scope>>> {
        if let Self::Local { ref mut parent, .. } = self {
            Some(parent)
        } else {
            None
        }
    }

    #[inline]
    fn children<'a>(&'a self) -> &'a Vec<Rc<Scope>> {
        match self {
            Self::Global { ref children, .. } | Self::Local { ref children, .. } => children,
        }
    }

    #[inline]
    fn children_mut<'a>(&'a mut self) -> &'a mut Vec<Rc<Scope>> {
        match self {
            Self::Global {
                ref mut children, ..
            }
            | Self::Local {
                ref mut children, ..
            } => children,
        }
    }

    #[inline]
    fn members<'a>(&'a self) -> &'a Vec<Symbol> {
        match self {
            Self::Global { members, .. } | Self::Local { members, .. } => &members,
        }
    }

    #[inline]
    fn members_mut<'a>(&'a mut self) -> &'a mut Vec<Symbol> {
        match self {
            Self::Global {
                ref mut members, ..
            }
            | Self::Local {
                ref mut members, ..
            } => members,
        }
    }

    #[inline]
    fn define(&mut self, symbol: Symbol) {
        self.members_mut().push(symbol);
    }

    #[inline]
    fn resolve<'a>(&'a self, name: SmallSpur) -> Option<&'a Symbol> {
        if let Some(sym) = self.members().iter().find(|s| s.name() == name) {
            return Some(sym);
        }

        self.get_parent().map(|p| p.resolve(name)).flatten()
    }

    #[inline]
    fn push(&mut self, mut scope: Rc<Scope>) {
        *scope
            .parent_mut()
            .expect("Cannot push global scopes to children") = Some(Rc::downgrade(self));
        self.children_mut().push(scope);
    }

    #[inline]
    fn pop(&mut self) -> Option<Rc<Scope>> {
        self.children_mut().pop()
    }
}

#[derive(Debug, Clone)]
enum Symbol {
    Type { name: SmallSpur },
    Variable { name: SmallSpur },
    Function { name: SmallSpur, scope: Rc<Scope> },
}

impl Symbol {
    #[inline]
    pub fn name(&self) -> SmallSpur {
        match self {
            Self::Type { name, .. } | Self::Variable { name, .. } | Self::Function { name, .. } => {
                *name
            }
        }
    }
}

/*
use crate::{
    error::{Error, ErrorHandler, Locatable, Location, ParseResult, SemanticError},
    files::FileId,
    interner::{Interner, SmallSpur},
    parser::{Alias, Ast, EnumVariant, Expression, Import, Statement, SyntaxTree, Type},
};

use alloc::{borrow::ToOwned, vec, vec::Vec};
use cfg_if::cfg_if;
use core::{fmt, iter::FromIterator};
use stadium::Stadium;

cfg_if! {
    if #[cfg(feature = "no-std")] {
        use hashbrown::{HashMap, HashSet};
        use hashbrown::HashMap as ConMap;
        type MapRef<'a, 'stmt, 'expr> = &'a Module<'stmt, 'expr>;
    } else if #[cfg(feature = "concurrent")] {
        use dashmap::DashMap as ConMap;
        type MapRef<'a, 'stmt, 'expr> = dashmap::mapref::one::Ref<'a, FileLoc, Module<'stmt, 'expr>>;

        cfg_if! {
            if #[cfg(feature = "no-std")] {
                use hashbrown::{HashMap, HashSet};
            }  else {
                use std::collections::{HashMap, HashSet};
            }
        }
    } else {
        use std::collections::{HashMap, HashSet};
        use std::collections::HashMap as ConMap;
        type MapRef<'a, 'stmt, 'expr> = &'a Module<'stmt, 'expr>;
    }
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct FileLoc {
    file: FileId,
    sym: SmallSpur,
}

impl FileLoc {
    pub const fn new(file: FileId, name: SmallSpur) -> Self {
        Self { file, sym: name }
    }
}

#[derive(Debug)]
pub struct GlobalSymbolTable<'stmt, 'expr> {
    packages: HashMap<SmallSpur, Package<'stmt, 'expr>>,
}

impl<'stmt, 'expr> GlobalSymbolTable<'stmt, 'expr> {
    pub fn new() -> Self {
        Self {
            packages: HashMap::new(),
        }
    }

    pub fn from(name: SmallSpur, package: Package<'stmt, 'expr>) -> Self {
        Self {
            packages: HashMap::from_iter(vec![(name, package)].into_iter()),
        }
    }

    pub fn package(&self, pkg: SmallSpur) -> Option<&Package<'stmt, 'expr>> {
        self.packages.get(&pkg)
    }
}

impl<'stmt, 'expr> Default for GlobalSymbolTable<'stmt, 'expr> {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug)]
pub struct Package<'stmt, 'expr> {
    dependencies: Vec<()>,
    modules: ConMap<FileLoc, Module<'stmt, 'expr>>,
}

impl<'stmt, 'expr> Package<'stmt, 'expr> {
    pub fn new() -> Self {
        Self {
            dependencies: Vec::new(),
            modules: ConMap::new(),
        }
    }

    pub fn from(file: FileLoc, module: Module<'stmt, 'expr>) -> Self {
        Self {
            dependencies: Vec::new(),
            modules: ConMap::from_iter(vec![(file, module)].into_iter()),
        }
    }

    pub fn module<'a>(&'a self, module: FileLoc) -> Option<MapRef<'a, 'stmt, 'expr>> {
        self.modules.get(&module)
    }
}

impl<'stmt, 'expr> Default for Package<'stmt, 'expr> {
    fn default() -> Self {
        Self::new()
    }
}

pub struct Module<'stmt, 'expr> {
    pub imports: Vec<Import>,
    pub symbols: HashMap<SmallSpur, Symbol<'stmt, 'expr>>,
    pub aliases: Vec<Alias<'expr>>,
    __exprs: Stadium<'expr, Expression<'expr>>,
    __stmts: Stadium<'stmt, Statement<'expr, 'stmt>>,
}

impl<'stmt, 'expr> Module<'stmt, 'expr> {
    pub fn new(
        SyntaxTree {
            ast,
            __exprs,
            __stmts,
        }: SyntaxTree<'stmt, 'expr>,
        error_handler: &mut ErrorHandler,
        interner: &Interner,
    ) -> Self {
        let mut imports = Vec::with_capacity(5);
        let mut aliases = Vec::new();
        let mut symbols = HashMap::with_capacity(ast.len());

        let redefinition = |node: &Ast, loc| {
            Locatable::new(
                Error::Semantic(SemanticError::Redefinition {
                    name: interner.resolve(&node.name().unwrap()).to_owned(),
                    first: node.location(),
                    second: loc,
                }),
                loc,
            )
        };

        for node in ast {
            let (name, location) = (node.name().unwrap(), node.location());

            match &node {
                Ast::Function(func) => {
                    let sig = Signature::Func {
                        loc: func.loc(),
                        args: HashMap::from_iter(
                            func.data()
                                .args
                                .iter()
                                .map(|a| (*a.data().name.data(), a.data().ty.data().clone())),
                        ),
                        // generics: func.data().generics.clone(),
                        returns: func.data().returns.data().clone(),
                    };

                    if let Some(Symbol::Unresolved(_sig, old)) =
                        symbols.insert(name, Symbol::Unresolved(sig, node))
                    {
                        error_handler.push_err(redefinition(&old, location));
                    }
                }

                Ast::Type(ty) => {
                    let sig = Signature::Type {
                        loc: ty.loc(),
                        generics: ty.data().generics.clone(),
                        members: ty
                            .data()
                            .members
                            .iter()
                            .map(|m| {
                                Locatable::new((m.data().name, m.data().ty.data().clone()), m.loc())
                            })
                            .collect(),
                    };

                    if let Some(Symbol::Unresolved(_sig, old)) =
                        symbols.insert(name, Symbol::Unresolved(sig, node))
                    {
                        error_handler.push_err(redefinition(&old, location));
                    }
                }

                Ast::Enum(en) => {
                    let sig = Signature::Enum {
                        loc: en.loc(),
                        generics: en.data().generics.clone(),
                        variants: HashMap::from_iter(en.data().variants.iter().map(|v| {
                            (
                                v.data().name(),
                                Locatable::new(
                                    if let EnumVariant::Tuple { elements, .. } = v.data() {
                                        Some(elements.iter().map(|e| e.data().clone()).collect())
                                    } else {
                                        None
                                    },
                                    v.loc(),
                                ),
                            )
                        })),
                    };

                    if let Some(Symbol::Unresolved(_sig, old)) =
                        symbols.insert(name, Symbol::Unresolved(sig, node))
                    {
                        error_handler.push_err(redefinition(&old, location));
                    }
                }

                Ast::Trait(tr) => {
                    let sig = Signature::Trait {
                        loc: tr.loc(),
                        generics: tr.data().generics.clone(),
                        methods: HashSet::from_iter(
                            tr.data().methods.iter().map(|m| m.data().name),
                        ),
                    };

                    if let Some(Symbol::Unresolved(_sig, old)) =
                        symbols.insert(name, Symbol::Unresolved(sig, node))
                    {
                        error_handler.push_err(redefinition(&old, location));
                    }
                }

                Ast::ExtendBlock(block) => {}

                Ast::Import(import) => imports.push(import.data().clone()),
                Ast::Alias(alias) => aliases.push(alias.data().clone()),
            }
        }

        Self {
            imports,
            symbols,
            aliases,
            __exprs,
            __stmts,
        }
    }
}

impl fmt::Debug for Module<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Module")
            .field("imports", &self.imports)
            .field("symbols", &self.symbols)
            .finish()
    }
}

#[derive(Debug, Clone, PartialEq)]
#[repr(u8)]
#[allow(clippy::large_enum_variant)]
pub enum Symbol<'stmt, 'expr> {
    Unresolved(Signature, Ast<'stmt, 'expr>),
    Resolving,
    Resolved(),
}

#[derive(Debug, Clone, PartialEq)]
#[repr(u8)]
pub enum Signature {
    Func {
        loc: Location,
        args: HashMap<SmallSpur, Type>,
        //generics: Vec<Locatable<Type>>,
        returns: Type,
    },

    Type {
        loc: Location,
        generics: Vec<Locatable<Type>>,
        members: Vec<Locatable<(SmallSpur, Type)>>,
        // TODO: This is a doozy, need to resolve all blocks to find type methods
        // methods: HashSet<SmallSpur>,
    },

    Enum {
        loc: Location,
        generics: Vec<Locatable<Type>>,
        variants: HashMap<SmallSpur, Locatable<Option<Vec<Type>>>>,
    },

    Trait {
        loc: Location,
        generics: Vec<Locatable<Type>>,
        methods: HashSet<SmallSpur>,
    },
}
*/
