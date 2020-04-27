use crate::{
    error::{Error, ErrorHandler, Locatable, Location, SemanticError},
    files::FileId,
    interner::{Interner, SmallSpur},
    parser::{Ast, EnumVariant, Expression, Import, Statement, SyntaxTree, Type},
};

use cfg_if::cfg_if;
use core::{convert::TryFrom, fmt, iter::FromIterator};
use dashmap::DashMap;
use stadium::Stadium;

cfg_if! {
    if #[cfg(feature = "no-std")] {
        use hashbrown::{HashMap, HashSet};
    } else {
        use std::collections::{HashMap, HashSet};
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

    pub fn package(&self, pkg: &SmallSpur) -> Option<&Package<'stmt, 'expr>> {
        self.packages.get(pkg)
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
    modules: DashMap<FileLoc, Module<'stmt, 'expr>>,
}

impl<'stmt, 'expr> Package<'stmt, 'expr> {
    pub fn new() -> Self {
        Self {
            dependencies: Vec::new(),
            modules: DashMap::new(),
        }
    }

    pub fn from(file: FileLoc, module: Module<'stmt, 'expr>) -> Self {
        Self {
            dependencies: Vec::new(),
            modules: DashMap::from_iter(vec![(file, module)].into_iter()),
        }
    }

    pub fn module<'a>(
        &'a self,
        module: &FileLoc,
    ) -> Option<dashmap::mapref::one::Ref<'a, FileLoc, Module<'stmt, 'expr>>> {
        self.modules.get(module)
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
        let mut symbols = HashMap::with_capacity(ast.len());

        for node in ast {
            let (name, location) = (node.name(), node.location());

            match Symbol::try_from(node) {
                Ok(sym) => {
                    if let Some(Symbol::Unresolved(_sig, node)) = symbols.insert(name, sym) {
                        error_handler.push_err(Locatable::new(
                            Error::Semantic(SemanticError::Redefinition {
                                name: interner.resolve(&node.name()).to_owned(),
                                first: node.location(),
                                second: location,
                            }),
                            location,
                        ));
                    }
                }

                Err(import) => imports.push(import.into_data()),
            }
        }

        Self {
            imports,
            symbols,
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
pub enum Symbol<'stmt, 'expr> {
    Unresolved(Signature, Ast<'stmt, 'expr>),
    Resolving,
    Resolved(),
}

impl<'stmt, 'expr> TryFrom<Ast<'stmt, 'expr>> for Symbol<'stmt, 'expr> {
    type Error = Locatable<Import>;

    fn try_from(node: Ast<'stmt, 'expr>) -> Result<Self, Self::Error> {
        let symbol = match &node {
            Ast::Function(func) => Signature::Func {
                loc: func.loc(),
                args: HashMap::from_iter(
                    func.data()
                        .args
                        .iter()
                        .map(|a| (*a.data().name.data(), a.data().ty.data().clone())),
                ),
                //generics: func.data().generics.clone(),
                returns: func.data().returns.data().clone(),
            },

            Ast::Type(ty) => Signature::Type {
                loc: ty.loc(),
                generics: ty.data().generics.clone(),
                members: ty
                    .data()
                    .members
                    .iter()
                    .map(|m| Locatable::new((m.data().name, m.data().ty.data().clone()), m.loc()))
                    .collect(),
                methods: HashSet::from_iter(ty.data().methods.iter().map(|m| m.data().name)),
            },

            Ast::Enum(en) => Signature::Enum {
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
            },

            Ast::Trait(tr) => Signature::Trait {
                loc: tr.loc(),
                generics: tr.data().generics.clone(),
                methods: HashSet::from_iter(tr.data().methods.iter().map(|m| m.data().name)),
            },

            Ast::Import(_) => {
                if let Ast::Import(import) = node {
                    return Err(import);
                } else {
                    unreachable!();
                }
            }
        };

        Ok(Self::Unresolved(symbol, node))
    }
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
        methods: HashSet<SmallSpur>,
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
