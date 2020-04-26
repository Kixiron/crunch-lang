use crate::{
    error::{Error, Locatable, Location, ParseResult, SemanticError},
    interner::{Interner, SmallSpur},
    parser::{Ast, EnumVariant, FuncArg, Type},
};

use cfg_if::cfg_if;
use core::iter::FromIterator;

cfg_if! {
    if #[cfg(feature = "no-std")] {
        use hashbrown::HashMap;
    } else {
        use std::collections::HashMap;
    }
}

#[derive(Debug, Clone)]
pub struct SymbolTable {
    table: HashMap<SymbolLocation, Symbol>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            table: HashMap::new(),
        }
    }

    pub fn insert(
        &mut self,
        interner: &Interner,
        key: SymbolLocation,
        val: Symbol,
    ) -> ParseResult<()> {
        if let Some(prev) = self.table.insert(key, val) {
            Err(Locatable::new(
                Error::Semantic(SemanticError::Redefinition {
                    name: interner.resolve(&key.sym).to_owned(),
                    first: prev.sig_loc(),
                    second: key.loc,
                }),
                key.loc,
            ))
        } else {
            Ok(())
        }
    }
}

// TODO: File location may not be needed, since they'll be hashmap-associated
#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct SymbolLocation {
    loc: Location,
    sym: SmallSpur,
}

impl SymbolLocation {
    pub fn new(loc: Location, sym: SmallSpur) -> Self {
        Self { loc, sym }
    }
}

// TODO: Probably should do something about generics here
#[derive(Debug, Clone)]
pub enum Symbol {
    Function {
        args: Vec<Locatable<FuncArg>>,
        returns: Locatable<Type>,
        sig_loc: Location,
    },

    Type {
        members: Vec<(SmallSpur, Locatable<Type>)>,
        // Potentially for associated structs/enums/traits eventually, currently only houses methods
        namespace: HashMap<SmallSpur, Symbol>,
        sig_loc: Location,
    },

    Enum {
        members: HashMap<SmallSpur, EnumSymbol>,
        sig_loc: Location,
    },

    Trait {
        // Potentially for associated structs/enums/traits eventually, currently only houses methods
        namespace: HashMap<SmallSpur, Symbol>,
        sig_loc: Location,
    },

    // TODO: Maybe just list all the imported files, so those namespaces are the next to be searched?
    Import {
        sig_loc: Location,
    },
}

impl Symbol {
    pub fn sig_loc(&self) -> Location {
        match self {
            Self::Function { sig_loc, .. } => *sig_loc,
            Self::Type { sig_loc, .. } => *sig_loc,
            Self::Enum { sig_loc, .. } => *sig_loc,
            Self::Trait { sig_loc, .. } => *sig_loc,
            Self::Import { sig_loc, .. } => *sig_loc,
        }
    }
}

impl From<&Ast<'_, '_>> for Symbol {
    fn from(node: &Ast<'_, '_>) -> Self {
        match node {
            Ast::Function(func) => Self::Function {
                args: func.data().args.clone(),
                returns: func.data().returns.clone(),
                sig_loc: func.loc(),
            },

            Ast::Type(ty) => {
                Self::Type {
                    members: ty
                        .data()
                        .members
                        .iter()
                        .map(|member| (member.data().name, member.data().ty.clone()))
                        .collect(),
                    sig_loc: ty.loc(),
                    namespace: HashMap::from_iter(ty.data().methods.iter().map(|node| {
                        (node.data().name, Symbol::from(&Ast::Function(node.clone())))
                    })),
                }
            }

            Ast::Enum(enu) => Self::Enum {
                members: HashMap::from_iter(enu.data().variants.iter().map(|var| {
                    (
                        var.name(),
                        match var {
                            EnumVariant::Unit { .. } => EnumSymbol::Unit,
                            EnumVariant::Tuple { elements, .. } => {
                                EnumSymbol::Tuple(elements.clone())
                            }
                        },
                    )
                })),
                sig_loc: enu.loc(),
            },

            Ast::Trait(trai) => {
                Self::Trait {
                    namespace: HashMap::from_iter(trai.data().methods.iter().map(|node| {
                        (node.data().name, Symbol::from(&Ast::Function(node.clone())))
                    })),
                    sig_loc: trai.loc(),
                }
            }

            Ast::Import(import) => Self::Import {
                sig_loc: import.loc(),
            },
        }
    }
}

#[derive(Debug, Clone)]
pub enum EnumSymbol {
    Unit,
    Tuple(Vec<Locatable<Type>>),
}
