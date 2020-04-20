use crate::{
    error::{Error, Locatable, Location, ParseResult, TypeError},
    files::FileId,
    interner::{Interner, SmallSpur},
    parser::{Ast, EnumVariant, Type, TypeMember},
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
        if let Some(_prev) = self.table.insert(key, val) {
            Err(Locatable::new(
                Error::Type(TypeError::Redefinition(
                    interner.resolve(&key.sym).to_owned(),
                    Location::file(key.file),
                )),
                Location::file(key.file),
            ))
        } else {
            Ok(())
        }
    }
}

// TODO: File location may not be needed, since they'll be hashmap-associated
#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct SymbolLocation {
    file: FileId,
    sym: SmallSpur,
}

impl SymbolLocation {
    pub fn new(file: FileId, sym: SmallSpur) -> Self {
        Self { file, sym }
    }
}

// TODO: Probably should do something about generics here
#[derive(Debug, Clone)]
pub enum Symbol {
    Function {
        params: Vec<(Type, bool)>,
        returns: Type,
    },

    Type {
        members: Vec<(SmallSpur, Type)>,
        // Potentially for associated structs/enums/traits eventually, currently only houses methods
        namespace: HashMap<SmallSpur, Symbol>,
    },

    Enum {
        members: HashMap<SmallSpur, EnumSymbol>,
    },

    Trait {
        // Potentially for associated structs/enums/traits eventually, currently only houses methods
        namespace: HashMap<SmallSpur, Symbol>,
    },

    // TODO: Maybe just list all the imported files, so those namespaces are the next to be searched?
    Import {},
}

#[derive(Debug, Clone)]
pub enum EnumSymbol {
    Unit,
    Tuple(Vec<Type>),
}

impl From<&Ast<'_, '_>> for Symbol {
    fn from(node: &Ast<'_, '_>) -> Self {
        match node {
            Ast::Function { args, returns, .. } => Self::Function {
                params: args.iter().map(|(_, ty, c)| (ty.clone(), *c)).collect(),
                returns: returns.clone(),
            },

            Ast::Type {
                members, methods, ..
            } => Self::Type {
                members: members
                    .iter()
                    .map(|TypeMember { name, ty, .. }| (*name, ty.clone()))
                    .collect(),
                namespace: HashMap::from_iter(
                    methods.iter().map(|node| (node.name(), Symbol::from(node))),
                ),
            },

            Ast::Enum { variants, .. } => Self::Enum {
                members: HashMap::from_iter(variants.iter().map(|var| {
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
            },

            Ast::Trait { methods, .. } => Self::Trait {
                namespace: HashMap::from_iter(
                    methods.iter().map(|node| (node.name(), Symbol::from(node))),
                ),
            },

            // TODO: ???
            Ast::Import { .. } => Self::Import {},
        }
    }
}
