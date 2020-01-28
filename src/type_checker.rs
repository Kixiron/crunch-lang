use crate::parser::ast::*;
use std::collections::HashMap;
use string_interner::Sym;

pub(crate) struct TypeChecker {
    symbol_table: HashMap<Sym, Program>,
}
