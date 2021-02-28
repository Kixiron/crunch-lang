#![allow(
    path_statements,
    unused_imports,
    non_snake_case,
    non_camel_case_types,
    non_upper_case_globals,
    unused_parens,
    non_shorthand_field_patterns,
    dead_code,
    overflowing_literals,
    unreachable_patterns,
    unused_variables,
    clippy::missing_safety_doc,
    clippy::match_single_binding,
    clippy::ptr_arg,
    clippy::redundant_closure,
    clippy::needless_lifetimes,
    clippy::borrowed_box,
    clippy::map_clone,
    clippy::toplevel_ref_arg,
    clippy::double_parens,
    clippy::collapsible_if,
    clippy::clone_on_copy,
    clippy::unused_unit,
    clippy::deref_addrof,
    clippy::clone_on_copy,
    clippy::needless_return,
    clippy::op_ref,
    clippy::match_like_matches_macro,
    clippy::comparison_chain,
    clippy::len_zero,
    clippy::extra_unused_lifetimes
)]

use ::num::One;
use ::std::ops::Deref;

use ::differential_dataflow::collection;
use ::timely::communication;
use ::timely::dataflow::scopes;
use ::timely::worker;

use ::ddlog_derive::{FromRecord, IntoRecord, Mutator};
use ::differential_datalog::ddval::DDValue;
use ::differential_datalog::ddval::DDValConvert;
use ::differential_datalog::program;
use ::differential_datalog::program::TupleTS;
use ::differential_datalog::program::XFormArrangement;
use ::differential_datalog::program::XFormCollection;
use ::differential_datalog::program::Weight;
use ::differential_datalog::record::FromRecord;
use ::differential_datalog::record::IntoRecord;
use ::differential_datalog::record::Mutator;
use ::serde::Deserialize;
use ::serde::Serialize;


// `usize` and `isize` are builtin Rust types; we therefore declare an alias to DDlog's `usize` and
// `isize`.
pub type std_usize = u64;
pub type std_isize = i64;


use core::fmt::Debug;

pub fn dbg<T: Debug>(thing: T) -> T {
    println!("[ddlog debug]: {:?}", thing);
    thing
}

#[derive(Eq, Ord, Clone, Hash, PartialEq, PartialOrd, IntoRecord, Mutator, Default, Serialize, Deserialize, FromRecord)]
#[ddlog(rename = "Errors")]
pub struct Errors {
    pub message: String
}
impl abomonation::Abomonation for Errors{}
impl ::std::fmt::Display for Errors {
    fn fmt(&self, __formatter: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match self {
            Errors{message} => {
                __formatter.write_str("Errors{")?;
                differential_datalog::record::format_ddlog_str(message, __formatter)?;
                __formatter.write_str("}")
            }
        }
    }
}
impl ::std::fmt::Debug for Errors {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        ::std::fmt::Display::fmt(&self, f)
    }
}
#[derive(Eq, Ord, Clone, Hash, PartialEq, PartialOrd, IntoRecord, Mutator, Default, Serialize, Deserialize, FromRecord)]
#[ddlog(rename = "Expressions")]
pub struct Expressions {
    pub expr_id: types__hir::ExprId,
    pub expr: types__hir::Expr
}
impl abomonation::Abomonation for Expressions{}
impl ::std::fmt::Display for Expressions {
    fn fmt(&self, __formatter: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match self {
            Expressions{expr_id,expr} => {
                __formatter.write_str("Expressions{")?;
                ::std::fmt::Debug::fmt(expr_id, __formatter)?;
                __formatter.write_str(",")?;
                ::std::fmt::Debug::fmt(expr, __formatter)?;
                __formatter.write_str("}")
            }
        }
    }
}
impl ::std::fmt::Debug for Expressions {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        ::std::fmt::Display::fmt(&self, f)
    }
}
#[derive(Eq, Ord, Clone, Hash, PartialEq, PartialOrd, IntoRecord, Mutator, Default, Serialize, Deserialize, FromRecord)]
#[ddlog(rename = "Functions")]
pub struct Functions {
    pub func_id: types__hir::FuncId,
    pub func: types__hir::Function
}
impl abomonation::Abomonation for Functions{}
impl ::std::fmt::Display for Functions {
    fn fmt(&self, __formatter: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match self {
            Functions{func_id,func} => {
                __formatter.write_str("Functions{")?;
                ::std::fmt::Debug::fmt(func_id, __formatter)?;
                __formatter.write_str(",")?;
                ::std::fmt::Debug::fmt(func, __formatter)?;
                __formatter.write_str("}")
            }
        }
    }
}
impl ::std::fmt::Debug for Functions {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        ::std::fmt::Display::fmt(&self, f)
    }
}
#[derive(Eq, Ord, Clone, Hash, PartialEq, PartialOrd, IntoRecord, Mutator, Default, Serialize, Deserialize, FromRecord)]
#[ddlog(rename = "Items")]
pub struct Items {
    pub id: types__hir::ItemId,
    pub item: types__hir::Item
}
impl abomonation::Abomonation for Items{}
impl ::std::fmt::Display for Items {
    fn fmt(&self, __formatter: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match self {
            Items{id,item} => {
                __formatter.write_str("Items{")?;
                ::std::fmt::Debug::fmt(id, __formatter)?;
                __formatter.write_str(",")?;
                ::std::fmt::Debug::fmt(item, __formatter)?;
                __formatter.write_str("}")
            }
        }
    }
}
impl ::std::fmt::Debug for Items {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        ::std::fmt::Display::fmt(&self, f)
    }
}
#[derive(Eq, Ord, Clone, Hash, PartialEq, PartialOrd, IntoRecord, Mutator, Default, Serialize, Deserialize, FromRecord)]
#[ddlog(rename = "Literals")]
pub struct Literals {
    pub lit: internment::Intern<types__hir::Literal>,
    pub lit_type: types__hir::TypeId
}
impl abomonation::Abomonation for Literals{}
impl ::std::fmt::Display for Literals {
    fn fmt(&self, __formatter: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match self {
            Literals{lit,lit_type} => {
                __formatter.write_str("Literals{")?;
                ::std::fmt::Debug::fmt(lit, __formatter)?;
                __formatter.write_str(",")?;
                ::std::fmt::Debug::fmt(lit_type, __formatter)?;
                __formatter.write_str("}")
            }
        }
    }
}
impl ::std::fmt::Debug for Literals {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        ::std::fmt::Display::fmt(&self, f)
    }
}
#[derive(Eq, Ord, Clone, Hash, PartialEq, PartialOrd, IntoRecord, Mutator, Default, Serialize, Deserialize, FromRecord)]
#[ddlog(rename = "Statements")]
pub struct Statements {
    pub stmt_id: types__hir::StmtId,
    pub stmt: types__hir::Stmt
}
impl abomonation::Abomonation for Statements{}
impl ::std::fmt::Display for Statements {
    fn fmt(&self, __formatter: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match self {
            Statements{stmt_id,stmt} => {
                __formatter.write_str("Statements{")?;
                ::std::fmt::Debug::fmt(stmt_id, __formatter)?;
                __formatter.write_str(",")?;
                ::std::fmt::Debug::fmt(stmt, __formatter)?;
                __formatter.write_str("}")
            }
        }
    }
}
impl ::std::fmt::Debug for Statements {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        ::std::fmt::Display::fmt(&self, f)
    }
}
#[derive(Eq, Ord, Clone, Hash, PartialEq, PartialOrd, IntoRecord, Mutator, Default, Serialize, Deserialize, FromRecord)]
#[ddlog(rename = "TypedExpressions")]
pub struct TypedExpressions {
    pub expr_id: types__hir::ExprId,
    pub type_id: types__hir::TypeId
}
impl abomonation::Abomonation for TypedExpressions{}
impl ::std::fmt::Display for TypedExpressions {
    fn fmt(&self, __formatter: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match self {
            TypedExpressions{expr_id,type_id} => {
                __formatter.write_str("TypedExpressions{")?;
                ::std::fmt::Debug::fmt(expr_id, __formatter)?;
                __formatter.write_str(",")?;
                ::std::fmt::Debug::fmt(type_id, __formatter)?;
                __formatter.write_str("}")
            }
        }
    }
}
impl ::std::fmt::Debug for TypedExpressions {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        ::std::fmt::Display::fmt(&self, f)
    }
}
#[derive(Eq, Ord, Clone, Hash, PartialEq, PartialOrd, IntoRecord, Mutator, Default, Serialize, Deserialize, FromRecord)]
#[ddlog(rename = "Types")]
pub struct Types {
    pub type_id: types__hir::TypeId,
    pub ty: types__hir::Type
}
impl abomonation::Abomonation for Types{}
impl ::std::fmt::Display for Types {
    fn fmt(&self, __formatter: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match self {
            Types{type_id,ty} => {
                __formatter.write_str("Types{")?;
                ::std::fmt::Debug::fmt(type_id, __formatter)?;
                __formatter.write_str(",")?;
                ::std::fmt::Debug::fmt(ty, __formatter)?;
                __formatter.write_str("}")
            }
        }
    }
}
impl ::std::fmt::Debug for Types {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        ::std::fmt::Display::fmt(&self, f)
    }
}
#[derive(Eq, Ord, Clone, Hash, PartialEq, PartialOrd, IntoRecord, Mutator, Default, Serialize, Deserialize, FromRecord)]
#[ddlog(rename = "UnifiedTypes")]
pub struct UnifiedTypes {
    pub expr_id: types__hir::ExprId,
    pub type_id: types__hir::TypeId
}
impl abomonation::Abomonation for UnifiedTypes{}
impl ::std::fmt::Display for UnifiedTypes {
    fn fmt(&self, __formatter: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match self {
            UnifiedTypes{expr_id,type_id} => {
                __formatter.write_str("UnifiedTypes{")?;
                ::std::fmt::Debug::fmt(expr_id, __formatter)?;
                __formatter.write_str(",")?;
                ::std::fmt::Debug::fmt(type_id, __formatter)?;
                __formatter.write_str("}")
            }
        }
    }
}
impl ::std::fmt::Debug for UnifiedTypes {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        ::std::fmt::Display::fmt(&self, f)
    }
}
#[derive(Eq, Ord, Clone, Hash, PartialEq, PartialOrd, IntoRecord, Mutator, Default, Serialize, Deserialize, FromRecord)]
#[ddlog(rename = "VariableScopes")]
pub struct VariableScopes {
    pub parent: types__hir::ScopeId,
    pub child: types__hir::ScopeId
}
impl abomonation::Abomonation for VariableScopes{}
impl ::std::fmt::Display for VariableScopes {
    fn fmt(&self, __formatter: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match self {
            VariableScopes{parent,child} => {
                __formatter.write_str("VariableScopes{")?;
                ::std::fmt::Debug::fmt(parent, __formatter)?;
                __formatter.write_str(",")?;
                ::std::fmt::Debug::fmt(child, __formatter)?;
                __formatter.write_str("}")
            }
        }
    }
}
impl ::std::fmt::Debug for VariableScopes {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        ::std::fmt::Display::fmt(&self, f)
    }
}
#[derive(Eq, Ord, Clone, Hash, PartialEq, PartialOrd, IntoRecord, Mutator, Default, Serialize, Deserialize, FromRecord)]
#[ddlog(rename = "Variables")]
pub struct Variables {
    pub var_id: types__hir::VarId,
    pub decl: types__hir::VariableDecl
}
impl abomonation::Abomonation for Variables{}
impl ::std::fmt::Display for Variables {
    fn fmt(&self, __formatter: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match self {
            Variables{var_id,decl} => {
                __formatter.write_str("Variables{")?;
                ::std::fmt::Debug::fmt(var_id, __formatter)?;
                __formatter.write_str(",")?;
                ::std::fmt::Debug::fmt(decl, __formatter)?;
                __formatter.write_str("}")
            }
        }
    }
}
impl ::std::fmt::Debug for Variables {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        ::std::fmt::Display::fmt(&self, f)
    }
}
#[derive(Eq, Ord, Clone, Hash, PartialEq, PartialOrd, IntoRecord, Mutator, Default, Serialize, Deserialize, FromRecord)]
#[ddlog(rename = "VariablesInScope")]
pub struct VariablesInScope {
    pub scope: types__hir::ScopeId,
    pub var_id: types__hir::VarId,
    pub decl_scope: types__hir::ScopeId
}
impl abomonation::Abomonation for VariablesInScope{}
impl ::std::fmt::Display for VariablesInScope {
    fn fmt(&self, __formatter: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match self {
            VariablesInScope{scope,var_id,decl_scope} => {
                __formatter.write_str("VariablesInScope{")?;
                ::std::fmt::Debug::fmt(scope, __formatter)?;
                __formatter.write_str(",")?;
                ::std::fmt::Debug::fmt(var_id, __formatter)?;
                __formatter.write_str(",")?;
                ::std::fmt::Debug::fmt(decl_scope, __formatter)?;
                __formatter.write_str("}")
            }
        }
    }
}
impl ::std::fmt::Debug for VariablesInScope {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        ::std::fmt::Display::fmt(&self, f)
    }
}
/* fn dbg<T: ::ddlog_rt::Val>(thing: & T) -> T */
pub fn unify_types<K: ::ddlog_rt::Val>(types: & ddlog_std::Group<K, ddlog_std::tuple2<types__hir::TypeId, types__hir::Type>>) -> types__hir::TypeId
{   for ref ty in types.iter() {
        {
            dbg(ty);
            ()
        }
    };
    (1 as u64)
}
pub static __Arng_Expressions_0 : ::once_cell::sync::Lazy<program::Arrangement> = ::once_cell::sync::Lazy::new(|| program::Arrangement::Map{
                                                                                                                     name: std::borrow::Cow::from(r###"(Expressions{.expr_id=(_: bit<64>), .expr=(hir::Expr{.kind=(hir::ExprVar{.variable=(_0: bit<64>)}: hir::ExprKind)}: hir::Expr)}: Expressions) /*join*/"###),
                                                                                                                      afun: {fn __f(__v: DDValue) -> Option<(DDValue,DDValue)>
                                                                                                                      {
                                                                                                                          let __cloned = __v.clone();
                                                                                                                          match <Expressions>::from_ddvalue(__v) {
                                                                                                                              Expressions{expr_id: _, expr: types__hir::Expr{kind: types__hir::ExprKind::ExprVar{variable: ref _0}}} => Some(((*_0).clone()).into_ddvalue()),
                                                                                                                              _ => None
                                                                                                                          }.map(|x|(x,__cloned))
                                                                                                                      }
                                                                                                                      __f},
                                                                                                                      queryable: false
                                                                                                                  });
pub static __Arng_Expressions_1 : ::once_cell::sync::Lazy<program::Arrangement> = ::once_cell::sync::Lazy::new(|| program::Arrangement::Map{
                                                                                                                     name: std::borrow::Cow::from(r###"(Expressions{.expr_id=(_: bit<64>), .expr=(hir::Expr{.kind=(hir::ExprAssign{.variable=(_0: bit<64>), .expr_id=(_: bit<64>)}: hir::ExprKind)}: hir::Expr)}: Expressions) /*join*/"###),
                                                                                                                      afun: {fn __f(__v: DDValue) -> Option<(DDValue,DDValue)>
                                                                                                                      {
                                                                                                                          let __cloned = __v.clone();
                                                                                                                          match <Expressions>::from_ddvalue(__v) {
                                                                                                                              Expressions{expr_id: _, expr: types__hir::Expr{kind: types__hir::ExprKind::ExprAssign{variable: ref _0, expr_id: _}}} => Some(((*_0).clone()).into_ddvalue()),
                                                                                                                              _ => None
                                                                                                                          }.map(|x|(x,__cloned))
                                                                                                                      }
                                                                                                                      __f},
                                                                                                                      queryable: false
                                                                                                                  });
pub static __Arng_Types_0 : ::once_cell::sync::Lazy<program::Arrangement> = ::once_cell::sync::Lazy::new(|| program::Arrangement::Map{
                                                                                                               name: std::borrow::Cow::from(r###"(Types{.type_id=(_: bit<64>), .ty=(_0: hir::Type)}: Types) /*join*/"###),
                                                                                                                afun: {fn __f(__v: DDValue) -> Option<(DDValue,DDValue)>
                                                                                                                {
                                                                                                                    let __cloned = __v.clone();
                                                                                                                    match <Types>::from_ddvalue(__v) {
                                                                                                                        Types{type_id: _, ty: ref _0} => Some(((*_0).clone()).into_ddvalue()),
                                                                                                                        _ => None
                                                                                                                    }.map(|x|(x,__cloned))
                                                                                                                }
                                                                                                                __f},
                                                                                                                queryable: false
                                                                                                            });
pub static __Arng_Types_1 : ::once_cell::sync::Lazy<program::Arrangement> = ::once_cell::sync::Lazy::new(|| program::Arrangement::Map{
                                                                                                               name: std::borrow::Cow::from(r###"(Types{.type_id=(_0: bit<64>), .ty=(_: hir::Type)}: Types) /*join*/"###),
                                                                                                                afun: {fn __f(__v: DDValue) -> Option<(DDValue,DDValue)>
                                                                                                                {
                                                                                                                    let __cloned = __v.clone();
                                                                                                                    match <Types>::from_ddvalue(__v) {
                                                                                                                        Types{type_id: ref _0, ty: _} => Some(((*_0).clone()).into_ddvalue()),
                                                                                                                        _ => None
                                                                                                                    }.map(|x|(x,__cloned))
                                                                                                                }
                                                                                                                __f},
                                                                                                                queryable: false
                                                                                                            });
pub static __Arng_VariableScopes_0 : ::once_cell::sync::Lazy<program::Arrangement> = ::once_cell::sync::Lazy::new(|| program::Arrangement::Map{
                                                                                                                        name: std::borrow::Cow::from(r###"(VariableScopes{.parent=(_0: bit<32>), .child=(_: bit<32>)}: VariableScopes) /*join*/"###),
                                                                                                                         afun: {fn __f(__v: DDValue) -> Option<(DDValue,DDValue)>
                                                                                                                         {
                                                                                                                             let __cloned = __v.clone();
                                                                                                                             match <VariableScopes>::from_ddvalue(__v) {
                                                                                                                                 VariableScopes{parent: ref _0, child: _} => Some(((*_0).clone()).into_ddvalue()),
                                                                                                                                 _ => None
                                                                                                                             }.map(|x|(x,__cloned))
                                                                                                                         }
                                                                                                                         __f},
                                                                                                                         queryable: false
                                                                                                                     });
pub static __Arng_Variables_0 : ::once_cell::sync::Lazy<program::Arrangement> = ::once_cell::sync::Lazy::new(|| program::Arrangement::Map{
                                                                                                                   name: std::borrow::Cow::from(r###"(Variables{.var_id=(_0: bit<64>), .decl=(hir::VariableDecl{.var_name=(_: bit<32>), .var_type=(_: bit<64>), .value=(_: bit<64>), .scope=(_: bit<32>)}: hir::VariableDecl)}: Variables) /*join*/"###),
                                                                                                                    afun: {fn __f(__v: DDValue) -> Option<(DDValue,DDValue)>
                                                                                                                    {
                                                                                                                        let __cloned = __v.clone();
                                                                                                                        match <Variables>::from_ddvalue(__v) {
                                                                                                                            Variables{var_id: ref _0, decl: types__hir::VariableDecl{var_name: _, var_type: _, value: _, scope: _}} => Some(((*_0).clone()).into_ddvalue()),
                                                                                                                            _ => None
                                                                                                                        }.map(|x|(x,__cloned))
                                                                                                                    }
                                                                                                                    __f},
                                                                                                                    queryable: false
                                                                                                                });
pub static __Arng_TypedExpressions_0 : ::once_cell::sync::Lazy<program::Arrangement> = ::once_cell::sync::Lazy::new(|| program::Arrangement::Map{
                                                                                                                          name: std::borrow::Cow::from(r###"(TypedExpressions{.expr_id=(_: bit<64>), .type_id=(_0: bit<64>)}: TypedExpressions) /*join*/"###),
                                                                                                                           afun: {fn __f(__v: DDValue) -> Option<(DDValue,DDValue)>
                                                                                                                           {
                                                                                                                               let __cloned = __v.clone();
                                                                                                                               match <TypedExpressions>::from_ddvalue(__v) {
                                                                                                                                   TypedExpressions{expr_id: _, type_id: ref _0} => Some(((*_0).clone()).into_ddvalue()),
                                                                                                                                   _ => None
                                                                                                                               }.map(|x|(x,__cloned))
                                                                                                                           }
                                                                                                                           __f},
                                                                                                                           queryable: false
                                                                                                                       });
pub static __Arng_TypedExpressions_1 : ::once_cell::sync::Lazy<program::Arrangement> = ::once_cell::sync::Lazy::new(|| program::Arrangement::Map{
                                                                                                                          name: std::borrow::Cow::from(r###"(TypedExpressions{.expr_id=(_0: bit<64>), .type_id=(_: bit<64>)}: TypedExpressions) /*join*/"###),
                                                                                                                           afun: {fn __f(__v: DDValue) -> Option<(DDValue,DDValue)>
                                                                                                                           {
                                                                                                                               let __cloned = __v.clone();
                                                                                                                               match <TypedExpressions>::from_ddvalue(__v) {
                                                                                                                                   TypedExpressions{expr_id: ref _0, type_id: _} => Some(((*_0).clone()).into_ddvalue()),
                                                                                                                                   _ => None
                                                                                                                               }.map(|x|(x,__cloned))
                                                                                                                           }
                                                                                                                           __f},
                                                                                                                           queryable: false
                                                                                                                       });
pub static __Arng_VariablesInScope_0 : ::once_cell::sync::Lazy<program::Arrangement> = ::once_cell::sync::Lazy::new(|| program::Arrangement::Map{
                                                                                                                          name: std::borrow::Cow::from(r###"(VariablesInScope{.scope=(_0: bit<32>), .var_id=(_: bit<64>), .decl_scope=(_: bit<32>)}: VariablesInScope) /*join*/"###),
                                                                                                                           afun: {fn __f(__v: DDValue) -> Option<(DDValue,DDValue)>
                                                                                                                           {
                                                                                                                               let __cloned = __v.clone();
                                                                                                                               match <VariablesInScope>::from_ddvalue(__v) {
                                                                                                                                   VariablesInScope{scope: ref _0, var_id: _, decl_scope: _} => Some(((*_0).clone()).into_ddvalue()),
                                                                                                                                   _ => None
                                                                                                                               }.map(|x|(x,__cloned))
                                                                                                                           }
                                                                                                                           __f},
                                                                                                                           queryable: false
                                                                                                                       });
pub static __Arng___Null_0 : ::once_cell::sync::Lazy<program::Arrangement> = ::once_cell::sync::Lazy::new(|| program::Arrangement::Map{
                                                                                                                name: std::borrow::Cow::from(r###"_ /*join*/"###),
                                                                                                                 afun: {fn __f(__v: DDValue) -> Option<(DDValue,DDValue)>
                                                                                                                 {
                                                                                                                     let __cloned = __v.clone();
                                                                                                                     match <()>::from_ddvalue(__v) {
                                                                                                                         _ => Some((()).into_ddvalue()),
                                                                                                                         _ => None
                                                                                                                     }.map(|x|(x,__cloned))
                                                                                                                 }
                                                                                                                 __f},
                                                                                                                 queryable: true
                                                                                                             });
pub static __Rule_Literals_0 : ::once_cell::sync::Lazy<program::Rule> = ::once_cell::sync::Lazy::new(|| /* Literals[(Literals{.lit=lit, .lit_type=lit_type}: Literals)] :- Expressions[(Expressions{.expr_id=(_: bit<64>), .expr=(hir::Expr{.kind=(hir::ExprLit{.lit=(lit: internment::Intern<hir::Literal>)}: hir::ExprKind)}: hir::Expr)}: Expressions)], ((var ty: hir::Type) = (hir::type_of(lit))), Types[(Types{.type_id=(lit_type: bit<64>), .ty=(ty: hir::Type)}: Types)]. */
                                                                                                        program::Rule::CollectionRule {
                                                                                                            description: std::borrow::Cow::from("Literals(.lit=lit, .lit_type=lit_type) :- Expressions(.expr_id=_, .expr=hir::Expr{.kind=hir::ExprLit{.lit=lit}}), (var ty = (hir::type_of(lit))), Types(.type_id=lit_type, .ty=ty)."),
                                                                                                            rel: 1,
                                                                                                            xform: Some(XFormCollection::Arrange{
                                                                                                                            description: std::borrow::Cow::from("arrange Expressions(.expr_id=_, .expr=hir::Expr{.kind=hir::ExprLit{.lit=lit}}) by (ty)"),
                                                                                                                            afun: {fn __f(__v: DDValue) -> Option<(DDValue,DDValue)>
                                                                                                                            {
                                                                                                                                let ref lit = match *<Expressions>::from_ddvalue_ref(&__v) {
                                                                                                                                    Expressions{expr_id: _, expr: types__hir::Expr{kind: types__hir::ExprKind::ExprLit{lit: ref lit}}} => (*lit).clone(),
                                                                                                                                    _ => return None
                                                                                                                                };
                                                                                                                                let ref ty: types__hir::Type = match types__hir::type_of(lit) {
                                                                                                                                    ty => ty,
                                                                                                                                    _ => return None
                                                                                                                                };
                                                                                                                                Some((((*ty).clone()).into_ddvalue(), ((*lit).clone()).into_ddvalue()))
                                                                                                                            }
                                                                                                                            __f},
                                                                                                                            next: Box::new(XFormArrangement::Join{
                                                                                                                                               description: std::borrow::Cow::from("Expressions(.expr_id=_, .expr=hir::Expr{.kind=hir::ExprLit{.lit=lit}}), (var ty = (hir::type_of(lit))), Types(.type_id=lit_type, .ty=ty)"),
                                                                                                                                               ffun: None,
                                                                                                                                               arrangement: (7,0),
                                                                                                                                               jfun: {fn __f(_: &DDValue, __v1: &DDValue, __v2: &DDValue) -> Option<DDValue>
                                                                                                                                               {
                                                                                                                                                   let ref lit = *<internment::Intern<types__hir::Literal>>::from_ddvalue_ref( __v1 );
                                                                                                                                                   let ref lit_type = match *<Types>::from_ddvalue_ref(__v2) {
                                                                                                                                                       Types{type_id: ref lit_type, ty: _} => (*lit_type).clone(),
                                                                                                                                                       _ => return None
                                                                                                                                                   };
                                                                                                                                                   Some(((Literals{lit: (*lit).clone(), lit_type: (*lit_type).clone()})).into_ddvalue())
                                                                                                                                               }
                                                                                                                                               __f},
                                                                                                                                               next: Box::new(None)
                                                                                                                                           })
                                                                                                                        })
                                                                                                        });
pub static __Rule_TypedExpressions_0 : ::once_cell::sync::Lazy<program::Rule> = ::once_cell::sync::Lazy::new(|| /* TypedExpressions[(TypedExpressions{.expr_id=expr_id, .type_id=type_id}: TypedExpressions)] :- Expressions[(Expressions{.expr_id=(expr_id: bit<64>), .expr=(hir::Expr{.kind=(hir::ExprLit{.lit=(lit: internment::Intern<hir::Literal>)}: hir::ExprKind)}: hir::Expr)}: Expressions)], ((var ty: hir::Type) = (hir::type_of(lit))), Types[(Types{.type_id=(type_id: bit<64>), .ty=(ty: hir::Type)}: Types)]. */
                                                                                                                program::Rule::CollectionRule {
                                                                                                                    description: std::borrow::Cow::from("TypedExpressions(.expr_id=expr_id, .type_id=type_id) :- Expressions(.expr_id=expr_id, .expr=hir::Expr{.kind=hir::ExprLit{.lit=lit}}), (var ty = (hir::type_of(lit))), Types(.type_id=type_id, .ty=ty)."),
                                                                                                                    rel: 1,
                                                                                                                    xform: Some(XFormCollection::Arrange{
                                                                                                                                    description: std::borrow::Cow::from("arrange Expressions(.expr_id=expr_id, .expr=hir::Expr{.kind=hir::ExprLit{.lit=lit}}) by (ty)"),
                                                                                                                                    afun: {fn __f(__v: DDValue) -> Option<(DDValue,DDValue)>
                                                                                                                                    {
                                                                                                                                        let (ref expr_id, ref lit) = match *<Expressions>::from_ddvalue_ref(&__v) {
                                                                                                                                            Expressions{expr_id: ref expr_id, expr: types__hir::Expr{kind: types__hir::ExprKind::ExprLit{lit: ref lit}}} => ((*expr_id).clone(), (*lit).clone()),
                                                                                                                                            _ => return None
                                                                                                                                        };
                                                                                                                                        let ref ty: types__hir::Type = match types__hir::type_of(lit) {
                                                                                                                                            ty => ty,
                                                                                                                                            _ => return None
                                                                                                                                        };
                                                                                                                                        Some((((*ty).clone()).into_ddvalue(), ((*expr_id).clone()).into_ddvalue()))
                                                                                                                                    }
                                                                                                                                    __f},
                                                                                                                                    next: Box::new(XFormArrangement::Join{
                                                                                                                                                       description: std::borrow::Cow::from("Expressions(.expr_id=expr_id, .expr=hir::Expr{.kind=hir::ExprLit{.lit=lit}}), (var ty = (hir::type_of(lit))), Types(.type_id=type_id, .ty=ty)"),
                                                                                                                                                       ffun: None,
                                                                                                                                                       arrangement: (7,0),
                                                                                                                                                       jfun: {fn __f(_: &DDValue, __v1: &DDValue, __v2: &DDValue) -> Option<DDValue>
                                                                                                                                                       {
                                                                                                                                                           let ref expr_id = *<u64>::from_ddvalue_ref( __v1 );
                                                                                                                                                           let ref type_id = match *<Types>::from_ddvalue_ref(__v2) {
                                                                                                                                                               Types{type_id: ref type_id, ty: _} => (*type_id).clone(),
                                                                                                                                                               _ => return None
                                                                                                                                                           };
                                                                                                                                                           Some(((TypedExpressions{expr_id: (*expr_id).clone(), type_id: (*type_id).clone()})).into_ddvalue())
                                                                                                                                                       }
                                                                                                                                                       __f},
                                                                                                                                                       next: Box::new(None)
                                                                                                                                                   })
                                                                                                                                })
                                                                                                                });
pub static __Rule_TypedExpressions_1 : ::once_cell::sync::Lazy<program::Rule> = ::once_cell::sync::Lazy::new(|| /* TypedExpressions[(TypedExpressions{.expr_id=expr_id, .type_id=type_id}: TypedExpressions)] :- Expressions[(Expressions{.expr_id=(expr_id: bit<64>), .expr=(hir::Expr{.kind=(hir::ExprVar{.variable=(var_id: bit<64>)}: hir::ExprKind)}: hir::Expr)}: Expressions)], Variables[(Variables{.var_id=(var_id: bit<64>), .decl=(hir::VariableDecl{.var_name=(_: bit<32>), .var_type=(type_id: bit<64>), .value=(_: bit<64>), .scope=(_: bit<32>)}: hir::VariableDecl)}: Variables)]. */
                                                                                                                program::Rule::ArrangementRule {
                                                                                                                    description: std::borrow::Cow::from( "TypedExpressions(.expr_id=expr_id, .type_id=type_id) :- Expressions(.expr_id=expr_id, .expr=hir::Expr{.kind=hir::ExprVar{.variable=var_id}}), Variables(.var_id=var_id, .decl=hir::VariableDecl{.var_name=_, .var_type=type_id, .value=_, .scope=_})."),
                                                                                                                    arr: ( 1, 0),
                                                                                                                    xform: XFormArrangement::Join{
                                                                                                                               description: std::borrow::Cow::from("Expressions(.expr_id=expr_id, .expr=hir::Expr{.kind=hir::ExprVar{.variable=var_id}}), Variables(.var_id=var_id, .decl=hir::VariableDecl{.var_name=_, .var_type=type_id, .value=_, .scope=_})"),
                                                                                                                               ffun: None,
                                                                                                                               arrangement: (10,0),
                                                                                                                               jfun: {fn __f(_: &DDValue, __v1: &DDValue, __v2: &DDValue) -> Option<DDValue>
                                                                                                                               {
                                                                                                                                   let (ref expr_id, ref var_id) = match *<Expressions>::from_ddvalue_ref(__v1) {
                                                                                                                                       Expressions{expr_id: ref expr_id, expr: types__hir::Expr{kind: types__hir::ExprKind::ExprVar{variable: ref var_id}}} => ((*expr_id).clone(), (*var_id).clone()),
                                                                                                                                       _ => return None
                                                                                                                                   };
                                                                                                                                   let ref type_id = match *<Variables>::from_ddvalue_ref(__v2) {
                                                                                                                                       Variables{var_id: _, decl: types__hir::VariableDecl{var_name: _, var_type: ref type_id, value: _, scope: _}} => (*type_id).clone(),
                                                                                                                                       _ => return None
                                                                                                                                   };
                                                                                                                                   Some(((TypedExpressions{expr_id: (*expr_id).clone(), type_id: (*type_id).clone()})).into_ddvalue())
                                                                                                                               }
                                                                                                                               __f},
                                                                                                                               next: Box::new(None)
                                                                                                                           }
                                                                                                                });
pub static __Rule_TypedExpressions_2 : ::once_cell::sync::Lazy<program::Rule> = ::once_cell::sync::Lazy::new(|| /* TypedExpressions[(TypedExpressions{.expr_id=assign_id, .type_id=type_id}: TypedExpressions)] :- __MultiHead_5[((type_id: bit<64>), (assign_id: bit<64>), (rhs_id: bit<64>))]. */
                                                                                                                program::Rule::CollectionRule {
                                                                                                                    description: std::borrow::Cow::from("TypedExpressions(.expr_id=assign_id, .type_id=type_id) :- __MultiHead_5[(type_id, assign_id, rhs_id)]."),
                                                                                                                    rel: 12,
                                                                                                                    xform: Some(XFormCollection::FilterMap{
                                                                                                                                    description: std::borrow::Cow::from("head of TypedExpressions(.expr_id=assign_id, .type_id=type_id) :- __MultiHead_5[(type_id, assign_id, rhs_id)]."),
                                                                                                                                    fmfun: {fn __f(__v: DDValue) -> Option<DDValue>
                                                                                                                                    {
                                                                                                                                        let (ref type_id, ref assign_id, ref rhs_id) = match *<ddlog_std::tuple3<u64, u64, u64>>::from_ddvalue_ref(&__v) {
                                                                                                                                            ddlog_std::tuple3(ref type_id, ref assign_id, ref rhs_id) => ((*type_id).clone(), (*assign_id).clone(), (*rhs_id).clone()),
                                                                                                                                            _ => return None
                                                                                                                                        };
                                                                                                                                        Some(((TypedExpressions{expr_id: (*assign_id).clone(), type_id: (*type_id).clone()})).into_ddvalue())
                                                                                                                                    }
                                                                                                                                    __f},
                                                                                                                                    next: Box::new(None)
                                                                                                                                })
                                                                                                                });
pub static __Rule_TypedExpressions_3 : ::once_cell::sync::Lazy<program::Rule> = ::once_cell::sync::Lazy::new(|| /* TypedExpressions[(TypedExpressions{.expr_id=rhs_id, .type_id=type_id}: TypedExpressions)] :- __MultiHead_5[((type_id: bit<64>), (assign_id: bit<64>), (rhs_id: bit<64>))]. */
                                                                                                                program::Rule::CollectionRule {
                                                                                                                    description: std::borrow::Cow::from("TypedExpressions(.expr_id=rhs_id, .type_id=type_id) :- __MultiHead_5[(type_id, assign_id, rhs_id)]."),
                                                                                                                    rel: 12,
                                                                                                                    xform: Some(XFormCollection::FilterMap{
                                                                                                                                    description: std::borrow::Cow::from("head of TypedExpressions(.expr_id=rhs_id, .type_id=type_id) :- __MultiHead_5[(type_id, assign_id, rhs_id)]."),
                                                                                                                                    fmfun: {fn __f(__v: DDValue) -> Option<DDValue>
                                                                                                                                    {
                                                                                                                                        let (ref type_id, ref assign_id, ref rhs_id) = match *<ddlog_std::tuple3<u64, u64, u64>>::from_ddvalue_ref(&__v) {
                                                                                                                                            ddlog_std::tuple3(ref type_id, ref assign_id, ref rhs_id) => ((*type_id).clone(), (*assign_id).clone(), (*rhs_id).clone()),
                                                                                                                                            _ => return None
                                                                                                                                        };
                                                                                                                                        Some(((TypedExpressions{expr_id: (*rhs_id).clone(), type_id: (*type_id).clone()})).into_ddvalue())
                                                                                                                                    }
                                                                                                                                    __f},
                                                                                                                                    next: Box::new(None)
                                                                                                                                })
                                                                                                                });
pub static __Rule___MultiHead_5_0 : ::once_cell::sync::Lazy<program::Rule> = ::once_cell::sync::Lazy::new(|| /* __MultiHead_5[((type_id: bit<64>), (assign_id: bit<64>), (rhs_id: bit<64>))] :- Expressions[(Expressions{.expr_id=(assign_id: bit<64>), .expr=(hir::Expr{.kind=(hir::ExprAssign{.variable=(var_id: bit<64>), .expr_id=(rhs_id: bit<64>)}: hir::ExprKind)}: hir::Expr)}: Expressions)], Variables[(Variables{.var_id=(var_id: bit<64>), .decl=(hir::VariableDecl{.var_name=(_: bit<32>), .var_type=(_: bit<64>), .value=(_: bit<64>), .scope=(_: bit<32>)}: hir::VariableDecl)}: Variables)], TypedExpressions[(TypedExpressions{.expr_id=(rhs_id: bit<64>), .type_id=(type_id: bit<64>)}: TypedExpressions)]. */
                                                                                                             program::Rule::ArrangementRule {
                                                                                                                 description: std::borrow::Cow::from( "__MultiHead_5[(type_id, assign_id, rhs_id)] :- Expressions(.expr_id=assign_id, .expr=hir::Expr{.kind=hir::ExprAssign{.variable=var_id, .expr_id=rhs_id}}), Variables(.var_id=var_id, .decl=hir::VariableDecl{.var_name=_, .var_type=_, .value=_, .scope=_}), TypedExpressions(.expr_id=rhs_id, .type_id=type_id)."),
                                                                                                                 arr: ( 1, 1),
                                                                                                                 xform: XFormArrangement::Join{
                                                                                                                            description: std::borrow::Cow::from("Expressions(.expr_id=assign_id, .expr=hir::Expr{.kind=hir::ExprAssign{.variable=var_id, .expr_id=rhs_id}}), Variables(.var_id=var_id, .decl=hir::VariableDecl{.var_name=_, .var_type=_, .value=_, .scope=_})"),
                                                                                                                            ffun: None,
                                                                                                                            arrangement: (10,0),
                                                                                                                            jfun: {fn __f(_: &DDValue, __v1: &DDValue, __v2: &DDValue) -> Option<DDValue>
                                                                                                                            {
                                                                                                                                let (ref assign_id, ref var_id, ref rhs_id) = match *<Expressions>::from_ddvalue_ref(__v1) {
                                                                                                                                    Expressions{expr_id: ref assign_id, expr: types__hir::Expr{kind: types__hir::ExprKind::ExprAssign{variable: ref var_id, expr_id: ref rhs_id}}} => ((*assign_id).clone(), (*var_id).clone(), (*rhs_id).clone()),
                                                                                                                                    _ => return None
                                                                                                                                };
                                                                                                                                let () = match *<Variables>::from_ddvalue_ref(__v2) {
                                                                                                                                    Variables{var_id: _, decl: types__hir::VariableDecl{var_name: _, var_type: _, value: _, scope: _}} => (),
                                                                                                                                    _ => return None
                                                                                                                                };
                                                                                                                                Some((ddlog_std::tuple2((*assign_id).clone(), (*rhs_id).clone())).into_ddvalue())
                                                                                                                            }
                                                                                                                            __f},
                                                                                                                            next: Box::new(Some(XFormCollection::Arrange{
                                                                                                                                                    description: std::borrow::Cow::from("arrange Expressions(.expr_id=assign_id, .expr=hir::Expr{.kind=hir::ExprAssign{.variable=var_id, .expr_id=rhs_id}}), Variables(.var_id=var_id, .decl=hir::VariableDecl{.var_name=_, .var_type=_, .value=_, .scope=_}) by (rhs_id)"),
                                                                                                                                                    afun: {fn __f(__v: DDValue) -> Option<(DDValue,DDValue)>
                                                                                                                                                    {
                                                                                                                                                        let ddlog_std::tuple2(ref assign_id, ref rhs_id) = *<ddlog_std::tuple2<u64, u64>>::from_ddvalue_ref( &__v );
                                                                                                                                                        Some((((*rhs_id).clone()).into_ddvalue(), (ddlog_std::tuple2((*assign_id).clone(), (*rhs_id).clone())).into_ddvalue()))
                                                                                                                                                    }
                                                                                                                                                    __f},
                                                                                                                                                    next: Box::new(XFormArrangement::Join{
                                                                                                                                                                       description: std::borrow::Cow::from("Expressions(.expr_id=assign_id, .expr=hir::Expr{.kind=hir::ExprAssign{.variable=var_id, .expr_id=rhs_id}}), Variables(.var_id=var_id, .decl=hir::VariableDecl{.var_name=_, .var_type=_, .value=_, .scope=_}), TypedExpressions(.expr_id=rhs_id, .type_id=type_id)"),
                                                                                                                                                                       ffun: None,
                                                                                                                                                                       arrangement: (6,1),
                                                                                                                                                                       jfun: {fn __f(_: &DDValue, __v1: &DDValue, __v2: &DDValue) -> Option<DDValue>
                                                                                                                                                                       {
                                                                                                                                                                           let ddlog_std::tuple2(ref assign_id, ref rhs_id) = *<ddlog_std::tuple2<u64, u64>>::from_ddvalue_ref( __v1 );
                                                                                                                                                                           let ref type_id = match *<TypedExpressions>::from_ddvalue_ref(__v2) {
                                                                                                                                                                               TypedExpressions{expr_id: _, type_id: ref type_id} => (*type_id).clone(),
                                                                                                                                                                               _ => return None
                                                                                                                                                                           };
                                                                                                                                                                           Some((ddlog_std::tuple3((*type_id).clone(), (*assign_id).clone(), (*rhs_id).clone())).into_ddvalue())
                                                                                                                                                                       }
                                                                                                                                                                       __f},
                                                                                                                                                                       next: Box::new(None)
                                                                                                                                                                   })
                                                                                                                                                }))
                                                                                                                        }
                                                                                                             });
pub static __Rule_UnifiedTypes_0 : ::once_cell::sync::Lazy<program::Rule> = ::once_cell::sync::Lazy::new(|| /* UnifiedTypes[(UnifiedTypes{.expr_id=expr_id, .type_id=unified_type}: UnifiedTypes)] :- TypedExpressions[(TypedExpressions{.expr_id=(expr_id: bit<64>), .type_id=(type_id: bit<64>)}: TypedExpressions)], Types[(Types{.type_id=(type_id: bit<64>), .ty=(ty: hir::Type)}: Types)], var __group = (type_id, ty).group_by(expr_id), ((var unified_type: bit<64>) = ((unify_types: function(ddlog_std::Group<bit<64>,(bit<64>, hir::Type)>):bit<64>)(__group))). */
                                                                                                            program::Rule::ArrangementRule {
                                                                                                                description: std::borrow::Cow::from( "UnifiedTypes(.expr_id=expr_id, .type_id=unified_type) :- TypedExpressions(.expr_id=expr_id, .type_id=type_id), Types(.type_id=type_id, .ty=ty), var __group = (type_id, ty).group_by(expr_id), (var unified_type = (unify_types(__group)))."),
                                                                                                                arr: ( 6, 0),
                                                                                                                xform: XFormArrangement::Join{
                                                                                                                           description: std::borrow::Cow::from("TypedExpressions(.expr_id=expr_id, .type_id=type_id), Types(.type_id=type_id, .ty=ty)"),
                                                                                                                           ffun: None,
                                                                                                                           arrangement: (7,1),
                                                                                                                           jfun: {fn __f(_: &DDValue, __v1: &DDValue, __v2: &DDValue) -> Option<DDValue>
                                                                                                                           {
                                                                                                                               let (ref expr_id, ref type_id) = match *<TypedExpressions>::from_ddvalue_ref(__v1) {
                                                                                                                                   TypedExpressions{expr_id: ref expr_id, type_id: ref type_id} => ((*expr_id).clone(), (*type_id).clone()),
                                                                                                                                   _ => return None
                                                                                                                               };
                                                                                                                               let ref ty = match *<Types>::from_ddvalue_ref(__v2) {
                                                                                                                                   Types{type_id: _, ty: ref ty} => (*ty).clone(),
                                                                                                                                   _ => return None
                                                                                                                               };
                                                                                                                               Some((ddlog_std::tuple3((*ty).clone(), (*expr_id).clone(), (*type_id).clone())).into_ddvalue())
                                                                                                                           }
                                                                                                                           __f},
                                                                                                                           next: Box::new(Some(XFormCollection::Arrange{
                                                                                                                                                   description: std::borrow::Cow::from("arrange TypedExpressions(.expr_id=expr_id, .type_id=type_id), Types(.type_id=type_id, .ty=ty) by (expr_id)"),
                                                                                                                                                   afun: {fn __f(__v: DDValue) -> Option<(DDValue,DDValue)>
                                                                                                                                                   {
                                                                                                                                                       let ddlog_std::tuple3(ref ty, ref expr_id, ref type_id) = *<ddlog_std::tuple3<types__hir::Type, u64, u64>>::from_ddvalue_ref( &__v );
                                                                                                                                                       Some((((*expr_id).clone()).into_ddvalue(), (ddlog_std::tuple3((*ty).clone(), (*expr_id).clone(), (*type_id).clone())).into_ddvalue()))
                                                                                                                                                   }
                                                                                                                                                   __f},
                                                                                                                                                   next: Box::new(XFormArrangement::Aggregate{
                                                                                                                                                                      description: std::borrow::Cow::from("TypedExpressions(.expr_id=expr_id, .type_id=type_id), Types(.type_id=type_id, .ty=ty), var __group = (type_id, ty).group_by(expr_id)"),
                                                                                                                                                                      ffun: None,
                                                                                                                                                                      aggfun: {fn __f(__key: &DDValue, __group__: &[(&DDValue, Weight)]) -> Option<DDValue>
                                                                                                                                                                  {
                                                                                                                                                                      let ref expr_id = *<u64>::from_ddvalue_ref( __key );
                                                                                                                                                                      let ref __group = unsafe{ddlog_std::Group::new_by_ref((*expr_id).clone(), __group__, {fn __f(__v: &DDValue) ->  ddlog_std::tuple2<u64, types__hir::Type>
                                                                                                                                                                                                                                                           {
                                                                                                                                                                                                                                                               let ddlog_std::tuple3(ref ty, ref expr_id, ref type_id) = *<ddlog_std::tuple3<types__hir::Type, u64, u64>>::from_ddvalue_ref( __v );
                                                                                                                                                                                                                                                               ddlog_std::tuple2((*type_id).clone(), (*ty).clone())
                                                                                                                                                                                                                                                           }
                                                                                                                                                                                                                                                           ::std::sync::Arc::new(__f)})};
                                                                                                                                                                      let ref unified_type: u64 = match unify_types::<u64>(__group) {
                                                                                                                                                                          unified_type => unified_type,
                                                                                                                                                                          _ => return None
                                                                                                                                                                      };
                                                                                                                                                                      Some((ddlog_std::tuple2((*unified_type).clone(), (*expr_id).clone())).into_ddvalue())
                                                                                                                                                                  }
                                                                                                                                                                  __f},
                                                                                                                                                                      next: Box::new(Some(XFormCollection::FilterMap{
                                                                                                                                                                                              description: std::borrow::Cow::from("head of UnifiedTypes(.expr_id=expr_id, .type_id=unified_type) :- TypedExpressions(.expr_id=expr_id, .type_id=type_id), Types(.type_id=type_id, .ty=ty), var __group = (type_id, ty).group_by(expr_id), (var unified_type = (unify_types(__group)))."),
                                                                                                                                                                                              fmfun: {fn __f(__v: DDValue) -> Option<DDValue>
                                                                                                                                                                                              {
                                                                                                                                                                                                  let ddlog_std::tuple2(ref unified_type, ref expr_id) = *<ddlog_std::tuple2<u64, u64>>::from_ddvalue_ref( &__v );
                                                                                                                                                                                                  Some(((UnifiedTypes{expr_id: (*expr_id).clone(), type_id: (*unified_type).clone()})).into_ddvalue())
                                                                                                                                                                                              }
                                                                                                                                                                                              __f},
                                                                                                                                                                                              next: Box::new(None)
                                                                                                                                                                                          }))
                                                                                                                                                                  })
                                                                                                                                               }))
                                                                                                                       }
                                                                                                            });
pub static __Rule_VariablesInScope_0 : ::once_cell::sync::Lazy<program::Rule> = ::once_cell::sync::Lazy::new(|| /* VariablesInScope[(VariablesInScope{.scope=scope, .var_id=var_id, .decl_scope=scope}: VariablesInScope)] :- Variables[(Variables{.var_id=(var_id: bit<64>), .decl=(hir::VariableDecl{.var_name=(_: bit<32>), .var_type=(_: bit<64>), .value=(_: bit<64>), .scope=(scope: bit<32>)}: hir::VariableDecl)}: Variables)]. */
                                                                                                                program::Rule::CollectionRule {
                                                                                                                    description: std::borrow::Cow::from("VariablesInScope(.scope=scope, .var_id=var_id, .decl_scope=scope) :- Variables(.var_id=var_id, .decl=hir::VariableDecl{.var_name=_, .var_type=_, .value=_, .scope=scope})."),
                                                                                                                    rel: 10,
                                                                                                                    xform: Some(XFormCollection::FilterMap{
                                                                                                                                    description: std::borrow::Cow::from("head of VariablesInScope(.scope=scope, .var_id=var_id, .decl_scope=scope) :- Variables(.var_id=var_id, .decl=hir::VariableDecl{.var_name=_, .var_type=_, .value=_, .scope=scope})."),
                                                                                                                                    fmfun: {fn __f(__v: DDValue) -> Option<DDValue>
                                                                                                                                    {
                                                                                                                                        let (ref var_id, ref scope) = match *<Variables>::from_ddvalue_ref(&__v) {
                                                                                                                                            Variables{var_id: ref var_id, decl: types__hir::VariableDecl{var_name: _, var_type: _, value: _, scope: ref scope}} => ((*var_id).clone(), (*scope).clone()),
                                                                                                                                            _ => return None
                                                                                                                                        };
                                                                                                                                        Some(((VariablesInScope{scope: (*scope).clone(), var_id: (*var_id).clone(), decl_scope: (*scope).clone()})).into_ddvalue())
                                                                                                                                    }
                                                                                                                                    __f},
                                                                                                                                    next: Box::new(None)
                                                                                                                                })
                                                                                                                });
pub static __Rule_VariablesInScope_1 : ::once_cell::sync::Lazy<program::Rule> = ::once_cell::sync::Lazy::new(|| /* VariablesInScope[(VariablesInScope{.scope=child, .var_id=var_id, .decl_scope=decl_scope}: VariablesInScope)] :- VariablesInScope[(VariablesInScope{.scope=(parent: bit<32>), .var_id=(var_id: bit<64>), .decl_scope=(decl_scope: bit<32>)}: VariablesInScope)], VariableScopes[(VariableScopes{.parent=(parent: bit<32>), .child=(child: bit<32>)}: VariableScopes)]. */
                                                                                                                program::Rule::ArrangementRule {
                                                                                                                    description: std::borrow::Cow::from( "VariablesInScope(.scope=child, .var_id=var_id, .decl_scope=decl_scope) :- VariablesInScope(.scope=parent, .var_id=var_id, .decl_scope=decl_scope), VariableScopes(.parent=parent, .child=child)."),
                                                                                                                    arr: ( 11, 0),
                                                                                                                    xform: XFormArrangement::Join{
                                                                                                                               description: std::borrow::Cow::from("VariablesInScope(.scope=parent, .var_id=var_id, .decl_scope=decl_scope), VariableScopes(.parent=parent, .child=child)"),
                                                                                                                               ffun: None,
                                                                                                                               arrangement: (9,0),
                                                                                                                               jfun: {fn __f(_: &DDValue, __v1: &DDValue, __v2: &DDValue) -> Option<DDValue>
                                                                                                                               {
                                                                                                                                   let (ref parent, ref var_id, ref decl_scope) = match *<VariablesInScope>::from_ddvalue_ref(__v1) {
                                                                                                                                       VariablesInScope{scope: ref parent, var_id: ref var_id, decl_scope: ref decl_scope} => ((*parent).clone(), (*var_id).clone(), (*decl_scope).clone()),
                                                                                                                                       _ => return None
                                                                                                                                   };
                                                                                                                                   let ref child = match *<VariableScopes>::from_ddvalue_ref(__v2) {
                                                                                                                                       VariableScopes{parent: _, child: ref child} => (*child).clone(),
                                                                                                                                       _ => return None
                                                                                                                                   };
                                                                                                                                   Some(((VariablesInScope{scope: (*child).clone(), var_id: (*var_id).clone(), decl_scope: (*decl_scope).clone()})).into_ddvalue())
                                                                                                                               }
                                                                                                                               __f},
                                                                                                                               next: Box::new(None)
                                                                                                                           }
                                                                                                                });