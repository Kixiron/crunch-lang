#![allow(
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
    clippy::unknown_clippy_lints,
    clippy::missing_safety_doc,
    clippy::toplevel_ref_arg
)]

use num::bigint::BigInt;
use std::convert::TryFrom;
use std::ops::Deref;
use std::ptr;
use std::result;
use std::sync;

use ordered_float::*;

use differential_dataflow::collection;
use timely::communication;
use timely::dataflow::scopes;
use timely::worker;

use differential_datalog::ddval::*;
use differential_datalog::int::*;
use differential_datalog::program::*;
use differential_datalog::record;
use differential_datalog::record::IntoRecord;
use differential_datalog::record::UpdCmd;
use differential_datalog::uint::*;
use differential_datalog::DDlogConvert;
use num_traits::cast::FromPrimitive;
use num_traits::identities::One;

use fnv::FnvHashMap;

use types::*;
pub use value::*;

pub mod api;
pub mod ovsdb;
pub mod update_handler;

use crate::api::updcmd2upd;

/// A default implementation of `DDlogConvert` that just forwards calls
/// to generated functions of equal name.
#[derive(Debug)]
pub struct DDlogConverter {}

impl DDlogConvert for DDlogConverter {
    fn relid2name(relId: RelId) -> Option<&'static str> {
        relid2name(relId)
    }

    fn indexid2name(idxId: IdxId) -> Option<&'static str> {
        indexid2name(idxId)
    }

    fn updcmd2upd(upd_cmd: &UpdCmd) -> result::Result<Update<DDValue>, String> {
        updcmd2upd(upd_cmd)
    }
}

pub fn prog(__update_cb: Box<dyn CBFn>) -> Program {
    let Errors = Relation {
        name: "Errors".to_string(),
        input: false,
        distinct: true,
        caching_mode: CachingMode::Set,
        key_func: None,
        id: Relations::Errors as RelId,
        rules: vec![],
        arrangements: vec![],
        change_cb: Some(sync::Arc::new(sync::Mutex::new(__update_cb.clone()))),
    };
    let Expressions = Relation {
                          name:         "Expressions".to_string(),
                          input:        true,
                          distinct:     false,
                          caching_mode: CachingMode::Set,
                          key_func:     None,
                          id:           Relations::Expressions as RelId,
                          rules:        vec![
                              ],
                          arrangements: vec![
                              Arrangement::Map{
                                 name: r###"(Expressions{.expr_id=(_: bit<64>), .expr=(hir::Expr{.kind=(hir::ExprVar{.variable=(_0: bit<64>)}: hir::ExprKind)}: hir::Expr)}: Expressions) /*join*/"###.to_string(),
                                  afun: &{fn __f(__v: DDValue) -> Option<(DDValue,DDValue)>
                                  {
                                      let __cloned = __v.clone();
                                      match unsafe { Value::Expressions::from_ddvalue(__v) }.0 {
                                          Expressions{expr_id: _, expr: hir_Expr{kind: hir_ExprKind::hir_ExprVar{variable: ref _0}}} => Some(Value::__Bitval64((*_0).clone()).into_ddvalue()),
                                          _ => None
                                      }.map(|x|(x,__cloned))
                                  }
                                  __f},
                                  queryable: false
                              },
                              Arrangement::Map{
                                 name: r###"(Expressions{.expr_id=(_: bit<64>), .expr=(hir::Expr{.kind=(hir::ExprAssign{.variable=(_0: bit<64>), .expr_id=(_: bit<64>)}: hir::ExprKind)}: hir::Expr)}: Expressions) /*join*/"###.to_string(),
                                  afun: &{fn __f(__v: DDValue) -> Option<(DDValue,DDValue)>
                                  {
                                      let __cloned = __v.clone();
                                      match unsafe { Value::Expressions::from_ddvalue(__v) }.0 {
                                          Expressions{expr_id: _, expr: hir_Expr{kind: hir_ExprKind::hir_ExprAssign{variable: ref _0, expr_id: _}}} => Some(Value::__Bitval64((*_0).clone()).into_ddvalue()),
                                          _ => None
                                      }.map(|x|(x,__cloned))
                                  }
                                  __f},
                                  queryable: false
                              }],
                          change_cb:    None
                      };
    let INPUT_Expressions = Relation {
        name: "INPUT_Expressions".to_string(),
        input: false,
        distinct: false,
        caching_mode: CachingMode::Set,
        key_func: None,
        id: Relations::INPUT_Expressions as RelId,
        rules: vec![
            /* INPUT_Expressions[x] :- Expressions[(x: Expressions)]. */
            Rule::CollectionRule {
                description: "INPUT_Expressions[x] :- Expressions[(x: Expressions)].".to_string(),
                rel: Relations::Expressions as RelId,
                xform: Some(XFormCollection::FilterMap {
                    description: "head of INPUT_Expressions[x] :- Expressions[(x: Expressions)]."
                        .to_string(),
                    fmfun: &{
                        fn __f(__v: DDValue) -> Option<DDValue> {
                            let ref x =
                                match unsafe { Value::Expressions::from_ddvalue_ref(&__v) }.0 {
                                    ref x => (*x).clone(),
                                    _ => return None,
                                };
                            Some(Value::Expressions((*x).clone()).into_ddvalue())
                        }
                        __f
                    },
                    next: Box::new(None),
                }),
            },
        ],
        arrangements: vec![],
        change_cb: Some(sync::Arc::new(sync::Mutex::new(__update_cb.clone()))),
    };
    let Functions = Relation {
        name: "Functions".to_string(),
        input: true,
        distinct: false,
        caching_mode: CachingMode::Set,
        key_func: None,
        id: Relations::Functions as RelId,
        rules: vec![],
        arrangements: vec![],
        change_cb: None,
    };
    let INPUT_Functions = Relation {
        name: "INPUT_Functions".to_string(),
        input: false,
        distinct: false,
        caching_mode: CachingMode::Set,
        key_func: None,
        id: Relations::INPUT_Functions as RelId,
        rules: vec![
            /* INPUT_Functions[x] :- Functions[(x: Functions)]. */
            Rule::CollectionRule {
                description: "INPUT_Functions[x] :- Functions[(x: Functions)].".to_string(),
                rel: Relations::Functions as RelId,
                xform: Some(XFormCollection::FilterMap {
                    description: "head of INPUT_Functions[x] :- Functions[(x: Functions)]."
                        .to_string(),
                    fmfun: &{
                        fn __f(__v: DDValue) -> Option<DDValue> {
                            let ref x = match unsafe { Value::Functions::from_ddvalue_ref(&__v) }.0
                            {
                                ref x => (*x).clone(),
                                _ => return None,
                            };
                            Some(Value::Functions((*x).clone()).into_ddvalue())
                        }
                        __f
                    },
                    next: Box::new(None),
                }),
            },
        ],
        arrangements: vec![],
        change_cb: Some(sync::Arc::new(sync::Mutex::new(__update_cb.clone()))),
    };
    let Items = Relation {
        name: "Items".to_string(),
        input: true,
        distinct: false,
        caching_mode: CachingMode::Set,
        key_func: None,
        id: Relations::Items as RelId,
        rules: vec![],
        arrangements: vec![],
        change_cb: None,
    };
    let INPUT_Items = Relation {
        name: "INPUT_Items".to_string(),
        input: false,
        distinct: false,
        caching_mode: CachingMode::Set,
        key_func: None,
        id: Relations::INPUT_Items as RelId,
        rules: vec![
            /* INPUT_Items[x] :- Items[(x: Items)]. */
            Rule::CollectionRule {
                description: "INPUT_Items[x] :- Items[(x: Items)].".to_string(),
                rel: Relations::Items as RelId,
                xform: Some(XFormCollection::FilterMap {
                    description: "head of INPUT_Items[x] :- Items[(x: Items)].".to_string(),
                    fmfun: &{
                        fn __f(__v: DDValue) -> Option<DDValue> {
                            let ref x = match unsafe { Value::Items::from_ddvalue_ref(&__v) }.0 {
                                ref x => (*x).clone(),
                                _ => return None,
                            };
                            Some(Value::Items((*x).clone()).into_ddvalue())
                        }
                        __f
                    },
                    next: Box::new(None),
                }),
            },
        ],
        arrangements: vec![],
        change_cb: Some(sync::Arc::new(sync::Mutex::new(__update_cb.clone()))),
    };
    let Statements = Relation {
        name: "Statements".to_string(),
        input: true,
        distinct: false,
        caching_mode: CachingMode::Set,
        key_func: None,
        id: Relations::Statements as RelId,
        rules: vec![],
        arrangements: vec![],
        change_cb: None,
    };
    let INPUT_Statements = Relation {
        name: "INPUT_Statements".to_string(),
        input: false,
        distinct: false,
        caching_mode: CachingMode::Set,
        key_func: None,
        id: Relations::INPUT_Statements as RelId,
        rules: vec![
            /* INPUT_Statements[x] :- Statements[(x: Statements)]. */
            Rule::CollectionRule {
                description: "INPUT_Statements[x] :- Statements[(x: Statements)].".to_string(),
                rel: Relations::Statements as RelId,
                xform: Some(XFormCollection::FilterMap {
                    description: "head of INPUT_Statements[x] :- Statements[(x: Statements)]."
                        .to_string(),
                    fmfun: &{
                        fn __f(__v: DDValue) -> Option<DDValue> {
                            let ref x = match unsafe { Value::Statements::from_ddvalue_ref(&__v) }.0
                            {
                                ref x => (*x).clone(),
                                _ => return None,
                            };
                            Some(Value::Statements((*x).clone()).into_ddvalue())
                        }
                        __f
                    },
                    next: Box::new(None),
                }),
            },
        ],
        arrangements: vec![],
        change_cb: Some(sync::Arc::new(sync::Mutex::new(__update_cb.clone()))),
    };
    let Types = Relation {
        name: "Types".to_string(),
        input: true,
        distinct: false,
        caching_mode: CachingMode::Set,
        key_func: None,
        id: Relations::Types as RelId,
        rules: vec![],
        arrangements: vec![
            Arrangement::Map {
                name: r###"(Types{.type_id=(_: bit<64>), .ty=(_0: hir::Type)}: Types) /*join*/"###
                    .to_string(),
                afun: &{
                    fn __f(__v: DDValue) -> Option<(DDValue, DDValue)> {
                        let __cloned = __v.clone();
                        match unsafe { Value::Types::from_ddvalue(__v) }.0 {
                            Types {
                                type_id: _,
                                ty: ref _0,
                            } => Some(Value::hir_Type((*_0).clone()).into_ddvalue()),
                            _ => None,
                        }
                        .map(|x| (x, __cloned))
                    }
                    __f
                },
                queryable: false,
            },
            Arrangement::Map {
                name: r###"(Types{.type_id=(_0: bit<64>), .ty=(_: hir::Type)}: Types) /*join*/"###
                    .to_string(),
                afun: &{
                    fn __f(__v: DDValue) -> Option<(DDValue, DDValue)> {
                        let __cloned = __v.clone();
                        match unsafe { Value::Types::from_ddvalue(__v) }.0 {
                            Types {
                                type_id: ref _0,
                                ty: _,
                            } => Some(Value::__Bitval64((*_0).clone()).into_ddvalue()),
                            _ => None,
                        }
                        .map(|x| (x, __cloned))
                    }
                    __f
                },
                queryable: false,
            },
        ],
        change_cb: None,
    };
    let Literals = Relation {
                       name:         "Literals".to_string(),
                       input:        false,
                       distinct:     true,
                       caching_mode: CachingMode::Set,
                       key_func:     None,
                       id:           Relations::Literals as RelId,
                       rules:        vec![
                           /* Literals[(Literals{.lit=lit, .lit_type=lit_type}: Literals)] :- Expressions[(Expressions{.expr_id=(_: bit<64>), .expr=(hir::Expr{.kind=(hir::ExprLit{.lit=(lit: internment::Intern<hir::Literal>)}: hir::ExprKind)}: hir::Expr)}: Expressions)], (var ty: hir::Type) = hir::typeof(lit), Types[(Types{.type_id=(lit_type: bit<64>), .ty=(ty: hir::Type)}: Types)]. */
                           Rule::CollectionRule {
                               description: "Literals[(Literals{.lit=lit, .lit_type=lit_type}: Literals)] :- Expressions[(Expressions{.expr_id=(_: bit<64>), .expr=(hir::Expr{.kind=(hir::ExprLit{.lit=(lit: internment::Intern<hir::Literal>)}: hir::ExprKind)}: hir::Expr)}: Expressions)], (var ty: hir::Type) = hir::typeof(lit), Types[(Types{.type_id=(lit_type: bit<64>), .ty=(ty: hir::Type)}: Types)].".to_string(),
                               rel: Relations::Expressions as RelId,
                               xform: Some(XFormCollection::Arrange {
                                               description: "arrange Expressions[(Expressions{.expr_id=(_: bit<64>), .expr=(hir::Expr{.kind=(hir::ExprLit{.lit=(lit: internment::Intern<hir::Literal>)}: hir::ExprKind)}: hir::Expr)}: Expressions)] by (ty)" .to_string(),
                                               afun: &{fn __f(__v: DDValue) -> Option<(DDValue,DDValue)>
                                               {
                                                   let ref lit = match unsafe {  Value::Expressions::from_ddvalue_ref(&__v) }.0 {
                                                       Expressions{expr_id: _, expr: hir_Expr{kind: hir_ExprKind::hir_ExprLit{lit: ref lit}}} => (*lit).clone(),
                                                       _ => return None
                                                   };
                                                   let ref ty: hir_Type = match hir_typeof(lit) {
                                                       ty => ty,
                                                       _ => return None
                                                   };
                                                   Some((Value::hir_Type((*ty).clone()).into_ddvalue(), Value::internment_Intern__hir_Literal((*lit).clone()).into_ddvalue()))
                                               }
                                               __f},
                                               next: Box::new(XFormArrangement::Join{
                                                                  description: "Expressions[(Expressions{.expr_id=(_: bit<64>), .expr=(hir::Expr{.kind=(hir::ExprLit{.lit=(lit: internment::Intern<hir::Literal>)}: hir::ExprKind)}: hir::Expr)}: Expressions)], (var ty: hir::Type) = hir::typeof(lit), Types[(Types{.type_id=(lit_type: bit<64>), .ty=(ty: hir::Type)}: Types)]".to_string(),
                                                                  ffun: None,
                                                                  arrangement: (Relations::Types as RelId,0),
                                                                  jfun: &{fn __f(_: &DDValue ,__v1: &DDValue,__v2: &DDValue) -> Option<DDValue>
                                                                  {
                                                                      let ref lit = unsafe { Value::internment_Intern__hir_Literal::from_ddvalue_ref( __v1 ) }.0;
                                                                      let ref lit_type = match unsafe {  Value::Types::from_ddvalue_ref(__v2) }.0 {
                                                                          Types{type_id: ref lit_type, ty: _} => (*lit_type).clone(),
                                                                          _ => return None
                                                                      };
                                                                      Some(Value::Literals((Literals{lit: (*lit).clone(), lit_type: (*lit_type).clone()})).into_ddvalue())
                                                                  }
                                                                  __f},
                                                                  next: Box::new(None)
                                                              })
                                           })
                           }],
                       arrangements: vec![
                           ],
                       change_cb:    Some(sync::Arc::new(sync::Mutex::new(__update_cb.clone())))
                   };
    let INPUT_Types = Relation {
        name: "INPUT_Types".to_string(),
        input: false,
        distinct: false,
        caching_mode: CachingMode::Set,
        key_func: None,
        id: Relations::INPUT_Types as RelId,
        rules: vec![
            /* INPUT_Types[x] :- Types[(x: Types)]. */
            Rule::CollectionRule {
                description: "INPUT_Types[x] :- Types[(x: Types)].".to_string(),
                rel: Relations::Types as RelId,
                xform: Some(XFormCollection::FilterMap {
                    description: "head of INPUT_Types[x] :- Types[(x: Types)].".to_string(),
                    fmfun: &{
                        fn __f(__v: DDValue) -> Option<DDValue> {
                            let ref x = match unsafe { Value::Types::from_ddvalue_ref(&__v) }.0 {
                                ref x => (*x).clone(),
                                _ => return None,
                            };
                            Some(Value::Types((*x).clone()).into_ddvalue())
                        }
                        __f
                    },
                    next: Box::new(None),
                }),
            },
        ],
        arrangements: vec![],
        change_cb: Some(sync::Arc::new(sync::Mutex::new(__update_cb.clone()))),
    };
    let VariableScopes = Relation {
                             name:         "VariableScopes".to_string(),
                             input:        true,
                             distinct:     false,
                             caching_mode: CachingMode::Set,
                             key_func:     None,
                             id:           Relations::VariableScopes as RelId,
                             rules:        vec![
                                 ],
                             arrangements: vec![
                                 Arrangement::Map{
                                    name: r###"(VariableScopes{.parent=(_0: bit<32>), .child=(_: bit<32>)}: VariableScopes) /*join*/"###.to_string(),
                                     afun: &{fn __f(__v: DDValue) -> Option<(DDValue,DDValue)>
                                     {
                                         let __cloned = __v.clone();
                                         match unsafe { Value::VariableScopes::from_ddvalue(__v) }.0 {
                                             VariableScopes{parent: ref _0, child: _} => Some(Value::__Bitval32((*_0).clone()).into_ddvalue()),
                                             _ => None
                                         }.map(|x|(x,__cloned))
                                     }
                                     __f},
                                     queryable: false
                                 }],
                             change_cb:    None
                         };
    let INPUT_VariableScopes = Relation {
        name: "INPUT_VariableScopes".to_string(),
        input: false,
        distinct: false,
        caching_mode: CachingMode::Set,
        key_func: None,
        id: Relations::INPUT_VariableScopes as RelId,
        rules: vec![
            /* INPUT_VariableScopes[x] :- VariableScopes[(x: VariableScopes)]. */
            Rule::CollectionRule {
                description: "INPUT_VariableScopes[x] :- VariableScopes[(x: VariableScopes)]."
                    .to_string(),
                rel: Relations::VariableScopes as RelId,
                xform: Some(XFormCollection::FilterMap {
                    description:
                        "head of INPUT_VariableScopes[x] :- VariableScopes[(x: VariableScopes)]."
                            .to_string(),
                    fmfun: &{
                        fn __f(__v: DDValue) -> Option<DDValue> {
                            let ref x =
                                match unsafe { Value::VariableScopes::from_ddvalue_ref(&__v) }.0 {
                                    ref x => (*x).clone(),
                                    _ => return None,
                                };
                            Some(Value::VariableScopes((*x).clone()).into_ddvalue())
                        }
                        __f
                    },
                    next: Box::new(None),
                }),
            },
        ],
        arrangements: vec![],
        change_cb: Some(sync::Arc::new(sync::Mutex::new(__update_cb.clone()))),
    };
    let Variables = Relation {
                        name:         "Variables".to_string(),
                        input:        true,
                        distinct:     false,
                        caching_mode: CachingMode::Set,
                        key_func:     None,
                        id:           Relations::Variables as RelId,
                        rules:        vec![
                            ],
                        arrangements: vec![
                            Arrangement::Map{
                               name: r###"(Variables{.var_id=(_0: bit<64>), .decl=(hir::VariableDecl{.var_name=(_: bit<32>), .var_type=(_: bit<64>), .value=(_: bit<64>), .scope=(_: bit<32>)}: hir::VariableDecl)}: Variables) /*join*/"###.to_string(),
                                afun: &{fn __f(__v: DDValue) -> Option<(DDValue,DDValue)>
                                {
                                    let __cloned = __v.clone();
                                    match unsafe { Value::Variables::from_ddvalue(__v) }.0 {
                                        Variables{var_id: ref _0, decl: hir_VariableDecl{var_name: _, var_type: _, value: _, scope: _}} => Some(Value::__Bitval64((*_0).clone()).into_ddvalue()),
                                        _ => None
                                    }.map(|x|(x,__cloned))
                                }
                                __f},
                                queryable: false
                            }],
                        change_cb:    None
                    };
    let TypedExpressions = Relation {
                               name:         "TypedExpressions".to_string(),
                               input:        false,
                               distinct:     false,
                               caching_mode: CachingMode::Set,
                               key_func:     None,
                               id:           Relations::TypedExpressions as RelId,
                               rules:        vec![
                                   /* TypedExpressions[(TypedExpressions{.expr_id=expr_id, .type_id=type_id}: TypedExpressions)] :- Expressions[(Expressions{.expr_id=(expr_id: bit<64>), .expr=(hir::Expr{.kind=(hir::ExprLit{.lit=(lit: internment::Intern<hir::Literal>)}: hir::ExprKind)}: hir::Expr)}: Expressions)], (var ty: hir::Type) = hir::typeof(lit), Types[(Types{.type_id=(type_id: bit<64>), .ty=(ty: hir::Type)}: Types)]. */
                                   Rule::CollectionRule {
                                       description: "TypedExpressions[(TypedExpressions{.expr_id=expr_id, .type_id=type_id}: TypedExpressions)] :- Expressions[(Expressions{.expr_id=(expr_id: bit<64>), .expr=(hir::Expr{.kind=(hir::ExprLit{.lit=(lit: internment::Intern<hir::Literal>)}: hir::ExprKind)}: hir::Expr)}: Expressions)], (var ty: hir::Type) = hir::typeof(lit), Types[(Types{.type_id=(type_id: bit<64>), .ty=(ty: hir::Type)}: Types)].".to_string(),
                                       rel: Relations::Expressions as RelId,
                                       xform: Some(XFormCollection::Arrange {
                                                       description: "arrange Expressions[(Expressions{.expr_id=(expr_id: bit<64>), .expr=(hir::Expr{.kind=(hir::ExprLit{.lit=(lit: internment::Intern<hir::Literal>)}: hir::ExprKind)}: hir::Expr)}: Expressions)] by (ty)" .to_string(),
                                                       afun: &{fn __f(__v: DDValue) -> Option<(DDValue,DDValue)>
                                                       {
                                                           let (ref expr_id, ref lit) = match unsafe {  Value::Expressions::from_ddvalue_ref(&__v) }.0 {
                                                               Expressions{expr_id: ref expr_id, expr: hir_Expr{kind: hir_ExprKind::hir_ExprLit{lit: ref lit}}} => ((*expr_id).clone(), (*lit).clone()),
                                                               _ => return None
                                                           };
                                                           let ref ty: hir_Type = match hir_typeof(lit) {
                                                               ty => ty,
                                                               _ => return None
                                                           };
                                                           Some((Value::hir_Type((*ty).clone()).into_ddvalue(), Value::__Bitval64((*expr_id).clone()).into_ddvalue()))
                                                       }
                                                       __f},
                                                       next: Box::new(XFormArrangement::Join{
                                                                          description: "Expressions[(Expressions{.expr_id=(expr_id: bit<64>), .expr=(hir::Expr{.kind=(hir::ExprLit{.lit=(lit: internment::Intern<hir::Literal>)}: hir::ExprKind)}: hir::Expr)}: Expressions)], (var ty: hir::Type) = hir::typeof(lit), Types[(Types{.type_id=(type_id: bit<64>), .ty=(ty: hir::Type)}: Types)]".to_string(),
                                                                          ffun: None,
                                                                          arrangement: (Relations::Types as RelId,0),
                                                                          jfun: &{fn __f(_: &DDValue ,__v1: &DDValue,__v2: &DDValue) -> Option<DDValue>
                                                                          {
                                                                              let ref expr_id = unsafe { Value::__Bitval64::from_ddvalue_ref( __v1 ) }.0;
                                                                              let ref type_id = match unsafe {  Value::Types::from_ddvalue_ref(__v2) }.0 {
                                                                                  Types{type_id: ref type_id, ty: _} => (*type_id).clone(),
                                                                                  _ => return None
                                                                              };
                                                                              Some(Value::TypedExpressions((TypedExpressions{expr_id: (*expr_id).clone(), type_id: (*type_id).clone()})).into_ddvalue())
                                                                          }
                                                                          __f},
                                                                          next: Box::new(None)
                                                                      })
                                                   })
                                   },
                                   /* TypedExpressions[(TypedExpressions{.expr_id=expr_id, .type_id=type_id}: TypedExpressions)] :- Expressions[(Expressions{.expr_id=(expr_id: bit<64>), .expr=(hir::Expr{.kind=(hir::ExprVar{.variable=(var_id: bit<64>)}: hir::ExprKind)}: hir::Expr)}: Expressions)], Variables[(Variables{.var_id=(var_id: bit<64>), .decl=(hir::VariableDecl{.var_name=(_: bit<32>), .var_type=(type_id: bit<64>), .value=(_: bit<64>), .scope=(_: bit<32>)}: hir::VariableDecl)}: Variables)]. */
                                   Rule::ArrangementRule {
                                       description: "TypedExpressions[(TypedExpressions{.expr_id=expr_id, .type_id=type_id}: TypedExpressions)] :- Expressions[(Expressions{.expr_id=(expr_id: bit<64>), .expr=(hir::Expr{.kind=(hir::ExprVar{.variable=(var_id: bit<64>)}: hir::ExprKind)}: hir::Expr)}: Expressions)], Variables[(Variables{.var_id=(var_id: bit<64>), .decl=(hir::VariableDecl{.var_name=(_: bit<32>), .var_type=(type_id: bit<64>), .value=(_: bit<64>), .scope=(_: bit<32>)}: hir::VariableDecl)}: Variables)].".to_string(),
                                       arr: ( Relations::Expressions as RelId, 0),
                                       xform: XFormArrangement::Join{
                                                  description: "Expressions[(Expressions{.expr_id=(expr_id: bit<64>), .expr=(hir::Expr{.kind=(hir::ExprVar{.variable=(var_id: bit<64>)}: hir::ExprKind)}: hir::Expr)}: Expressions)], Variables[(Variables{.var_id=(var_id: bit<64>), .decl=(hir::VariableDecl{.var_name=(_: bit<32>), .var_type=(type_id: bit<64>), .value=(_: bit<64>), .scope=(_: bit<32>)}: hir::VariableDecl)}: Variables)]".to_string(),
                                                  ffun: None,
                                                  arrangement: (Relations::Variables as RelId,0),
                                                  jfun: &{fn __f(_: &DDValue ,__v1: &DDValue,__v2: &DDValue) -> Option<DDValue>
                                                  {
                                                      let (ref expr_id, ref var_id) = match unsafe {  Value::Expressions::from_ddvalue_ref(__v1) }.0 {
                                                          Expressions{expr_id: ref expr_id, expr: hir_Expr{kind: hir_ExprKind::hir_ExprVar{variable: ref var_id}}} => ((*expr_id).clone(), (*var_id).clone()),
                                                          _ => return None
                                                      };
                                                      let ref type_id = match unsafe {  Value::Variables::from_ddvalue_ref(__v2) }.0 {
                                                          Variables{var_id: _, decl: hir_VariableDecl{var_name: _, var_type: ref type_id, value: _, scope: _}} => (*type_id).clone(),
                                                          _ => return None
                                                      };
                                                      Some(Value::TypedExpressions((TypedExpressions{expr_id: (*expr_id).clone(), type_id: (*type_id).clone()})).into_ddvalue())
                                                  }
                                                  __f},
                                                  next: Box::new(None)
                                              }
                                   },
                                   /* TypedExpressions[(TypedExpressions{.expr_id=assign_id, .type_id=type_id}: TypedExpressions)] :- __MultiHead_5[((assign_id: bit<64>), (type_id: bit<64>), (rhs_id: bit<64>))]. */
                                   Rule::CollectionRule {
                                       description: "TypedExpressions[(TypedExpressions{.expr_id=assign_id, .type_id=type_id}: TypedExpressions)] :- __MultiHead_5[((assign_id: bit<64>), (type_id: bit<64>), (rhs_id: bit<64>))].".to_string(),
                                       rel: Relations::__MultiHead_5 as RelId,
                                       xform: Some(XFormCollection::FilterMap{
                                                       description: "head of TypedExpressions[(TypedExpressions{.expr_id=assign_id, .type_id=type_id}: TypedExpressions)] :- __MultiHead_5[((assign_id: bit<64>), (type_id: bit<64>), (rhs_id: bit<64>))]." .to_string(),
                                                       fmfun: &{fn __f(__v: DDValue) -> Option<DDValue>
                                                       {
                                                           let (ref assign_id, ref type_id, ref rhs_id) = match unsafe {  Value::__Tuple3____Bitval64___Bitval64___Bitval64::from_ddvalue_ref(&__v) }.0 {
                                                               (ref assign_id, ref type_id, ref rhs_id) => ((*assign_id).clone(), (*type_id).clone(), (*rhs_id).clone()),
                                                               _ => return None
                                                           };
                                                           Some(Value::TypedExpressions((TypedExpressions{expr_id: (*assign_id).clone(), type_id: (*type_id).clone()})).into_ddvalue())
                                                       }
                                                       __f},
                                                       next: Box::new(None)
                                                   })
                                   },
                                   /* TypedExpressions[(TypedExpressions{.expr_id=rhs_id, .type_id=type_id}: TypedExpressions)] :- __MultiHead_5[((assign_id: bit<64>), (type_id: bit<64>), (rhs_id: bit<64>))]. */
                                   Rule::CollectionRule {
                                       description: "TypedExpressions[(TypedExpressions{.expr_id=rhs_id, .type_id=type_id}: TypedExpressions)] :- __MultiHead_5[((assign_id: bit<64>), (type_id: bit<64>), (rhs_id: bit<64>))].".to_string(),
                                       rel: Relations::__MultiHead_5 as RelId,
                                       xform: Some(XFormCollection::FilterMap{
                                                       description: "head of TypedExpressions[(TypedExpressions{.expr_id=rhs_id, .type_id=type_id}: TypedExpressions)] :- __MultiHead_5[((assign_id: bit<64>), (type_id: bit<64>), (rhs_id: bit<64>))]." .to_string(),
                                                       fmfun: &{fn __f(__v: DDValue) -> Option<DDValue>
                                                       {
                                                           let (ref assign_id, ref type_id, ref rhs_id) = match unsafe {  Value::__Tuple3____Bitval64___Bitval64___Bitval64::from_ddvalue_ref(&__v) }.0 {
                                                               (ref assign_id, ref type_id, ref rhs_id) => ((*assign_id).clone(), (*type_id).clone(), (*rhs_id).clone()),
                                                               _ => return None
                                                           };
                                                           Some(Value::TypedExpressions((TypedExpressions{expr_id: (*rhs_id).clone(), type_id: (*type_id).clone()})).into_ddvalue())
                                                       }
                                                       __f},
                                                       next: Box::new(None)
                                                   })
                                   }],
                               arrangements: vec![
                                   Arrangement::Map{
                                      name: r###"(TypedExpressions{.expr_id=(_: bit<64>), .type_id=(_0: bit<64>)}: TypedExpressions) /*join*/"###.to_string(),
                                       afun: &{fn __f(__v: DDValue) -> Option<(DDValue,DDValue)>
                                       {
                                           let __cloned = __v.clone();
                                           match unsafe { Value::TypedExpressions::from_ddvalue(__v) }.0 {
                                               TypedExpressions{expr_id: _, type_id: ref _0} => Some(Value::__Bitval64((*_0).clone()).into_ddvalue()),
                                               _ => None
                                           }.map(|x|(x,__cloned))
                                       }
                                       __f},
                                       queryable: false
                                   },
                                   Arrangement::Map{
                                      name: r###"(TypedExpressions{.expr_id=(_0: bit<64>), .type_id=(_: bit<64>)}: TypedExpressions) /*join*/"###.to_string(),
                                       afun: &{fn __f(__v: DDValue) -> Option<(DDValue,DDValue)>
                                       {
                                           let __cloned = __v.clone();
                                           match unsafe { Value::TypedExpressions::from_ddvalue(__v) }.0 {
                                               TypedExpressions{expr_id: ref _0, type_id: _} => Some(Value::__Bitval64((*_0).clone()).into_ddvalue()),
                                               _ => None
                                           }.map(|x|(x,__cloned))
                                       }
                                       __f},
                                       queryable: false
                                   }],
                               change_cb:    Some(sync::Arc::new(sync::Mutex::new(__update_cb.clone())))
                           };
    let __MultiHead_5 = Relation {
                            name:         "__MultiHead_5".to_string(),
                            input:        false,
                            distinct:     false,
                            caching_mode: CachingMode::Set,
                            key_func:     None,
                            id:           Relations::__MultiHead_5 as RelId,
                            rules:        vec![
                                /* __MultiHead_5[((assign_id: bit<64>), (type_id: bit<64>), (rhs_id: bit<64>))] :- Expressions[(Expressions{.expr_id=(assign_id: bit<64>), .expr=(hir::Expr{.kind=(hir::ExprAssign{.variable=(var_id: bit<64>), .expr_id=(rhs_id: bit<64>)}: hir::ExprKind)}: hir::Expr)}: Expressions)], Variables[(Variables{.var_id=(var_id: bit<64>), .decl=(hir::VariableDecl{.var_name=(_: bit<32>), .var_type=(_: bit<64>), .value=(_: bit<64>), .scope=(_: bit<32>)}: hir::VariableDecl)}: Variables)], TypedExpressions[(TypedExpressions{.expr_id=(rhs_id: bit<64>), .type_id=(type_id: bit<64>)}: TypedExpressions)]. */
                                Rule::ArrangementRule {
                                    description: "__MultiHead_5[((assign_id: bit<64>), (type_id: bit<64>), (rhs_id: bit<64>))] :- Expressions[(Expressions{.expr_id=(assign_id: bit<64>), .expr=(hir::Expr{.kind=(hir::ExprAssign{.variable=(var_id: bit<64>), .expr_id=(rhs_id: bit<64>)}: hir::ExprKind)}: hir::Expr)}: Expressions)], Variables[(Variables{.var_id=(var_id: bit<64>), .decl=(hir::VariableDecl{.var_name=(_: bit<32>), .var_type=(_: bit<64>), .value=(_: bit<64>), .scope=(_: bit<32>)}: hir::VariableDecl)}: Variables)], TypedExpressions[(TypedExpressions{.expr_id=(rhs_id: bit<64>), .type_id=(type_id: bit<64>)}: TypedExpressions)].".to_string(),
                                    arr: ( Relations::Expressions as RelId, 1),
                                    xform: XFormArrangement::Join{
                                               description: "Expressions[(Expressions{.expr_id=(assign_id: bit<64>), .expr=(hir::Expr{.kind=(hir::ExprAssign{.variable=(var_id: bit<64>), .expr_id=(rhs_id: bit<64>)}: hir::ExprKind)}: hir::Expr)}: Expressions)], Variables[(Variables{.var_id=(var_id: bit<64>), .decl=(hir::VariableDecl{.var_name=(_: bit<32>), .var_type=(_: bit<64>), .value=(_: bit<64>), .scope=(_: bit<32>)}: hir::VariableDecl)}: Variables)]".to_string(),
                                               ffun: None,
                                               arrangement: (Relations::Variables as RelId,0),
                                               jfun: &{fn __f(_: &DDValue ,__v1: &DDValue,__v2: &DDValue) -> Option<DDValue>
                                               {
                                                   let (ref assign_id, ref var_id, ref rhs_id) = match unsafe {  Value::Expressions::from_ddvalue_ref(__v1) }.0 {
                                                       Expressions{expr_id: ref assign_id, expr: hir_Expr{kind: hir_ExprKind::hir_ExprAssign{variable: ref var_id, expr_id: ref rhs_id}}} => ((*assign_id).clone(), (*var_id).clone(), (*rhs_id).clone()),
                                                       _ => return None
                                                   };
                                                   let () = match unsafe {  Value::Variables::from_ddvalue_ref(__v2) }.0 {
                                                       Variables{var_id: _, decl: hir_VariableDecl{var_name: _, var_type: _, value: _, scope: _}} => (),
                                                       _ => return None
                                                   };
                                                   Some(Value::__Tuple2____Bitval64___Bitval64(((*assign_id).clone(), (*rhs_id).clone())).into_ddvalue())
                                               }
                                               __f},
                                               next: Box::new(Some(XFormCollection::Arrange {
                                                                       description: "arrange Expressions[(Expressions{.expr_id=(assign_id: bit<64>), .expr=(hir::Expr{.kind=(hir::ExprAssign{.variable=(var_id: bit<64>), .expr_id=(rhs_id: bit<64>)}: hir::ExprKind)}: hir::Expr)}: Expressions)], Variables[(Variables{.var_id=(var_id: bit<64>), .decl=(hir::VariableDecl{.var_name=(_: bit<32>), .var_type=(_: bit<64>), .value=(_: bit<64>), .scope=(_: bit<32>)}: hir::VariableDecl)}: Variables)] by (rhs_id)" .to_string(),
                                                                       afun: &{fn __f(__v: DDValue) -> Option<(DDValue,DDValue)>
                                                                       {
                                                                           let (ref assign_id, ref rhs_id) = unsafe { Value::__Tuple2____Bitval64___Bitval64::from_ddvalue_ref( &__v ) }.0;
                                                                           Some((Value::__Bitval64((*rhs_id).clone()).into_ddvalue(), Value::__Tuple2____Bitval64___Bitval64(((*assign_id).clone(), (*rhs_id).clone())).into_ddvalue()))
                                                                       }
                                                                       __f},
                                                                       next: Box::new(XFormArrangement::Join{
                                                                                          description: "Expressions[(Expressions{.expr_id=(assign_id: bit<64>), .expr=(hir::Expr{.kind=(hir::ExprAssign{.variable=(var_id: bit<64>), .expr_id=(rhs_id: bit<64>)}: hir::ExprKind)}: hir::Expr)}: Expressions)], Variables[(Variables{.var_id=(var_id: bit<64>), .decl=(hir::VariableDecl{.var_name=(_: bit<32>), .var_type=(_: bit<64>), .value=(_: bit<64>), .scope=(_: bit<32>)}: hir::VariableDecl)}: Variables)], TypedExpressions[(TypedExpressions{.expr_id=(rhs_id: bit<64>), .type_id=(type_id: bit<64>)}: TypedExpressions)]".to_string(),
                                                                                          ffun: None,
                                                                                          arrangement: (Relations::TypedExpressions as RelId,1),
                                                                                          jfun: &{fn __f(_: &DDValue ,__v1: &DDValue,__v2: &DDValue) -> Option<DDValue>
                                                                                          {
                                                                                              let (ref assign_id, ref rhs_id) = unsafe { Value::__Tuple2____Bitval64___Bitval64::from_ddvalue_ref( __v1 ) }.0;
                                                                                              let ref type_id = match unsafe {  Value::TypedExpressions::from_ddvalue_ref(__v2) }.0 {
                                                                                                  TypedExpressions{expr_id: _, type_id: ref type_id} => (*type_id).clone(),
                                                                                                  _ => return None
                                                                                              };
                                                                                              Some(Value::__Tuple3____Bitval64___Bitval64___Bitval64(((*assign_id).clone(), (*type_id).clone(), (*rhs_id).clone())).into_ddvalue())
                                                                                          }
                                                                                          __f},
                                                                                          next: Box::new(None)
                                                                                      })
                                                                   }))
                                           }
                                }],
                            arrangements: vec![
                                ],
                            change_cb:    None
                        };
    let UnifiedTypes = Relation {
                           name:         "UnifiedTypes".to_string(),
                           input:        false,
                           distinct:     false,
                           caching_mode: CachingMode::Set,
                           key_func:     None,
                           id:           Relations::UnifiedTypes as RelId,
                           rules:        vec![
                               /* UnifiedTypes[(UnifiedTypes{.expr_id=expr_id, .type_id=unified_type}: UnifiedTypes)] :- TypedExpressions[(TypedExpressions{.expr_id=(expr_id: bit<64>), .type_id=(type_id: bit<64>)}: TypedExpressions)], Types[(Types{.type_id=(type_id: bit<64>), .ty=(ty: hir::Type)}: Types)], var unified_type = Aggregate(expr_id, unify_types((type_id, ty))). */
                               Rule::ArrangementRule {
                                   description: "UnifiedTypes[(UnifiedTypes{.expr_id=expr_id, .type_id=unified_type}: UnifiedTypes)] :- TypedExpressions[(TypedExpressions{.expr_id=(expr_id: bit<64>), .type_id=(type_id: bit<64>)}: TypedExpressions)], Types[(Types{.type_id=(type_id: bit<64>), .ty=(ty: hir::Type)}: Types)], var unified_type = Aggregate(expr_id, unify_types((type_id, ty))).".to_string(),
                                   arr: ( Relations::TypedExpressions as RelId, 0),
                                   xform: XFormArrangement::Join{
                                              description: "TypedExpressions[(TypedExpressions{.expr_id=(expr_id: bit<64>), .type_id=(type_id: bit<64>)}: TypedExpressions)], Types[(Types{.type_id=(type_id: bit<64>), .ty=(ty: hir::Type)}: Types)]".to_string(),
                                              ffun: None,
                                              arrangement: (Relations::Types as RelId,1),
                                              jfun: &{fn __f(_: &DDValue ,__v1: &DDValue,__v2: &DDValue) -> Option<DDValue>
                                              {
                                                  let (ref expr_id, ref type_id) = match unsafe {  Value::TypedExpressions::from_ddvalue_ref(__v1) }.0 {
                                                      TypedExpressions{expr_id: ref expr_id, type_id: ref type_id} => ((*expr_id).clone(), (*type_id).clone()),
                                                      _ => return None
                                                  };
                                                  let ref ty = match unsafe {  Value::Types::from_ddvalue_ref(__v2) }.0 {
                                                      Types{type_id: _, ty: ref ty} => (*ty).clone(),
                                                      _ => return None
                                                  };
                                                  Some(Value::__Tuple3____Bitval64___Bitval64_hir_Type(((*expr_id).clone(), (*type_id).clone(), (*ty).clone())).into_ddvalue())
                                              }
                                              __f},
                                              next: Box::new(Some(XFormCollection::Arrange {
                                                                      description: "arrange TypedExpressions[(TypedExpressions{.expr_id=(expr_id: bit<64>), .type_id=(type_id: bit<64>)}: TypedExpressions)], Types[(Types{.type_id=(type_id: bit<64>), .ty=(ty: hir::Type)}: Types)] by (expr_id)" .to_string(),
                                                                      afun: &{fn __f(__v: DDValue) -> Option<(DDValue,DDValue)>
                                                                      {
                                                                          let (ref expr_id, ref type_id, ref ty) = unsafe { Value::__Tuple3____Bitval64___Bitval64_hir_Type::from_ddvalue_ref( &__v ) }.0;
                                                                          Some((Value::__Bitval64((*expr_id).clone()).into_ddvalue(), Value::__Tuple3____Bitval64___Bitval64_hir_Type(((*expr_id).clone(), (*type_id).clone(), (*ty).clone())).into_ddvalue()))
                                                                      }
                                                                      __f},
                                                                      next: Box::new(XFormArrangement::Aggregate{
                                                                                         description: "TypedExpressions[(TypedExpressions{.expr_id=(expr_id: bit<64>), .type_id=(type_id: bit<64>)}: TypedExpressions)], Types[(Types{.type_id=(type_id: bit<64>), .ty=(ty: hir::Type)}: Types)], var unified_type = Aggregate(expr_id, unify_types((type_id, ty)))".to_string(),
                                                                                         ffun: None,
                                                                                         aggfun: &{fn __f(__key: &DDValue, __group__: &[(&DDValue, Weight)]) -> DDValue
                                                                                     {
                                                                                         let ref expr_id = unsafe { Value::__Bitval64::from_ddvalue_ref( __key ) }.0;
                                                                                         let unified_type = unify_types::<u64>(&std_Group::new(&(*expr_id).clone(), __group__, {fn __f(__v: &DDValue) ->  (u64, hir_Type)
                                                                                                                                                                               {
                                                                                                                                                                                   let (ref expr_id, ref type_id, ref ty) = unsafe { Value::__Tuple3____Bitval64___Bitval64_hir_Type::from_ddvalue_ref( __v ) }.0;
                                                                                                                                                                                   ((*type_id).clone(), (*ty).clone())
                                                                                                                                                                               }
                                                                                                                                                                               std::rc::Rc::new(__f)}));
                                                                                         Value::__Tuple2____Bitval64___Bitval64((unified_type.clone(), (*expr_id).clone())).into_ddvalue()
                                                                                     }
                                                                                     __f},
                                                                                         next: Box::new(Some(XFormCollection::FilterMap{
                                                                                                                 description: "head of UnifiedTypes[(UnifiedTypes{.expr_id=expr_id, .type_id=unified_type}: UnifiedTypes)] :- TypedExpressions[(TypedExpressions{.expr_id=(expr_id: bit<64>), .type_id=(type_id: bit<64>)}: TypedExpressions)], Types[(Types{.type_id=(type_id: bit<64>), .ty=(ty: hir::Type)}: Types)], var unified_type = Aggregate(expr_id, unify_types((type_id, ty)))." .to_string(),
                                                                                                                 fmfun: &{fn __f(__v: DDValue) -> Option<DDValue>
                                                                                                                 {
                                                                                                                     let (ref unified_type, ref expr_id) = unsafe { Value::__Tuple2____Bitval64___Bitval64::from_ddvalue_ref( &__v ) }.0;
                                                                                                                     Some(Value::UnifiedTypes((UnifiedTypes{expr_id: (*expr_id).clone(), type_id: (*unified_type).clone()})).into_ddvalue())
                                                                                                                 }
                                                                                                                 __f},
                                                                                                                 next: Box::new(None)
                                                                                                             }))
                                                                                     })
                                                                  }))
                                          }
                               }],
                           arrangements: vec![
                               ],
                           change_cb:    Some(sync::Arc::new(sync::Mutex::new(__update_cb.clone())))
                       };
    let INPUT_Variables = Relation {
        name: "INPUT_Variables".to_string(),
        input: false,
        distinct: false,
        caching_mode: CachingMode::Set,
        key_func: None,
        id: Relations::INPUT_Variables as RelId,
        rules: vec![
            /* INPUT_Variables[x] :- Variables[(x: Variables)]. */
            Rule::CollectionRule {
                description: "INPUT_Variables[x] :- Variables[(x: Variables)].".to_string(),
                rel: Relations::Variables as RelId,
                xform: Some(XFormCollection::FilterMap {
                    description: "head of INPUT_Variables[x] :- Variables[(x: Variables)]."
                        .to_string(),
                    fmfun: &{
                        fn __f(__v: DDValue) -> Option<DDValue> {
                            let ref x = match unsafe { Value::Variables::from_ddvalue_ref(&__v) }.0
                            {
                                ref x => (*x).clone(),
                                _ => return None,
                            };
                            Some(Value::Variables((*x).clone()).into_ddvalue())
                        }
                        __f
                    },
                    next: Box::new(None),
                }),
            },
        ],
        arrangements: vec![],
        change_cb: Some(sync::Arc::new(sync::Mutex::new(__update_cb.clone()))),
    };
    let VariablesInScope = Relation {
                               name:         "VariablesInScope".to_string(),
                               input:        false,
                               distinct:     false,
                               caching_mode: CachingMode::Set,
                               key_func:     None,
                               id:           Relations::VariablesInScope as RelId,
                               rules:        vec![
                                   /* VariablesInScope[(VariablesInScope{.scope=scope, .var_id=var_id, .decl_scope=scope}: VariablesInScope)] :- Variables[(Variables{.var_id=(var_id: bit<64>), .decl=(hir::VariableDecl{.var_name=(_: bit<32>), .var_type=(_: bit<64>), .value=(_: bit<64>), .scope=(scope: bit<32>)}: hir::VariableDecl)}: Variables)]. */
                                   Rule::CollectionRule {
                                       description: "VariablesInScope[(VariablesInScope{.scope=scope, .var_id=var_id, .decl_scope=scope}: VariablesInScope)] :- Variables[(Variables{.var_id=(var_id: bit<64>), .decl=(hir::VariableDecl{.var_name=(_: bit<32>), .var_type=(_: bit<64>), .value=(_: bit<64>), .scope=(scope: bit<32>)}: hir::VariableDecl)}: Variables)].".to_string(),
                                       rel: Relations::Variables as RelId,
                                       xform: Some(XFormCollection::FilterMap{
                                                       description: "head of VariablesInScope[(VariablesInScope{.scope=scope, .var_id=var_id, .decl_scope=scope}: VariablesInScope)] :- Variables[(Variables{.var_id=(var_id: bit<64>), .decl=(hir::VariableDecl{.var_name=(_: bit<32>), .var_type=(_: bit<64>), .value=(_: bit<64>), .scope=(scope: bit<32>)}: hir::VariableDecl)}: Variables)]." .to_string(),
                                                       fmfun: &{fn __f(__v: DDValue) -> Option<DDValue>
                                                       {
                                                           let (ref var_id, ref scope) = match unsafe {  Value::Variables::from_ddvalue_ref(&__v) }.0 {
                                                               Variables{var_id: ref var_id, decl: hir_VariableDecl{var_name: _, var_type: _, value: _, scope: ref scope}} => ((*var_id).clone(), (*scope).clone()),
                                                               _ => return None
                                                           };
                                                           Some(Value::VariablesInScope((VariablesInScope{scope: (*scope).clone(), var_id: (*var_id).clone(), decl_scope: (*scope).clone()})).into_ddvalue())
                                                       }
                                                       __f},
                                                       next: Box::new(None)
                                                   })
                                   },
                                   /* VariablesInScope[(VariablesInScope{.scope=child, .var_id=var_id, .decl_scope=decl_scope}: VariablesInScope)] :- VariablesInScope[(VariablesInScope{.scope=(parent: bit<32>), .var_id=(var_id: bit<64>), .decl_scope=(decl_scope: bit<32>)}: VariablesInScope)], VariableScopes[(VariableScopes{.parent=(parent: bit<32>), .child=(child: bit<32>)}: VariableScopes)]. */
                                   Rule::ArrangementRule {
                                       description: "VariablesInScope[(VariablesInScope{.scope=child, .var_id=var_id, .decl_scope=decl_scope}: VariablesInScope)] :- VariablesInScope[(VariablesInScope{.scope=(parent: bit<32>), .var_id=(var_id: bit<64>), .decl_scope=(decl_scope: bit<32>)}: VariablesInScope)], VariableScopes[(VariableScopes{.parent=(parent: bit<32>), .child=(child: bit<32>)}: VariableScopes)].".to_string(),
                                       arr: ( Relations::VariablesInScope as RelId, 0),
                                       xform: XFormArrangement::Join{
                                                  description: "VariablesInScope[(VariablesInScope{.scope=(parent: bit<32>), .var_id=(var_id: bit<64>), .decl_scope=(decl_scope: bit<32>)}: VariablesInScope)], VariableScopes[(VariableScopes{.parent=(parent: bit<32>), .child=(child: bit<32>)}: VariableScopes)]".to_string(),
                                                  ffun: None,
                                                  arrangement: (Relations::VariableScopes as RelId,0),
                                                  jfun: &{fn __f(_: &DDValue ,__v1: &DDValue,__v2: &DDValue) -> Option<DDValue>
                                                  {
                                                      let (ref parent, ref var_id, ref decl_scope) = match unsafe {  Value::VariablesInScope::from_ddvalue_ref(__v1) }.0 {
                                                          VariablesInScope{scope: ref parent, var_id: ref var_id, decl_scope: ref decl_scope} => ((*parent).clone(), (*var_id).clone(), (*decl_scope).clone()),
                                                          _ => return None
                                                      };
                                                      let ref child = match unsafe {  Value::VariableScopes::from_ddvalue_ref(__v2) }.0 {
                                                          VariableScopes{parent: _, child: ref child} => (*child).clone(),
                                                          _ => return None
                                                      };
                                                      Some(Value::VariablesInScope((VariablesInScope{scope: (*child).clone(), var_id: (*var_id).clone(), decl_scope: (*decl_scope).clone()})).into_ddvalue())
                                                  }
                                                  __f},
                                                  next: Box::new(None)
                                              }
                                   }],
                               arrangements: vec![
                                   Arrangement::Map{
                                      name: r###"(VariablesInScope{.scope=(_0: bit<32>), .var_id=(_: bit<64>), .decl_scope=(_: bit<32>)}: VariablesInScope) /*join*/"###.to_string(),
                                       afun: &{fn __f(__v: DDValue) -> Option<(DDValue,DDValue)>
                                       {
                                           let __cloned = __v.clone();
                                           match unsafe { Value::VariablesInScope::from_ddvalue(__v) }.0 {
                                               VariablesInScope{scope: ref _0, var_id: _, decl_scope: _} => Some(Value::__Bitval32((*_0).clone()).into_ddvalue()),
                                               _ => None
                                           }.map(|x|(x,__cloned))
                                       }
                                       __f},
                                       queryable: false
                                   }],
                               change_cb:    Some(sync::Arc::new(sync::Mutex::new(__update_cb.clone())))
                           };
    let __Null = Relation {
        name: "__Null".to_string(),
        input: false,
        distinct: false,
        caching_mode: CachingMode::Set,
        key_func: None,
        id: Relations::__Null as RelId,
        rules: vec![],
        arrangements: vec![Arrangement::Map {
            name: r###"_ /*join*/"###.to_string(),
            afun: &{
                fn __f(__v: DDValue) -> Option<(DDValue, DDValue)> {
                    let __cloned = __v.clone();
                    match unsafe { Value::__Tuple0__::from_ddvalue(__v) }.0 {
                        _ => Some(Value::__Tuple0__(()).into_ddvalue()),
                        _ => None,
                    }
                    .map(|x| (x, __cloned))
                }
                __f
            },
            queryable: true,
        }],
        change_cb: None,
    };
    Program {
        nodes: vec![
            ProgNode::Rel { rel: Errors },
            ProgNode::Rel { rel: Expressions },
            ProgNode::Rel {
                rel: INPUT_Expressions,
            },
            ProgNode::Rel { rel: Functions },
            ProgNode::Rel {
                rel: INPUT_Functions,
            },
            ProgNode::Rel { rel: Items },
            ProgNode::Rel { rel: INPUT_Items },
            ProgNode::Rel { rel: Statements },
            ProgNode::Rel {
                rel: INPUT_Statements,
            },
            ProgNode::Rel { rel: Types },
            ProgNode::Rel { rel: Literals },
            ProgNode::Rel { rel: INPUT_Types },
            ProgNode::Rel {
                rel: VariableScopes,
            },
            ProgNode::Rel {
                rel: INPUT_VariableScopes,
            },
            ProgNode::Rel { rel: Variables },
            ProgNode::SCC {
                rels: vec![
                    RecursiveRelation {
                        rel: TypedExpressions,
                        distinct: true,
                    },
                    RecursiveRelation {
                        rel: __MultiHead_5,
                        distinct: true,
                    },
                ],
            },
            ProgNode::Rel { rel: UnifiedTypes },
            ProgNode::Rel {
                rel: INPUT_Variables,
            },
            ProgNode::SCC {
                rels: vec![RecursiveRelation {
                    rel: VariablesInScope,
                    distinct: true,
                }],
            },
            ProgNode::Rel { rel: __Null },
        ],
        init_data: vec![],
    }
}
