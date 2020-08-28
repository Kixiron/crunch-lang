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
                                 name: r###"(Expressions{.expr_id=(_: bit<64>), .expr=(hir::Expr{.kind=(hir::ExprLit{.lit=(_0: internment::Intern<hir::Literal>)}: hir::ExprKind)}: hir::Expr)}: Expressions) /*join*/"###.to_string(),
                                  afun: &{fn __f(__v: DDValue) -> Option<(DDValue,DDValue)>
                                  {
                                      let __cloned = __v.clone();
                                      match unsafe { Value::Expressions::from_ddvalue(__v) }.0 {
                                          Expressions{expr_id: _, expr: hir_Expr{kind: hir_ExprKind::hir_ExprLit{lit: ref _0}}} => Some(Value::internment_Intern__hir_Literal((*_0).clone()).into_ddvalue()),
                                          _ => None
                                      }.map(|x|(x,__cloned))
                                  }
                                  __f},
                                  queryable: false
                              },
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
    let Types = Relation {
        name: "Types".to_string(),
        input: true,
        distinct: false,
        caching_mode: CachingMode::Set,
        key_func: None,
        id: Relations::Types as RelId,
        rules: vec![],
        arrangements: vec![Arrangement::Map {
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
        }],
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
                           Arrangement::Map{
                              name: r###"(Literals{.lit=(_0: internment::Intern<hir::Literal>), .lit_type=(_: bit<64>)}: Literals) /*join*/"###.to_string(),
                               afun: &{fn __f(__v: DDValue) -> Option<(DDValue,DDValue)>
                               {
                                   let __cloned = __v.clone();
                                   match unsafe { Value::Literals::from_ddvalue(__v) }.0 {
                                       Literals{lit: ref _0, lit_type: _} => Some(Value::internment_Intern__hir_Literal((*_0).clone()).into_ddvalue()),
                                       _ => None
                                   }.map(|x|(x,__cloned))
                               }
                               __f},
                               queryable: false
                           }],
                       change_cb:    Some(sync::Arc::new(sync::Mutex::new(__update_cb.clone())))
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
                               name: r###"(Variables{.var_id=(_0: bit<64>), .variable=(hir::Variable{.var_name=(_: bit<32>), .var_type=(_: bit<64>), .value=(_: bit<64>), .scope=(_: bit<32>)}: hir::Variable)}: Variables) /*join*/"###.to_string(),
                                afun: &{fn __f(__v: DDValue) -> Option<(DDValue,DDValue)>
                                {
                                    let __cloned = __v.clone();
                                    match unsafe { Value::Variables::from_ddvalue(__v) }.0 {
                                        Variables{var_id: ref _0, variable: hir_Variable{var_name: _, var_type: _, value: _, scope: _}} => Some(Value::__Bitval64((*_0).clone()).into_ddvalue()),
                                        _ => None
                                    }.map(|x|(x,__cloned))
                                }
                                __f},
                                queryable: false
                            }],
                        change_cb:    None
                    };
    let __Prefix_0 = Relation {
                         name:         "__Prefix_0".to_string(),
                         input:        false,
                         distinct:     false,
                         caching_mode: CachingMode::Set,
                         key_func:     None,
                         id:           Relations::__Prefix_0 as RelId,
                         rules:        vec![
                             /* __Prefix_0[((expr_id: bit<64>), (var_id: bit<64>), (rhs_id: bit<64>), (type_id: bit<64>))] :- Expressions[(Expressions{.expr_id=(expr_id: bit<64>), .expr=(hir::Expr{.kind=(hir::ExprAssign{.variable=(var_id: bit<64>), .expr_id=(rhs_id: bit<64>)}: hir::ExprKind)}: hir::Expr)}: Expressions)], Variables[(Variables{.var_id=(var_id: bit<64>), .variable=(hir::Variable{.var_name=(_: bit<32>), .var_type=(type_id: bit<64>), .value=(_: bit<64>), .scope=(_: bit<32>)}: hir::Variable)}: Variables)]. */
                             Rule::ArrangementRule {
                                 description: "__Prefix_0[((expr_id: bit<64>), (var_id: bit<64>), (rhs_id: bit<64>), (type_id: bit<64>))] :- Expressions[(Expressions{.expr_id=(expr_id: bit<64>), .expr=(hir::Expr{.kind=(hir::ExprAssign{.variable=(var_id: bit<64>), .expr_id=(rhs_id: bit<64>)}: hir::ExprKind)}: hir::Expr)}: Expressions)], Variables[(Variables{.var_id=(var_id: bit<64>), .variable=(hir::Variable{.var_name=(_: bit<32>), .var_type=(type_id: bit<64>), .value=(_: bit<64>), .scope=(_: bit<32>)}: hir::Variable)}: Variables)].".to_string(),
                                 arr: ( Relations::Expressions as RelId, 2),
                                 xform: XFormArrangement::Join{
                                            description: "Expressions[(Expressions{.expr_id=(expr_id: bit<64>), .expr=(hir::Expr{.kind=(hir::ExprAssign{.variable=(var_id: bit<64>), .expr_id=(rhs_id: bit<64>)}: hir::ExprKind)}: hir::Expr)}: Expressions)], Variables[(Variables{.var_id=(var_id: bit<64>), .variable=(hir::Variable{.var_name=(_: bit<32>), .var_type=(type_id: bit<64>), .value=(_: bit<64>), .scope=(_: bit<32>)}: hir::Variable)}: Variables)]".to_string(),
                                            ffun: None,
                                            arrangement: (Relations::Variables as RelId,0),
                                            jfun: &{fn __f(_: &DDValue ,__v1: &DDValue,__v2: &DDValue) -> Option<DDValue>
                                            {
                                                let (ref expr_id, ref var_id, ref rhs_id) = match unsafe {  Value::Expressions::from_ddvalue_ref(__v1) }.0 {
                                                    Expressions{expr_id: ref expr_id, expr: hir_Expr{kind: hir_ExprKind::hir_ExprAssign{variable: ref var_id, expr_id: ref rhs_id}}} => ((*expr_id).clone(), (*var_id).clone(), (*rhs_id).clone()),
                                                    _ => return None
                                                };
                                                let ref type_id = match unsafe {  Value::Variables::from_ddvalue_ref(__v2) }.0 {
                                                    Variables{var_id: _, variable: hir_Variable{var_name: _, var_type: ref type_id, value: _, scope: _}} => (*type_id).clone(),
                                                    _ => return None
                                                };
                                                Some(Value::__Tuple4____Bitval64___Bitval64___Bitval64___Bitval64(((*expr_id).clone(), (*var_id).clone(), (*rhs_id).clone(), (*type_id).clone())).into_ddvalue())
                                            }
                                            __f},
                                            next: Box::new(None)
                                        }
                             }],
                         arrangements: vec![
                             Arrangement::Map{
                                name: r###"((_: bit<64>), (_: bit<64>), (_0: bit<64>), (_: bit<64>)) /*join*/"###.to_string(),
                                 afun: &{fn __f(__v: DDValue) -> Option<(DDValue,DDValue)>
                                 {
                                     let __cloned = __v.clone();
                                     match unsafe { Value::__Tuple4____Bitval64___Bitval64___Bitval64___Bitval64::from_ddvalue(__v) }.0 {
                                         (_, _, ref _0, _) => Some(Value::__Bitval64((*_0).clone()).into_ddvalue()),
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
                                   /* TypedExpressions[(TypedExpressions{.expr_id=expr_id, .type_id=type_id}: TypedExpressions)] :- Expressions[(Expressions{.expr_id=(expr_id: bit<64>), .expr=(hir::Expr{.kind=(hir::ExprLit{.lit=(lit: internment::Intern<hir::Literal>)}: hir::ExprKind)}: hir::Expr)}: Expressions)], Literals[(Literals{.lit=(lit: internment::Intern<hir::Literal>), .lit_type=(type_id: bit<64>)}: Literals)]. */
                                   Rule::ArrangementRule {
                                       description: "TypedExpressions[(TypedExpressions{.expr_id=expr_id, .type_id=type_id}: TypedExpressions)] :- Expressions[(Expressions{.expr_id=(expr_id: bit<64>), .expr=(hir::Expr{.kind=(hir::ExprLit{.lit=(lit: internment::Intern<hir::Literal>)}: hir::ExprKind)}: hir::Expr)}: Expressions)], Literals[(Literals{.lit=(lit: internment::Intern<hir::Literal>), .lit_type=(type_id: bit<64>)}: Literals)].".to_string(),
                                       arr: ( Relations::Expressions as RelId, 0),
                                       xform: XFormArrangement::Join{
                                                  description: "Expressions[(Expressions{.expr_id=(expr_id: bit<64>), .expr=(hir::Expr{.kind=(hir::ExprLit{.lit=(lit: internment::Intern<hir::Literal>)}: hir::ExprKind)}: hir::Expr)}: Expressions)], Literals[(Literals{.lit=(lit: internment::Intern<hir::Literal>), .lit_type=(type_id: bit<64>)}: Literals)]".to_string(),
                                                  ffun: None,
                                                  arrangement: (Relations::Literals as RelId,0),
                                                  jfun: &{fn __f(_: &DDValue ,__v1: &DDValue,__v2: &DDValue) -> Option<DDValue>
                                                  {
                                                      let (ref expr_id, ref lit) = match unsafe {  Value::Expressions::from_ddvalue_ref(__v1) }.0 {
                                                          Expressions{expr_id: ref expr_id, expr: hir_Expr{kind: hir_ExprKind::hir_ExprLit{lit: ref lit}}} => ((*expr_id).clone(), (*lit).clone()),
                                                          _ => return None
                                                      };
                                                      let ref type_id = match unsafe {  Value::Literals::from_ddvalue_ref(__v2) }.0 {
                                                          Literals{lit: _, lit_type: ref type_id} => (*type_id).clone(),
                                                          _ => return None
                                                      };
                                                      Some(Value::TypedExpressions((TypedExpressions{expr_id: (*expr_id).clone(), type_id: (*type_id).clone()})).into_ddvalue())
                                                  }
                                                  __f},
                                                  next: Box::new(None)
                                              }
                                   },
                                   /* TypedExpressions[(TypedExpressions{.expr_id=expr_id, .type_id=type_id}: TypedExpressions)] :- Expressions[(Expressions{.expr_id=(expr_id: bit<64>), .expr=(hir::Expr{.kind=(hir::ExprVar{.variable=(var_id: bit<64>)}: hir::ExprKind)}: hir::Expr)}: Expressions)], Variables[(Variables{.var_id=(var_id: bit<64>), .variable=(hir::Variable{.var_name=(_: bit<32>), .var_type=(type_id: bit<64>), .value=(_: bit<64>), .scope=(_: bit<32>)}: hir::Variable)}: Variables)]. */
                                   Rule::ArrangementRule {
                                       description: "TypedExpressions[(TypedExpressions{.expr_id=expr_id, .type_id=type_id}: TypedExpressions)] :- Expressions[(Expressions{.expr_id=(expr_id: bit<64>), .expr=(hir::Expr{.kind=(hir::ExprVar{.variable=(var_id: bit<64>)}: hir::ExprKind)}: hir::Expr)}: Expressions)], Variables[(Variables{.var_id=(var_id: bit<64>), .variable=(hir::Variable{.var_name=(_: bit<32>), .var_type=(type_id: bit<64>), .value=(_: bit<64>), .scope=(_: bit<32>)}: hir::Variable)}: Variables)].".to_string(),
                                       arr: ( Relations::Expressions as RelId, 1),
                                       xform: XFormArrangement::Join{
                                                  description: "Expressions[(Expressions{.expr_id=(expr_id: bit<64>), .expr=(hir::Expr{.kind=(hir::ExprVar{.variable=(var_id: bit<64>)}: hir::ExprKind)}: hir::Expr)}: Expressions)], Variables[(Variables{.var_id=(var_id: bit<64>), .variable=(hir::Variable{.var_name=(_: bit<32>), .var_type=(type_id: bit<64>), .value=(_: bit<64>), .scope=(_: bit<32>)}: hir::Variable)}: Variables)]".to_string(),
                                                  ffun: None,
                                                  arrangement: (Relations::Variables as RelId,0),
                                                  jfun: &{fn __f(_: &DDValue ,__v1: &DDValue,__v2: &DDValue) -> Option<DDValue>
                                                  {
                                                      let (ref expr_id, ref var_id) = match unsafe {  Value::Expressions::from_ddvalue_ref(__v1) }.0 {
                                                          Expressions{expr_id: ref expr_id, expr: hir_Expr{kind: hir_ExprKind::hir_ExprVar{variable: ref var_id}}} => ((*expr_id).clone(), (*var_id).clone()),
                                                          _ => return None
                                                      };
                                                      let ref type_id = match unsafe {  Value::Variables::from_ddvalue_ref(__v2) }.0 {
                                                          Variables{var_id: _, variable: hir_Variable{var_name: _, var_type: ref type_id, value: _, scope: _}} => (*type_id).clone(),
                                                          _ => return None
                                                      };
                                                      Some(Value::TypedExpressions((TypedExpressions{expr_id: (*expr_id).clone(), type_id: (*type_id).clone()})).into_ddvalue())
                                                  }
                                                  __f},
                                                  next: Box::new(None)
                                              }
                                   },
                                   /* TypedExpressions[(TypedExpressions{.expr_id=expr_id, .type_id=type_id}: TypedExpressions)] :- __Prefix_0[((expr_id: bit<64>), (var_id: bit<64>), (rhs_id: bit<64>), (type_id: bit<64>))], TypedExpressions[(TypedExpressions{.expr_id=(rhs_id: bit<64>), .type_id=(rhs_ty: bit<64>)}: TypedExpressions)], (rhs_ty == type_id). */
                                   Rule::ArrangementRule {
                                       description: "TypedExpressions[(TypedExpressions{.expr_id=expr_id, .type_id=type_id}: TypedExpressions)] :- __Prefix_0[((expr_id: bit<64>), (var_id: bit<64>), (rhs_id: bit<64>), (type_id: bit<64>))], TypedExpressions[(TypedExpressions{.expr_id=(rhs_id: bit<64>), .type_id=(rhs_ty: bit<64>)}: TypedExpressions)], (rhs_ty == type_id).".to_string(),
                                       arr: ( Relations::__Prefix_0 as RelId, 0),
                                       xform: XFormArrangement::Join{
                                                  description: "__Prefix_0[((expr_id: bit<64>), (var_id: bit<64>), (rhs_id: bit<64>), (type_id: bit<64>))], TypedExpressions[(TypedExpressions{.expr_id=(rhs_id: bit<64>), .type_id=(rhs_ty: bit<64>)}: TypedExpressions)]".to_string(),
                                                  ffun: None,
                                                  arrangement: (Relations::TypedExpressions as RelId,0),
                                                  jfun: &{fn __f(_: &DDValue ,__v1: &DDValue,__v2: &DDValue) -> Option<DDValue>
                                                  {
                                                      let (ref expr_id, ref var_id, ref rhs_id, ref type_id) = match unsafe {  Value::__Tuple4____Bitval64___Bitval64___Bitval64___Bitval64::from_ddvalue_ref(__v1) }.0 {
                                                          (ref expr_id, ref var_id, ref rhs_id, ref type_id) => ((*expr_id).clone(), (*var_id).clone(), (*rhs_id).clone(), (*type_id).clone()),
                                                          _ => return None
                                                      };
                                                      let ref rhs_ty = match unsafe {  Value::TypedExpressions::from_ddvalue_ref(__v2) }.0 {
                                                          TypedExpressions{expr_id: _, type_id: ref rhs_ty} => (*rhs_ty).clone(),
                                                          _ => return None
                                                      };
                                                      if !((&*rhs_ty) == (&*type_id)) {return None;};
                                                      Some(Value::TypedExpressions((TypedExpressions{expr_id: (*expr_id).clone(), type_id: (*type_id).clone()})).into_ddvalue())
                                                  }
                                                  __f},
                                                  next: Box::new(None)
                                              }
                                   }],
                               arrangements: vec![
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
    let Errors = Relation {
                     name:         "Errors".to_string(),
                     input:        false,
                     distinct:     true,
                     caching_mode: CachingMode::Set,
                     key_func:     None,
                     id:           Relations::Errors as RelId,
                     rules:        vec![
                         /* Errors[(Errors{.message=message}: Errors)] :- __Prefix_0[((expr_id: bit<64>), (var_id: bit<64>), (rhs_id: bit<64>), (type_id: bit<64>))], TypedExpressions[(TypedExpressions{.expr_id=(rhs_id: bit<64>), .type_id=(rhs_ty: bit<64>)}: TypedExpressions)], (rhs_ty != type_id), (var message: string) = ((((((("The expressions " ++ (std::__builtin_2string(expr_id): string)) ++ " and ") ++ (std::__builtin_2string(rhs_id): string)) ++ " have unequal types: ") ++ (std::__builtin_2string(type_id): string)) ++ " != ") ++ (std::__builtin_2string(rhs_ty): string)). */
                         Rule::ArrangementRule {
                             description: "Errors[(Errors{.message=message}: Errors)] :- __Prefix_0[((expr_id: bit<64>), (var_id: bit<64>), (rhs_id: bit<64>), (type_id: bit<64>))], TypedExpressions[(TypedExpressions{.expr_id=(rhs_id: bit<64>), .type_id=(rhs_ty: bit<64>)}: TypedExpressions)], (rhs_ty != type_id), (var message: string) = (((((((\"The expressions \" ++ (std::__builtin_2string(expr_id): string)) ++ \" and \") ++ (std::__builtin_2string(rhs_id): string)) ++ \" have unequal types: \") ++ (std::__builtin_2string(type_id): string)) ++ \" != \") ++ (std::__builtin_2string(rhs_ty): string)).".to_string(),
                             arr: ( Relations::__Prefix_0 as RelId, 0),
                             xform: XFormArrangement::Join{
                                        description: "__Prefix_0[((expr_id: bit<64>), (var_id: bit<64>), (rhs_id: bit<64>), (type_id: bit<64>))], TypedExpressions[(TypedExpressions{.expr_id=(rhs_id: bit<64>), .type_id=(rhs_ty: bit<64>)}: TypedExpressions)]".to_string(),
                                        ffun: None,
                                        arrangement: (Relations::TypedExpressions as RelId,0),
                                        jfun: &{fn __f(_: &DDValue ,__v1: &DDValue,__v2: &DDValue) -> Option<DDValue>
                                        {
                                            let (ref expr_id, ref var_id, ref rhs_id, ref type_id) = match unsafe {  Value::__Tuple4____Bitval64___Bitval64___Bitval64___Bitval64::from_ddvalue_ref(__v1) }.0 {
                                                (ref expr_id, ref var_id, ref rhs_id, ref type_id) => ((*expr_id).clone(), (*var_id).clone(), (*rhs_id).clone(), (*type_id).clone()),
                                                _ => return None
                                            };
                                            let ref rhs_ty = match unsafe {  Value::TypedExpressions::from_ddvalue_ref(__v2) }.0 {
                                                TypedExpressions{expr_id: _, type_id: ref rhs_ty} => (*rhs_ty).clone(),
                                                _ => return None
                                            };
                                            if !((&*rhs_ty) != (&*type_id)) {return None;};
                                            let ref message: String = match string_append(string_append_str(string_append(string_append_str(string_append(string_append_str(string_append(String::from(r###"The expressions "###), (&std___builtin_2string(expr_id))), r###" and "###), (&std___builtin_2string(rhs_id))), r###" have unequal types: "###), (&std___builtin_2string(type_id))), r###" != "###), (&std___builtin_2string(rhs_ty))) {
                                                message => message,
                                                _ => return None
                                            };
                                            Some(Value::Errors((Errors{message: (*message).clone()})).into_ddvalue())
                                        }
                                        __f},
                                        next: Box::new(None)
                                    }
                         }],
                     arrangements: vec![
                         ],
                     change_cb:    Some(sync::Arc::new(sync::Mutex::new(__update_cb.clone())))
                 };
    let VariablesInScope = Relation {
                               name:         "VariablesInScope".to_string(),
                               input:        false,
                               distinct:     false,
                               caching_mode: CachingMode::Set,
                               key_func:     None,
                               id:           Relations::VariablesInScope as RelId,
                               rules:        vec![
                                   /* VariablesInScope[(VariablesInScope{.scope=scope, .var_id=var_id, .decl_scope=scope}: VariablesInScope)] :- Variables[(Variables{.var_id=(var_id: bit<64>), .variable=(hir::Variable{.var_name=(_: bit<32>), .var_type=(_: bit<64>), .value=(_: bit<64>), .scope=(scope: bit<32>)}: hir::Variable)}: Variables)]. */
                                   Rule::CollectionRule {
                                       description: "VariablesInScope[(VariablesInScope{.scope=scope, .var_id=var_id, .decl_scope=scope}: VariablesInScope)] :- Variables[(Variables{.var_id=(var_id: bit<64>), .variable=(hir::Variable{.var_name=(_: bit<32>), .var_type=(_: bit<64>), .value=(_: bit<64>), .scope=(scope: bit<32>)}: hir::Variable)}: Variables)].".to_string(),
                                       rel: Relations::Variables as RelId,
                                       xform: Some(XFormCollection::FilterMap{
                                                       description: "head of VariablesInScope[(VariablesInScope{.scope=scope, .var_id=var_id, .decl_scope=scope}: VariablesInScope)] :- Variables[(Variables{.var_id=(var_id: bit<64>), .variable=(hir::Variable{.var_name=(_: bit<32>), .var_type=(_: bit<64>), .value=(_: bit<64>), .scope=(scope: bit<32>)}: hir::Variable)}: Variables)]." .to_string(),
                                                       fmfun: &{fn __f(__v: DDValue) -> Option<DDValue>
                                                       {
                                                           let (ref var_id, ref scope) = match unsafe {  Value::Variables::from_ddvalue_ref(&__v) }.0 {
                                                               Variables{var_id: ref var_id, variable: hir_Variable{var_name: _, var_type: _, value: _, scope: ref scope}} => ((*var_id).clone(), (*scope).clone()),
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
            ProgNode::Rel { rel: Expressions },
            ProgNode::Rel { rel: Functions },
            ProgNode::Rel { rel: Items },
            ProgNode::Rel { rel: Statements },
            ProgNode::Rel { rel: Types },
            ProgNode::Rel { rel: Literals },
            ProgNode::Rel {
                rel: VariableScopes,
            },
            ProgNode::Rel { rel: Variables },
            ProgNode::Rel { rel: __Prefix_0 },
            ProgNode::SCC {
                rels: vec![RecursiveRelation {
                    rel: TypedExpressions,
                    distinct: true,
                }],
            },
            ProgNode::Rel { rel: Errors },
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
