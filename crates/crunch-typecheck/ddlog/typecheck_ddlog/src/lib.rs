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
    clippy::missing_safety_doc
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
    let Expr = Relation {
                   name:         "Expr".to_string(),
                   input:        true,
                   distinct:     false,
                   caching_mode: CachingMode::Set,
                   key_func:     None,
                   id:           Relations::Expr as RelId,
                   rules:        vec![
                       ],
                   arrangements: vec![
                       ],
                   change_cb:    None
               };
    let PropagateExprType = Relation {
                                name:         "PropagateExprType".to_string(),
                                input:        false,
                                distinct:     false,
                                caching_mode: CachingMode::Set,
                                key_func:     None,
                                id:           Relations::PropagateExprType as RelId,
                                rules:        vec![
                                    /* PropagateExprType[(PropagateExprType{.id=id, .kind=kind, .ty=ty}: PropagateExprType)] :- Expr[(Expr{.id=(id: bit<64>), .kind=(kind: internment::Intern<ExprKind>), .ty=(_: internment::Intern<TypeKind>)}: Expr)], (Lit{.lit=(var lit: internment::Intern<LiteralVal>)}: ExprKind) = (internment::ival(kind): ExprKind), (var ty: internment::Intern<TypeKind>) = typeof_literal(lit). */
                                    Rule::CollectionRule {
                                        description: "PropagateExprType[(PropagateExprType{.id=id, .kind=kind, .ty=ty}: PropagateExprType)] :- Expr[(Expr{.id=(id: bit<64>), .kind=(kind: internment::Intern<ExprKind>), .ty=(_: internment::Intern<TypeKind>)}: Expr)], (Lit{.lit=(var lit: internment::Intern<LiteralVal>)}: ExprKind) = (internment::ival(kind): ExprKind), (var ty: internment::Intern<TypeKind>) = typeof_literal(lit).".to_string(),
                                        rel: Relations::Expr as RelId,
                                        xform: Some(XFormCollection::FilterMap{
                                                        description: "head of PropagateExprType[(PropagateExprType{.id=id, .kind=kind, .ty=ty}: PropagateExprType)] :- Expr[(Expr{.id=(id: bit<64>), .kind=(kind: internment::Intern<ExprKind>), .ty=(_: internment::Intern<TypeKind>)}: Expr)], (Lit{.lit=(var lit: internment::Intern<LiteralVal>)}: ExprKind) = (internment::ival(kind): ExprKind), (var ty: internment::Intern<TypeKind>) = typeof_literal(lit)." .to_string(),
                                                        fmfun: &{fn __f(__v: DDValue) -> Option<DDValue>
                                                        {
                                                            let (ref id, ref kind) = match unsafe {  Value::Expr::from_ddvalue_ref(&__v) }.0 {
                                                                Expr{id: ref id, kind: ref kind, ty: _} => ((*id).clone(), (*kind).clone()),
                                                                _ => return None
                                                            };
                                                            let ref lit: internment_Intern<LiteralVal> = match (*internment_ival(kind)).clone() {
                                                                ExprKind::Lit{lit: lit} => lit,
                                                                _ => return None
                                                            };
                                                            let ref ty: internment_Intern<TypeKind> = match typeof_literal(lit) {
                                                                ty => ty,
                                                                _ => return None
                                                            };
                                                            Some(Value::PropagateExprType((PropagateExprType{id: (*id).clone(), kind: (*kind).clone(), ty: (*ty).clone()})).into_ddvalue())
                                                        }
                                                        __f},
                                                        next: Box::new(None)
                                                    })
                                    },
                                    /* PropagateExprType[(PropagateExprType{.id=id, .kind=kind, .ty=ty}: PropagateExprType)] :- Expr[(Expr{.id=(id: bit<64>), .kind=(kind: internment::Intern<ExprKind>), .ty=(_: internment::Intern<TypeKind>)}: Expr)], (Assign{.variable=(_: bit<64>), .expr_id=(var expr_id: bit<64>)}: ExprKind) = (internment::ival(kind): ExprKind), PropagateExprType[(PropagateExprType{.id=(expr_id: bit<64>), .kind=(_: internment::Intern<ExprKind>), .ty=(ty: internment::Intern<TypeKind>)}: PropagateExprType)]. */
                                    Rule::CollectionRule {
                                        description: "PropagateExprType[(PropagateExprType{.id=id, .kind=kind, .ty=ty}: PropagateExprType)] :- Expr[(Expr{.id=(id: bit<64>), .kind=(kind: internment::Intern<ExprKind>), .ty=(_: internment::Intern<TypeKind>)}: Expr)], (Assign{.variable=(_: bit<64>), .expr_id=(var expr_id: bit<64>)}: ExprKind) = (internment::ival(kind): ExprKind), PropagateExprType[(PropagateExprType{.id=(expr_id: bit<64>), .kind=(_: internment::Intern<ExprKind>), .ty=(ty: internment::Intern<TypeKind>)}: PropagateExprType)].".to_string(),
                                        rel: Relations::Expr as RelId,
                                        xform: Some(XFormCollection::Arrange {
                                                        description: "arrange Expr[(Expr{.id=(id: bit<64>), .kind=(kind: internment::Intern<ExprKind>), .ty=(_: internment::Intern<TypeKind>)}: Expr)] by (expr_id)" .to_string(),
                                                        afun: &{fn __f(__v: DDValue) -> Option<(DDValue,DDValue)>
                                                        {
                                                            let (ref id, ref kind) = match unsafe {  Value::Expr::from_ddvalue_ref(&__v) }.0 {
                                                                Expr{id: ref id, kind: ref kind, ty: _} => ((*id).clone(), (*kind).clone()),
                                                                _ => return None
                                                            };
                                                            let ref expr_id: u64 = match (*internment_ival(kind)).clone() {
                                                                ExprKind::Assign{variable: _, expr_id: expr_id} => expr_id,
                                                                _ => return None
                                                            };
                                                            Some((Value::__Bitval64((*expr_id).clone()).into_ddvalue(), Value::__Tuple2____Bitval64_internment_Intern__ExprKind(((*id).clone(), (*kind).clone())).into_ddvalue()))
                                                        }
                                                        __f},
                                                        next: Box::new(XFormArrangement::Join{
                                                                           description: "Expr[(Expr{.id=(id: bit<64>), .kind=(kind: internment::Intern<ExprKind>), .ty=(_: internment::Intern<TypeKind>)}: Expr)], (Assign{.variable=(_: bit<64>), .expr_id=(var expr_id: bit<64>)}: ExprKind) = (internment::ival(kind): ExprKind), PropagateExprType[(PropagateExprType{.id=(expr_id: bit<64>), .kind=(_: internment::Intern<ExprKind>), .ty=(ty: internment::Intern<TypeKind>)}: PropagateExprType)]".to_string(),
                                                                           ffun: None,
                                                                           arrangement: (Relations::PropagateExprType as RelId,0),
                                                                           jfun: &{fn __f(_: &DDValue ,__v1: &DDValue,__v2: &DDValue) -> Option<DDValue>
                                                                           {
                                                                               let (ref id, ref kind) = unsafe { Value::__Tuple2____Bitval64_internment_Intern__ExprKind::from_ddvalue_ref( __v1 ) }.0;
                                                                               let ref ty = match unsafe {  Value::PropagateExprType::from_ddvalue_ref(__v2) }.0 {
                                                                                   PropagateExprType{id: _, kind: _, ty: ref ty} => (*ty).clone(),
                                                                                   _ => return None
                                                                               };
                                                                               Some(Value::PropagateExprType((PropagateExprType{id: (*id).clone(), kind: (*kind).clone(), ty: (*ty).clone()})).into_ddvalue())
                                                                           }
                                                                           __f},
                                                                           next: Box::new(None)
                                                                       })
                                                    })
                                    },
                                    /* PropagateExprType[(PropagateExprType{.id=id, .kind=kind, .ty=ty}: PropagateExprType)] :- Expr[(Expr{.id=(id: bit<64>), .kind=(kind: internment::Intern<ExprKind>), .ty=(_: internment::Intern<TypeKind>)}: Expr)], (Variable{.variable=(var expr_var: bit<64>)}: ExprKind) = (internment::ival(kind): ExprKind), PropagateExprType[(PropagateExprType{.id=(_: bit<64>), .kind=(prop_kind: internment::Intern<ExprKind>), .ty=(ty: internment::Intern<TypeKind>)}: PropagateExprType)], (Assign{.variable=(var prop_var: bit<64>), .expr_id=(_: bit<64>)}: ExprKind) = (internment::ival(prop_kind): ExprKind), (expr_var == prop_var). */
                                    Rule::CollectionRule {
                                        description: "PropagateExprType[(PropagateExprType{.id=id, .kind=kind, .ty=ty}: PropagateExprType)] :- Expr[(Expr{.id=(id: bit<64>), .kind=(kind: internment::Intern<ExprKind>), .ty=(_: internment::Intern<TypeKind>)}: Expr)], (Variable{.variable=(var expr_var: bit<64>)}: ExprKind) = (internment::ival(kind): ExprKind), PropagateExprType[(PropagateExprType{.id=(_: bit<64>), .kind=(prop_kind: internment::Intern<ExprKind>), .ty=(ty: internment::Intern<TypeKind>)}: PropagateExprType)], (Assign{.variable=(var prop_var: bit<64>), .expr_id=(_: bit<64>)}: ExprKind) = (internment::ival(prop_kind): ExprKind), (expr_var == prop_var).".to_string(),
                                        rel: Relations::Expr as RelId,
                                        xform: Some(XFormCollection::Arrange {
                                                        description: "arrange Expr[(Expr{.id=(id: bit<64>), .kind=(kind: internment::Intern<ExprKind>), .ty=(_: internment::Intern<TypeKind>)}: Expr)] by ()" .to_string(),
                                                        afun: &{fn __f(__v: DDValue) -> Option<(DDValue,DDValue)>
                                                        {
                                                            let (ref id, ref kind) = match unsafe {  Value::Expr::from_ddvalue_ref(&__v) }.0 {
                                                                Expr{id: ref id, kind: ref kind, ty: _} => ((*id).clone(), (*kind).clone()),
                                                                _ => return None
                                                            };
                                                            let ref expr_var: u64 = match (*internment_ival(kind)).clone() {
                                                                ExprKind::Variable{variable: expr_var} => expr_var,
                                                                _ => return None
                                                            };
                                                            Some((Value::__Tuple0__(()).into_ddvalue(), Value::__Tuple3____Bitval64_internment_Intern__ExprKind___Bitval64(((*id).clone(), (*kind).clone(), (*expr_var).clone())).into_ddvalue()))
                                                        }
                                                        __f},
                                                        next: Box::new(XFormArrangement::Join{
                                                                           description: "Expr[(Expr{.id=(id: bit<64>), .kind=(kind: internment::Intern<ExprKind>), .ty=(_: internment::Intern<TypeKind>)}: Expr)], (Variable{.variable=(var expr_var: bit<64>)}: ExprKind) = (internment::ival(kind): ExprKind), PropagateExprType[(PropagateExprType{.id=(_: bit<64>), .kind=(prop_kind: internment::Intern<ExprKind>), .ty=(ty: internment::Intern<TypeKind>)}: PropagateExprType)]".to_string(),
                                                                           ffun: None,
                                                                           arrangement: (Relations::PropagateExprType as RelId,1),
                                                                           jfun: &{fn __f(_: &DDValue ,__v1: &DDValue,__v2: &DDValue) -> Option<DDValue>
                                                                           {
                                                                               let (ref id, ref kind, ref expr_var) = unsafe { Value::__Tuple3____Bitval64_internment_Intern__ExprKind___Bitval64::from_ddvalue_ref( __v1 ) }.0;
                                                                               let (ref prop_kind, ref ty) = match unsafe {  Value::PropagateExprType::from_ddvalue_ref(__v2) }.0 {
                                                                                   PropagateExprType{id: _, kind: ref prop_kind, ty: ref ty} => ((*prop_kind).clone(), (*ty).clone()),
                                                                                   _ => return None
                                                                               };
                                                                               let ref prop_var: u64 = match (*internment_ival(prop_kind)).clone() {
                                                                                   ExprKind::Assign{variable: prop_var, expr_id: _} => prop_var,
                                                                                   _ => return None
                                                                               };
                                                                               if !((&*expr_var) == (&*prop_var)) {return None;};
                                                                               Some(Value::PropagateExprType((PropagateExprType{id: (*id).clone(), kind: (*kind).clone(), ty: (*ty).clone()})).into_ddvalue())
                                                                           }
                                                                           __f},
                                                                           next: Box::new(None)
                                                                       })
                                                    })
                                    }],
                                arrangements: vec![
                                    Arrangement::Map{
                                       name: r###"(PropagateExprType{.id=(_0: bit<64>), .kind=(_: internment::Intern<ExprKind>), .ty=(_: internment::Intern<TypeKind>)}: PropagateExprType) /*join*/"###.to_string(),
                                        afun: &{fn __f(__v: DDValue) -> Option<(DDValue,DDValue)>
                                        {
                                            let __cloned = __v.clone();
                                            match unsafe { Value::PropagateExprType::from_ddvalue(__v) }.0 {
                                                PropagateExprType{id: ref _0, kind: _, ty: _} => Some(Value::__Bitval64((*_0).clone()).into_ddvalue()),
                                                _ => None
                                            }.map(|x|(x,__cloned))
                                        }
                                        __f},
                                        queryable: false
                                    },
                                    Arrangement::Map{
                                       name: r###"(PropagateExprType{.id=(_: bit<64>), .kind=(_: internment::Intern<ExprKind>), .ty=(_: internment::Intern<TypeKind>)}: PropagateExprType) /*join*/"###.to_string(),
                                        afun: &{fn __f(__v: DDValue) -> Option<(DDValue,DDValue)>
                                        {
                                            let __cloned = __v.clone();
                                            match unsafe { Value::PropagateExprType::from_ddvalue(__v) }.0 {
                                                PropagateExprType{id: _, kind: _, ty: _} => Some(Value::__Tuple0__(()).into_ddvalue()),
                                                _ => None
                                            }.map(|x|(x,__cloned))
                                        }
                                        __f},
                                        queryable: false
                                    }],
                                change_cb:    None
                            };
    let ClampUnknownInt = Relation {
                              name:         "ClampUnknownInt".to_string(),
                              input:        false,
                              distinct:     true,
                              caching_mode: CachingMode::Set,
                              key_func:     None,
                              id:           Relations::ClampUnknownInt as RelId,
                              rules:        vec![
                                  /* ClampUnknownInt[(ClampUnknownInt{.id=id, .kind=kind, .ty=ty}: ClampUnknownInt)] :- PropagateExprType[(PropagateExprType{.id=(id: bit<64>), .kind=(kind: internment::Intern<ExprKind>), .ty=(ty: internment::Intern<TypeKind>)}: PropagateExprType)], (Int{.is_signed=(std::None{}: std::Option<bool>), .width=(std::None{}: std::Option<std::u16>)}: TypeKind) = (internment::ival(ty): TypeKind), (var ty: internment::Intern<TypeKind>) = (internment::intern((Int{.is_signed=(std::Some{.x=true}: std::Option<bool>), .width=(std::Some{.x=16'd32}: std::Option<std::u16>)}: TypeKind)): internment::Intern<TypeKind>). */
                                  Rule::CollectionRule {
                                      description: "ClampUnknownInt[(ClampUnknownInt{.id=id, .kind=kind, .ty=ty}: ClampUnknownInt)] :- PropagateExprType[(PropagateExprType{.id=(id: bit<64>), .kind=(kind: internment::Intern<ExprKind>), .ty=(ty: internment::Intern<TypeKind>)}: PropagateExprType)], (Int{.is_signed=(std::None{}: std::Option<bool>), .width=(std::None{}: std::Option<std::u16>)}: TypeKind) = (internment::ival(ty): TypeKind), (var ty: internment::Intern<TypeKind>) = (internment::intern((Int{.is_signed=(std::Some{.x=true}: std::Option<bool>), .width=(std::Some{.x=16'd32}: std::Option<std::u16>)}: TypeKind)): internment::Intern<TypeKind>).".to_string(),
                                      rel: Relations::PropagateExprType as RelId,
                                      xform: Some(XFormCollection::FilterMap{
                                                      description: "head of ClampUnknownInt[(ClampUnknownInt{.id=id, .kind=kind, .ty=ty}: ClampUnknownInt)] :- PropagateExprType[(PropagateExprType{.id=(id: bit<64>), .kind=(kind: internment::Intern<ExprKind>), .ty=(ty: internment::Intern<TypeKind>)}: PropagateExprType)], (Int{.is_signed=(std::None{}: std::Option<bool>), .width=(std::None{}: std::Option<std::u16>)}: TypeKind) = (internment::ival(ty): TypeKind), (var ty: internment::Intern<TypeKind>) = (internment::intern((Int{.is_signed=(std::Some{.x=true}: std::Option<bool>), .width=(std::Some{.x=16'd32}: std::Option<std::u16>)}: TypeKind)): internment::Intern<TypeKind>)." .to_string(),
                                                      fmfun: &{fn __f(__v: DDValue) -> Option<DDValue>
                                                      {
                                                          let (ref id, ref kind, ref ty) = match unsafe {  Value::PropagateExprType::from_ddvalue_ref(&__v) }.0 {
                                                              PropagateExprType{id: ref id, kind: ref kind, ty: ref ty} => ((*id).clone(), (*kind).clone(), (*ty).clone()),
                                                              _ => return None
                                                          };
                                                          let (): () = match (*internment_ival(ty)).clone() {
                                                              TypeKind::Int{is_signed: std_Option::std_None{}, width: std_Option::std_None{}} => (),
                                                              _ => return None
                                                          };
                                                          let ref ty: internment_Intern<TypeKind> = match (*(&*__STATIC_6)).clone() {
                                                              ty => ty,
                                                              _ => return None
                                                          };
                                                          Some(Value::ClampUnknownInt((ClampUnknownInt{id: (*id).clone(), kind: (*kind).clone(), ty: (*ty).clone()})).into_ddvalue())
                                                      }
                                                      __f},
                                                      next: Box::new(None)
                                                  })
                                  },
                                  /* ClampUnknownInt[(ClampUnknownInt{.id=id, .kind=kind, .ty=ty}: ClampUnknownInt)] :- PropagateExprType[(PropagateExprType{.id=(id: bit<64>), .kind=(kind: internment::Intern<ExprKind>), .ty=(ty: internment::Intern<TypeKind>)}: PropagateExprType)], (Int{.is_signed=(std::Some{.x=(_: bool)}: std::Option<bool>), .width=(std::Some{.x=(_: bit<16>)}: std::Option<std::u16>)}: TypeKind) = (internment::ival(ty): TypeKind). */
                                  Rule::CollectionRule {
                                      description: "ClampUnknownInt[(ClampUnknownInt{.id=id, .kind=kind, .ty=ty}: ClampUnknownInt)] :- PropagateExprType[(PropagateExprType{.id=(id: bit<64>), .kind=(kind: internment::Intern<ExprKind>), .ty=(ty: internment::Intern<TypeKind>)}: PropagateExprType)], (Int{.is_signed=(std::Some{.x=(_: bool)}: std::Option<bool>), .width=(std::Some{.x=(_: bit<16>)}: std::Option<std::u16>)}: TypeKind) = (internment::ival(ty): TypeKind).".to_string(),
                                      rel: Relations::PropagateExprType as RelId,
                                      xform: Some(XFormCollection::FilterMap{
                                                      description: "head of ClampUnknownInt[(ClampUnknownInt{.id=id, .kind=kind, .ty=ty}: ClampUnknownInt)] :- PropagateExprType[(PropagateExprType{.id=(id: bit<64>), .kind=(kind: internment::Intern<ExprKind>), .ty=(ty: internment::Intern<TypeKind>)}: PropagateExprType)], (Int{.is_signed=(std::Some{.x=(_: bool)}: std::Option<bool>), .width=(std::Some{.x=(_: bit<16>)}: std::Option<std::u16>)}: TypeKind) = (internment::ival(ty): TypeKind)." .to_string(),
                                                      fmfun: &{fn __f(__v: DDValue) -> Option<DDValue>
                                                      {
                                                          let (ref id, ref kind, ref ty) = match unsafe {  Value::PropagateExprType::from_ddvalue_ref(&__v) }.0 {
                                                              PropagateExprType{id: ref id, kind: ref kind, ty: ref ty} => ((*id).clone(), (*kind).clone(), (*ty).clone()),
                                                              _ => return None
                                                          };
                                                          let (): () = match (*internment_ival(ty)).clone() {
                                                              TypeKind::Int{is_signed: std_Option::std_Some{x: _}, width: std_Option::std_Some{x: _}} => (),
                                                              _ => return None
                                                          };
                                                          Some(Value::ClampUnknownInt((ClampUnknownInt{id: (*id).clone(), kind: (*kind).clone(), ty: (*ty).clone()})).into_ddvalue())
                                                      }
                                                      __f},
                                                      next: Box::new(None)
                                                  })
                                  },
                                  /* ClampUnknownInt[(ClampUnknownInt{.id=id, .kind=kind, .ty=ty}: ClampUnknownInt)] :- PropagateExprType[(PropagateExprType{.id=(id: bit<64>), .kind=(kind: internment::Intern<ExprKind>), .ty=(ty: internment::Intern<TypeKind>)}: PropagateExprType)], (not is_int(ty)). */
                                  Rule::CollectionRule {
                                      description: "ClampUnknownInt[(ClampUnknownInt{.id=id, .kind=kind, .ty=ty}: ClampUnknownInt)] :- PropagateExprType[(PropagateExprType{.id=(id: bit<64>), .kind=(kind: internment::Intern<ExprKind>), .ty=(ty: internment::Intern<TypeKind>)}: PropagateExprType)], (not is_int(ty)).".to_string(),
                                      rel: Relations::PropagateExprType as RelId,
                                      xform: Some(XFormCollection::FilterMap{
                                                      description: "head of ClampUnknownInt[(ClampUnknownInt{.id=id, .kind=kind, .ty=ty}: ClampUnknownInt)] :- PropagateExprType[(PropagateExprType{.id=(id: bit<64>), .kind=(kind: internment::Intern<ExprKind>), .ty=(ty: internment::Intern<TypeKind>)}: PropagateExprType)], (not is_int(ty))." .to_string(),
                                                      fmfun: &{fn __f(__v: DDValue) -> Option<DDValue>
                                                      {
                                                          let (ref id, ref kind, ref ty) = match unsafe {  Value::PropagateExprType::from_ddvalue_ref(&__v) }.0 {
                                                              PropagateExprType{id: ref id, kind: ref kind, ty: ref ty} => ((*id).clone(), (*kind).clone(), (*ty).clone()),
                                                              _ => return None
                                                          };
                                                          if !(!is_int(ty)) {return None;};
                                                          Some(Value::ClampUnknownInt((ClampUnknownInt{id: (*id).clone(), kind: (*kind).clone(), ty: (*ty).clone()})).into_ddvalue())
                                                      }
                                                      __f},
                                                      next: Box::new(None)
                                                  })
                                  }],
                              arrangements: vec![
                                  ],
                              change_cb:    Some(sync::Arc::new(sync::Mutex::new(__update_cb.clone())))
                          };
    let InputItems = Relation {
                         name:         "InputItems".to_string(),
                         input:        true,
                         distinct:     false,
                         caching_mode: CachingMode::Set,
                         key_func:     None,
                         id:           Relations::InputItems as RelId,
                         rules:        vec![
                             ],
                         arrangements: vec![
                             ],
                         change_cb:    None
                     };
    let __MultiHead_6 = Relation {
                            name:         "__MultiHead_6".to_string(),
                            input:        false,
                            distinct:     false,
                            caching_mode: CachingMode::Set,
                            key_func:     None,
                            id:           Relations::__MultiHead_6 as RelId,
                            rules:        vec![
                                /* __MultiHead_6[((path: internment::Intern<std::Vec<StrT>>), (signature: Signature), (func: Function))] :- InputItems[(InputItems{.item=(item@ (ItemFunc{.func=(func: Function)}: Item))}: InputItems)], (not has_unknown_types(item)), (var path: internment::Intern<std::Vec<StrT>>) = path(item), (var signature: Signature) = signature(item). */
                                Rule::CollectionRule {
                                    description: "__MultiHead_6[((path: internment::Intern<std::Vec<StrT>>), (signature: Signature), (func: Function))] :- InputItems[(InputItems{.item=(item@ (ItemFunc{.func=(func: Function)}: Item))}: InputItems)], (not has_unknown_types(item)), (var path: internment::Intern<std::Vec<StrT>>) = path(item), (var signature: Signature) = signature(item).".to_string(),
                                    rel: Relations::InputItems as RelId,
                                    xform: Some(XFormCollection::FilterMap{
                                                    description: "head of __MultiHead_6[((path: internment::Intern<std::Vec<StrT>>), (signature: Signature), (func: Function))] :- InputItems[(InputItems{.item=(item@ (ItemFunc{.func=(func: Function)}: Item))}: InputItems)], (not has_unknown_types(item)), (var path: internment::Intern<std::Vec<StrT>>) = path(item), (var signature: Signature) = signature(item)." .to_string(),
                                                    fmfun: &{fn __f(__v: DDValue) -> Option<DDValue>
                                                    {
                                                        let (ref item, ref func) = match unsafe {  Value::InputItems::from_ddvalue_ref(&__v) }.0 {
                                                            InputItems{item: ref item} => match item {
                                                                                              Item::ItemFunc{func: ref func} => ((*item).clone(), (*func).clone()),
                                                                                              _ => return None
                                                                                          },
                                                            _ => return None
                                                        };
                                                        if !(!has_unknown_types(item)) {return None;};
                                                        let ref path: internment_Intern<std_Vec<StrT>> = match path(item) {
                                                            path => path,
                                                            _ => return None
                                                        };
                                                        let ref signature: Signature = match signature(item) {
                                                            signature => signature,
                                                            _ => return None
                                                        };
                                                        Some(Value::__Tuple3__internment_Intern__std_Vec____Bitval32_Signature_Function(((*path).clone(), (*signature).clone(), (*func).clone())).into_ddvalue())
                                                    }
                                                    __f},
                                                    next: Box::new(None)
                                                })
                                }],
                            arrangements: vec![
                                ],
                            change_cb:    None
                        };
    let __MultiHead_7 = Relation {
                            name:         "__MultiHead_7".to_string(),
                            input:        false,
                            distinct:     false,
                            caching_mode: CachingMode::Set,
                            key_func:     None,
                            id:           Relations::__MultiHead_7 as RelId,
                            rules:        vec![
                                /* __MultiHead_7[((path: internment::Intern<std::Vec<StrT>>), (signature: Signature), (message: string), (func: Function))] :- InputItems[(InputItems{.item=(item@ (ItemFunc{.func=(func: Function)}: Item))}: InputItems)], has_unknown_types(item), var message = FlatMap(unknown_type_errors(item)), (var path: internment::Intern<std::Vec<StrT>>) = path(item), (var signature: Signature) = corrected_signature(item). */
                                Rule::CollectionRule {
                                    description: "__MultiHead_7[((path: internment::Intern<std::Vec<StrT>>), (signature: Signature), (message: string), (func: Function))] :- InputItems[(InputItems{.item=(item@ (ItemFunc{.func=(func: Function)}: Item))}: InputItems)], has_unknown_types(item), var message = FlatMap(unknown_type_errors(item)), (var path: internment::Intern<std::Vec<StrT>>) = path(item), (var signature: Signature) = corrected_signature(item).".to_string(),
                                    rel: Relations::InputItems as RelId,
                                    xform: Some(XFormCollection::FlatMap{
                                                    description: "InputItems[(InputItems{.item=(item@ (ItemFunc{.func=(func: Function)}: Item))}: InputItems)], has_unknown_types(item), var message = FlatMap(unknown_type_errors(item))" .to_string(),
                                                    fmfun: &{fn __f(__v: DDValue) -> Option<Box<dyn Iterator<Item=DDValue>>>
                                                    {
                                                        let (ref item, ref func) = match unsafe {  Value::InputItems::from_ddvalue_ref(&__v) }.0 {
                                                            InputItems{item: ref item} => match item {
                                                                                              Item::ItemFunc{func: ref func} => ((*item).clone(), (*func).clone()),
                                                                                              _ => return None
                                                                                          },
                                                            _ => return None
                                                        };
                                                        if !has_unknown_types(item) {return None;};
                                                        let __flattened = unknown_type_errors(item);
                                                        let item = (*item).clone();
                                                        let func = (*func).clone();
                                                        Some(Box::new(__flattened.into_iter().map(move |message|Value::__Tuple3____Stringval_Item_Function((message.clone(), item.clone(), func.clone())).into_ddvalue())))
                                                    }
                                                    __f},
                                                    next: Box::new(Some(XFormCollection::FilterMap{
                                                                            description: "head of __MultiHead_7[((path: internment::Intern<std::Vec<StrT>>), (signature: Signature), (message: string), (func: Function))] :- InputItems[(InputItems{.item=(item@ (ItemFunc{.func=(func: Function)}: Item))}: InputItems)], has_unknown_types(item), var message = FlatMap(unknown_type_errors(item)), (var path: internment::Intern<std::Vec<StrT>>) = path(item), (var signature: Signature) = corrected_signature(item)." .to_string(),
                                                                            fmfun: &{fn __f(__v: DDValue) -> Option<DDValue>
                                                                            {
                                                                                let (ref message, ref item, ref func) = unsafe { Value::__Tuple3____Stringval_Item_Function::from_ddvalue_ref( &__v ) }.0;
                                                                                let ref path: internment_Intern<std_Vec<StrT>> = match path(item) {
                                                                                    path => path,
                                                                                    _ => return None
                                                                                };
                                                                                let ref signature: Signature = match corrected_signature(item) {
                                                                                    signature => signature,
                                                                                    _ => return None
                                                                                };
                                                                                Some(Value::__Tuple4__internment_Intern__std_Vec____Bitval32_Signature___Stringval_Function(((*path).clone(), (*signature).clone(), (*message).clone(), (*func).clone())).into_ddvalue())
                                                                            }
                                                                            __f},
                                                                            next: Box::new(None)
                                                                        }))
                                                })
                                }],
                            arrangements: vec![
                                ],
                            change_cb:    None
                        };
    let Errors = Relation {
                     name:         "Errors".to_string(),
                     input:        false,
                     distinct:     true,
                     caching_mode: CachingMode::Set,
                     key_func:     None,
                     id:           Relations::Errors as RelId,
                     rules:        vec![
                         /* Errors[(Errors{.message=message}: Errors)] :- __MultiHead_7[((path: internment::Intern<std::Vec<StrT>>), (signature: Signature), (message: string), (func: Function))]. */
                         Rule::CollectionRule {
                             description: "Errors[(Errors{.message=message}: Errors)] :- __MultiHead_7[((path: internment::Intern<std::Vec<StrT>>), (signature: Signature), (message: string), (func: Function))].".to_string(),
                             rel: Relations::__MultiHead_7 as RelId,
                             xform: Some(XFormCollection::FilterMap{
                                             description: "head of Errors[(Errors{.message=message}: Errors)] :- __MultiHead_7[((path: internment::Intern<std::Vec<StrT>>), (signature: Signature), (message: string), (func: Function))]." .to_string(),
                                             fmfun: &{fn __f(__v: DDValue) -> Option<DDValue>
                                             {
                                                 let (ref path, ref signature, ref message, ref func) = match unsafe {  Value::__Tuple4__internment_Intern__std_Vec____Bitval32_Signature___Stringval_Function::from_ddvalue_ref(&__v) }.0 {
                                                     (ref path, ref signature, ref message, ref func) => ((*path).clone(), (*signature).clone(), (*message).clone(), (*func).clone()),
                                                     _ => return None
                                                 };
                                                 Some(Value::Errors((Errors{message: (*message).clone()})).into_ddvalue())
                                             }
                                             __f},
                                             next: Box::new(None)
                                         })
                         }],
                     arrangements: vec![
                         ],
                     change_cb:    Some(sync::Arc::new(sync::Mutex::new(__update_cb.clone())))
                 };
    let Functions = Relation {
                        name:         "Functions".to_string(),
                        input:        false,
                        distinct:     true,
                        caching_mode: CachingMode::Set,
                        key_func:     None,
                        id:           Relations::Functions as RelId,
                        rules:        vec![
                            /* Functions[(Functions{.func=(internment::intern(func): internment::Intern<Function>)}: Functions)] :- __MultiHead_6[((path: internment::Intern<std::Vec<StrT>>), (signature: Signature), (func: Function))]. */
                            Rule::CollectionRule {
                                description: "Functions[(Functions{.func=(internment::intern(func): internment::Intern<Function>)}: Functions)] :- __MultiHead_6[((path: internment::Intern<std::Vec<StrT>>), (signature: Signature), (func: Function))].".to_string(),
                                rel: Relations::__MultiHead_6 as RelId,
                                xform: Some(XFormCollection::FilterMap{
                                                description: "head of Functions[(Functions{.func=(internment::intern(func): internment::Intern<Function>)}: Functions)] :- __MultiHead_6[((path: internment::Intern<std::Vec<StrT>>), (signature: Signature), (func: Function))]." .to_string(),
                                                fmfun: &{fn __f(__v: DDValue) -> Option<DDValue>
                                                {
                                                    let (ref path, ref signature, ref func) = match unsafe {  Value::__Tuple3__internment_Intern__std_Vec____Bitval32_Signature_Function::from_ddvalue_ref(&__v) }.0 {
                                                        (ref path, ref signature, ref func) => ((*path).clone(), (*signature).clone(), (*func).clone()),
                                                        _ => return None
                                                    };
                                                    Some(Value::Functions((Functions{func: internment_intern(func)})).into_ddvalue())
                                                }
                                                __f},
                                                next: Box::new(None)
                                            })
                            },
                            /* Functions[(Functions{.func=(internment::intern(func): internment::Intern<Function>)}: Functions)] :- __MultiHead_7[((path: internment::Intern<std::Vec<StrT>>), (signature: Signature), (message: string), (func: Function))]. */
                            Rule::CollectionRule {
                                description: "Functions[(Functions{.func=(internment::intern(func): internment::Intern<Function>)}: Functions)] :- __MultiHead_7[((path: internment::Intern<std::Vec<StrT>>), (signature: Signature), (message: string), (func: Function))].".to_string(),
                                rel: Relations::__MultiHead_7 as RelId,
                                xform: Some(XFormCollection::FilterMap{
                                                description: "head of Functions[(Functions{.func=(internment::intern(func): internment::Intern<Function>)}: Functions)] :- __MultiHead_7[((path: internment::Intern<std::Vec<StrT>>), (signature: Signature), (message: string), (func: Function))]." .to_string(),
                                                fmfun: &{fn __f(__v: DDValue) -> Option<DDValue>
                                                {
                                                    let (ref path, ref signature, ref message, ref func) = match unsafe {  Value::__Tuple4__internment_Intern__std_Vec____Bitval32_Signature___Stringval_Function::from_ddvalue_ref(&__v) }.0 {
                                                        (ref path, ref signature, ref message, ref func) => ((*path).clone(), (*signature).clone(), (*message).clone(), (*func).clone()),
                                                        _ => return None
                                                    };
                                                    Some(Value::Functions((Functions{func: internment_intern(func)})).into_ddvalue())
                                                }
                                                __f},
                                                next: Box::new(None)
                                            })
                            }],
                        arrangements: vec![
                            ],
                        change_cb:    Some(sync::Arc::new(sync::Mutex::new(__update_cb.clone())))
                    };
    let Statements = Relation {
                         name:         "Statements".to_string(),
                         input:        false,
                         distinct:     false,
                         caching_mode: CachingMode::Set,
                         key_func:     None,
                         id:           Relations::Statements as RelId,
                         rules:        vec![
                             /* Statements[(Statements{.stmt=stmt, .scope=(internment::intern((ScopeFunction{.func=func}: Scope)): internment::Intern<Scope>)}: Statements)] :- Functions[(Functions{.func=(func: internment::Intern<Function>)}: Functions)], (Function{.name=(_: internment::Intern<std::Vec<StrT>>), .vis=(_: Vis), .args=(_: std::Vec<FuncArg>), .body=(var stmt: internment::Intern<Stmt>), .ret=(_: internment::Intern<TypeKind>)}: Function) = (internment::ival(func): Function). */
                             Rule::CollectionRule {
                                 description: "Statements[(Statements{.stmt=stmt, .scope=(internment::intern((ScopeFunction{.func=func}: Scope)): internment::Intern<Scope>)}: Statements)] :- Functions[(Functions{.func=(func: internment::Intern<Function>)}: Functions)], (Function{.name=(_: internment::Intern<std::Vec<StrT>>), .vis=(_: Vis), .args=(_: std::Vec<FuncArg>), .body=(var stmt: internment::Intern<Stmt>), .ret=(_: internment::Intern<TypeKind>)}: Function) = (internment::ival(func): Function).".to_string(),
                                 rel: Relations::Functions as RelId,
                                 xform: Some(XFormCollection::FilterMap{
                                                 description: "head of Statements[(Statements{.stmt=stmt, .scope=(internment::intern((ScopeFunction{.func=func}: Scope)): internment::Intern<Scope>)}: Statements)] :- Functions[(Functions{.func=(func: internment::Intern<Function>)}: Functions)], (Function{.name=(_: internment::Intern<std::Vec<StrT>>), .vis=(_: Vis), .args=(_: std::Vec<FuncArg>), .body=(var stmt: internment::Intern<Stmt>), .ret=(_: internment::Intern<TypeKind>)}: Function) = (internment::ival(func): Function)." .to_string(),
                                                 fmfun: &{fn __f(__v: DDValue) -> Option<DDValue>
                                                 {
                                                     let ref func = match unsafe {  Value::Functions::from_ddvalue_ref(&__v) }.0 {
                                                         Functions{func: ref func} => (*func).clone(),
                                                         _ => return None
                                                     };
                                                     let ref stmt: internment_Intern<Stmt> = match (*internment_ival(func)).clone() {
                                                         Function{name: _, vis: _, args: _, body: stmt, ret: _} => stmt,
                                                         _ => return None
                                                     };
                                                     Some(Value::Statements((Statements{stmt: (*stmt).clone(), scope: internment_intern((&(Scope::ScopeFunction{func: (*func).clone()})))})).into_ddvalue())
                                                 }
                                                 __f},
                                                 next: Box::new(None)
                                             })
                             },
                             /* Statements[(Statements{.stmt=stmt1, .scope=(internment::intern((ScopeSeq1{.parent=parent}: Scope)): internment::Intern<Scope>)}: Statements)] :- __MultiHead_9[((parent: internment::Intern<Scope>), (stmt1: internment::Intern<Stmt>), (stmt2: internment::Intern<Stmt>))]. */
                             Rule::CollectionRule {
                                 description: "Statements[(Statements{.stmt=stmt1, .scope=(internment::intern((ScopeSeq1{.parent=parent}: Scope)): internment::Intern<Scope>)}: Statements)] :- __MultiHead_9[((parent: internment::Intern<Scope>), (stmt1: internment::Intern<Stmt>), (stmt2: internment::Intern<Stmt>))].".to_string(),
                                 rel: Relations::__MultiHead_9 as RelId,
                                 xform: Some(XFormCollection::FilterMap{
                                                 description: "head of Statements[(Statements{.stmt=stmt1, .scope=(internment::intern((ScopeSeq1{.parent=parent}: Scope)): internment::Intern<Scope>)}: Statements)] :- __MultiHead_9[((parent: internment::Intern<Scope>), (stmt1: internment::Intern<Stmt>), (stmt2: internment::Intern<Stmt>))]." .to_string(),
                                                 fmfun: &{fn __f(__v: DDValue) -> Option<DDValue>
                                                 {
                                                     let (ref parent, ref stmt1, ref stmt2) = match unsafe {  Value::__Tuple3__internment_Intern__Scope_internment_Intern__Stmt_internment_Intern__Stmt::from_ddvalue_ref(&__v) }.0 {
                                                         (ref parent, ref stmt1, ref stmt2) => ((*parent).clone(), (*stmt1).clone(), (*stmt2).clone()),
                                                         _ => return None
                                                     };
                                                     Some(Value::Statements((Statements{stmt: (*stmt1).clone(), scope: internment_intern((&(Scope::ScopeSeq1{parent: (*parent).clone()})))})).into_ddvalue())
                                                 }
                                                 __f},
                                                 next: Box::new(None)
                                             })
                             },
                             /* Statements[(Statements{.stmt=stmt2, .scope=(internment::intern((ScopeSeq2{.parent=parent}: Scope)): internment::Intern<Scope>)}: Statements)] :- __MultiHead_9[((parent: internment::Intern<Scope>), (stmt1: internment::Intern<Stmt>), (stmt2: internment::Intern<Stmt>))]. */
                             Rule::CollectionRule {
                                 description: "Statements[(Statements{.stmt=stmt2, .scope=(internment::intern((ScopeSeq2{.parent=parent}: Scope)): internment::Intern<Scope>)}: Statements)] :- __MultiHead_9[((parent: internment::Intern<Scope>), (stmt1: internment::Intern<Stmt>), (stmt2: internment::Intern<Stmt>))].".to_string(),
                                 rel: Relations::__MultiHead_9 as RelId,
                                 xform: Some(XFormCollection::FilterMap{
                                                 description: "head of Statements[(Statements{.stmt=stmt2, .scope=(internment::intern((ScopeSeq2{.parent=parent}: Scope)): internment::Intern<Scope>)}: Statements)] :- __MultiHead_9[((parent: internment::Intern<Scope>), (stmt1: internment::Intern<Stmt>), (stmt2: internment::Intern<Stmt>))]." .to_string(),
                                                 fmfun: &{fn __f(__v: DDValue) -> Option<DDValue>
                                                 {
                                                     let (ref parent, ref stmt1, ref stmt2) = match unsafe {  Value::__Tuple3__internment_Intern__Scope_internment_Intern__Stmt_internment_Intern__Stmt::from_ddvalue_ref(&__v) }.0 {
                                                         (ref parent, ref stmt1, ref stmt2) => ((*parent).clone(), (*stmt1).clone(), (*stmt2).clone()),
                                                         _ => return None
                                                     };
                                                     Some(Value::Statements((Statements{stmt: (*stmt2).clone(), scope: internment_intern((&(Scope::ScopeSeq2{parent: (*parent).clone()})))})).into_ddvalue())
                                                 }
                                                 __f},
                                                 next: Box::new(None)
                                             })
                             }],
                         arrangements: vec![
                             ],
                         change_cb:    Some(sync::Arc::new(sync::Mutex::new(__update_cb.clone())))
                     };
    let __MultiHead_9 = Relation {
                            name:         "__MultiHead_9".to_string(),
                            input:        false,
                            distinct:     false,
                            caching_mode: CachingMode::Set,
                            key_func:     None,
                            id:           Relations::__MultiHead_9 as RelId,
                            rules:        vec![
                                /* __MultiHead_9[((parent: internment::Intern<Scope>), (stmt1: internment::Intern<Stmt>), (stmt2: internment::Intern<Stmt>))] :- Statements[(Statements{.stmt=(stmt: internment::Intern<Stmt>), .scope=(parent: internment::Intern<Scope>)}: Statements)], (StmtSeq{.first=(var stmt1: internment::Intern<Stmt>), .second=(var stmt2: internment::Intern<Stmt>)}: Stmt) = (internment::ival(stmt): Stmt). */
                                Rule::CollectionRule {
                                    description: "__MultiHead_9[((parent: internment::Intern<Scope>), (stmt1: internment::Intern<Stmt>), (stmt2: internment::Intern<Stmt>))] :- Statements[(Statements{.stmt=(stmt: internment::Intern<Stmt>), .scope=(parent: internment::Intern<Scope>)}: Statements)], (StmtSeq{.first=(var stmt1: internment::Intern<Stmt>), .second=(var stmt2: internment::Intern<Stmt>)}: Stmt) = (internment::ival(stmt): Stmt).".to_string(),
                                    rel: Relations::Statements as RelId,
                                    xform: Some(XFormCollection::FilterMap{
                                                    description: "head of __MultiHead_9[((parent: internment::Intern<Scope>), (stmt1: internment::Intern<Stmt>), (stmt2: internment::Intern<Stmt>))] :- Statements[(Statements{.stmt=(stmt: internment::Intern<Stmt>), .scope=(parent: internment::Intern<Scope>)}: Statements)], (StmtSeq{.first=(var stmt1: internment::Intern<Stmt>), .second=(var stmt2: internment::Intern<Stmt>)}: Stmt) = (internment::ival(stmt): Stmt)." .to_string(),
                                                    fmfun: &{fn __f(__v: DDValue) -> Option<DDValue>
                                                    {
                                                        let (ref stmt, ref parent) = match unsafe {  Value::Statements::from_ddvalue_ref(&__v) }.0 {
                                                            Statements{stmt: ref stmt, scope: ref parent} => ((*stmt).clone(), (*parent).clone()),
                                                            _ => return None
                                                        };
                                                        let (ref stmt1, ref stmt2): (internment_Intern<Stmt>, internment_Intern<Stmt>) = match (*internment_ival(stmt)).clone() {
                                                            Stmt::StmtSeq{first: stmt1, second: stmt2} => (stmt1, stmt2),
                                                            _ => return None
                                                        };
                                                        Some(Value::__Tuple3__internment_Intern__Scope_internment_Intern__Stmt_internment_Intern__Stmt(((*parent).clone(), (*stmt1).clone(), (*stmt2).clone())).into_ddvalue())
                                                    }
                                                    __f},
                                                    next: Box::new(None)
                                                })
                                }],
                            arrangements: vec![
                                ],
                            change_cb:    None
                        };
    let SymbolTable = Relation {
                          name:         "SymbolTable".to_string(),
                          input:        false,
                          distinct:     true,
                          caching_mode: CachingMode::Set,
                          key_func:     None,
                          id:           Relations::SymbolTable as RelId,
                          rules:        vec![
                              /* SymbolTable[(SymbolTable{.path=path, .signature=signature}: SymbolTable)] :- __MultiHead_6[((path: internment::Intern<std::Vec<StrT>>), (signature: Signature), (func: Function))]. */
                              Rule::CollectionRule {
                                  description: "SymbolTable[(SymbolTable{.path=path, .signature=signature}: SymbolTable)] :- __MultiHead_6[((path: internment::Intern<std::Vec<StrT>>), (signature: Signature), (func: Function))].".to_string(),
                                  rel: Relations::__MultiHead_6 as RelId,
                                  xform: Some(XFormCollection::FilterMap{
                                                  description: "head of SymbolTable[(SymbolTable{.path=path, .signature=signature}: SymbolTable)] :- __MultiHead_6[((path: internment::Intern<std::Vec<StrT>>), (signature: Signature), (func: Function))]." .to_string(),
                                                  fmfun: &{fn __f(__v: DDValue) -> Option<DDValue>
                                                  {
                                                      let (ref path, ref signature, ref func) = match unsafe {  Value::__Tuple3__internment_Intern__std_Vec____Bitval32_Signature_Function::from_ddvalue_ref(&__v) }.0 {
                                                          (ref path, ref signature, ref func) => ((*path).clone(), (*signature).clone(), (*func).clone()),
                                                          _ => return None
                                                      };
                                                      Some(Value::SymbolTable((SymbolTable{path: (*path).clone(), signature: (*signature).clone()})).into_ddvalue())
                                                  }
                                                  __f},
                                                  next: Box::new(None)
                                              })
                              },
                              /* SymbolTable[(SymbolTable{.path=path, .signature=signature}: SymbolTable)] :- __MultiHead_7[((path: internment::Intern<std::Vec<StrT>>), (signature: Signature), (message: string), (func: Function))]. */
                              Rule::CollectionRule {
                                  description: "SymbolTable[(SymbolTable{.path=path, .signature=signature}: SymbolTable)] :- __MultiHead_7[((path: internment::Intern<std::Vec<StrT>>), (signature: Signature), (message: string), (func: Function))].".to_string(),
                                  rel: Relations::__MultiHead_7 as RelId,
                                  xform: Some(XFormCollection::FilterMap{
                                                  description: "head of SymbolTable[(SymbolTable{.path=path, .signature=signature}: SymbolTable)] :- __MultiHead_7[((path: internment::Intern<std::Vec<StrT>>), (signature: Signature), (message: string), (func: Function))]." .to_string(),
                                                  fmfun: &{fn __f(__v: DDValue) -> Option<DDValue>
                                                  {
                                                      let (ref path, ref signature, ref message, ref func) = match unsafe {  Value::__Tuple4__internment_Intern__std_Vec____Bitval32_Signature___Stringval_Function::from_ddvalue_ref(&__v) }.0 {
                                                          (ref path, ref signature, ref message, ref func) => ((*path).clone(), (*signature).clone(), (*message).clone(), (*func).clone()),
                                                          _ => return None
                                                      };
                                                      Some(Value::SymbolTable((SymbolTable{path: (*path).clone(), signature: (*signature).clone()})).into_ddvalue())
                                                  }
                                                  __f},
                                                  next: Box::new(None)
                                              })
                              }],
                          arrangements: vec![
                              ],
                          change_cb:    Some(sync::Arc::new(sync::Mutex::new(__update_cb.clone())))
                      };
    let __Null = Relation {
                     name:         "__Null".to_string(),
                     input:        false,
                     distinct:     false,
                     caching_mode: CachingMode::Set,
                     key_func:     None,
                     id:           Relations::__Null as RelId,
                     rules:        vec![
                         ],
                     arrangements: vec![
                         Arrangement::Map{
                            name: r###"_ /*join*/"###.to_string(),
                             afun: &{fn __f(__v: DDValue) -> Option<(DDValue,DDValue)>
                             {
                                 let __cloned = __v.clone();
                                 match unsafe { Value::__Tuple0__::from_ddvalue(__v) }.0 {
                                     _ => Some(Value::__Tuple0__(()).into_ddvalue()),
                                     _ => None
                                 }.map(|x|(x,__cloned))
                             }
                             __f},
                             queryable: true
                         }],
                     change_cb:    None
                 };
    Program {
        nodes: vec![
            ProgNode::Rel{rel: Expr},
            ProgNode::SCC{rels: vec![RecursiveRelation{rel: PropagateExprType, distinct: true}]},
            ProgNode::Rel{rel: ClampUnknownInt},
            ProgNode::Rel{rel: InputItems},
            ProgNode::Rel{rel: __MultiHead_6},
            ProgNode::Rel{rel: __MultiHead_7},
            ProgNode::Rel{rel: Errors},
            ProgNode::Rel{rel: Functions},
            ProgNode::SCC{rels: vec![RecursiveRelation{rel: Statements, distinct: true}, RecursiveRelation{rel: __MultiHead_9, distinct: true}]},
            ProgNode::Rel{rel: SymbolTable},
            ProgNode::Rel{rel: __Null}
        ],
        init_data: vec![
        ]
    }
}