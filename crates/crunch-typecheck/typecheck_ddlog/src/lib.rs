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
                                    /* PropagateExprType[(PropagateExprType{.id=id, .kind=kind, .ty=ty}: PropagateExprType)] :- Expr[(Expr{.id=(id: bit<64>), .kind=(kind: internment::Intern<hir::ExprKind>), .ty=(_: internment::Intern<hir::TypeKind>)}: Expr)], (hir::Lit{.lit=(var lit: internment::Intern<hir::LiteralVal>)}: hir::ExprKind) = (internment::ival(kind): hir::ExprKind), (var ty: internment::Intern<hir::TypeKind>) = hir::typeof_literal(lit). */
                                    Rule::CollectionRule {
                                        description: "PropagateExprType[(PropagateExprType{.id=id, .kind=kind, .ty=ty}: PropagateExprType)] :- Expr[(Expr{.id=(id: bit<64>), .kind=(kind: internment::Intern<hir::ExprKind>), .ty=(_: internment::Intern<hir::TypeKind>)}: Expr)], (hir::Lit{.lit=(var lit: internment::Intern<hir::LiteralVal>)}: hir::ExprKind) = (internment::ival(kind): hir::ExprKind), (var ty: internment::Intern<hir::TypeKind>) = hir::typeof_literal(lit).".to_string(),
                                        rel: Relations::Expr as RelId,
                                        xform: Some(XFormCollection::FilterMap{
                                                        description: "head of PropagateExprType[(PropagateExprType{.id=id, .kind=kind, .ty=ty}: PropagateExprType)] :- Expr[(Expr{.id=(id: bit<64>), .kind=(kind: internment::Intern<hir::ExprKind>), .ty=(_: internment::Intern<hir::TypeKind>)}: Expr)], (hir::Lit{.lit=(var lit: internment::Intern<hir::LiteralVal>)}: hir::ExprKind) = (internment::ival(kind): hir::ExprKind), (var ty: internment::Intern<hir::TypeKind>) = hir::typeof_literal(lit)." .to_string(),
                                                        fmfun: &{fn __f(__v: DDValue) -> Option<DDValue>
                                                        {
                                                            let (ref id, ref kind) = match unsafe {  Value::Expr::from_ddvalue_ref(&__v) }.0 {
                                                                Expr{id: ref id, kind: ref kind, ty: _} => ((*id).clone(), (*kind).clone()),
                                                                _ => return None
                                                            };
                                                            let ref lit: internment_Intern<hir_LiteralVal> = match (*internment_ival(kind)).clone() {
                                                                hir_ExprKind::hir_Lit{lit: lit} => lit,
                                                                _ => return None
                                                            };
                                                            let ref ty: internment_Intern<hir_TypeKind> = match hir_typeof_literal(lit) {
                                                                ty => ty,
                                                                _ => return None
                                                            };
                                                            Some(Value::PropagateExprType((PropagateExprType{id: (*id).clone(), kind: (*kind).clone(), ty: (*ty).clone()})).into_ddvalue())
                                                        }
                                                        __f},
                                                        next: Box::new(None)
                                                    })
                                    },
                                    /* PropagateExprType[(PropagateExprType{.id=id, .kind=kind, .ty=ty}: PropagateExprType)] :- Expr[(Expr{.id=(id: bit<64>), .kind=(kind: internment::Intern<hir::ExprKind>), .ty=(_: internment::Intern<hir::TypeKind>)}: Expr)], (hir::Assign{.variable=(_: bit<64>), .expr_id=(var expr_id: bit<64>)}: hir::ExprKind) = (internment::ival(kind): hir::ExprKind), PropagateExprType[(PropagateExprType{.id=(expr_id: bit<64>), .kind=(_: internment::Intern<hir::ExprKind>), .ty=(ty: internment::Intern<hir::TypeKind>)}: PropagateExprType)]. */
                                    Rule::CollectionRule {
                                        description: "PropagateExprType[(PropagateExprType{.id=id, .kind=kind, .ty=ty}: PropagateExprType)] :- Expr[(Expr{.id=(id: bit<64>), .kind=(kind: internment::Intern<hir::ExprKind>), .ty=(_: internment::Intern<hir::TypeKind>)}: Expr)], (hir::Assign{.variable=(_: bit<64>), .expr_id=(var expr_id: bit<64>)}: hir::ExprKind) = (internment::ival(kind): hir::ExprKind), PropagateExprType[(PropagateExprType{.id=(expr_id: bit<64>), .kind=(_: internment::Intern<hir::ExprKind>), .ty=(ty: internment::Intern<hir::TypeKind>)}: PropagateExprType)].".to_string(),
                                        rel: Relations::Expr as RelId,
                                        xform: Some(XFormCollection::Arrange {
                                                        description: "arrange Expr[(Expr{.id=(id: bit<64>), .kind=(kind: internment::Intern<hir::ExprKind>), .ty=(_: internment::Intern<hir::TypeKind>)}: Expr)] by (expr_id)" .to_string(),
                                                        afun: &{fn __f(__v: DDValue) -> Option<(DDValue,DDValue)>
                                                        {
                                                            let (ref id, ref kind) = match unsafe {  Value::Expr::from_ddvalue_ref(&__v) }.0 {
                                                                Expr{id: ref id, kind: ref kind, ty: _} => ((*id).clone(), (*kind).clone()),
                                                                _ => return None
                                                            };
                                                            let ref expr_id: u64 = match (*internment_ival(kind)).clone() {
                                                                hir_ExprKind::hir_Assign{variable: _, expr_id: expr_id} => expr_id,
                                                                _ => return None
                                                            };
                                                            Some((Value::__Bitval64((*expr_id).clone()).into_ddvalue(), Value::__Tuple2____Bitval64_internment_Intern__hir_ExprKind(((*id).clone(), (*kind).clone())).into_ddvalue()))
                                                        }
                                                        __f},
                                                        next: Box::new(XFormArrangement::Join{
                                                                           description: "Expr[(Expr{.id=(id: bit<64>), .kind=(kind: internment::Intern<hir::ExprKind>), .ty=(_: internment::Intern<hir::TypeKind>)}: Expr)], (hir::Assign{.variable=(_: bit<64>), .expr_id=(var expr_id: bit<64>)}: hir::ExprKind) = (internment::ival(kind): hir::ExprKind), PropagateExprType[(PropagateExprType{.id=(expr_id: bit<64>), .kind=(_: internment::Intern<hir::ExprKind>), .ty=(ty: internment::Intern<hir::TypeKind>)}: PropagateExprType)]".to_string(),
                                                                           ffun: None,
                                                                           arrangement: (Relations::PropagateExprType as RelId,0),
                                                                           jfun: &{fn __f(_: &DDValue ,__v1: &DDValue,__v2: &DDValue) -> Option<DDValue>
                                                                           {
                                                                               let (ref id, ref kind) = unsafe { Value::__Tuple2____Bitval64_internment_Intern__hir_ExprKind::from_ddvalue_ref( __v1 ) }.0;
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
                                    /* PropagateExprType[(PropagateExprType{.id=id, .kind=kind, .ty=ty}: PropagateExprType)] :- Expr[(Expr{.id=(id: bit<64>), .kind=(kind: internment::Intern<hir::ExprKind>), .ty=(_: internment::Intern<hir::TypeKind>)}: Expr)], (hir::Variable{.variable=(var expr_var: bit<64>)}: hir::ExprKind) = (internment::ival(kind): hir::ExprKind), PropagateExprType[(PropagateExprType{.id=(_: bit<64>), .kind=(prop_kind: internment::Intern<hir::ExprKind>), .ty=(ty: internment::Intern<hir::TypeKind>)}: PropagateExprType)], (hir::Assign{.variable=(var prop_var: bit<64>), .expr_id=(_: bit<64>)}: hir::ExprKind) = (internment::ival(prop_kind): hir::ExprKind), (expr_var == prop_var). */
                                    Rule::CollectionRule {
                                        description: "PropagateExprType[(PropagateExprType{.id=id, .kind=kind, .ty=ty}: PropagateExprType)] :- Expr[(Expr{.id=(id: bit<64>), .kind=(kind: internment::Intern<hir::ExprKind>), .ty=(_: internment::Intern<hir::TypeKind>)}: Expr)], (hir::Variable{.variable=(var expr_var: bit<64>)}: hir::ExprKind) = (internment::ival(kind): hir::ExprKind), PropagateExprType[(PropagateExprType{.id=(_: bit<64>), .kind=(prop_kind: internment::Intern<hir::ExprKind>), .ty=(ty: internment::Intern<hir::TypeKind>)}: PropagateExprType)], (hir::Assign{.variable=(var prop_var: bit<64>), .expr_id=(_: bit<64>)}: hir::ExprKind) = (internment::ival(prop_kind): hir::ExprKind), (expr_var == prop_var).".to_string(),
                                        rel: Relations::Expr as RelId,
                                        xform: Some(XFormCollection::Arrange {
                                                        description: "arrange Expr[(Expr{.id=(id: bit<64>), .kind=(kind: internment::Intern<hir::ExprKind>), .ty=(_: internment::Intern<hir::TypeKind>)}: Expr)] by ()" .to_string(),
                                                        afun: &{fn __f(__v: DDValue) -> Option<(DDValue,DDValue)>
                                                        {
                                                            let (ref id, ref kind) = match unsafe {  Value::Expr::from_ddvalue_ref(&__v) }.0 {
                                                                Expr{id: ref id, kind: ref kind, ty: _} => ((*id).clone(), (*kind).clone()),
                                                                _ => return None
                                                            };
                                                            let ref expr_var: u64 = match (*internment_ival(kind)).clone() {
                                                                hir_ExprKind::hir_Variable{variable: expr_var} => expr_var,
                                                                _ => return None
                                                            };
                                                            Some((Value::__Tuple0__(()).into_ddvalue(), Value::__Tuple3____Bitval64_internment_Intern__hir_ExprKind___Bitval64(((*id).clone(), (*kind).clone(), (*expr_var).clone())).into_ddvalue()))
                                                        }
                                                        __f},
                                                        next: Box::new(XFormArrangement::Join{
                                                                           description: "Expr[(Expr{.id=(id: bit<64>), .kind=(kind: internment::Intern<hir::ExprKind>), .ty=(_: internment::Intern<hir::TypeKind>)}: Expr)], (hir::Variable{.variable=(var expr_var: bit<64>)}: hir::ExprKind) = (internment::ival(kind): hir::ExprKind), PropagateExprType[(PropagateExprType{.id=(_: bit<64>), .kind=(prop_kind: internment::Intern<hir::ExprKind>), .ty=(ty: internment::Intern<hir::TypeKind>)}: PropagateExprType)]".to_string(),
                                                                           ffun: None,
                                                                           arrangement: (Relations::PropagateExprType as RelId,1),
                                                                           jfun: &{fn __f(_: &DDValue ,__v1: &DDValue,__v2: &DDValue) -> Option<DDValue>
                                                                           {
                                                                               let (ref id, ref kind, ref expr_var) = unsafe { Value::__Tuple3____Bitval64_internment_Intern__hir_ExprKind___Bitval64::from_ddvalue_ref( __v1 ) }.0;
                                                                               let (ref prop_kind, ref ty) = match unsafe {  Value::PropagateExprType::from_ddvalue_ref(__v2) }.0 {
                                                                                   PropagateExprType{id: _, kind: ref prop_kind, ty: ref ty} => ((*prop_kind).clone(), (*ty).clone()),
                                                                                   _ => return None
                                                                               };
                                                                               let ref prop_var: u64 = match (*internment_ival(prop_kind)).clone() {
                                                                                   hir_ExprKind::hir_Assign{variable: prop_var, expr_id: _} => prop_var,
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
                                       name: r###"(PropagateExprType{.id=(_0: bit<64>), .kind=(_: internment::Intern<hir::ExprKind>), .ty=(_: internment::Intern<hir::TypeKind>)}: PropagateExprType) /*join*/"###.to_string(),
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
                                       name: r###"(PropagateExprType{.id=(_: bit<64>), .kind=(_: internment::Intern<hir::ExprKind>), .ty=(_: internment::Intern<hir::TypeKind>)}: PropagateExprType) /*join*/"###.to_string(),
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
                                  /* ClampUnknownInt[(ClampUnknownInt{.id=id, .kind=kind, .ty=ty}: ClampUnknownInt)] :- PropagateExprType[(PropagateExprType{.id=(id: bit<64>), .kind=(kind: internment::Intern<hir::ExprKind>), .ty=(ty: internment::Intern<hir::TypeKind>)}: PropagateExprType)], (hir::Int{.is_signed=(std::None{}: std::Option<bool>), .width=(std::None{}: std::Option<std::u16>)}: hir::TypeKind) = (internment::ival(ty): hir::TypeKind), (var ty: internment::Intern<hir::TypeKind>) = (internment::intern((hir::Int{.is_signed=(std::Some{.x=true}: std::Option<bool>), .width=(std::Some{.x=16'd32}: std::Option<std::u16>)}: hir::TypeKind)): internment::Intern<hir::TypeKind>). */
                                  Rule::CollectionRule {
                                      description: "ClampUnknownInt[(ClampUnknownInt{.id=id, .kind=kind, .ty=ty}: ClampUnknownInt)] :- PropagateExprType[(PropagateExprType{.id=(id: bit<64>), .kind=(kind: internment::Intern<hir::ExprKind>), .ty=(ty: internment::Intern<hir::TypeKind>)}: PropagateExprType)], (hir::Int{.is_signed=(std::None{}: std::Option<bool>), .width=(std::None{}: std::Option<std::u16>)}: hir::TypeKind) = (internment::ival(ty): hir::TypeKind), (var ty: internment::Intern<hir::TypeKind>) = (internment::intern((hir::Int{.is_signed=(std::Some{.x=true}: std::Option<bool>), .width=(std::Some{.x=16'd32}: std::Option<std::u16>)}: hir::TypeKind)): internment::Intern<hir::TypeKind>).".to_string(),
                                      rel: Relations::PropagateExprType as RelId,
                                      xform: Some(XFormCollection::FilterMap{
                                                      description: "head of ClampUnknownInt[(ClampUnknownInt{.id=id, .kind=kind, .ty=ty}: ClampUnknownInt)] :- PropagateExprType[(PropagateExprType{.id=(id: bit<64>), .kind=(kind: internment::Intern<hir::ExprKind>), .ty=(ty: internment::Intern<hir::TypeKind>)}: PropagateExprType)], (hir::Int{.is_signed=(std::None{}: std::Option<bool>), .width=(std::None{}: std::Option<std::u16>)}: hir::TypeKind) = (internment::ival(ty): hir::TypeKind), (var ty: internment::Intern<hir::TypeKind>) = (internment::intern((hir::Int{.is_signed=(std::Some{.x=true}: std::Option<bool>), .width=(std::Some{.x=16'd32}: std::Option<std::u16>)}: hir::TypeKind)): internment::Intern<hir::TypeKind>)." .to_string(),
                                                      fmfun: &{fn __f(__v: DDValue) -> Option<DDValue>
                                                      {
                                                          let (ref id, ref kind, ref ty) = match unsafe {  Value::PropagateExprType::from_ddvalue_ref(&__v) }.0 {
                                                              PropagateExprType{id: ref id, kind: ref kind, ty: ref ty} => ((*id).clone(), (*kind).clone(), (*ty).clone()),
                                                              _ => return None
                                                          };
                                                          let (): () = match (*internment_ival(ty)).clone() {
                                                              hir_TypeKind::hir_Int{is_signed: std_Option::std_None{}, width: std_Option::std_None{}} => (),
                                                              _ => return None
                                                          };
                                                          let ref ty: internment_Intern<hir_TypeKind> = match (*(&*__STATIC_6)).clone() {
                                                              ty => ty,
                                                              _ => return None
                                                          };
                                                          Some(Value::ClampUnknownInt((ClampUnknownInt{id: (*id).clone(), kind: (*kind).clone(), ty: (*ty).clone()})).into_ddvalue())
                                                      }
                                                      __f},
                                                      next: Box::new(None)
                                                  })
                                  },
                                  /* ClampUnknownInt[(ClampUnknownInt{.id=id, .kind=kind, .ty=ty}: ClampUnknownInt)] :- PropagateExprType[(PropagateExprType{.id=(id: bit<64>), .kind=(kind: internment::Intern<hir::ExprKind>), .ty=(ty: internment::Intern<hir::TypeKind>)}: PropagateExprType)], (hir::Int{.is_signed=(std::Some{.x=(_: bool)}: std::Option<bool>), .width=(std::Some{.x=(_: bit<16>)}: std::Option<std::u16>)}: hir::TypeKind) = (internment::ival(ty): hir::TypeKind). */
                                  Rule::CollectionRule {
                                      description: "ClampUnknownInt[(ClampUnknownInt{.id=id, .kind=kind, .ty=ty}: ClampUnknownInt)] :- PropagateExprType[(PropagateExprType{.id=(id: bit<64>), .kind=(kind: internment::Intern<hir::ExprKind>), .ty=(ty: internment::Intern<hir::TypeKind>)}: PropagateExprType)], (hir::Int{.is_signed=(std::Some{.x=(_: bool)}: std::Option<bool>), .width=(std::Some{.x=(_: bit<16>)}: std::Option<std::u16>)}: hir::TypeKind) = (internment::ival(ty): hir::TypeKind).".to_string(),
                                      rel: Relations::PropagateExprType as RelId,
                                      xform: Some(XFormCollection::FilterMap{
                                                      description: "head of ClampUnknownInt[(ClampUnknownInt{.id=id, .kind=kind, .ty=ty}: ClampUnknownInt)] :- PropagateExprType[(PropagateExprType{.id=(id: bit<64>), .kind=(kind: internment::Intern<hir::ExprKind>), .ty=(ty: internment::Intern<hir::TypeKind>)}: PropagateExprType)], (hir::Int{.is_signed=(std::Some{.x=(_: bool)}: std::Option<bool>), .width=(std::Some{.x=(_: bit<16>)}: std::Option<std::u16>)}: hir::TypeKind) = (internment::ival(ty): hir::TypeKind)." .to_string(),
                                                      fmfun: &{fn __f(__v: DDValue) -> Option<DDValue>
                                                      {
                                                          let (ref id, ref kind, ref ty) = match unsafe {  Value::PropagateExprType::from_ddvalue_ref(&__v) }.0 {
                                                              PropagateExprType{id: ref id, kind: ref kind, ty: ref ty} => ((*id).clone(), (*kind).clone(), (*ty).clone()),
                                                              _ => return None
                                                          };
                                                          let (): () = match (*internment_ival(ty)).clone() {
                                                              hir_TypeKind::hir_Int{is_signed: std_Option::std_Some{x: _}, width: std_Option::std_Some{x: _}} => (),
                                                              _ => return None
                                                          };
                                                          Some(Value::ClampUnknownInt((ClampUnknownInt{id: (*id).clone(), kind: (*kind).clone(), ty: (*ty).clone()})).into_ddvalue())
                                                      }
                                                      __f},
                                                      next: Box::new(None)
                                                  })
                                  },
                                  /* ClampUnknownInt[(ClampUnknownInt{.id=id, .kind=kind, .ty=ty}: ClampUnknownInt)] :- PropagateExprType[(PropagateExprType{.id=(id: bit<64>), .kind=(kind: internment::Intern<hir::ExprKind>), .ty=(ty: internment::Intern<hir::TypeKind>)}: PropagateExprType)], (not hir::is_int(ty)). */
                                  Rule::CollectionRule {
                                      description: "ClampUnknownInt[(ClampUnknownInt{.id=id, .kind=kind, .ty=ty}: ClampUnknownInt)] :- PropagateExprType[(PropagateExprType{.id=(id: bit<64>), .kind=(kind: internment::Intern<hir::ExprKind>), .ty=(ty: internment::Intern<hir::TypeKind>)}: PropagateExprType)], (not hir::is_int(ty)).".to_string(),
                                      rel: Relations::PropagateExprType as RelId,
                                      xform: Some(XFormCollection::FilterMap{
                                                      description: "head of ClampUnknownInt[(ClampUnknownInt{.id=id, .kind=kind, .ty=ty}: ClampUnknownInt)] :- PropagateExprType[(PropagateExprType{.id=(id: bit<64>), .kind=(kind: internment::Intern<hir::ExprKind>), .ty=(ty: internment::Intern<hir::TypeKind>)}: PropagateExprType)], (not hir::is_int(ty))." .to_string(),
                                                      fmfun: &{fn __f(__v: DDValue) -> Option<DDValue>
                                                      {
                                                          let (ref id, ref kind, ref ty) = match unsafe {  Value::PropagateExprType::from_ddvalue_ref(&__v) }.0 {
                                                              PropagateExprType{id: ref id, kind: ref kind, ty: ref ty} => ((*id).clone(), (*kind).clone(), (*ty).clone()),
                                                              _ => return None
                                                          };
                                                          if !(!hir_is_int(ty)) {return None;};
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
    let __MultiHead_0 = Relation {
                            name:         "__MultiHead_0".to_string(),
                            input:        false,
                            distinct:     false,
                            caching_mode: CachingMode::Set,
                            key_func:     None,
                            id:           Relations::__MultiHead_0 as RelId,
                            rules:        vec![
                                /* __MultiHead_0[((path: internment::Intern<std::Vec<hir::StrT>>), (signature: hir::Signature), (func: hir::Function))] :- InputItems[(InputItems{.item=(item@ (hir::ItemFunc{.func=(func: hir::Function)}: hir::Item))}: InputItems)], (not hir::has_unknown_types(item)), (var path: internment::Intern<std::Vec<hir::StrT>>) = hir::path(item), (var signature: hir::Signature) = hir::signature(item). */
                                Rule::CollectionRule {
                                    description: "__MultiHead_0[((path: internment::Intern<std::Vec<hir::StrT>>), (signature: hir::Signature), (func: hir::Function))] :- InputItems[(InputItems{.item=(item@ (hir::ItemFunc{.func=(func: hir::Function)}: hir::Item))}: InputItems)], (not hir::has_unknown_types(item)), (var path: internment::Intern<std::Vec<hir::StrT>>) = hir::path(item), (var signature: hir::Signature) = hir::signature(item).".to_string(),
                                    rel: Relations::InputItems as RelId,
                                    xform: Some(XFormCollection::FilterMap{
                                                    description: "head of __MultiHead_0[((path: internment::Intern<std::Vec<hir::StrT>>), (signature: hir::Signature), (func: hir::Function))] :- InputItems[(InputItems{.item=(item@ (hir::ItemFunc{.func=(func: hir::Function)}: hir::Item))}: InputItems)], (not hir::has_unknown_types(item)), (var path: internment::Intern<std::Vec<hir::StrT>>) = hir::path(item), (var signature: hir::Signature) = hir::signature(item)." .to_string(),
                                                    fmfun: &{fn __f(__v: DDValue) -> Option<DDValue>
                                                    {
                                                        let (ref item, ref func) = match unsafe {  Value::InputItems::from_ddvalue_ref(&__v) }.0 {
                                                            InputItems{item: ref item} => match item {
                                                                                              hir_Item::hir_ItemFunc{func: ref func} => ((*item).clone(), (*func).clone()),
                                                                                              _ => return None
                                                                                          },
                                                            _ => return None
                                                        };
                                                        if !(!hir_has_unknown_types(item)) {return None;};
                                                        let ref path: internment_Intern<std_Vec<hir_StrT>> = match hir_path(item) {
                                                            path => path,
                                                            _ => return None
                                                        };
                                                        let ref signature: hir_Signature = match hir_signature(item) {
                                                            signature => signature,
                                                            _ => return None
                                                        };
                                                        Some(Value::__Tuple3__internment_Intern__std_Vec____Bitval32_hir_Signature_hir_Function(((*path).clone(), (*signature).clone(), (*func).clone())).into_ddvalue())
                                                    }
                                                    __f},
                                                    next: Box::new(None)
                                                })
                                }],
                            arrangements: vec![
                                ],
                            change_cb:    None
                        };
    let __MultiHead_1 = Relation {
                            name:         "__MultiHead_1".to_string(),
                            input:        false,
                            distinct:     false,
                            caching_mode: CachingMode::Set,
                            key_func:     None,
                            id:           Relations::__MultiHead_1 as RelId,
                            rules:        vec![
                                /* __MultiHead_1[((path: internment::Intern<std::Vec<hir::StrT>>), (signature: hir::Signature), (message: string), (func: hir::Function))] :- InputItems[(InputItems{.item=(item@ (hir::ItemFunc{.func=(func: hir::Function)}: hir::Item))}: InputItems)], hir::has_unknown_types(item), var message = FlatMap(hir::unknown_type_errors(item)), (var path: internment::Intern<std::Vec<hir::StrT>>) = hir::path(item), (var signature: hir::Signature) = hir::corrected_signature(item). */
                                Rule::CollectionRule {
                                    description: "__MultiHead_1[((path: internment::Intern<std::Vec<hir::StrT>>), (signature: hir::Signature), (message: string), (func: hir::Function))] :- InputItems[(InputItems{.item=(item@ (hir::ItemFunc{.func=(func: hir::Function)}: hir::Item))}: InputItems)], hir::has_unknown_types(item), var message = FlatMap(hir::unknown_type_errors(item)), (var path: internment::Intern<std::Vec<hir::StrT>>) = hir::path(item), (var signature: hir::Signature) = hir::corrected_signature(item).".to_string(),
                                    rel: Relations::InputItems as RelId,
                                    xform: Some(XFormCollection::FlatMap{
                                                    description: "InputItems[(InputItems{.item=(item@ (hir::ItemFunc{.func=(func: hir::Function)}: hir::Item))}: InputItems)], hir::has_unknown_types(item), var message = FlatMap(hir::unknown_type_errors(item))" .to_string(),
                                                    fmfun: &{fn __f(__v: DDValue) -> Option<Box<dyn Iterator<Item=DDValue>>>
                                                    {
                                                        let (ref item, ref func) = match unsafe {  Value::InputItems::from_ddvalue_ref(&__v) }.0 {
                                                            InputItems{item: ref item} => match item {
                                                                                              hir_Item::hir_ItemFunc{func: ref func} => ((*item).clone(), (*func).clone()),
                                                                                              _ => return None
                                                                                          },
                                                            _ => return None
                                                        };
                                                        if !hir_has_unknown_types(item) {return None;};
                                                        let __flattened = hir_unknown_type_errors(item);
                                                        let item = (*item).clone();
                                                        let func = (*func).clone();
                                                        Some(Box::new(__flattened.into_iter().map(move |message|Value::__Tuple3____Stringval_hir_Item_hir_Function((message.clone(), item.clone(), func.clone())).into_ddvalue())))
                                                    }
                                                    __f},
                                                    next: Box::new(Some(XFormCollection::FilterMap{
                                                                            description: "head of __MultiHead_1[((path: internment::Intern<std::Vec<hir::StrT>>), (signature: hir::Signature), (message: string), (func: hir::Function))] :- InputItems[(InputItems{.item=(item@ (hir::ItemFunc{.func=(func: hir::Function)}: hir::Item))}: InputItems)], hir::has_unknown_types(item), var message = FlatMap(hir::unknown_type_errors(item)), (var path: internment::Intern<std::Vec<hir::StrT>>) = hir::path(item), (var signature: hir::Signature) = hir::corrected_signature(item)." .to_string(),
                                                                            fmfun: &{fn __f(__v: DDValue) -> Option<DDValue>
                                                                            {
                                                                                let (ref message, ref item, ref func) = unsafe { Value::__Tuple3____Stringval_hir_Item_hir_Function::from_ddvalue_ref( &__v ) }.0;
                                                                                let ref path: internment_Intern<std_Vec<hir_StrT>> = match hir_path(item) {
                                                                                    path => path,
                                                                                    _ => return None
                                                                                };
                                                                                let ref signature: hir_Signature = match hir_corrected_signature(item) {
                                                                                    signature => signature,
                                                                                    _ => return None
                                                                                };
                                                                                Some(Value::__Tuple4__internment_Intern__std_Vec____Bitval32_hir_Signature___Stringval_hir_Function(((*path).clone(), (*signature).clone(), (*message).clone(), (*func).clone())).into_ddvalue())
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
                         /* Errors[(Errors{.message=message}: Errors)] :- __MultiHead_1[((path: internment::Intern<std::Vec<hir::StrT>>), (signature: hir::Signature), (message: string), (func: hir::Function))]. */
                         Rule::CollectionRule {
                             description: "Errors[(Errors{.message=message}: Errors)] :- __MultiHead_1[((path: internment::Intern<std::Vec<hir::StrT>>), (signature: hir::Signature), (message: string), (func: hir::Function))].".to_string(),
                             rel: Relations::__MultiHead_1 as RelId,
                             xform: Some(XFormCollection::FilterMap{
                                             description: "head of Errors[(Errors{.message=message}: Errors)] :- __MultiHead_1[((path: internment::Intern<std::Vec<hir::StrT>>), (signature: hir::Signature), (message: string), (func: hir::Function))]." .to_string(),
                                             fmfun: &{fn __f(__v: DDValue) -> Option<DDValue>
                                             {
                                                 let (ref path, ref signature, ref message, ref func) = match unsafe {  Value::__Tuple4__internment_Intern__std_Vec____Bitval32_hir_Signature___Stringval_hir_Function::from_ddvalue_ref(&__v) }.0 {
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
                            /* Functions[(Functions{.func=(internment::intern(func): internment::Intern<hir::Function>)}: Functions)] :- __MultiHead_0[((path: internment::Intern<std::Vec<hir::StrT>>), (signature: hir::Signature), (func: hir::Function))]. */
                            Rule::CollectionRule {
                                description: "Functions[(Functions{.func=(internment::intern(func): internment::Intern<hir::Function>)}: Functions)] :- __MultiHead_0[((path: internment::Intern<std::Vec<hir::StrT>>), (signature: hir::Signature), (func: hir::Function))].".to_string(),
                                rel: Relations::__MultiHead_0 as RelId,
                                xform: Some(XFormCollection::FilterMap{
                                                description: "head of Functions[(Functions{.func=(internment::intern(func): internment::Intern<hir::Function>)}: Functions)] :- __MultiHead_0[((path: internment::Intern<std::Vec<hir::StrT>>), (signature: hir::Signature), (func: hir::Function))]." .to_string(),
                                                fmfun: &{fn __f(__v: DDValue) -> Option<DDValue>
                                                {
                                                    let (ref path, ref signature, ref func) = match unsafe {  Value::__Tuple3__internment_Intern__std_Vec____Bitval32_hir_Signature_hir_Function::from_ddvalue_ref(&__v) }.0 {
                                                        (ref path, ref signature, ref func) => ((*path).clone(), (*signature).clone(), (*func).clone()),
                                                        _ => return None
                                                    };
                                                    Some(Value::Functions((Functions{func: internment_intern(func)})).into_ddvalue())
                                                }
                                                __f},
                                                next: Box::new(None)
                                            })
                            },
                            /* Functions[(Functions{.func=(internment::intern(func): internment::Intern<hir::Function>)}: Functions)] :- __MultiHead_1[((path: internment::Intern<std::Vec<hir::StrT>>), (signature: hir::Signature), (message: string), (func: hir::Function))]. */
                            Rule::CollectionRule {
                                description: "Functions[(Functions{.func=(internment::intern(func): internment::Intern<hir::Function>)}: Functions)] :- __MultiHead_1[((path: internment::Intern<std::Vec<hir::StrT>>), (signature: hir::Signature), (message: string), (func: hir::Function))].".to_string(),
                                rel: Relations::__MultiHead_1 as RelId,
                                xform: Some(XFormCollection::FilterMap{
                                                description: "head of Functions[(Functions{.func=(internment::intern(func): internment::Intern<hir::Function>)}: Functions)] :- __MultiHead_1[((path: internment::Intern<std::Vec<hir::StrT>>), (signature: hir::Signature), (message: string), (func: hir::Function))]." .to_string(),
                                                fmfun: &{fn __f(__v: DDValue) -> Option<DDValue>
                                                {
                                                    let (ref path, ref signature, ref message, ref func) = match unsafe {  Value::__Tuple4__internment_Intern__std_Vec____Bitval32_hir_Signature___Stringval_hir_Function::from_ddvalue_ref(&__v) }.0 {
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
                             /* Statements[(Statements{.stmt=stmt, .scope=(internment::intern((hir::ScopeFunction{.func=func}: hir::Scope)): internment::Intern<hir::Scope>)}: Statements)] :- Functions[(Functions{.func=(func: internment::Intern<hir::Function>)}: Functions)], (hir::Function{.name=(_: internment::Intern<std::Vec<hir::StrT>>), .vis=(_: hir::Vis), .args=(_: std::Vec<hir::FuncArg>), .body=(var stmt: internment::Intern<hir::Stmt>), .ret=(_: internment::Intern<hir::TypeKind>)}: hir::Function) = (internment::ival(func): hir::Function). */
                             Rule::CollectionRule {
                                 description: "Statements[(Statements{.stmt=stmt, .scope=(internment::intern((hir::ScopeFunction{.func=func}: hir::Scope)): internment::Intern<hir::Scope>)}: Statements)] :- Functions[(Functions{.func=(func: internment::Intern<hir::Function>)}: Functions)], (hir::Function{.name=(_: internment::Intern<std::Vec<hir::StrT>>), .vis=(_: hir::Vis), .args=(_: std::Vec<hir::FuncArg>), .body=(var stmt: internment::Intern<hir::Stmt>), .ret=(_: internment::Intern<hir::TypeKind>)}: hir::Function) = (internment::ival(func): hir::Function).".to_string(),
                                 rel: Relations::Functions as RelId,
                                 xform: Some(XFormCollection::FilterMap{
                                                 description: "head of Statements[(Statements{.stmt=stmt, .scope=(internment::intern((hir::ScopeFunction{.func=func}: hir::Scope)): internment::Intern<hir::Scope>)}: Statements)] :- Functions[(Functions{.func=(func: internment::Intern<hir::Function>)}: Functions)], (hir::Function{.name=(_: internment::Intern<std::Vec<hir::StrT>>), .vis=(_: hir::Vis), .args=(_: std::Vec<hir::FuncArg>), .body=(var stmt: internment::Intern<hir::Stmt>), .ret=(_: internment::Intern<hir::TypeKind>)}: hir::Function) = (internment::ival(func): hir::Function)." .to_string(),
                                                 fmfun: &{fn __f(__v: DDValue) -> Option<DDValue>
                                                 {
                                                     let ref func = match unsafe {  Value::Functions::from_ddvalue_ref(&__v) }.0 {
                                                         Functions{func: ref func} => (*func).clone(),
                                                         _ => return None
                                                     };
                                                     let ref stmt: internment_Intern<hir_Stmt> = match (*internment_ival(func)).clone() {
                                                         hir_Function{name: _, vis: _, args: _, body: stmt, ret: _} => stmt,
                                                         _ => return None
                                                     };
                                                     Some(Value::Statements((Statements{stmt: (*stmt).clone(), scope: internment_intern((&(hir_Scope::hir_ScopeFunction{func: (*func).clone()})))})).into_ddvalue())
                                                 }
                                                 __f},
                                                 next: Box::new(None)
                                             })
                             },
                             /* Statements[(Statements{.stmt=stmt1, .scope=(internment::intern((hir::ScopeSeq1{.parent=parent}: hir::Scope)): internment::Intern<hir::Scope>)}: Statements)] :- __MultiHead_3[((parent: internment::Intern<hir::Scope>), (stmt1: internment::Intern<hir::Stmt>), (stmt2: internment::Intern<hir::Stmt>))]. */
                             Rule::CollectionRule {
                                 description: "Statements[(Statements{.stmt=stmt1, .scope=(internment::intern((hir::ScopeSeq1{.parent=parent}: hir::Scope)): internment::Intern<hir::Scope>)}: Statements)] :- __MultiHead_3[((parent: internment::Intern<hir::Scope>), (stmt1: internment::Intern<hir::Stmt>), (stmt2: internment::Intern<hir::Stmt>))].".to_string(),
                                 rel: Relations::__MultiHead_3 as RelId,
                                 xform: Some(XFormCollection::FilterMap{
                                                 description: "head of Statements[(Statements{.stmt=stmt1, .scope=(internment::intern((hir::ScopeSeq1{.parent=parent}: hir::Scope)): internment::Intern<hir::Scope>)}: Statements)] :- __MultiHead_3[((parent: internment::Intern<hir::Scope>), (stmt1: internment::Intern<hir::Stmt>), (stmt2: internment::Intern<hir::Stmt>))]." .to_string(),
                                                 fmfun: &{fn __f(__v: DDValue) -> Option<DDValue>
                                                 {
                                                     let (ref parent, ref stmt1, ref stmt2) = match unsafe {  Value::__Tuple3__internment_Intern__hir_Scope_internment_Intern__hir_Stmt_internment_Intern__hir_Stmt::from_ddvalue_ref(&__v) }.0 {
                                                         (ref parent, ref stmt1, ref stmt2) => ((*parent).clone(), (*stmt1).clone(), (*stmt2).clone()),
                                                         _ => return None
                                                     };
                                                     Some(Value::Statements((Statements{stmt: (*stmt1).clone(), scope: internment_intern((&(hir_Scope::hir_ScopeSeq1{parent: (*parent).clone()})))})).into_ddvalue())
                                                 }
                                                 __f},
                                                 next: Box::new(None)
                                             })
                             },
                             /* Statements[(Statements{.stmt=stmt2, .scope=(internment::intern((hir::ScopeSeq2{.parent=parent}: hir::Scope)): internment::Intern<hir::Scope>)}: Statements)] :- __MultiHead_3[((parent: internment::Intern<hir::Scope>), (stmt1: internment::Intern<hir::Stmt>), (stmt2: internment::Intern<hir::Stmt>))]. */
                             Rule::CollectionRule {
                                 description: "Statements[(Statements{.stmt=stmt2, .scope=(internment::intern((hir::ScopeSeq2{.parent=parent}: hir::Scope)): internment::Intern<hir::Scope>)}: Statements)] :- __MultiHead_3[((parent: internment::Intern<hir::Scope>), (stmt1: internment::Intern<hir::Stmt>), (stmt2: internment::Intern<hir::Stmt>))].".to_string(),
                                 rel: Relations::__MultiHead_3 as RelId,
                                 xform: Some(XFormCollection::FilterMap{
                                                 description: "head of Statements[(Statements{.stmt=stmt2, .scope=(internment::intern((hir::ScopeSeq2{.parent=parent}: hir::Scope)): internment::Intern<hir::Scope>)}: Statements)] :- __MultiHead_3[((parent: internment::Intern<hir::Scope>), (stmt1: internment::Intern<hir::Stmt>), (stmt2: internment::Intern<hir::Stmt>))]." .to_string(),
                                                 fmfun: &{fn __f(__v: DDValue) -> Option<DDValue>
                                                 {
                                                     let (ref parent, ref stmt1, ref stmt2) = match unsafe {  Value::__Tuple3__internment_Intern__hir_Scope_internment_Intern__hir_Stmt_internment_Intern__hir_Stmt::from_ddvalue_ref(&__v) }.0 {
                                                         (ref parent, ref stmt1, ref stmt2) => ((*parent).clone(), (*stmt1).clone(), (*stmt2).clone()),
                                                         _ => return None
                                                     };
                                                     Some(Value::Statements((Statements{stmt: (*stmt2).clone(), scope: internment_intern((&(hir_Scope::hir_ScopeSeq2{parent: (*parent).clone()})))})).into_ddvalue())
                                                 }
                                                 __f},
                                                 next: Box::new(None)
                                             })
                             }],
                         arrangements: vec![
                             ],
                         change_cb:    Some(sync::Arc::new(sync::Mutex::new(__update_cb.clone())))
                     };
    let __MultiHead_3 = Relation {
                            name:         "__MultiHead_3".to_string(),
                            input:        false,
                            distinct:     false,
                            caching_mode: CachingMode::Set,
                            key_func:     None,
                            id:           Relations::__MultiHead_3 as RelId,
                            rules:        vec![
                                /* __MultiHead_3[((parent: internment::Intern<hir::Scope>), (stmt1: internment::Intern<hir::Stmt>), (stmt2: internment::Intern<hir::Stmt>))] :- Statements[(Statements{.stmt=(stmt: internment::Intern<hir::Stmt>), .scope=(parent: internment::Intern<hir::Scope>)}: Statements)], (hir::StmtSeq{.first=(var stmt1: internment::Intern<hir::Stmt>), .second=(var stmt2: internment::Intern<hir::Stmt>)}: hir::Stmt) = (internment::ival(stmt): hir::Stmt). */
                                Rule::CollectionRule {
                                    description: "__MultiHead_3[((parent: internment::Intern<hir::Scope>), (stmt1: internment::Intern<hir::Stmt>), (stmt2: internment::Intern<hir::Stmt>))] :- Statements[(Statements{.stmt=(stmt: internment::Intern<hir::Stmt>), .scope=(parent: internment::Intern<hir::Scope>)}: Statements)], (hir::StmtSeq{.first=(var stmt1: internment::Intern<hir::Stmt>), .second=(var stmt2: internment::Intern<hir::Stmt>)}: hir::Stmt) = (internment::ival(stmt): hir::Stmt).".to_string(),
                                    rel: Relations::Statements as RelId,
                                    xform: Some(XFormCollection::FilterMap{
                                                    description: "head of __MultiHead_3[((parent: internment::Intern<hir::Scope>), (stmt1: internment::Intern<hir::Stmt>), (stmt2: internment::Intern<hir::Stmt>))] :- Statements[(Statements{.stmt=(stmt: internment::Intern<hir::Stmt>), .scope=(parent: internment::Intern<hir::Scope>)}: Statements)], (hir::StmtSeq{.first=(var stmt1: internment::Intern<hir::Stmt>), .second=(var stmt2: internment::Intern<hir::Stmt>)}: hir::Stmt) = (internment::ival(stmt): hir::Stmt)." .to_string(),
                                                    fmfun: &{fn __f(__v: DDValue) -> Option<DDValue>
                                                    {
                                                        let (ref stmt, ref parent) = match unsafe {  Value::Statements::from_ddvalue_ref(&__v) }.0 {
                                                            Statements{stmt: ref stmt, scope: ref parent} => ((*stmt).clone(), (*parent).clone()),
                                                            _ => return None
                                                        };
                                                        let (ref stmt1, ref stmt2): (internment_Intern<hir_Stmt>, internment_Intern<hir_Stmt>) = match (*internment_ival(stmt)).clone() {
                                                            hir_Stmt::hir_StmtSeq{first: stmt1, second: stmt2} => (stmt1, stmt2),
                                                            _ => return None
                                                        };
                                                        Some(Value::__Tuple3__internment_Intern__hir_Scope_internment_Intern__hir_Stmt_internment_Intern__hir_Stmt(((*parent).clone(), (*stmt1).clone(), (*stmt2).clone())).into_ddvalue())
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
                              /* SymbolTable[(SymbolTable{.path=path, .signature=signature}: SymbolTable)] :- __MultiHead_0[((path: internment::Intern<std::Vec<hir::StrT>>), (signature: hir::Signature), (func: hir::Function))]. */
                              Rule::CollectionRule {
                                  description: "SymbolTable[(SymbolTable{.path=path, .signature=signature}: SymbolTable)] :- __MultiHead_0[((path: internment::Intern<std::Vec<hir::StrT>>), (signature: hir::Signature), (func: hir::Function))].".to_string(),
                                  rel: Relations::__MultiHead_0 as RelId,
                                  xform: Some(XFormCollection::FilterMap{
                                                  description: "head of SymbolTable[(SymbolTable{.path=path, .signature=signature}: SymbolTable)] :- __MultiHead_0[((path: internment::Intern<std::Vec<hir::StrT>>), (signature: hir::Signature), (func: hir::Function))]." .to_string(),
                                                  fmfun: &{fn __f(__v: DDValue) -> Option<DDValue>
                                                  {
                                                      let (ref path, ref signature, ref func) = match unsafe {  Value::__Tuple3__internment_Intern__std_Vec____Bitval32_hir_Signature_hir_Function::from_ddvalue_ref(&__v) }.0 {
                                                          (ref path, ref signature, ref func) => ((*path).clone(), (*signature).clone(), (*func).clone()),
                                                          _ => return None
                                                      };
                                                      Some(Value::SymbolTable((SymbolTable{path: (*path).clone(), signature: (*signature).clone()})).into_ddvalue())
                                                  }
                                                  __f},
                                                  next: Box::new(None)
                                              })
                              },
                              /* SymbolTable[(SymbolTable{.path=path, .signature=signature}: SymbolTable)] :- __MultiHead_1[((path: internment::Intern<std::Vec<hir::StrT>>), (signature: hir::Signature), (message: string), (func: hir::Function))]. */
                              Rule::CollectionRule {
                                  description: "SymbolTable[(SymbolTable{.path=path, .signature=signature}: SymbolTable)] :- __MultiHead_1[((path: internment::Intern<std::Vec<hir::StrT>>), (signature: hir::Signature), (message: string), (func: hir::Function))].".to_string(),
                                  rel: Relations::__MultiHead_1 as RelId,
                                  xform: Some(XFormCollection::FilterMap{
                                                  description: "head of SymbolTable[(SymbolTable{.path=path, .signature=signature}: SymbolTable)] :- __MultiHead_1[((path: internment::Intern<std::Vec<hir::StrT>>), (signature: hir::Signature), (message: string), (func: hir::Function))]." .to_string(),
                                                  fmfun: &{fn __f(__v: DDValue) -> Option<DDValue>
                                                  {
                                                      let (ref path, ref signature, ref message, ref func) = match unsafe {  Value::__Tuple4__internment_Intern__std_Vec____Bitval32_hir_Signature___Stringval_hir_Function::from_ddvalue_ref(&__v) }.0 {
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
            ProgNode::Rel{rel: __MultiHead_0},
            ProgNode::Rel{rel: __MultiHead_1},
            ProgNode::Rel{rel: Errors},
            ProgNode::Rel{rel: Functions},
            ProgNode::SCC{rels: vec![RecursiveRelation{rel: Statements, distinct: true}, RecursiveRelation{rel: __MultiHead_3, distinct: true}]},
            ProgNode::Rel{rel: SymbolTable},
            ProgNode::Rel{rel: __Null}
        ],
        init_data: vec![
        ]
    }
}