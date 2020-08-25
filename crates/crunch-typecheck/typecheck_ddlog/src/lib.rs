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
    let InputExpressions = Relation {
        name: "InputExpressions".to_string(),
        input: true,
        distinct: false,
        caching_mode: CachingMode::Set,
        key_func: None,
        id: Relations::InputExpressions as RelId,
        rules: vec![],
        arrangements: vec![],
        change_cb: None,
    };
    let OutputExpressions = Relation {
                                name:         "OutputExpressions".to_string(),
                                input:        false,
                                distinct:     false,
                                caching_mode: CachingMode::Set,
                                key_func:     None,
                                id:           Relations::OutputExpressions as RelId,
                                rules:        vec![
                                    /* OutputExpressions[(OutputExpressions{.id=id, .kind=kind, .ty=ty}: OutputExpressions)] :- InputExpressions[(InputExpressions{.id=(id: bit<64>), .kind=(kind: internment::Intern<hir::ExprKind>), .ty=(ty: internment::Intern<hir::TypeKind>)}: InputExpressions)]. */
                                    Rule::CollectionRule {
                                        description: "OutputExpressions[(OutputExpressions{.id=id, .kind=kind, .ty=ty}: OutputExpressions)] :- InputExpressions[(InputExpressions{.id=(id: bit<64>), .kind=(kind: internment::Intern<hir::ExprKind>), .ty=(ty: internment::Intern<hir::TypeKind>)}: InputExpressions)].".to_string(),
                                        rel: Relations::InputExpressions as RelId,
                                        xform: Some(XFormCollection::FilterMap{
                                                        description: "head of OutputExpressions[(OutputExpressions{.id=id, .kind=kind, .ty=ty}: OutputExpressions)] :- InputExpressions[(InputExpressions{.id=(id: bit<64>), .kind=(kind: internment::Intern<hir::ExprKind>), .ty=(ty: internment::Intern<hir::TypeKind>)}: InputExpressions)]." .to_string(),
                                                        fmfun: &{fn __f(__v: DDValue) -> Option<DDValue>
                                                        {
                                                            let (ref id, ref kind, ref ty) = match unsafe {  Value::InputExpressions::from_ddvalue_ref(&__v) }.0 {
                                                                InputExpressions{id: ref id, kind: ref kind, ty: ref ty} => ((*id).clone(), (*kind).clone(), (*ty).clone()),
                                                                _ => return None
                                                            };
                                                            Some(Value::OutputExpressions((OutputExpressions{id: (*id).clone(), kind: (*kind).clone(), ty: (*ty).clone()})).into_ddvalue())
                                                        }
                                                        __f},
                                                        next: Box::new(None)
                                                    })
                                    }],
                                arrangements: vec![
                                    ],
                                change_cb:    Some(sync::Arc::new(sync::Mutex::new(__update_cb.clone())))
                            };
    let PropagateExprType = Relation {
                                name:         "PropagateExprType".to_string(),
                                input:        false,
                                distinct:     false,
                                caching_mode: CachingMode::Set,
                                key_func:     None,
                                id:           Relations::PropagateExprType as RelId,
                                rules:        vec![
                                    /* PropagateExprType[(PropagateExprType{.id=id, .kind=kind, .ty=ty}: PropagateExprType)] :- InputExpressions[(InputExpressions{.id=(id: bit<64>), .kind=(kind: internment::Intern<hir::ExprKind>), .ty=(_: internment::Intern<hir::TypeKind>)}: InputExpressions)], (hir::ExprLit{.lit=(var lit: internment::Intern<hir::Literal>)}: hir::ExprKind) = (internment::ival(kind): hir::ExprKind), (var ty: internment::Intern<hir::TypeKind>) = hir::typeof(lit). */
                                    Rule::CollectionRule {
                                        description: "PropagateExprType[(PropagateExprType{.id=id, .kind=kind, .ty=ty}: PropagateExprType)] :- InputExpressions[(InputExpressions{.id=(id: bit<64>), .kind=(kind: internment::Intern<hir::ExprKind>), .ty=(_: internment::Intern<hir::TypeKind>)}: InputExpressions)], (hir::ExprLit{.lit=(var lit: internment::Intern<hir::Literal>)}: hir::ExprKind) = (internment::ival(kind): hir::ExprKind), (var ty: internment::Intern<hir::TypeKind>) = hir::typeof(lit).".to_string(),
                                        rel: Relations::InputExpressions as RelId,
                                        xform: Some(XFormCollection::FilterMap{
                                                        description: "head of PropagateExprType[(PropagateExprType{.id=id, .kind=kind, .ty=ty}: PropagateExprType)] :- InputExpressions[(InputExpressions{.id=(id: bit<64>), .kind=(kind: internment::Intern<hir::ExprKind>), .ty=(_: internment::Intern<hir::TypeKind>)}: InputExpressions)], (hir::ExprLit{.lit=(var lit: internment::Intern<hir::Literal>)}: hir::ExprKind) = (internment::ival(kind): hir::ExprKind), (var ty: internment::Intern<hir::TypeKind>) = hir::typeof(lit)." .to_string(),
                                                        fmfun: &{fn __f(__v: DDValue) -> Option<DDValue>
                                                        {
                                                            let (ref id, ref kind) = match unsafe {  Value::InputExpressions::from_ddvalue_ref(&__v) }.0 {
                                                                InputExpressions{id: ref id, kind: ref kind, ty: _} => ((*id).clone(), (*kind).clone()),
                                                                _ => return None
                                                            };
                                                            let ref lit: internment_Intern<hir_Literal> = match (*internment_ival(kind)).clone() {
                                                                hir_ExprKind::hir_ExprLit{lit: lit} => lit,
                                                                _ => return None
                                                            };
                                                            let ref ty: internment_Intern<hir_TypeKind> = match hir_typeof(lit) {
                                                                ty => ty,
                                                                _ => return None
                                                            };
                                                            Some(Value::PropagateExprType((PropagateExprType{id: (*id).clone(), kind: (*kind).clone(), ty: (*ty).clone()})).into_ddvalue())
                                                        }
                                                        __f},
                                                        next: Box::new(None)
                                                    })
                                    },
                                    /* PropagateExprType[(PropagateExprType{.id=id, .kind=kind, .ty=ty}: PropagateExprType)] :- InputExpressions[(InputExpressions{.id=(id: bit<64>), .kind=(kind: internment::Intern<hir::ExprKind>), .ty=(_: internment::Intern<hir::TypeKind>)}: InputExpressions)], (hir::ExprAssign{.variable=(_: bit<64>), .expr_id=(var expr_id: bit<64>)}: hir::ExprKind) = (internment::ival(kind): hir::ExprKind), PropagateExprType[(PropagateExprType{.id=(expr_id: bit<64>), .kind=(_: internment::Intern<hir::ExprKind>), .ty=(ty: internment::Intern<hir::TypeKind>)}: PropagateExprType)]. */
                                    Rule::CollectionRule {
                                        description: "PropagateExprType[(PropagateExprType{.id=id, .kind=kind, .ty=ty}: PropagateExprType)] :- InputExpressions[(InputExpressions{.id=(id: bit<64>), .kind=(kind: internment::Intern<hir::ExprKind>), .ty=(_: internment::Intern<hir::TypeKind>)}: InputExpressions)], (hir::ExprAssign{.variable=(_: bit<64>), .expr_id=(var expr_id: bit<64>)}: hir::ExprKind) = (internment::ival(kind): hir::ExprKind), PropagateExprType[(PropagateExprType{.id=(expr_id: bit<64>), .kind=(_: internment::Intern<hir::ExprKind>), .ty=(ty: internment::Intern<hir::TypeKind>)}: PropagateExprType)].".to_string(),
                                        rel: Relations::InputExpressions as RelId,
                                        xform: Some(XFormCollection::Arrange {
                                                        description: "arrange InputExpressions[(InputExpressions{.id=(id: bit<64>), .kind=(kind: internment::Intern<hir::ExprKind>), .ty=(_: internment::Intern<hir::TypeKind>)}: InputExpressions)] by (expr_id)" .to_string(),
                                                        afun: &{fn __f(__v: DDValue) -> Option<(DDValue,DDValue)>
                                                        {
                                                            let (ref id, ref kind) = match unsafe {  Value::InputExpressions::from_ddvalue_ref(&__v) }.0 {
                                                                InputExpressions{id: ref id, kind: ref kind, ty: _} => ((*id).clone(), (*kind).clone()),
                                                                _ => return None
                                                            };
                                                            let ref expr_id: u64 = match (*internment_ival(kind)).clone() {
                                                                hir_ExprKind::hir_ExprAssign{variable: _, expr_id: expr_id} => expr_id,
                                                                _ => return None
                                                            };
                                                            Some((Value::__Bitval64((*expr_id).clone()).into_ddvalue(), Value::__Tuple2____Bitval64_internment_Intern__hir_ExprKind(((*id).clone(), (*kind).clone())).into_ddvalue()))
                                                        }
                                                        __f},
                                                        next: Box::new(XFormArrangement::Join{
                                                                           description: "InputExpressions[(InputExpressions{.id=(id: bit<64>), .kind=(kind: internment::Intern<hir::ExprKind>), .ty=(_: internment::Intern<hir::TypeKind>)}: InputExpressions)], (hir::ExprAssign{.variable=(_: bit<64>), .expr_id=(var expr_id: bit<64>)}: hir::ExprKind) = (internment::ival(kind): hir::ExprKind), PropagateExprType[(PropagateExprType{.id=(expr_id: bit<64>), .kind=(_: internment::Intern<hir::ExprKind>), .ty=(ty: internment::Intern<hir::TypeKind>)}: PropagateExprType)]".to_string(),
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
                                    /* PropagateExprType[(PropagateExprType{.id=id, .kind=kind, .ty=ty}: PropagateExprType)] :- InputExpressions[(InputExpressions{.id=(id: bit<64>), .kind=(kind: internment::Intern<hir::ExprKind>), .ty=(_: internment::Intern<hir::TypeKind>)}: InputExpressions)], (hir::ExprVar{.variable=(var expr_var: bit<64>)}: hir::ExprKind) = (internment::ival(kind): hir::ExprKind), PropagateExprType[(PropagateExprType{.id=(_: bit<64>), .kind=(prop_kind: internment::Intern<hir::ExprKind>), .ty=(ty: internment::Intern<hir::TypeKind>)}: PropagateExprType)], (hir::ExprAssign{.variable=(var prop_var: bit<64>), .expr_id=(_: bit<64>)}: hir::ExprKind) = (internment::ival(prop_kind): hir::ExprKind), (expr_var == prop_var). */
                                    Rule::CollectionRule {
                                        description: "PropagateExprType[(PropagateExprType{.id=id, .kind=kind, .ty=ty}: PropagateExprType)] :- InputExpressions[(InputExpressions{.id=(id: bit<64>), .kind=(kind: internment::Intern<hir::ExprKind>), .ty=(_: internment::Intern<hir::TypeKind>)}: InputExpressions)], (hir::ExprVar{.variable=(var expr_var: bit<64>)}: hir::ExprKind) = (internment::ival(kind): hir::ExprKind), PropagateExprType[(PropagateExprType{.id=(_: bit<64>), .kind=(prop_kind: internment::Intern<hir::ExprKind>), .ty=(ty: internment::Intern<hir::TypeKind>)}: PropagateExprType)], (hir::ExprAssign{.variable=(var prop_var: bit<64>), .expr_id=(_: bit<64>)}: hir::ExprKind) = (internment::ival(prop_kind): hir::ExprKind), (expr_var == prop_var).".to_string(),
                                        rel: Relations::InputExpressions as RelId,
                                        xform: Some(XFormCollection::Arrange {
                                                        description: "arrange InputExpressions[(InputExpressions{.id=(id: bit<64>), .kind=(kind: internment::Intern<hir::ExprKind>), .ty=(_: internment::Intern<hir::TypeKind>)}: InputExpressions)] by ()" .to_string(),
                                                        afun: &{fn __f(__v: DDValue) -> Option<(DDValue,DDValue)>
                                                        {
                                                            let (ref id, ref kind) = match unsafe {  Value::InputExpressions::from_ddvalue_ref(&__v) }.0 {
                                                                InputExpressions{id: ref id, kind: ref kind, ty: _} => ((*id).clone(), (*kind).clone()),
                                                                _ => return None
                                                            };
                                                            let ref expr_var: u64 = match (*internment_ival(kind)).clone() {
                                                                hir_ExprKind::hir_ExprVar{variable: expr_var} => expr_var,
                                                                _ => return None
                                                            };
                                                            Some((Value::__Tuple0__(()).into_ddvalue(), Value::__Tuple3____Bitval64_internment_Intern__hir_ExprKind___Bitval64(((*id).clone(), (*kind).clone(), (*expr_var).clone())).into_ddvalue()))
                                                        }
                                                        __f},
                                                        next: Box::new(XFormArrangement::Join{
                                                                           description: "InputExpressions[(InputExpressions{.id=(id: bit<64>), .kind=(kind: internment::Intern<hir::ExprKind>), .ty=(_: internment::Intern<hir::TypeKind>)}: InputExpressions)], (hir::ExprVar{.variable=(var expr_var: bit<64>)}: hir::ExprKind) = (internment::ival(kind): hir::ExprKind), PropagateExprType[(PropagateExprType{.id=(_: bit<64>), .kind=(prop_kind: internment::Intern<hir::ExprKind>), .ty=(ty: internment::Intern<hir::TypeKind>)}: PropagateExprType)]".to_string(),
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
                                                                                   hir_ExprKind::hir_ExprAssign{variable: prop_var, expr_id: _} => prop_var,
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
                                                          let ref ty: internment_Intern<hir_TypeKind> = match (*(&*__STATIC_3)).clone() {
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
                                                          if !(!hir_is_int_internment_Intern__hir_TypeKind_1(ty)) {return None;};
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
    let InputFunctions = Relation {
                             name:         "InputFunctions".to_string(),
                             input:        true,
                             distinct:     false,
                             caching_mode: CachingMode::Set,
                             key_func:     None,
                             id:           Relations::InputFunctions as RelId,
                             rules:        vec![
                                 ],
                             arrangements: vec![
                                 Arrangement::Map{
                                    name: r###"(InputFunctions{.id=(_0: bit<64>), .func=(_: internment::Intern<hir::Function>)}: InputFunctions) /*join*/"###.to_string(),
                                     afun: &{fn __f(__v: DDValue) -> Option<(DDValue,DDValue)>
                                     {
                                         let __cloned = __v.clone();
                                         match unsafe { Value::InputFunctions::from_ddvalue(__v) }.0 {
                                             InputFunctions{id: ref _0, func: _} => Some(Value::__Bitval64((*_0).clone()).into_ddvalue()),
                                             _ => None
                                         }.map(|x|(x,__cloned))
                                     }
                                     __f},
                                     queryable: false
                                 }],
                             change_cb:    None
                         };
    let OutputFunctions = Relation {
                              name:         "OutputFunctions".to_string(),
                              input:        false,
                              distinct:     false,
                              caching_mode: CachingMode::Set,
                              key_func:     None,
                              id:           Relations::OutputFunctions as RelId,
                              rules:        vec![
                                  /* OutputFunctions[(OutputFunctions{.id=id, .func=func}: OutputFunctions)] :- InputFunctions[(InputFunctions{.id=(id: bit<64>), .func=(func: internment::Intern<hir::Function>)}: InputFunctions)]. */
                                  Rule::CollectionRule {
                                      description: "OutputFunctions[(OutputFunctions{.id=id, .func=func}: OutputFunctions)] :- InputFunctions[(InputFunctions{.id=(id: bit<64>), .func=(func: internment::Intern<hir::Function>)}: InputFunctions)].".to_string(),
                                      rel: Relations::InputFunctions as RelId,
                                      xform: Some(XFormCollection::FilterMap{
                                                      description: "head of OutputFunctions[(OutputFunctions{.id=id, .func=func}: OutputFunctions)] :- InputFunctions[(InputFunctions{.id=(id: bit<64>), .func=(func: internment::Intern<hir::Function>)}: InputFunctions)]." .to_string(),
                                                      fmfun: &{fn __f(__v: DDValue) -> Option<DDValue>
                                                      {
                                                          let (ref id, ref func) = match unsafe {  Value::InputFunctions::from_ddvalue_ref(&__v) }.0 {
                                                              InputFunctions{id: ref id, func: ref func} => ((*id).clone(), (*func).clone()),
                                                              _ => return None
                                                          };
                                                          Some(Value::OutputFunctions((OutputFunctions{id: (*id).clone(), func: (*func).clone()})).into_ddvalue())
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
                             Arrangement::Map{
                                name: r###"(InputItems{.id=(_: bit<64>), .item=(hir::ItemFunc{.func=(_0: bit<64>)}: hir::Item)}: InputItems) /*join*/"###.to_string(),
                                 afun: &{fn __f(__v: DDValue) -> Option<(DDValue,DDValue)>
                                 {
                                     let __cloned = __v.clone();
                                     match unsafe { Value::InputItems::from_ddvalue(__v) }.0 {
                                         InputItems{id: _, item: hir_Item::hir_ItemFunc{func: ref _0}} => Some(Value::__Bitval64((*_0).clone()).into_ddvalue()),
                                         _ => None
                                     }.map(|x|(x,__cloned))
                                 }
                                 __f},
                                 queryable: false
                             }],
                         change_cb:    None
                     };
    let __MultiHead_3 = Relation {
                            name:         "__MultiHead_3".to_string(),
                            input:        false,
                            distinct:     false,
                            caching_mode: CachingMode::Set,
                            key_func:     None,
                            id:           Relations::__MultiHead_3 as RelId,
                            rules:        vec![
                                /* __MultiHead_3[((path: internment::Intern<std::Vec<hir::StrT>>), (signature: hir::Signature), (func_id: bit<64>))] :- InputItems[(InputItems{.id=(_: bit<64>), .item=(hir::ItemFunc{.func=(func_id: bit<64>)}: hir::Item)}: InputItems)], InputFunctions[(InputFunctions{.id=(func_id: bit<64>), .func=(func: internment::Intern<hir::Function>)}: InputFunctions)], (not hir::has_unknown_types(func)), (var path: internment::Intern<std::Vec<hir::StrT>>) = hir::path(func), (var signature: hir::Signature) = hir::signature(func). */
                                Rule::ArrangementRule {
                                    description: "__MultiHead_3[((path: internment::Intern<std::Vec<hir::StrT>>), (signature: hir::Signature), (func_id: bit<64>))] :- InputItems[(InputItems{.id=(_: bit<64>), .item=(hir::ItemFunc{.func=(func_id: bit<64>)}: hir::Item)}: InputItems)], InputFunctions[(InputFunctions{.id=(func_id: bit<64>), .func=(func: internment::Intern<hir::Function>)}: InputFunctions)], (not hir::has_unknown_types(func)), (var path: internment::Intern<std::Vec<hir::StrT>>) = hir::path(func), (var signature: hir::Signature) = hir::signature(func).".to_string(),
                                    arr: ( Relations::InputItems as RelId, 0),
                                    xform: XFormArrangement::Join{
                                               description: "InputItems[(InputItems{.id=(_: bit<64>), .item=(hir::ItemFunc{.func=(func_id: bit<64>)}: hir::Item)}: InputItems)], InputFunctions[(InputFunctions{.id=(func_id: bit<64>), .func=(func: internment::Intern<hir::Function>)}: InputFunctions)]".to_string(),
                                               ffun: None,
                                               arrangement: (Relations::InputFunctions as RelId,0),
                                               jfun: &{fn __f(_: &DDValue ,__v1: &DDValue,__v2: &DDValue) -> Option<DDValue>
                                               {
                                                   let ref func_id = match unsafe {  Value::InputItems::from_ddvalue_ref(__v1) }.0 {
                                                       InputItems{id: _, item: hir_Item::hir_ItemFunc{func: ref func_id}} => (*func_id).clone(),
                                                       _ => return None
                                                   };
                                                   let ref func = match unsafe {  Value::InputFunctions::from_ddvalue_ref(__v2) }.0 {
                                                       InputFunctions{id: _, func: ref func} => (*func).clone(),
                                                       _ => return None
                                                   };
                                                   if !(!hir_has_unknown_types(func)) {return None;};
                                                   let ref path: internment_Intern<std_Vec<hir_StrT>> = match hir_path(func) {
                                                       path => path,
                                                       _ => return None
                                                   };
                                                   let ref signature: hir_Signature = match hir_signature(func) {
                                                       signature => signature,
                                                       _ => return None
                                                   };
                                                   Some(Value::__Tuple3__internment_Intern__std_Vec____Bitval32_hir_Signature___Bitval64(((*path).clone(), (*signature).clone(), (*func_id).clone())).into_ddvalue())
                                               }
                                               __f},
                                               next: Box::new(None)
                                           }
                                }],
                            arrangements: vec![
                                ],
                            change_cb:    None
                        };
    let __MultiHead_4 = Relation {
                            name:         "__MultiHead_4".to_string(),
                            input:        false,
                            distinct:     false,
                            caching_mode: CachingMode::Set,
                            key_func:     None,
                            id:           Relations::__MultiHead_4 as RelId,
                            rules:        vec![
                                /* __MultiHead_4[((path: internment::Intern<std::Vec<hir::StrT>>), (signature: hir::Signature), (message: string), (func_id: bit<64>))] :- InputItems[(InputItems{.id=(_: bit<64>), .item=(hir::ItemFunc{.func=(func_id: bit<64>)}: hir::Item)}: InputItems)], InputFunctions[(InputFunctions{.id=(func_id: bit<64>), .func=(func: internment::Intern<hir::Function>)}: InputFunctions)], hir::has_unknown_types(func), var message = FlatMap(hir::unknown_type_errors(func)), (var path: internment::Intern<std::Vec<hir::StrT>>) = hir::path(func), (var signature: hir::Signature) = hir::corrected_signature(func). */
                                Rule::ArrangementRule {
                                    description: "__MultiHead_4[((path: internment::Intern<std::Vec<hir::StrT>>), (signature: hir::Signature), (message: string), (func_id: bit<64>))] :- InputItems[(InputItems{.id=(_: bit<64>), .item=(hir::ItemFunc{.func=(func_id: bit<64>)}: hir::Item)}: InputItems)], InputFunctions[(InputFunctions{.id=(func_id: bit<64>), .func=(func: internment::Intern<hir::Function>)}: InputFunctions)], hir::has_unknown_types(func), var message = FlatMap(hir::unknown_type_errors(func)), (var path: internment::Intern<std::Vec<hir::StrT>>) = hir::path(func), (var signature: hir::Signature) = hir::corrected_signature(func).".to_string(),
                                    arr: ( Relations::InputItems as RelId, 0),
                                    xform: XFormArrangement::Join{
                                               description: "InputItems[(InputItems{.id=(_: bit<64>), .item=(hir::ItemFunc{.func=(func_id: bit<64>)}: hir::Item)}: InputItems)], InputFunctions[(InputFunctions{.id=(func_id: bit<64>), .func=(func: internment::Intern<hir::Function>)}: InputFunctions)]".to_string(),
                                               ffun: None,
                                               arrangement: (Relations::InputFunctions as RelId,0),
                                               jfun: &{fn __f(_: &DDValue ,__v1: &DDValue,__v2: &DDValue) -> Option<DDValue>
                                               {
                                                   let ref func_id = match unsafe {  Value::InputItems::from_ddvalue_ref(__v1) }.0 {
                                                       InputItems{id: _, item: hir_Item::hir_ItemFunc{func: ref func_id}} => (*func_id).clone(),
                                                       _ => return None
                                                   };
                                                   let ref func = match unsafe {  Value::InputFunctions::from_ddvalue_ref(__v2) }.0 {
                                                       InputFunctions{id: _, func: ref func} => (*func).clone(),
                                                       _ => return None
                                                   };
                                                   if !hir_has_unknown_types(func) {return None;};
                                                   Some(Value::__Tuple2____Bitval64_internment_Intern__hir_Function(((*func_id).clone(), (*func).clone())).into_ddvalue())
                                               }
                                               __f},
                                               next: Box::new(Some(XFormCollection::FlatMap{
                                                                       description: "InputItems[(InputItems{.id=(_: bit<64>), .item=(hir::ItemFunc{.func=(func_id: bit<64>)}: hir::Item)}: InputItems)], InputFunctions[(InputFunctions{.id=(func_id: bit<64>), .func=(func: internment::Intern<hir::Function>)}: InputFunctions)], hir::has_unknown_types(func), var message = FlatMap(hir::unknown_type_errors(func))" .to_string(),
                                                                       fmfun: &{fn __f(__v: DDValue) -> Option<Box<dyn Iterator<Item=DDValue>>>
                                                                       {
                                                                           let (ref func_id, ref func) = unsafe { Value::__Tuple2____Bitval64_internment_Intern__hir_Function::from_ddvalue_ref( &__v ) }.0;
                                                                           let __flattened = hir_unknown_type_errors(func);
                                                                           let func_id = (*func_id).clone();
                                                                           let func = (*func).clone();
                                                                           Some(Box::new(__flattened.into_iter().map(move |message|Value::__Tuple3____Stringval___Bitval64_internment_Intern__hir_Function((message.clone(), func_id.clone(), func.clone())).into_ddvalue())))
                                                                       }
                                                                       __f},
                                                                       next: Box::new(Some(XFormCollection::FilterMap{
                                                                                               description: "head of __MultiHead_4[((path: internment::Intern<std::Vec<hir::StrT>>), (signature: hir::Signature), (message: string), (func_id: bit<64>))] :- InputItems[(InputItems{.id=(_: bit<64>), .item=(hir::ItemFunc{.func=(func_id: bit<64>)}: hir::Item)}: InputItems)], InputFunctions[(InputFunctions{.id=(func_id: bit<64>), .func=(func: internment::Intern<hir::Function>)}: InputFunctions)], hir::has_unknown_types(func), var message = FlatMap(hir::unknown_type_errors(func)), (var path: internment::Intern<std::Vec<hir::StrT>>) = hir::path(func), (var signature: hir::Signature) = hir::corrected_signature(func)." .to_string(),
                                                                                               fmfun: &{fn __f(__v: DDValue) -> Option<DDValue>
                                                                                               {
                                                                                                   let (ref message, ref func_id, ref func) = unsafe { Value::__Tuple3____Stringval___Bitval64_internment_Intern__hir_Function::from_ddvalue_ref( &__v ) }.0;
                                                                                                   let ref path: internment_Intern<std_Vec<hir_StrT>> = match hir_path(func) {
                                                                                                       path => path,
                                                                                                       _ => return None
                                                                                                   };
                                                                                                   let ref signature: hir_Signature = match hir_corrected_signature(func) {
                                                                                                       signature => signature,
                                                                                                       _ => return None
                                                                                                   };
                                                                                                   Some(Value::__Tuple4__internment_Intern__std_Vec____Bitval32_hir_Signature___Stringval___Bitval64(((*path).clone(), (*signature).clone(), (*message).clone(), (*func_id).clone())).into_ddvalue())
                                                                                               }
                                                                                               __f},
                                                                                               next: Box::new(None)
                                                                                           }))
                                                                   }))
                                           }
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
                         /* Errors[(Errors{.message=message}: Errors)] :- __MultiHead_4[((path: internment::Intern<std::Vec<hir::StrT>>), (signature: hir::Signature), (message: string), (func_id: bit<64>))]. */
                         Rule::CollectionRule {
                             description: "Errors[(Errors{.message=message}: Errors)] :- __MultiHead_4[((path: internment::Intern<std::Vec<hir::StrT>>), (signature: hir::Signature), (message: string), (func_id: bit<64>))].".to_string(),
                             rel: Relations::__MultiHead_4 as RelId,
                             xform: Some(XFormCollection::FilterMap{
                                             description: "head of Errors[(Errors{.message=message}: Errors)] :- __MultiHead_4[((path: internment::Intern<std::Vec<hir::StrT>>), (signature: hir::Signature), (message: string), (func_id: bit<64>))]." .to_string(),
                                             fmfun: &{fn __f(__v: DDValue) -> Option<DDValue>
                                             {
                                                 let (ref path, ref signature, ref message, ref func_id) = match unsafe {  Value::__Tuple4__internment_Intern__std_Vec____Bitval32_hir_Signature___Stringval___Bitval64::from_ddvalue_ref(&__v) }.0 {
                                                     (ref path, ref signature, ref message, ref func_id) => ((*path).clone(), (*signature).clone(), (*message).clone(), (*func_id).clone()),
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
                            /* Functions[(Functions{.func=func_id}: Functions)] :- __MultiHead_3[((path: internment::Intern<std::Vec<hir::StrT>>), (signature: hir::Signature), (func_id: bit<64>))]. */
                            Rule::CollectionRule {
                                description: "Functions[(Functions{.func=func_id}: Functions)] :- __MultiHead_3[((path: internment::Intern<std::Vec<hir::StrT>>), (signature: hir::Signature), (func_id: bit<64>))].".to_string(),
                                rel: Relations::__MultiHead_3 as RelId,
                                xform: Some(XFormCollection::FilterMap{
                                                description: "head of Functions[(Functions{.func=func_id}: Functions)] :- __MultiHead_3[((path: internment::Intern<std::Vec<hir::StrT>>), (signature: hir::Signature), (func_id: bit<64>))]." .to_string(),
                                                fmfun: &{fn __f(__v: DDValue) -> Option<DDValue>
                                                {
                                                    let (ref path, ref signature, ref func_id) = match unsafe {  Value::__Tuple3__internment_Intern__std_Vec____Bitval32_hir_Signature___Bitval64::from_ddvalue_ref(&__v) }.0 {
                                                        (ref path, ref signature, ref func_id) => ((*path).clone(), (*signature).clone(), (*func_id).clone()),
                                                        _ => return None
                                                    };
                                                    Some(Value::Functions((Functions{func: (*func_id).clone()})).into_ddvalue())
                                                }
                                                __f},
                                                next: Box::new(None)
                                            })
                            },
                            /* Functions[(Functions{.func=func_id}: Functions)] :- __MultiHead_4[((path: internment::Intern<std::Vec<hir::StrT>>), (signature: hir::Signature), (message: string), (func_id: bit<64>))]. */
                            Rule::CollectionRule {
                                description: "Functions[(Functions{.func=func_id}: Functions)] :- __MultiHead_4[((path: internment::Intern<std::Vec<hir::StrT>>), (signature: hir::Signature), (message: string), (func_id: bit<64>))].".to_string(),
                                rel: Relations::__MultiHead_4 as RelId,
                                xform: Some(XFormCollection::FilterMap{
                                                description: "head of Functions[(Functions{.func=func_id}: Functions)] :- __MultiHead_4[((path: internment::Intern<std::Vec<hir::StrT>>), (signature: hir::Signature), (message: string), (func_id: bit<64>))]." .to_string(),
                                                fmfun: &{fn __f(__v: DDValue) -> Option<DDValue>
                                                {
                                                    let (ref path, ref signature, ref message, ref func_id) = match unsafe {  Value::__Tuple4__internment_Intern__std_Vec____Bitval32_hir_Signature___Stringval___Bitval64::from_ddvalue_ref(&__v) }.0 {
                                                        (ref path, ref signature, ref message, ref func_id) => ((*path).clone(), (*signature).clone(), (*message).clone(), (*func_id).clone()),
                                                        _ => return None
                                                    };
                                                    Some(Value::Functions((Functions{func: (*func_id).clone()})).into_ddvalue())
                                                }
                                                __f},
                                                next: Box::new(None)
                                            })
                            }],
                        arrangements: vec![
                            ],
                        change_cb:    Some(sync::Arc::new(sync::Mutex::new(__update_cb.clone())))
                    };
    let SymbolTable = Relation {
                          name:         "SymbolTable".to_string(),
                          input:        false,
                          distinct:     true,
                          caching_mode: CachingMode::Set,
                          key_func:     None,
                          id:           Relations::SymbolTable as RelId,
                          rules:        vec![
                              /* SymbolTable[(SymbolTable{.path=path, .signature=signature}: SymbolTable)] :- __MultiHead_3[((path: internment::Intern<std::Vec<hir::StrT>>), (signature: hir::Signature), (func_id: bit<64>))]. */
                              Rule::CollectionRule {
                                  description: "SymbolTable[(SymbolTable{.path=path, .signature=signature}: SymbolTable)] :- __MultiHead_3[((path: internment::Intern<std::Vec<hir::StrT>>), (signature: hir::Signature), (func_id: bit<64>))].".to_string(),
                                  rel: Relations::__MultiHead_3 as RelId,
                                  xform: Some(XFormCollection::FilterMap{
                                                  description: "head of SymbolTable[(SymbolTable{.path=path, .signature=signature}: SymbolTable)] :- __MultiHead_3[((path: internment::Intern<std::Vec<hir::StrT>>), (signature: hir::Signature), (func_id: bit<64>))]." .to_string(),
                                                  fmfun: &{fn __f(__v: DDValue) -> Option<DDValue>
                                                  {
                                                      let (ref path, ref signature, ref func_id) = match unsafe {  Value::__Tuple3__internment_Intern__std_Vec____Bitval32_hir_Signature___Bitval64::from_ddvalue_ref(&__v) }.0 {
                                                          (ref path, ref signature, ref func_id) => ((*path).clone(), (*signature).clone(), (*func_id).clone()),
                                                          _ => return None
                                                      };
                                                      Some(Value::SymbolTable((SymbolTable{path: (*path).clone(), signature: (*signature).clone()})).into_ddvalue())
                                                  }
                                                  __f},
                                                  next: Box::new(None)
                                              })
                              },
                              /* SymbolTable[(SymbolTable{.path=path, .signature=signature}: SymbolTable)] :- __MultiHead_4[((path: internment::Intern<std::Vec<hir::StrT>>), (signature: hir::Signature), (message: string), (func_id: bit<64>))]. */
                              Rule::CollectionRule {
                                  description: "SymbolTable[(SymbolTable{.path=path, .signature=signature}: SymbolTable)] :- __MultiHead_4[((path: internment::Intern<std::Vec<hir::StrT>>), (signature: hir::Signature), (message: string), (func_id: bit<64>))].".to_string(),
                                  rel: Relations::__MultiHead_4 as RelId,
                                  xform: Some(XFormCollection::FilterMap{
                                                  description: "head of SymbolTable[(SymbolTable{.path=path, .signature=signature}: SymbolTable)] :- __MultiHead_4[((path: internment::Intern<std::Vec<hir::StrT>>), (signature: hir::Signature), (message: string), (func_id: bit<64>))]." .to_string(),
                                                  fmfun: &{fn __f(__v: DDValue) -> Option<DDValue>
                                                  {
                                                      let (ref path, ref signature, ref message, ref func_id) = match unsafe {  Value::__Tuple4__internment_Intern__std_Vec____Bitval32_hir_Signature___Stringval___Bitval64::from_ddvalue_ref(&__v) }.0 {
                                                          (ref path, ref signature, ref message, ref func_id) => ((*path).clone(), (*signature).clone(), (*message).clone(), (*func_id).clone()),
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
    let InputStatements = Relation {
                              name:         "InputStatements".to_string(),
                              input:        true,
                              distinct:     false,
                              caching_mode: CachingMode::Set,
                              key_func:     None,
                              id:           Relations::InputStatements as RelId,
                              rules:        vec![
                                  ],
                              arrangements: vec![
                                  Arrangement::Map{
                                     name: r###"(InputStatements{.id=(_0: bit<64>), .stmt=(_: internment::Intern<hir::Stmt>), .scope=(_: internment::Intern<hir::Scope>)}: InputStatements) /*join*/"###.to_string(),
                                      afun: &{fn __f(__v: DDValue) -> Option<(DDValue,DDValue)>
                                      {
                                          let __cloned = __v.clone();
                                          match unsafe { Value::InputStatements::from_ddvalue(__v) }.0 {
                                              InputStatements{id: ref _0, stmt: _, scope: _} => Some(Value::__Bitval64((*_0).clone()).into_ddvalue()),
                                              _ => None
                                          }.map(|x|(x,__cloned))
                                      }
                                      __f},
                                      queryable: false
                                  }],
                              change_cb:    None
                          };
    let Statements = Relation {
                         name:         "Statements".to_string(),
                         input:        false,
                         distinct:     false,
                         caching_mode: CachingMode::Set,
                         key_func:     None,
                         id:           Relations::Statements as RelId,
                         rules:        vec![
                             /* Statements[(Statements{.stmt=stmt, .scope=(internment::intern((hir::ScopeFunction{.func=func_id}: hir::Scope)): internment::Intern<hir::Scope>)}: Statements)] :- InputFunctions[(InputFunctions{.id=(func_id: bit<64>), .func=(func: internment::Intern<hir::Function>)}: InputFunctions)], (hir::Function{.name=(_: internment::Intern<std::Vec<hir::StrT>>), .vis=(_: hir::Vis), .args=(_: std::Vec<hir::FuncArg>), .body=(var stmt_id: bit<64>), .ret=(_: internment::Intern<hir::TypeKind>)}: hir::Function) = (internment::ival(func): hir::Function), InputStatements[(InputStatements{.id=(stmt_id: bit<64>), .stmt=(stmt: internment::Intern<hir::Stmt>), .scope=(_: internment::Intern<hir::Scope>)}: InputStatements)]. */
                             Rule::CollectionRule {
                                 description: "Statements[(Statements{.stmt=stmt, .scope=(internment::intern((hir::ScopeFunction{.func=func_id}: hir::Scope)): internment::Intern<hir::Scope>)}: Statements)] :- InputFunctions[(InputFunctions{.id=(func_id: bit<64>), .func=(func: internment::Intern<hir::Function>)}: InputFunctions)], (hir::Function{.name=(_: internment::Intern<std::Vec<hir::StrT>>), .vis=(_: hir::Vis), .args=(_: std::Vec<hir::FuncArg>), .body=(var stmt_id: bit<64>), .ret=(_: internment::Intern<hir::TypeKind>)}: hir::Function) = (internment::ival(func): hir::Function), InputStatements[(InputStatements{.id=(stmt_id: bit<64>), .stmt=(stmt: internment::Intern<hir::Stmt>), .scope=(_: internment::Intern<hir::Scope>)}: InputStatements)].".to_string(),
                                 rel: Relations::InputFunctions as RelId,
                                 xform: Some(XFormCollection::Arrange {
                                                 description: "arrange InputFunctions[(InputFunctions{.id=(func_id: bit<64>), .func=(func: internment::Intern<hir::Function>)}: InputFunctions)] by (stmt_id)" .to_string(),
                                                 afun: &{fn __f(__v: DDValue) -> Option<(DDValue,DDValue)>
                                                 {
                                                     let (ref func_id, ref func) = match unsafe {  Value::InputFunctions::from_ddvalue_ref(&__v) }.0 {
                                                         InputFunctions{id: ref func_id, func: ref func} => ((*func_id).clone(), (*func).clone()),
                                                         _ => return None
                                                     };
                                                     let ref stmt_id: u64 = match (*internment_ival(func)).clone() {
                                                         hir_Function{name: _, vis: _, args: _, body: stmt_id, ret: _} => stmt_id,
                                                         _ => return None
                                                     };
                                                     Some((Value::__Bitval64((*stmt_id).clone()).into_ddvalue(), Value::__Bitval64((*func_id).clone()).into_ddvalue()))
                                                 }
                                                 __f},
                                                 next: Box::new(XFormArrangement::Join{
                                                                    description: "InputFunctions[(InputFunctions{.id=(func_id: bit<64>), .func=(func: internment::Intern<hir::Function>)}: InputFunctions)], (hir::Function{.name=(_: internment::Intern<std::Vec<hir::StrT>>), .vis=(_: hir::Vis), .args=(_: std::Vec<hir::FuncArg>), .body=(var stmt_id: bit<64>), .ret=(_: internment::Intern<hir::TypeKind>)}: hir::Function) = (internment::ival(func): hir::Function), InputStatements[(InputStatements{.id=(stmt_id: bit<64>), .stmt=(stmt: internment::Intern<hir::Stmt>), .scope=(_: internment::Intern<hir::Scope>)}: InputStatements)]".to_string(),
                                                                    ffun: None,
                                                                    arrangement: (Relations::InputStatements as RelId,0),
                                                                    jfun: &{fn __f(_: &DDValue ,__v1: &DDValue,__v2: &DDValue) -> Option<DDValue>
                                                                    {
                                                                        let ref func_id = unsafe { Value::__Bitval64::from_ddvalue_ref( __v1 ) }.0;
                                                                        let ref stmt = match unsafe {  Value::InputStatements::from_ddvalue_ref(__v2) }.0 {
                                                                            InputStatements{id: _, stmt: ref stmt, scope: _} => (*stmt).clone(),
                                                                            _ => return None
                                                                        };
                                                                        Some(Value::Statements((Statements{stmt: (*stmt).clone(), scope: internment_intern((&(hir_Scope::hir_ScopeFunction{func: (*func_id).clone()})))})).into_ddvalue())
                                                                    }
                                                                    __f},
                                                                    next: Box::new(None)
                                                                })
                                             })
                             },
                             /* Statements[(Statements{.stmt=stmt1, .scope=(internment::intern((hir::ScopeSeq1{.parent=parent}: hir::Scope)): internment::Intern<hir::Scope>)}: Statements)] :- __MultiHead_6[((parent: internment::Intern<hir::Scope>), (stmt1: internment::Intern<hir::Stmt>), (stmt2: internment::Intern<hir::Stmt>))]. */
                             Rule::CollectionRule {
                                 description: "Statements[(Statements{.stmt=stmt1, .scope=(internment::intern((hir::ScopeSeq1{.parent=parent}: hir::Scope)): internment::Intern<hir::Scope>)}: Statements)] :- __MultiHead_6[((parent: internment::Intern<hir::Scope>), (stmt1: internment::Intern<hir::Stmt>), (stmt2: internment::Intern<hir::Stmt>))].".to_string(),
                                 rel: Relations::__MultiHead_6 as RelId,
                                 xform: Some(XFormCollection::FilterMap{
                                                 description: "head of Statements[(Statements{.stmt=stmt1, .scope=(internment::intern((hir::ScopeSeq1{.parent=parent}: hir::Scope)): internment::Intern<hir::Scope>)}: Statements)] :- __MultiHead_6[((parent: internment::Intern<hir::Scope>), (stmt1: internment::Intern<hir::Stmt>), (stmt2: internment::Intern<hir::Stmt>))]." .to_string(),
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
                             /* Statements[(Statements{.stmt=stmt2, .scope=(internment::intern((hir::ScopeSeq2{.parent=parent}: hir::Scope)): internment::Intern<hir::Scope>)}: Statements)] :- __MultiHead_6[((parent: internment::Intern<hir::Scope>), (stmt1: internment::Intern<hir::Stmt>), (stmt2: internment::Intern<hir::Stmt>))]. */
                             Rule::CollectionRule {
                                 description: "Statements[(Statements{.stmt=stmt2, .scope=(internment::intern((hir::ScopeSeq2{.parent=parent}: hir::Scope)): internment::Intern<hir::Scope>)}: Statements)] :- __MultiHead_6[((parent: internment::Intern<hir::Scope>), (stmt1: internment::Intern<hir::Stmt>), (stmt2: internment::Intern<hir::Stmt>))].".to_string(),
                                 rel: Relations::__MultiHead_6 as RelId,
                                 xform: Some(XFormCollection::FilterMap{
                                                 description: "head of Statements[(Statements{.stmt=stmt2, .scope=(internment::intern((hir::ScopeSeq2{.parent=parent}: hir::Scope)): internment::Intern<hir::Scope>)}: Statements)] :- __MultiHead_6[((parent: internment::Intern<hir::Scope>), (stmt1: internment::Intern<hir::Stmt>), (stmt2: internment::Intern<hir::Stmt>))]." .to_string(),
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
    let __MultiHead_6 = Relation {
                            name:         "__MultiHead_6".to_string(),
                            input:        false,
                            distinct:     false,
                            caching_mode: CachingMode::Set,
                            key_func:     None,
                            id:           Relations::__MultiHead_6 as RelId,
                            rules:        vec![
                                /* __MultiHead_6[((parent: internment::Intern<hir::Scope>), (stmt1: internment::Intern<hir::Stmt>), (stmt2: internment::Intern<hir::Stmt>))] :- Statements[(Statements{.stmt=(stmt: internment::Intern<hir::Stmt>), .scope=(parent: internment::Intern<hir::Scope>)}: Statements)], (hir::StmtSeq{.first=(var stmt1_id: bit<64>), .second=(var stmt2_id: bit<64>)}: hir::Stmt) = (internment::ival(stmt): hir::Stmt), InputStatements[(InputStatements{.id=(stmt1_id: bit<64>), .stmt=(stmt1: internment::Intern<hir::Stmt>), .scope=(_: internment::Intern<hir::Scope>)}: InputStatements)], InputStatements[(InputStatements{.id=(stmt2_id: bit<64>), .stmt=(stmt2: internment::Intern<hir::Stmt>), .scope=(_: internment::Intern<hir::Scope>)}: InputStatements)]. */
                                Rule::CollectionRule {
                                    description: "__MultiHead_6[((parent: internment::Intern<hir::Scope>), (stmt1: internment::Intern<hir::Stmt>), (stmt2: internment::Intern<hir::Stmt>))] :- Statements[(Statements{.stmt=(stmt: internment::Intern<hir::Stmt>), .scope=(parent: internment::Intern<hir::Scope>)}: Statements)], (hir::StmtSeq{.first=(var stmt1_id: bit<64>), .second=(var stmt2_id: bit<64>)}: hir::Stmt) = (internment::ival(stmt): hir::Stmt), InputStatements[(InputStatements{.id=(stmt1_id: bit<64>), .stmt=(stmt1: internment::Intern<hir::Stmt>), .scope=(_: internment::Intern<hir::Scope>)}: InputStatements)], InputStatements[(InputStatements{.id=(stmt2_id: bit<64>), .stmt=(stmt2: internment::Intern<hir::Stmt>), .scope=(_: internment::Intern<hir::Scope>)}: InputStatements)].".to_string(),
                                    rel: Relations::Statements as RelId,
                                    xform: Some(XFormCollection::Arrange {
                                                    description: "arrange Statements[(Statements{.stmt=(stmt: internment::Intern<hir::Stmt>), .scope=(parent: internment::Intern<hir::Scope>)}: Statements)] by (stmt1_id)" .to_string(),
                                                    afun: &{fn __f(__v: DDValue) -> Option<(DDValue,DDValue)>
                                                    {
                                                        let (ref stmt, ref parent) = match unsafe {  Value::Statements::from_ddvalue_ref(&__v) }.0 {
                                                            Statements{stmt: ref stmt, scope: ref parent} => ((*stmt).clone(), (*parent).clone()),
                                                            _ => return None
                                                        };
                                                        let (ref stmt1_id, ref stmt2_id): (u64, u64) = match (*internment_ival(stmt)).clone() {
                                                            hir_Stmt::hir_StmtSeq{first: stmt1_id, second: stmt2_id} => (stmt1_id, stmt2_id),
                                                            _ => return None
                                                        };
                                                        Some((Value::__Bitval64((*stmt1_id).clone()).into_ddvalue(), Value::__Tuple2__internment_Intern__hir_Scope___Bitval64(((*parent).clone(), (*stmt2_id).clone())).into_ddvalue()))
                                                    }
                                                    __f},
                                                    next: Box::new(XFormArrangement::Join{
                                                                       description: "Statements[(Statements{.stmt=(stmt: internment::Intern<hir::Stmt>), .scope=(parent: internment::Intern<hir::Scope>)}: Statements)], (hir::StmtSeq{.first=(var stmt1_id: bit<64>), .second=(var stmt2_id: bit<64>)}: hir::Stmt) = (internment::ival(stmt): hir::Stmt), InputStatements[(InputStatements{.id=(stmt1_id: bit<64>), .stmt=(stmt1: internment::Intern<hir::Stmt>), .scope=(_: internment::Intern<hir::Scope>)}: InputStatements)]".to_string(),
                                                                       ffun: None,
                                                                       arrangement: (Relations::InputStatements as RelId,0),
                                                                       jfun: &{fn __f(_: &DDValue ,__v1: &DDValue,__v2: &DDValue) -> Option<DDValue>
                                                                       {
                                                                           let (ref parent, ref stmt2_id) = unsafe { Value::__Tuple2__internment_Intern__hir_Scope___Bitval64::from_ddvalue_ref( __v1 ) }.0;
                                                                           let ref stmt1 = match unsafe {  Value::InputStatements::from_ddvalue_ref(__v2) }.0 {
                                                                               InputStatements{id: _, stmt: ref stmt1, scope: _} => (*stmt1).clone(),
                                                                               _ => return None
                                                                           };
                                                                           Some(Value::__Tuple3__internment_Intern__hir_Scope___Bitval64_internment_Intern__hir_Stmt(((*parent).clone(), (*stmt2_id).clone(), (*stmt1).clone())).into_ddvalue())
                                                                       }
                                                                       __f},
                                                                       next: Box::new(Some(XFormCollection::Arrange {
                                                                                               description: "arrange Statements[(Statements{.stmt=(stmt: internment::Intern<hir::Stmt>), .scope=(parent: internment::Intern<hir::Scope>)}: Statements)], (hir::StmtSeq{.first=(var stmt1_id: bit<64>), .second=(var stmt2_id: bit<64>)}: hir::Stmt) = (internment::ival(stmt): hir::Stmt), InputStatements[(InputStatements{.id=(stmt1_id: bit<64>), .stmt=(stmt1: internment::Intern<hir::Stmt>), .scope=(_: internment::Intern<hir::Scope>)}: InputStatements)] by (stmt2_id)" .to_string(),
                                                                                               afun: &{fn __f(__v: DDValue) -> Option<(DDValue,DDValue)>
                                                                                               {
                                                                                                   let (ref parent, ref stmt2_id, ref stmt1) = unsafe { Value::__Tuple3__internment_Intern__hir_Scope___Bitval64_internment_Intern__hir_Stmt::from_ddvalue_ref( &__v ) }.0;
                                                                                                   Some((Value::__Bitval64((*stmt2_id).clone()).into_ddvalue(), Value::__Tuple2__internment_Intern__hir_Scope_internment_Intern__hir_Stmt(((*parent).clone(), (*stmt1).clone())).into_ddvalue()))
                                                                                               }
                                                                                               __f},
                                                                                               next: Box::new(XFormArrangement::Join{
                                                                                                                  description: "Statements[(Statements{.stmt=(stmt: internment::Intern<hir::Stmt>), .scope=(parent: internment::Intern<hir::Scope>)}: Statements)], (hir::StmtSeq{.first=(var stmt1_id: bit<64>), .second=(var stmt2_id: bit<64>)}: hir::Stmt) = (internment::ival(stmt): hir::Stmt), InputStatements[(InputStatements{.id=(stmt1_id: bit<64>), .stmt=(stmt1: internment::Intern<hir::Stmt>), .scope=(_: internment::Intern<hir::Scope>)}: InputStatements)], InputStatements[(InputStatements{.id=(stmt2_id: bit<64>), .stmt=(stmt2: internment::Intern<hir::Stmt>), .scope=(_: internment::Intern<hir::Scope>)}: InputStatements)]".to_string(),
                                                                                                                  ffun: None,
                                                                                                                  arrangement: (Relations::InputStatements as RelId,0),
                                                                                                                  jfun: &{fn __f(_: &DDValue ,__v1: &DDValue,__v2: &DDValue) -> Option<DDValue>
                                                                                                                  {
                                                                                                                      let (ref parent, ref stmt1) = unsafe { Value::__Tuple2__internment_Intern__hir_Scope_internment_Intern__hir_Stmt::from_ddvalue_ref( __v1 ) }.0;
                                                                                                                      let ref stmt2 = match unsafe {  Value::InputStatements::from_ddvalue_ref(__v2) }.0 {
                                                                                                                          InputStatements{id: _, stmt: ref stmt2, scope: _} => (*stmt2).clone(),
                                                                                                                          _ => return None
                                                                                                                      };
                                                                                                                      Some(Value::__Tuple3__internment_Intern__hir_Scope_internment_Intern__hir_Stmt_internment_Intern__hir_Stmt(((*parent).clone(), (*stmt1).clone(), (*stmt2).clone())).into_ddvalue())
                                                                                                                  }
                                                                                                                  __f},
                                                                                                                  next: Box::new(None)
                                                                                                              })
                                                                                           }))
                                                                   })
                                                })
                                }],
                            arrangements: vec![
                                ],
                            change_cb:    None
                        };
    let OutputStatements = Relation {
                               name:         "OutputStatements".to_string(),
                               input:        false,
                               distinct:     false,
                               caching_mode: CachingMode::Set,
                               key_func:     None,
                               id:           Relations::OutputStatements as RelId,
                               rules:        vec![
                                   /* OutputStatements[(OutputStatements{.id=id, .stmt=stmt, .scope=scope}: OutputStatements)] :- InputStatements[(InputStatements{.id=(id: bit<64>), .stmt=(stmt: internment::Intern<hir::Stmt>), .scope=(scope: internment::Intern<hir::Scope>)}: InputStatements)]. */
                                   Rule::CollectionRule {
                                       description: "OutputStatements[(OutputStatements{.id=id, .stmt=stmt, .scope=scope}: OutputStatements)] :- InputStatements[(InputStatements{.id=(id: bit<64>), .stmt=(stmt: internment::Intern<hir::Stmt>), .scope=(scope: internment::Intern<hir::Scope>)}: InputStatements)].".to_string(),
                                       rel: Relations::InputStatements as RelId,
                                       xform: Some(XFormCollection::FilterMap{
                                                       description: "head of OutputStatements[(OutputStatements{.id=id, .stmt=stmt, .scope=scope}: OutputStatements)] :- InputStatements[(InputStatements{.id=(id: bit<64>), .stmt=(stmt: internment::Intern<hir::Stmt>), .scope=(scope: internment::Intern<hir::Scope>)}: InputStatements)]." .to_string(),
                                                       fmfun: &{fn __f(__v: DDValue) -> Option<DDValue>
                                                       {
                                                           let (ref id, ref stmt, ref scope) = match unsafe {  Value::InputStatements::from_ddvalue_ref(&__v) }.0 {
                                                               InputStatements{id: ref id, stmt: ref stmt, scope: ref scope} => ((*id).clone(), (*stmt).clone(), (*scope).clone()),
                                                               _ => return None
                                                           };
                                                           Some(Value::OutputStatements((OutputStatements{id: (*id).clone(), stmt: (*stmt).clone(), scope: (*scope).clone()})).into_ddvalue())
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
            ProgNode::Rel {
                rel: InputExpressions,
            },
            ProgNode::Rel {
                rel: OutputExpressions,
            },
            ProgNode::SCC {
                rels: vec![RecursiveRelation {
                    rel: PropagateExprType,
                    distinct: true,
                }],
            },
            ProgNode::Rel {
                rel: ClampUnknownInt,
            },
            ProgNode::Rel {
                rel: InputFunctions,
            },
            ProgNode::Rel {
                rel: OutputFunctions,
            },
            ProgNode::Rel { rel: InputItems },
            ProgNode::Rel { rel: __MultiHead_3 },
            ProgNode::Rel { rel: __MultiHead_4 },
            ProgNode::Rel { rel: Errors },
            ProgNode::Rel { rel: Functions },
            ProgNode::Rel { rel: SymbolTable },
            ProgNode::Rel {
                rel: InputStatements,
            },
            ProgNode::SCC {
                rels: vec![
                    RecursiveRelation {
                        rel: Statements,
                        distinct: true,
                    },
                    RecursiveRelation {
                        rel: __MultiHead_6,
                        distinct: true,
                    },
                ],
            },
            ProgNode::Rel {
                rel: OutputStatements,
            },
            ProgNode::Rel { rel: __Null },
        ],
        init_data: vec![],
    }
}
