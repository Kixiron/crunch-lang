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
    clippy::missing_safety_doc,
    clippy::toplevel_ref_arg,
    clippy::double_parens,
    clippy::clone_on_copy,
    clippy::just_underscores_and_digits,
    clippy::match_single_binding,
    clippy::op_ref,
    clippy::nonminimal_bool,
    clippy::redundant_clone
)]

use num::bigint::BigInt;
use std::convert::TryFrom;
use std::hash::Hash;
use std::ops::Deref;
use std::ptr;
use std::result;
use std::{any::TypeId, sync};

use ordered_float::*;

use differential_dataflow::collection;
use timely::communication;
use timely::dataflow::scopes;
use timely::worker;

use differential_datalog::ddval::*;
use differential_datalog::program;
use differential_datalog::record;
use differential_datalog::record::FromRecord;
use differential_datalog::record::IntoRecord;
use differential_datalog::record::RelIdentifier;
use differential_datalog::record::UpdCmd;
use differential_datalog::DDlogConvert;
use num_traits::cast::FromPrimitive;
use num_traits::identities::One;
use once_cell::sync::Lazy;

use fnv::FnvHashMap;

pub mod api;
pub mod ovsdb_api;
pub mod update_handler;

use crate::api::updcmd2upd;

use serde::ser::SerializeTuple;
use serde::Deserialize;
use serde::Deserializer;
use serde::Serialize;
use serde::Serializer;

// This import is only needed to convince the OS X compiler to export
// `extern C` functions declared in ddlog_log.rs in the generated lib.
#[doc(hidden)]
#[cfg(feature = "c_api")]
pub use ddlog_log as hidden_ddlog_log;

/// A default implementation of `DDlogConvert` that just forwards calls
/// to generated functions of equal name.
#[derive(Debug)]
pub struct DDlogConverter {}

impl DDlogConvert for DDlogConverter {
    fn relid2name(relId: program::RelId) -> Option<&'static str> {
        relid2name(relId)
    }

    fn indexid2name(idxId: program::IdxId) -> Option<&'static str> {
        indexid2name(idxId)
    }

    fn updcmd2upd(upd_cmd: &UpdCmd) -> ::std::result::Result<program::Update<DDValue>, String> {
        updcmd2upd(upd_cmd)
    }
}

/* Wrapper around `Update<DDValue>` type that implements `Serialize` and `Deserialize`
 * traits.  It is currently only used by the distributed_ddlog crate in order to
 * serialize updates before sending them over the network and deserializing them on the
 * way back.  In other scenarios, the user either creates a `Update<DDValue>` type
 * themselves (when using the strongly typed DDlog API) or deserializes `Update<DDValue>`
 * from `Record` using `DDlogConvert::updcmd2upd()`.
 *
 * Why use a wrapper instead of implementing the traits for `Update<DDValue>` directly?
 * `Update<>` and `DDValue` types are both declared in the `differential_datalog` crate,
 * whereas the `Deserialize` implementation is program-specific and must be in one of the
 * generated crates, so we need a wrapper to avoid creating an orphan `impl`.
 *
 * Serialized representation: we currently only serialize `Insert` and `DeleteValue`
 * commands, represented in serialized form as (polarity, relid, value) tuple.  This way
 * the deserializer first reads relid and uses it to decide which value to deserialize
 * next.
 *
 * `impl Serialize` - serializes the value by forwarding `serialize` call to the `DDValue`
 * object (in fact, it is generic and could be in the `differential_datalog` crate, but we
 * keep it here to make it easier to keep it in sync with `Deserialize`).
 *
 * `impl Deserialize` - gets generated in `Compile.hs` using the macro below.  The macro
 * takes a list of `(relid, type)` and generates a match statement that uses type-specific
 * `Deserialize` for each `relid`.
 */
#[derive(Debug)]
pub struct UpdateSerializer(program::Update<DDValue>);

impl From<program::Update<DDValue>> for UpdateSerializer {
    fn from(u: program::Update<DDValue>) -> Self {
        UpdateSerializer(u)
    }
}
impl From<UpdateSerializer> for program::Update<DDValue> {
    fn from(u: UpdateSerializer) -> Self {
        u.0
    }
}

impl Serialize for UpdateSerializer {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let mut tup = serializer.serialize_tuple(3)?;
        match &self.0 {
            program::Update::Insert { relid, v } => {
                tup.serialize_element(&true)?;
                tup.serialize_element(relid)?;
                tup.serialize_element(v)?;
            }
            program::Update::DeleteValue { relid, v } => {
                tup.serialize_element(&false)?;
                tup.serialize_element(relid)?;
                tup.serialize_element(v)?;
            }
            _ => panic!("Cannot serialize InsertOrUpdate/Modify/DeleteKey update"),
        };
        tup.end()
    }
}

#[macro_export]
macro_rules! decl_update_deserializer {
    ( $n:ty, $(($rel:expr, $typ:ty)),* ) => {
        impl<'de> ::serde::Deserialize<'de> for $n {
            fn deserialize<D: ::serde::Deserializer<'de>>(deserializer: D) -> ::std::result::Result<Self, D::Error> {

                struct UpdateVisitor;

                impl<'de> ::serde::de::Visitor<'de> for UpdateVisitor {
                    type Value = $n;

                    fn expecting(&self, formatter: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                        formatter.write_str("(polarity, relid, value) tuple")
                    }

                    fn visit_seq<A>(self, mut seq: A) -> ::std::result::Result<Self::Value, A::Error>
                    where A: ::serde::de::SeqAccess<'de> {
                        let polarity = seq.next_element::<bool>()?.ok_or_else(|| <A::Error as ::serde::de::Error>::custom("Missing polarity"))?;
                        let relid = seq.next_element::<program::RelId>()?.ok_or_else(|| <A::Error as ::serde::de::Error>::custom("Missing relation id"))?;
                        match relid {
                            $(
                                $rel => {
                                    let v = seq.next_element::<$typ>()?.ok_or_else(|| <A::Error as ::serde::de::Error>::custom("Missing value"))?.into_ddvalue();
                                    if polarity {
                                        Ok(UpdateSerializer(program::Update::Insert{relid, v}))
                                    } else {
                                        Ok(UpdateSerializer(program::Update::DeleteValue{relid, v}))
                                    }
                                },
                            )*
                            _ => {
                                ::std::result::Result::Err(<A::Error as ::serde::de::Error>::custom(format!("Unknown input relation id {}", relid)))
                            }
                        }
                    }
                }

                deserializer.deserialize_tuple(3, UpdateVisitor)
            }
        }
    };
}

/* FlatBuffers bindings generated by `ddlog` */
#[cfg(feature = "flatbuf")]
pub mod flatbuf;

#[cfg(feature = "flatbuf")]
pub mod flatbuf_generated;

impl TryFrom<&RelIdentifier> for Relations {
    type Error = ();

    fn try_from(rel_id: &RelIdentifier) -> ::std::result::Result<Self, ()> {
        match rel_id {
            RelIdentifier::RelName(rname) => Relations::try_from(rname.as_ref()),
            RelIdentifier::RelId(id) => Relations::try_from(*id),
        }
    }
}


pub mod typedefs
{
    pub use ::types::VariablesInScope;
    pub use ::types::Variables;
    pub use ::types::VariableScopes;
    pub use ::types::UnifiedTypes;
    pub use ::types::Types;
    pub use ::types::TypedExpressions;
    pub use ::types::Statements;
    pub use ::types::Literals;
    pub use ::types::Items;
    pub use ::types::Functions;
    pub use ::types::Expressions;
    pub use ::types::Errors;
    pub use ::types::unify_types;
    pub use ::types::dbg;
    pub mod ddlog_std
    {
        pub use ::ddlog_std::s8;
        pub use ::ddlog_std::s64;
        pub use ::ddlog_std::s32;
        pub use ::ddlog_std::s16;
        pub use ::ddlog_std::s128;
        pub use ::ddlog_std::Vec;
        pub use ::ddlog_std::Set;
        pub use ::ddlog_std::Result;
        pub use ::ddlog_std::Ref;
        pub use ::ddlog_std::Option;
        pub use ::ddlog_std::Map;
        pub use ::ddlog_std::Group;
        pub use ::ddlog_std::Either;
        pub use ::ddlog_std::DDlogGroup;
        pub use ::ddlog_std::DDWeight;
        pub use ::ddlog_std::DDNestedTS;
        pub use ::ddlog_std::DDIteration;
        pub use ::ddlog_std::DDEpoch;
        pub use ::ddlog_std::zip;
        pub use ::ddlog_std::vec_zip;
        pub use ::ddlog_std::vec_with_length;
        pub use ::ddlog_std::vec_with_capacity;
        pub use ::ddlog_std::vec_update_nth;
        pub use ::ddlog_std::vec_truncate;
        pub use ::ddlog_std::vec_to_set;
        pub use ::ddlog_std::vec_swap_nth;
        pub use ::ddlog_std::vec_sort_imm;
        pub use ::ddlog_std::vec_sort;
        pub use ::ddlog_std::vec_singleton;
        pub use ::ddlog_std::vec_resize;
        pub use ::ddlog_std::vec_push_imm;
        pub use ::ddlog_std::vec_push;
        pub use ::ddlog_std::vec_pop;
        pub use ::ddlog_std::vec_nth;
        pub use ::ddlog_std::vec_len;
        pub use ::ddlog_std::vec_is_empty;
        pub use ::ddlog_std::vec_empty;
        pub use ::ddlog_std::vec_contains;
        pub use ::ddlog_std::vec_append;
        pub use ::ddlog_std::update_nth;
        pub use ::ddlog_std::unzip;
        pub use ::ddlog_std::unwrap_or_default_ddlog_std_Result__V_E_V;
        pub use ::ddlog_std::unwrap_or_default_ddlog_std_Option__A_A;
        pub use ::ddlog_std::unwrap_or_ddlog_std_Result__V_E_V_V;
        pub use ::ddlog_std::unwrap_or_ddlog_std_Option__A_A_A;
        pub use ::ddlog_std::unions;
        pub use ::ddlog_std::union_ddlog_std_Vec__ddlog_std_Set__X_ddlog_std_Set__X;
        pub use ::ddlog_std::union_ddlog_std_Set__X_ddlog_std_Set__X_ddlog_std_Set__X;
        pub use ::ddlog_std::union_ddlog_std_Map__K_V_ddlog_std_Map__K_V_ddlog_std_Map__K_V;
        pub use ::ddlog_std::union_ddlog_std_Group__K_ddlog_std_Ref__ddlog_std_Set__A_ddlog_std_Ref__ddlog_std_Set__A;
        pub use ::ddlog_std::union_ddlog_std_Group__K_ddlog_std_Set__A_ddlog_std_Set__A;
        pub use ::ddlog_std::u8_pow32;
        pub use ::ddlog_std::u64_pow32;
        pub use ::ddlog_std::u32_pow32;
        pub use ::ddlog_std::u16_pow32;
        pub use ::ddlog_std::u128_pow32;
        pub use ::ddlog_std::truncate;
        pub use ::ddlog_std::trim;
        pub use ::ddlog_std::to_vec_ddlog_std_Set__A_ddlog_std_Vec__A;
        pub use ::ddlog_std::to_vec_ddlog_std_Group__K_V_ddlog_std_Vec__V;
        pub use ::ddlog_std::to_vec_ddlog_std_Option__X_ddlog_std_Vec__X;
        pub use ::ddlog_std::to_uppercase;
        pub use ::ddlog_std::to_string___Stringval___Stringval;
        pub use ::ddlog_std::to_string___Bitval128___Stringval;
        pub use ::ddlog_std::to_string___Bitval64___Stringval;
        pub use ::ddlog_std::to_string___Bitval32___Stringval;
        pub use ::ddlog_std::to_string___Bitval16___Stringval;
        pub use ::ddlog_std::to_string___Bitval8___Stringval;
        pub use ::ddlog_std::to_string___Signedval128___Stringval;
        pub use ::ddlog_std::to_string___Signedval64___Stringval;
        pub use ::ddlog_std::to_string___Signedval32___Stringval;
        pub use ::ddlog_std::to_string___Signedval16___Stringval;
        pub use ::ddlog_std::to_string___Signedval8___Stringval;
        pub use ::ddlog_std::to_string___Doubleval___Stringval;
        pub use ::ddlog_std::to_string___Floatval___Stringval;
        pub use ::ddlog_std::to_string___Intval___Stringval;
        pub use ::ddlog_std::to_string___Boolval___Stringval;
        pub use ::ddlog_std::to_string_ddlog_std_DDNestedTS___Stringval;
        pub use ::ddlog_std::to_setmap;
        pub use ::ddlog_std::to_set_ddlog_std_Vec__A_ddlog_std_Set__A;
        pub use ::ddlog_std::to_set_ddlog_std_Group__K_V_ddlog_std_Set__V;
        pub use ::ddlog_std::to_set_ddlog_std_Option__X_ddlog_std_Set__X;
        pub use ::ddlog_std::to_map_ddlog_std_Vec____Tuple2__K_V_ddlog_std_Map__K_V;
        pub use ::ddlog_std::to_map_ddlog_std_Group__K1___Tuple2__K2_V_ddlog_std_Map__K2_V;
        pub use ::ddlog_std::to_lowercase;
        pub use ::ddlog_std::to_bytes;
        pub use ::ddlog_std::swap_nth;
        pub use ::ddlog_std::substr;
        pub use ::ddlog_std::string_trim;
        pub use ::ddlog_std::string_to_uppercase;
        pub use ::ddlog_std::string_to_lowercase;
        pub use ::ddlog_std::string_to_bytes;
        pub use ::ddlog_std::string_substr;
        pub use ::ddlog_std::string_starts_with;
        pub use ::ddlog_std::string_split;
        pub use ::ddlog_std::string_reverse;
        pub use ::ddlog_std::string_replace;
        pub use ::ddlog_std::string_len;
        pub use ::ddlog_std::string_join;
        pub use ::ddlog_std::string_ends_with;
        pub use ::ddlog_std::string_contains;
        pub use ::ddlog_std::str_to_lower;
        pub use ::ddlog_std::starts_with;
        pub use ::ddlog_std::split;
        pub use ::ddlog_std::sort_imm;
        pub use ::ddlog_std::sort;
        pub use ::ddlog_std::size_ddlog_std_Set__X___Bitval64;
        pub use ::ddlog_std::size_ddlog_std_Map__K_V___Bitval64;
        pub use ::ddlog_std::size_ddlog_std_Group__K_V___Bitval64;
        pub use ::ddlog_std::setref_unions;
        pub use ::ddlog_std::set_unions;
        pub use ::ddlog_std::set_union;
        pub use ::ddlog_std::set_to_vec;
        pub use ::ddlog_std::set_size;
        pub use ::ddlog_std::set_singleton;
        pub use ::ddlog_std::set_nth;
        pub use ::ddlog_std::set_is_empty;
        pub use ::ddlog_std::set_intersection;
        pub use ::ddlog_std::set_insert_imm;
        pub use ::ddlog_std::set_insert;
        pub use ::ddlog_std::set_empty;
        pub use ::ddlog_std::set_difference;
        pub use ::ddlog_std::set_contains;
        pub use ::ddlog_std::s8_pow32;
        pub use ::ddlog_std::s64_pow32;
        pub use ::ddlog_std::s32_pow32;
        pub use ::ddlog_std::s16_pow32;
        pub use ::ddlog_std::s128_pow32;
        pub use ::ddlog_std::reverse;
        pub use ::ddlog_std::result_unwrap_or_default;
        pub use ::ddlog_std::resize;
        pub use ::ddlog_std::replace;
        pub use ::ddlog_std::remove;
        pub use ::ddlog_std::ref_new;
        pub use ::ddlog_std::range_vec;
        pub use ::ddlog_std::push_imm;
        pub use ::ddlog_std::push;
        pub use ::ddlog_std::pow32___Intval___Bitval32___Intval;
        pub use ::ddlog_std::pow32___Signedval128___Bitval32___Signedval128;
        pub use ::ddlog_std::pow32___Signedval64___Bitval32___Signedval64;
        pub use ::ddlog_std::pow32___Signedval32___Bitval32___Signedval32;
        pub use ::ddlog_std::pow32___Signedval16___Bitval32___Signedval16;
        pub use ::ddlog_std::pow32___Signedval8___Bitval32___Signedval8;
        pub use ::ddlog_std::pow32___Bitval128___Bitval32___Bitval128;
        pub use ::ddlog_std::pow32___Bitval64___Bitval32___Bitval64;
        pub use ::ddlog_std::pow32___Bitval32___Bitval32___Bitval32;
        pub use ::ddlog_std::pow32___Bitval16___Bitval32___Bitval16;
        pub use ::ddlog_std::pow32___Bitval8___Bitval32___Bitval8;
        pub use ::ddlog_std::pop;
        pub use ::ddlog_std::parse_dec_u64;
        pub use ::ddlog_std::parse_dec_i64;
        pub use ::ddlog_std::option_unwrap_or_default;
        pub use ::ddlog_std::ok_or_else;
        pub use ::ddlog_std::ok_or;
        pub use ::ddlog_std::ntohs;
        pub use ::ddlog_std::ntohl;
        pub use ::ddlog_std::nth_ddlog_std_Set__X___Bitval64_ddlog_std_Option__X;
        pub use ::ddlog_std::nth_ddlog_std_Vec__X___Bitval64_ddlog_std_Option__X;
        pub use ::ddlog_std::nth_ddlog_std_Group__K_V___Bitval64_ddlog_std_Option__V;
        pub use ::ddlog_std::min_ddlog_std_Group__K_V_V;
        pub use ::ddlog_std::min_A_A_A;
        pub use ::ddlog_std::max_ddlog_std_Group__K_V_V;
        pub use ::ddlog_std::max_A_A_A;
        pub use ::ddlog_std::map_union;
        pub use ::ddlog_std::map_size;
        pub use ::ddlog_std::map_singleton;
        pub use ::ddlog_std::map_remove;
        pub use ::ddlog_std::map_keys;
        pub use ::ddlog_std::map_is_empty;
        pub use ::ddlog_std::map_insert_imm;
        pub use ::ddlog_std::map_insert;
        pub use ::ddlog_std::map_get;
        pub use ::ddlog_std::map_err;
        pub use ::ddlog_std::map_empty;
        pub use ::ddlog_std::map_contains_key;
        pub use ::ddlog_std::map_ddlog_std_Result__V1_E___Closureimm_V1_ret_V2_ddlog_std_Result__V2_E;
        pub use ::ddlog_std::map_ddlog_std_Option__A___Closureimm_A_ret_B_ddlog_std_Option__B;
        pub use ::ddlog_std::len_ddlog_std_Vec__X___Bitval64;
        pub use ::ddlog_std::len___Stringval___Bitval64;
        pub use ::ddlog_std::keys;
        pub use ::ddlog_std::key;
        pub use ::ddlog_std::join;
        pub use ::ddlog_std::is_some;
        pub use ::ddlog_std::is_ok;
        pub use ::ddlog_std::is_none;
        pub use ::ddlog_std::is_err;
        pub use ::ddlog_std::is_empty_ddlog_std_Set__X___Boolval;
        pub use ::ddlog_std::is_empty_ddlog_std_Map__K_V___Boolval;
        pub use ::ddlog_std::is_empty_ddlog_std_Vec__X___Boolval;
        pub use ::ddlog_std::intersection;
        pub use ::ddlog_std::insert_imm_ddlog_std_Set__X_X_ddlog_std_Set__X;
        pub use ::ddlog_std::insert_imm_ddlog_std_Map__K_V_K_V_ddlog_std_Map__K_V;
        pub use ::ddlog_std::insert_ddlog_std_Set__X_X___Tuple0__;
        pub use ::ddlog_std::insert_ddlog_std_Map__K_V_K_V___Tuple0__;
        pub use ::ddlog_std::htons;
        pub use ::ddlog_std::htonl;
        pub use ::ddlog_std::hex;
        pub use ::ddlog_std::hash64;
        pub use ::ddlog_std::hash128;
        pub use ::ddlog_std::group_unzip;
        pub use ::ddlog_std::group_to_vec;
        pub use ::ddlog_std::group_to_setmap;
        pub use ::ddlog_std::group_to_set;
        pub use ::ddlog_std::group_to_map;
        pub use ::ddlog_std::group_sum;
        pub use ::ddlog_std::group_setref_unions;
        pub use ::ddlog_std::group_set_unions;
        pub use ::ddlog_std::group_nth;
        pub use ::ddlog_std::group_min;
        pub use ::ddlog_std::group_max;
        pub use ::ddlog_std::group_key;
        pub use ::ddlog_std::group_first;
        pub use ::ddlog_std::group_count;
        pub use ::ddlog_std::get;
        pub use ::ddlog_std::first;
        pub use ::ddlog_std::ends_with;
        pub use ::ddlog_std::difference;
        pub use ::ddlog_std::deref;
        pub use ::ddlog_std::default;
        pub use ::ddlog_std::count;
        pub use ::ddlog_std::contains_key;
        pub use ::ddlog_std::contains_ddlog_std_Set__X_X___Boolval;
        pub use ::ddlog_std::contains_ddlog_std_Vec__X_X___Boolval;
        pub use ::ddlog_std::contains___Stringval___Stringval___Boolval;
        pub use ::ddlog_std::bigint_pow32;
        pub use ::ddlog_std::append;
        pub use ::ddlog_std::and_then;
        pub use ::ddlog_std::__builtin_2string;
    }
    pub mod debug
    {
        pub use ::debug::DDlogOpId;
        pub use ::debug::debug_split_group;
        pub use ::debug::debug_event_join;
        pub use ::debug::debug_event;
    }
    pub mod hir
    {
        pub use ::types__hir::Vis;
        pub use ::types__hir::VariableDecl;
        pub use ::types__hir::VarId;
        pub use ::types__hir::TypeKind;
        pub use ::types__hir::TypeId;
        pub use ::types__hir::Type;
        pub use ::types__hir::StrT;
        pub use ::types__hir::StmtId;
        pub use ::types__hir::Stmt;
        pub use ::types__hir::Span;
        pub use ::types__hir::ScopeId;
        pub use ::types__hir::Scope;
        pub use ::types__hir::Pattern;
        pub use ::types__hir::MatchArm;
        pub use ::types__hir::Match;
        pub use ::types__hir::Location;
        pub use ::types__hir::Literal;
        pub use ::types__hir::ItemPath;
        pub use ::types__hir::ItemId;
        pub use ::types__hir::Item;
        pub use ::types__hir::Function;
        pub use ::types__hir::FuncId;
        pub use ::types__hir::FuncArg;
        pub use ::types__hir::FileId;
        pub use ::types__hir::ExprKind;
        pub use ::types__hir::ExprId;
        pub use ::types__hir::Expr;
        pub use ::types__hir::Binding;
        pub use ::types__hir::BinaryOp;
        pub use ::types__hir::BinOp;
        pub use ::types__hir::type_of;
        pub use ::types__hir::is_unknown;
        pub use ::types__hir::is_str;
        pub use ::types__hir::is_int_internment_Intern__hir_Literal___Boolval;
        pub use ::types__hir::is_int_hir_Type___Boolval;
        pub use ::types__hir::is_bool;
    }
    pub mod internment
    {
        pub use ::internment::istring;
        pub use ::internment::Intern;
        pub use ::internment::trim;
        pub use ::internment::to_uppercase;
        pub use ::internment::to_string;
        pub use ::internment::to_lowercase;
        pub use ::internment::to_bytes;
        pub use ::internment::substr;
        pub use ::internment::starts_with;
        pub use ::internment::split;
        pub use ::internment::reverse;
        pub use ::internment::replace;
        pub use ::internment::parse_dec_u64;
        pub use ::internment::parse_dec_i64;
        pub use ::internment::len;
        pub use ::internment::join;
        pub use ::internment::ival;
        pub use ::internment::istring_trim;
        pub use ::internment::istring_to_uppercase;
        pub use ::internment::istring_to_lowercase;
        pub use ::internment::istring_to_bytes;
        pub use ::internment::istring_substr;
        pub use ::internment::istring_starts_with;
        pub use ::internment::istring_split;
        pub use ::internment::istring_reverse;
        pub use ::internment::istring_replace;
        pub use ::internment::istring_len;
        pub use ::internment::istring_join;
        pub use ::internment::istring_ends_with;
        pub use ::internment::istring_contains;
        pub use ::internment::intern;
        pub use ::internment::ends_with;
        pub use ::internment::contains;
    }
}
decl_update_deserializer!(UpdateSerializer,(0, types::Errors), (1, types::Expressions), (2, types::Functions), (3, types::Items), (5, types::Statements), (7, types::Types), (9, types::VariableScopes), (10, types::Variables));
impl TryFrom<&str> for Relations {
    type Error = ();
    fn try_from(rname: &str) -> ::std::result::Result<Self, ()> {
         match rname {
        "Errors" => Ok(Relations::Errors),
        "Expressions" => Ok(Relations::Expressions),
        "Functions" => Ok(Relations::Functions),
        "Items" => Ok(Relations::Items),
        "Literals" => Ok(Relations::Literals),
        "Statements" => Ok(Relations::Statements),
        "TypedExpressions" => Ok(Relations::TypedExpressions),
        "Types" => Ok(Relations::Types),
        "UnifiedTypes" => Ok(Relations::UnifiedTypes),
        "VariableScopes" => Ok(Relations::VariableScopes),
        "Variables" => Ok(Relations::Variables),
        "VariablesInScope" => Ok(Relations::VariablesInScope),
        "__MultiHead_5" => Ok(Relations::__MultiHead_5),
        "__Null" => Ok(Relations::__Null),
             _  => Err(())
         }
    }
}
impl Relations {
    pub fn is_output(&self) -> bool {
        match self {
        Relations::Errors => true,
            _  => false
        }
    }
}
impl Relations {
    pub fn is_input(&self) -> bool {
        match self {
        Relations::Expressions => true,
        Relations::Functions => true,
        Relations::Items => true,
        Relations::Statements => true,
        Relations::Types => true,
        Relations::VariableScopes => true,
        Relations::Variables => true,
            _  => false
        }
    }
}
impl Relations {
    pub fn type_id(&self) -> ::std::any::TypeId {
        match self {
            Relations::Errors => ::std::any::TypeId::of::<types::Errors>(),
            Relations::Expressions => ::std::any::TypeId::of::<types::Expressions>(),
            Relations::Functions => ::std::any::TypeId::of::<types::Functions>(),
            Relations::Items => ::std::any::TypeId::of::<types::Items>(),
            Relations::Literals => ::std::any::TypeId::of::<types::Literals>(),
            Relations::Statements => ::std::any::TypeId::of::<types::Statements>(),
            Relations::TypedExpressions => ::std::any::TypeId::of::<types::TypedExpressions>(),
            Relations::Types => ::std::any::TypeId::of::<types::Types>(),
            Relations::UnifiedTypes => ::std::any::TypeId::of::<types::UnifiedTypes>(),
            Relations::VariableScopes => ::std::any::TypeId::of::<types::VariableScopes>(),
            Relations::Variables => ::std::any::TypeId::of::<types::Variables>(),
            Relations::VariablesInScope => ::std::any::TypeId::of::<types::VariablesInScope>(),
            Relations::__MultiHead_5 => ::std::any::TypeId::of::<ddlog_std::tuple3<u64, u64, u64>>(),
            Relations::__Null => ::std::any::TypeId::of::<()>(),
        }
    }
}
impl TryFrom<program::RelId> for Relations {
    type Error = ();
    fn try_from(rid: program::RelId) -> ::std::result::Result<Self, ()> {
         match rid {
        0 => Ok(Relations::Errors),
        1 => Ok(Relations::Expressions),
        2 => Ok(Relations::Functions),
        3 => Ok(Relations::Items),
        4 => Ok(Relations::Literals),
        5 => Ok(Relations::Statements),
        6 => Ok(Relations::TypedExpressions),
        7 => Ok(Relations::Types),
        8 => Ok(Relations::UnifiedTypes),
        9 => Ok(Relations::VariableScopes),
        10 => Ok(Relations::Variables),
        11 => Ok(Relations::VariablesInScope),
        12 => Ok(Relations::__MultiHead_5),
        13 => Ok(Relations::__Null),
             _  => Err(())
         }
    }
}
pub fn relid2name(rid: program::RelId) -> Option<&'static str> {
   match rid {
        0 => Some(&"Errors"),
        1 => Some(&"Expressions"),
        2 => Some(&"Functions"),
        3 => Some(&"Items"),
        4 => Some(&"Literals"),
        5 => Some(&"Statements"),
        6 => Some(&"TypedExpressions"),
        7 => Some(&"Types"),
        8 => Some(&"UnifiedTypes"),
        9 => Some(&"VariableScopes"),
        10 => Some(&"Variables"),
        11 => Some(&"VariablesInScope"),
        12 => Some(&"__MultiHead_5"),
        13 => Some(&"__Null"),
       _  => None
   }
}
#[cfg(feature = "c_api")]
pub fn relid2cname(rid: program::RelId) -> Option<&'static ::std::ffi::CStr> {
    RELIDMAPC.get(&rid).copied()
}   /// A map of `RelId`s to their name as an `&'static str`
pub static RELIDMAP: ::once_cell::sync::Lazy<::fnv::FnvHashMap<Relations, &'static str>> =
    ::once_cell::sync::Lazy::new(|| {
        let mut map = ::fnv::FnvHashMap::with_capacity_and_hasher(14, ::fnv::FnvBuildHasher::default());
        map.insert(Relations::Errors, "Errors");
        map.insert(Relations::Expressions, "Expressions");
        map.insert(Relations::Functions, "Functions");
        map.insert(Relations::Items, "Items");
        map.insert(Relations::Literals, "Literals");
        map.insert(Relations::Statements, "Statements");
        map.insert(Relations::TypedExpressions, "TypedExpressions");
        map.insert(Relations::Types, "Types");
        map.insert(Relations::UnifiedTypes, "UnifiedTypes");
        map.insert(Relations::VariableScopes, "VariableScopes");
        map.insert(Relations::Variables, "Variables");
        map.insert(Relations::VariablesInScope, "VariablesInScope");
        map.insert(Relations::__MultiHead_5, "__MultiHead_5");
        map.insert(Relations::__Null, "__Null");
        map
    });
    /// A map of `RelId`s to their name as an `&'static CStr`
#[cfg(feature = "c_api")]
pub static RELIDMAPC: ::once_cell::sync::Lazy<::fnv::FnvHashMap<program::RelId, &'static ::std::ffi::CStr>> =
    ::once_cell::sync::Lazy::new(|| {
        let mut map = ::fnv::FnvHashMap::with_capacity_and_hasher(14, ::fnv::FnvBuildHasher::default());
        map.insert(0, ::std::ffi::CStr::from_bytes_with_nul(b"Errors\0").expect("Unreachable: A null byte was specifically inserted"));
        map.insert(1, ::std::ffi::CStr::from_bytes_with_nul(b"Expressions\0").expect("Unreachable: A null byte was specifically inserted"));
        map.insert(2, ::std::ffi::CStr::from_bytes_with_nul(b"Functions\0").expect("Unreachable: A null byte was specifically inserted"));
        map.insert(3, ::std::ffi::CStr::from_bytes_with_nul(b"Items\0").expect("Unreachable: A null byte was specifically inserted"));
        map.insert(4, ::std::ffi::CStr::from_bytes_with_nul(b"Literals\0").expect("Unreachable: A null byte was specifically inserted"));
        map.insert(5, ::std::ffi::CStr::from_bytes_with_nul(b"Statements\0").expect("Unreachable: A null byte was specifically inserted"));
        map.insert(6, ::std::ffi::CStr::from_bytes_with_nul(b"TypedExpressions\0").expect("Unreachable: A null byte was specifically inserted"));
        map.insert(7, ::std::ffi::CStr::from_bytes_with_nul(b"Types\0").expect("Unreachable: A null byte was specifically inserted"));
        map.insert(8, ::std::ffi::CStr::from_bytes_with_nul(b"UnifiedTypes\0").expect("Unreachable: A null byte was specifically inserted"));
        map.insert(9, ::std::ffi::CStr::from_bytes_with_nul(b"VariableScopes\0").expect("Unreachable: A null byte was specifically inserted"));
        map.insert(10, ::std::ffi::CStr::from_bytes_with_nul(b"Variables\0").expect("Unreachable: A null byte was specifically inserted"));
        map.insert(11, ::std::ffi::CStr::from_bytes_with_nul(b"VariablesInScope\0").expect("Unreachable: A null byte was specifically inserted"));
        map.insert(12, ::std::ffi::CStr::from_bytes_with_nul(b"__MultiHead_5\0").expect("Unreachable: A null byte was specifically inserted"));
        map.insert(13, ::std::ffi::CStr::from_bytes_with_nul(b"__Null\0").expect("Unreachable: A null byte was specifically inserted"));
        map
    });
    /// A map of input `Relations`s to their name as an `&'static str`
pub static INPUT_RELIDMAP: ::once_cell::sync::Lazy<::fnv::FnvHashMap<Relations, &'static str>> =
    ::once_cell::sync::Lazy::new(|| {
        let mut map = ::fnv::FnvHashMap::with_capacity_and_hasher(7, ::fnv::FnvBuildHasher::default());
        map.insert(Relations::Expressions, "Expressions");
        map.insert(Relations::Functions, "Functions");
        map.insert(Relations::Items, "Items");
        map.insert(Relations::Statements, "Statements");
        map.insert(Relations::Types, "Types");
        map.insert(Relations::VariableScopes, "VariableScopes");
        map.insert(Relations::Variables, "Variables");
        map
    });
    /// A map of output `Relations`s to their name as an `&'static str`
pub static OUTPUT_RELIDMAP: ::once_cell::sync::Lazy<::fnv::FnvHashMap<Relations, &'static str>> =
    ::once_cell::sync::Lazy::new(|| {
        let mut map = ::fnv::FnvHashMap::with_capacity_and_hasher(1, ::fnv::FnvBuildHasher::default());
        map.insert(Relations::Errors, "Errors");
        map
    });
impl TryFrom<&str> for Indexes {
    type Error = ();
    fn try_from(iname: &str) -> ::std::result::Result<Self, ()> {
         match iname {
        "__Null_by_none" => Ok(Indexes::__Null_by_none),
             _  => Err(())
         }
    }
}
impl TryFrom<program::IdxId> for Indexes {
    type Error = ();
    fn try_from(iid: program::IdxId) -> ::core::result::Result<Self, ()> {
         match iid {
        0 => Ok(Indexes::__Null_by_none),
             _  => Err(())
         }
    }
}
pub fn indexid2name(iid: program::IdxId) -> Option<&'static str> {
   match iid {
        0 => Some(&"__Null_by_none"),
       _  => None
   }
}
#[cfg(feature = "c_api")]
pub fn indexid2cname(iid: program::IdxId) -> Option<&'static ::std::ffi::CStr> {
    IDXIDMAPC.get(&iid).copied()
}   /// A map of `Indexes` to their name as an `&'static str`
pub static IDXIDMAP: ::once_cell::sync::Lazy<::fnv::FnvHashMap<Indexes, &'static str>> =
    ::once_cell::sync::Lazy::new(|| {
        let mut map = ::fnv::FnvHashMap::with_capacity_and_hasher(1, ::fnv::FnvBuildHasher::default());
        map.insert(Indexes::__Null_by_none, "__Null_by_none");
        map
    });
    /// A map of `IdxId`s to their name as an `&'static CStr`
#[cfg(feature = "c_api")]
pub static IDXIDMAPC: ::once_cell::sync::Lazy<::fnv::FnvHashMap<program::IdxId, &'static ::std::ffi::CStr>> =
    ::once_cell::sync::Lazy::new(|| {
        let mut map = ::fnv::FnvHashMap::with_capacity_and_hasher(1, ::fnv::FnvBuildHasher::default());
        map.insert(0, ::std::ffi::CStr::from_bytes_with_nul(b"__Null_by_none\0").expect("Unreachable: A null byte was specifically inserted"));
        map
    });
pub fn relval_from_record(rel: Relations, _rec: &differential_datalog::record::Record) -> ::std::result::Result<DDValue, String> {
    match rel {
        Relations::Errors => {
            Ok(<types::Errors>::from_record(_rec)?.into_ddvalue())
        },
        Relations::Expressions => {
            Ok(<types::Expressions>::from_record(_rec)?.into_ddvalue())
        },
        Relations::Functions => {
            Ok(<types::Functions>::from_record(_rec)?.into_ddvalue())
        },
        Relations::Items => {
            Ok(<types::Items>::from_record(_rec)?.into_ddvalue())
        },
        Relations::Literals => {
            Ok(<types::Literals>::from_record(_rec)?.into_ddvalue())
        },
        Relations::Statements => {
            Ok(<types::Statements>::from_record(_rec)?.into_ddvalue())
        },
        Relations::TypedExpressions => {
            Ok(<types::TypedExpressions>::from_record(_rec)?.into_ddvalue())
        },
        Relations::Types => {
            Ok(<types::Types>::from_record(_rec)?.into_ddvalue())
        },
        Relations::UnifiedTypes => {
            Ok(<types::UnifiedTypes>::from_record(_rec)?.into_ddvalue())
        },
        Relations::VariableScopes => {
            Ok(<types::VariableScopes>::from_record(_rec)?.into_ddvalue())
        },
        Relations::Variables => {
            Ok(<types::Variables>::from_record(_rec)?.into_ddvalue())
        },
        Relations::VariablesInScope => {
            Ok(<types::VariablesInScope>::from_record(_rec)?.into_ddvalue())
        },
        Relations::__MultiHead_5 => {
            Ok(<ddlog_std::tuple3<u64, u64, u64>>::from_record(_rec)?.into_ddvalue())
        },
        Relations::__Null => {
            Ok(<()>::from_record(_rec)?.into_ddvalue())
        }
    }
}
pub fn relkey_from_record(rel: Relations, _rec: &differential_datalog::record::Record) -> ::std::result::Result<DDValue, String> {
    match rel {
        _ => Err(format!("relation {:?} does not have a primary key", rel))
    }
}
pub fn idxkey_from_record(idx: Indexes, _rec: &differential_datalog::record::Record) -> ::std::result::Result<DDValue, String> {
    match idx {
        Indexes::__Null_by_none => {
            Ok(<()>::from_record(_rec)?.into_ddvalue())
        }
    }
}
pub fn indexes2arrid(idx: Indexes) -> program::ArrId {
    match idx {
        Indexes::__Null_by_none => ( 13, 0),
    }
}
#[derive(Copy,Clone,Debug,PartialEq,Eq,Hash)]
pub enum Relations {
    Errors = 0,
    Expressions = 1,
    Functions = 2,
    Items = 3,
    Literals = 4,
    Statements = 5,
    TypedExpressions = 6,
    Types = 7,
    UnifiedTypes = 8,
    VariableScopes = 9,
    Variables = 10,
    VariablesInScope = 11,
    __MultiHead_5 = 12,
    __Null = 13
}
#[derive(Copy,Clone,Debug,PartialEq,Eq,Hash)]
pub enum Indexes {
    __Null_by_none = 0
}
pub fn prog(__update_cb: std::sync::Arc<dyn program::RelationCallback>) -> program::Program {
    let Errors = program::Relation {
                     name:         std::borrow::Cow::from("Errors"),
                     input:        false,
                     distinct:     true,
                     caching_mode: program::CachingMode::Set,
                     key_func:     None,
                     id:           0,
                     rules:        vec![
                     ],
                     arrangements: vec![
                     ],
                     change_cb:    Some(std::sync::Arc::clone(&__update_cb))
                 };
    let Expressions = program::Relation {
                          name:         std::borrow::Cow::from("Expressions"),
                          input:        true,
                          distinct:     false,
                          caching_mode: program::CachingMode::Set,
                          key_func:     None,
                          id:           1,
                          rules:        vec![
                          ],
                          arrangements: vec![
                              types::__Arng_Expressions_0.clone(),
                              types::__Arng_Expressions_1.clone()
                          ],
                          change_cb:    None
                      };
    let Functions = program::Relation {
                        name:         std::borrow::Cow::from("Functions"),
                        input:        true,
                        distinct:     false,
                        caching_mode: program::CachingMode::Set,
                        key_func:     None,
                        id:           2,
                        rules:        vec![
                        ],
                        arrangements: vec![
                        ],
                        change_cb:    None
                    };
    let Items = program::Relation {
                    name:         std::borrow::Cow::from("Items"),
                    input:        true,
                    distinct:     false,
                    caching_mode: program::CachingMode::Set,
                    key_func:     None,
                    id:           3,
                    rules:        vec![
                    ],
                    arrangements: vec![
                    ],
                    change_cb:    None
                };
    let Statements = program::Relation {
                         name:         std::borrow::Cow::from("Statements"),
                         input:        true,
                         distinct:     false,
                         caching_mode: program::CachingMode::Set,
                         key_func:     None,
                         id:           5,
                         rules:        vec![
                         ],
                         arrangements: vec![
                         ],
                         change_cb:    None
                     };
    let Types = program::Relation {
                    name:         std::borrow::Cow::from("Types"),
                    input:        true,
                    distinct:     false,
                    caching_mode: program::CachingMode::Set,
                    key_func:     None,
                    id:           7,
                    rules:        vec![
                    ],
                    arrangements: vec![
                        types::__Arng_Types_0.clone(),
                        types::__Arng_Types_1.clone()
                    ],
                    change_cb:    None
                };
    let Literals = program::Relation {
                       name:         std::borrow::Cow::from("Literals"),
                       input:        false,
                       distinct:     false,
                       caching_mode: program::CachingMode::Set,
                       key_func:     None,
                       id:           4,
                       rules:        vec![
                           types::__Rule_Literals_0.clone()
                       ],
                       arrangements: vec![
                       ],
                       change_cb:    None
                   };
    let VariableScopes = program::Relation {
                             name:         std::borrow::Cow::from("VariableScopes"),
                             input:        true,
                             distinct:     false,
                             caching_mode: program::CachingMode::Set,
                             key_func:     None,
                             id:           9,
                             rules:        vec![
                             ],
                             arrangements: vec![
                                 types::__Arng_VariableScopes_0.clone()
                             ],
                             change_cb:    None
                         };
    let Variables = program::Relation {
                        name:         std::borrow::Cow::from("Variables"),
                        input:        true,
                        distinct:     false,
                        caching_mode: program::CachingMode::Set,
                        key_func:     None,
                        id:           10,
                        rules:        vec![
                        ],
                        arrangements: vec![
                            types::__Arng_Variables_0.clone()
                        ],
                        change_cb:    None
                    };
    let TypedExpressions = program::Relation {
                               name:         std::borrow::Cow::from("TypedExpressions"),
                               input:        false,
                               distinct:     false,
                               caching_mode: program::CachingMode::Set,
                               key_func:     None,
                               id:           6,
                               rules:        vec![
                                   types::__Rule_TypedExpressions_0.clone(),
                                   types::__Rule_TypedExpressions_1.clone(),
                                   types::__Rule_TypedExpressions_2.clone(),
                                   types::__Rule_TypedExpressions_3.clone()
                               ],
                               arrangements: vec![
                                   types::__Arng_TypedExpressions_0.clone(),
                                   types::__Arng_TypedExpressions_1.clone()
                               ],
                               change_cb:    None
                           };
    let __MultiHead_5 = program::Relation {
                            name:         std::borrow::Cow::from("__MultiHead_5"),
                            input:        false,
                            distinct:     false,
                            caching_mode: program::CachingMode::Set,
                            key_func:     None,
                            id:           12,
                            rules:        vec![
                                types::__Rule___MultiHead_5_0.clone()
                            ],
                            arrangements: vec![
                            ],
                            change_cb:    None
                        };
    let UnifiedTypes = program::Relation {
                           name:         std::borrow::Cow::from("UnifiedTypes"),
                           input:        false,
                           distinct:     false,
                           caching_mode: program::CachingMode::Set,
                           key_func:     None,
                           id:           8,
                           rules:        vec![
                               types::__Rule_UnifiedTypes_0.clone()
                           ],
                           arrangements: vec![
                           ],
                           change_cb:    None
                       };
    let VariablesInScope = program::Relation {
                               name:         std::borrow::Cow::from("VariablesInScope"),
                               input:        false,
                               distinct:     false,
                               caching_mode: program::CachingMode::Set,
                               key_func:     None,
                               id:           11,
                               rules:        vec![
                                   types::__Rule_VariablesInScope_0.clone(),
                                   types::__Rule_VariablesInScope_1.clone()
                               ],
                               arrangements: vec![
                                   types::__Arng_VariablesInScope_0.clone()
                               ],
                               change_cb:    None
                           };
    let __Null = program::Relation {
                     name:         std::borrow::Cow::from("__Null"),
                     input:        false,
                     distinct:     false,
                     caching_mode: program::CachingMode::Set,
                     key_func:     None,
                     id:           13,
                     rules:        vec![
                     ],
                     arrangements: vec![
                         types::__Arng___Null_0.clone()
                     ],
                     change_cb:    None
                 };
    let nodes: std::vec::Vec<program::ProgNode> = vec![
            program::ProgNode::Rel{rel: Errors},
            program::ProgNode::Rel{rel: Expressions},
            program::ProgNode::Rel{rel: Functions},
            program::ProgNode::Rel{rel: Items},
            program::ProgNode::Rel{rel: Statements},
            program::ProgNode::Rel{rel: Types},
            program::ProgNode::Rel{rel: Literals},
            program::ProgNode::Rel{rel: VariableScopes},
            program::ProgNode::Rel{rel: Variables},
            program::ProgNode::SCC{rels: vec![program::RecursiveRelation{rel: TypedExpressions, distinct: true}, program::RecursiveRelation{rel: __MultiHead_5, distinct: true}]},
            program::ProgNode::Rel{rel: UnifiedTypes},
            program::ProgNode::SCC{rels: vec![program::RecursiveRelation{rel: VariablesInScope, distinct: true}]},
            program::ProgNode::Rel{rel: __Null}
    ];
    let delayed_rels = vec![];
    let init_data: std::vec::Vec<(program::RelId, DDValue)> = vec![];
    program::Program {
        nodes,
        delayed_rels,
        init_data,
    }
}