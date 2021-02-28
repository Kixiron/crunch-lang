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


#[derive(Eq, Ord, Clone, Hash, PartialEq, PartialOrd, IntoRecord, Mutator, Serialize, Deserialize, FromRecord)]
#[ddlog(rename = "hir::BinOp")]
pub enum BinOp {
    #[ddlog(rename = "hir::Mult")]
    Mult,
    #[ddlog(rename = "hir::Div")]
    Div,
    #[ddlog(rename = "hir::Add")]
    Add,
    #[ddlog(rename = "hir::Sub")]
    Sub,
    #[ddlog(rename = "hir::Mod")]
    Mod,
    #[ddlog(rename = "hir::Pow")]
    Pow,
    #[ddlog(rename = "hir::BitAnd")]
    BitAnd,
    #[ddlog(rename = "hir::BitOr")]
    BitOr,
    #[ddlog(rename = "hir::BitXor")]
    BitXor,
    #[ddlog(rename = "hir::Shl")]
    Shl,
    #[ddlog(rename = "hir::Shr")]
    Shr
}
impl abomonation::Abomonation for BinOp{}
impl ::std::fmt::Display for BinOp {
    fn fmt(&self, __formatter: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match self {
            BinOp::Mult{} => {
                __formatter.write_str("hir::Mult{")?;
                __formatter.write_str("}")
            },
            BinOp::Div{} => {
                __formatter.write_str("hir::Div{")?;
                __formatter.write_str("}")
            },
            BinOp::Add{} => {
                __formatter.write_str("hir::Add{")?;
                __formatter.write_str("}")
            },
            BinOp::Sub{} => {
                __formatter.write_str("hir::Sub{")?;
                __formatter.write_str("}")
            },
            BinOp::Mod{} => {
                __formatter.write_str("hir::Mod{")?;
                __formatter.write_str("}")
            },
            BinOp::Pow{} => {
                __formatter.write_str("hir::Pow{")?;
                __formatter.write_str("}")
            },
            BinOp::BitAnd{} => {
                __formatter.write_str("hir::BitAnd{")?;
                __formatter.write_str("}")
            },
            BinOp::BitOr{} => {
                __formatter.write_str("hir::BitOr{")?;
                __formatter.write_str("}")
            },
            BinOp::BitXor{} => {
                __formatter.write_str("hir::BitXor{")?;
                __formatter.write_str("}")
            },
            BinOp::Shl{} => {
                __formatter.write_str("hir::Shl{")?;
                __formatter.write_str("}")
            },
            BinOp::Shr{} => {
                __formatter.write_str("hir::Shr{")?;
                __formatter.write_str("}")
            }
        }
    }
}
impl ::std::fmt::Debug for BinOp {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        ::std::fmt::Display::fmt(&self, f)
    }
}
impl ::std::default::Default for BinOp {
    fn default() -> Self {
        BinOp::Mult{}
    }
}
#[derive(Eq, Ord, Clone, Hash, PartialEq, PartialOrd, IntoRecord, Mutator, Default, Serialize, Deserialize, FromRecord)]
#[ddlog(rename = "hir::BinaryOp")]
pub struct BinaryOp {
    pub lhs: ExprId,
    pub op: BinOp,
    pub rhs: ExprId
}
impl abomonation::Abomonation for BinaryOp{}
impl ::std::fmt::Display for BinaryOp {
    fn fmt(&self, __formatter: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match self {
            BinaryOp{lhs,op,rhs} => {
                __formatter.write_str("hir::BinaryOp{")?;
                ::std::fmt::Debug::fmt(lhs, __formatter)?;
                __formatter.write_str(",")?;
                ::std::fmt::Debug::fmt(op, __formatter)?;
                __formatter.write_str(",")?;
                ::std::fmt::Debug::fmt(rhs, __formatter)?;
                __formatter.write_str("}")
            }
        }
    }
}
impl ::std::fmt::Debug for BinaryOp {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        ::std::fmt::Display::fmt(&self, f)
    }
}
#[derive(Eq, Ord, Clone, Hash, PartialEq, PartialOrd, IntoRecord, Mutator, Default, Serialize, Deserialize, FromRecord)]
#[ddlog(rename = "hir::Binding")]
pub struct Binding {
    pub reference: bool,
    pub mutable: bool,
    pub pattern: Pattern,
    pub ty: ddlog_std::Option<TypeId>
}
impl abomonation::Abomonation for Binding{}
impl ::std::fmt::Display for Binding {
    fn fmt(&self, __formatter: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match self {
            Binding{reference,mutable,pattern,ty} => {
                __formatter.write_str("hir::Binding{")?;
                ::std::fmt::Debug::fmt(reference, __formatter)?;
                __formatter.write_str(",")?;
                ::std::fmt::Debug::fmt(mutable, __formatter)?;
                __formatter.write_str(",")?;
                ::std::fmt::Debug::fmt(pattern, __formatter)?;
                __formatter.write_str(",")?;
                ::std::fmt::Debug::fmt(ty, __formatter)?;
                __formatter.write_str("}")
            }
        }
    }
}
impl ::std::fmt::Debug for Binding {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        ::std::fmt::Display::fmt(&self, f)
    }
}
#[derive(Eq, Ord, Clone, Hash, PartialEq, PartialOrd, IntoRecord, Mutator, Default, Serialize, Deserialize, FromRecord)]
#[ddlog(rename = "hir::Expr")]
pub struct Expr {
    pub kind: ExprKind
}
impl abomonation::Abomonation for Expr{}
impl ::std::fmt::Display for Expr {
    fn fmt(&self, __formatter: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match self {
            Expr{kind} => {
                __formatter.write_str("hir::Expr{")?;
                ::std::fmt::Debug::fmt(kind, __formatter)?;
                __formatter.write_str("}")
            }
        }
    }
}
impl ::std::fmt::Debug for Expr {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        ::std::fmt::Display::fmt(&self, f)
    }
}
pub type ExprId = u64;
#[derive(Eq, Ord, Clone, Hash, PartialEq, PartialOrd, IntoRecord, Mutator, Serialize, Deserialize, FromRecord)]
#[ddlog(rename = "hir::ExprKind")]
pub enum ExprKind {
    #[ddlog(rename = "hir::ExprLit")]
    ExprLit {
        lit: internment::Intern<Literal>
    },
    #[ddlog(rename = "hir::ExprVar")]
    ExprVar {
        variable: VarId
    },
    #[ddlog(rename = "hir::ExprAssign")]
    ExprAssign {
        variable: VarId,
        expr_id: ExprId
    },
    #[ddlog(rename = "hir::ExprMatch")]
    ExprMatch {
        match_: Match
    },
    #[ddlog(rename = "hir::ExprScope")]
    ExprScope {
        block: StmtId
    },
    #[ddlog(rename = "hir::ExprReturn")]
    ExprReturn {
        val: ddlog_std::Option<ExprId>
    },
    #[ddlog(rename = "hir::ExprBinOp")]
    ExprBinOp {
        op: BinaryOp
    }
}
impl abomonation::Abomonation for ExprKind{}
impl ::std::fmt::Display for ExprKind {
    fn fmt(&self, __formatter: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match self {
            ExprKind::ExprLit{lit} => {
                __formatter.write_str("hir::ExprLit{")?;
                ::std::fmt::Debug::fmt(lit, __formatter)?;
                __formatter.write_str("}")
            },
            ExprKind::ExprVar{variable} => {
                __formatter.write_str("hir::ExprVar{")?;
                ::std::fmt::Debug::fmt(variable, __formatter)?;
                __formatter.write_str("}")
            },
            ExprKind::ExprAssign{variable,expr_id} => {
                __formatter.write_str("hir::ExprAssign{")?;
                ::std::fmt::Debug::fmt(variable, __formatter)?;
                __formatter.write_str(",")?;
                ::std::fmt::Debug::fmt(expr_id, __formatter)?;
                __formatter.write_str("}")
            },
            ExprKind::ExprMatch{match_} => {
                __formatter.write_str("hir::ExprMatch{")?;
                ::std::fmt::Debug::fmt(match_, __formatter)?;
                __formatter.write_str("}")
            },
            ExprKind::ExprScope{block} => {
                __formatter.write_str("hir::ExprScope{")?;
                ::std::fmt::Debug::fmt(block, __formatter)?;
                __formatter.write_str("}")
            },
            ExprKind::ExprReturn{val} => {
                __formatter.write_str("hir::ExprReturn{")?;
                ::std::fmt::Debug::fmt(val, __formatter)?;
                __formatter.write_str("}")
            },
            ExprKind::ExprBinOp{op} => {
                __formatter.write_str("hir::ExprBinOp{")?;
                ::std::fmt::Debug::fmt(op, __formatter)?;
                __formatter.write_str("}")
            }
        }
    }
}
impl ::std::fmt::Debug for ExprKind {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        ::std::fmt::Display::fmt(&self, f)
    }
}
impl ::std::default::Default for ExprKind {
    fn default() -> Self {
        ExprKind::ExprLit{lit : ::std::default::Default::default()}
    }
}
pub type FileId = u32;
#[derive(Eq, Ord, Clone, Hash, PartialEq, PartialOrd, IntoRecord, Mutator, Default, Serialize, Deserialize, FromRecord)]
#[ddlog(rename = "hir::FuncArg")]
pub struct FuncArg {
    pub name: VarId,
    pub kind: TypeId
}
impl abomonation::Abomonation for FuncArg{}
impl ::std::fmt::Display for FuncArg {
    fn fmt(&self, __formatter: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match self {
            FuncArg{name,kind} => {
                __formatter.write_str("hir::FuncArg{")?;
                ::std::fmt::Debug::fmt(name, __formatter)?;
                __formatter.write_str(",")?;
                ::std::fmt::Debug::fmt(kind, __formatter)?;
                __formatter.write_str("}")
            }
        }
    }
}
impl ::std::fmt::Debug for FuncArg {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        ::std::fmt::Display::fmt(&self, f)
    }
}
pub type FuncId = u64;
#[derive(Eq, Ord, Clone, Hash, PartialEq, PartialOrd, IntoRecord, Mutator, Default, Serialize, Deserialize, FromRecord)]
#[ddlog(rename = "hir::Function")]
pub struct Function {
    pub name: ItemPath,
    pub vis: Vis,
    pub args: ddlog_std::Vec<FuncArg>,
    pub body: StmtId,
    pub ret: TypeId,
    pub decl_scope: ScopeId
}
impl abomonation::Abomonation for Function{}
impl ::std::fmt::Display for Function {
    fn fmt(&self, __formatter: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match self {
            Function{name,vis,args,body,ret,decl_scope} => {
                __formatter.write_str("hir::Function{")?;
                ::std::fmt::Debug::fmt(name, __formatter)?;
                __formatter.write_str(",")?;
                ::std::fmt::Debug::fmt(vis, __formatter)?;
                __formatter.write_str(",")?;
                ::std::fmt::Debug::fmt(args, __formatter)?;
                __formatter.write_str(",")?;
                ::std::fmt::Debug::fmt(body, __formatter)?;
                __formatter.write_str(",")?;
                ::std::fmt::Debug::fmt(ret, __formatter)?;
                __formatter.write_str(",")?;
                ::std::fmt::Debug::fmt(decl_scope, __formatter)?;
                __formatter.write_str("}")
            }
        }
    }
}
impl ::std::fmt::Debug for Function {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        ::std::fmt::Display::fmt(&self, f)
    }
}
#[derive(Eq, Ord, Clone, Hash, PartialEq, PartialOrd, IntoRecord, Mutator, Serialize, Deserialize, FromRecord)]
#[ddlog(rename = "hir::Item")]
pub enum Item {
    #[ddlog(rename = "hir::ItemFunc")]
    ItemFunc {
        func: FuncId
    },
    #[ddlog(rename = "hir::ItemStruct")]
    ItemStruct
}
impl abomonation::Abomonation for Item{}
impl ::std::fmt::Display for Item {
    fn fmt(&self, __formatter: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match self {
            Item::ItemFunc{func} => {
                __formatter.write_str("hir::ItemFunc{")?;
                ::std::fmt::Debug::fmt(func, __formatter)?;
                __formatter.write_str("}")
            },
            Item::ItemStruct{} => {
                __formatter.write_str("hir::ItemStruct{")?;
                __formatter.write_str("}")
            }
        }
    }
}
impl ::std::fmt::Debug for Item {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        ::std::fmt::Display::fmt(&self, f)
    }
}
impl ::std::default::Default for Item {
    fn default() -> Self {
        Item::ItemFunc{func : ::std::default::Default::default()}
    }
}
pub type ItemId = u64;
pub type ItemPath = internment::Intern<ddlog_std::Vec<StrT>>;
#[derive(Eq, Ord, Clone, Hash, PartialEq, PartialOrd, IntoRecord, Mutator, Serialize, Deserialize, FromRecord)]
#[ddlog(rename = "hir::Literal")]
pub enum Literal {
    #[ddlog(rename = "hir::String")]
    String {
        str: String
    },
    #[ddlog(rename = "hir::Boolean")]
    Boolean {
        boolean: bool
    },
    #[ddlog(rename = "hir::Integer")]
    Integer {
        int: u64
    }
}
impl abomonation::Abomonation for Literal{}
impl ::std::fmt::Display for Literal {
    fn fmt(&self, __formatter: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match self {
            Literal::String{str} => {
                __formatter.write_str("hir::String{")?;
                differential_datalog::record::format_ddlog_str(str, __formatter)?;
                __formatter.write_str("}")
            },
            Literal::Boolean{boolean} => {
                __formatter.write_str("hir::Boolean{")?;
                ::std::fmt::Debug::fmt(boolean, __formatter)?;
                __formatter.write_str("}")
            },
            Literal::Integer{int} => {
                __formatter.write_str("hir::Integer{")?;
                ::std::fmt::Debug::fmt(int, __formatter)?;
                __formatter.write_str("}")
            }
        }
    }
}
impl ::std::fmt::Debug for Literal {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        ::std::fmt::Display::fmt(&self, f)
    }
}
impl ::std::default::Default for Literal {
    fn default() -> Self {
        Literal::String{str : ::std::default::Default::default()}
    }
}
#[derive(Eq, Ord, Clone, Hash, PartialEq, PartialOrd, IntoRecord, Mutator, Default, Serialize, Deserialize, FromRecord)]
#[ddlog(rename = "hir::Location")]
pub struct Location {
    pub span: Span,
    pub file: FileId
}
impl abomonation::Abomonation for Location{}
impl ::std::fmt::Display for Location {
    fn fmt(&self, __formatter: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match self {
            Location{span,file} => {
                __formatter.write_str("hir::Location{")?;
                ::std::fmt::Debug::fmt(span, __formatter)?;
                __formatter.write_str(",")?;
                ::std::fmt::Debug::fmt(file, __formatter)?;
                __formatter.write_str("}")
            }
        }
    }
}
impl ::std::fmt::Debug for Location {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        ::std::fmt::Display::fmt(&self, f)
    }
}
#[derive(Eq, Ord, Clone, Hash, PartialEq, PartialOrd, IntoRecord, Mutator, Default, Serialize, Deserialize, FromRecord)]
#[ddlog(rename = "hir::Match")]
pub struct Match {
    pub cond: ExprId,
    pub arms: ddlog_std::Vec<MatchArm>,
    pub ty: TypeId
}
impl abomonation::Abomonation for Match{}
impl ::std::fmt::Display for Match {
    fn fmt(&self, __formatter: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match self {
            Match{cond,arms,ty} => {
                __formatter.write_str("hir::Match{")?;
                ::std::fmt::Debug::fmt(cond, __formatter)?;
                __formatter.write_str(",")?;
                ::std::fmt::Debug::fmt(arms, __formatter)?;
                __formatter.write_str(",")?;
                ::std::fmt::Debug::fmt(ty, __formatter)?;
                __formatter.write_str("}")
            }
        }
    }
}
impl ::std::fmt::Debug for Match {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        ::std::fmt::Display::fmt(&self, f)
    }
}
#[derive(Eq, Ord, Clone, Hash, PartialEq, PartialOrd, IntoRecord, Mutator, Default, Serialize, Deserialize, FromRecord)]
#[ddlog(rename = "hir::MatchArm")]
pub struct MatchArm {
    pub bind: Binding,
    pub guard: ddlog_std::Option<ExprId>,
    pub body: StmtId,
    pub ty: TypeId
}
impl abomonation::Abomonation for MatchArm{}
impl ::std::fmt::Display for MatchArm {
    fn fmt(&self, __formatter: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match self {
            MatchArm{bind,guard,body,ty} => {
                __formatter.write_str("hir::MatchArm{")?;
                ::std::fmt::Debug::fmt(bind, __formatter)?;
                __formatter.write_str(",")?;
                ::std::fmt::Debug::fmt(guard, __formatter)?;
                __formatter.write_str(",")?;
                ::std::fmt::Debug::fmt(body, __formatter)?;
                __formatter.write_str(",")?;
                ::std::fmt::Debug::fmt(ty, __formatter)?;
                __formatter.write_str("}")
            }
        }
    }
}
impl ::std::fmt::Debug for MatchArm {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        ::std::fmt::Display::fmt(&self, f)
    }
}
#[derive(Eq, Ord, Clone, Hash, PartialEq, PartialOrd, IntoRecord, Mutator, Serialize, Deserialize, FromRecord)]
#[ddlog(rename = "hir::Pattern")]
pub enum Pattern {
    #[ddlog(rename = "hir::PatLit")]
    PatLit {
        lit: Literal,
        ty: TypeId
    },
    #[ddlog(rename = "hir::Ident")]
    Ident
}
impl abomonation::Abomonation for Pattern{}
impl ::std::fmt::Display for Pattern {
    fn fmt(&self, __formatter: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match self {
            Pattern::PatLit{lit,ty} => {
                __formatter.write_str("hir::PatLit{")?;
                ::std::fmt::Debug::fmt(lit, __formatter)?;
                __formatter.write_str(",")?;
                ::std::fmt::Debug::fmt(ty, __formatter)?;
                __formatter.write_str("}")
            },
            Pattern::Ident{} => {
                __formatter.write_str("hir::Ident{")?;
                __formatter.write_str("}")
            }
        }
    }
}
impl ::std::fmt::Debug for Pattern {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        ::std::fmt::Display::fmt(&self, f)
    }
}
impl ::std::default::Default for Pattern {
    fn default() -> Self {
        Pattern::PatLit{lit : ::std::default::Default::default(), ty : ::std::default::Default::default()}
    }
}
#[derive(Eq, Ord, Clone, Hash, PartialEq, PartialOrd, IntoRecord, Mutator, Serialize, Deserialize, FromRecord)]
#[ddlog(rename = "hir::Scope")]
pub enum Scope {
    #[ddlog(rename = "hir::ScopeFunction")]
    ScopeFunction {
        func: FuncId
    },
    #[ddlog(rename = "hir::ScopeSeq1")]
    ScopeSeq1 {
        parent: internment::Intern<Scope>
    },
    #[ddlog(rename = "hir::ScopeSeq2")]
    ScopeSeq2 {
        parent: internment::Intern<Scope>
    },
    #[ddlog(rename = "hir::ScopeToDo")]
    ScopeToDo
}
impl abomonation::Abomonation for Scope{}
impl ::std::fmt::Display for Scope {
    fn fmt(&self, __formatter: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match self {
            Scope::ScopeFunction{func} => {
                __formatter.write_str("hir::ScopeFunction{")?;
                ::std::fmt::Debug::fmt(func, __formatter)?;
                __formatter.write_str("}")
            },
            Scope::ScopeSeq1{parent} => {
                __formatter.write_str("hir::ScopeSeq1{")?;
                ::std::fmt::Debug::fmt(parent, __formatter)?;
                __formatter.write_str("}")
            },
            Scope::ScopeSeq2{parent} => {
                __formatter.write_str("hir::ScopeSeq2{")?;
                ::std::fmt::Debug::fmt(parent, __formatter)?;
                __formatter.write_str("}")
            },
            Scope::ScopeToDo{} => {
                __formatter.write_str("hir::ScopeToDo{")?;
                __formatter.write_str("}")
            }
        }
    }
}
impl ::std::fmt::Debug for Scope {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        ::std::fmt::Display::fmt(&self, f)
    }
}
impl ::std::default::Default for Scope {
    fn default() -> Self {
        Scope::ScopeFunction{func : ::std::default::Default::default()}
    }
}
pub type ScopeId = u32;
#[derive(Eq, Ord, Clone, Hash, PartialEq, PartialOrd, IntoRecord, Mutator, Default, Serialize, Deserialize, FromRecord)]
#[ddlog(rename = "hir::Span")]
pub struct Span {
    pub start: u32,
    pub end: u32
}
impl abomonation::Abomonation for Span{}
impl ::std::fmt::Display for Span {
    fn fmt(&self, __formatter: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match self {
            Span{start,end} => {
                __formatter.write_str("hir::Span{")?;
                ::std::fmt::Debug::fmt(start, __formatter)?;
                __formatter.write_str(",")?;
                ::std::fmt::Debug::fmt(end, __formatter)?;
                __formatter.write_str("}")
            }
        }
    }
}
impl ::std::fmt::Debug for Span {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        ::std::fmt::Display::fmt(&self, f)
    }
}
#[derive(Eq, Ord, Clone, Hash, PartialEq, PartialOrd, IntoRecord, Mutator, Serialize, Deserialize, FromRecord)]
#[ddlog(rename = "hir::Stmt")]
pub enum Stmt {
    #[ddlog(rename = "hir::StmtExpr")]
    StmtExpr {
        expr: ExprId
    },
    #[ddlog(rename = "hir::StmtItem")]
    StmtItem {
        item: ItemId
    },
    #[ddlog(rename = "hir::StmtVarDecl")]
    StmtVarDecl {
        decl: VariableDecl
    },
    #[ddlog(rename = "hir::StmtScope")]
    StmtScope {
        scope: ddlog_std::Vec<StmtId>
    }
}
impl abomonation::Abomonation for Stmt{}
impl ::std::fmt::Display for Stmt {
    fn fmt(&self, __formatter: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match self {
            Stmt::StmtExpr{expr} => {
                __formatter.write_str("hir::StmtExpr{")?;
                ::std::fmt::Debug::fmt(expr, __formatter)?;
                __formatter.write_str("}")
            },
            Stmt::StmtItem{item} => {
                __formatter.write_str("hir::StmtItem{")?;
                ::std::fmt::Debug::fmt(item, __formatter)?;
                __formatter.write_str("}")
            },
            Stmt::StmtVarDecl{decl} => {
                __formatter.write_str("hir::StmtVarDecl{")?;
                ::std::fmt::Debug::fmt(decl, __formatter)?;
                __formatter.write_str("}")
            },
            Stmt::StmtScope{scope} => {
                __formatter.write_str("hir::StmtScope{")?;
                ::std::fmt::Debug::fmt(scope, __formatter)?;
                __formatter.write_str("}")
            }
        }
    }
}
impl ::std::fmt::Debug for Stmt {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        ::std::fmt::Display::fmt(&self, f)
    }
}
impl ::std::default::Default for Stmt {
    fn default() -> Self {
        Stmt::StmtExpr{expr : ::std::default::Default::default()}
    }
}
pub type StmtId = u64;
pub type StrT = u32;
#[derive(Eq, Ord, Clone, Hash, PartialEq, PartialOrd, IntoRecord, Mutator, Default, Serialize, Deserialize, FromRecord)]
#[ddlog(rename = "hir::Type")]
pub struct Type {
    pub kind: TypeKind
}
impl abomonation::Abomonation for Type{}
impl ::std::fmt::Display for Type {
    fn fmt(&self, __formatter: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match self {
            Type{kind} => {
                __formatter.write_str("hir::Type{")?;
                ::std::fmt::Debug::fmt(kind, __formatter)?;
                __formatter.write_str("}")
            }
        }
    }
}
impl ::std::fmt::Debug for Type {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        ::std::fmt::Display::fmt(&self, f)
    }
}
pub type TypeId = u64;
#[derive(Eq, Ord, Clone, Hash, PartialEq, PartialOrd, IntoRecord, Mutator, Serialize, Deserialize, FromRecord)]
#[ddlog(rename = "hir::TypeKind")]
pub enum TypeKind {
    #[ddlog(rename = "hir::Unknown")]
    Unknown,
    #[ddlog(rename = "hir::Str")]
    Str,
    #[ddlog(rename = "hir::Bool")]
    Bool,
    #[ddlog(rename = "hir::Unit")]
    Unit,
    #[ddlog(rename = "hir::Absurd")]
    Absurd,
    #[ddlog(rename = "hir::Error")]
    Error,
    #[ddlog(rename = "hir::Int")]
    Int {
        is_signed: ddlog_std::Option<bool>,
        width: ddlog_std::Option<u16>
    }
}
impl abomonation::Abomonation for TypeKind{}
impl ::std::fmt::Display for TypeKind {
    fn fmt(&self, __formatter: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match self {
            TypeKind::Unknown{} => {
                __formatter.write_str("hir::Unknown{")?;
                __formatter.write_str("}")
            },
            TypeKind::Str{} => {
                __formatter.write_str("hir::Str{")?;
                __formatter.write_str("}")
            },
            TypeKind::Bool{} => {
                __formatter.write_str("hir::Bool{")?;
                __formatter.write_str("}")
            },
            TypeKind::Unit{} => {
                __formatter.write_str("hir::Unit{")?;
                __formatter.write_str("}")
            },
            TypeKind::Absurd{} => {
                __formatter.write_str("hir::Absurd{")?;
                __formatter.write_str("}")
            },
            TypeKind::Error{} => {
                __formatter.write_str("hir::Error{")?;
                __formatter.write_str("}")
            },
            TypeKind::Int{is_signed,width} => {
                __formatter.write_str("hir::Int{")?;
                ::std::fmt::Debug::fmt(is_signed, __formatter)?;
                __formatter.write_str(",")?;
                ::std::fmt::Debug::fmt(width, __formatter)?;
                __formatter.write_str("}")
            }
        }
    }
}
impl ::std::fmt::Debug for TypeKind {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        ::std::fmt::Display::fmt(&self, f)
    }
}
impl ::std::default::Default for TypeKind {
    fn default() -> Self {
        TypeKind::Unknown{}
    }
}
pub type VarId = u64;
#[derive(Eq, Ord, Clone, Hash, PartialEq, PartialOrd, IntoRecord, Mutator, Default, Serialize, Deserialize, FromRecord)]
#[ddlog(rename = "hir::VariableDecl")]
pub struct VariableDecl {
    pub var_name: StrT,
    pub var_type: TypeId,
    pub value: ExprId,
    pub scope: ScopeId
}
impl abomonation::Abomonation for VariableDecl{}
impl ::std::fmt::Display for VariableDecl {
    fn fmt(&self, __formatter: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match self {
            VariableDecl{var_name,var_type,value,scope} => {
                __formatter.write_str("hir::VariableDecl{")?;
                ::std::fmt::Debug::fmt(var_name, __formatter)?;
                __formatter.write_str(",")?;
                ::std::fmt::Debug::fmt(var_type, __formatter)?;
                __formatter.write_str(",")?;
                ::std::fmt::Debug::fmt(value, __formatter)?;
                __formatter.write_str(",")?;
                ::std::fmt::Debug::fmt(scope, __formatter)?;
                __formatter.write_str("}")
            }
        }
    }
}
impl ::std::fmt::Debug for VariableDecl {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        ::std::fmt::Display::fmt(&self, f)
    }
}
#[derive(Eq, Ord, Clone, Hash, PartialEq, PartialOrd, IntoRecord, Mutator, Serialize, Deserialize, FromRecord)]
#[ddlog(rename = "hir::Vis")]
pub enum Vis {
    #[ddlog(rename = "hir::FileLocal")]
    FileLocal,
    #[ddlog(rename = "hir::Package")]
    Package,
    #[ddlog(rename = "hir::Exposed")]
    Exposed
}
impl abomonation::Abomonation for Vis{}
impl ::std::fmt::Display for Vis {
    fn fmt(&self, __formatter: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match self {
            Vis::FileLocal{} => {
                __formatter.write_str("hir::FileLocal{")?;
                __formatter.write_str("}")
            },
            Vis::Package{} => {
                __formatter.write_str("hir::Package{")?;
                __formatter.write_str("}")
            },
            Vis::Exposed{} => {
                __formatter.write_str("hir::Exposed{")?;
                __formatter.write_str("}")
            }
        }
    }
}
impl ::std::fmt::Debug for Vis {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        ::std::fmt::Display::fmt(&self, f)
    }
}
impl ::std::default::Default for Vis {
    fn default() -> Self {
        Vis::FileLocal{}
    }
}
pub fn is_bool(ty: & internment::Intern<Literal>) -> bool
{   match (*internment::ival(ty)) {
        Literal::Boolean{boolean: _} => true,
        _ => false
    }
}
pub fn is_int_hir_Type___Boolval(ty: & Type) -> bool
{   match ty.kind {
        TypeKind::Int{is_signed: _, width: _} => true,
        _ => false
    }
}
pub fn is_int_internment_Intern__hir_Literal___Boolval(ty: & internment::Intern<Literal>) -> bool
{   match (*internment::ival(ty)) {
        Literal::Integer{int: _} => true,
        _ => false
    }
}
pub fn is_str(ty: & internment::Intern<Literal>) -> bool
{   match (*internment::ival(ty)) {
        Literal::String{str: _} => true,
        _ => false
    }
}
pub fn is_unknown(ty: & Type) -> bool
{   ((&*(&ty.kind)) == (&*(&(TypeKind::Unknown{}))))
}
pub fn type_of(literal: & internment::Intern<Literal>) -> Type
{   let ref mut ty: TypeKind = match (*internment::ival(literal)) {
                                   Literal::String{str: _} => (TypeKind::Str{}),
                                   Literal::Boolean{boolean: _} => (TypeKind::Bool{}),
                                   Literal::Integer{int: _} => (TypeKind::Int{is_signed: (ddlog_std::Option::None{}), width: (ddlog_std::Option::None{})})
                               };
    (Type{kind: (*ty).clone()})
}