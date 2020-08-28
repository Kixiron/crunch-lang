use crate::{
    error::{Locatable, Location, Span},
    strings::{StrInterner, StrT},
    trees::{CallConv, ItemPath, Sided, Sign},
};
#[cfg(feature = "no-std")]
use alloc::{
    borrow::ToOwned,
    format,
    string::{String, ToString},
    vec,
    vec::Vec,
};
use core::fmt::{Debug, Display, Formatter, Result, Write};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Item<'ctx> {
    pub decorators: Vec<Decorator<'ctx>>,
    pub attrs: Vec<Attribute>,
    pub kind: ItemKind<'ctx>,
    pub loc: Location,
    pub name: Option<StrT>,
    pub vis: Option<Vis>,
}

impl<'ctx> Item<'ctx> {
    pub const fn location(&self) -> Location {
        self.loc
    }

    pub fn span(&self) -> Span {
        self.loc.span()
    }
}

// #[nanopass(file = "src/passes/ast.toml")]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ItemKind<'ctx> {
    Func {
        generics: Option<Locatable<Vec<Locatable<&'ctx Type<'ctx>>>>>,
        args: Locatable<Vec<FuncArg<'ctx>>>,
        body: Block<'ctx>,
        ret: Locatable<&'ctx Type<'ctx>>,
        sig: Location,
    },

    Type {
        generics: Option<Locatable<Vec<Locatable<&'ctx Type<'ctx>>>>>,
        members: Vec<TypeMember<'ctx>>,
    },

    Enum {
        generics: Option<Locatable<Vec<Locatable<&'ctx Type<'ctx>>>>>,
        variants: Vec<Variant<'ctx>>,
    },

    Trait {
        generics: Option<Locatable<Vec<Locatable<&'ctx Type<'ctx>>>>>,
        methods: Vec<&'ctx Item<'ctx>>,
    },

    // TODO: Should imports have visibility for exporting or should that be a separate node?
    Import {
        file: ItemPath,
        dest: Dest,
        exposes: Exposure,
    },

    ExtendBlock(ExtendBlock<'ctx>),

    Alias {
        alias: Locatable<&'ctx Type<'ctx>>,
        actual: Locatable<&'ctx Type<'ctx>>,
    },

    ExternBlock(ExternBlock<'ctx>),
    ExternFunc(ExternFunc<'ctx>),
}

impl<'ctx> ItemKind<'ctx> {
    pub fn is_func(&self) -> bool {
        matches!(self, Self::Func { .. })
    }

    pub fn is_type(&self) -> bool {
        matches!(self, Self::Type { .. })
    }

    pub fn is_enum(&self) -> bool {
        matches!(self, Self::Enum { .. })
    }

    pub fn is_trait(&self) -> bool {
        matches!(self, Self::Trait { .. })
    }

    pub fn is_import(&self) -> bool {
        matches!(self, Self::Import { .. })
    }

    pub fn is_extend_block(&self) -> bool {
        matches!(self, Self::ExtendBlock { .. })
    }

    pub fn is_alias(&self) -> bool {
        matches!(self, Self::Alias { .. })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExtendBlock<'ctx> {
    pub target: Locatable<&'ctx Type<'ctx>>,
    pub extender: Option<Locatable<&'ctx Type<'ctx>>>,
    pub items: Vec<&'ctx Item<'ctx>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExternBlock<'ctx> {
    pub items: Vec<&'ctx Item<'ctx>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExternFunc<'ctx> {
    pub generics: Option<Locatable<Vec<Locatable<&'ctx Type<'ctx>>>>>,
    pub args: Locatable<Vec<FuncArg<'ctx>>>,
    pub ret: Locatable<&'ctx Type<'ctx>>,
    pub callconv: CallConv,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Exposure {
    None(StrT),
    All,
    Items(Vec<(ItemPath, StrT)>),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Dest {
    NativeLib,
    Package,
    Relative,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeMember<'ctx> {
    pub decorators: Vec<Decorator<'ctx>>,
    pub attrs: Vec<Attribute>,
    pub name: StrT,
    pub ty: Locatable<&'ctx Type<'ctx>>,
    // pub loc: Location,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Variant<'ctx> {
    Unit {
        name: StrT,
        decorators: Vec<Decorator<'ctx>>,
    },

    Tuple {
        name: StrT,
        elms: Vec<Locatable<&'ctx Type<'ctx>>>,
        decorators: Vec<Decorator<'ctx>>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Decorator<'ctx> {
    pub name: Locatable<StrT>,
    pub args: Vec<&'ctx Expr<'ctx>>,
    pub loc: Location,
}

impl<'ctx> Decorator<'ctx> {
    pub const fn location(&self) -> Location {
        self.loc
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Attribute {
    Const,
    Async,
    Unsafe,
}

impl Display for Attribute {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let pretty = match self {
            Self::Const => "const",
            Self::Async => "async",
            Self::Unsafe => "unsafe",
        };

        f.write_str(pretty)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Vis {
    FileLocal,
    Package,
    Exposed,
}

impl Default for Vis {
    fn default() -> Self {
        Self::FileLocal
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FuncArg<'ctx> {
    pub name: StrT,
    pub ty: Locatable<&'ctx Type<'ctx>>,
    pub loc: Location,
}

impl<'ctx> FuncArg<'ctx> {
    pub const fn location(&self) -> Location {
        self.loc
    }

    pub fn span(&self) -> Span {
        self.loc.span()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Stmt<'ctx> {
    pub kind: StmtKind<'ctx>,
    pub loc: Location,
}

impl<'ctx> Stmt<'ctx> {
    pub const fn location(&self) -> Location {
        self.loc
    }

    pub fn span(&self) -> Span {
        self.loc.span()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum StmtKind<'ctx> {
    VarDecl(VarDecl<'ctx>),
    Item(&'ctx Item<'ctx>),
    Expr(&'ctx Expr<'ctx>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct VarDecl<'ctx> {
    pub name: StrT,
    pub ty: Locatable<&'ctx Type<'ctx>>,
    pub val: &'ctx Expr<'ctx>,
    pub constant: bool,
    pub mutable: bool,
    // pub loc: Location,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Expr<'ctx> {
    pub kind: ExprKind<'ctx>,
    pub loc: Location,
    // TODO: Type of expression
}

impl<'ctx> Expr<'ctx> {
    pub fn location(&self) -> Location {
        self.loc
    }

    pub fn span(&self) -> Span {
        self.loc.span()
    }

    pub fn as_literal(&self) -> Option<&Locatable<Literal<'ctx>>> {
        if let ExprKind::Literal(ref literal) = self.kind {
            Some(literal)
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ExprKind<'ctx> {
    If(If<'ctx>),
    Return(Option<&'ctx Expr<'ctx>>),
    Break(Option<&'ctx Expr<'ctx>>),
    Continue,
    While(While<'ctx>),
    Loop(Loop<'ctx>),
    For(For<'ctx>),
    Match(Match<'ctx>),
    Variable(Locatable<StrT>),
    Literal(Locatable<Literal<'ctx>>),
    UnaryOp(UnaryOp, &'ctx Expr<'ctx>),
    BinaryOp(Sided<BinaryOp, &'ctx Expr<'ctx>>),
    Comparison(Sided<CompOp, &'ctx Expr<'ctx>>),
    Assign(Sided<AssignKind, &'ctx Expr<'ctx>>),
    Paren(&'ctx Expr<'ctx>),
    Array(Vec<&'ctx Expr<'ctx>>),
    Tuple(Vec<&'ctx Expr<'ctx>>),
    // TODO: Add range kind (inclusive, exclusive, etc.)
    Range(&'ctx Expr<'ctx>, &'ctx Expr<'ctx>),
    Index {
        var: &'ctx Expr<'ctx>,
        index: &'ctx Expr<'ctx>,
    },
    FuncCall {
        caller: &'ctx Expr<'ctx>,
        args: Vec<&'ctx Expr<'ctx>>,
    },
    MemberFuncCall {
        member: &'ctx Expr<'ctx>,
        func: &'ctx Expr<'ctx>,
    },
    Reference {
        mutable: bool,
        expr: &'ctx Expr<'ctx>,
    },
    Cast {
        expr: &'ctx Expr<'ctx>,
        ty: Locatable<&'ctx Type<'ctx>>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct If<'ctx> {
    pub clauses: Vec<IfCond<'ctx>>,
    pub else_: Option<Block<'ctx>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IfCond<'ctx> {
    pub cond: &'ctx Expr<'ctx>,
    pub body: Block<'ctx>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct While<'ctx> {
    pub cond: &'ctx Expr<'ctx>,
    pub body: Block<'ctx>,
    pub then: Option<Block<'ctx>>,
    pub else_: Option<Block<'ctx>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Loop<'ctx> {
    pub body: Block<'ctx>,
    pub else_: Option<Block<'ctx>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct For<'ctx> {
    pub var: &'ctx Expr<'ctx>,
    pub cond: &'ctx Expr<'ctx>,
    pub body: Block<'ctx>,
    pub then: Option<Block<'ctx>>,
    pub else_: Option<Block<'ctx>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Match<'ctx> {
    pub var: &'ctx Expr<'ctx>,
    pub arms: Vec<Arm<'ctx>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Arm<'ctx> {
    pub bind: Binding<'ctx>,
    pub guard: Option<&'ctx Expr<'ctx>>,
    pub body: Block<'ctx>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Literal<'ctx> {
    pub val: LiteralVal<'ctx>,
    pub ty: &'ctx Type<'ctx>,
    pub loc: Location,
}

impl<'ctx> Literal<'ctx> {
    pub fn location(&self) -> Location {
        self.loc
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum LiteralVal<'ctx> {
    Integer(Integer),
    Bool(bool),
    String(Text),
    Rune(Rune),
    Float(Float),
    Array(Vec<Literal<'ctx>>),
    // TODO: Tuples, slices, others?
}

impl<'ctx> LiteralVal<'ctx> {
    pub fn into_integer(self) -> Option<Integer> {
        if let Self::Integer(int) = self {
            Some(int)
        } else {
            None
        }
    }

    pub fn as_string(&self) -> Option<&Text> {
        if let Self::String(ref text) = self {
            Some(text)
        } else {
            None
        }
    }
}

impl Display for LiteralVal<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Integer(int) => write!(f, "{}", int),
            Self::Bool(b) => write!(f, "{}", b),
            Self::String(text) => write!(f, "{}", text),
            Self::Rune(rune) => write!(f, "{}", rune),
            Self::Float(float) => write!(f, "{}", float),
            Self::Array(arr) => write!(
                f,
                "[{}]",
                arr.iter()
                    .map(|elm| format!("{}", elm.val))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct Text(String);

impl Text {
    pub fn new(text: String) -> Self {
        Self(text)
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        self.0.clone().into_bytes()
    }
}

impl Debug for Text {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        f.write_str(&self.0)
    }
}

impl Display for Text {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        f.write_str(&self.0)
    }
}

impl From<&str> for Text {
    fn from(string: &str) -> Self {
        Self(string.to_owned())
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct Rune(u32);

impl Rune {
    pub const fn as_u32(self) -> u32 {
        self.0
    }

    pub fn from_u32(i: u32) -> Option<Self> {
        core::char::from_u32(i).map(|i| Self(i as u32))
    }

    pub fn as_char(self) -> char {
        core::char::from_u32(self.0).unwrap()
    }

    pub const fn from_char(i: char) -> Self {
        Self(i as u32)
    }
}

impl PartialEq<char> for Rune {
    fn eq(&self, other: &char) -> bool {
        self.as_u32() == *other as u32
    }
}

impl From<char> for Rune {
    fn from(c: char) -> Self {
        Self(c as u32)
    }
}

impl From<u32> for Rune {
    fn from(i: u32) -> Self {
        Self(i)
    }
}

impl Debug for Rune {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", &self.as_char())
    }
}

impl Display for Rune {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", &self.as_char())
    }
}
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Integer {
    pub sign: Sign,
    pub bits: u128,
}

impl Display for Integer {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}{}", &self.sign, &self.bits)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct Float(pub u64);

impl Display for Float {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", f64::from_bits(self.0))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type<'ctx> {
    Operand(Sided<TypeOp, Locatable<&'ctx Type<'ctx>>>),
    Const(StrT, Locatable<&'ctx Type<'ctx>>),
    Not(Locatable<&'ctx Type<'ctx>>),
    Paren(Locatable<&'ctx Type<'ctx>>),
    Func {
        params: Vec<Locatable<&'ctx Type<'ctx>>>,
        ret: Locatable<&'ctx Type<'ctx>>,
    },
    Trait(Vec<Locatable<&'ctx Type<'ctx>>>),
    Bounded {
        path: ItemPath,
        bounds: Vec<Locatable<&'ctx Type<'ctx>>>,
    },
    ItemPath(ItemPath),
    Unknown,
    Integer {
        signed: Option<bool>,
        width: Option<u16>,
    },
    IntReg {
        signed: bool,
    },
    IntPtr {
        signed: bool,
    },
    Float {
        width: u16,
    },
    Bool,
    String,
    Rune,
    Absurd,
    Unit,
    Array {
        element: Locatable<&'ctx Type<'ctx>>,
        length: u64,
    },
    Slice {
        element: Locatable<&'ctx Type<'ctx>>,
    },
    Tuple(Vec<Locatable<&'ctx Type<'ctx>>>),
    Pointer {
        pointee: Locatable<&'ctx Type<'ctx>>,
        mutable: bool,
    },
    Reference {
        referee: Locatable<&'ctx Type<'ctx>>,
        mutable: bool,
    },
}

impl<'ctx> Type<'ctx> {
    pub fn internal_types(&self) -> Vec<ItemPath> {
        let mut buf = Vec::with_capacity(1);
        self.internal_types_inner(&mut buf);

        buf
    }

    fn internal_types_inner(&self, buf: &mut Vec<ItemPath>) {
        match self {
            Self::Operand(Sided { rhs, op: _, lhs }) => {
                rhs.internal_types_inner(buf);
                lhs.internal_types_inner(buf);
            }
            Self::Const(name, ty) => {
                buf.push(ItemPath::new(vec![*name]));
                ty.internal_types_inner(buf);
            }
            Self::Not(ty) => ty.internal_types_inner(buf),
            Self::Paren(ty) => ty.internal_types_inner(buf),
            Self::Func { params, ret } => {
                for param in params {
                    param.internal_types_inner(buf);
                }

                ret.internal_types_inner(buf);
            }
            Self::Trait(types) => {
                for ty in types {
                    ty.internal_types_inner(buf);
                }
            }
            Self::Bounded { path, bounds } => {
                buf.push(path.clone());

                for bound in bounds {
                    bound.internal_types_inner(buf);
                }
            }
            Self::ItemPath(path) => buf.push(path.clone()),

            _ => todo!(),
        }
    }

    // TODO: Optimize by writing into a buffer
    pub fn to_string(&self, intern: &StrInterner) -> String {
        match self {
            Self::Unknown => "{{unknown}}".to_string(),
            Self::Not(ty) => format!("!{}", ty.to_string(intern)),
            Self::Paren(ty) => format!("({})", ty.to_string(intern)),
            Self::Const(ident, ty) => format!(
                "const {}: {}",
                intern.resolve(*ident).as_ref(),
                ty.to_string(intern),
            ),
            Self::Operand(Sided { lhs, op, rhs }) => {
                format!("{} {} {}", lhs.to_string(intern), op, rhs.to_string(intern),)
            }
            Self::Func { params, ret } => format!(
                "fn({}) -> {}",
                params
                    .iter()
                    .map(|ty| ty.to_string(intern))
                    .collect::<Vec<String>>()
                    .join(", "),
                ret.to_string(intern)
            ),
            Self::Trait(traits) => format!(
                "type[{}]",
                traits
                    .iter()
                    .map(|ty| ty.to_string(intern))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Self::Bounded { path, bounds } => format!(
                "{}[{}]",
                path.to_string(intern),
                bounds
                    .iter()
                    .map(|ty| ty.to_string(intern))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Self::ItemPath(path) => path.to_string(intern),
            Self::Integer { signed, width } => format!(
                "{}{}",
                if signed.unwrap_or(true) { "i" } else { "u" },
                width.unwrap_or(32),
            ),
            Self::IntPtr { signed } => format!("{}ptr", if *signed { "i" } else { "u" }),
            Self::IntReg { signed } => format!("{}reg", if *signed { "i" } else { "u" }),
            Self::Float { width } => format!("f{}", width),
            Self::Bool => "bool".to_string(),
            Self::String => "str".to_string(),
            Self::Rune => "rune".to_string(),
            Self::Unit => "unit".to_string(),
            Self::Absurd => "absurd".to_string(),
            Self::Array { element, length } => {
                format!("arr[{}; {}]", length, element.to_string(intern))
            }
            Self::Slice { element } => format!("slice[{}]", element.to_string(intern)),
            Self::Tuple(types) => format!(
                "tup[{}]",
                types
                    .iter()
                    .map(|ty| ty.to_string(intern))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),

            _ => todo!(),
        }
    }
}

impl<'ctx> Default for Type<'ctx> {
    fn default() -> Self {
        Self::Unit
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum TypeOp {
    And,
    Or,
}

impl Display for TypeOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let op = match self {
            Self::And => '&',
            Self::Or => '|',
        };

        f.write_char(op)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Binding<'ctx> {
    // TODO: Enum for mutability/referential status?
    pub reference: bool,
    pub mutable: bool,
    pub pattern: Pattern<'ctx>,
    pub ty: Option<Locatable<&'ctx Type<'ctx>>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Pattern<'ctx> {
    Literal(Literal<'ctx>),
    Ident(StrT),
    ItemPath(ItemPath),
    Wildcard,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Block<'ctx> {
    pub stmts: Vec<&'ctx Stmt<'ctx>>,
    pub loc: Location,
}

impl<'ctx> Block<'ctx> {
    pub const fn location(&self) -> Location {
        self.loc
    }

    pub fn span(&self) -> Span {
        self.loc.span()
    }

    pub fn len(&self) -> usize {
        self.stmts.len()
    }

    pub fn is_empty(&self) -> bool {
        self.stmts.is_empty()
    }

    pub fn iter<'a>(&'a self) -> impl Iterator<Item = &'ctx Stmt<'ctx>> + 'a {
        self.stmts.iter().copied()
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum CompOp {
    Equal,
    NotEqual,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
}

impl Display for CompOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let pretty = match self {
            Self::Greater => ">",
            Self::Less => "<",
            Self::GreaterEqual => ">=",
            Self::LessEqual => "<=",
            Self::Equal => "==",
            Self::NotEqual => "!=",
        };

        f.write_str(pretty)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum AssignKind {
    Normal,
    BinaryOp(BinaryOp),
}

impl Display for AssignKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Normal => f.write_str(":="),
            Self::BinaryOp(op) => write!(f, "{}=", op),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum BinaryOp {
    Mult,
    Div,
    Add,
    Sub,
    Mod,
    Pow,
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let pretty = match self {
            Self::Add => "+",
            Self::Sub => "-",
            Self::Mult => "*",
            Self::Div => "/",
            Self::Mod => "%",
            Self::Pow => "**",
            Self::BitAnd => "&",
            Self::BitOr => "|",
            Self::BitXor => "^",
            Self::Shl => "<<",
            Self::Shr => ">>",
        };

        f.write_str(pretty)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum UnaryOp {
    Positive,
    Negative,
    Not,
}

impl Display for UnaryOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let pretty = match self {
            Self::Positive => '+',
            Self::Negative => '-',
            Self::Not => '!',
        };

        f.write_char(pretty)
    }
}
