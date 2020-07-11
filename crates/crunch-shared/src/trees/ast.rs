use crate::{
    error::{Locatable, Location, Span},
    strings::{StrInterner, StrT},
    trees::{CallConv, ItemPath, Ref, Sided, Signedness},
};
#[cfg(feature = "no-std")]
use alloc::{
    borrow::ToOwned,
    format,
    string::{String, ToString},
    vec,
    vec::Vec,
};
use core::{
    fmt::{Debug, Display, Formatter, Result, Write},
    ops::Not,
};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Eq, Hash, Deserialize, Serialize)]
pub struct Item {
    pub decorators: Vec<Decorator>,
    pub attrs: Vec<Attribute>,
    pub kind: ItemKind,
    pub loc: Location,
    pub name: Option<StrT>,
    pub vis: Option<Vis>,
}

impl Item {
    #[inline]
    pub const fn location(&self) -> Location {
        self.loc
    }

    #[inline]
    pub fn span(&self) -> Span {
        self.loc.span()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Deserialize, Serialize)]
pub enum ItemKind {
    Func {
        generics: Option<Locatable<Vec<Locatable<Type>>>>,
        args: Locatable<Vec<FuncArg>>,
        body: Block,
        ret: Ref<Locatable<Type>>,
        sig: Location,
    },

    Type {
        generics: Option<Locatable<Vec<Locatable<Type>>>>,
        members: Vec<TypeMember>,
    },

    Enum {
        generics: Option<Locatable<Vec<Locatable<Type>>>>,
        variants: Vec<Variant>,
    },

    Trait {
        generics: Option<Locatable<Vec<Locatable<Type>>>>,
        methods: Vec<Item>,
    },

    // TODO: Should imports have visibility for exporting or should that be a separate node?
    Import {
        file: ItemPath,
        dest: Dest,
        exposes: Exposure,
    },

    ExtendBlock {
        target: Ref<Locatable<Type>>,
        extender: Option<Ref<Locatable<Type>>>,
        items: Vec<Item>,
    },

    Alias {
        alias: Ref<Locatable<Type>>,
        actual: Ref<Locatable<Type>>,
    },

    ExternBlock {
        items: Vec<Item>,
    },

    ExternFunc {
        generics: Option<Locatable<Vec<Locatable<Type>>>>,
        args: Locatable<Vec<FuncArg>>,
        ret: Ref<Locatable<Type>>,
        callconv: CallConv,
    },
}

impl ItemKind {
    #[inline]
    pub fn is_func(&self) -> bool {
        matches!(self, Self::Func { .. })
    }

    #[inline]
    pub fn is_type(&self) -> bool {
        matches!(self, Self::Type { .. })
    }

    #[inline]
    pub fn is_enum(&self) -> bool {
        matches!(self, Self::Enum { .. })
    }

    #[inline]
    pub fn is_trait(&self) -> bool {
        matches!(self, Self::Trait { .. })
    }

    #[inline]
    pub fn is_import(&self) -> bool {
        matches!(self, Self::Import { .. })
    }

    #[inline]
    pub fn is_extend_block(&self) -> bool {
        matches!(self, Self::ExtendBlock { .. })
    }

    #[inline]
    pub fn is_alias(&self) -> bool {
        matches!(self, Self::Alias { .. })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Deserialize, Serialize)]
pub enum Exposure {
    None(StrT),
    All,
    Items(Vec<(ItemPath, StrT)>),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Deserialize, Serialize)]
pub enum Dest {
    NativeLib,
    Package,
    Relative,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Deserialize, Serialize)]
pub struct TypeMember {
    pub decorators: Vec<Decorator>,
    pub attrs: Vec<Attribute>,
    pub name: StrT,
    pub ty: Ref<Locatable<Type>>,
    // pub loc: Location,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Deserialize, Serialize)]
pub enum Variant {
    Unit {
        name: StrT,
        decorators: Vec<Decorator>,
    },

    Tuple {
        name: StrT,
        elms: Vec<Locatable<Type>>,
        decorators: Vec<Decorator>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Deserialize, Serialize)]
pub struct Decorator {
    pub name: Locatable<StrT>,
    pub args: Vec<Expr>,
    pub loc: Location,
}

impl Decorator {
    pub const fn location(&self) -> Location {
        self.loc
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Deserialize, Serialize)]
pub enum Attribute {
    Const,
    Async,
    Unsafe,
}

impl Display for Attribute {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let pretty = match self {
            Self::Const => "const",
            Self::Async => "async",
            Self::Unsafe => "unsafe",
        };

        f.write_str(pretty)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Deserialize, Serialize)]
pub enum Vis {
    FileLocal,
    Package,
    Exposed,
}

impl Default for Vis {
    #[inline]
    fn default() -> Self {
        Self::FileLocal
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Deserialize, Serialize)]
pub struct FuncArg {
    pub name: StrT,
    pub ty: Ref<Locatable<Type>>,
    pub loc: Location,
}

impl FuncArg {
    #[inline]
    pub const fn location(&self) -> Location {
        self.loc
    }

    #[inline]
    pub fn span(&self) -> Span {
        self.loc.span()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Deserialize, Serialize)]
pub struct Stmt {
    pub kind: StmtKind,
    pub loc: Location,
}

impl Stmt {
    #[inline]
    pub const fn location(&self) -> Location {
        self.loc
    }

    #[inline]
    pub fn span(&self) -> Span {
        self.loc.span()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Deserialize, Serialize)]
pub enum StmtKind {
    VarDecl(Ref<VarDecl>),
    Item(Ref<Item>),
    Expr(Ref<Expr>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Deserialize, Serialize)]
pub struct VarDecl {
    pub name: StrT,
    pub ty: Ref<Locatable<Type>>,
    pub val: Ref<Expr>,
    pub constant: bool,
    pub mutable: bool,
    // pub loc: Location,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Deserialize, Serialize)]
pub struct Expr {
    pub kind: ExprKind,
    pub loc: Location,
    // TODO: Type of expression
}

impl Expr {
    #[inline]
    pub fn location(&self) -> Location {
        self.loc
    }

    #[inline]
    pub fn span(&self) -> Span {
        self.loc.span()
    }

    pub fn as_literal(&self) -> Option<&Locatable<Literal>> {
        if let ExprKind::Literal(ref literal) = self.kind {
            Some(literal)
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Deserialize, Serialize)]
pub enum ExprKind {
    If(If),
    Return(Option<Ref<Expr>>),
    Break(Option<Ref<Expr>>),
    Continue,
    While(While),
    Loop(Loop),
    For(For),
    Match(Match),
    Variable(Locatable<StrT>),
    Literal(Locatable<Literal>),
    UnaryOp(UnaryOp, Ref<Expr>),
    BinaryOp(Sided<BinaryOp, Ref<Expr>>),
    Comparison(Sided<CompOp, Ref<Expr>>),
    Assign(Sided<AssignKind, Ref<Expr>>),
    Paren(Ref<Expr>),
    Array(Vec<Expr>),
    Tuple(Vec<Expr>),
    // TODO: Add range kind (inclusive, exclusive, etc.)
    Range(Ref<Expr>, Ref<Expr>),
    Index {
        var: Ref<Expr>,
        index: Ref<Expr>,
    },
    FuncCall {
        caller: Ref<Expr>,
        args: Vec<Expr>,
    },
    MemberFuncCall {
        member: Ref<Expr>,
        func: Ref<Expr>,
    },
    Reference {
        mutable: bool,
        expr: Ref<Expr>,
    },
    Cast {
        expr: Ref<Expr>,
        ty: Ref<Locatable<Type>>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Deserialize, Serialize)]
pub struct If {
    pub clauses: Vec<IfCond>,
    pub else_: Option<Block>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Deserialize, Serialize)]
pub struct IfCond {
    pub cond: Ref<Expr>,
    pub body: Block,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Deserialize, Serialize)]
pub struct While {
    pub cond: Ref<Expr>,
    pub body: Block,
    pub then: Option<Block>,
    pub else_: Option<Block>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Deserialize, Serialize)]
pub struct Loop {
    pub body: Block,
    pub else_: Option<Block>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Deserialize, Serialize)]
pub struct For {
    pub var: Ref<Expr>,
    pub cond: Ref<Expr>,
    pub body: Block,
    pub then: Option<Block>,
    pub else_: Option<Block>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Deserialize, Serialize)]
pub struct Match {
    pub var: Ref<Expr>,
    pub arms: Vec<Arm>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Deserialize, Serialize)]
pub struct Arm {
    pub bind: Binding,
    pub guard: Option<Ref<Expr>>,
    pub body: Block,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Deserialize, Serialize)]
pub enum Literal {
    Integer(Integer),
    Bool(bool),
    String(Text),
    Rune(Rune),
    Float(Float),
    Array(Vec<Literal>),
    // TODO: Tuples, slices, others?
}

impl Literal {
    #[inline]
    pub fn into_integer(self) -> Option<Integer> {
        if let Self::Integer(int) = self {
            Some(int)
        } else {
            None
        }
    }

    #[inline]
    pub fn as_string(&self) -> Option<&Text> {
        if let Self::String(ref text) = self {
            Some(text)
        } else {
            None
        }
    }
}

impl Display for Literal {
    #[inline]
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
                    .map(|elm| format!("{}", elm))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Deserialize, Serialize)]
#[repr(transparent)]
pub struct Text(String);

impl Text {
    #[inline]
    pub fn new(text: String) -> Self {
        Self(text)
    }

    #[inline]
    pub fn to_bytes(&self) -> Vec<u8> {
        self.0.clone().into_bytes()
    }
}

impl Debug for Text {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        f.write_str(&self.0)
    }
}

impl Display for Text {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        f.write_str(&self.0)
    }
}

impl From<&str> for Text {
    #[inline]
    fn from(string: &str) -> Self {
        Self(string.to_owned())
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Deserialize, Serialize)]
#[repr(transparent)]
pub struct Rune(u32);

impl Rune {
    #[inline]
    pub const fn as_u32(self) -> u32 {
        self.0
    }

    #[inline]
    pub fn from_u32(i: u32) -> Option<Self> {
        core::char::from_u32(i).map(|i| Self(i as u32))
    }

    #[inline]
    pub fn as_char(self) -> char {
        core::char::from_u32(self.0).unwrap()
    }

    #[inline]
    pub const fn from_char(i: char) -> Self {
        Self(i as u32)
    }
}

impl PartialEq<char> for Rune {
    #[inline]
    fn eq(&self, other: &char) -> bool {
        self.as_u32() == *other as u32
    }
}

impl From<char> for Rune {
    #[inline]
    fn from(c: char) -> Self {
        Self(c as u32)
    }
}

impl From<u32> for Rune {
    #[inline]
    fn from(i: u32) -> Self {
        Self(i)
    }
}

impl Debug for Rune {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", &self.as_char())
    }
}

impl Display for Rune {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", &self.as_char())
    }
}
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Deserialize, Serialize)]
pub struct Integer {
    pub sign: Sign,
    pub bits: u128,
}

impl Display for Integer {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}{}", &self.sign, &self.bits)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Deserialize, Serialize)]
pub enum Sign {
    Positive,
    Negative,
}

impl Sign {
    #[inline]
    pub fn is_negative(self) -> bool {
        self == Self::Negative
    }

    #[inline]
    pub fn maybe_negate<T>(self, integer: T) -> T
    where
        T: Not<Output = T>,
    {
        if self.is_negative() {
            !integer
        } else {
            integer
        }
    }
}

impl Display for Sign {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "{}",
            match self {
                Self::Positive => "",
                Self::Negative => "-",
            },
        )
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Deserialize, Serialize)]
#[repr(transparent)]
pub struct Float(pub u64);

impl Display for Float {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", f64::from_bits(self.0))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Deserialize, Serialize)]
pub enum Type {
    Operand(Sided<TypeOp, Ref<Locatable<Type>>>),
    Const(StrT, Ref<Locatable<Type>>),
    Not(Ref<Locatable<Type>>),
    Paren(Ref<Locatable<Type>>),
    Func {
        params: Vec<Locatable<Type>>,
        ret: Ref<Locatable<Type>>,
    },
    Trait(Vec<Locatable<Type>>),
    Bounded {
        path: ItemPath,
        bounds: Vec<Locatable<Type>>,
    },
    ItemPath(ItemPath),
    Infer,
    Integer {
        sign: Signedness,
        width: u16,
    },
    IntReg(Signedness),
    IntPtr(Signedness),
    Float {
        width: u16,
    },
    Bool,
    String,
    Rune,
    Absurd,
    Unit,
    Array(u128, Ref<Locatable<Type>>),
    Slice(Ref<Locatable<Type>>),
    Tuple(Vec<Locatable<Type>>),
    Pointer {
        mutable: bool,
        ty: Ref<Locatable<Type>>,
    },
    Reference {
        mutable: bool,
        ty: Ref<Locatable<Type>>,
    },
}

impl Type {
    #[inline]
    pub fn internal_types(&self) -> Vec<ItemPath> {
        let mut buf = Vec::with_capacity(1);
        self.internal_types_inner(&mut buf);

        buf
    }

    #[inline]
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
    #[inline]
    pub fn to_string(&self, intern: &StrInterner) -> String {
        match self {
            Self::Infer => "infer".to_string(),
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
            Self::Integer { sign, width } => format!(
                "{}{}",
                match sign {
                    Signedness::Signed => "i",
                    Signedness::Unsigned => "u",
                },
                width
            ),
            Self::IntPtr(sign) => format!(
                "{}ptr",
                match sign {
                    Signedness::Signed => "i",
                    Signedness::Unsigned => "u",
                },
            ),
            Self::IntReg(sign) => format!(
                "{}reg",
                match sign {
                    Signedness::Signed => "i",
                    Signedness::Unsigned => "u",
                },
            ),
            Self::Float { width } => format!("f{}", width),
            Self::Bool => "bool".to_string(),
            Self::String => "str".to_string(),
            Self::Rune => "rune".to_string(),
            Self::Unit => "unit".to_string(),
            Self::Absurd => "absurd".to_string(),
            Self::Array(len, ty) => format!("arr[{}, {}]", len, ty.to_string(intern)),
            Self::Slice(ty) => format!("slice[{}]", ty.to_string(intern)),
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

impl Default for Type {
    #[inline]
    fn default() -> Self {
        Self::Unit
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Deserialize, Serialize)]
pub enum TypeOp {
    And,
    Or,
}

impl Display for TypeOp {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let op = match self {
            Self::And => '&',
            Self::Or => '|',
        };

        f.write_char(op)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Deserialize, Serialize)]
pub struct Binding {
    // TODO: Enum for mutability/referential status?
    pub reference: bool,
    pub mutable: bool,
    pub pattern: Pattern,
    pub ty: Option<Ref<Locatable<Type>>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Deserialize, Serialize)]
pub enum Pattern {
    Literal(Literal),
    Ident(StrT),
    ItemPath(ItemPath),
    Wildcard,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Deserialize, Serialize)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub loc: Location,
}

impl Block {
    #[inline]
    pub const fn location(&self) -> Location {
        self.loc
    }

    #[inline]
    pub fn span(&self) -> Span {
        self.loc.span()
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.stmts.len()
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.stmts.is_empty()
    }

    #[inline]
    pub fn iter<'a>(&'a self) -> impl Iterator<Item = &'a Stmt> + 'a {
        self.stmts.iter()
    }

    #[inline]
    pub fn iter_mut<'a>(&'a mut self) -> impl Iterator<Item = &'a mut Stmt> + 'a {
        self.stmts.iter_mut()
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Deserialize, Serialize)]
pub enum CompOp {
    Equal,
    NotEqual,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
}

impl Display for CompOp {
    #[inline]
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

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Deserialize, Serialize)]
pub enum AssignKind {
    Normal,
    BinaryOp(BinaryOp),
}

impl Display for AssignKind {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Normal => f.write_str(":="),
            Self::BinaryOp(op) => write!(f, "{}=", op),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Deserialize, Serialize)]
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
    #[inline]
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

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Deserialize, Serialize)]
pub enum UnaryOp {
    Positive,
    Negative,
    Not,
}

impl Display for UnaryOp {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let pretty = match self {
            Self::Positive => '+',
            Self::Negative => '-',
            Self::Not => '!',
        };

        f.write_char(pretty)
    }
}
