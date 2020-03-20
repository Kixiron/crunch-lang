use super::*;

#[derive(Debug, Clone)]
pub enum Ast {
    Function {
        decorators: Vec<Decorator>,
        attributes: Vec<Attribute>,
        name: Sym,
        generics: Vec<Sym>,
        args: Vec<(Sym, Type)>,
        returns: Type,
        body: Vec<Statement>,
    },

    Type {
        decorators: Vec<Decorator>,
        attributes: Vec<Attribute>,
        name: Sym,
        generics: Vec<Sym>,
        members: Vec<TypeMember>,
        methods: Vec<Ast>,
    },

    Enum {
        decorators: Vec<Decorator>,
        attributes: Vec<Attribute>,
        name: Sym,
        generics: Vec<Sym>,
        variants: Vec<EnumVariant>,
    },

    Trait {
        decorators: Vec<Decorator>,
        attributes: Vec<Attribute>,
        name: Sym,
        generics: Vec<Sym>,
        methods: Vec<Ast>,
    },

    Import {
        file: Sym,
        dest: ImportDest,
        exposes: ImportExposure,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Builtin(BuiltinType),
    Custom(Sym),
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BuiltinType {
    Integer,
    Float,
    Boolean,
    String,
    Vec(Box<Type>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ImportExposure {
    None(Sym),
    All,
    Members(Vec<Sym>),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ImportDest {
    NativeLib,
    Package,
    Relative,
}

#[derive(Debug, Clone)]
pub enum EnumVariant {
    Unit(Sym),
    Tuple { name: Sym, elements: Vec<Sym> },
}

#[derive(Debug, Clone)]
pub struct TypeMember {
    decorators: Vec<Decorator>,
    attributes: Vec<Attribute>,
    name: Sym,
    ty: Option<Sym>,
}

#[derive(Debug, Clone)]
pub struct Decorator {
    name: Sym,
    args: Vec<Expression>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Attribute {
    Visibility(Visibility),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Visibility {
    FileOnly,
    LibraryOnly,
    All,
}
