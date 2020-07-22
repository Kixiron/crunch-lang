use crate::{
    error::{Locatable, Location},
    strings::StrT,
    trees::{
        ast::{Block, Dest, Exposure, FuncArg, Item, Type as AstType, TypeMember, Variant},
        CallConv, ItemPath,
    },
    utils::HashMap,
    visitors::ast::ItemVisitor,
};
use alloc::{vec, vec::Vec};

type ModuleId = usize;
type TypeId = usize;
type FunctionId = usize;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Resolver {
    modules: Vec<Module>,
    types: Vec<Type>,
    functions: Vec<Function>,
    current_path: ItemPath,
    current_module: ModuleId,
}

impl Resolver {
    pub fn new(current_path: ItemPath) -> Self {
        Self {
            modules: vec![Module::new(*current_path.last().unwrap())],
            types: vec![
                Type::Bool,
                Type::String,
                Type::Rune,
                Type::Unit,
                Type::Absurd,
                Type::Infer,
                Type::Integer,
                Type::Pointer,
            ],
            functions: Vec::new(),
            current_path,
            current_module: 0,
        }
    }

    // FIXME: This is just bad
    pub fn finalize(&mut self) {
        // FIXME: This just won't work
        let hacky = self.clone(); // Your mother can't be borrowed immutably because she's already borrowed mutably

        for ty in self.types.iter_mut() {
            if let Type::Custom { members, .. } = ty {
                for member in members.values_mut() {
                    if let Either::Right((name, module)) = *member {
                        let ty = self.modules[module].lookup_type(&hacky, name);

                        *member = Either::Left(ty.unwrap());
                    }
                }
            }
        }

        for func in self.functions.iter_mut() {
            for (_, arg_ty) in func.args.iter_mut() {
                if let Either::Right((name, module)) = *arg_ty {
                    let ty = self.modules[module].lookup_type(&hacky, name);

                    *arg_ty = Either::Left(ty.unwrap());
                }
            }

            if let Either::Right((name, module)) = func.ret {
                let ty = self.modules[module].lookup_type(&hacky, name);

                func.ret = Either::Left(ty.unwrap());
            }
        }
    }

    pub fn current(&self) -> &Module {
        &self.modules[self.current_module]
    }

    pub fn current_mut(&mut self) -> &mut Module {
        &mut self.modules[self.current_module]
    }

    pub fn push_func(&mut self, func: Function) -> FunctionId {
        let id = self.functions.len();
        self.functions.push(func);

        id
    }

    pub fn push_ty(&mut self, ty: Type) -> TypeId {
        let id = self.types.len();
        self.types.push(ty);

        id
    }

    pub fn ty(&mut self, ty: &AstType) -> Either<TypeId, (StrT, ModuleId)> {
        match ty {
            AstType::Bool => Either::Left(0),
            AstType::String => Either::Left(1),
            AstType::Rune => Either::Left(2),
            AstType::Unit => Either::Left(3),
            AstType::Absurd => Either::Left(4),
            AstType::Unknown => Either::Left(5),
            AstType::Integer {
                signed: Some(true),
                width: Some(32),
            } => Either::Left(6),
            AstType::Integer {
                signed: Some(true),
                width: Some(64),
            } => Either::Left(6),
            AstType::Pointer { .. } => Either::Left(7),
            AstType::Array { .. } => Either::Left(7),
            AstType::Slice { .. } => Either::Left(7),
            AstType::Reference { .. } => Either::Left(7),
            AstType::ItemPath(path) => self
                .current()
                .lookup_type(self, *path.last().unwrap())
                .map_or_else(
                    || Either::Right((*path.last().unwrap(), self.current_module)),
                    |id| Either::Left(id),
                ),
            t => todo!("{:?}", t),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Module {
    name: StrT,
    parent: Option<ModuleId>,
    imports: Vec<ModuleId>,
    exports: Vec<Export>,
    functions: Vec<FunctionId>,
    types: Vec<TypeId>,
    modules: Vec<ModuleId>,
}

impl Module {
    pub fn new(name: StrT) -> Self {
        Self {
            name,
            parent: None,
            imports: Vec::new(),
            exports: Vec::new(),
            functions: Vec::new(),
            types: Vec::new(),
            modules: Vec::new(),
        }
    }
}

impl ItemVisitor for Resolver {
    type Output = ();

    fn visit_func(
        &mut self,
        item: &Item,
        _generics: Option<Locatable<&[Locatable<AstType>]>>,
        func_args: Locatable<&[FuncArg]>,
        _body: &Block,
        ret: Locatable<&AstType>,
        _loc: Location,
    ) -> Self::Output {
        let mut args = HashMap::with_capacity(func_args.len());
        for FuncArg { name, ty, .. } in *func_args {
            args.insert(*name, self.ty(ty.as_ref()));
        }

        let func = Function {
            name: item.name.unwrap(),
            args,
            ret: self.ty(*ret),
            parent: self.current_module,
        };

        let func = self.push_func(func);
        self.current_mut().functions.push(func);
    }

    fn visit_type_decl(
        &mut self,
        item: &Item,
        _generics: Option<Locatable<&[Locatable<AstType>]>>,
        ty_members: &[TypeMember],
    ) -> Self::Output {
        let mut members = HashMap::with_capacity(ty_members.len());
        for TypeMember { name, ty, .. } in ty_members {
            members.insert(*name, self.ty(&**ty));
        }

        let ty = Type::Custom {
            name: item.name.unwrap(),
            members,
            methods: HashMap::new(),
            parent: self.current_module,
        };

        let ty = self.push_ty(ty);
        self.current_mut().types.push(ty);
    }

    fn visit_enum(
        &mut self,
        _item: &Item,
        _generics: Option<Locatable<&[Locatable<AstType>]>>,
        _variants: &[Variant],
    ) -> Self::Output {
        todo!()
    }

    fn visit_trait(
        &mut self,
        _item: &Item,
        _generics: Option<Locatable<&[Locatable<AstType>]>>,
        _methods: &[Item],
    ) -> Self::Output {
        todo!()
    }

    fn visit_import(
        &mut self,
        _item: &Item,
        _file: &ItemPath,
        _dest: &Dest,
        _exposes: &Exposure,
    ) -> Self::Output {
        todo!()
    }

    fn visit_extend_block(
        &mut self,
        _item: &Item,
        _target: Locatable<&AstType>,
        _extender: Option<Locatable<&AstType>>,
        _items: &[Item],
    ) -> Self::Output {
        todo!()
    }

    fn visit_alias(
        &mut self,
        _item: &Item,
        _alias: Locatable<&AstType>,
        _actual: Locatable<&AstType>,
    ) -> Self::Output {
        todo!()
    }

    fn visit_extern_block(&mut self, _item: &Item, items: &[Item]) -> Self::Output {
        for item in items {
            self.visit_item(item);
        }
    }

    fn visit_extern_func(
        &mut self,
        item: &Item,
        _generics: Option<Locatable<&[Locatable<AstType>]>>,
        func_args: Locatable<&[FuncArg]>,
        ret: Locatable<&AstType>,
        _callconv: CallConv,
    ) -> Self::Output {
        let mut args = HashMap::with_capacity(func_args.len());
        for FuncArg { name, ty, .. } in *func_args {
            args.insert(*name, self.ty(ty.as_ref()));
        }

        let func = Function {
            name: item.name.unwrap(),
            args,
            ret: self.ty(*ret),
            parent: self.current_module,
        };

        let func = self.push_func(func);
        self.current_mut().functions.push(func);
    }
}

impl Module {
    pub fn lookup_type(&self, resolver: &Resolver, ty_name: StrT) -> Option<TypeId> {
        self.types
            .iter()
            .find_map(|&id| {
                if let Type::Custom { name, .. } = resolver.types[id] {
                    if name == ty_name {
                        return Some(id);
                    }
                }

                None
            })
            .or_else(|| {
                self.imports
                    .iter()
                    .find_map(|&id| resolver.modules[id].lookup_exported_type(resolver, ty_name))
            })
    }

    pub fn lookup_exported_type(&self, resolver: &Resolver, ty_name: StrT) -> Option<TypeId> {
        self.exports.iter().find_map(|&exp| {
            if let Export::Type(id) = exp {
                if let Type::Custom { name, .. } = resolver.types[id] {
                    if name == ty_name {
                        return Some(id);
                    }
                }
            }

            None
        })
    }

    pub fn lookup_function(&self, resolver: &Resolver, name: StrT) -> Option<FunctionId> {
        self.functions
            .iter()
            .find_map(|&id| {
                if resolver.functions[id].name == name {
                    Some(id)
                } else {
                    None
                }
            })
            .or_else(|| {
                self.imports
                    .iter()
                    .find_map(|&id| resolver.modules[id].lookup_exported_function(resolver, name))
            })
    }

    pub fn lookup_exported_function(&self, resolver: &Resolver, name: StrT) -> Option<FunctionId> {
        self.exports.iter().find_map(|&exp| {
            if let Export::Function(id) = exp {
                if resolver.functions[id].name == name {
                    return Some(id);
                }
            }

            None
        })
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Export {
    Function(FunctionId),
    Type(TypeId),
    Module(ModuleId),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Function {
    name: StrT,
    args: HashMap<StrT, Either<TypeId, (StrT, ModuleId)>>,
    ret: Either<TypeId, (StrT, ModuleId)>,
    parent: ModuleId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Bool,
    String,
    Rune,
    Unit,
    Absurd,
    Infer,
    Integer,
    Pointer,
    Custom {
        name: StrT,
        members: HashMap<StrT, Either<TypeId, (StrT, ModuleId)>>,
        methods: HashMap<StrT, FunctionId>,
        parent: ModuleId,
    },
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Either<L, R> {
    Left(L),
    Right(R),
}
