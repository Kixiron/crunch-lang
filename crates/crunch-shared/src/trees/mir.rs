use crate as crunch_shared;
use crate::{
    error::Location,
    trees::{
        ast::Integer,
        hir::{
            BinaryOp, Binding, Block as HirBlock, CompOp, Expr, ExternFunc as HirExternFunc,
            FuncArg, FuncCall, Function as HirFunction, Item, Literal, Match, MatchArm, Pattern,
            Return, Stmt, TypeKind as HirType, TypeKind, Var, VarDecl,
        },
        CallConv, ItemPath, Ref, Signedness,
    },
    utils::{Either, HashMap},
    visitors::hir::{ExprVisitor, ItemVisitor, StmtVisitor},
};
use alloc::{boxed::Box, vec::Vec};
use core::{iter::FromIterator, mem};
use crunch_proc::instrument;

type Result<T> = core::result::Result<T, ()>;

#[derive(Debug)]
pub struct Mir {
    functions: Vec<Either<Function, ExternFunc>>,
}

impl Mir {
    pub fn iter(&self) -> impl Iterator<Item = &Either<Function, ExternFunc>> {
        self.functions.iter()
    }

    pub fn len(&self) -> usize {
        self.functions.len()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub id: FuncId,
    pub name: Option<ItemPath>,
    pub args: Vec<(VarId, Type)>,
    pub ret: Type,
    pub blocks: Vec<Block>,
}

impl Function {
    pub fn iter(&self) -> impl Iterator<Item = &Block> {
        self.blocks.iter()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExternFunc {
    pub id: FuncId,
    pub name: Option<ItemPath>,
    pub args: Vec<(VarId, Type)>,
    pub ret: Type,
    pub callconv: CallConv,
}

// TODO: Make branching part of the basic block
#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub id: BlockId,
    pub instructions: Vec<Instruction>,
}

impl Block {
    pub const fn new(id: BlockId) -> Self {
        Self {
            id,
            instructions: Vec::new(),
        }
    }

    pub fn push(&mut self, inst: Instruction) {
        self.instructions.push(inst);
    }

    pub fn is_empty(&self) -> bool {
        self.instructions.is_empty()
    }

    pub fn iter(&self) -> impl Iterator<Item = &Instruction> {
        self.instructions.iter()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    Return(Option<RightValue>),
    // TODO: Rename `Jump`
    Goto(BlockId),
    Assign(VarId, RightValue),
    Switch(RightValue, Vec<SwitchArm>),
    // TODO: Add `Branch` for conditional branches
}

#[derive(Debug, Clone, PartialEq)]
pub struct SwitchArm {
    pub kind: SwitchArmKind,
    pub block: BlockId,
}

#[derive(Debug, Clone, PartialEq)]
pub enum SwitchArmKind {
    Default(Option<RightValue>),
    Rval(RightValue),
}

#[derive(Debug, Clone, PartialEq)]
pub struct RightValue {
    pub ty: Type,
    pub val: Rval,
}

impl RightValue {
    // TODO: Change this when there's floats
    pub fn is_float(&self) -> bool {
        false
    }

    // TODO: Add more ints
    pub fn is_signed(&self) -> bool {
        matches!(self.ty, Type::I64)
    }

    // TODO: Add more ints
    pub fn is_unsigned(&self) -> bool {
        matches!(self.ty, Type::U8)
    }

    pub fn is_bool(&self) -> bool {
        matches!(self.ty, Type::Bool)
    }

    pub fn into_constant(self) -> Option<Constant> {
        if let Rval::Const(constant) = self.val {
            Some(constant)
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Rval {
    Var(VarId),
    Const(Constant),
    Phi(BlockId, BlockId),
    Call(FuncId, Vec<RightValue>),
    Add(Box<RightValue>, Box<RightValue>),
    Sub(Box<RightValue>, Box<RightValue>),
    Mul(Box<RightValue>, Box<RightValue>),
    Div(Box<RightValue>, Box<RightValue>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Constant {
    I64(i64),
    U8(u8),
    I8(i8),
    Bool(bool),
    String(Vec<u8>),
    Array(Vec<Constant>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    U8,
    I8,
    U16,
    I16,
    U32,
    I32,
    U64,
    I64,
    Bool,
    Unit,
    Pointer(Ref<Self>),
    Array(Ref<Self>, u32),
    String,
    Absurd,
}

impl Type {
    pub fn array_elements(&self) -> Option<&Self> {
        if let Self::Array(elem, ..) = self {
            Some(&**elem)
        } else {
            None
        }
    }
}

impl From<&HirType> for Type {
    fn from(ty: &HirType) -> Self {
        match ty {
            #[rustfmt::skip]
            HirType::Integer { sign, width } => match (sign, width) {
                (Signedness::Unsigned, 8)  => Self::U8,
                (Signedness::Signed,   8)  => Self::I8,
                (Signedness::Unsigned, 16) => Self::U16,
                (Signedness::Signed,   16) => Self::I16,
                (Signedness::Unsigned, 32) => Self::U32,
                (Signedness::Signed,   32) => Self::I32,
                (Signedness::Unsigned, 64) => Self::U64,
                (Signedness::Signed,   64) => Self::I64,

                (sign, width) => todo!("{}{}s are currently unsupported", sign, width),
            },
            HirType::Bool => Self::Bool,
            HirType::Unit => Self::Unit,
            HirType::Pointer(ty) => Self::Pointer(Ref::new(Self::from(&**ty))),
            HirType::String => Self::String,
            HirType::Absurd => Self::Absurd,
            HirType::Array(elem, len) => Self::Array(Ref::new(Self::from(&**elem)), *len),

            HirType::Infer => unreachable!("All types should have been inferred by now"),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct FuncId(pub usize);

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct BlockId(pub usize);

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct VarId(pub u64);

#[derive(Debug)]
pub struct MirBuilder {
    functions: Vec<Either<Function, ExternFunc>>,
    blocks: Vec<Block>,
    current_block: usize,
    var_counter: u64,
    variables: HashMap<Var, (VarId, Type)>,
    function_names: HashMap<ItemPath, (FuncId, Type)>,
}

impl MirBuilder {
    pub fn new() -> Self {
        Self {
            functions: Vec::new(),
            blocks: Vec::new(),
            current_block: 0,
            var_counter: 0,
            variables: HashMap::new(),
            function_names: HashMap::new(),
        }
    }

    #[instrument(name = "lowering to mir")]
    pub fn lower(mut self, items: &[Item]) -> Result<Mir> {
        self.function_names =
            HashMap::from_iter(items.iter().enumerate().filter_map(|(i, item)| {
                if let Item::Function(HirFunction { name, ret, .. }) = item {
                    Some((name.clone(), (FuncId(i), Type::from(&ret.kind))))
                } else if let Item::ExternFunc(HirExternFunc { name, ret, .. }) = item {
                    Some((name.clone(), (FuncId(i), Type::from(&ret.kind))))
                } else {
                    None
                }
            }));

        for item in items {
            self.visit_item(item)?;
        }

        Ok(Mir {
            functions: self.functions,
        })
    }

    fn next_var(&mut self) -> VarId {
        let id = VarId(self.var_counter);
        self.var_counter += 1;
        id
    }

    fn next_block(&mut self) -> BlockId {
        let id = BlockId(self.blocks.len());
        self.blocks.push(Block::new(id));
        self.current_block = id.0;

        id
    }

    fn move_to_block(&mut self, block: BlockId) {
        self.current_block = block.0;
    }

    fn current_block_mut(&mut self) -> &mut Block {
        &mut self.blocks[self.current_block]
    }
}

impl ItemVisitor for MirBuilder {
    type Output = Result<()>;

    fn visit_func(&mut self, func: &HirFunction) -> Self::Output {
        self.blocks.clear();
        self.blocks.push(Block::new(BlockId(0)));
        self.current_block = 0;

        self.var_counter = 0;
        self.variables.clear();

        let id = FuncId(self.functions.len() as usize);

        let mut args = Vec::with_capacity(func.args.len());
        for FuncArg { name, kind, .. } in func.args.iter() {
            let id = self.next_var();
            let ty: Type = kind.into();

            self.variables.insert(*name, (id, ty.clone()));
            args.push((id, ty));
        }

        for stmt in func.body.iter() {
            self.visit_stmt(stmt)?;
        }

        let func = Function {
            id,
            name: Some(func.name.clone()),
            args,
            ret: (&func.ret.kind).into(),
            blocks: mem::take(&mut self.blocks),
        };
        self.functions.push(Either::Left(func));

        Ok(())
    }

    fn visit_extern_func(&mut self, func: &HirExternFunc) -> Self::Output {
        let id = FuncId(self.functions.len() as usize);

        let mut args = Vec::with_capacity(func.args.len());
        for FuncArg { name, kind, .. } in func.args.iter() {
            let id = self.next_var();
            let ty: Type = kind.into();

            self.variables.insert(*name, (id, ty.clone()));
            args.push((id, ty));
        }

        let func = ExternFunc {
            id,
            name: Some(func.name.clone()),
            args,
            ret: (&func.ret.kind).into(),
            callconv: func.callconv,
        };
        self.functions.push(Either::Right(func));

        Ok(())
    }
}

impl StmtVisitor for MirBuilder {
    type Output = Result<()>;

    fn visit_stmt(&mut self, stmt: &Stmt) -> <Self as StmtVisitor>::Output {
        match stmt {
            Stmt::Item(item) => self.visit_item(item)?,
            Stmt::Expr(expr) => {
                if let Some(val) = self.visit_expr(expr)? {
                    let id = self.next_var();
                    self.current_block_mut().push(Instruction::Assign(id, val));
                }
            }
            Stmt::VarDecl(var) => self.visit_var_decl(var)?,
        }

        Ok(())
    }

    fn visit_var_decl(&mut self, var: &VarDecl) -> <Self as StmtVisitor>::Output {
        let rval = self.visit_expr(&*var.value)?.unwrap();

        let id = self.next_var();
        self.variables.insert(var.name, (id, rval.ty.clone()));
        self.current_block_mut().push(Instruction::Assign(id, rval));

        Ok(())
    }
}

impl ExprVisitor for MirBuilder {
    type Output = Result<Option<RightValue>>;

    fn visit_return(&mut self, _loc: Location, ret: &Return) -> Self::Output {
        let value = ret
            .val
            .as_ref()
            .map_or(Ok(None), |val| self.visit_expr(&**val))?;
        self.current_block_mut().push(Instruction::Return(value));

        Ok(None)
    }

    fn visit_break(&mut self, _loc: Location, _value: &super::hir::Break) -> Self::Output {
        todo!()
    }

    fn visit_continue(&mut self, _loc: Location) -> Self::Output {
        todo!()
    }

    fn visit_loop(
        &mut self,
        _loc: Location,
        _body: &super::hir::Block<super::hir::Stmt>,
    ) -> Self::Output {
        todo!()
    }

    fn visit_match(&mut self, loc: Location, Match { cond, arms, .. }: &Match) -> Self::Output {
        let current_block = BlockId(self.current_block);

        let mut cases = Vec::with_capacity(arms.len());
        for MatchArm {
            bind: Binding { pattern, .. },
            body,
            ..
        } in arms
        {
            self.next_block();
            let block_id = BlockId(self.current_block);

            for stmt in body.iter() {
                self.visit_stmt(stmt)?;
            }

            self.move_to_block(current_block);

            let kind = match pattern {
                Pattern::Literal(lit) => {
                    SwitchArmKind::Rval(self.visit_literal(loc, lit)?.unwrap())
                }

                // TODO: Get the type somehow
                Pattern::Ident(ident) => SwitchArmKind::Default(Some(
                    self.visit_variable(loc, Var::User(*ident), &TypeKind::Infer)?
                        .unwrap(),
                )),

                Pattern::ItemPath(..) => todo!(),
                Pattern::Wildcard => SwitchArmKind::Default(None),
            };
            cases.push(SwitchArm {
                kind,
                block: block_id,
            });
        }

        self.move_to_block(current_block);

        let switch = Instruction::Switch(self.visit_expr(cond)?.unwrap(), cases);
        self.current_block_mut().push(switch);

        self.next_block();

        Ok(None)
    }

    fn visit_variable(&mut self, _loc: Location, var: Var, _ty: &TypeKind) -> Self::Output {
        Ok(self.variables.get(&var).map(|(var, ty)| RightValue {
            ty: ty.clone(),
            val: Rval::Var(*var),
        }))
    }

    // FIXME: Give literals their type in hir
    fn visit_literal(&mut self, loc: Location, literal: &Literal) -> Self::Output {
        match literal {
            Literal::Integer(Integer { sign, bits }) => {
                // FIXME: Doesn't respect types
                let val = Rval::Const(Constant::I64(sign.maybe_negate(*bits) as i64));
                let rval = RightValue { ty: Type::I64, val };

                Ok(Some(rval))
            }

            Literal::Bool(b) => Ok(Some(RightValue {
                ty: Type::Bool,
                val: Rval::Const(Constant::Bool(*b)),
            })),

            Literal::String(string) => Ok(Some(RightValue {
                ty: Type::String,
                val: Rval::Const(Constant::String(string.to_bytes())),
            })),

            Literal::Array(array) => Ok(Some(RightValue {
                // FIXME: Doesn't respect types
                ty: Type::Array(Ref::new(Type::U8), array.len() as u32),
                val: Rval::Const(Constant::Array(
                    array
                        .into_iter()
                        .map(|e| {
                            self.visit_literal(loc, e)
                                .map(|e| e.unwrap().into_constant().unwrap())
                        })
                        .collect::<Result<Vec<_>>>()?,
                )),
            })),

            lit => todo!("{:?}", lit),
        }
    }

    fn visit_scope(&mut self, _loc: Location, _body: &HirBlock<Stmt>) -> Self::Output {
        todo!()
    }

    fn visit_func_call(&mut self, _loc: Location, call: &FuncCall) -> Self::Output {
        let (func, ty) = self.function_names.get(&call.func).unwrap().clone();
        let val = Rval::Call(
            func,
            call.args
                .iter()
                .map(|a| Ok(self.visit_expr(a)?.unwrap()))
                .collect::<Result<Vec<RightValue>>>()?,
        );

        Ok(Some(RightValue {
            // TODO: Get the actual return type
            ty,
            val,
        }))
    }

    fn visit_comparison(
        &mut self,
        _loc: Location,
        _lhs: &Expr,
        _op: CompOp,
        _rhs: &Expr,
    ) -> Self::Output {
        todo!()
    }

    fn visit_assign(&mut self, _loc: Location, var: Var, value: &Expr) -> Self::Output {
        let rval = self.visit_expr(value)?.unwrap();
        let (_old_id, old_ty) = self.variables.get(&var).unwrap().clone();
        let new_id = self.next_var();
        assert_eq!(rval.ty, old_ty);

        self.variables.insert(var, (new_id, rval.ty.clone()));
        self.current_block_mut()
            .push(Instruction::Assign(new_id, rval));

        Ok(None)
    }

    fn visit_binop(
        &mut self,
        _loc: Location,
        lhs: &Expr,
        op: BinaryOp,
        rhs: &Expr,
    ) -> Self::Output {
        let (lhs, rhs) = (
            self.visit_expr(lhs)?.unwrap(),
            self.visit_expr(rhs)?.unwrap(),
        );
        let (lhs, rhs) = (Box::new(lhs), Box::new(rhs));
        assert_eq!(lhs.ty, rhs.ty);

        let ty = lhs.ty.clone();
        let val = match op {
            BinaryOp::Add => Rval::Add(lhs, rhs),
            BinaryOp::Sub => Rval::Sub(lhs, rhs),
            BinaryOp::Mult => Rval::Mul(lhs, rhs),
            BinaryOp::Div => Rval::Div(lhs, rhs),

            _ => todo!(),
        };

        Ok(Some(RightValue { ty, val }))
    }

    fn visit_cast(&mut self, _loc: Location, _cast: &super::hir::Cast) -> Self::Output {
        todo!()
    }
}
