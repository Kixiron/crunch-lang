use crate::{
    error::Location,
    trees::{
        ast::ItemPath,
        hir::{
            Block as HirBlock, Expr, FuncArg, FuncCall, Function as HirFunction, Item, Literal,
            Return, Stmt, TypeKind as HirType, TypeKind, Var, VarDecl,
        },
    },
    utils::HashMap,
    visitors::hir::{ExprVisitor, ItemVisitor, StmtVisitor},
};
use alloc::{boxed::Box, vec::Vec};
use core::mem;

#[derive(Debug)]
pub struct Mir {
    functions: Vec<Function>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub id: FuncId,
    pub args: Vec<(VarId, Type)>,
    pub ret: Type,
    pub blocks: Vec<Block>,
}

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
}

#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    Return(Option<Rval>),
    Goto(BlockId),
    Assign(VarId, Type, Rval),
    Switch(Rval, Vec<(Rval, BlockId)>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Rval {
    Var(VarId),
    Const(Constant),
    Phi(BlockId, BlockId),
    Call(FuncId, Vec<Rval>),
    Intrinsic(Intrinsic, Vec<Rval>),
    Add(Box<Rval>, Box<Rval>),
    Sub(Box<Rval>, Box<Rval>),
    Mul(Box<Rval>, Box<Rval>),
    Div(Box<Rval>, Box<Rval>),
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Intrinsic {
    Printf,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Constant {
    I64(i64),
    U8(u8),
    Bool(bool),
    Unit,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Type {
    I64,
    U8,
    Bool,
    Unit,
}

impl From<&HirType> for Type {
    fn from(ty: &HirType) -> Self {
        match ty {
            HirType::Integer => Self::I64,
            HirType::Bool => Self::Bool,
            HirType::Unit => Self::Unit,
            HirType::String => todo!("Slice or something"),
            HirType::Infer => Type::Unit, // FIXME
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
    functions: Vec<Function>,
    blocks: Vec<Block>,
    current_block: usize,
    var_counter: u64,
    variables: HashMap<Var, (VarId, Type)>,
    function_names: HashMap<ItemPath, FuncId>,
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

    pub fn lower(mut self, items: &[Item]) -> Vec<Function> {
        for item in items {
            self.visit_item(item);
        }

        self.functions
    }

    fn next_var(&mut self) -> VarId {
        let id = VarId(self.var_counter);
        self.var_counter += 1;
        id
    }

    fn current_block_mut(&mut self) -> &mut Block {
        &mut self.blocks[self.current_block]
    }
}

impl ItemVisitor for MirBuilder {
    type Output = ();

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
            self.visit_stmt(stmt);
        }

        let func = Function {
            id,
            args,
            ret: (&func.ret).into(),
            blocks: mem::take(&mut self.blocks),
        };

        self.functions.push(func);
    }
}

impl StmtVisitor for MirBuilder {
    type Output = ();

    fn visit_stmt(&mut self, stmt: &Stmt) -> <Self as StmtVisitor>::Output {
        match stmt {
            Stmt::Item(item) => self.visit_item(item).into(),
            Stmt::Expr(expr) => {
                if let Some((ty, val)) = self.visit_expr(expr) {
                    let id = self.next_var();
                    self.current_block_mut()
                        .push(Instruction::Assign(id, ty, val));
                }
            }
            Stmt::VarDecl(var) => self.visit_var_decl(var),
        }
    }

    fn visit_var_decl(&mut self, var: &VarDecl) -> <Self as StmtVisitor>::Output {
        let (ty, rval) = self.visit_expr(&*var.value).unwrap();

        let id = self.next_var();
        self.variables.insert(var.name, (id, ty.clone()));
        self.current_block_mut()
            .push(Instruction::Assign(id, ty, rval));
    }
}

impl ExprVisitor for MirBuilder {
    type Output = Option<(Type, Rval)>;

    fn visit_return(&mut self, _loc: Location, ret: &Return) -> Self::Output {
        let value = ret
            .val
            .as_ref()
            .map(|val| self.visit_expr(&**val).unwrap().1);
        self.current_block_mut().push(Instruction::Return(value));

        None
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

    fn visit_match(&mut self, _loc: Location, _match_: &super::hir::Match) -> Self::Output {
        todo!()
    }

    fn visit_variable(&mut self, _loc: Location, var: Var, _ty: &TypeKind) -> Self::Output {
        self.variables
            .get(&var)
            .map(|(var, ty)| (*ty, Rval::Var(*var)))
    }

    fn visit_literal(&mut self, _loc: Location, literal: &Literal) -> Self::Output {
        match literal {
            Literal::Integer(int) => Some((
                Type::Bool,
                Rval::Const(Constant::I64(if int.sign.is_negative() {
                    -(int.bits as i64)
                } else {
                    int.bits as i64
                })),
            )),
            Literal::Bool(b) => Some((Type::Bool, Rval::Const(Constant::Bool(*b)))),
            _ => todo!(),
        }
    }

    fn visit_scope(&mut self, _loc: Location, _body: &HirBlock<Stmt>) -> Self::Output {
        todo!()
    }

    fn visit_func_call(&mut self, _loc: Location, call: &FuncCall) -> Self::Output {
        Some((
            Type::Unit,
            if let Some(func) = self.function_names.get(&call.func) {
                Rval::Call(
                    *func,
                    call.args
                        .iter()
                        .map(|a| self.visit_expr(a).unwrap().1)
                        .collect(),
                )
            } else {
                Rval::Intrinsic(
                    Intrinsic::Printf,
                    call.args
                        .iter()
                        .map(|a| self.visit_expr(a).unwrap().1)
                        .collect(),
                )
            },
        ))
    }

    fn visit_comparison(
        &mut self,
        _loc: Location,
        _lhs: &Expr,
        _op: super::hir::CompOp,
        _rhs: &Expr,
    ) -> Self::Output {
        todo!()
    }

    fn visit_assign(
        &mut self,
        _loc: Location,
        _var: super::hir::Var,
        _value: &Expr,
    ) -> Self::Output {
        todo!()
    }
}
