use crate as crunch_shared;
use crate::{
    error,
    error::{Location, MirError, MirResult},
    strings::StrT,
    trees::{
        ast::Integer,
        hir::{
            BinaryOp, Binding, Block as HirBlock, Cast, CompOp, Expr, ExternFunc as HirExternFunc,
            FuncArg, FuncCall, Function as HirFunction, Item, Literal as HirLiteral,
            LiteralVal as HirLiteralVal, Match, MatchArm, Pattern, Reference, Return, Stmt,
            TypeKind as HirType, TypeKind, Var as HirVar, VarDecl,
        },
        CallConv, ItemPath, Ref, Sign, Signedness,
    },
    utils::HashMap,
    visitors::hir::{ExprVisitor, ItemVisitor, StmtVisitor},
};
use alloc::{string::ToString, vec, vec::Vec};
use core::iter::FromIterator;
use crunch_proc::instrument;
use derive_more::Display;

#[derive(Debug)]
pub struct Mir {
    functions: Vec<Function>,
    external_functions: Vec<ExternFunc>,
}

impl Mir {
    /// An iterator over all functions
    pub fn functions(&self) -> impl Iterator<Item = &Function> {
        self.functions.iter()
    }

    /// An iterator over all external functions
    pub fn external_functions(&self) -> impl Iterator<Item = &ExternFunc> {
        self.external_functions.iter()
    }
}

/// A function
#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    /// The id of the current function
    pub id: FuncId,
    /// The name of the function
    pub name: Option<ItemPath>,
    /// The arguments passed to the function
    pub args: Vec<Variable>,
    /// The return type of the function
    pub ret: Type,
    /// The body of the function
    pub blocks: Vec<BasicBlock>,
}

impl Function {
    /// An iterator over the `BasicBlock`s of a function
    pub fn iter(&self) -> impl Iterator<Item = &BasicBlock> {
        self.blocks.iter()
    }
}

/// A typed variable
#[derive(Debug, Clone, PartialEq)]
pub struct Variable {
    /// The id of the variable
    pub id: VarId,
    /// The type of the variable
    pub ty: Type,
}

/// An externally defined function
#[derive(Debug, Clone, PartialEq)]
pub struct ExternFunc {
    /// The id of the current function
    pub id: FuncId,
    /// The name of the function, used during codegen and linking
    pub name: ItemPath,
    /// The arguments passed to the function
    pub args: Vec<Variable>,
    /// The return type of the function
    pub ret: Type,
    /// The function's calling convention
    pub callconv: CallConv,
}

/// A basic block
///
/// Each basic block is a completely self-contained scope with its own variable scope
/// starting at zero and outside communication only possible by passing arguments
// TODO: Sealing
#[derive(Debug, Clone, PartialEq)]
pub struct BasicBlock {
    /// The id of the current block
    pub id: BlockId,
    /// The name of the current block, if there is one
    pub name: Option<StrT>,
    /// The arguments passed to the current block when called
    pub args: Vec<Variable>,
    /// The body of the block
    pub instructions: Vec<Instruction>,
    /// The block's terminator
    pub terminator: Option<Terminator>,
}

impl BasicBlock {
    pub fn new(id: BlockId, name: Option<StrT>) -> Self {
        Self {
            id,
            name,
            args: Vec::new(),
            instructions: Vec::new(),
            terminator: None,
        }
    }

    /// Returns `true` if the current block has no instructions
    pub fn is_empty(&self) -> bool {
        self.instructions.is_empty()
    }

    /// Returns an iterator over the block's instructions
    pub fn iter(&self) -> impl Iterator<Item = &Instruction> {
        self.instructions.iter()
    }

    pub fn set_name(&mut self, name: StrT) {
        self.name = Some(name)
    }

    /// Push an instruction to the current basic block
    pub fn push(&mut self, inst: Instruction) {
        self.instructions.push(inst);
    }

    pub fn push_argument(&mut self, arg: Variable) {
        self.args.push(arg);
    }

    /// Sets the block's terminator
    pub fn set_terminator(&mut self, terminator: Terminator) {
        if self.terminator.is_some() {
            // TODO: Should this be a fatal error?
            error!("Overwrote a block's terminator");
        }

        self.terminator = Some(terminator);
    }

    /// Verifies the current block
    pub fn verify(&self) -> MirResult<()> {
        let BasicBlock {
            id,
            args,
            terminator,
            ..
        } = &self;

        // Make sure that the block has a terminator
        if terminator.is_none() {
            // TODO: Resolve the block's name for the error
            return Err(MirError::MissingTerminator(id.0.to_string()));
        }

        // Make sure no block args are duplicated
        for arg in args.iter() {
            let count = args.iter().filter(|a| a.id == arg.id).count();
            if count > 1 {
                // TODO: Resolve the block's name for the error
                return Err(MirError::DuplicatedBBArg(id.0, arg.id.0));
            }
        }

        Ok(())
    }
}

/// A block terminator, every `BasicBlock` is closed by one
#[derive(Debug, Clone, PartialEq)]
pub enum Terminator {
    /// A return instruction, `None` for a unit return and `Some` for returning a value
    Return(Option<VarId>),
    /// An unconditional jump to the given block
    Jump(BlockId),
    /// A conditional branch
    Branch {
        /// The condition being branched on, should be a boolean
        condition: VarId,
        /// The block jumped to if the condition is true
        truthy: BlockId,
        /// The block jumped to if the condition is false
        falsy: BlockId,
    },
    /// A switch instruction
    Switch {
        /// The condition being switched off of
        condition: VarId,
        /// The cases of the switch
        cases: Vec<SwitchCase>,
        /// The default block to jump to if all cases fail
        default: DefaultSwitchCase,
    },
    /// An unreachable instruction
    Unreachable,
}

/// A switch case, contains a condition and a block
#[derive(Debug, Clone, PartialEq)]
pub struct SwitchCase {
    /// The condition being tested
    pub condition: VarId,
    /// The block jumped to if the condition is true
    pub block: BlockId,
    /// The arguments passed to the basic block
    pub args: Vec<VarId>,
}

/// The default case of a switch
#[derive(Debug, Clone, PartialEq)]
pub struct DefaultSwitchCase {
    /// The block jumped to
    pub block: BlockId,
    /// The arguments passed to the basic block
    pub args: Vec<VarId>,
}

/// An instruction
#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    /// An assignment of a value to a variable
    Assign(Assign),
    /// A standalone function call, should return void
    Call(FnCall),
}

impl Instruction {
    /// Returns `Some` if the current instruction is an assignment
    pub fn as_assign(&self) -> Option<&Assign> {
        if let Self::Assign(assign) = self {
            Some(assign)
        } else {
            None
        }
    }

    /// Fills a vector with the ids of all variables *used* within the current instruction
    ///
    /// Does not contain the variables that are assigned to within the current instruction
    pub fn variable_usages(&self, buf: &mut Vec<VarId>) {
        match self {
            Self::Assign(Assign { val, .. }) => val.val.variable_usages(buf),
            Self::Call(FnCall { args, .. }) => buf.extend(args.iter().copied()),
        }
    }
}

/// A variable assignment
#[derive(Debug, Clone, PartialEq)]
pub struct Assign {
    /// The id of the variable being assigned to
    pub var: VarId,
    /// The value being assigned to the variable
    pub val: Rval,
    /// The type of the variable
    pub ty: Type,
}

/// A function call
#[derive(Debug, Clone, PartialEq)]
pub struct FnCall {
    /// The function being called
    pub function: FuncId,
    /// The arguments being passed to the function
    pub args: Vec<VarId>,
}

/// The right-hand side of an expression
#[derive(Debug, Clone, PartialEq)]
pub struct Rval {
    pub val: Value,
    pub ty: Type,
}

impl Rval {
    /// Returns `true` if the current value is a float
    // TODO: Change this when there's floats
    pub fn is_float(&self) -> bool {
        false
    }

    /// Returns `true` if the current value is an signed integer of any type
    // TODO: Add more ints
    pub fn is_signed(&self) -> bool {
        matches!(self.ty, Type::I64)
    }

    /// Returns `true` if the current value is a unsigned integer of any type
    // TODO: Add more ints
    pub fn is_unsigned(&self) -> bool {
        matches!(self.ty, Type::U8)
    }

    /// Returns `true` if the current value is a boolean
    pub fn is_bool(&self) -> bool {
        matches!(self.ty, Type::Bool)
    }

    /// Returns `Some` if the current value is a constant
    pub fn into_constant(self) -> Option<Constant> {
        if let Value::Const(constant) = self.val {
            Some(constant)
        } else {
            None
        }
    }
}

/// The inner value of an `Rval`
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    /// A variable
    Variable(VarId),
    /// A constant value
    Const(Constant),
    /// A function call that returns a value
    Call(FnCall),
    /// The addition of two values
    Add(VarId, VarId),
    /// The subtraction of two values
    Sub(VarId, VarId),
    /// The multiplication of two values
    Mul(VarId, VarId),
    /// The division of two values
    Div(VarId, VarId),
    /// Fetches a pointer to a variable, returning a `Pointer` value
    GetPointer {
        /// The variable being pointed to
        var: VarId,
        /// Whether the pointer is mutable
        mutable: bool,
        /// Whether the pointer is aliasable
        aliasable: bool,
    },
    /// Casts a variable to a different type
    Cast(VarId, Type),
}

impl Value {
    /// Fills a vector with the ids of all variables used within the current value
    pub fn variable_usages(&self, buf: &mut Vec<VarId>) {
        match self {
            Self::Call(FnCall { args, .. }) => buf.extend(args.iter().copied()),
            Self::Add(lhs, rhs)
            | Self::Sub(lhs, rhs)
            | Self::Mul(lhs, rhs)
            | Self::Div(lhs, rhs) => {
                buf.push(*lhs);
                buf.push(*rhs);
            }
            Self::Variable(var) | Self::GetPointer { var, .. } | Self::Cast(var, _) => {
                buf.push(*var);
            }
            Self::Const(_) => {}
        }
    }
}

/// A compile time known constant value
#[derive(Debug, Clone, PartialEq)]
pub enum Constant {
    Integer {
        sign: Sign,
        bits: u128,
    },
    /// A boolean
    Bool(bool),
    /// A string
    String(Vec<u8>),
    /// An array of constants
    Array(Vec<Constant>),
}

macro_rules! is_type {
    ($($func:ident => $variant:pat),* $(,)?) => {
        $(
            pub fn $func(&self) -> bool {
                matches!(self, $variant)
            }
        )*
    };
}

/// A type
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
    Pointer(Ref<Type>),
    Array(Ref<Type>, u32),
    Slice(Ref<Type>),
    Reference(bool, Ref<Type>),
    String,
    Absurd,
}

impl Type {
    /// If the current type is an array, get the type of the elements it contains
    pub fn array_elements(&self) -> Option<&Self> {
        if let Self::Array(elem, ..) = self {
            Some(&**elem)
        } else {
            None
        }
    }

    pub fn is_unsigned(&self) -> bool {
        matches!(self, Self::U8 | Self::U16 | Self::U32 | Self::U64)
    }

    pub fn is_signed(&self) -> bool {
        matches!(self, Self::U8 | Self::U16 | Self::U32 | Self::U64)
    }

    pub fn is_integer(&self) -> bool {
        matches!(
            self,
            Self::U8
                | Self::I8
                | Self::U16
                | Self::I16
                | Self::U32
                | Self::I32
                | Self::U64
                | Self::I64
        )
    }

    is_type! {
        is_u8     => Self::U8,
        is_i8     => Self::I8,
        is_u64    => Self::U64,
        is_i64    => Self::I64,
        is_bool   => Self::Bool,
        is_array  => Self::Array(..),
        is_string => Self::String,
    }
}

// FIXME: Favor an actual function
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
            HirType::Slice(elem) => Self::Slice(Ref::new(Self::from(&**elem))),
            HirType::Reference(mutable, ty) => {
                Self::Reference(*mutable, Ref::new(Self::from(&**ty)))
            }

            HirType::Infer => unreachable!("All types should have been inferred by now"),
        }
    }
}

// FIXME: This all needs to be elsewhere, changes to hir -> mir lowering cause a global recompile

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Display)]
#[repr(transparent)]
pub struct FuncId(u64);

impl FuncId {
    pub const fn new(id: u64) -> Self {
        Self(id)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Display)]
#[repr(transparent)]
pub struct BlockId(u64);

impl BlockId {
    pub const fn new(id: u64) -> Self {
        Self(id)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Display)]
#[repr(transparent)]
pub struct VarId(u64);

impl VarId {
    pub const fn new(id: u64) -> Self {
        Self(id)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
enum Var {
    User(StrT),
    HirAuto(usize),
    MirAuto(u64),
}

impl From<HirVar> for Var {
    #[inline]
    fn from(var: HirVar) -> Self {
        match var {
            HirVar::User(user) => Self::User(user),
            HirVar::Auto(auto) => Self::HirAuto(auto),
        }
    }
}

impl From<&HirVar> for Var {
    #[inline]
    fn from(var: &HirVar) -> Self {
        Self::from(*var)
    }
}

#[derive(Debug)]
pub struct MirBuilder {
    functions: Vec<Function>,
    external_functions: Vec<ExternFunc>,
    // TODO: Custom enum to represent the block superposition
    blocks: Vec<BasicBlock>,
    current_block: BlockId,
    // TODO: Custom struct w/ function arg & ret types
    function_names: HashMap<ItemPath, (FuncId, Type)>,
    current_function: FuncId,
    func_counter: FuncId,
    // TODO: Reinserting variables will shadow previous ones, but is that a bad thing?
    variables: HashMap<Var, Variable>,
    var_counter: VarId,
}

impl MirBuilder {
    pub fn new() -> Self {
        Self {
            functions: Vec::new(),
            external_functions: Vec::new(),
            blocks: Vec::new(),
            current_block: BlockId::new(0),
            function_names: HashMap::new(),
            current_function: FuncId::new(0),
            func_counter: FuncId::new(0),
            variables: HashMap::new(),
            var_counter: VarId::new(0),
        }
    }

    #[instrument(name = "lowering to mir")]
    pub fn lower(mut self, items: &[Item]) -> MirResult<Mir> {
        self.function_names = HashMap::from_iter(items.iter().filter_map(|item| {
            if let Item::Function(HirFunction { name, ret, .. }) = item {
                Some((name.clone(), (self.next_func_id(), Type::from(&ret.kind))))
            } else if let Item::ExternFunc(HirExternFunc { name, ret, .. }) = item {
                Some((name.clone(), (self.next_func_id(), Type::from(&ret.kind))))
            } else {
                None
            }
        }));

        for item in items {
            self.visit_item(item)?;
        }

        Ok(Mir {
            functions: self.functions,
            external_functions: self.external_functions,
        })
    }

    fn next_var(&mut self) -> VarId {
        let id = self.var_counter;
        self.var_counter.0 += 1;

        id
    }

    fn next_func_id(&mut self) -> FuncId {
        let id = self.func_counter;
        self.func_counter.0 += 1;

        id
    }

    fn next_block(&mut self) -> BlockId {
        let id = BlockId(self.blocks.len() as u64);

        self.blocks.push(BasicBlock::new(id, None));
        self.current_block = id;

        id
    }

    fn insert_variable(&mut self, var: Var, ty: Type) -> VarId {
        let id = self.next_var();

        // FIXME: https://github.com/rust-lang/rust/issues/62633
        let prev_var = self.variables.insert(var, Variable { id, ty }).is_none();
        assert!(prev_var, "Reinserted a variable into a function",);

        id
    }

    fn get_function_id(&self, name: &ItemPath) -> FuncId {
        self.function_names
            .get(name)
            .expect("Attempted to fetch a nonexistent function")
            .0
    }

    fn make_assignment(&mut self, var: impl Into<Option<Var>>, val: Rval) -> VarId {
        let var = var
            .into()
            .unwrap_or_else(|| Var::MirAuto(self.next_var().0));
        let ty = val.ty.clone();
        let id = self.insert_variable(var, ty.clone());

        self.current_block_mut()
            .push(Instruction::Assign(Assign { var: id, val, ty }));

        id
    }

    fn move_to_block(&mut self, block_id: BlockId) {
        assert!(
            self.blocks.iter().any(|block| block.id == block_id),
            "Attempted to move to a nonexistent block"
        );

        self.current_block = block_id;
    }

    fn current_block_mut(&mut self) -> &mut BasicBlock {
        &mut self.blocks[self.current_block.0 as usize]
    }

    fn verify_current_block(&mut self) -> MirResult<()> {
        self.blocks[self.current_block.0 as usize].verify()
    }

    fn get_variable(&self, var: Var) -> &Variable {
        self.variables
            .get(&var.into())
            .expect("Attempted to get a variable that doesn't exist")
    }
}

impl ItemVisitor for MirBuilder {
    type Output = MirResult<()>;

    fn visit_func(&mut self, func: &HirFunction) -> Self::Output {
        self.blocks.clear();
        self.blocks.push(BasicBlock::new(BlockId::new(0), None));
        self.current_block = BlockId::new(0);

        let id = self.get_function_id(&func.name);

        let mut args = Vec::with_capacity(func.args.len());
        for FuncArg { name, kind, .. } in func.args.iter() {
            let ty: Type = kind.into();
            let id = self.insert_variable(name.into(), ty.clone());

            args.push(Variable { id, ty });
        }

        for stmt in func.body.iter() {
            self.visit_stmt(stmt)?;
        }

        // FIXME: Use a better system of a "current block" that's an `Option<BlockId>` with operations
        //        automatically creating a new one if needed and not relying on one to already exist.
        //        This'd guarantee that only used blocks are ever created instead of speculatively creating
        //        blocks for future use
        let blocks = self
            .blocks
            .drain(..)
            .filter(|block| !block.is_empty())
            .collect();

        let func = Function {
            id,
            name: Some(func.name.clone()),
            args,
            ret: (&func.ret.kind).into(),

            blocks,
        };
        self.functions.push(func);

        // TODO: Return the function's id
        Ok(())
    }

    fn visit_extern_func(&mut self, func: &HirExternFunc) -> Self::Output {
        let id = self.get_function_id(&func.name);

        let mut args = Vec::with_capacity(func.args.len());
        for FuncArg { name, kind, .. } in func.args.iter() {
            let ty: Type = kind.into();
            let id = self.insert_variable(name.into(), ty.clone());

            args.push(Variable { id, ty });
        }

        let func = ExternFunc {
            id,
            name: func.name.clone(),
            args,
            ret: (&func.ret.kind).into(),
            callconv: func.callconv,
        };
        self.external_functions.push(func);

        // TODO: Return the function's id
        Ok(())
    }
}

impl StmtVisitor for MirBuilder {
    type Output = MirResult<()>;

    fn visit_stmt(&mut self, stmt: &Stmt) -> <Self as StmtVisitor>::Output {
        match stmt {
            Stmt::Item(item) => self.visit_item(item)?,
            Stmt::Expr(expr) => {
                if let Some(val) = self.visit_expr(expr)? {
                    self.make_assignment(None, val);
                }
            }
            Stmt::VarDecl(var) => self.visit_var_decl(var)?,
        }

        Ok(())
    }

    fn visit_var_decl(&mut self, var: &VarDecl) -> <Self as StmtVisitor>::Output {
        let val = self
            .visit_expr(&*var.value)?
            .expect("Assigned nothing to a variable");
        self.make_assignment(Some(var.name.into()), val);

        Ok(())
    }
}

impl ExprVisitor for MirBuilder {
    type Output = MirResult<Option<Rval>>;

    fn visit_return(&mut self, _loc: Location, ret: &Return) -> Self::Output {
        match ret.val.as_ref() {
            Some(val) => {
                // Evaluate the returned value and assign it to a temp variable before returning it
                let ret = self
                    .visit_expr(&**val)?
                    // TODO: This may be ok behavior in the case of something like a `return return 0`,
                    //       may need an unnesting pass to remove those
                    .expect("Received nothing where a value was expected");

                let return_value = self.make_assignment(None, ret);
                self.current_block_mut()
                    .set_terminator(Terminator::Return(Some(return_value)));
            }

            None => self
                .current_block_mut()
                .set_terminator(Terminator::Return(None)),
        }

        self.verify_current_block()?;
        self.next_block();

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
        let current_block = self.current_block;

        let (condition, condition_type) = {
            let cond = self
                .visit_expr(cond)?
                .expect("Received nothing where a value was expected");
            let cond_ty = cond.ty.clone();
            let cond = self.make_assignment(None, cond);

            (cond, cond_ty)
        };

        let mut cases = Vec::with_capacity(arms.len());
        let mut default = None;
        for MatchArm {
            bind: Binding { pattern, .. },
            body,
            ..
        } in arms
        {
            let case_block = self.next_block();
            self.move_to_block(current_block);

            match pattern {
                Pattern::Literal(lit) => {
                    let case = self.visit_literal(loc, lit)?.unwrap();
                    debug_assert!(condition_type == case.ty);

                    cases.push(SwitchCase {
                        condition: self.make_assignment(None, case),
                        block: case_block,
                        args: Vec::new(),
                    })
                }
                Pattern::ItemPath(..) => todo!(),
                Pattern::Ident(ident) => {
                    self.move_to_block(case_block);

                    let id = self.next_var();
                    self.current_block_mut().push_argument(Variable {
                        id,
                        ty: condition_type.clone(),
                    });

                    self.move_to_block(current_block);

                    // FIXME: https://github.com/rust-lang/rust/issues/62633
                    let prev_default = default.replace(DefaultSwitchCase {
                        block: case_block,
                        args: vec![self.get_variable(Var::User(*ident)).id],
                    });
                    assert!(
                        prev_default.is_none(),
                        "Inserted multiple default cases in a switch"
                    );
                }
                Pattern::Wildcard => {
                    // FIXME: https://github.com/rust-lang/rust/issues/62633
                    let prev_default = default.replace(DefaultSwitchCase {
                        block: case_block,
                        args: Vec::new(),
                    });
                    assert!(
                        prev_default.is_none(),
                        "Inserted multiple default cases in a switch"
                    );
                }
            }

            self.move_to_block(case_block);
            for stmt in body.iter() {
                self.visit_stmt(stmt)?;
            }
        }

        self.move_to_block(current_block);
        self.current_block_mut().set_terminator(Terminator::Switch {
            condition,
            default: default.expect("Created a switch with no default case"),
            cases,
        });

        self.verify_current_block()?;
        // TODO: Joining blocks just doesn't happen here
        self.next_block();

        // TODO: Maybe return unit?
        Ok(None)
    }

    fn visit_variable(&mut self, _loc: Location, var: HirVar, ty: &TypeKind) -> Self::Output {
        let Variable {
            id: val,
            ty: var_ty,
        } = self.get_variable(var.into());
        assert_eq!(var_ty, &Type::from(ty));

        Ok(Some(Rval {
            val: Value::Variable(*val),
            ty: var_ty.clone(),
        }))
    }

    // FIXME: Give literals their type in hir
    fn visit_literal(
        &mut self,
        loc: Location,
        HirLiteral { val, ty, .. }: &HirLiteral,
    ) -> Self::Output {
        match val {
            HirLiteralVal::Integer(Integer { sign, bits }) => {
                // FIXME: Doesn't respect types
                let val = Value::Const(Constant::Integer {
                    sign: *sign,
                    bits: *bits,
                });
                let rval = Rval {
                    ty: Type::from(&ty.kind),
                    val,
                };

                Ok(Some(rval))
            }

            HirLiteralVal::Bool(b) => Ok(Some(Rval {
                ty: Type::Bool,
                val: Value::Const(Constant::Bool(*b)),
            })),

            HirLiteralVal::String(string) => Ok(Some(Rval {
                ty: Type::String,
                val: Value::Const(Constant::String(string.to_bytes())),
            })),

            HirLiteralVal::Array(array) => Ok(Some(Rval {
                // FIXME: Doesn't respect types
                ty: Type::Array(Ref::new(Type::U8), array.len() as u32),
                val: Value::Const(Constant::Array(
                    array
                        .into_iter()
                        .map(|e| {
                            self.visit_literal(loc, e)
                                .map(|e| e.unwrap().into_constant().unwrap())
                        })
                        .collect::<MirResult<Vec<_>>>()?,
                )),
            })),

            lit => todo!("{:?}", lit),
        }
    }

    fn visit_scope(&mut self, _loc: Location, _body: &HirBlock<Stmt>) -> Self::Output {
        todo!()
    }

    fn visit_func_call(&mut self, _loc: Location, call: &FuncCall) -> Self::Output {
        let (function, ty) = self
            .function_names
            .get(&call.func)
            .expect("Attempted to call a function that doesn't exist")
            .clone();

        let args: Vec<VarId> = call
            .args
            .iter()
            .map(|a| {
                let val = self
                    .visit_expr(a)?
                    .expect("Received no value where one was expected");

                Ok(self.make_assignment(None, val))
            })
            .collect::<MirResult<_>>()?;

        let val = Value::Call(FnCall { function, args });

        Ok(Some(Rval { ty, val }))
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

    fn visit_assign(&mut self, _loc: Location, var: HirVar, value: &Expr) -> Self::Output {
        let old_var = self.get_variable(var.into()).clone();
        let rval = self
            .visit_expr(value)?
            .expect("Received no value where one was expected");
        assert_eq!(rval.ty, old_var.ty);

        let ty = rval.ty.clone();
        let id = self.make_assignment(None, rval);
        self.variables.insert(var.into(), Variable { id, ty });

        // TODO: Return unit?
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
            self.visit_expr(lhs)?
                .expect("Received no value where one was expected"),
            self.visit_expr(rhs)?
                .expect("Received no value where one was expected"),
        );
        assert_eq!(lhs.ty, rhs.ty);
        let ty = lhs.ty.clone();

        let (lhs, rhs) = (
            self.make_assignment(None, lhs),
            self.make_assignment(None, rhs),
        );

        #[rustfmt::skip]
        let val = match op {
            BinaryOp::Add  => Value::Add(lhs, rhs),
            BinaryOp::Sub  => Value::Sub(lhs, rhs),
            BinaryOp::Mult => Value::Mul(lhs, rhs),
            BinaryOp::Div  => Value::Div(lhs, rhs),

            _ => todo!(),
        };

        Ok(Some(Rval { ty, val }))
    }

    fn visit_cast(&mut self, _loc: Location, Cast { casted, ty }: &Cast) -> Self::Output {
        let ty = Type::from(&ty.kind);
        let casted = self
            .visit_expr(casted)?
            .expect("Received no value where one was expected");
        let val = Value::Cast(self.make_assignment(None, casted), ty.clone());

        Ok(Some(Rval { ty, val }))
    }

    fn visit_reference(
        &mut self,
        _loc: Location,
        Reference { mutable, reference }: &Reference,
    ) -> Self::Output {
        let reference = self
            .visit_expr(reference)?
            .expect("Received no value where one was expected");
        let ty = reference.ty.clone();

        let pointee = self.make_assignment(None, reference);
        let val = Value::GetPointer {
            var: pointee,
            mutable: *mutable,
            aliasable: false,
        };

        Ok(Some(Rval { ty, val }))
    }
}
