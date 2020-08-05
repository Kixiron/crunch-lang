use crate::{
    context::ContextDatabase,
    error::{Locatable, MirError, MirResult},
    strings::{StrInterner, StrT},
    trees::{hir::Var as HirVar, CallConv, ItemPath, Ref, Sign},
};
use alloc::{string::ToString, vec::Vec};
use core::iter;
use derive_more::Display;
use pretty::{BoxAllocator, DocAllocator, DocBuilder};

#[derive(Debug, PartialEq, Eq)]
pub struct Mir {
    functions: Vec<Function>,
    external_functions: Vec<ExternFunc>,
}

impl Mir {
    pub const fn new(functions: Vec<Function>, external_functions: Vec<ExternFunc>) -> Self {
        Self {
            functions,
            external_functions,
        }
    }

    /// An iterator over all functions
    pub fn functions(&self) -> impl Iterator<Item = &Function> {
        self.functions.iter()
    }

    /// An iterator over all external functions
    pub fn external_functions(&self) -> impl Iterator<Item = &ExternFunc> {
        self.external_functions.iter()
    }

    pub fn to_doc<'a, D>(
        &self,
        alloc: &'a D,
        mir: &Mir,
        interner: &StrInterner,
    ) -> DocBuilder<'a, D>
    where
        D: DocAllocator<'a>,
        D::Doc: Clone,
    {
        alloc
            .nil()
            .append(alloc.intersperse(
                self.functions().map(|f| f.to_doc(alloc, mir, interner)),
                alloc.line(),
            ))
            .append(alloc.line())
            .append(
                alloc.intersperse(
                    self.external_functions()
                        .map(|f| f.to_doc(alloc, mir, interner)),
                    alloc.line(),
                ),
            )
    }

    // FIXME: This isn't super efficient
    pub fn write_pretty(&self, interner: &StrInterner) -> String {
        crate::warn!("Using an inefficient method of MIR rendering");

        self.to_doc(&BoxAllocator, &self, interner)
            .1
            .pretty(80)
            .to_string()
    }
}

/// A function
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Function {
    /// The id of the current function
    pub id: FuncId,
    /// The name of the function
    pub name: ItemPath,
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

    pub fn to_doc<'a, D>(
        &self,
        alloc: &'a D,
        mir: &Mir,
        interner: &StrInterner,
    ) -> DocBuilder<'a, D>
    where
        D: DocAllocator<'a>,
        D::Doc: Clone,
    {
        alloc
            .text("fn")
            .append(alloc.space())
            .append(alloc.text(self.name.to_string(interner)))
            .append(alloc.text("("))
            .append(
                alloc
                    .intersperse(
                        self.args.iter().map(|a| a.to_doc(alloc, mir, interner)),
                        alloc.text(",").append(alloc.space()),
                    )
                    .group(),
            )
            .append(alloc.text(")"))
            .append(alloc.space())
            .append(alloc.text("->"))
            .append(alloc.space())
            .append(self.ret.to_doc(alloc, mir, interner))
            .append(alloc.hardline())
            .append(
                alloc
                    .intersperse(
                        self.blocks.iter().map(|b| b.to_doc(alloc, mir, interner)),
                        alloc.hardline().append(alloc.hardline()),
                    )
                    .indent(4),
            )
            .append(alloc.hardline())
            .append(alloc.text("end"))
            .append(alloc.line())
    }
}

/// A typed variable
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Variable {
    /// The id of the variable
    pub id: VarId,
    /// The type of the variable
    pub ty: Type,
}

impl Variable {
    pub const fn new(id: VarId, ty: Type) -> Self {
        Self { id, ty }
    }

    pub fn to_doc<'a, D>(
        &self,
        alloc: &'a D,
        mir: &Mir,
        interner: &StrInterner,
    ) -> DocBuilder<'a, D>
    where
        D: DocAllocator<'a>,
        D::Doc: Clone,
    {
        alloc
            .nil()
            .append(self.id.to_doc(alloc, interner))
            .append(alloc.text(":"))
            .append(alloc.space())
            .append(self.ty.to_doc(alloc, mir, interner))
    }
}

/// An externally defined function
#[derive(Debug, Clone, PartialEq, Eq)]
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

impl ExternFunc {
    pub fn to_doc<'a, D>(
        &self,
        alloc: &'a D,
        mir: &Mir,
        interner: &StrInterner,
    ) -> DocBuilder<'a, D>
    where
        D: DocAllocator<'a>,
        D::Doc: Clone,
    {
        alloc
            .text("@extern(\"")
            .append(alloc.text(self.callconv.to_string()))
            .append(alloc.text("\")"))
            .append(alloc.line())
            .append(alloc.text("fn"))
            .append(alloc.space())
            .append(alloc.text(self.name.to_string(interner)))
            .append(alloc.text("("))
            .append(
                alloc
                    .intersperse(
                        self.args.iter().map(|a| a.to_doc(alloc, mir, interner)),
                        alloc.text(",").append(alloc.space()),
                    )
                    .group(),
            )
            .append(alloc.text(")"))
            .append(alloc.space())
            .append(alloc.text("->"))
            .append(alloc.space())
            .append(self.ret.to_doc(alloc, mir, interner))
            .append(alloc.text(";"))
            .append(alloc.line())
    }
}

/// A basic block
///
/// Each basic block is a completely self-contained scope with its own variable scope
/// starting at zero and outside communication only possible by passing arguments
// TODO: Sealing
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BasicBlock {
    /// The id of the current block
    pub id: BlockId,
    /// The name of the current block, if there is one
    pub name: Option<StrT>,
    /// The arguments passed to the current block when called
    pub args: Vec<(Variable, Vec<BlockId>)>,
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

    pub fn name(&self, db: &dyn ContextDatabase) -> String {
        if let Some(name) = self.name {
            format!(
                "BasicBlock No.{} ('{}')",
                self.id,
                db.context().strings().resolve(name).as_ref(),
            )
        } else {
            format!("BasicBlock No.{}", self.id)
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

    pub fn push_argument(&mut self, arg: Variable, successors: Vec<BlockId>) {
        // TODO: Make sure the successors aren't duplicated
        self.args.push((arg, successors));
    }

    /// Sets the block's terminator
    pub fn set_terminator(&mut self, terminator: Terminator) {
        if self.terminator.is_some() {
            // TODO: Should this be a fatal error?
            crate::error!("Overwrote a block's terminator");
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
            return Err(Locatable::none(MirError::MissingTerminator(
                id.0.to_string(),
            )));
        }

        // Make sure no block args are duplicated
        for (arg, block) in args.iter() {
            let count = args
                .iter()
                .filter(|(a, b)| a.id == arg.id || b == block)
                .count();

            if count > 1 {
                // TODO: Resolve the block's name for the error
                return Err(Locatable::none(MirError::DuplicatedBBArg(id.0, arg.id.0)));
            }
        }

        Ok(())
    }

    pub fn to_doc<'a, D>(
        &self,
        alloc: &'a D,
        mir: &Mir,
        interner: &StrInterner,
    ) -> DocBuilder<'a, D>
    where
        D: DocAllocator<'a>,
        D::Doc: Clone,
    {
        alloc
            .nil()
            .append(
                self.name
                    .map(|n| {
                        alloc
                            .text("@named(\"")
                            .append(alloc.text(interner.resolve(n).as_ref().to_owned()))
                            .append(alloc.text("\")"))
                            .append(alloc.hardline())
                    })
                    .unwrap_or_else(|| alloc.nil()),
            )
            .append(alloc.text("block"))
            .append(alloc.space())
            .append(self.id.to_doc(alloc, interner))
            .append(if self.args.is_empty() {
                alloc.nil()
            } else {
                alloc.text(" << ").append(
                    alloc
                        .intersperse(
                            self.args
                                .iter()
                                .map(|(a, _)| a.to_doc(alloc, mir, interner)),
                            alloc.text(",").append(alloc.space()),
                        )
                        .group(),
                )
            })
            .append(alloc.hardline())
            .append(
                alloc
                    .intersperse(
                        self.instructions
                            .iter()
                            .map(|inst| inst.to_doc(alloc, mir, interner))
                            .chain(iter::once(
                                alloc.hardline().append(
                                    self.terminator
                                        .as_ref()
                                        .map(|term| term.to_doc(alloc, mir, interner))
                                        .unwrap_or_else(|| {
                                            alloc.text("<Missing block terminator>")
                                        }),
                                ),
                            )),
                        alloc.hardline(),
                    )
                    .indent(4),
            )
            .append(alloc.hardline())
            .append(alloc.text("end"))
    }
}

/// A block terminator, every `BasicBlock` is closed by one
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Terminator {
    /// A return instruction, `None` for a unit return and `Some` for returning a value
    Return(Option<VarId>),
    /// An unconditional jump to the given block
    Jump(BlockId, Vec<VarId>),
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

impl Terminator {
    pub fn to_doc<'a, D>(
        &self,
        alloc: &'a D,
        _mir: &Mir,
        interner: &StrInterner,
    ) -> DocBuilder<'a, D>
    where
        D: DocAllocator<'a>,
        D::Doc: Clone,
    {
        match self {
            Self::Return(ret) => {
                alloc
                    .text("return")
                    .append(alloc.space())
                    .append(if let Some(ret) = ret {
                        ret.to_doc(alloc, interner)
                    } else {
                        alloc.text("unit")
                    })
            }

            Self::Jump(block, args) => alloc
                .text("jump")
                .append(alloc.space())
                .append(block.to_doc(alloc, interner))
                .append(if args.is_empty() {
                    alloc.nil()
                } else {
                    alloc
                        .space()
                        .append(alloc.text("<<"))
                        .append(alloc.space())
                        .append(alloc.intersperse(
                            args.iter().map(|a| a.to_doc(alloc, interner)),
                            alloc.text(",").append(alloc.space()),
                        ))
                }),

            Self::Branch {
                condition,
                truthy,
                falsy,
            } => alloc
                .text("branch")
                .append(alloc.space())
                .append(alloc.text("if"))
                .append(alloc.space())
                .append(condition.to_doc(alloc, interner))
                .append(alloc.space())
                .append(alloc.text("then"))
                .append(alloc.space())
                .append(truthy.to_doc(alloc, interner))
                .append(alloc.space())
                .append(alloc.text("else"))
                .append(alloc.space())
                .append(falsy.to_doc(alloc, interner)),

            Self::Switch {
                condition,
                cases,
                default,
            } => alloc
                .text("switch")
                .append(alloc.space())
                .append(condition.to_doc(alloc, interner))
                .append(alloc.hardline())
                .append(
                    alloc
                        .intersperse(
                            cases.iter().map(|case| case.to_doc(alloc, interner)),
                            alloc.hardline(),
                        )
                        .indent(4),
                )
                .append(alloc.hardline())
                .append(default.to_doc(alloc, interner).indent(4))
                .append(alloc.hardline())
                .append(alloc.text("end")),

            Self::Unreachable => alloc.text("unreachable"),
        }
    }
}

/// A switch case, contains a condition and a block
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SwitchCase {
    /// The condition being tested
    pub condition: VarId,
    /// The block jumped to if the condition is true
    pub block: BlockId,
    /// The arguments passed to the basic block
    pub args: Vec<VarId>,
}

impl SwitchCase {
    pub fn to_doc<'a, D>(&self, alloc: &'a D, interner: &StrInterner) -> DocBuilder<'a, D>
    where
        D: DocAllocator<'a>,
        D::Doc: Clone,
    {
        self.condition
            .to_doc(alloc, interner)
            .append(alloc.space())
            .append(alloc.text("->"))
            .append(alloc.space())
            .append(self.block.to_doc(alloc, interner))
            .append(if self.args.is_empty() {
                alloc.nil()
            } else {
                alloc
                    .space()
                    .append(alloc.text("<<"))
                    .append(alloc.space())
                    .append(alloc.intersperse(
                        self.args.iter().map(|a| a.to_doc(alloc, interner)),
                        alloc.text(",").append(alloc.space()),
                    ))
            })
    }
}

/// The default case of a switch
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DefaultSwitchCase {
    /// The block jumped to
    pub block: BlockId,
    /// The arguments passed to the basic block
    pub args: Vec<VarId>,
}

impl DefaultSwitchCase {
    pub fn to_doc<'a, D>(&self, alloc: &'a D, interner: &StrInterner) -> DocBuilder<'a, D>
    where
        D: DocAllocator<'a>,
        D::Doc: Clone,
    {
        alloc
            .text("default")
            .append(alloc.space())
            .append(alloc.text("->"))
            .append(alloc.space())
            .append(self.block.to_doc(alloc, interner))
            .append(if self.args.is_empty() {
                alloc.nil()
            } else {
                alloc
                    .space()
                    .append(alloc.text("<<"))
                    .append(alloc.space())
                    .append(alloc.intersperse(
                        self.args.iter().map(|a| a.to_doc(alloc, interner)),
                        alloc.text(",").append(alloc.space()),
                    ))
            })
    }
}

/// An instruction
#[derive(Debug, Clone, PartialEq, Eq)]
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

    pub fn to_doc<'a, D>(
        &self,
        alloc: &'a D,
        mir: &Mir,
        interner: &StrInterner,
    ) -> DocBuilder<'a, D>
    where
        D: DocAllocator<'a>,
        D::Doc: Clone,
    {
        match self {
            Self::Assign(assign) => assign.to_doc(alloc, mir, interner),
            Self::Call(call) => call.to_doc(alloc, mir, interner),
        }
    }
}

/// A variable assignment
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Assign {
    /// The id of the variable being assigned to
    pub var: VarId,
    /// The value being assigned to the variable
    pub val: Rval,
    /// The type of the variable
    pub ty: Type,
}

impl Assign {
    pub fn to_doc<'a, D>(
        &self,
        alloc: &'a D,
        mir: &Mir,
        interner: &StrInterner,
    ) -> DocBuilder<'a, D>
    where
        D: DocAllocator<'a>,
        D::Doc: Clone,
    {
        alloc
            .text("let")
            .append(alloc.space())
            .append(self.var.to_doc(alloc, interner))
            .append(alloc.text(":"))
            .append(alloc.space())
            .append(self.ty.to_doc(alloc, mir, interner))
            .append(alloc.space())
            .append(alloc.text(":="))
            .append(alloc.space())
            .append(self.val.to_doc(alloc, mir, interner))
    }
}

/// A function call
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FnCall {
    /// The function being called
    pub function: FuncId,
    /// The arguments being passed to the function
    pub args: Vec<VarId>,
}

impl FnCall {
    pub fn to_doc<'a, D>(
        &self,
        alloc: &'a D,
        mir: &Mir,
        interner: &StrInterner,
    ) -> DocBuilder<'a, D>
    where
        D: DocAllocator<'a>,
        D::Doc: Clone,
    {
        mir.functions
            .iter()
            .find_map(|f| {
                if f.id == self.function {
                    Some(&f.name)
                } else {
                    None
                }
            })
            .map(|name| {
                alloc
                    .text("call")
                    .append(alloc.space())
                    .append(alloc.text(name.to_string(interner)))
            })
            .unwrap_or_else(|| {
                mir.external_functions
                    .iter()
                    .find_map(|f| {
                        if f.id == self.function {
                            Some(&f.name)
                        } else {
                            None
                        }
                    })
                    .map(|name| {
                        alloc
                            .text("extern call")
                            .append(alloc.space())
                            .append(alloc.text(name.to_string(interner)))
                    })
                    .unwrap()
            })
            .append(alloc.text("("))
            .append(
                alloc
                    .intersperse(
                        self.args.iter().map(|a| a.to_doc(alloc, interner)),
                        alloc.text(",").append(alloc.space()),
                    )
                    .group(),
            )
            .append(alloc.text(")"))
    }
}

/// The right-hand side of an expression
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Rval {
    pub val: Value,
    pub ty: Type,
}

impl Rval {
    pub const fn new(val: Value, ty: Type) -> Self {
        Self { val, ty }
    }

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

    pub fn to_doc<'a, D>(
        &self,
        alloc: &'a D,
        mir: &Mir,
        interner: &StrInterner,
    ) -> DocBuilder<'a, D>
    where
        D: DocAllocator<'a>,
        D::Doc: Clone,
    {
        alloc.nil().append(self.val.to_doc(alloc, mir, interner))
    }
}

/// The inner value of an `Rval`
#[derive(Debug, Clone, PartialEq, Eq)]
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
    /// Returns a `true` boolean if the values are equal
    Eq(VarId, VarId),
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
            | Self::Div(lhs, rhs)
            | Self::Eq(lhs, rhs) => {
                buf.push(*lhs);
                buf.push(*rhs);
            }
            Self::Variable(var) | Self::GetPointer { var, .. } | Self::Cast(var, _) => {
                buf.push(*var);
            }
            Self::Const(_) => {}
        }
    }

    pub fn to_doc<'a, D>(
        &self,
        alloc: &'a D,
        mir: &Mir,
        interner: &StrInterner,
    ) -> DocBuilder<'a, D>
    where
        D: DocAllocator<'a>,
        D::Doc: Clone,
    {
        match self {
            Self::Variable(var) => var.to_doc(alloc, interner),
            Self::Const(constant) => constant.to_doc(alloc, interner),
            Self::Call(call) => call.to_doc(alloc, mir, interner),

            Self::Add(lhs, rhs) => alloc
                .text("add")
                .append(alloc.space())
                .append(lhs.to_doc(alloc, interner))
                .append(alloc.text(","))
                .append(alloc.space())
                .append(rhs.to_doc(alloc, interner)),

            Self::Sub(lhs, rhs) => alloc
                .text("sub")
                .append(alloc.space())
                .append(lhs.to_doc(alloc, interner))
                .append(alloc.text(","))
                .append(alloc.space())
                .append(rhs.to_doc(alloc, interner)),

            Self::Mul(lhs, rhs) => alloc
                .text("mul")
                .append(alloc.space())
                .append(lhs.to_doc(alloc, interner))
                .append(alloc.text(","))
                .append(alloc.space())
                .append(rhs.to_doc(alloc, interner)),

            Self::Div(lhs, rhs) => alloc
                .text("div")
                .append(alloc.space())
                .append(lhs.to_doc(alloc, interner))
                .append(alloc.text(","))
                .append(alloc.space())
                .append(rhs.to_doc(alloc, interner)),

            Self::Eq(lhs, rhs) => alloc
                .text("eq")
                .append(alloc.space())
                .append(lhs.to_doc(alloc, interner))
                .append(alloc.text(","))
                .append(alloc.space())
                .append(rhs.to_doc(alloc, interner)),

            Self::GetPointer {
                var,
                mutable,
                aliasable,
            } => alloc
                .text("ptr")
                .append(alloc.space())
                .append(if *mutable {
                    alloc.text("mut").append(alloc.space())
                } else {
                    alloc.nil()
                })
                .append(if *aliasable {
                    alloc.text("alias").append(alloc.space())
                } else {
                    alloc.nil()
                })
                .append(var.to_doc(alloc, interner)),

            Self::Cast(var, ty) => alloc
                .text("cast")
                .append(alloc.space())
                .append(var.to_doc(alloc, interner))
                .append(alloc.space())
                .append(alloc.text("as"))
                .append(alloc.space())
                .append(ty.to_doc(alloc, mir, interner)),
        }
    }
}

/// A compile time known constant value
#[derive(Debug, Clone, PartialEq, Eq)]
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

impl Constant {
    pub fn to_doc<'a, D>(&self, alloc: &'a D, interner: &StrInterner) -> DocBuilder<'a, D>
    where
        D: DocAllocator<'a>,
        D::Doc: Clone,
    {
        let constant = match self {
            Self::Integer { sign, bits } => alloc
                .text(sign.to_string())
                .append(alloc.text(bits.to_string())),
            Self::Bool(boolean) => alloc.text(boolean.to_string()),
            Self::String(string) => alloc.text(String::from_utf8(string.clone()).unwrap()),
            Self::Array(array) => alloc
                .text("arr[")
                .append(
                    alloc
                        .intersperse(
                            array.iter().map(|c| c.to_doc(alloc, interner)),
                            alloc.text(",").append(alloc.space()),
                        )
                        .group(),
                )
                .append(alloc.text("]")),
        };

        alloc.text("const").append(alloc.space()).append(constant)
    }
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
#[derive(Debug, Clone, PartialEq, Eq)]
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
    Array { element: Ref<Type>, length: u64 },
    Slice { element: Ref<Type> },
    Reference { referee: Ref<Type>, mutable: bool },
    Pointer { pointee: Ref<Type>, mutable: bool },
    String,
    Absurd,
}

impl Type {
    /// If the current type is an array, get the type of the elements it contains
    pub fn array_elements(&self) -> Option<&Self> {
        if let Self::Array { element, .. } = self {
            Some(&**element)
        } else {
            None
        }
    }

    pub fn is_unsigned(&self) -> bool {
        matches!(self, Self::U8 | Self::U16 | Self::U32 | Self::U64)
    }

    pub fn is_signed(&self) -> bool {
        matches!(self, Self::I8 | Self::I16 | Self::I32 | Self::I64)
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
        is_unit   => Self::Unit,
        is_array  => Self::Array { .. },
        is_string => Self::String,
    }

    pub fn to_doc<'a, D>(
        &self,
        alloc: &'a D,
        mir: &Mir,
        interner: &StrInterner,
    ) -> DocBuilder<'a, D>
    where
        D: DocAllocator<'a>,
        D::Doc: Clone,
    {
        match self {
            Self::U8 => alloc.text("u8"),
            Self::I8 => alloc.text("i8"),
            Self::U16 => alloc.text("u16"),
            Self::I16 => alloc.text("i16"),
            Self::U32 => alloc.text("u32"),
            Self::I32 => alloc.text("i32"),
            Self::U64 => alloc.text("u64"),
            Self::I64 => alloc.text("i64"),
            Self::Bool => alloc.text("bool"),
            Self::Unit => alloc.text("unit"),

            &Self::Pointer {
                ref pointee,
                mutable,
            } => alloc
                .nil()
                .append(if mutable {
                    alloc.text("*mut")
                } else {
                    alloc.text("*const")
                })
                .append(alloc.space())
                .append(pointee.to_doc(alloc, mir, interner)),

            &Self::Array {
                ref element,
                length,
            } => alloc
                .text("arr[")
                .append(alloc.text(length.to_string()))
                .append(alloc.text(";"))
                .append(alloc.space())
                .append(element.to_doc(alloc, mir, interner))
                .append(alloc.text("]")),

            Self::Slice { element } => alloc
                .text("slice[")
                .append(element.to_doc(alloc, mir, interner))
                .append(alloc.text("]")),

            &Self::Reference {
                ref referee,
                mutable,
            } => alloc
                .text("&")
                .append(if mutable {
                    alloc.text("mut").append(alloc.space())
                } else {
                    alloc.nil()
                })
                .append(referee.to_doc(alloc, mir, interner)),

            Self::String => alloc.text("str"),
            Self::Absurd => alloc.text("absurd"),
        }
    }
}

// FIXME: This all needs to be elsewhere, changes to hir -> mir lowering cause a global recompile

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Display)]
#[repr(transparent)]
pub struct FuncId(pub u64);

impl FuncId {
    pub const fn new(id: u64) -> Self {
        Self(id)
    }

    pub fn to_doc<'a, D>(&self, alloc: &'a D, _interner: &StrInterner) -> DocBuilder<'a, D>
    where
        D: DocAllocator<'a>,
        D::Doc: Clone,
    {
        alloc.text(format!("@{}", self.0))
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Display)]
#[repr(transparent)]
pub struct BlockId(pub u64);

impl BlockId {
    pub const fn new(id: u64) -> Self {
        Self(id)
    }

    pub fn to_doc<'a, D>(&self, alloc: &'a D, _interner: &StrInterner) -> DocBuilder<'a, D>
    where
        D: DocAllocator<'a>,
        D::Doc: Clone,
    {
        alloc.text(format!("bb{}", self.0))
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Display)]
#[repr(transparent)]
pub struct VarId(pub u64);

impl VarId {
    pub const fn new(id: u64) -> Self {
        Self(id)
    }

    pub fn to_doc<'a, D>(&self, alloc: &'a D, _interner: &StrInterner) -> DocBuilder<'a, D>
    where
        D: DocAllocator<'a>,
        D::Doc: Clone,
    {
        alloc.text(format!("_{}", self.0))
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Var {
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
