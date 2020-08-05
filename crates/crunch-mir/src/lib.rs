//#![no_std]

extern crate alloc;

use alloc::{sync::Arc, vec, vec::Vec};
use core::{fmt, iter::FromIterator};
use crunch_shared::{
    config::EmissionKind,
    context::ContextDatabase,
    error::{Error, ErrorHandler, Location, MirResult},
    files::FileId,
    salsa,
    trees::{
        ast::Integer,
        hir::{
            BinaryOp, Binding, Block as HirBlock, Block, Break, Cast, CompOp, Expr,
            ExternFunc as HirExternFunc, FuncArg, FuncCall, Function as HirFunction, Item,
            Literal as HirLiteral, LiteralVal as HirLiteralVal, Match, MatchArm, Pattern,
            Reference, Return, Stmt, TypeId, TypeKind as HirTypeKind, Var as HirVar, VarDecl,
        },
        mir::{
            Assign, BasicBlock, BlockId, Constant, DefaultSwitchCase, ExternFunc, FnCall, FuncId,
            Function, Instruction, Mir, Rval, SwitchCase, Terminator, Type, Value, Var, VarId,
            Variable,
        },
        ItemPath, Ref,
    },
    utils::{HashMap, Hasher},
    visitors::hir::{ExprVisitor, ItemVisitor, StmtVisitor, TypeVisitor},
};
use crunch_typecheck::TypecheckDatabase;
use ladder::HirDatabase;

#[salsa::query_group(MirDatabaseStorage)]
pub trait MirDatabase: salsa::Database + ContextDatabase + HirDatabase + TypecheckDatabase {
    // FIXME: Actual lifetimes when salsa allows
    fn lower_mir(&self, file: FileId) -> Result<Arc<Mir>, Arc<ErrorHandler>>;
}

fn lower_mir(db: &dyn MirDatabase, file: FileId) -> Result<Arc<Mir>, Arc<ErrorHandler>> {
    let config = db.config();
    let items = db.lower_hir(file)?;
    db.typecheck(file)?;

    let mir = crunch_shared::allocator::CRUNCHC_ALLOCATOR
        .record_region("mir lowering", || MirBuilder::new(db).lower(&*items))
        .map_err(|err| {
            let mut errors = ErrorHandler::new();
            errors.push_err(err.map(Error::Mir));

            errors
        })?;

    if config.emit.contains(&EmissionKind::Mir) {
        let path = db
            .config()
            .out_dir
            .join(&*db.file_name(file))
            .with_extension("mir");

        std::fs::write(
            &path,
            format!("{}", mir.write_pretty(db.context().strings())),
        )
        .unwrap();
    }

    if config.print.contains(&EmissionKind::Mir) {
        println!("{}", mir.write_pretty(db.context().strings()));
    }

    Ok(Arc::new(mir))
}

#[derive(Debug)]
struct BlockState {
    blocks: Vec<BasicBlock>,
    current_block: BlockId,
}

pub struct MirBuilder<'db> {
    functions: Vec<Function>,
    external_functions: Vec<ExternFunc>,
    // TODO: Custom enum to represent the block superposition
    blocks: Vec<BlockState>,
    current_block: BlockId,
    // TODO: Custom struct w/ function arg & ret types
    function_names: HashMap<ItemPath, (FuncId, Type)>,
    func_counter: FuncId,
    variables: Vec<HashMap<Var, Variable>>,
    var_counter: VarId,
    // TODO: Give MirBuilder access to the type engine for type resolution or make a final pass in the engine to resolve types
    // TODO: Salsa for types?
    db: &'db dyn MirDatabase,
}

impl<'db> MirBuilder<'db> {
    pub fn new(db: &'db dyn MirDatabase) -> Self {
        Self {
            functions: Vec::new(),
            external_functions: Vec::new(),
            blocks: Vec::new(),
            current_block: BlockId::new(0),
            function_names: HashMap::with_hasher(Hasher::default()),
            func_counter: FuncId::new(0),
            variables: Vec::new(),
            var_counter: VarId::new(0),
            db,
        }
    }

    pub fn lower(mut self, items: &[&'db Item<'db>]) -> MirResult<Mir> {
        self.with_scope(|builder| {
            builder.function_names =
                HashMap::from_iter(items.iter().filter_map(|item| match item {
                    &&Item::Function(HirFunction { ref name, ret, .. })
                    | &&Item::ExternFunc(HirExternFunc { ref name, ret, .. }) => Some((
                        name.clone(),
                        (builder.next_func_id(), builder.visit_type(ret)),
                    )),
                }));

            for item in items {
                builder.visit_item(item)?;
            }

            Ok(())
        })?;

        Ok(Mir::new(self.functions, self.external_functions))
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
        let id = BlockId(self.blocks.last().unwrap().blocks.len() as u64);

        self.blocks
            .last_mut()
            .unwrap()
            .blocks
            .push(BasicBlock::new(id, None));
        self.current_block = id;

        crunch_shared::trace!("Created a new block: {:?}", id);

        id
    }

    fn create_variable(&mut self, var: Var, ty: Type) -> VarId {
        let id = self.next_var();
        self.insert_variable(var, Variable { id, ty });

        id
    }

    fn insert_variable(&mut self, name: Var, var: Variable) {
        self.variables.last_mut().unwrap().insert(name, var);
    }

    fn get_function_id(&self, name: &ItemPath) -> FuncId {
        self.function_names
            .get(name)
            .expect("Attempted to fetch a nonexistent function")
            .0
    }

    fn make_assignment(&mut self, var: impl Into<Option<Var>>, val: Rval) -> VarId {
        // FIXME: Stop reassigning variables to variables
        // if let &Value::Variable(var) = &val.val {
        //     // Replace usages somehow to remove re-assigns
        // } else {
        //     // Create var
        // }

        let var = if let Some(var) = var.into() {
            var
        } else {
            Var::MirAuto(self.next_var().0)
        };

        let ty = val.ty.clone();
        let id = self.create_variable(var, ty.clone());

        self.current_block_mut()
            .push(Instruction::Assign(Assign { var: id, val, ty }));

        id
    }

    fn move_to_block(&mut self, block_id: BlockId) {
        assert!(
            self.blocks
                .last()
                .unwrap()
                .blocks
                .iter()
                .any(|block| block.id == block_id),
            "Attempted to move to a nonexistent block",
        );

        self.current_block = block_id;
    }

    fn current_block_mut(&mut self) -> &mut BasicBlock {
        &mut self.blocks.last_mut().unwrap().blocks[self.current_block.0 as usize]
    }

    fn verify_current_block(&mut self) -> MirResult<()> {
        self.blocks.last_mut().unwrap().blocks[self.current_block.0 as usize].verify()
    }

    fn get_block_mut(&mut self, block: BlockId) -> Option<&mut BasicBlock> {
        self.blocks
            .last_mut()
            .unwrap()
            .blocks
            .get_mut(block.0 as usize)
    }

    fn get_variable(&self, var: Var) -> Option<&Variable> {
        self.variables.iter().rev().find_map(|vars| vars.get(&var))
    }

    fn make_block(&mut self, current_block: BlockId, arm: &MatchArm<'db>) -> MirResult<BlockId> {
        let block = self.next_block();

        self.move_to_block(block);
        self.with_scope(|builder| {
            for stmt in arm.body.iter() {
                builder.visit_stmt(stmt)?;
            }

            Ok(())
        })?;

        self.move_to_block(current_block);
        Ok(block)
    }

    fn push_scope(&mut self) {
        self.variables.push(HashMap::with_hasher(Hasher::default()));
    }

    fn pop_scope(&mut self) {
        self.variables.pop().unwrap();
    }

    fn with_scope<F, T>(&mut self, func: F) -> T
    where
        F: FnOnce(&mut Self) -> T,
    {
        self.push_scope();
        let result = func(self);
        self.pop_scope();

        result
    }

    fn push_all_blocks(&mut self) {
        self.blocks.push(BlockState {
            blocks: vec![BasicBlock::new(BlockId::new(0), None)],
            current_block: BlockId::new(0),
        });
        self.current_block = BlockId::new(0);
    }

    fn pop_all_blocks(&mut self) -> Vec<BasicBlock> {
        self.blocks.pop().unwrap().blocks
    }

    fn with_blocks<F, B, T, L>(&mut self, func: F, blocks: B) -> L
    where
        F: FnOnce(&mut Self) -> T,
        B: FnOnce(&mut Self, Vec<BasicBlock>, T) -> L,
    {
        self.push_all_blocks();
        let result = func(self);
        let popped = self.pop_all_blocks();

        blocks(self, popped, result)
    }
}

impl<'db> ItemVisitor<'db> for MirBuilder<'db> {
    type Output = MirResult<()>;

    fn visit_func(&mut self, func: &HirFunction<'db>) -> Self::Output {
        self.with_blocks(
            |builder| {
                builder.with_scope(|builder| {
                    let id = builder.get_function_id(&func.name);

                    let mut args = Vec::with_capacity(func.args.len());
                    for &FuncArg { name, kind, .. } in func.args.iter() {
                        let ty = builder.visit_type(kind);
                        let id = builder.create_variable(name.into(), ty.clone());

                        args.push(Variable { id, ty });
                    }

                    for stmt in func.body.iter() {
                        builder.visit_stmt(stmt)?;
                    }

                    Ok((id, func.name.clone(), args, builder.visit_type(func.ret)))
                })
            },
            |builder, blocks, res| {
                let (id, name, args, ret) = res?;

                // FIXME: Use a better system of a "current block" that's an `Option<BlockId>` with operations
                //        automatically creating a new one if needed and not relying on one to already exist.
                //        This would guarantee that only used blocks are ever created instead of speculatively
                //        creating blocks for future use
                // FIXME: https://github.com/rust-lang/rust/issues/43244
                let blocks = blocks
                    .into_iter()
                    .filter(|block| !block.is_empty())
                    .collect();

                let func = Function {
                    id,
                    name,
                    args,
                    ret,
                    blocks,
                };
                builder.functions.push(func);

                // TODO: Return the function's id?
                Ok(())
            },
        )
    }

    fn visit_extern_func(&mut self, func: &HirExternFunc) -> Self::Output {
        let id = self.get_function_id(&func.name);

        let mut args = Vec::with_capacity(func.args.len());
        for &FuncArg { name, kind, .. } in func.args.iter() {
            let ty = self.visit_type(kind);
            let id = self.create_variable(name.into(), ty.clone());

            args.push(Variable { id, ty });
        }

        let func = ExternFunc {
            id,
            name: func.name.clone(),
            args,
            ret: self.visit_type(func.ret),
            callconv: func.callconv,
        };
        self.external_functions.push(func);

        // TODO: Return the function's id
        Ok(())
    }
}

impl<'db> StmtVisitor<'db> for MirBuilder<'db> {
    type Output = MirResult<Option<Rval>>;

    fn visit_stmt(&mut self, stmt: &'db Stmt<'db>) -> <Self as StmtVisitor<'db>>::Output {
        match stmt {
            Stmt::Item(item) => {
                self.visit_item(item)?;
                Ok(None)
            }
            Stmt::Expr(expr) => self.visit_expr(expr),
            Stmt::VarDecl(var) => self.visit_var_decl(var),
        }
    }

    fn visit_var_decl(&mut self, var: &VarDecl<'db>) -> <Self as StmtVisitor<'db>>::Output {
        let val = self
            .visit_expr(var.value)?
            .expect("Assigned nothing to a variable");
        self.make_assignment(Some(var.name.into()), val);

        Ok(None)
    }
}

impl<'db> ExprVisitor<'db> for MirBuilder<'db> {
    type Output = MirResult<Option<Rval>>;

    fn visit_return(&mut self, _loc: Location, ret: &Return<'db>) -> Self::Output {
        match ret.val {
            Some(val) => {
                // Evaluate the returned value and assign it to a temp variable before returning it
                let ret = self
                    .visit_expr(val)?
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

    fn visit_break(&mut self, _loc: Location, _value: &Break) -> Self::Output {
        todo!()
    }

    fn visit_continue(&mut self, _loc: Location) -> Self::Output {
        todo!()
    }

    fn visit_loop(&mut self, _loc: Location, _body: &Block<&'db Stmt<'db>>) -> Self::Output {
        todo!()
    }

    fn visit_match(
        &mut self,
        loc: Location,
        &Match {
            cond, ref arms, ty, ..
        }: &Match<'db>,
    ) -> Self::Output {
        let current_block = self.current_block;
        let end_block = self.next_block();

        let return_var = {
            let return_type = self.visit_type(ty);

            if return_type.is_unit() {
                None
            } else {
                let id = self.next_var();
                let block = self.get_block_mut(end_block).unwrap();
                block.push_argument(
                    Variable::new(id, return_type.clone()),
                    Vec::with_capacity(arms.len()),
                );

                Some(Rval::new(Value::Variable(id), return_type))
            }
        };

        self.move_to_block(current_block);

        let (condition, condition_type) = {
            let cond = self
                .visit_expr(cond)?
                .expect("Received nothing where a value was expected");
            let cond_ty = cond.ty.clone();
            let mut cond = self.make_assignment(None, cond);

            if cond_ty.is_bool() && arms.len() == 2 {
                let true_ = self.make_assignment(
                    None,
                    Rval::new(Value::Const(Constant::Bool(true)), Type::Bool),
                );
                let normalized_cond =
                    self.make_assignment(None, Rval::new(Value::Eq(cond, true_), Type::Bool));

                cond = normalized_cond;
            }

            (cond, cond_ty)
        };

        if arms.len() == 2 && condition_type.is_bool() {
            let (truthy, falsy) = if let [truthy, falsy] = arms.as_slice() {
                (
                    self.make_block(current_block, truthy)?,
                    self.make_block(current_block, falsy)?,
                )
            } else {
                unreachable!();
            };

            self.move_to_block(current_block);
            self.current_block_mut().set_terminator(Terminator::Branch {
                condition,
                truthy,
                falsy,
            });
        } else {
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
                        // FIXME: Sometimes things just don't work?
                        assert!(condition_type == case.ty);

                        cases.push(SwitchCase {
                            condition: self.make_assignment(None, case),
                            block: case_block,
                            args: Vec::new(),
                        });
                    }
                    &Pattern::Ident(ident) => {
                        let passed_var =
                            self.create_variable(Var::User(ident), condition_type.clone());

                        self.move_to_block(case_block);
                        self.current_block_mut().push_argument(
                            Variable::new(passed_var, condition_type.clone()),
                            vec![current_block],
                        );

                        self.move_to_block(current_block);
                        // FIXME: https://github.com/rust-lang/rust/issues/62633
                        let prev_default = default.replace(DefaultSwitchCase {
                            block: case_block,
                            args: vec![passed_var],
                        });
                        assert!(
                            prev_default.is_none(),
                            "Inserted multiple default cases in a switch",
                        );
                    }
                    Pattern::ItemPath(..) => todo!(),
                    Pattern::Wildcard => {
                        // FIXME: https://github.com/rust-lang/rust/issues/62633
                        let prev_default = default.replace(DefaultSwitchCase {
                            block: case_block,
                            args: Vec::new(),
                        });
                        assert!(
                            prev_default.is_none(),
                            "Inserted multiple default cases in a switch",
                        );
                    }
                }

                self.move_to_block(case_block);
                let passed_val = self.with_scope(|builder| {
                    body.iter()
                        .map(|stmt| builder.visit_stmt(stmt).transpose())
                        .last()
                        .flatten()
                        .transpose()
                })?;

                if return_var.is_some() {
                    let passed_val = passed_val.expect("Expected to pass a value to a child block");
                    let passed_val = self.make_assignment(None, passed_val);

                    self.get_block_mut(case_block)
                        .unwrap()
                        .set_terminator(Terminator::Jump(end_block, vec![passed_val]))
                }
            }

            self.move_to_block(current_block);
            self.current_block_mut().set_terminator(Terminator::Switch {
                condition,
                default: default.expect("Created a switch with no default case"),
                cases,
            });
        }

        self.verify_current_block()?;
        self.move_to_block(end_block);

        // TODO: Maybe return unit?
        Ok(return_var)
    }

    fn visit_variable(&mut self, _loc: Location, var: HirVar, _ty: TypeId) -> Self::Output {
        let &Variable {
            id: val,
            ty: ref var_ty,
        } = self
            .get_variable(var.into())
            .expect("Attempted to get a variable that doesn't exist");

        Ok(Some(Rval {
            val: Value::Variable(val),
            ty: var_ty.clone(),
        }))
    }

    // FIXME: Give literals their type in hir
    fn visit_literal(
        &mut self,
        loc: Location,
        &HirLiteral { ref val, ty, .. }: &HirLiteral,
    ) -> Self::Output {
        match val {
            &HirLiteralVal::Integer(Integer { sign, bits }) => {
                // FIXME: Doesn't respect types
                let val = Value::Const(Constant::Integer { sign, bits });
                let rval = Rval {
                    ty: self.visit_type(ty),
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

            HirLiteralVal::Array { elements } => Ok(Some(Rval {
                // FIXME: Doesn't respect types
                ty: Type::Array {
                    element: Ref::new(Type::U8),
                    length: elements.len() as u64,
                },
                val: Value::Const(Constant::Array(
                    elements
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

    fn visit_scope(&mut self, _loc: Location, body: &HirBlock<&'db Stmt<'db>>) -> Self::Output {
        self.with_scope(|builder| {
            body.iter()
                .filter_map(|s| builder.visit_stmt(s).transpose())
                .last()
                .transpose()
        })
    }

    fn visit_func_call(&mut self, _loc: Location, call: &FuncCall<'db>) -> Self::Output {
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
        _lhs: &'db Expr<'db>,
        _op: CompOp,
        _rhs: &'db Expr<'db>,
    ) -> Self::Output {
        todo!()
    }

    fn visit_assign(&mut self, _loc: Location, var: HirVar, value: &'db Expr<'db>) -> Self::Output {
        let old_var = self
            .get_variable(var.into())
            .expect("Attempted to get a variable that doesn't exist")
            .clone();
        let rval = self
            .visit_expr(value)?
            .expect("Received no value where one was expected");
        assert_eq!(rval.ty, old_var.ty);

        let ty = rval.ty.clone();
        let id = self.make_assignment(None, rval);
        self.insert_variable(var.into(), Variable { id, ty });

        // TODO: Return unit?
        Ok(None)
    }

    fn visit_binop(
        &mut self,
        _loc: Location,
        lhs: &'db Expr<'db>,
        op: BinaryOp,
        rhs: &'db Expr<'db>,
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

    fn visit_cast(&mut self, _loc: Location, &Cast { casted, ty }: &Cast<'db>) -> Self::Output {
        let ty = self.visit_type(ty);
        let casted = self
            .visit_expr(casted)?
            .expect("Received no value where one was expected");
        let val = Value::Cast(self.make_assignment(None, casted), ty.clone());

        Ok(Some(Rval { ty, val }))
    }

    fn visit_reference(
        &mut self,
        _loc: Location,
        Reference { mutable, reference }: &Reference<'db>,
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

impl<'db> TypeVisitor<'db> for MirBuilder<'db> {
    type Output = Type;

    fn visit_type(&mut self, ty: TypeId) -> Self::Output {
        match self.db.context().get_hir_type(ty).unwrap().kind {
            HirTypeKind::Variable(ty) => self.visit_type(ty),
            HirTypeKind::Integer { signed, width } => {
                match (signed.unwrap_or(true), width.unwrap_or(32)) {
                    (false, 8) => Type::U8,
                    (true, 8) => Type::I8,
                    (false, 16) => Type::U16,
                    (true, 16) => Type::I16,
                    (false, 32) => Type::U32,
                    (true, 32) => Type::I32,
                    (false, 64) => Type::U64,
                    (true, 64) => Type::I64,

                    (sign, width) => todo!("{}{}", sign, width),
                }
            }
            HirTypeKind::Bool => Type::Bool,
            HirTypeKind::Unit => Type::Unit,
            HirTypeKind::Pointer { pointee, mutable } => Type::Pointer {
                pointee: Ref::new(self.visit_type(pointee)),
                mutable,
            },
            HirTypeKind::String => Type::String,
            HirTypeKind::Absurd => Type::Absurd,
            HirTypeKind::Array { element, length } => Type::Array {
                element: Ref::new(self.visit_type(element)),
                length,
            },
            HirTypeKind::Slice { element } => Type::Slice {
                element: Ref::new(self.visit_type(element)),
            },
            HirTypeKind::Reference { referee, mutable } => Type::Reference {
                referee: Ref::new(self.visit_type(referee)),
                mutable,
            },

            // FIXME: This should be an error for the user
            HirTypeKind::Unknown => {
                crunch_shared::warn!("This should be an error for the user");
                unreachable!("All types should have been inferred by now");
            }
        }
    }
}

impl fmt::Debug for MirBuilder<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("MirBuilder")
            .field("functions", &self.functions)
            .field("external_functions", &self.external_functions)
            .field("blocks", &self.blocks)
            .field("function_names", &self.function_names)
            .field("func_counter", &self.func_counter)
            .field("var_counter", &self.var_counter)
            .finish()
    }
}
