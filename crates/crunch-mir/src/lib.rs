#![no_std]

extern crate alloc;

use alloc::{vec, vec::Vec};
use core::iter::FromIterator;
use crunch_shared::{
    crunch_proc::instrument,
    error::{Location, MirResult},
    trees::{
        ast::Integer,
        hir::{
            BinaryOp, Binding, Block as HirBlock, Block, Break, Cast, CompOp, Expr,
            ExternFunc as HirExternFunc, FuncArg, FuncCall, Function as HirFunction, Item,
            Literal as HirLiteral, LiteralVal as HirLiteralVal, Match, MatchArm, Pattern,
            Reference, Return, Stmt, Type as HirType, TypeKind as HirTypeKind, Var as HirVar,
            VarDecl,
        },
        mir::{
            Assign, BasicBlock, BlockId, Constant, DefaultSwitchCase, ExternFunc, FnCall, FuncId,
            Function, Instruction, Mir, Rval, SwitchCase, Terminator, Type, Value, Var, VarId,
            Variable,
        },
        ItemPath, Ref,
    },
    utils::HashMap,
    visitors::hir::{ExprVisitor, ItemVisitor, StmtVisitor, TypeVisitor},
};
use crunch_typecheck::Engine;

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
    type_engine: Engine,
    // TODO: Give MirBuilder access to the type engine for type resolution or make a final pass in the engine to resolve types
    // TODO: Salsa for types?
}

impl MirBuilder {
    pub fn new(type_engine: Engine) -> Self {
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
            type_engine,
        }
    }

    #[instrument(name = "lowering to mir")]
    pub fn lower(mut self, items: &[Item]) -> MirResult<Mir> {
        self.function_names = HashMap::from_iter(items.iter().filter_map(|item| {
            if let Item::Function(HirFunction { name, ret, .. }) = item {
                Some((name.clone(), (self.next_func_id(), self.visit_type(ret))))
            } else if let Item::ExternFunc(HirExternFunc { name, ret, .. }) = item {
                Some((name.clone(), (self.next_func_id(), self.visit_type(ret))))
            } else {
                None
            }
        }));

        for item in items {
            self.visit_item(item)?;
        }

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
        let id = BlockId(self.blocks.len() as u64);

        self.blocks.push(BasicBlock::new(id, None));
        self.current_block = id;

        id
    }

    fn insert_variable(&mut self, var: Var, ty: Type) -> VarId {
        let id = self.next_var();
        self.variables.insert(var, Variable { id, ty });

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
            let ty = self.visit_type(kind);
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
            name: func.name.clone(),
            args,
            ret: self.visit_type(&func.ret),
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
            let ty = self.visit_type(kind);
            let id = self.insert_variable(name.into(), ty.clone());

            args.push(Variable { id, ty });
        }

        let func = ExternFunc {
            id,
            name: func.name.clone(),
            args,
            ret: self.visit_type(&func.ret),
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

    fn visit_break(&mut self, _loc: Location, _value: &Break) -> Self::Output {
        todo!()
    }

    fn visit_continue(&mut self, _loc: Location) -> Self::Output {
        todo!()
    }

    fn visit_loop(&mut self, _loc: Location, _body: &Block<Stmt>) -> Self::Output {
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
                    // FIXME: Sometimes things just don't work?
                    assert!(condition_type == case.ty);

                    cases.push(SwitchCase {
                        condition: self.make_assignment(None, case),
                        block: case_block,
                        args: Vec::new(),
                    });
                }
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
                Pattern::ItemPath(..) => todo!(),
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

    fn visit_variable(&mut self, _loc: Location, var: HirVar, _ty: &HirType) -> Self::Output {
        let Variable {
            id: val,
            ty: var_ty,
        } = self.get_variable(var.into());

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

impl TypeVisitor for MirBuilder {
    type Output = Type;

    fn visit_type(&mut self, r#type: &HirType) -> Self::Output {
        match &r#type.kind {
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
            &HirTypeKind::Pointer {
                ref pointee,
                mutable,
            } => Type::Pointer {
                pointee: Ref::new(self.visit_type(pointee.as_ref())),
                mutable,
            },
            HirTypeKind::String => Type::String,
            HirTypeKind::Absurd => Type::Absurd,
            &HirTypeKind::Array {
                ref element,
                length,
            } => Type::Array {
                element: Ref::new(self.visit_type(element.as_ref())),
                length,
            },
            HirTypeKind::Slice { element } => Type::Slice {
                element: Ref::new(self.visit_type(element.as_ref())),
            },
            &HirTypeKind::Reference {
                ref referee,
                mutable,
            } => Type::Reference {
                referee: Ref::new(self.visit_type(referee.as_ref())),
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
