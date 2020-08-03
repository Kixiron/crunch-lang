pub mod llvm;

use core::convert::TryInto;
use crunch_mir::MirDatabase;
use crunch_shared::{
    allocator::CRUNCHC_ALLOCATOR,
    config::EmissionKind,
    context::ContextDatabase,
    error::ErrorHandler,
    files::FileId,
    salsa,
    trees::{
        mir::{
            Assign, BasicBlock, BlockId, Constant, FnCall, FuncId, Function as MirFunction,
            Instruction, Mir, Rval, SwitchCase, Terminator, Type, Value, VarId, Variable,
        },
        CallConv,
    },
    utils::{Either, HashMap, Hasher},
    visitors::mir::MirVisitor,
};
use llvm::{
    module::{BuildingBlock, FunctionBuilder, Linkage, Module},
    types::{ArrayType, IntType, SealedAnyType, Type as LLVMType, VoidType, I1},
    utils::{AddressSpace, CallingConvention, IntOperand, EMPTY_CSTR},
    values::{
        AnyValue, ArrayValue, BasicBlock as LLVMBasicBlock, CallSiteValue, FunctionValue,
        InstructionValue, SealedAnyValue, Value as RawLLVMValue,
    },
    Context, Result as LLVMResult,
};
use std::sync::Arc;

#[derive(Debug, PartialEq, Eq)]
pub struct BundledModule {
    context: Option<Context>,
    module: Option<Module<'static>>,
}

impl BundledModule {
    pub fn get<'ctx>(&'ctx self) -> &'ctx Module<'ctx> {
        self.module.as_ref().unwrap()
    }
}

impl Drop for BundledModule {
    fn drop(&mut self) {
        drop(self.module.take());
        drop(self.context.take());
    }
}

#[salsa::query_group(CodegenDatabaseStorage)]
pub trait CodegenDatabase: salsa::Database + ContextDatabase + MirDatabase {
    // FIXME: Actual lifetimes when salsa allows
    // FIXME: Better system for LLVM Contexts
    fn generate_module(&self, file: FileId) -> Result<Arc<BundledModule>, Arc<ErrorHandler>>;
}

fn generate_module(
    db: &dyn CodegenDatabase,
    file: FileId,
) -> Result<Arc<BundledModule>, Arc<ErrorHandler>> {
    let config = db.config();
    let mir = db.lower_mir(file)?;

    let context = Context::new().unwrap();
    let module =
        crunch_shared::allocator::CRUNCHC_ALLOCATOR.record_region("code generation", || {
            let module = context.module(&*db.file_name(file)).unwrap();
            CodeGenerator::new(db, &context, &module)
                .generate(&*mir)
                .unwrap();

            // FIXME: Actual lifetimes when salsa allows + a context database?
            unsafe { std::mem::transmute::<Module<'_>, Module<'static>>(module) }
        });

    // Verify the generated module
    CRUNCHC_ALLOCATOR.record_region("module verification", || module.verify().unwrap());

    if config.emit.contains(&EmissionKind::LlvmIr) {
        let path = db
            .config()
            .out_dir
            .join(&*db.file_name(file))
            .with_extension("ll");

        module.emit_ir_to_file(&path).unwrap();
    }

    // TODO: Use native LLVM function since it's probably more efficient
    if config.print.contains(&EmissionKind::LlvmIr) {
        crunch_shared::warn!("Using an inefficient method of printing LLVM IR to stdout");
        println!("{:?}", module);
    }

    if config.emit.contains(&EmissionKind::LlvmBc) {
        let path = db
            .config()
            .out_dir
            .join(&*db.file_name(file))
            .with_extension("bc");

        module.emit_ir_to_file(config.out_dir.join(&path)).unwrap();
    }

    if config.emit.contains(&EmissionKind::LlvmBc) {
        // TODO: Print object file to stdout?
        println!("Printing LLVM Bitcode to stdout is currently unsupported");
    }

    Ok(Arc::new(BundledModule {
        context: Some(context),
        module: Some(module),
    }))
}

pub struct CodeGenerator<'db> {
    functions: HashMap<FuncId, FunctionValue<'db>>,
    blocks: HashMap<BlockId, LLVMBasicBlock<'db>>,
    values: HashMap<VarId, (LLVMValue<'db>, Type)>,
    function_builder: Option<FunctionBuilder<'db>>,
    block_builder: Option<BuildingBlock<'db>>,
    context: &'db Context,
    module: &'db Module<'db>,
    db: &'db dyn CodegenDatabase,
}

impl<'db> CodeGenerator<'db> {
    pub fn new(
        db: &'db dyn CodegenDatabase,
        context: &'db Context,
        module: &'db Module<'db>,
    ) -> Self {
        Self {
            module,
            values: HashMap::with_hasher(Hasher::default()),
            blocks: HashMap::with_hasher(Hasher::default()),
            functions: HashMap::with_hasher(Hasher::default()),
            function_builder: None,
            block_builder: None,
            context,
            db,
        }
    }

    pub fn generate(mut self, mir: &Mir) -> LLVMResult<()> {
        for function in mir.functions() {
            let args: Vec<LLVMType<'db>> = function
                .args
                .iter()
                .map(|Variable { ty, .. }| self.visit_type(ty).unwrap())
                .collect();

            let sig =
                self.module
                    .function_ty(self.visit_type(&function.ret)?, args.as_slice(), false)?;

            let function_val = self
                .module
                .create_function(function.name.to_string(self.db.context().strings()), sig)?;

            function_val.with_linkage(Linkage::External);
            self.functions.insert(function.id, function_val);
        }

        for function in mir.external_functions() {
            let args: Vec<LLVMType<'db>> = function
                .args
                .iter()
                .map(|Variable { ty, .. }| self.visit_type(ty).unwrap())
                .collect();

            let sig =
                self.module
                    .function_ty(self.visit_type(&function.ret)?, args.as_slice(), false)?;

            let function_val = self
                .module
                .create_function(function.name.to_string(self.db.context().strings()), sig)?;

            function_val
                .with_linkage(Linkage::External)
                .with_calling_convention(match function.callconv {
                    CallConv::C => CallingConvention::C,
                    CallConv::Crunch => CallingConvention::X86RegCall,
                });
            self.functions.insert(function.id, function_val);
        }

        for function in mir.functions() {
            self.visit_function(function)?;
        }

        Ok(())
    }

    fn current_function(&self) -> FunctionValue<'db> {
        self.function_builder
            .as_ref()
            .expect("called current_function outside of a function")
            .function()
    }

    fn get_function_builder(&self) -> &FunctionBuilder<'db> {
        self.function_builder
            .as_ref()
            .expect("Called CodeGenerator::get_function_builder outside of a function")
    }

    fn get_block_builder(&self) -> &BuildingBlock<'db> {
        self.block_builder
            .as_ref()
            .expect("called CodeGenerator::get_builder outside of a basic block")
    }

    fn get_function(&self, function: FuncId) -> FunctionValue<'db> {
        *self
            .functions
            .get(&function)
            .expect("attempted to get a function that doesn't exist")
    }

    fn get_block(&self, block: BlockId) -> LLVMBasicBlock<'db> {
        *self
            .blocks
            .get(&block)
            .expect("attempted to get a basic block that doesn't exist")
    }

    fn get_var(&self, id: VarId) -> (LLVMValue<'db>, &Type) {
        let (val, ty) = self
            .values
            .get(&id)
            .expect("attempted to get a var that doesn't exist");

        (*val, ty)
    }

    fn get_var_value(&self, id: VarId) -> LLVMValue<'db> {
        self.get_var(id).0
    }

    // TODO: Query this shit
    // TODO: Macro this shit

    fn add(&self, lhs: LLVMValue<'db>, rhs: LLVMValue<'db>) -> LLVMResult<LLVMValue<'db>> {
        let block_builder = self.get_block_builder();

        Ok(match (lhs, rhs) {
            (LLVMValue::U8(lhs), LLVMValue::U8(rhs)) => {
                LLVMValue::U8(block_builder.add(lhs, rhs)?.into())
            }
            (LLVMValue::I8(lhs), LLVMValue::I8(rhs)) => {
                LLVMValue::I8(block_builder.add(lhs, rhs)?.into())
            }

            (LLVMValue::U16(lhs), LLVMValue::U16(rhs)) => {
                LLVMValue::U16(block_builder.add(lhs, rhs)?.into())
            }
            (LLVMValue::I16(lhs), LLVMValue::I16(rhs)) => {
                LLVMValue::I16(block_builder.add(lhs, rhs)?.into())
            }

            (LLVMValue::U32(lhs), LLVMValue::U32(rhs)) => {
                LLVMValue::U32(block_builder.add(lhs, rhs)?.into())
            }
            (LLVMValue::I32(lhs), LLVMValue::I32(rhs)) => {
                LLVMValue::I32(block_builder.add(lhs, rhs)?.into())
            }

            (LLVMValue::U64(lhs), LLVMValue::U64(rhs)) => {
                LLVMValue::U64(block_builder.add(lhs, rhs)?.into())
            }
            (LLVMValue::I64(lhs), LLVMValue::I64(rhs)) => {
                LLVMValue::I64(block_builder.add(lhs, rhs)?.into())
            }

            (lhs, rhs) => panic!("Illegal instruction: Add {:?} by {:?}", lhs, rhs),
        })
    }

    fn sub(&self, lhs: LLVMValue<'db>, rhs: LLVMValue<'db>) -> LLVMResult<LLVMValue<'db>> {
        let block_builder = self.get_block_builder();

        Ok(match (lhs, rhs) {
            (LLVMValue::U8(lhs), LLVMValue::U8(rhs)) => {
                LLVMValue::U8(block_builder.sub(lhs, rhs)?.into())
            }
            (LLVMValue::I8(lhs), LLVMValue::I8(rhs)) => {
                LLVMValue::I8(block_builder.sub(lhs, rhs)?.into())
            }

            (LLVMValue::U16(lhs), LLVMValue::U16(rhs)) => {
                LLVMValue::U16(block_builder.sub(lhs, rhs)?.into())
            }
            (LLVMValue::I16(lhs), LLVMValue::I16(rhs)) => {
                LLVMValue::I16(block_builder.sub(lhs, rhs)?.into())
            }

            (LLVMValue::U32(lhs), LLVMValue::U32(rhs)) => {
                LLVMValue::U32(block_builder.sub(lhs, rhs)?.into())
            }
            (LLVMValue::I32(lhs), LLVMValue::I32(rhs)) => {
                LLVMValue::I32(block_builder.sub(lhs, rhs)?.into())
            }

            (LLVMValue::U64(lhs), LLVMValue::U64(rhs)) => {
                LLVMValue::U64(block_builder.sub(lhs, rhs)?.into())
            }
            (LLVMValue::I64(lhs), LLVMValue::I64(rhs)) => {
                LLVMValue::I64(block_builder.sub(lhs, rhs)?.into())
            }

            (lhs, rhs) => panic!("Illegal instruction: Subtract {:?} by {:?}", lhs, rhs),
        })
    }

    fn mul(&self, lhs: LLVMValue<'db>, rhs: LLVMValue<'db>) -> LLVMResult<LLVMValue<'db>> {
        let block_builder = self.get_block_builder();

        Ok(match (lhs, rhs) {
            (LLVMValue::U8(lhs), LLVMValue::U8(rhs)) => {
                LLVMValue::U8(block_builder.mul(lhs, rhs)?.into())
            }
            (LLVMValue::I8(lhs), LLVMValue::I8(rhs)) => {
                LLVMValue::I8(block_builder.mul(lhs, rhs)?.into())
            }

            (LLVMValue::U16(lhs), LLVMValue::U16(rhs)) => {
                LLVMValue::U16(block_builder.mul(lhs, rhs)?.into())
            }
            (LLVMValue::I16(lhs), LLVMValue::I16(rhs)) => {
                LLVMValue::I16(block_builder.mul(lhs, rhs)?.into())
            }

            (LLVMValue::U32(lhs), LLVMValue::U32(rhs)) => {
                LLVMValue::U32(block_builder.mul(lhs, rhs)?.into())
            }
            (LLVMValue::I32(lhs), LLVMValue::I32(rhs)) => {
                LLVMValue::I32(block_builder.mul(lhs, rhs)?.into())
            }

            (LLVMValue::U64(lhs), LLVMValue::U64(rhs)) => {
                LLVMValue::U64(block_builder.mul(lhs, rhs)?.into())
            }
            (LLVMValue::I64(lhs), LLVMValue::I64(rhs)) => {
                LLVMValue::I64(block_builder.mul(lhs, rhs)?.into())
            }

            (lhs, rhs) => panic!("Illegal instruction: Multiply {:?} by {:?}", lhs, rhs),
        })
    }

    fn div(&self, lhs: LLVMValue<'db>, rhs: LLVMValue<'db>) -> LLVMResult<LLVMValue<'db>> {
        let block_builder = self.get_block_builder();

        Ok(match (lhs, rhs) {
            (LLVMValue::U8(lhs), LLVMValue::U8(rhs)) => {
                LLVMValue::U8(block_builder.udiv(lhs, rhs)?.into())
            }

            (LLVMValue::I8(lhs), LLVMValue::I8(rhs)) => {
                LLVMValue::I8(block_builder.sdiv(lhs, rhs)?.into())
            }

            (LLVMValue::U16(lhs), LLVMValue::U16(rhs)) => {
                LLVMValue::U16(block_builder.udiv(lhs, rhs)?.into())
            }
            (LLVMValue::I16(lhs), LLVMValue::I16(rhs)) => {
                LLVMValue::I16(block_builder.sdiv(lhs, rhs)?.into())
            }

            (LLVMValue::U32(lhs), LLVMValue::U32(rhs)) => {
                LLVMValue::U32(block_builder.udiv(lhs, rhs)?.into())
            }
            (LLVMValue::I32(lhs), LLVMValue::I32(rhs)) => {
                LLVMValue::I32(block_builder.sdiv(lhs, rhs)?.into())
            }

            (LLVMValue::U64(lhs), LLVMValue::U64(rhs)) => {
                LLVMValue::U64(block_builder.udiv(lhs, rhs)?.into())
            }
            (LLVMValue::I64(lhs), LLVMValue::I64(rhs)) => {
                LLVMValue::I64(block_builder.sdiv(lhs, rhs)?.into())
            }

            (lhs, rhs) => panic!("Illegal instruction: Divide {:?} by {:?}", lhs, rhs),
        })
    }

    fn ret(&self, val: Option<LLVMValue<'db>>) -> LLVMResult<InstructionValue<'db>> {
        crunch_shared::warn!("Check that returned values match their function signature");

        // TODO: Verify return type against current function's return type
        self.get_block_builder().ret(val.map(|v| v.as_value()))
    }

    fn eq(&self, lhs: LLVMValue<'db>, rhs: LLVMValue<'db>) -> LLVMResult<LLVMValue<'db>> {
        crunch_shared::warn!("Check that lhs and rhs have the same types in Eq");

        Ok(LLVMValue::Bool(self.get_block_builder().icmp(
            lhs,
            IntOperand::Equal,
            rhs,
        )?))
    }
}

impl<'db> MirVisitor for CodeGenerator<'db> {
    type FunctionOutput = LLVMResult<FunctionValue<'db>>;
    fn visit_function(&mut self, function: &MirFunction) -> Self::FunctionOutput {
        self.block_builder = None;
        self.values.clear();
        self.blocks.clear();

        let builder = self
            .module
            .resume_function(self.get_function(function.id))?;

        for block in function.blocks.iter() {
            self.blocks
                .insert(block.id, builder.append_block()?.basic_block());
        }
        self.function_builder = Some(builder);

        for (Variable { id, ty }, val) in function.args.iter().zip(self.current_function().args()?)
        {
            self.values
                .insert(*id, (LLVMValue::new(val, &ty), ty.clone()));
        }

        for block in function.blocks.iter() {
            self.visit_block(&block)?;
        }

        self.function_builder
            .take()
            .expect("there should be a finished function after a function finishes")
            .finish()
    }

    type BlockOutput = LLVMResult<LLVMBasicBlock<'db>>;
    fn visit_block(&mut self, block: &BasicBlock) -> Self::BlockOutput {
        let basic_block = self.get_block(block.id);
        let building_block = self.get_function_builder().move_to_end(basic_block)?;
        self.block_builder = Some(building_block);

        for (_arg, _successors) in block.args.iter() {
            // https://docs.rs/llvm-sys/100.1.0/llvm_sys/core/fn.LLVMAddIncoming.html
        }

        for instruction in block.iter() {
            self.visit_instruction(instruction)?;
        }

        self.visit_terminator(
            block
                .terminator
                .as_ref()
                .expect("BasicBlock was missing terminator"),
        )?;

        Ok(basic_block)
    }

    type InstructionOutput = LLVMResult<Either<VarId, CallSiteValue<'db>>>;
    fn visit_instruction(&mut self, instruction: &Instruction) -> Self::InstructionOutput {
        match instruction {
            Instruction::Assign(Assign { var, val, ty }) => {
                let val = self.visit_rval(val)?;
                self.values.insert(*var, (val, ty.clone()));

                Ok(Either::Left(*var))
            }

            Instruction::Call(FnCall { function, args }) => {
                let args = args.iter().map(|arg| self.get_var_value(*arg).as_value());
                let call = self
                    .get_block_builder()
                    .call(self.get_function(*function), args)?;

                Ok(Either::Right(call))
            }
        }
    }

    type TerminatorOutput = LLVMResult<InstructionValue<'db>>;
    // FIXME: Figure out BB arg -> Phi node mapping
    fn visit_terminator(&mut self, terminator: &Terminator) -> Self::TerminatorOutput {
        match terminator {
            &Terminator::Return(ret) => self.ret(ret.map(|ret| self.get_var_value(ret))),

            Terminator::Jump(block, _args) => {
                self.get_block_builder().branch(self.get_block(*block))
            }

            Terminator::Branch {
                condition,
                truthy,
                falsy,
            } => self.get_block_builder().conditional_branch(
                self.get_var_value(*condition).as_value(),
                self.get_block(*truthy),
                self.get_block(*falsy),
            ),

            Terminator::Switch {
                condition,
                cases,
                default,
            } => {
                let jumps = cases.iter().map(
                    |SwitchCase {
                         condition, block, ..
                     }| {
                        (
                            self.get_var_value(*condition).as_value(),
                            self.get_block(*block),
                        )
                    },
                );

                self.get_block_builder().switch(
                    self.get_var_value(*condition).as_value(),
                    self.get_block(default.block),
                    jumps,
                )
            }

            Terminator::Unreachable => self.get_block_builder().unreachable(),
        }
    }

    type RvalOutput = LLVMResult<LLVMValue<'db>>;
    fn visit_rval(&mut self, Rval { ty, val }: &Rval) -> Self::RvalOutput {
        match val {
            &Value::Variable(id) => Ok(self.get_var_value(id)),
            Value::Const(constant) => self.visit_constant(constant, ty),
            &Value::Add(lhs, rhs) => self.add(self.get_var_value(lhs), self.get_var_value(rhs)),
            &Value::Sub(lhs, rhs) => self.sub(self.get_var_value(lhs), self.get_var_value(rhs)),
            &Value::Mul(lhs, rhs) => self.mul(self.get_var_value(lhs), self.get_var_value(rhs)),
            &Value::Div(lhs, rhs) => self.div(self.get_var_value(lhs), self.get_var_value(rhs)),
            &Value::Eq(lhs, rhs) => self.eq(self.get_var_value(lhs), self.get_var_value(rhs)),

            Value::Call(FnCall { function, args }) => {
                let args = args.iter().map(|arg| self.get_var_value(*arg).as_value());
                let call = self
                    .get_block_builder()
                    .call(self.get_function(*function), args)?;

                Ok(LLVMValue::Raw(call.as_value()))
            }

            // FIXME: This is so incredibly not good
            &Value::GetPointer { var, .. } => {
                crunch_shared::warn!("Value::GetPointer is the sketchiest shit alive");
                let val = self.get_var_value(var);

                Ok(val)
            }

            Value::Cast(casted, ty) => unsafe {
                Ok(LLVMValue::Raw(RawLLVMValue::from_raw(
                    llvm_sys::core::LLVMBuildIntCast(
                        self.get_block_builder().builder().as_mut_ptr(),
                        self.get_var_value(*casted).as_ptr(),
                        self.visit_type(ty)?.as_mut_ptr(),
                        EMPTY_CSTR,
                    ),
                )?))
            },
        }
    }

    type ConstantOutput = LLVMResult<LLVMValue<'db>>;
    fn visit_constant(&mut self, constant: &Constant, ty: &Type) -> Self::ConstantOutput {
        let constant = match constant {
            Constant::Integer { sign, bits } => {
                assert!(ty.is_integer());

                let llvm_type = self.visit_type(ty)?;

                #[rustfmt::skip]
                let val = match ty {
                    Type::U8  => IntType::<'db, u8>::from_ty(llvm_type).erase(),
                    Type::I8  => IntType::<'db, i8>::from_ty(llvm_type).erase(),
                    Type::U16 => IntType::<'db, u16>::from_ty(llvm_type).erase(),
                    Type::I16 => IntType::<'db, i16>::from_ty(llvm_type).erase(),
                    Type::U32 => IntType::<'db, u32>::from_ty(llvm_type).erase(),
                    Type::I32 => IntType::<'db, i32>::from_ty(llvm_type).erase(),
                    Type::U64 => IntType::<'db, u64>::from_ty(llvm_type).erase(),
                    Type::I64 => IntType::<'db, i64>::from_ty(llvm_type).erase(),

                    _ => unreachable!(),
                };

                val.constant(sign.maybe_negate(*bits) as u64, false)?.into()
            }

            Constant::Bool(boolean) => {
                assert!(ty.is_bool());

                let ty = self.visit_type(ty)?;
                let val = IntType::<'db, I1>::from_ty(ty).constant(*boolean as u64, true)?;

                val.into()
            }

            Constant::String(string) => {
                assert!(ty.is_string());

                let string = ArrayValue::const_string(self.module.context(), string, false)?;
                let string_type: ArrayType<'db> = string.as_type()?.try_into()?;
                println!(
                    "Made a constant string, LLVM typed it as {:?} while the type's value is {:?}",
                    string_type,
                    self.visit_type(ty).unwrap(),
                );

                self.module
                    .add_global(string_type, None, "")?
                    .with_initializer(string.as_value())
                    .as_value()
            }

            Constant::Array(array) => {
                assert!(ty.is_array());

                let llvm_type = self.visit_type(ty)?;
                // TODO: Assert all elements have the same type
                let elements = array
                    .iter()
                    .map(|c| {
                        self.visit_constant(c, ty.array_elements().unwrap())
                            .map(|c| c.as_value())
                    })
                    .collect::<LLVMResult<Vec<_>>>()?;

                let array = ArrayValue::const_array(llvm_type.into(), elements.as_slice())?;
                assert_eq!(llvm_type, array.as_type().unwrap());

                let array_type: ArrayType<'db> = llvm_type.try_into()?;
                self.module
                    .add_global(array_type, None, "")?
                    .with_initializer(array.as_value())
                    .as_value()
            }
        };

        Ok(LLVMValue::new(constant, ty))
    }

    type TypeOutput = LLVMResult<LLVMType<'db>>;
    #[rustfmt::skip]
    fn visit_type(&mut self, ty: &Type) -> Self::TypeOutput {
        let ty = match ty {
            Type::Bool   => IntType::i1(&self.context)?.into(),
            Type::Unit   => VoidType::new(&self.context)?.into(),
            Type::String => IntType::i8(&self.context)?.into(),
            Type::Absurd => VoidType::new(&self.context)?.into(), // TODO: ???
            Type::U8     => IntType::u8(&self.context)?.into(),
            Type::I8     => IntType::i8(&self.context)?.into(),
            Type::U16    => IntType::u16(&self.context)?.into(),
            Type::I16    => IntType::i16(&self.context)?.into(),
            Type::U32    => IntType::u32(&self.context)?.into(),
            Type::I32    => IntType::i32(&self.context)?.into(),
            Type::U64    => IntType::u64(&self.context)?.into(),
            Type::I64    => IntType::i64(&self.context)?.into(),
            Type::Slice { element } => {
                self.module.create_struct(&[self.visit_type(element)?.make_pointer(AddressSpace::Generic)?.into(), IntType::u64(&self.context)?.into()], false)?.into()
            }
            &Type::Array { ref element, length } => self.visit_type(element)?.make_array(length as u32)?.into(),
            Type::Pointer { pointee, .. } => self
                .visit_type(pointee)?
                .make_pointer(AddressSpace::Generic)?
                .into(),
            Type::Reference { referee, .. } => self
                .visit_type(referee)?
                .make_pointer(AddressSpace::Generic)?
                .into(),
        };

        Ok(ty)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum LLVMValue<'db> {
    U8(RawLLVMValue<'db>),
    I8(RawLLVMValue<'db>),
    U16(RawLLVMValue<'db>),
    I16(RawLLVMValue<'db>),
    U32(RawLLVMValue<'db>),
    I32(RawLLVMValue<'db>),
    U64(RawLLVMValue<'db>),
    I64(RawLLVMValue<'db>),
    Bool(RawLLVMValue<'db>),
    Raw(RawLLVMValue<'db>),
}

impl<'db> LLVMValue<'db> {
    pub fn new(val: RawLLVMValue<'db>, ty: &Type) -> Self {
        match ty {
            Type::U8 => Self::U8(val),
            Type::I8 => Self::I8(val),
            Type::U16 => Self::U16(val),
            Type::I16 => Self::I16(val),
            Type::U32 => Self::U32(val),
            Type::I32 => Self::I32(val),
            Type::U64 => Self::U64(val),
            Type::I64 => Self::I64(val),
            Type::Bool => Self::Bool(val),

            ty => {
                crunch_shared::warn!("Unhandled LLVM type: {:?}", ty);
                Self::Raw(val)
            }
        }
    }

    pub fn as_value(&self) -> RawLLVMValue<'db> {
        unsafe { RawLLVMValue::from_raw(self.as_ptr()).unwrap() }
    }

    pub fn as_ptr(&self) -> *mut llvm_sys::LLVMValue {
        match self {
            Self::U8(int) => int.as_mut_ptr(),
            Self::I8(int) => int.as_mut_ptr(),
            Self::U16(int) => int.as_mut_ptr(),
            Self::I16(int) => int.as_mut_ptr(),
            Self::U32(int) => int.as_mut_ptr(),
            Self::I32(int) => int.as_mut_ptr(),
            Self::U64(int) => int.as_mut_ptr(),
            Self::I64(int) => int.as_mut_ptr(),
            Self::Bool(b) => b.as_mut_ptr(),
            Self::Raw(raw) => raw.as_mut_ptr(),
        }
    }
}

impl<'db> Into<RawLLVMValue<'db>> for LLVMValue<'db> {
    fn into(self) -> RawLLVMValue<'db> {
        self.as_value()
    }
}
