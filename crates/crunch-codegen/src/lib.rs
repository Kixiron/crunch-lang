pub mod llvm;

use crunch_shared::{
    crunch_proc::instrument,
    strings::StrInterner,
    trees::{
        mir::{
            Assign, BasicBlock, BlockId, Constant, FnCall, FuncId, Function as MirFunction,
            Instruction, Mir, Rval, SwitchCase, Terminator, Type, Value, VarId, Variable,
        },
        CallConv,
    },
    utils::{Either, HashMap},
    visitors::mir::MirVisitor,
};
use llvm::{
    module::{BuildingBlock, FunctionBuilder, Linkage, Module},
    types::{ArrayType, IntType, SealedAnyType, Type as LLVMType, VoidType, I1},
    utils::{AddressSpace, CallingConvention, EMPTY_CSTR},
    values::{
        AnyValue, ArrayValue, BasicBlock as LLVMBasicBlock, CallSiteValue, FunctionValue,
        InstructionValue, SealedAnyValue, Value as LLVMValue,
    },
    Context, Result,
};
use std::convert::TryInto;

pub struct CodeGenerator<'ctx> {
    module: &'ctx Module<'ctx>,
    ctx: &'ctx Context,

    functions: HashMap<FuncId, FunctionValue<'ctx>>,
    blocks: HashMap<BlockId, LLVMBasicBlock<'ctx>>,
    values: HashMap<VarId, (LLVMValue<'ctx>, Type)>,

    function_builder: Option<FunctionBuilder<'ctx>>,
    block_builder: Option<BuildingBlock<'ctx>>,

    interner: &'ctx StrInterner,
}

impl<'ctx> CodeGenerator<'ctx> {
    pub fn new(module: &'ctx Module<'ctx>, interner: &'ctx StrInterner) -> Self {
        Self {
            module,
            ctx: module.context(),
            values: HashMap::new(),
            blocks: HashMap::new(),
            functions: HashMap::new(),
            function_builder: None,
            block_builder: None,
            interner,
        }
    }

    #[instrument(name = "codegen")]
    pub fn generate(mut self, mir: Mir) -> Result<()> {
        for function in mir.functions() {
            let args: Vec<LLVMType<'ctx>> = function
                .args
                .iter()
                .map(|Variable { ty, .. }| self.visit_type(ty).unwrap())
                .collect();

            let sig =
                self.module
                    .function_ty(self.visit_type(&function.ret)?, args.as_slice(), false)?;

            let function_val = self.module.create_function(
                function
                    .name
                    .as_ref()
                    .map_or_else(|| function.id.to_string(), |n| n.to_string(self.interner)),
                sig,
            )?;

            function_val.with_linkage(Linkage::External);
            self.functions.insert(function.id, function_val);
        }

        for function in mir.external_functions() {
            let args: Vec<LLVMType<'ctx>> = function
                .args
                .iter()
                .map(|Variable { ty, .. }| self.visit_type(ty).unwrap())
                .collect();

            let sig =
                self.module
                    .function_ty(self.visit_type(&function.ret)?, args.as_slice(), false)?;

            let function_val = self
                .module
                .create_function(function.name.to_string(self.interner), sig)?;

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

    fn current_function(&self) -> FunctionValue<'ctx> {
        self.function_builder
            .as_ref()
            .expect("called current_function outside of a function")
            .function()
    }

    fn get_function_builder(&self) -> &FunctionBuilder<'ctx> {
        self.function_builder
            .as_ref()
            .expect("Called CodeGenerator::get_function_builder outside of a function")
    }

    fn get_block_builder(&self) -> &BuildingBlock<'ctx> {
        self.block_builder
            .as_ref()
            .expect("called CodeGenerator::get_builder outside of a basic block")
    }

    fn get_function(&self, function: FuncId) -> FunctionValue<'ctx> {
        *self
            .functions
            .get(&function)
            .expect("attempted to get a function that doesn't exist")
    }

    fn get_block(&self, block: BlockId) -> LLVMBasicBlock<'ctx> {
        *self
            .blocks
            .get(&block)
            .expect("attempted to get a basic block that doesn't exist")
    }

    fn get_var(&self, id: VarId) -> (LLVMValue<'ctx>, &Type) {
        let (val, ty) = self
            .values
            .get(&id)
            .expect("attempted to get a var that doesn't exist");

        (*val, ty)
    }

    fn get_var_value(&self, id: VarId) -> LLVMValue<'ctx> {
        self.get_var(id).0
    }
}

impl<'ctx> MirVisitor for CodeGenerator<'ctx> {
    type FunctionOutput = Result<FunctionValue<'ctx>>;
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
            self.values.insert(*id, (val, ty.clone()));
        }

        for block in function.blocks.iter() {
            self.visit_block(&block)?;
        }

        self.function_builder
            .take()
            .expect("there should be a finished function after a function finishes")
            .finish()
    }

    type BlockOutput = Result<LLVMBasicBlock<'ctx>>;
    fn visit_block(&mut self, block: &BasicBlock) -> Self::BlockOutput {
        let basic_block = self.get_block(block.id);
        let building_block = self.get_function_builder().move_to_end(basic_block)?;
        self.block_builder = Some(building_block);

        for instruction in block.iter() {
            self.visit_instruction(instruction)?;
        }

        self.visit_terminator(block.terminator.as_ref().unwrap())?;

        Ok(basic_block)
    }

    type InstructionOutput = Result<Either<VarId, CallSiteValue<'ctx>>>;
    fn visit_instruction(&mut self, instruction: &Instruction) -> Self::InstructionOutput {
        match instruction {
            Instruction::Assign(Assign { var, val, ty }) => {
                let val = self.visit_rval(val)?;
                self.values.insert(*var, (val, ty.clone()));

                Ok(Either::Left(*var))
            }

            Instruction::Call(FnCall { function, args }) => {
                let args = args.iter().map(|arg| self.get_var_value(*arg));
                let call = self
                    .get_block_builder()
                    .call(self.get_function(*function), args)?;

                Ok(Either::Right(call))
            }
        }
    }

    type TerminatorOutput = Result<InstructionValue<'ctx>>;
    // FIXME: Figure out BB arg -> Phi node mapping
    fn visit_terminator(&mut self, terminator: &Terminator) -> Self::TerminatorOutput {
        match terminator {
            Terminator::Return(ret) => {
                // If the return value is `None` then it's assumed to return void
                self.get_block_builder()
                    .ret(ret.map(|ret| self.get_var_value(ret)))
            }
            Terminator::Jump(block) => self.get_block_builder().branch(self.get_block(*block)),
            Terminator::Branch {
                condition,
                truthy,
                falsy,
            } => self.get_block_builder().conditional_branch(
                self.get_var_value(*condition),
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
                        (self.get_var_value(*condition), self.get_block(*block))
                    },
                );

                self.get_block_builder().switch(
                    self.get_var_value(*condition),
                    self.get_block(default.block),
                    jumps,
                )
            }
            Terminator::Unreachable => self.get_block_builder().unreachable(),
        }
    }

    type RvalOutput = Result<LLVMValue<'ctx>>;
    fn visit_rval(&mut self, Rval { ty, val }: &Rval) -> Self::RvalOutput {
        match val {
            Value::Variable(id) => Ok(self.get_var_value(*id)),
            Value::Const(constant) => self.visit_constant(constant, ty),
            Value::Add(lhs, rhs) => self
                .get_block_builder()
                .add(self.get_var_value(*lhs), self.get_var_value(*rhs)),
            Value::Sub(lhs, rhs) => self
                .get_block_builder()
                .sub(self.get_var_value(*lhs), self.get_var_value(*rhs)),
            Value::Mul(lhs, rhs) => self
                .get_block_builder()
                .mul(self.get_var_value(*lhs), self.get_var_value(*rhs)),

            Value::Div(lhs, rhs) => {
                let ((lhs, lhs_type), (rhs, rhs_type)) = (self.get_var(*lhs), self.get_var(*rhs));
                assert_eq!(lhs_type, rhs_type);
                assert!(lhs_type.is_integer());

                let div = if lhs_type.is_unsigned() {
                    BuildingBlock::unsigned_div
                } else if lhs_type.is_signed() {
                    BuildingBlock::signed_div
                } else {
                    todo!("{:?}", lhs)
                };

                div(self.get_block_builder(), lhs, rhs)
            }

            Value::Call(FnCall { function, args }) => {
                let args = args.iter().map(|arg| self.get_var_value(*arg));
                let call = self
                    .get_block_builder()
                    .call(self.get_function(*function), args)?
                    .into();

                Ok(call)
            }

            // FIXME: This is so incredibly not good
            Value::GetPointer { var, .. } => {
                crunch_shared::warn!("Value::GetPointer is the sketchiest shit alive");
                let val = self.get_var_value(*var);

                Ok(val)
            }

            Value::Cast(casted, ty) => unsafe {
                LLVMValue::from_raw(llvm_sys::core::LLVMBuildIntCast(
                    self.get_block_builder().builder().as_mut_ptr(),
                    self.get_var_value(*casted).as_mut_ptr(),
                    self.visit_type(ty)?.as_mut_ptr(),
                    EMPTY_CSTR,
                ))
            },
        }
    }

    type ConstantOutput = Result<LLVMValue<'ctx>>;
    fn visit_constant(&mut self, constant: &Constant, ty: &Type) -> Self::ConstantOutput {
        let constant = match constant {
            Constant::Integer { sign, bits } => {
                assert!(ty.is_integer());

                let llvm_type = self.visit_type(ty)?;

                #[rustfmt::skip]
                let val = match ty {
                    Type::U8  => IntType::<'ctx, u8>::from_ty(llvm_type).erase(),
                    Type::I8  => IntType::<'ctx, i8>::from_ty(llvm_type).erase(),
                    Type::U16 => IntType::<'ctx, u16>::from_ty(llvm_type).erase(),
                    Type::I16 => IntType::<'ctx, i16>::from_ty(llvm_type).erase(),
                    Type::U32 => IntType::<'ctx, u32>::from_ty(llvm_type).erase(),
                    Type::I32 => IntType::<'ctx, i32>::from_ty(llvm_type).erase(),
                    Type::U64 => IntType::<'ctx, u64>::from_ty(llvm_type).erase(),
                    Type::I64 => IntType::<'ctx, i64>::from_ty(llvm_type).erase(),

                    _ => unreachable!(),
                };

                val.constant(sign.maybe_negate(*bits) as u64, false)?.into()
            }

            Constant::Bool(boolean) => {
                assert!(ty.is_bool());

                let ty = self.visit_type(ty)?;
                let val = IntType::<'ctx, I1>::from_ty(ty).constant(*boolean as u64, true)?;

                val.into()
            }

            Constant::String(string) => {
                assert!(ty.is_string());

                let string = ArrayValue::const_string(self.module.context(), string, false)?;
                let string_type: ArrayType<'ctx> = string.as_type()?.try_into()?;
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
                    .map(|c| self.visit_constant(c, ty.array_elements().unwrap()))
                    .collect::<Result<Vec<_>>>()?;

                let array = ArrayValue::const_array(llvm_type.into(), elements.as_slice())?;
                assert_eq!(llvm_type, array.as_type().unwrap());

                let array_type: ArrayType<'ctx> = llvm_type.try_into()?;
                self.module
                    .add_global(array_type, None, "")?
                    .with_initializer(array.as_value())
                    .as_value()
            }
        };

        Ok(constant)
    }

    type TypeOutput = Result<LLVMType<'ctx>>;
    #[rustfmt::skip]
    fn visit_type(&mut self, ty: &Type) -> Self::TypeOutput {
        let ty = match ty {
            Type::Bool   => IntType::i1(self.ctx)?.into(),
            Type::Unit   => VoidType::new(self.ctx)?.into(),
            Type::String => IntType::i8(self.ctx)?.into(),
            Type::Absurd => VoidType::new(self.ctx)?.into(), // TODO: ???
            Type::U8     => IntType::u8(self.ctx)?.into(),
            Type::I8     => IntType::i8(self.ctx)?.into(),
            Type::U16    => IntType::u16(self.ctx)?.into(),
            Type::I16    => IntType::i16(self.ctx)?.into(),
            Type::U32    => IntType::u32(self.ctx)?.into(),
            Type::I32    => IntType::i32(self.ctx)?.into(),
            Type::U64    => IntType::u64(self.ctx)?.into(),
            Type::I64    => IntType::i64(self.ctx)?.into(),
            Type::Slice(elem) => {
                self.module.create_struct(&[self.visit_type(elem)?.make_pointer(AddressSpace::Generic)?.into(), IntType::u64(self.ctx)?.into()], false)?.into()
            }
            Type::Array(elem, len) => self.visit_type(&**elem)?.make_array(*len)?.into(),
            Type::Pointer(ptr) => self
                .visit_type(ptr)?
                .make_pointer(AddressSpace::Generic)?
                .into(),
            Type::Reference(_, ty) => self
                .visit_type(ty)?
                .make_pointer(AddressSpace::Generic)?
                .into(),
        };

        Ok(ty)
    }
}

#[test]
fn mir() {
    use crunch_parser::{ExternUnnester, Parser};
    use crunch_shared::{
        context::Context as ParseContext,
        files::{CurrentFile, FileId, Files},
        symbol_table::Resolver,
        trees::mir::MirBuilder,
        utils::Timer,
        visitors::ast::ItemVisitor,
    };
    use ladder::Ladder;
    use llvm::{
        target_machine::{CodegenFileKind, Target, TargetConf, TargetMachine},
        Context,
    };
    use std::fs::File;

    simple_logger::init().ok();

    let source = r#"
    extern
        @callconv("C")
        fn puts(string: *const i8) -> i32;
    end

    fn main() -> i32
        return puts("Hello, world!")
    end
    "#;

    let compilation = Timer::start("compilation");

    let parse_ctx = ParseContext::default();
    let mut files = Files::new();
    files.add("<test>", source);

    match Parser::new(
        source,
        CurrentFile::new(FileId::new(0), source.len()),
        parse_ctx.clone(),
    )
    .parse()
    {
        Ok((items, mut warnings)) => {
            warnings.emit(&files);

            let items = ExternUnnester::new().unnest(items);

            let mut resolver = Resolver::new(vec![parse_ctx.strings().intern("<test>")].into());
            for item in items.iter() {
                resolver.visit_item(item);
            }
            resolver.finalize();

            let ladder = Ladder::new().lower(&items);
            let mir = MirBuilder::new().lower(&ladder).unwrap();

            let ctx = Context::new().unwrap();
            let module = ctx.module("crunch-module").unwrap();

            CodeGenerator::new(&module, &parse_ctx.strings)
                .generate(mir)
                .unwrap();

            module.verify().unwrap();

            let object_emission = Timer::start("object file emission");

            Target::init_native(TargetConf::all()).unwrap();
            let target = Target::from_triple("x86_64-pc-windows-msvc").unwrap();
            let target_machine = TargetMachine::new(
                &target,
                "x86_64-pc-windows-msvc",
                None,
                None,
                None,
                None,
                None,
            )
            .unwrap();

            target_machine
                .emit_to_file(&module, "crunch.o", CodegenFileKind::Object)
                .unwrap();

            object_emission.end();

            let linking = Timer::start("linking");

            // TODO: Use `cc` to get the relevant linkers
            std::process::Command::new("clang")
                .args(&["crunch.o", "-o", "crunch.exe"])
                .spawn()
                .unwrap()
                .wait()
                .unwrap();

            linking.end();
            compilation.end();

            let runtime = Timer::start("running executable");
            let command = std::process::Command::new("crunch.exe")
                .spawn()
                .unwrap()
                .wait_with_output()
                .unwrap();
            runtime.end();

            target_machine
                .emit_to_file(&module, "crunch.s", CodegenFileKind::Assembly)
                .unwrap();

            let llvm_ir = format!("{:?}", module)
                .trim()
                .lines()
                .map(|l| "    ".to_string() + l)
                .collect::<Vec<String>>()
                .join("\n");
            let assembly = std::fs::read_to_string("crunch.s")
                .unwrap()
                .trim()
                .lines()
                .map(|l| "    ".to_string() + l)
                .collect::<Vec<String>>()
                .join("\n");

            let source_len = source.as_bytes().len();
            let object_file_len = File::open("crunch.o").unwrap().metadata().unwrap().len();
            let executable_len = File::open("crunch.exe").unwrap().metadata().unwrap().len();

            println!(
                "Source Code:{}\n\n\
                 LLVM IR:\n{}\n\n\
                 Assembly:\n{}\n\n\
                 File Sizes:\n    \
                     Source: {:>4} bytes\n    \
                     Object: {:>4} bytes\n    \
                     Binary: {:>4} bytes\n\n\
                 Exited with code {:?}",
                source,
                llvm_ir,
                assembly,
                source_len,
                object_file_len,
                executable_len,
                command.status.code(),
            );
        }

        Err(mut err) => {
            err.emit(&files);
        }
    }
}
