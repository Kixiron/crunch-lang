pub mod llvm;

use crunch_shared::{
    crunch_proc::instrument,
    strings::StrInterner,
    trees::{
        mir::{
            Block, BlockId, Constant, FuncId, Function as MirFunction, Instruction, Mir,
            RightValue, Rval, SwitchArm, SwitchArmKind, Type as MirType, VarId,
        },
        CallConv,
    },
    utils::{Either, HashMap},
    visitors::mir::MirVisitor,
};
use llvm::{
    module::{BuildingBlock, FunctionBuilder, Linkage, Module},
    types::{AnyType, IntType, Type, VoidType},
    utils::{AddressSpace, CallingConvention},
    values::{AnyValue, ArrayValue, BasicBlock, FunctionValue, InstructionValue, Value},
    Context, Result,
};

pub struct CodeGenerator<'ctx> {
    module: &'ctx Module<'ctx>,
    ctx: &'ctx Context,
    values: HashMap<VarId, Value<'ctx>>,
    blocks: HashMap<BlockId, BasicBlock<'ctx>>,
    functions: HashMap<FuncId, FunctionValue<'ctx>>,
    function_builder: Option<FunctionBuilder<'ctx>>,
    block_builder: Option<BuildingBlock<'ctx>>,
    interner: &'ctx StrInterner,
    raw_blocks: Vec<Option<Block>>,
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
            raw_blocks: Vec::new(),
        }
    }

    #[instrument(name = "codegen")]
    pub fn generate(mut self, mir: Mir) -> Result<()> {
        self.functions.reserve(mir.len());
        for function in mir.iter() {
            match function {
                Either::Left(function) => {
                    let args: Vec<Type<'ctx>> = function
                        .args
                        .iter()
                        .map(|(_, arg)| self.visit_type(arg).unwrap())
                        .collect();

                    let sig = self.module.function_ty(
                        self.visit_type(&function.ret)?,
                        args.as_slice(),
                        false,
                    )?;

                    let function_val = self.module.create_function(
                        function.name.as_ref().map_or_else(
                            || function.id.0.to_string(),
                            |n| n.to_string(self.interner),
                        ),
                        sig,
                    )?;

                    function_val.with_linkage(Linkage::External);
                    self.functions.insert(function.id, function_val);
                }

                Either::Right(extern_func) => {
                    let args: Vec<Type<'ctx>> = extern_func
                        .args
                        .iter()
                        .map(|(_, arg)| self.visit_type(arg).unwrap())
                        .collect();

                    let sig = self.module.function_ty(
                        self.visit_type(&extern_func.ret)?,
                        args.as_slice(),
                        false,
                    )?;

                    let function_val = self.module.create_function(
                        extern_func.name.as_ref().map_or_else(
                            || extern_func.id.0.to_string(),
                            |n| n.to_string(self.interner),
                        ),
                        sig,
                    )?;

                    function_val
                        .with_linkage(Linkage::External)
                        .with_calling_convention(match extern_func.callconv {
                            CallConv::C => CallingConvention::C,
                            CallConv::Crunch => CallingConvention::X86RegCall,
                        });
                    self.functions.insert(extern_func.id, function_val);
                }
            }
        }

        for function in mir.iter().filter_map(|func| {
            if let Either::Left(func) = func {
                Some(func)
            } else {
                None
            }
        }) {
            self.visit_function(function)?;
        }

        Ok(())
    }

    fn value(&self, id: &VarId) -> Value<'ctx> {
        *self
            .values
            .get(id)
            .expect("attempted to get a value that doesn't exist")
    }

    fn function(&self, id: &FuncId) -> FunctionValue<'ctx> {
        *self
            .functions
            .get(id)
            .expect("attempted to get a function that doesn't exist")
    }

    fn next_block(&mut self) -> Result<BasicBlock<'ctx>> {
        let block_builder = self
            .function_builder
            .as_ref()
            .expect("called next_block outside of a function")
            .append_block()?;
        let basic_block = block_builder.basic_block();
        self.block_builder = Some(block_builder);

        Ok(basic_block)
    }

    fn move_to_block(&mut self, block: BasicBlock<'ctx>) -> Result<()> {
        self.block_builder = Some(
            self.function_builder
                .as_ref()
                .expect("called move_to_block outside of a function")
                .move_to_end(block)?,
        );

        Ok(())
    }

    fn current_block(&self) -> BasicBlock<'ctx> {
        self.block_builder
            .as_ref()
            .expect("called current_block outside of a basic block")
            .basic_block()
    }

    fn current_function(&self) -> FunctionValue<'ctx> {
        self.function_builder
            .as_ref()
            .expect("called current_function outside of a function")
            .function()
    }
}

impl<'ctx> MirVisitor for CodeGenerator<'ctx> {
    type FunctionOutput = Result<FunctionValue<'ctx>>;
    type BlockOutput = Result<BasicBlock<'ctx>>;
    type InstructionOutput = Result<Option<InstructionValue<'ctx>>>;
    type RvalOutput = Result<Value<'ctx>>;
    type ConstantOutput = Result<Value<'ctx>>;
    type TypeOutput = Result<Type<'ctx>>;

    fn visit_function(&mut self, function: &MirFunction) -> Self::FunctionOutput {
        self.values.clear();
        self.blocks.clear();
        self.block_builder = None;

        // TODO: Preload blocks like functions
        self.raw_blocks = function
            .blocks
            .clone()
            .into_iter()
            .filter_map(|b| if !b.is_empty() { Some(Some(b)) } else { None })
            .collect();
        self.function_builder = Some(self.module.resume_function(self.function(&function.id))?);

        for ((arg, _), val) in function.args.iter().zip(self.current_function().args()?) {
            self.values.insert(*arg, val);
        }

        while let Some(block) = self.raw_blocks.iter_mut().find_map(|b| b.take()) {
            self.visit_block(&block)?;
        }

        self.function_builder
            .take()
            .expect("there should be a finished function after a function finishes")
            .finish()
    }

    fn visit_block(&mut self, block: &Block) -> Self::BlockOutput {
        let basic_block = self.next_block()?;
        assert!(self.blocks.insert(block.id, basic_block).is_none());

        for instruction in block.iter() {
            self.visit_instruction(instruction)?;
        }

        Ok(basic_block)
    }

    fn visit_instruction(&mut self, instruction: &Instruction) -> Self::InstructionOutput {
        match instruction {
            Instruction::Return(ret) => {
                // If the return value is `None` then it's interpreted as returning void
                let return_value = if let Some(ret) = ret {
                    Some(self.visit_rval(ret)?)
                } else {
                    None
                };

                self.block_builder
                    .as_ref()
                    .expect("instructions should be in a block")
                    .ret(return_value)
                    .map(Some)
            }

            Instruction::Goto(bl) => self
                .block_builder
                .as_ref()
                .expect("instructions should be in a block")
                .branch(self.blocks.get(&bl).unwrap())
                .map(Some),

            Instruction::Assign(id, val) => {
                let val = self.visit_rval(val)?;
                self.values.insert(*id, val);

                Ok(None)
            }

            Instruction::Switch(cond, branches) => {
                let current_block = self.current_block();
                let cond = self.visit_rval(cond)?;

                if branches.len() == 2 {
                    let true_branch = branches[0].clone();
                    let true_branch = match self.blocks.get(&true_branch.block) {
                        Some(block) => *block,
                        None => {
                            let block = self.raw_blocks[true_branch.block.0].take().unwrap();
                            self.visit_block(&block)?
                        }
                    };

                    let false_branch = branches[1].clone();
                    let false_branch = match self.blocks.get(&false_branch.block) {
                        Some(block) => *block,
                        None => {
                            let block = self.raw_blocks[false_branch.block.0].take().unwrap();
                            self.visit_block(&block)?
                        }
                    };

                    self.move_to_block(current_block)?;
                    let instruction = self
                        .block_builder
                        .as_ref()
                        .expect("instructions should be in a block")
                        .conditional_branch(cond, true_branch, false_branch)
                        .map(Some);

                    instruction
                } else {
                    let mut default = None;
                    let mut jumps = Vec::with_capacity(branches.len() - 1);

                    for SwitchArm { kind, block } in branches.iter() {
                        let block = match self.blocks.get(&block) {
                            Some(block) => *block,
                            None => {
                                let block = self.raw_blocks[block.0].take().unwrap();
                                self.visit_block(&block)?
                            }
                        };

                        self.move_to_block(current_block)?;
                        match kind {
                            SwitchArmKind::Default(_default_val) => {
                                default = Some(block);
                            }
                            SwitchArmKind::Rval(val) => {
                                let val = self.visit_rval(val)?;
                                jumps.push((val, block));
                            }
                        }
                    }

                    self.move_to_block(current_block)?;
                    Ok(Some(
                        self.block_builder
                            .as_ref()
                            .expect("instructions should be in a block")
                            .switch(cond, default.unwrap(), &jumps)?
                            .into(),
                    ))
                }
            }
        }
    }

    fn visit_rval(&mut self, RightValue { ty: _ty, val }: &RightValue) -> Self::RvalOutput {
        match val {
            Rval::Const(constant) => self.visit_constant(constant),
            Rval::Var(id) => Ok(self.value(id)),

            Rval::Add(lhs, rhs) => {
                let (lhs, rhs) = (self.visit_rval(lhs)?, self.visit_rval(rhs)?);
                self.block_builder
                    .as_ref()
                    .expect("rvals should be inside blocks")
                    .add(lhs, rhs)
            }

            Rval::Sub(lhs, rhs) => {
                let (lhs, rhs) = (self.visit_rval(lhs)?, self.visit_rval(rhs)?);
                self.block_builder
                    .as_ref()
                    .expect("rvals should be inside blocks")
                    .sub(lhs, rhs)
            }

            Rval::Mul(lhs, rhs) => {
                let (lhs, rhs) = (self.visit_rval(lhs)?, self.visit_rval(rhs)?);
                self.block_builder
                    .as_ref()
                    .expect("rvals should be inside blocks")
                    .mult(lhs, rhs)
            }

            Rval::Div(lhs, rhs) => {
                let div = if lhs.is_float() {
                    assert!(rhs.is_float());

                    BuildingBlock::float_div
                } else if lhs.is_unsigned() {
                    assert!(rhs.is_unsigned());

                    BuildingBlock::unsigned_div
                } else if lhs.is_signed() {
                    assert!(rhs.is_signed());

                    BuildingBlock::signed_div
                } else {
                    todo!("{:?}", lhs)
                };

                let (lhs, rhs) = (self.visit_rval(lhs)?, self.visit_rval(rhs)?);
                div(
                    self.block_builder
                        .as_ref()
                        .expect("rvals should be inside blocks"),
                    lhs,
                    rhs,
                )
            }

            Rval::Call(function, args) => {
                let args = args
                    .iter()
                    .map(|arg| self.visit_rval(arg))
                    .collect::<Result<Vec<Value<'ctx>>>>()?;

                self.block_builder
                    .as_ref()
                    .expect("rvals should be inside blocks")
                    .call(self.function(function), &args)
                    .map(|f| f.into())
            }

            Rval::Phi(_, _) => todo!(),
        }
    }

    fn visit_constant(&mut self, constant: &Constant) -> Self::ConstantOutput {
        match constant {
            Constant::I64(int) => IntType::i64(self.ctx)?
                .constant(*int as u64, true)
                .map(|i| i.into()),

            Constant::U8(int) => IntType::u8(self.ctx)?
                .constant(*int as u64, true)
                .map(|i| i.into()),

            Constant::Bool(boolean) => IntType::i1(self.ctx)?
                .constant(*boolean as u64, true)
                .map(|i| i.into()),

            Constant::String(string) => {
                let llvm_string = ArrayValue::const_string(self.module.context(), string, false)?;

                Ok(self
                    .module
                    .add_global(
                        IntType::i8(self.module.context())?.make_array(string.len() as u32)?,
                        None,
                        "",
                    )?
                    .with_initializer(llvm_string.as_value())
                    .as_value())
            }
        }
    }

    fn visit_type(&mut self, ty: &MirType) -> Self::TypeOutput {
        match ty {
            MirType::I64 => IntType::i64(self.ctx).map(|i| i.into()),
            MirType::U8 => IntType::u8(self.ctx).map(|i| i.into()),
            MirType::Bool => IntType::i1(self.ctx).map(|i| i.into()),
            MirType::Unit => VoidType::new(self.ctx).map(|i| i.into()),
            MirType::Pointer(ptr) => self
                .visit_type(ptr)?
                .make_pointer(AddressSpace::Generic)
                .map(|i| i.into()),
            MirType::String => IntType::i8(self.ctx).map(|i| i.into()),
        }
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
