use crunch_shared::{
    strings::StrInterner,
    trees::mir::{
        Block, BlockId, Constant, Function as MirFunction, Instruction, Rval, Type as MirType,
        VarId,
    },
    utils::HashMap,
};
use llvm::{
    module::{BuildingBlock, FunctionBuilder, Linkage, Module},
    types::{IntType, Type, VoidType},
    values::{BasicBlock, FunctionValue, SealedAnyValue, Value},
    Result,
};

pub mod llvm;

fn ty<'ctx>(module: &'ctx Module<'ctx>, ty: &MirType) -> Result<Type<'ctx>> {
    match ty {
        MirType::I64 => IntType::i64(module.context()).map(|i| i.into()),
        MirType::Unit => VoidType::new(module.context()).map(|i| i.into()),
        _ => todo!(),
    }
}

fn rval<'ctx>(
    module: &'ctx Module<'ctx>,
    block: &BuildingBlock<'ctx>,
    values: &HashMap<VarId, Value<'ctx>>,
    val: Rval,
) -> Result<Value<'ctx>> {
    match val {
        Rval::Const(Constant::I64(i)) => IntType::i64(module.context())?
            .constant(i as u64, false)
            .map(|i| i.into()),
        Rval::Const(Constant::Bool(b)) => IntType::i1(module.context())?
            .constant(b as u64, false)
            .map(|i| i.into()),
        Rval::Var(id) => Ok(*values.get(&id).unwrap()),
        Rval::Mul(lhs, rhs) => block.mult(
            rval(module, block, values, *lhs)?,
            rval(module, block, values, *rhs)?,
            "mult",
        ),
        v => todo!("{:?}", v),
    }
}

fn block<'ctx>(
    module: &'ctx Module<'ctx>,
    builder: &'ctx FunctionBuilder<'ctx>,
    raw_blocks: &mut Vec<Option<Block>>,
    blocks: &mut HashMap<BlockId, BasicBlock<'ctx>>,
    values: &mut HashMap<VarId, Value<'ctx>>,
    name: String,
    bl: Block,
) -> Result<()> {
    let block = builder.append_block(name)?;

    blocks.insert(bl.id, block.basic_block());

    for inst in bl.instructions {
        match inst {
            Instruction::Return(ret) => {
                block.ret(ret.map(|ret| rval(module, &block, &values, ret).unwrap()))?;
            }
            Instruction::Goto(bl) => {
                block.branch(blocks.get(&bl).unwrap())?;
            }
            Instruction::Assign(id, _, val) => {
                let val = rval(module, &block, &values, val)?;
                values.insert(id, val);
            }
            Instruction::Switch(cond, branches) => {
                let cond = rval(module, &block, &values, cond)?;
                let br1 = branches[0].clone();

                let br1 = match blocks.get(&br1.1) {
                    Some(b) => *b,
                    None => {
                        let b = raw_blocks[(br1.1).0].clone().unwrap();
                        crate::block(
                            module,
                            builder,
                            raw_blocks,
                            blocks,
                            values,
                            (br1.1).0.to_string(),
                            b,
                        )?;

                        *blocks.get(&br1.1).unwrap()
                    }
                };

                let br2 = branches[1].clone();
                let br2 = match blocks.get(&br2.1) {
                    Some(b) => *b,
                    None => {
                        let b = raw_blocks[(br2.1).0].clone().unwrap();

                        crate::block(
                            module,
                            builder,
                            raw_blocks,
                            blocks,
                            values,
                            (br2.1).0.to_string(),
                            b,
                        )?;

                        *blocks.get(&br2.1).unwrap()
                    }
                };

                unsafe {
                    llvm_sys::core::LLVMBuildCondBr(
                        block.builder().as_mut_ptr(),
                        cond.as_mut_ptr(),
                        br1.as_mut_ptr(),
                        br2.as_mut_ptr(),
                    );
                }
            }
            _ => todo!(),
        }
    }

    Ok(())
}

pub fn translate<'ctx>(
    interner: &StrInterner,
    module: &'ctx Module<'ctx>,
    mut function: MirFunction,
) -> Result<FunctionValue<'ctx>> {
    let args: Vec<Type<'ctx>> = function
        .args
        .iter()
        .map(|(_, a)| ty(module, a).unwrap())
        .collect();

    let sig = module.function_ty(ty(module, &function.ret)?, args.as_slice(), false)?;
    let mut raw_blocks: Vec<_> = function
        .blocks
        .drain(..)
        .filter_map(|b| if !b.is_empty() { Some(Some(b)) } else { None })
        .collect();

    module.build_function(
        function
            .name
            .as_ref()
            .map_or_else(|| function.id.0.to_string(), |n| n.to_string(interner)),
        sig,
        |builder| {
            builder.with_linkage(Linkage::External);

            let mut blocks = HashMap::new();
            let mut values = HashMap::new();

            while let Some(bl) = raw_blocks.iter_mut().find_map(|b| b.take()) {
                block(
                    &module,
                    &builder,
                    &mut raw_blocks,
                    &mut blocks,
                    &mut values,
                    bl.id.0.to_string(),
                    bl,
                )?;
            }

            Ok(())
        },
    )
}

#[test]
fn mir() {
    use crunch_parser::Parser;
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
    fn main() -> i64
        let mut greeting: i64 := 10
        greeting *= 10

        if false
            return greeting
        else
            return 0
        end
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

            let mut resolver = Resolver::new(vec![parse_ctx.strings().intern("<test>")].into());
            for item in items.iter() {
                resolver.visit_item(item);
            }
            resolver.finalize();

            let ladder = Ladder::new().lower(&items);
            let mir = MirBuilder::new().lower(&ladder);

            let codegen = Timer::start("codegen");
            let ctx = Context::new().unwrap();
            let module = ctx.module("crunch-module").unwrap();

            for func in dbg!(mir) {
                translate(&parse_ctx.strings(), &module, func).unwrap();
            }

            module.verify().unwrap();
            codegen.end();

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

            std::process::Command::new("lld-link")
                .args(&["/ENTRY:main", "crunch.o"])
                .spawn()
                .unwrap()
                .wait()
                .unwrap();

            linking.end();
            compilation.end();

            let exit_code = std::process::Command::new("crunch.exe")
                .spawn()
                .unwrap()
                .wait_with_output()
                .unwrap()
                .status;
            println!(
                "LLVM IR:\n{:?}\n\nExited with code {:?}\nFile Sizes:\n  Source: {:>4} bytes\n  Object: {:>4} bytes\n  Binary: {:>4} bytes",
                module,
                exit_code.code(),
                source.as_bytes().len(),
                File::open("crunch.o")
                    .unwrap()
                    .metadata()
                    .unwrap()
                    .len(),
                File::open("crunch.exe")
                    .unwrap()
                    .metadata()
                    .unwrap()
                    .len(),
            );
        }

        Err(mut err) => {
            err.emit(&files);
        }
    }
}
