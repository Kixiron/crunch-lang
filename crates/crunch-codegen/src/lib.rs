use crunch_shared::{
    strings::StrInterner,
    trees::mir::{Constant, Function as MirFunction, Instruction, Rval, Type as MirType, VarId},
    utils::HashMap,
};
use llvm::{
    module::{BuildingBlock, Linkage, Module},
    types::{IntType, Type, VoidType},
    values::{FunctionValue, Value},
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
        Rval::Var(id) => Ok(*values.get(&id).unwrap()),
        Rval::Mul(lhs, rhs) => block.mult(
            rval(module, block, values, *lhs)?,
            rval(module, block, values, *rhs)?,
            "mult",
        ),
        v => todo!("{:?}", v),
    }
}

pub fn translate<'ctx>(
    interner: &StrInterner,
    module: &'ctx Module<'ctx>,
    function: MirFunction,
) -> Result<FunctionValue<'ctx>> {
    let args: Vec<Type<'ctx>> = function
        .args
        .iter()
        .map(|(_, a)| ty(module, a).unwrap())
        .collect();

    let sig = module.function_ty(ty(module, &function.ret)?, args.as_slice(), false)?;

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

            for (i, bl) in function.blocks.into_iter().enumerate() {
                let block = builder.append_block(if i == 0 {
                    "entry".to_string()
                } else {
                    bl.id.0.to_string()
                })?;
                blocks.insert(bl.id, block.basic_block());

                for inst in bl.instructions {
                    match inst {
                        Instruction::Return(ret) => {
                            block
                                .ret(ret.map(|ret| rval(module, &block, &values, ret).unwrap()))?;
                        }
                        Instruction::Goto(bl) => {
                            block.branch(blocks.get(&bl).unwrap())?;
                        }
                        Instruction::Assign(id, _, val) => {
                            let val = rval(module, &block, &values, val)?;
                            values.insert(id, val);
                        }
                        _ => todo!(),
                    }
                }
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
        end_timer,
        files::{CurrentFile, FileId, Files},
        start_timer,
        symbol_table::Resolver,
        trees::mir::MirBuilder,
        visitors::ast::ItemVisitor,
    };
    use ladder::Ladder;
    use llvm::{
        target_machine::{CodegenFileKind, Target, TargetConf, TargetMachine},
        Context,
    };

    simple_logger::init().ok();

    let source = r#"
    fn main() -> i64
        let mut greeting: i64 := 10
        greeting *= 10

        return greeting
    end
    "#;

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
            println!("{:#?}", &mir);

            let start = start_timer!("codegen");
            let ctx = Context::new().unwrap();
            let module = ctx.module("crunch-module").unwrap();

            for func in mir {
                translate(&parse_ctx.strings(), &module, func).unwrap();
            }

            module.verify().unwrap();

            end_timer!("codegen", start);
            println!("{:?}", &module);

            let start = start_timer!("object file emission");

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
                .emit_to_file(&module, "crunch.asm", CodegenFileKind::Object)
                .unwrap();

            end_timer!("object file emission", start);
        }

        Err(mut err) => {
            err.emit(&files);
        }
    }
}
