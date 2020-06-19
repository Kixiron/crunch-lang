use crunch_shared::{
    trees::mir::{
        Constant, Function as MirFunction, Instruction, Intrinsic, Rval, Type as MirType, VarId,
    },
    utils::HashMap,
};
use llvm::{
    module::{BuildingBlock, Linkage, Module},
    types::{AnyType, IntType, Type, VoidType},
    utils::{AddressSpace, CallingConvention, UNNAMED},
    values::{AnyValue, ArrayValue, FunctionValue, Global, PointerValue, SealedAnyValue, Value},
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
        Rval::Const(Constant::I64(i)) => dbg!(IntType::i64(module.context())?
            .constant(i as u64, false)
            .map(|i| i.into())),
        Rval::Var(id) => Ok(*values.get(&id).unwrap()),
        Rval::Intrinsic(Intrinsic::Printf, args) => unsafe {
            dbg!(&args);
            let iee_ate = IntType::i8(module.context())?;
            let iee_32 = IntType::i32(module.context())?;

            let printf = module
                .get_or_create_function("printf", |module| {
                    module.function_ty(
                        iee_32.into(),
                        &[iee_ate.make_pointer(AddressSpace::Generic)?.into()],
                        true,
                    )
                })?
                .with_linkage(Linkage::External)
                .with_calling_convention(CallingConvention::C);

            let fmt = b"Test %i\0";
            let string =
                module.add_global(iee_ate.make_array(fmt.len() as u32)?, None, "printf_fmt")?;

            string.set_initializer(ArrayValue::const_string(module.context(), fmt, true)?);
            let string = block.ptr_cast(
                string.as_value(),
                iee_ate.make_pointer(AddressSpace::Global)?.0,
                "printfmt_fmt_casted",
            )?;

            let args: Vec<Value<'ctx>> = std::iter::once(string)
                .chain(
                    args.iter()
                        .map(|arg| rval(module, block, values, arg.clone()).unwrap()),
                )
                .collect();

            Ok(block.call(printf, &args, "hope_this_works")?.as_value())
        },
        _ => todo!(),
    }
}

pub fn translate<'ctx>(
    module: &'ctx Module<'ctx>,
    function: MirFunction,
) -> Result<FunctionValue<'ctx>> {
    let args: Vec<Type<'ctx>> = function
        .args
        .iter()
        .map(|(_, a)| ty(module, a).unwrap())
        .collect();

    let sig = module.function_ty(ty(module, &function.ret)?, args.as_slice(), false)?;

    module.build_function(function.id.0.to_string(), sig, |builder| {
        let mut blocks = HashMap::new();
        let mut values = HashMap::new();

        for bl in function.blocks {
            let block = builder.append_block(bl.id.0.to_string())?;
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
                    _ => todo!(),
                }
            }
        }

        Ok(())
    })
}

#[test]
fn mir() {
    use crunch_parser::Parser;
    use crunch_shared::{
        context::Context as ParseContext,
        files::{CurrentFile, FileId, Files},
        symbol_table::Resolver,
        trees::mir::MirBuilder,
        visitors::ast::ItemVisitor,
    };
    use ladder::Ladder;
    use llvm::Context;

    let source = r#"
    fn main()
        let greeting: i64 := 10
        printf(greeting)
        return
    end
    "#;

    let ctx = ParseContext::default();
    let mut files = Files::new();
    files.add("<test>", source);

    match Parser::new(
        source,
        CurrentFile::new(FileId::new(0), source.len()),
        ctx.clone(),
    )
    .parse()
    {
        Ok((items, mut warnings)) => {
            warnings.emit(&files);

            let mut resolver = Resolver::new(vec![ctx.strings.intern("<test>")].into());
            for item in items.iter() {
                resolver.visit_item(item);
            }
            resolver.finalize();
            let ladder = Ladder::new().lower(&items);
            let mir = MirBuilder::new().lower(&ladder);
            println!("{:?}", &mir);

            let ctx = Context::new().unwrap();
            unsafe { llvm_sys::error_handling::LLVMEnablePrettyStackTrace() };
            let module = ctx.module("crunch-module").unwrap();

            for func in mir {
                translate(&module, func).unwrap();
            }

            println!("{:?}", &module);
            unsafe {
                assert_eq!(
                    llvm_sys::bit_writer::LLVMWriteBitcodeToFile(
                        module.as_mut_ptr(),
                        b"output.o".as_ptr() as *const i8,
                    ),
                    0,
                );
            }
        }

        Err(mut err) => {
            err.emit(&files);
        }
    }
}
