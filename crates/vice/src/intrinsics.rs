use crate::code_builder::{CodeBuilder, FunctionContext};

use compactor::bytecode;
use crunch_error::compile_prelude::CompileResult;

// TODO: Roll own perfect hash map
pub static INTRINSICS: phf::Map<
    &'static str,
    fn(&mut CodeBuilder, &mut FunctionContext) -> CompileResult<()>,
> = phf::phf_map! {
    "print" => |_builder, ctx| {
        let reg = ctx.reserve_reg(None)?;

        bytecode!(@append ctx.current_block() => {
            pop reg;
            print reg;
        });

        Ok(())
    },
    "println" => |_builder, ctx| {
        let reg = ctx.reserve_reg(None)?;
        let newline = ctx.reserve_reg(None)?;

        bytecode!(@append ctx.current_block() => {
            pop reg;
            print reg;
            load "\n", newline;
            print newline;
            drop newline;
        });

        Ok(())
    },
    "halt" => |_builder, ctx| {
        bytecode!(@append ctx.current_block() => {
            halt;
        });

        Ok(())
    },
    "range" => |builder, _ctx| {
        let range = builder.intern("range");
        builder.build_function(range, |_builder, ctx| {
            let val = ctx.reserve_reg(None)?;
            let max = ctx.reserve_reg(None)?;
            let null = ctx.reserve_reg(None)?;
            let increment = ctx.reserve_reg(None)?;

            ctx.add_block();
            let generator_block = ctx.current_block;
            bytecode!(@append ctx.current_block() => {
                load 1i32, increment;
                pop val;
                pop max;
                add increment, val;
                opr val;
                less val, max;
            });

            ctx.add_block();
            let success = ctx.current_block;
            bytecode!(@append ctx.current_block() => {
                push max;
                push val;
                yield;
                jump generator_block as i32;
            });

            ctx.add_block();
            let fail = ctx.current_block;
            bytecode!(@append ctx.current_block() => {
                load null, null;
                push max;
                push null;
                yield;
                jump fail as i32;
            });

            bytecode!(@append ctx.get_block(generator_block) => {
                jumpcmp success as i32;
                jump fail as i32;
            });

            Ok(())
        })?;

        Ok(())
    },
};
