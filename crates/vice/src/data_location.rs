use crate::code_builder::FunctionContext;

use compactor::bytecode;
use crunch_error::compile_prelude::*;
use crunch_parser::string_interner::Sym;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum DataLocation {
    Register(u8),
    Comparison,
    Operation,
}

impl DataLocation {
    pub fn to_register(
        self,
        ctx: &mut FunctionContext,
        target: impl Into<Option<Sym>>,
    ) -> CompileResult<u8> {
        match self {
            Self::Register(reg) => {
                if let Some(target) = target.into() {
                    let target = ctx.reserve_reg(target)?;
                    bytecode!(@append ctx.current_block() => {
                        copy reg, target;
                    });

                    Ok(target)
                } else {
                    Ok(reg)
                }
            }

            Self::Comparison => {
                let reg = ctx.reserve_reg(target)?;
                bytecode!(@append ctx.current_block() => {
                    cmpr reg;
                });

                Ok(reg)
            }

            Self::Operation => {
                let reg = ctx.reserve_reg(target)?;
                bytecode!(@append ctx.current_block() => {
                    opr reg;
                });

                Ok(reg)
            }
        }
    }

    pub fn is_register(self) -> bool {
        if let Self::Register(_) = self {
            true
        } else {
            false
        }
    }

    pub fn is_comparison(self) -> bool {
        if let Self::Comparison = self {
            true
        } else {
            false
        }
    }

    pub fn is_operation(self) -> bool {
        if let Self::Operation = self {
            true
        } else {
            false
        }
    }
}
