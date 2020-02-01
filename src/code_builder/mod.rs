#![allow(dead_code)]

use crate::{
    Instruction, Register, Result, RuntimeError, RuntimeErrorTy, RuntimeValue, Type, Visibility,
    NUMBER_REGISTERS,
};
use rand::{
    distributions::{Alphanumeric, Distribution, Standard},
    rngs::SmallRng,
    Rng, SeedableRng,
};
use std::collections::{HashMap, HashSet};
use string_interner::{StringInterner, Sym};

mod block;
mod flow_graph_analyzer;
mod function;
mod ty;

pub use block::*;
pub use flow_graph_analyzer::*;
pub use function::*;
pub use ty::*;

pub trait Ident: Into<String> + AsRef<str> {}
impl<T: Into<String> + AsRef<str>> Ident for T {}

#[derive(Debug, Clone)]
pub struct Namespace {
    functions: HashMap<Sym, (Visibility, FunctionContext)>,
    types: HashMap<Sym, (Visibility, TypeContext)>,
}

#[derive(Debug, Clone)]
pub struct CodeBuilder {
    functions: HashMap<Sym, (FunctionContext, Option<u32>)>,
    types: HashMap<Sym, TypeContext>,
    pub interner: StringInterner<Sym>,
    gc_ids: HashSet<u32>,
    local_symbols: HashMap<Sym, u32>,
    rng: SmallRng,
    func_index: u32,
    last_jump_id: u32,
    current_namespace: Option<Sym>,
}

impl CodeBuilder {
    pub fn new() -> Self {
        Self {
            functions: HashMap::new(),
            types: HashMap::new(),
            interner: StringInterner::new(),
            gc_ids: HashSet::new(),
            local_symbols: HashMap::new(),
            rng: SmallRng::from_entropy(),
            func_index: 1,
            last_jump_id: 0,
            current_namespace: None,
        }
    }

    pub fn from_interner(interner: StringInterner<Sym>) -> Self {
        Self {
            functions: HashMap::new(),
            types: HashMap::new(),
            interner,
            gc_ids: HashSet::new(),
            local_symbols: HashMap::new(),
            rng: SmallRng::from_entropy(),
            func_index: 1,
            last_jump_id: 0,
            current_namespace: None,
        }
    }

    pub fn build_function<F>(&mut self, name: Sym, function: F) -> Result<()>
    where
        F: FnOnce(&mut Self, &mut FunctionContext) -> Result<()>,
    {
        let mut context = FunctionContext::new();

        (function)(self, &mut context)?;

        self.functions.insert(name, (context, None));

        Ok(())
    }

    pub fn build_type<T>(&mut self, name: Sym, ty: T) -> Result<()>
    where
        T: FnOnce(&mut Self, &mut TypeContext) -> Result<()>,
    {
        let mut context = TypeContext::new(name);

        (ty)(self, &mut context)?;

        self.types.insert(name, context);

        Ok(())
    }

    #[inline]
    pub fn intern<T>(&mut self, string: T) -> Sym
    where
        T: Into<String> + AsRef<str>,
    {
        self.interner.get_or_intern(string)
    }

    pub fn solidify_id(&mut self, old_id: u32) -> u32 {
        let mut id = old_id;
        loop {
            if self.gc_ids.get(&id).is_none() {
                self.gc_ids.insert(id);
                break;
            }

            id += 1;
        }

        id
    }

    #[inline]
    pub fn gen_clobber_str(&mut self, len: usize) -> String {
        Alphanumeric.sample_iter(&mut self.rng).take(len).collect()
    }

    #[inline]
    pub fn gen_rand<T>(&mut self) -> T
    where
        Standard: Distribution<T>,
    {
        self.rng.gen::<T>()
    }

    #[inline]
    pub fn next_jump_id(&mut self) -> u32 {
        self.last_jump_id += 1;
        self.last_jump_id
    }

    pub fn build(mut self) -> Result<Vec<Vec<Instruction>>> {
        let mut functions = Vec::new();

        for (sym, (func, _index)) in self.functions.clone() {
            let mut func = func.build(&mut self)?;

            if func.iter().last() != Some(&Instruction::Return) {
                func.push(Instruction::Return);
            }

            if let Some(ident) = self.interner.resolve(sym) {
                if ident == "main" {
                    functions.insert(0, func);
                } else {
                    functions.push(func);
                }
            } else {
                error!("Unresolved function name: {:?}", sym);
            }
        }

        Ok(functions)
    }
}

#[derive(Clone, Debug)]
pub struct PartialInstruction {
    uninit_inst: Instruction,
    func_sym: Option<Sym>,
    global_sym: Option<Sym>,
    local_sym: Option<Sym>,
}

impl PartialInstruction {
    pub fn solidify(self, builder: &mut CodeBuilder) -> Result<Instruction> {
        match self.uninit_inst {
            Instruction::Func(_) => {
                let (_instructions, func_index) = if let Some(entry) = builder.functions.get(
                    &self
                        .func_sym
                        .expect("Should have a func_sym for a function instruction"),
                ) {
                    entry
                } else {
                    error!(
                        "Failed to find the function {:?} ({:?})",
                        self.func_sym.unwrap(),
                        builder.interner.resolve(self.func_sym.unwrap()).unwrap()
                    );
                    return Err(RuntimeError {
                        ty: RuntimeErrorTy::MissingSymbol,
                        message:
                            "A malformed function instruction was encountered during compilation"
                                .to_string(),
                    });
                };

                if let Some(func_index) = func_index {
                    Ok(Instruction::Func(*func_index))
                } else {
                    let func_index = if let Some(name) = builder.interner.resolve(
                        self.func_sym
                            .expect("Should have a func_sym for a function instruction"),
                    ) {
                        if name == "main" {
                            0
                        } else {
                            builder.func_index += 1;
                            builder.func_index - 1
                        }
                    } else {
                        builder.func_index += 1;
                        builder.func_index - 1
                    };

                    let entry = builder
                        .functions
                        .get_mut(
                            &self
                                .func_sym
                                .expect("Should have a func_sym for a function instruction"),
                        )
                        .expect("The check has already been preformed");

                    entry.1 = Some(func_index);

                    Ok(Instruction::Func(func_index))
                }
            }

            _ => Ok(self.uninit_inst),
        }
    }
}

impl From<Instruction> for PartialInstruction {
    fn from(inst: Instruction) -> Self {
        Self {
            uninit_inst: inst,
            func_sym: None,
            global_sym: None,
            local_sym: None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Crunch, OptionBuilder};

    #[test]
    fn codebuilder_test() {
        let mut builder = CodeBuilder::new();

        let main = builder.intern("main");
        builder
            .build_function(main, |builder, ctx| {
                let mut block = Block::new();
                block
                    .inst_load(0, RuntimeValue::Str("Hello from the main function!\n"))
                    .inst_print(0)
                    .inst_drop(0, ctx)
                    .inst_func_call(builder.intern("test"))
                    .inst_load(
                        1,
                        RuntimeValue::Str("Hello from the main function again!\n"),
                    )
                    .inst_print(1)
                    .inst_drop(1, ctx)
                    .inst_return();

                ctx.push_block(block);

                Ok(())
            })
            .unwrap();

        let test = builder.intern("test");
        builder
            .build_function(test, |_builder, ctx| {
                let mut block = Block::new();
                block
                    .inst_load(0, RuntimeValue::Str("Hello from the test function!\n"))
                    .inst_print(0)
                    .inst_drop(0, ctx)
                    .inst_return();

                ctx.push_block(block);

                Ok(())
            })
            .unwrap();

        let functions = builder.build().unwrap();

        Crunch::new(
            OptionBuilder::new("./codebuilder_test")
                .debug_log(true)
                .build(),
        )
        .execute(&functions)
        .unwrap();
    }
}
