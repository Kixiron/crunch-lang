use compactor::Instruction;
use crunch_error::compile_prelude::*;
use crunch_parser::{
    ast::Visibility,
    string_interner::{StringInterner, Sym},
};
use std::collections::{HashMap, HashSet};

mod block;
mod function;
mod passes;
mod ty;

pub use block::*;
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
    // TODO: `into_interner` method
    pub interner: StringInterner<Sym>,
    gc_ids: HashSet<u32>,
    local_symbols: HashMap<Sym, u32>,
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
            func_index: 1,
            last_jump_id: 0,
            current_namespace: None,
        }
    }

    pub fn into_interner(self) -> StringInterner<Sym> {
        self.interner
    }

    pub fn build_function<F>(&mut self, name: Sym, function: F) -> CompileResult<()>
    where
        F: FnOnce(&mut Self, &mut FunctionContext) -> CompileResult<()>,
    {
        let mut context = FunctionContext::new(name);

        (function)(self, &mut context)?;

        self.functions.insert(name, (context, None));

        Ok(())
    }

    pub fn build_type<T>(&mut self, name: Sym, ty: T) -> CompileResult<()>
    where
        T: FnOnce(&mut Self, &mut TypeContext) -> CompileResult<()>,
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
    pub fn next_jump_id(&mut self) -> u32 {
        self.last_jump_id += 1;
        self.last_jump_id
    }

    pub fn build(mut self) -> CompileResult<Vec<Vec<Instruction>>> {
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

impl Default for CodeBuilder {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use compactor::{Compactor, Value};

    #[test]
    fn codebuilder_test() {
        let mut builder = CodeBuilder::new();

        let main = builder.intern("main");
        builder
            .build_function(main, |builder, ctx| {
                let mut block = Block::new();
                block
                    .inst_load(0, Value::Str("Hello from the main function!\n"))
                    .inst_print(0);
                ctx.inst_drop_block(0, ctx.current_block)
                    .inst_func_call(builder.intern("test"))
                    .inst_load(1, Value::Str("Hello from the main function again!\n"))
                    .inst_print(1);
                ctx.inst_drop_block(1, ctx.current_block).inst_return();

                ctx.push_block(block);

                Ok(())
            })
            .unwrap();

        let test = builder.intern("test");
        builder
            .build_function(test, |_builder, ctx| {
                ctx.current_block()
                    .inst_load(0, Value::Str("Hello from the test function!\n"))
                    .inst_print(0);
                ctx.inst_drop_block(0, ctx.current_block).inst_return();

                Ok(())
            })
            .unwrap();

        let functions = builder.build().unwrap();

        let mut stdout = std::io::stdout();
        Compactor::with_stdout(Box::new(&mut stdout))
            .execute(&functions)
            .unwrap();
    }
}
