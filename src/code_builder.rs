#![allow(dead_code)]

use crate::{
    Instruction, Register, Result, RuntimeError, RuntimeErrorTy, RuntimeValue, NUMBER_REGISTERS,
};
use rand::{
    distributions::{Alphanumeric, Distribution, Standard},
    rngs::SmallRng,
    Rng, SeedableRng,
};
use std::collections::{HashMap, HashSet};
use string_interner::{StringInterner, Sym};

pub trait Ident: Into<String> + AsRef<str> {}
impl<T: Into<String> + AsRef<str>> Ident for T {}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum MangleStatus {
    Function,
    Global,
    Local,
    Type,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Visibility {
    Namespace,
    Private,
    Public,
}

#[derive(Debug, Clone)]
pub struct Namespace {
    functions: HashMap<Sym, (Visibility, FunctionContext)>,
    types: HashMap<Sym, (Visibility, TypeContext)>,
}

#[derive(Debug, Clone)]
pub struct CodeBuilder {
    functions: HashMap<Sym, (FunctionContext, Option<u32>)>,
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
            interner,
            gc_ids: HashSet::new(),
            local_symbols: HashMap::new(),
            rng: SmallRng::from_entropy(),
            func_index: 1,
            last_jump_id: 0,
            current_namespace: None,
        }
    }

    pub fn function<F>(&mut self, name: Sym, function: F) -> Result<()>
    where
        F: FnOnce(&mut Self, &mut FunctionContext) -> Result<()>,
    {
        let mut context = FunctionContext::new();

        (function)(self, &mut context)?;

        self.functions.insert(name, (context, None));

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

    pub fn mangle(&mut self, path: &[&str], status: MangleStatus) -> String {
        let status_prefix = match status {
            MangleStatus::Function => "function",
            MangleStatus::Global => "global",
            MangleStatus::Local => "local",
            MangleStatus::Type => "type",
        };

        let path = path
            .iter()
            .map(|segment| format!("{}{}", segment.len(), segment))
            .collect::<Vec<_>>()
            .join("");

        let rand_len = self.rng.gen_range(5, 15);

        format!(
            "__{}_{}__{}",
            status_prefix,
            path,
            Alphanumeric
                .sample_iter(&mut self.rng)
                .take(rand_len)
                .collect::<String>()
        )
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

            if func[func.len() - 1] != Instruction::Return {
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

#[derive(Debug, Clone)]
pub struct Scope {}

impl Scope {
    #[inline]
    pub const fn new() -> Self {
        Self {}
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    Int,
    Float,
    String,
    Bool,
    Void,
    Infer,
    Any,
    Custom(Sym),
}

#[derive(Debug, Clone)]
pub struct TypeContext {
    name: Sym,
    members: HashMap<Sym, Type>,
    methods: HashMap<Sym, (Visibility, FunctionContext)>,
}

#[derive(Debug, Clone)]
pub struct FunctionContext {
    registers: [Option<Option<Sym>>; NUMBER_REGISTERS],
    variables: HashSet<Sym>,
    block: Vec<PartialInstruction>,
    pub scope: Scope,
}

impl FunctionContext {
    #[inline]
    pub fn new() -> Self {
        Self {
            registers: [None; NUMBER_REGISTERS],
            variables: HashSet::new(),
            block: Vec::new(),
            scope: Scope::new(),
        }
    }

    #[inline]
    pub fn free_reg(&mut self, reg: impl Into<Register>) -> &mut Self {
        let reg = reg.into();
        trace!("Freeing register {}", reg.0);

        self.registers[*reg as usize] = None;

        self
    }

    #[inline]
    pub fn reserve_caller_reg(&mut self, sym: impl Into<Option<Sym>>) -> Result<Register> {
        if let Some(idx) = self.registers.iter().take(5).position(Option::is_none) {
            let sym = sym.into();
            trace!("Reserving register {} for {:?}", idx, sym);

            self.registers[idx] = Some(sym);
            Ok((idx as u8).into())
        } else {
            error!("Failed to find avaliable register");
            Err(RuntimeError {
                ty: RuntimeErrorTy::CompilationError,
                message: "Failed to fetch available register".to_string(),
            })
        }
    }

    #[inline]
    pub fn reserve_reg(&mut self, sym: impl Into<Option<Sym>>) -> Result<Register> {
        if let Some((idx, _)) = self
            .registers
            .iter()
            .enumerate()
            .rev()
            .find(|(_idx, r)| r.is_none())
        {
            let sym = sym.into();
            trace!("Reserving register {} for {:?}", idx, sym);

            self.registers[idx] = Some(sym);
            Ok((idx as u8).into())
        } else {
            error!("Failed to find avaliable register");
            Err(RuntimeError {
                ty: RuntimeErrorTy::CompilationError,
                message: "Failed to fetch available register".to_string(),
            })
        }
    }

    pub fn get_cached_reg(&mut self, sym: Sym) -> Result<Register> {
        match self.registers.iter().position(|r| *r == Some(Some(sym))) {
            Some(pos) => Ok((pos as u8).into()),
            None => Err(RuntimeError {
                ty: RuntimeErrorTy::CompilationError,
                message: "Failed to fetch cached register".to_string(),
            }),
        }
    }

    #[inline]
    pub fn add_var(&mut self, sym: Sym) {
        self.variables.insert(sym);
    }

    pub fn build(self, builder: &mut CodeBuilder) -> Result<Vec<Instruction>> {
        let mut instructions = Vec::with_capacity(self.block.len());
        for inst in self.block {
            instructions.push(inst.solidify(builder)?);
        }

        let mut jumps: HashMap<u32, u32> = HashMap::new(); // JumpId, JumpIndex
        for _ in 0..2 {
            for (index, instruction) in instructions.iter_mut().enumerate() {
                match instruction {
                    Instruction::Jump(id) | Instruction::JumpComp(id) => {
                        if let Some(loc) = jumps.get(&(*id as u32)) {
                            *id = *loc as i32 - index as i32;
                        }
                    }
                    Instruction::JumpPoint(id) => {
                        jumps.insert(*id, index as u32);
                    }

                    _ => {}
                }
            }
        }

        Ok(instructions)
    }

    pub fn inst_load(&mut self, register: impl Into<Register>, value: RuntimeValue) -> &mut Self {
        self.block
            .push(Instruction::Load(value, register.into()).into());

        self
    }
    pub fn inst_mov(
        &mut self,
        target: impl Into<Register>,
        source: impl Into<Register>,
    ) -> &mut Self {
        self.block
            .push(Instruction::Move(target.into(), source.into()).into());

        self
    }

    pub fn inst_comp_to_reg(&mut self, register: impl Into<Register>) -> &mut Self {
        self.block
            .push(Instruction::CompToReg(register.into()).into());

        self
    }
    pub fn inst_op_to_reg(&mut self, register: impl Into<Register>) -> &mut Self {
        self.block
            .push(Instruction::OpToReg(register.into()).into());

        self
    }

    pub fn inst_drop(&mut self, register: impl Into<Register>) -> &mut Self {
        let register = register.into();

        self.block.push(Instruction::Drop(register).into());
        self.free_reg(register);

        self
    }

    pub fn inst_add(&mut self, left: impl Into<Register>, right: impl Into<Register>) -> &mut Self {
        self.block
            .push(Instruction::Add(left.into(), right.into()).into());

        self
    }
    pub fn inst_sub(&mut self, left: impl Into<Register>, right: impl Into<Register>) -> &mut Self {
        self.block
            .push(Instruction::Sub(left.into(), right.into()).into());

        self
    }
    pub fn inst_mult(
        &mut self,
        left: impl Into<Register>,
        right: impl Into<Register>,
    ) -> &mut Self {
        self.block
            .push(Instruction::Mult(left.into(), right.into()).into());

        self
    }
    pub fn inst_div(&mut self, left: impl Into<Register>, right: impl Into<Register>) -> &mut Self {
        self.block
            .push(Instruction::Div(left.into(), right.into()).into());

        self
    }

    pub fn inst_print(&mut self, register: impl Into<Register>) -> &mut Self {
        self.block.push(Instruction::Print(register.into()).into());

        self
    }

    pub fn inst_jump(&mut self, id: u32) -> &mut Self {
        self.block.push(Instruction::Jump(id as i32).into());

        self
    }
    pub fn inst_jump_comp(&mut self, id: u32) -> &mut Self {
        self.block.push(Instruction::JumpComp(id as i32).into());

        self
    }

    pub fn inst_jump_point(&mut self, id: u32) -> &mut Self {
        self.block.push(Instruction::JumpPoint(id).into());

        self
    }

    pub fn inst_and(&mut self, left: impl Into<Register>, right: impl Into<Register>) -> &mut Self {
        self.block
            .push(Instruction::And(left.into(), right.into()).into());

        self
    }
    pub fn inst_or(&mut self, left: impl Into<Register>, right: impl Into<Register>) -> &mut Self {
        self.block
            .push(Instruction::Or(left.into(), right.into()).into());

        self
    }
    pub fn inst_xor(&mut self, left: impl Into<Register>, right: impl Into<Register>) -> &mut Self {
        self.block
            .push(Instruction::Xor(left.into(), right.into()).into());

        self
    }
    pub fn inst_not(&mut self, register: impl Into<Register>) -> &mut Self {
        self.block.push(Instruction::Not(register.into()).into());

        self
    }

    pub fn inst_eq(&mut self, left: impl Into<Register>, right: impl Into<Register>) -> &mut Self {
        self.block
            .push(Instruction::Eq(left.into(), right.into()).into());

        self
    }
    pub fn inst_not_eq(
        &mut self,
        left: impl Into<Register>,
        right: impl Into<Register>,
    ) -> &mut Self {
        self.block
            .push(Instruction::NotEq(left.into(), right.into()).into());

        self
    }
    pub fn inst_greater_than(
        &mut self,
        left: impl Into<Register>,
        right: impl Into<Register>,
    ) -> &mut Self {
        self.block
            .push(Instruction::GreaterThan(left.into(), right.into()).into());

        self
    }
    pub fn inst_less_than(
        &mut self,
        left: impl Into<Register>,
        right: impl Into<Register>,
    ) -> &mut Self {
        self.block
            .push(Instruction::LessThan(left.into(), right.into()).into());

        self
    }

    pub fn inst_collect(&mut self) -> &mut Self {
        self.block.push(Instruction::Collect.into());

        self
    }
    pub fn inst_return(&mut self) -> &mut Self {
        self.block.push(Instruction::Return.into());

        self
    }
    pub fn inst_func_call(&mut self, func_name: Sym) -> &mut Self {
        self.block.push(PartialInstruction {
            uninit_inst: Instruction::Func(0),
            func_sym: Some(func_name),
            global_sym: None,
            local_sym: None,
        });

        self
    }
    pub fn inst_halt(&mut self) -> &mut Self {
        self.block.push(Instruction::Halt.into());

        self
    }
    pub fn inst_noop(&mut self) -> &mut Self {
        self.block.push(Instruction::NoOp.into());

        self
    }
    pub fn inst_illegal(&mut self) -> &mut Self {
        self.block.push(Instruction::Illegal.into());

        self
    }
}

#[derive(Clone, Debug)]
struct PartialInstruction {
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
            .function(main, |builder, ctx| {
                ctx.inst_load(0, RuntimeValue::Str("Hello from the main function!\n"))
                    .inst_print(0)
                    .inst_drop(0)
                    .inst_func_call(builder.intern("test"))
                    .inst_load(
                        1,
                        RuntimeValue::Str("Hello from the main function again!\n"),
                    )
                    .inst_print(1)
                    .inst_drop(1)
                    .inst_return();

                Ok(())
            })
            .unwrap();

        let test = builder.intern("test");
        builder
            .function(test, |_builder, ctx| {
                ctx.inst_load(0, RuntimeValue::Str("Hello from the test function!\n"))
                    .inst_print(0)
                    .inst_drop(0)
                    .inst_return();

                Ok(())
            })
            .unwrap();

        let functions = builder.build().unwrap();

        Crunch::new(
            OptionBuilder::new("./codebuilder_test")
                .debug_log(true)
                .build(),
        )
        .execute(functions)
        .unwrap();
    }
}
