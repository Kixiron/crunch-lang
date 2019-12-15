#![allow(dead_code)]

use crate::{
    Instruction, Register, Result, RuntimeError, RuntimeErrorTy, RuntimeValue, NUMBER_REGISTERS,
};
use rand::{
    distributions::{Alphanumeric, Distribution, Standard},
    rngs::SmallRng,
    Rng, SeedableRng,
};
use std::{
    collections::{HashMap, HashSet},
    fmt,
};
use string_interner::{StringInterner, Sym};

pub trait Ident: Into<String> + AsRef<str> {}
impl<T: Into<String> + AsRef<str>> Ident for T {}

#[derive(Debug, Clone)]
pub struct CodeBuilder {
    functions: HashMap<Sym, (Vec<Instruction>, Option<u32>)>,
    interner: StringInterner<Sym>,
    gc_ids: HashSet<u32>,
    local_symbols: HashMap<Sym, u32>,
    rng: SmallRng,
}

impl CodeBuilder {
    pub fn new() -> Self {
        Self {
            functions: HashMap::new(),
            interner: StringInterner::new(),
            gc_ids: HashSet::new(),
            local_symbols: HashMap::new(),
            rng: SmallRng::from_entropy(),
        }
    }

    pub fn function<N, F>(&mut self, name: N, mut function: F) -> Result<()>
    where
        N: Ident,
        F: FnMut(&mut Self, &mut FunctionContext) -> Result<()>,
    {
        let name = self.interner.get_or_intern(name);
        let mut context = FunctionContext::new();

        (function)(self, &mut context)?;

        let built_function = context.build(self);

        self.functions.insert(name, (built_function, None));

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

    pub fn build(self) -> (Vec<Instruction>, Vec<Vec<Instruction>>) {
        let mut main = Vec::new();
        let mut functions = Vec::new();

        for (sym, (mut func, _index)) in self.functions.clone().into_iter() {
            if let Some(ident) = self.interner.resolve(sym) {
                if ident == "main" {
                    if func[func.len() - 1] != Instruction::Halt {
                        func.push(Instruction::Halt);
                    }
                    main = func;
                    continue;
                }
            }

            functions.push(func);
        }

        (main, functions)
    }
}

#[derive(Debug, Clone)]
pub struct Scope {}

impl Scope {
    #[inline]
    pub fn new() -> Self {
        Self {}
    }
}

// pub struct VariableMeta<'a> {
//     ty: Type<'a>,
// }

#[derive(Clone)]
pub struct FunctionContext {
    registers: [Option<Option<Sym>>; NUMBER_REGISTERS],
    pub variables: HashSet<Sym>,
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
        self.registers[*reg.into() as usize] = None;

        self
    }

    #[inline]
    pub fn reserve_reg(&mut self) -> Result<Register> {
        match self.registers.iter().position(|r| r.is_none()) {
            Some(pos) => {
                self.registers[pos] = Some(None);
                Ok((pos as u8).into())
            }
            None => Err(RuntimeError {
                ty: RuntimeErrorTy::CompilationError,
                message: "Failed to fetch available register".to_string(),
            }),
        }
    }

    #[inline]
    pub fn reserve_reg_sym(&mut self, sym: Sym) -> Result<Register> {
        match self.registers.iter().position(|r| r.is_none()) {
            Some(pos) => {
                self.registers[pos] = Some(Some(sym));
                Ok((pos as u8).into())
            }
            None => Err(RuntimeError {
                ty: RuntimeErrorTy::CompilationError,
                message: "Failed to fetch available register".to_string(),
            }),
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

    pub fn build(self, builder: &mut CodeBuilder) -> Vec<Instruction> {
        let mut instructions = Vec::with_capacity(self.block.len());
        for inst in self.block {
            instructions.push(inst.solidify(builder));
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

        instructions
            .into_iter()
            .map(|i| {
                if let Instruction::JumpPoint(_) = i {
                    Instruction::NoOp
                } else {
                    i
                }
            })
            .collect()
    }

    pub fn inst_load(&mut self, register: impl Into<Register>, value: RuntimeValue) -> &mut Self {
        self.block
            .push(Instruction::Load(value, register.into()).into());

        self
    }
    pub fn inst_cache(
        &mut self,
        register: impl Into<Register>,
        value: RuntimeValue,
        name: Sym,
    ) -> Result<&mut Self> {
        let gc_id = &value as *const _ as u32;

        self.block.push(PartialInstruction {
            uninit_inst: Instruction::Cache(gc_id, value, register.into()),
            func_sym: None,
            global_sym: None,
            local_sym: Some(name),
        });

        self.reserve_reg_sym(name)?;

        Ok(self)
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

    pub fn inst_drop_reg(&mut self, register: impl Into<Register>) -> &mut Self {
        let register = register.into();

        self.block.push(Instruction::DropReg(register).into());
        self.free_reg(register);

        self
    }
    pub fn inst_drop(&mut self, name: Sym) -> Result<&mut Self> {
        self.block.push(PartialInstruction {
            uninit_inst: Instruction::Drop(0).into(),
            func_sym: None,
            global_sym: None,
            local_sym: Some(name),
        });

        let register = self.get_cached_reg(name)?;
        self.free_reg(register);

        Ok(self)
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
    pub fn inst_halt(&mut self) -> &mut Self {
        self.block.push(Instruction::Halt.into());

        self
    }
    pub fn inst_illegal(&mut self) -> &mut Self {
        self.block.push(Instruction::Illegal.into());

        self
    }
}

impl fmt::Debug for FunctionContext {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("FunctionContext")
            .field("block", &self.block)
            .field(
                "registers",
                &("[".to_string()
                    + &self
                        .registers
                        .iter()
                        .map(|v| format!("{:?}", v))
                        .collect::<Vec<String>>()
                        .join(", ")
                    + "]"),
            )
            .field("variables", &self.variables)
            .field("block", &self.block)
            .finish()
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
    pub fn solidify(self, builder: &mut CodeBuilder) -> Instruction {
        match self.uninit_inst {
            Instruction::Cache(start_id, value, register) => {
                let concrete_id = builder.solidify_id(start_id);

                builder
                    .local_symbols
                    .insert(self.local_sym.expect("Expected Local Symbol"), concrete_id);

                Instruction::Cache(concrete_id, value, register)
            }

            Instruction::Drop(_) => Instruction::Drop(
                *builder
                    .local_symbols
                    .get(&self.local_sym.expect("Expected Local Symbol"))
                    .expect("No Registered Local Symbol"),
            ),

            _ => self.uninit_inst,
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
        color_backtrace::install();

        let mut builder = CodeBuilder::new();

        builder
            .function("main", |_builder, ctx| {
                ctx.inst_load(0, RuntimeValue::Str("Hello World!\n"))
                    .inst_load(1, RuntimeValue::I32(10))
                    .inst_load(2, RuntimeValue::I32(0))
                    .inst_load(3, RuntimeValue::I32(1))
                    .inst_jump_point(77)
                    .inst_print(0)
                    .inst_sub(1, 3)
                    .inst_op_to_reg(1)
                    .inst_greater_than(1, 2)
                    .inst_jump_comp(77)
                    .inst_halt();

                Ok(())
            })
            .unwrap();

        let (main, functions) = builder.build();
        let crunch = (
            main,
            functions,
            OptionBuilder::new("./codebuilder_test")
                .debug_log(true)
                .build(),
        );

        Crunch::from(crunch).execute();
    }
}
