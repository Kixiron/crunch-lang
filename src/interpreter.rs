use crate::{
    code_builder::{Block, CodeBuilder, FunctionContext},
    instruction::Result,
    instruction::{RuntimeError, RuntimeErrorTy},
    parser::*,
    Instruction, Options, Register, RuntimeValue,
};
use std::{collections::HashMap, path::PathBuf};
use string_interner::{StringInterner, Sym};

type IntrinsicsMap =
    HashMap<&'static str, fn(&mut CodeBuilder, &mut FunctionContext, &mut Block) -> Result<()>>;

lazy_static::lazy_static! {
    static ref INTRINSICS: IntrinsicsMap = {
        let mut intrinsics: IntrinsicsMap = HashMap::with_capacity(10);

        // TODO: Think through this better
        // TODO: Replace the unwraps with actual errors

        intrinsics.insert("print", |_builder, ctx, block| {
            let reg = ctx.reserve_reg(None)?;
            block.inst_pop(reg).inst_print(reg);

            Ok(())
        });
        intrinsics.insert("println", |_builder, ctx, block| {
            let reg = ctx.reserve_reg(None)?;
            let newline = ctx.reserve_reg(None)?;

            block.inst_pop(reg)
                .inst_print(reg)
                .inst_load(newline, RuntimeValue::Str("\n"))
                .inst_print(newline)
                .inst_drop(newline, ctx);

            Ok(())
        });
        intrinsics.insert("halt", |_builder, _ctx, block| {
            block.inst_halt();

            Ok(())
        });

        intrinsics
    };
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum DataLocation {
    Register(Register),
    Comparison,
    Operation,
}

impl DataLocation {
    pub fn to_register(
        self,
        ctx: &mut FunctionContext,
        target: impl Into<Option<Sym>>,
        block: &mut Block,
    ) -> Result<Register> {
        match self {
            Self::Register(reg) => Ok(reg),
            Self::Comparison => {
                let reg = ctx.reserve_reg(target)?;
                block.inst_comp_to_reg(reg);
                Ok(reg)
            }
            Self::Operation => {
                let reg = ctx.reserve_reg(target)?;
                block.inst_op_to_reg(reg);
                Ok(reg)
            }
        }
    }

    pub fn is_register(&self) -> bool {
        if let Self::Register(_) = self {
            true
        } else {
            false
        }
    }

    pub fn is_comparison(&self) -> bool {
        if let Self::Comparison = self {
            true
        } else {
            false
        }
    }

    pub fn is_operation(&self) -> bool {
        if let Self::Operation = self {
            true
        } else {
            false
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct InterpOptions {
    pub fault_tolerant: bool,
}

impl From<&Options> for InterpOptions {
    fn from(options: &Options) -> Self {
        Self {
            fault_tolerant: options.fault_tolerant,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Interpreter {
    pub gc: u32,
    pub functions: HashMap<Sym, (Vec<Instruction>, Option<usize>)>,
    pub current_function: Vec<Instruction>,
    pub options: InterpOptions,
    pub func_index: usize,
    pub builder: CodeBuilder,
}

impl Interpreter {
    #[allow(dead_code)]
    pub fn new(options: &Options) -> Self {
        Self {
            gc: 0,
            functions: HashMap::new(),
            current_function: Vec::new(),
            options: InterpOptions::from(options),
            func_index: 0,
            builder: CodeBuilder::new(),
        }
    }

    pub fn from_interner(options: &Options, interner: StringInterner<Sym>) -> Self {
        Self {
            gc: 0,
            functions: HashMap::new(),
            current_function: Vec::new(),
            options: InterpOptions::from(options),
            func_index: 0,
            builder: CodeBuilder::from_interner(interner),
        }
    }

    /// Interpret the contained ast and return the instructions
    pub fn interpret<'a>(mut self, ast: Vec<Program>) -> Result<Vec<Vec<Instruction>>> {
        self.interpret_module(ast)?;
        let functions = self.builder.build()?;

        trace!("Interp Output: {:?}", functions);

        Ok(functions)
    }

    fn interpret_module<'a>(&mut self, mut ast: Vec<Program>) -> Result<()> {
        while let Some(node) = ast.pop() {
            match node {
                Program::FunctionDecl(func) => {
                    // Interpret the function
                    let (name, index) = self.interp_func(func)?;
                    if self.current_function.iter().last() != Some(&Instruction::Return) {
                        self.current_function.push(Instruction::Return);
                    }

                    // Will contain the newly created function
                    let mut func = Vec::new();

                    // Switch the current function and the function just created
                    std::mem::swap(&mut self.current_function, &mut func);

                    // Insert the function
                    self.functions.insert(name, (func, index));
                }

                Program::Import(import) => {
                    self.interpret_import(import)?;
                }

                Program::TypeDecl(ty) => {
                    self.interpret_type(ty)?;
                } // node => todo!("Implement all Program-level nodes: {:?}", node),
            }
        }

        Ok(())
    }

    fn interpret_type(&mut self, ty: TypeDecl) -> Result<()> {
        let mut builder = CodeBuilder::new();
        std::mem::swap(&mut builder, &mut self.builder);

        let type_name = ty.name;
        builder.build_type(type_name, |builder, ctx| {
            for (vis, name, ty) in ty.members {
                ctx.add_member(name, vis, ty);
            }

            for method in ty.methods {
                ctx.add_method(builder, method.name, Visibility::Exposed, |builder, ctx| {
                    let mut argument_block = Block::new();
                    for (arg, _ty) in method.arguments {
                        let loc = ctx.reserve_reg(arg)?;
                        argument_block.inst_pop(loc);
                    }
                    ctx.push_block(argument_block);

                    let mut statements = Block::new();
                    // For each expression in the function, evaluate it into instructions
                    for statement in method.body {
                        self.statement(statement, builder, ctx, &mut statements)?;
                    }

                    Ok(())
                })?;
            }

            Ok(())
        })?;

        std::mem::swap(&mut builder, &mut self.builder);
        drop(builder);

        Ok(())
    }

    fn interpret_import<'a>(&mut self, import: Import) -> Result<()> {
        match import.source {
            ImportSource::File(relative_path) => {
                // TODO: allow importing folders

                let contents = {
                    use std::{fs::File, io::Read};

                    let mut path = PathBuf::from("./");
                    path.push(&relative_path);

                    let mut file = match File::open(&path.with_extension("crunch")) {
                        Ok(file) => file,
                        Err(err) => {
                            error!("Error opening imported file: {:?}", err);

                            return Err(RuntimeError {
                                ty: RuntimeErrorTy::FileError,
                                message: format!(
                                    "The file '{}' does not exist",
                                    relative_path.display()
                                ),
                            });
                        }
                    };

                    let mut contents = String::new();

                    if let Err(err) = file.read_to_string(&mut contents) {
                        error!("Error reading imported file: {:?}", err);

                        return Err(RuntimeError {
                            ty: RuntimeErrorTy::FileError,
                            message: format!("Cannot read the file '{}'", relative_path.display()),
                        });
                    }

                    contents
                };

                let file_name = relative_path.to_string_lossy();
                let import_ast = match Parser::new(Some(&*file_name), &contents).parse() {
                    Ok((ast, _diagnostics)) => {
                        // TODO: Emit errors
                        ast
                    }
                    Err(_err) => {
                        // TODO: Emit errors
                        return Err(RuntimeError {
                            ty: RuntimeErrorTy::CompilationError,
                            message: format!("The dependency '{}' failed to compile", file_name),
                        });
                    }
                };

                self.interpret_module(import_ast)?;
            }
            ImportSource::Package(sym) => {
                let _package = self.builder.interner.resolve(sym).unwrap();
                todo!("Package code loading")
            }
            ImportSource::Native(sym) => {
                let _path = self.builder.interner.resolve(sym).unwrap();
                todo!("Native code loading")
            }
        }

        Ok(())
    }

    /// Interpret a function
    fn interp_func(&mut self, func: FunctionDecl) -> Result<(Sym, Option<usize>)> {
        let mut builder = CodeBuilder::new();
        std::mem::swap(&mut builder, &mut self.builder);

        let func_name = func.name;
        builder.build_function(func_name, |builder, ctx| {
            let mut arg_block = Block::new();
            for (arg, _ty) in func.arguments {
                let loc = ctx.reserve_reg(arg)?;
                arg_block.inst_pop(loc);
            }
            ctx.push_block(arg_block);

            let mut statements = Block::new();
            // For each expression in the function, evaluate it into instructions
            for statement in func.body {
                self.statement(statement, builder, ctx, &mut statements)?;
            }
            ctx.push_block(statements);

            Ok(())
        })?;

        std::mem::swap(&mut builder, &mut self.builder);
        drop(builder);

        let index = match self.builder.interner.resolve(func_name) {
            Some("main") => None,
            _ => Some(self.get_next_func_id()),
        };

        Ok((func_name, index))
    }

    pub fn get_next_func_id(&mut self) -> usize {
        self.func_index += 1;
        self.func_index
    }

    fn expr<'a>(
        &mut self,
        builder: &mut CodeBuilder,
        ctx: &mut FunctionContext,
        expr: Expr,
        target: impl Into<Option<Sym>>,
        block: &mut Block,
    ) -> Result<DataLocation> {
        match expr {
            Expr::Literal(literal) => {
                let addr = ctx.reserve_reg(target)?;

                let value = match literal {
                    Literal::String(sym) => RuntimeValue::Str(Box::leak(
                        builder
                            .interner
                            .resolve(sym)
                            .unwrap()
                            .to_string()
                            .into_boxed_str(),
                    )),
                    Literal::Integer(int) => RuntimeValue::I32(int),
                    Literal::Boolean(boolean) => RuntimeValue::Bool(boolean),
                };

                block.inst_load(addr, value);

                Ok(DataLocation::Register(addr))
            }
            Expr::Range(_range) => todo!("What even do I do here?"),
            Expr::Comparison(comparison) => {
                let (left, right) = (
                    self.expr(builder, ctx, *comparison.left, None, block)?
                        .to_register(ctx, None, block)?,
                    self.expr(builder, ctx, *comparison.right, None, block)?
                        .to_register(ctx, None, block)?,
                );

                match comparison.comparison {
                    Comparator::Equal => block.inst_eq(left, right),
                    Comparator::NotEqual => block.inst_not_eq(left, right),
                    Comparator::LessEqual => block.inst_less_than_eq(left, right),
                    Comparator::GreaterEqual => block.inst_greater_than_eq(left, right),
                    Comparator::Less => block.inst_less_than(left, right),
                    Comparator::Greater => block.inst_greater_than(left, right),
                };

                Ok(DataLocation::Comparison)
            }
            Expr::BinaryOperation(bin_op) => {
                let (left, right) = (
                    self.expr(builder, ctx, *bin_op.left, None, block)?
                        .to_register(ctx, None, block)?,
                    self.expr(builder, ctx, *bin_op.right, None, block)?
                        .to_register(ctx, None, block)?,
                );
                let output = ctx.reserve_reg(target)?;

                // TODO: Handle different operation types
                match bin_op.op {
                    (BinaryOp::Plus, _ty) => block.inst_add(left, right),
                    (BinaryOp::Minus, _ty) => block.inst_sub(left, right),
                    (BinaryOp::Mult, _ty) => block.inst_mult(left, right),
                    (BinaryOp::Div, _ty) => block.inst_div(left, right),
                    (BinaryOp::And, _ty) => block.inst_and(left, right),
                    (BinaryOp::Or, _ty) => block.inst_or(left, right),
                    (BinaryOp::Xor, _ty) => block.inst_xor(left, right),
                };
                block.inst_op_to_reg(output);

                Ok(DataLocation::Register(output))
            }
            Expr::Ident(sym) => Ok(DataLocation::Register(ctx.get_cached_reg(sym)?)),
            Expr::Expr(expr) => self.expr(builder, ctx, *expr, target, block),

            Expr::FunctionCall(func_call) => {
                for arg in func_call.arguments {
                    let reg = self
                        .expr(builder, ctx, arg, None, block)?
                        .to_register(ctx, None, block)?;
                    block.inst_push(reg);
                    ctx.free_reg(reg);
                }

                let mut intrinsic_fn = false;
                if let Some(abs_path) = builder.interner.resolve(func_call.name) {
                    if let Some(intrinsic) = INTRINSICS.get(abs_path) {
                        (intrinsic)(builder, ctx, block)?;
                        intrinsic_fn = true;
                    }
                }

                let ret_val = ctx.reserve_reg(None)?;
                if !intrinsic_fn {
                    block.inst_func_call(func_call.name);
                    block.inst_pop(ret_val);
                }

                Ok(DataLocation::Register(ret_val))
            }
        }
    }

    fn statement(
        &mut self,
        statement: Statement,
        builder: &mut CodeBuilder,
        ctx: &mut FunctionContext,
        block: &mut Block,
    ) -> Result<()> {
        match statement {
            Statement::Assign(assign) => {
                let reg = ctx.get_cached_reg(assign.var)?;
                let loaded = self
                    .expr(builder, ctx, assign.expr, None, block)?
                    .to_register(ctx, None, block)?;

                block.inst_mov(reg, loaded, ctx);
            }

            Statement::While(_while_loop) => todo!("Compile While loops"),
            Statement::Loop(_loop_loop) => todo!("Compile Loops"),
            Statement::For(_for_loop) => todo!("Compile For loops"),

            Statement::VarDecl(var_decl) => {
                self.expr(builder, ctx, var_decl.expr, var_decl.name, block)?
                    .to_register(ctx, var_decl.name, block)?;
            }

            Statement::Return(ret) => {
                let loaded = self
                    .expr(builder, ctx, ret.expr, None, block)?
                    .to_register(ctx, None, block)?;
                block.inst_push(loaded).inst_return();

                let mut new_block = Block::new();
                std::mem::swap(block, &mut new_block);
                ctx.push_block(new_block);
            }
            Statement::Continue => todo!("Compile Continue statements"),
            Statement::Break => todo!("Compile break statements"),
            Statement::Expr(expr) => {
                self.expr(builder, ctx, expr, None, block)?;
            }
            Statement::Empty => { /* Do nothing for `empty` */ }

            Statement::Conditional(conditional) => {
                let mut after_block = Block::new();

                let mut conditions = Vec::with_capacity(conditional._if.len() + 1);
                for If { condition, body } in conditional._if {
                    self.expr(builder, ctx, condition, None, block)?;

                    let mut if_block = Block::new();
                    for statement in body {
                        self.statement(statement, builder, ctx, &mut if_block)?;
                    }
                    if_block.inst_jump(after_block.id);

                    block.inst_jump_comp(if_block.id);
                    conditions.push(if_block);
                }

                if let Some(_else) = conditional._else {
                    let mut else_block = Block::new();
                    for statement in _else.body {
                        self.statement(statement, builder, ctx, &mut else_block)?;
                    }

                    block.inst_jump(after_block.id);
                    conditions.push(else_block);
                }

                std::mem::swap(block, &mut after_block);
                ctx.push_block(after_block);

                for cond in conditions {
                    ctx.push_block(cond);
                }
            }
        }

        Ok(())
    }

    /*
    fn reserve_ident_literal(
        &mut self,
        ident_literal: IdentLiteral<'a>,
        builder: &mut CodeBuilder,
        ctx: &mut FunctionContext,
    ) -> Result<Register> {
        match ident_literal {
            IdentLiteral::Literal(literal) => {
                let reg = ctx.reserve_caller_reg(None)?;
                ctx.inst_load(reg, literal.val.into());

                Ok(reg)
            }

            IdentLiteral::Variable(variable) => {
                let ident = builder.intern(variable.name);
                let reg = if let Ok(reg) = ctx.get_cached_reg(ident) {
                    reg
                } else {
                    error!("Failed to find variable for Print parameter");
                    return Err(RuntimeError {
                        ty: RuntimeErrorTy::NullVar,
                        message: format!("The variable {:?} does not exist", variable.name),
                    });
                };

                Ok(reg)
            }
        }
    }
    */
}
