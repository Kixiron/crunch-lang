use crate::{
    code_builder::{Block, CodeBuilder, FunctionContext},
    instruction::Result,
    instruction::{RuntimeError, RuntimeErrorTy},
    parser::*,
    Instruction, Options, Register, Value,
};
use std::{collections::HashMap, path::PathBuf};
use string_interner::{StringInterner, Sym};

type IntrinsicsMap =
    HashMap<&'static str, fn(&mut CodeBuilder, &mut FunctionContext) -> Result<()>>;

lazy_static::lazy_static! {
    static ref INTRINSICS: IntrinsicsMap = {
        let mut intrinsics: IntrinsicsMap = HashMap::with_capacity(10);

        // TODO: Think through this better

        intrinsics.insert("print", |_builder, ctx| {
            let reg = ctx.reserve_reg(None)?;
            ctx.current_block().inst_pop(reg).inst_print(reg);

            Ok(())
        });
        intrinsics.insert("println", |_builder, ctx| {
            let reg = ctx.reserve_reg(None)?;
            let newline = ctx.reserve_reg(None)?;

            ctx.current_block().inst_pop(reg)
                .inst_print(reg)
                .inst_load(newline, Value::Str("\n"))
                .inst_print(newline);
            ctx.inst_drop_block(newline, ctx.current_block);

            Ok(())
        });
        intrinsics.insert("halt", |_builder, ctx| {
            ctx.current_block().inst_halt();

            Ok(())
        });
        intrinsics.insert("range", |builder, _ctx| {
            let range = builder.intern("range");
            builder.build_function(range, |_builder, ctx| {
                let val = ctx.reserve_reg(None)?;
                let max = ctx.reserve_reg(None)?;
                let null = ctx.reserve_reg(None)?;
                let increment = ctx.reserve_reg(None)?;

                ctx.add_block();
                let generator_block = ctx.current_block;
                ctx.current_block()
                    .inst_load(increment, Value::I32(1))
                    .inst_pop(val)
                    .inst_pop(max)
                    .inst_add(increment, val)
                    .inst_op_to_reg(val)
                    .inst_less_than(val, max);

                ctx.add_block();
                let success = ctx.current_block;
                ctx.current_block()
                    .inst_push(max)
                    .inst_push(val)
                    .inst_yield()
                    .inst_jump(generator_block as u32);

                ctx.add_block();
                let fail = ctx.current_block;
                ctx.current_block()
                    .inst_load(null, Value::Null)
                    .inst_push(max)
                    .inst_push(null)
                    .inst_yield()
                    .inst_jump(fail as u32);

                ctx.get_block(generator_block)
                    .inst_jump_comp(success as u32)
                    .inst_jump(fail as u32);

                Ok(())
            })?;

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
    ) -> Result<Register> {
        match self {
            Self::Register(reg) => {
                let target = target.into();

                if let Some(target) = target {
                    let target = ctx.reserve_reg(target)?;
                    ctx.current_block().inst_copy(reg, target);
                    Ok(target)
                } else {
                    Ok(reg)
                }
            }
            Self::Comparison => {
                let reg = ctx.reserve_reg(target)?;
                ctx.current_block().inst_comp_to_reg(reg);
                Ok(reg)
            }
            Self::Operation => {
                let reg = ctx.reserve_reg(target)?;
                ctx.current_block().inst_op_to_reg(reg);
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
    pub fn interpret(mut self, ast: Vec<Program>) -> Result<Vec<Vec<Instruction>>> {
        self.interpret_module(ast)?;
        let functions = self.builder.build()?;

        trace!("Interp Output: {:?}", functions);

        Ok(functions)
    }

    fn interpret_module(&mut self, mut ast: Vec<Program>) -> Result<()> {
        while let Some(node) = ast.pop() {
            match node {
                Program::FunctionDecl(func) => {
                    // Interpret the function
                    let (name, index) = self.interp_func(func)?;

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

                    // For each expression in the function, evaluate it into instructions
                    for statement in method.body {
                        self.statement(statement, builder, ctx)?;
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

    fn interpret_import(&mut self, import: Import) -> Result<()> {
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
            for (arg, _ty) in func.arguments {
                let loc = ctx.reserve_reg(arg)?;
                ctx.current_block().inst_pop(loc);
            }

            // For each expression in the function, evaluate it into instructions
            for statement in func.body {
                self.statement(statement, builder, ctx)?;
            }

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

    fn expr(
        &mut self,
        builder: &mut CodeBuilder,
        ctx: &mut FunctionContext,
        expr: Expr,
        target: impl Into<Option<Sym>>,
    ) -> Result<DataLocation> {
        match expr {
            Expr::Literal(literal) => {
                let addr = ctx.reserve_reg(target)?;

                let value = match literal {
                    Literal::String(sym) => Value::Str(Box::leak(
                        builder
                            .interner
                            .resolve(sym)
                            .unwrap()
                            .to_string()
                            .into_boxed_str(),
                    )),
                    Literal::Integer(int) => Value::I32(int),
                    Literal::Boolean(boolean) => Value::Bool(boolean),
                };

                ctx.current_block().inst_load(addr, value);

                Ok(DataLocation::Register(addr))
            }
            Expr::Range(_range) => todo!("What even do I do here?"),
            Expr::Comparison(comparison) => {
                let (left, right) = (
                    self.expr(builder, ctx, *comparison.left, None)?
                        .to_register(ctx, None)?,
                    self.expr(builder, ctx, *comparison.right, None)?
                        .to_register(ctx, None)?,
                );

                match comparison.comparison {
                    Comparator::Equal => ctx.current_block().inst_eq(left, right),
                    Comparator::NotEqual => ctx.current_block().inst_not_eq(left, right),
                    Comparator::LessEqual => ctx.current_block().inst_less_than_eq(left, right),
                    Comparator::GreaterEqual => {
                        ctx.current_block().inst_greater_than_eq(left, right)
                    }
                    Comparator::Less => ctx.current_block().inst_less_than(left, right),
                    Comparator::Greater => ctx.current_block().inst_greater_than(left, right),
                };

                Ok(DataLocation::Comparison)
            }
            Expr::BinaryOperation(bin_op) => {
                let (left, right) = (
                    self.expr(builder, ctx, *bin_op.left, None)?
                        .to_register(ctx, None)?,
                    self.expr(builder, ctx, *bin_op.right, None)?
                        .to_register(ctx, None)?,
                );
                let output = ctx.reserve_reg(target)?;

                // TODO: Handle different operation types
                match bin_op.op {
                    (BinaryOp::Plus, _ty) => ctx.current_block().inst_add(left, right),
                    (BinaryOp::Minus, _ty) => ctx.current_block().inst_sub(left, right),
                    (BinaryOp::Mult, _ty) => ctx.current_block().inst_mult(left, right),
                    (BinaryOp::Div, _ty) => ctx.current_block().inst_div(left, right),
                    (BinaryOp::And, _ty) => ctx.current_block().inst_and(left, right),
                    (BinaryOp::Or, _ty) => ctx.current_block().inst_or(left, right),
                    (BinaryOp::Xor, _ty) => ctx.current_block().inst_xor(left, right),
                };

                ctx.current_block().inst_op_to_reg(output);

                Ok(DataLocation::Register(output))
            }
            Expr::Ident(sym) => Ok(DataLocation::Register(ctx.get_cached_reg(sym).map_err(
                |err| {
                    dbg!(builder.interner.resolve(sym));
                    err
                },
            )?)),
            Expr::Expr(expr) => self.expr(builder, ctx, *expr, target),
            Expr::Array(arr) => {
                ctx.add_block();
                let array = ctx.reserve_reg(target)?;

                for expr in arr {
                    let expr = self
                        .expr(builder, ctx, expr, None)?
                        .to_register(ctx, None)?;
                    ctx.inst_push_arr(array, expr, ctx.current_block);
                }
                ctx.add_block();

                Ok(DataLocation::Register(array))
            }
            Expr::FunctionCall(func_call) => {
                for arg in func_call.arguments {
                    let reg = self.expr(builder, ctx, arg, None)?.to_register(ctx, None)?;

                    ctx.current_block().inst_push(reg);
                    ctx.free_reg(reg);
                }

                let mut intrinsic_fn = false;
                if let Some(abs_path) = builder.interner.resolve(func_call.name) {
                    if let Some(intrinsic) = INTRINSICS.get(abs_path) {
                        (intrinsic)(builder, ctx)?;
                        intrinsic_fn = true;
                    }
                }

                let ret_val = ctx.reserve_reg(None)?;
                if !intrinsic_fn {
                    ctx.current_block()
                        .inst_func_call(func_call.name)
                        .inst_pop(ret_val);
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
    ) -> Result<()> {
        match statement {
            Statement::Assign(assign) => {
                // TODO: If type is not copyable, move it or clone it

                let reg = ctx.get_cached_reg(assign.var)?;
                let loaded = self
                    .expr(builder, ctx, assign.expr, None)?
                    .to_register(ctx, None)?;

                if assign.ty == AssignType::Normal {
                    ctx.current_block().inst_copy(loaded, reg);
                } else if let AssignType::BinaryOp(op) = assign.ty {
                    match op {
                        BinaryOp::Plus => ctx.current_block().inst_add(loaded, reg),
                        BinaryOp::Minus => ctx.current_block().inst_sub(loaded, reg),
                        BinaryOp::Mult => ctx.current_block().inst_mult(loaded, reg),
                        BinaryOp::Div => ctx.current_block().inst_div(loaded, reg),
                        BinaryOp::Xor => ctx.current_block().inst_xor(loaded, reg),
                        BinaryOp::Or => ctx.current_block().inst_or(loaded, reg),
                        BinaryOp::And => ctx.current_block().inst_and(loaded, reg),
                    }
                    .inst_op_to_reg(reg);
                } else {
                    unreachable!()
                }
            }

            Statement::While(_while_loop) => todo!("Compile While loops"),
            Statement::Loop(_loop_loop) => todo!("Compile Loops"),
            Statement::For(for_loop) => {
                // TODO: Make this general-purpose
                if let Expr::Range(range) = for_loop.range {
                    let start = self
                        .expr(builder, ctx, *range.start, for_loop.element)?
                        .to_register(ctx, for_loop.element)?;
                    let end = self
                        .expr(builder, ctx, *range.end, None)?
                        .to_register(ctx, None)?;
                    let increment = ctx.reserve_reg(None)?;
                    ctx.current_block().inst_load(increment, Value::I32(1));

                    ctx.add_block();
                    let block = ctx.current_block;

                    for statement in for_loop.body {
                        self.statement(statement, builder, ctx)?;
                    }

                    ctx.add_block();
                    ctx.current_block()
                        .inst_add(start, increment)
                        .inst_op_to_reg(start)
                        .inst_less_than(start, end)
                        .inst_jump_comp(block as u32);

                    ctx.add_block();
                    ctx.inst_drop_block(increment, ctx.current_block);
                    ctx.inst_drop_block(start, ctx.current_block);
                    ctx.inst_drop_block(end, ctx.current_block);
                    ctx.add_block();
                } else {
                    todo!("Other range types")
                }
            }

            Statement::VarDecl(var_decl) => {
                self.expr(builder, ctx, var_decl.expr, var_decl.name)?
                    .to_register(ctx, var_decl.name)?;
            }

            Statement::Return(ret) => {
                let loaded = self
                    .expr(builder, ctx, ret.expr, None)?
                    .to_register(ctx, None)?;

                ctx.current_block().inst_push(loaded).inst_return();
                ctx.add_block();
            }
            Statement::Continue => todo!("Compile Continue statements"),
            Statement::Break => todo!("Compile break statements"),
            Statement::Expr(expr) => {
                self.expr(builder, ctx, expr, None)?;
            }
            Statement::Empty => { /* Do nothing for `empty` */ }

            Statement::Conditional(conditional) => {
                ctx.add_block();
                let conditional_block = ctx.current_block;

                let mut conditions = Vec::with_capacity(conditional._if.len() + 1);
                for If {
                    condition, body, ..
                } in conditional._if
                {
                    ctx.move_to_block(conditional_block);
                    self.expr(builder, ctx, condition, None)?;

                    ctx.add_block();
                    let if_block = ctx.current_block;
                    conditions.push(if_block);

                    ctx.move_to_block(conditional_block);
                    ctx.current_block().inst_jump_comp(if_block as u32);

                    ctx.move_to_block(if_block);
                    for statement in body {
                        self.statement(statement, builder, ctx)?;
                    }
                }

                if let Some(Else { body, .. }) = conditional._else {
                    ctx.add_block();
                    let else_block = ctx.current_block;
                    conditions.push(else_block);

                    ctx.move_to_block(conditional_block);
                    ctx.current_block().inst_jump(else_block as u32);

                    ctx.move_to_block(else_block);
                    for statement in body {
                        self.statement(statement, builder, ctx)?;
                    }
                }

                ctx.add_block();
                let after_block = ctx.current_block;
                for block in conditions {
                    ctx.get_block(block).inst_jump(after_block as u32);
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
