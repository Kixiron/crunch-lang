use crate::{
    code_builder::{CodeBuilder, FunctionContext},
    instruction::Result,
    instruction::{RuntimeError, RuntimeErrorTy},
    parser::*,
    Instruction, Options, Register, RuntimeValue,
};
use std::{collections::HashMap, path::PathBuf};
use string_interner::{StringInterner, Sym};

lazy_static::lazy_static! {
    static ref INTRINSICS: HashMap<&'static str, fn(&mut CodeBuilder, &mut FunctionContext, &[Option<Register>]) -> Result<()>> = {
        let mut intrinsics: HashMap<&'static str, fn(&mut CodeBuilder, &mut FunctionContext, &[Option<Register>]) -> Result<()>> = HashMap::with_capacity(10);

        // TODO: Think through this better

        intrinsics.insert("print", |_builder, ctx, registers| {
            ctx.inst_print(registers[0].unwrap());

            Ok(())
        });
        intrinsics.insert("println", |_builder, ctx, registers| {
            let newline = ctx.reserve_reg(None)?;
            ctx.inst_print(registers[0].unwrap())
                .inst_load(newline, RuntimeValue::Str("\n"))
                .inst_print(newline)
                .inst_drop(newline);

            Ok(())
        });

        intrinsics
    };
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
                    let (mut args, mut number) = (method.arguments.into_iter(), 0);
                    while let Some((arg_name, _arg_type)) = args.next() {
                        if number <= 5 {
                            ctx.reserve_caller_reg(arg_name)?;
                            number += 1;
                        } else {
                            let reg = ctx.reserve_reg(arg_name)?;
                            ctx.inst_pop(reg);
                        }
                    }

                    // For each expression in the method, evaluate it into instructions
                    for statement in method.body {
                        // TODO: This has gotta be bad
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
            let (mut args, mut number) = (func.arguments.into_iter(), 0);
            while let Some((arg_name, _arg_type)) = args.next() {
                if number <= 5 {
                    ctx.reserve_caller_reg(arg_name)?;
                    number += 1;
                } else {
                    let reg = ctx.reserve_reg(arg_name)?;
                    ctx.inst_pop(reg);
                }
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

    fn expr<'a>(
        &mut self,
        builder: &mut CodeBuilder,
        ctx: &mut FunctionContext,
        expr: Expr,
    ) -> Result<Register> {
        match expr {
            Expr::Literal(literal) => {
                let addr = ctx.reserve_reg(None)?;

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

                ctx.inst_load(addr, value);

                Ok(addr)
            }
            Expr::Range(_range) => todo!("What even do I do here?"),
            Expr::Comparison(comparison) => {
                let (left, right) = (
                    self.expr(builder, ctx, *comparison.left)?,
                    self.expr(builder, ctx, *comparison.right)?,
                );

                match comparison.comparison {
                    Comparator::Equal => ctx.inst_eq(left, right),
                    Comparator::NotEqual => ctx.inst_not_eq(left, right),
                    Comparator::LessEqual => ctx.inst_less_than_eq(left, right),
                    Comparator::GreaterEqual => ctx.inst_greater_than_eq(left, right),
                    Comparator::Less => ctx.inst_less_than(left, right),
                    Comparator::Greater => ctx.inst_greater_than(left, right),
                };

                warn!(
                    "Expr::Comparison returns the left register as a return value, but the Comparison Operation \
                    does not have a meaningful return value, as the comparison is stored in `vm.prev_op`"
                );
                Ok(left)
            }
            Expr::BinaryOperation(bin_op) => {
                let (left, right) = (
                    self.expr(builder, ctx, *bin_op.left)?,
                    self.expr(builder, ctx, *bin_op.right)?,
                );
                let output = ctx.reserve_reg(None)?;

                // TODO: Handle different operation types
                match bin_op.op {
                    (BinaryOp::Plus, _ty) => ctx.inst_add(left, right),
                    (BinaryOp::Minus, _ty) => ctx.inst_sub(left, right),
                    (BinaryOp::Mult, _ty) => ctx.inst_mult(left, right),
                    (BinaryOp::Div, _ty) => ctx.inst_div(left, right),
                    (BinaryOp::And, _ty) => ctx.inst_and(left, right),
                    (BinaryOp::Or, _ty) => ctx.inst_or(left, right),
                    (BinaryOp::Xor, _ty) => ctx.inst_xor(left, right),
                };
                ctx.inst_op_to_reg(output);

                Ok(output)
            }
            Expr::Ident(sym) => ctx.get_cached_reg(sym),
            Expr::Expr(expr) => self.expr(builder, ctx, *expr),

            Expr::FunctionCall(func_call) => {
                // let mut caller_registers = Vec::with_capacity(func_call.arguments.len());
                // for expr in func_call.arguments {
                //     caller_registers.push(self.expr(builder, ctx, expr)?);
                // }

                // TODO: What?

                let (mut args, mut number, mut out_args): (_, _, Vec<Option<Register>>) =
                    (func_call.arguments.into_iter(), 0, Vec::new());
                while let Some(arg) = args.next() {
                    // TODO: Verify that it's the correct kind of error
                    let arg = match self.expr(builder, ctx, arg) {
                        Ok(reg) => Some(reg),
                        Err(err) => {
                            error!("Error evaluating function: {:?}", err);
                            None
                        }
                    };

                    if number < 5 {
                        // TODO: Pop them back after the function call
                        if ctx.reserve_nth_reg(number).is_err() {
                            ctx.inst_push(number).reserve_nth_reg(number)?;
                        }
                        ctx.inst_mov(number, arg.unwrap());
                        out_args.push(Some(number.into()));
                        number += 1;
                    } else {
                        ctx.inst_push(arg.expect("Expected an argument register"));
                        out_args.push(None);
                    }
                }

                let mut intrinsic_fn = false;
                if let Some(abs_path) = builder.interner.resolve(func_call.name) {
                    if let Some(intrinsic) = INTRINSICS.get(abs_path) {
                        (intrinsic)(builder, ctx, &out_args)?;
                        intrinsic_fn = true;
                    }
                }

                if !intrinsic_fn {
                    ctx.inst_func_call(func_call.name);
                }

                for reg in out_args {
                    if let Some(reg) = reg {
                        ctx.inst_pop(reg);
                    }
                }

                Ok(0.into())
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
                let reg = ctx.get_cached_reg(assign.var)?;
                let loaded = self.expr(builder, ctx, assign.expr)?;

                ctx.inst_mov(reg, loaded).inst_drop(loaded);
            }

            Statement::While(_while_loop) => todo!("Compile While loops"),
            Statement::Loop(_loop_loop) => todo!("Compile Loops"),
            Statement::For(_for_loop) => todo!("Compile For loops"),

            Statement::VarDecl(var_decl) => {
                let reg = ctx.reserve_reg(var_decl.name)?;
                let loaded = self.expr(builder, ctx, var_decl.expr)?;

                ctx.inst_mov(reg, loaded).inst_drop(loaded);
            }

            Statement::Return(_ret) => todo!("Compile return statements"),
            Statement::Continue => todo!("Compile Continue statements"),
            Statement::Break => todo!("Compile break statements"),
            Statement::Expr(expr) => {
                self.expr(builder, ctx, expr)?;
            }
            Statement::Empty => { /* Do nothing for `empty` */ }

            Statement::Conditional(conditional) => {
                let endif = builder.next_jump_id();

                let true_reg = ctx.reserve_reg(None)?;
                ctx.inst_load(true_reg, RuntimeValue::Bool(true));

                let mut bodies: Vec<
                    Box<
                        dyn FnOnce(
                            &mut Interpreter,
                            &mut CodeBuilder,
                            &mut FunctionContext,
                        ) -> Result<()>,
                    >,
                > = Vec::new();

                for If { condition, body } in conditional._if {
                    self.expr(builder, ctx, condition)?;
                    let block_start = builder.next_jump_id();

                    ctx.inst_jump_comp(block_start);

                    bodies.push(Box::new(move |interp, builder, ctx| {
                        ctx.inst_jump_point(block_start);

                        for statement in body {
                            interp.statement(statement, builder, ctx)?;
                        }

                        ctx.inst_jump(endif);

                        Ok(())
                    }));
                }

                if let Some(_else) = conditional._else {
                    let else_jump = builder.next_jump_id();
                    ctx.inst_jump(else_jump);

                    bodies.push(Box::new(move |interp, builder, ctx| {
                        ctx.inst_jump_point(else_jump);

                        for statement in _else.body {
                            interp.statement(statement, builder, ctx)?;
                        }

                        ctx.inst_jump(endif);

                        Ok(())
                    }));
                }

                for func in bodies {
                    (func)(self, builder, ctx)?;
                }

                ctx.inst_jump_point(endif).inst_drop(true_reg);
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
