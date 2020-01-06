use crate::{
    code_builder::{CodeBuilder, FunctionContext},
    instruction::Result,
    instruction::{RuntimeError, RuntimeErrorTy},
    parser::*,
    Instruction, Options, Register, RuntimeValue,
};
use std::{collections::HashMap, path::PathBuf};
use string_interner::Sym;

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
    pub functions: HashMap<String, (Vec<Instruction>, Option<usize>)>,
    pub current_function: Vec<Instruction>,
    pub options: InterpOptions,
    pub func_index: usize,
    pub builder: CodeBuilder,
}

impl Interpreter {
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

    /// Interpret the contained ast and return the instructions
    pub fn interpret<'a>(mut self, ast: Vec<Node<'a>>) -> Result<Vec<Vec<Instruction>>> {
        self.interpret_module(ast)?;
        let functions = self.builder.build()?;

        trace!("Interp Output: {:?}", functions);

        Ok(functions)
    }

    fn interpret_module<'a>(&mut self, mut ast: Vec<Node<'a>>) -> Result<()> {
        while let Some(node) = ast.pop() {
            match node {
                Node::Func(func) => {
                    // Interpret the function
                    let (name, index) = self.interp_func(func)?;

                    // Will contain the newly created function
                    let mut func = Vec::new();

                    // Switch the current function and the function just created
                    std::mem::swap(&mut self.current_function, &mut func);

                    // Insert the function
                    self.functions.insert(name, (func, index));
                }

                Node::Import(import) => {
                    self.interpret_import(import)?;
                }
            }
        }

        Ok(())
    }

    fn interpret_import<'a>(&mut self, import: Import<'a>) -> Result<()> {
        if import.ty == ImportType::File {
            // TODO: allow importing folders

            let contents = {
                use std::{fs::File, io::Read};

                let mut path = PathBuf::from("./");
                path.push(&import.file);

                let mut file = match File::open(&path.with_extension("crunch")) {
                    Ok(file) => file,
                    Err(err) => {
                        error!("Error opening imported file: {:?}", err);

                        return Err(RuntimeError {
                            ty: RuntimeErrorTy::FileError,
                            message: format!("The file '{}' does not exist", import.file.display()),
                        });
                    }
                };

                let mut contents = String::new();

                if let Err(err) = file.read_to_string(&mut contents) {
                    error!("Error reading imported file: {:?}", err);

                    return Err(RuntimeError {
                        ty: RuntimeErrorTy::FileError,
                        message: format!("Cannot read the file '{}'", import.file.display()),
                    });
                }

                contents
            };

            let file_name = import.file.to_string_lossy();
            let parser = match Parser::new(Some(&*file_name), &contents).parse() {
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

            self.interpret_module(parser)?;
        } else {
            unimplemented!(
                "The import type {:?} has not been implemented yet",
                import.ty
            );
        }

        Ok(())
    }

    fn load_binding_val<'a>(
        &mut self,
        builder: &mut CodeBuilder,
        ctx: &mut FunctionContext,
        binding_val: BindingVal<'a>,
        binding_name: impl Into<Option<Sym>>,
    ) -> Result<Register> {
        match binding_val {
            BindingVal::Literal(literal) => {
                let addr = ctx.reserve_reg(binding_name)?;

                ctx.inst_load(addr, literal.val.into());

                Ok(addr)
            }

            BindingVal::BinOp(bin_op) => match (bin_op.left, bin_op.right) {
                (BinOpSide::Literal(left), BinOpSide::Literal(right)) => {
                    let (left, right): (RuntimeValue, RuntimeValue) =
                        (left.val.into(), right.val.into());
                    let (left_reg, right_reg) =
                        (ctx.reserve_reg(binding_name)?, ctx.reserve_reg(None)?);

                    // TODO: Compile-time evaluation
                    ctx.inst_load(left_reg, left).inst_load(right_reg, right);
                    match bin_op.op {
                        Op::Add => ctx.inst_add(left_reg, right_reg),
                        Op::Sub => ctx.inst_sub(left_reg, right_reg),
                        Op::Mult => ctx.inst_mult(left_reg, right_reg),
                        Op::Div => ctx.inst_div(left_reg, right_reg),
                        Op::IsEqual => ctx.inst_eq(left_reg, right_reg),
                    };
                    ctx.inst_op_to_reg(left_reg).inst_drop(right_reg);

                    Ok(left_reg)
                }

                (BinOpSide::Literal(left), BinOpSide::Variable(right)) => {
                    let left_reg = ctx.reserve_reg(binding_name)?;
                    ctx.inst_load(left_reg, left.val.into());

                    let right_ident = builder.intern(&*right.name);
                    let (right_reg, faulted) = if let Ok(reg) = ctx.get_cached_reg(right_ident) {
                        (reg, false)
                    } else if self.options.fault_tolerant {
                        let reg = ctx.reserve_reg(right_ident)?;

                        ctx.inst_load(reg, RuntimeValue::None);

                        (reg, true)
                    } else {
                        error!("Failed to find variable for Literal/Variable binary operation");
                        return Err(RuntimeError {
                            ty: RuntimeErrorTy::NullVar,
                            message: format!("The variable {:?} does not exist", &*right.name),
                        });
                    };

                    if let Ok(right_reg) = ctx.get_cached_reg(right_ident) {
                        ctx.inst_add(right_reg, left_reg);

                        if self.options.fault_tolerant && faulted {
                            ctx.inst_drop(left_reg).free_reg(right_reg);
                        }
                    } else {
                        let left_reg = ctx.reserve_reg(right_ident)?;

                        match bin_op.op {
                            Op::Add => ctx.inst_add(left_reg, right_reg),
                            Op::Sub => ctx.inst_sub(left_reg, right_reg),
                            Op::Mult => ctx.inst_mult(left_reg, right_reg),
                            Op::Div => ctx.inst_div(left_reg, right_reg),
                            Op::IsEqual => ctx.inst_eq(left_reg, right_reg),
                        };
                    }

                    ctx.inst_drop(right_reg).inst_op_to_reg(left_reg);

                    Ok(left_reg)
                }

                (BinOpSide::Variable(left), BinOpSide::Literal(right)) => {
                    let left_ident = builder.intern(&*left.name);
                    let left_reg = if let Ok(reg) = ctx.get_cached_reg(left_ident) {
                        reg
                    } else if self.options.fault_tolerant {
                        let reg = ctx.reserve_reg(left_ident)?;
                        ctx.inst_load(reg, RuntimeValue::None);

                        reg
                    } else {
                        error!("Failed to find variable for Variable/Literal binary operation");
                        return Err(RuntimeError {
                            ty: RuntimeErrorTy::NullVar,
                            message: format!("The variable {:?} does not exist", &*left.name),
                        });
                    };

                    let right_reg = ctx.reserve_reg(None)?;

                    ctx.inst_load(right_reg, right.val.into())
                        .inst_add(left_reg, right_reg);

                    let output = ctx.reserve_reg(binding_name)?;
                    ctx.inst_drop(right_reg).inst_op_to_reg(output);

                    Ok(output)
                }
                _ => unimplemented!(),
            },
            _ => unimplemented!(),
        }
    }

    /// Interpret a function
    fn interp_func<'a>(&mut self, mut func: Func<'a>) -> Result<(String, Option<usize>)> {
        let mut builder = CodeBuilder::new();
        std::mem::swap(&mut builder, &mut self.builder);

        let function_name = func.name.name.to_string();
        let body = {
            let mut body = Vec::new();
            std::mem::swap(&mut func.body, &mut body);
            body
        };

        builder.function(func.name.name, |builder, ctx| {
            for param in func.params.into_iter().take(5) {
                ctx.reserve_caller_reg(builder.intern(param.name.name))?;
            }

            // For each expression in the function, evaluate it into instructions
            for expr in body {
                self.interp_func_body(builder, ctx, expr)?
            }

            Ok(())
        })?;

        std::mem::swap(&mut builder, &mut self.builder);
        drop(builder);

        let index = match &*function_name {
            "main" => None,
            _ => Some(self.get_next_func_id()),
        };

        Ok((function_name.to_string(), index))
    }

    pub fn get_next_func_id(&mut self) -> usize {
        self.func_index += 1;
        self.func_index
    }

    fn interp_func_body<'a>(
        &mut self,
        builder: &mut CodeBuilder,
        ctx: &mut FunctionContext,
        expr: FuncExpr<'a>,
    ) -> Result<()> {
        match expr {
            FuncExpr::NoOp => return Ok(()),

            FuncExpr::Conditional(conditional) => {
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

                for If { condition, body } in conditional.if_clauses {
                    let comparison = self.load_binding_val(builder, ctx, condition, None)?;
                    let block_start = builder.next_jump_id();

                    ctx.inst_eq(comparison, true_reg)
                        .inst_drop(comparison)
                        .inst_jump_comp(block_start);

                    bodies.push(Box::new(move |interp, builder, ctx| {
                        ctx.inst_jump_point(block_start);

                        for expr in body {
                            interp.interp_func_body(builder, ctx, expr)?;
                        }

                        ctx.inst_jump(endif);

                        Ok(())
                    }));
                }

                if let Some(body) = conditional.else_body {
                    let else_jump = builder.next_jump_id();
                    ctx.inst_jump(else_jump);

                    bodies.push(Box::new(move |interp, builder, ctx| {
                        ctx.inst_jump_point(else_jump);

                        for expr in body {
                            interp.interp_func_body(builder, ctx, expr)?;
                        }

                        ctx.inst_jump(endif);

                        Ok(())
                    }));
                }

                for func in bodies {
                    (func)(self, builder, ctx)?;
                }

                ctx.inst_jump_point(endif);
                ctx.inst_drop(true_reg);
            }

            // Bind a variable to a value
            FuncExpr::Binding(binding) => {
                let name = builder.intern(binding.name.name);
                self.load_binding_val(builder, ctx, binding.val, name)?;
            }

            // Compiler builtin functions
            FuncExpr::Builtin(builtin) => match builtin {
                // GC Collection cycle
                Builtin::Collect => {
                    ctx.inst_collect();
                }

                // Halt execution
                Builtin::Halt => {
                    ctx.inst_halt();
                }

                Builtin::SyscallExit(_exit_code) => {
                    unimplemented!("Syscalls have not been implemented");

                    /*
                    #[allow(unreachable_code)]
                    match exit_code {
                        // For literals fed into the print function, load them, print them, and drop them
                        IdentLiteral::Literal(literal) => {
                            let reg = ctx.reserve_reg()?;

                            ctx.inst_load(reg, literal.val.into()).free_reg(reg);
                        }

                        // For existing variables, fetch them and print them
                        IdentLiteral::Variable(variable) => {
                            let ident = builder.intern(&*variable.name);
                            let (_reg, _faulted) =
                                if let Ok(reg) = ctx.get_cached_reg(ident) {
                                    (reg, false)
                                } else if self.options.fault_tolerant {
                                    let reg = ctx.reserve_reg()?;
                                    ctx.inst_load(reg, RuntimeValue::None);

                                    (reg, true)
                                } else {
                                    return Err(RuntimeError {
                                        ty: RuntimeErrorTy::NullVar,
                                        message: format!(
                                            "The variable {:?} does not exist",
                                            &*variable.name
                                        ),
                                    });
                                };
                        }
                    }
                    */
                }

                // Print values
                Builtin::Print(params) => {
                    for param in params {
                        match param {
                            // For literals fed into the print function, load them, print them, and drop them
                            IdentLiteral::Literal(literal) => {
                                let reg = ctx.reserve_reg(None)?;

                                // Literals can just be moved into a register
                                ctx.inst_load(reg, literal.val.into())
                                    .inst_print(reg)
                                    .inst_drop(reg);
                            }

                            // For existing variables, fetch them and print them
                            IdentLiteral::Variable(variable) => {
                                let ident = builder.intern(variable.name);
                                let (reg, faulted) = if let Ok(reg) = ctx.get_cached_reg(ident) {
                                    (reg, false)
                                } else if self.options.fault_tolerant {
                                    let reg = ctx.reserve_reg(builder.intern(variable.name))?;
                                    ctx.inst_load(reg, RuntimeValue::None);

                                    (reg, true)
                                } else {
                                    error!("Failed to find variable for Print parameter");
                                    return Err(RuntimeError {
                                        ty: RuntimeErrorTy::NullVar,
                                        message: format!(
                                            "The variable {:?} does not exist",
                                            variable.name
                                        ),
                                    });
                                };

                                ctx.inst_print(reg);

                                if faulted {
                                    ctx.free_reg(reg);
                                }
                            }
                        }
                    }
                }
            },

            // FuncExpr::Assign(assign) => {
            //     // pub struct Assign<'a> {
            //     //     pub name: Ident<'a>,
            //     //     pub val: IdentLiteral<'a>,
            //     //     pub info: LocInfo,
            //     // }
            //
            //     // variables: HashMap<String, (Location, Type<'a>)>,
            //
            //     let var = if let Some(var) =
            //         self.current_scope.variables.get(&*assign.name.name)
            //     {
            //         var
            //     } else {
            //         return Err(RuntimeError {
            //             ty: RuntimeErrorTy::MissingValue,
            //             message: "The variable being assigned to does not exist".to_string(),
            //         });
            //     };
            //
            //     let reg = self.reserve_reg(Some(var.0), None);
            //
            //     self.add_to_current(&[Instruction::Load( )]);
            // }
            FuncExpr::FuncCall(func_call) => {
                let func_name = builder.intern(func_call.func_name.name);

                let mut caller_registers = Vec::with_capacity(func_call.params.len());
                for param in func_call.params {
                    caller_registers.push(self.reserve_ident_literal(param, builder, ctx)?);
                }

                ctx.inst_func_call(func_name);

                for register in caller_registers {
                    ctx.inst_drop(register);
                }
            }
            FuncExpr::Assign(_) => unimplemented!(),
        }

        Ok(())
    }

    fn reserve_ident_literal<'a>(
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
}
