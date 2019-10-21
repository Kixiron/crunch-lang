use crate::{
    instruction::{RuntimeError, RuntimeErrorTy},
    parser::*,
    Instruction, Options, Value, NUMBER_REGISTERS,
};
use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
};

struct Scope<'a> {
    variables: HashMap<String, (u32, Type<'a>)>,
    registers: [Option<u32>; NUMBER_REGISTERS],
}

impl<'a> Scope<'a> {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
            registers: [None; NUMBER_REGISTERS],
        }
    }
}

type Result<T> = std::result::Result<T, RuntimeError>;

/// Convenience macro to get the next available register's position and reserve it
macro_rules! pos {
    ($var:expr, $addr:expr) => {{
        let pos = $var.iter().position(Option::is_none).unwrap();
        $var[pos] = Some($addr);
        (pos as u8).into()
    }};
}

macro_rules! next_gc_id {
    ($gc:expr) => {{
        *$gc.iter().last().unwrap_or(&0) as u32
    }};
}

struct InterpOptions {
    fault_tolerant: bool,
}

impl From<&Options> for InterpOptions {
    fn from(options: &Options) -> Self {
        Self {
            fault_tolerant: options.fault_tolerant,
        }
    }
}

pub struct Interpreter<'a> {
    ast: Vec<Node<'a>>,
    scopes: Vec<Scope<'a>>,
    current_scope: Scope<'a>,
    gc: HashSet<usize>,
    functions: HashMap<String, (Vec<Instruction>, Option<usize>)>,
    current_function: Vec<Instruction>,
    options: InterpOptions,
}

impl<'a> Interpreter<'a> {
    pub fn new(ast: Vec<Node<'a>>, options: &Options) -> Self {
        Self {
            ast,
            scopes: Vec::new(),
            current_scope: Scope::new(),
            gc: HashSet::new(),
            functions: HashMap::new(),
            current_function: Vec::new(),
            options: InterpOptions::from(options),
        }
    }

    /// Interpret the contained ast and return the instructions
    pub fn interpret(mut self) -> Result<(Vec<Instruction>, Vec<Vec<Instruction>>)> {
        for node_index in 0..self.ast.len() - 1 {
            // UNSAFE: Safe because node_index will never be greater than ast.len() - 1
            // or less than zero, and therefore should never be out of bounds
            match unsafe { self.ast.get_unchecked(node_index) } {
                Node::Func(_) => self.interp_func(node_index)?,
            }
        }

        // Create fresh functions hashmap
        let mut functions = HashMap::new();

        // Swap the filled and fresh functions
        std::mem::swap(&mut self.functions, &mut functions);

        // Turn the functions into a Vector of the instructions paired with their function indices,
        // discarding the function names
        let mut functions = functions
            .into_iter()
            .map(|(_key, val)| val)
            .collect::<Vec<(Vec<Instruction>, Option<usize>)>>();

        // Sort the functions by their indices
        functions.sort_by_key(|(_, index)| *index);

        // If there are no functions, just make one with only the Halt instruction
        if functions.is_empty() {
            let functions = (vec![Instruction::Halt], Vec::<Vec<Instruction>>::new());
            trace!("Interp Output: {:?}", &functions);
            return Ok(functions);
        }

        // Get the man function from functions
        let main_func = {
            let pos = if let Some(pos) = functions.iter().position(|(_, index)| index.is_none()) {
                pos
            } else {
                return Err(RuntimeError {
                    ty: RuntimeErrorTy::MissingMain,
                    message: "Missing Main function".to_string(),
                });
            };

            let mut main = functions.remove(pos).0;
            // Push the Halt instruction onto Main
            main.push(Instruction::Halt);
            main
        };

        // Filter out the other functions named main, will cause issues
        // FIXME: Handle extra main functions
        let functions = functions
            .into_iter()
            .filter_map(
                |(vec, index)| {
                    if index.is_some() {
                        Some(vec)
                    } else {
                        None
                    }
                },
            )
            .collect::<Vec<Vec<Instruction>>>();

        trace!("Interp Output: {:?}", (&main_func, &functions));

        Ok((main_func, functions))
    }

    /// Interpret a function
    fn interp_func(&mut self, node_index: usize) -> Result<()> {
        #[allow(irrefutable_let_patterns)] // Note: Will not always be irrefutable
        let func = if let Node::Func(func) = self.ast.remove(node_index) {
            func
        } else {
            unreachable!("Already a confirmed function");
        };

        // For each expression in the function, evaluate it into instructions
        for expr in func.body {
            match expr.expr {
                // Bind a variable to a value
                FuncExpr::Binding(binding) => {
                    let (val, gc_id) = {
                        // Insert the variable into the gc
                        let gc_id = next_gc_id!(self.gc);

                        // Add the variable to the current scope
                        self.current_scope.variables.insert(
                            match binding.name.name {
                                Cow::Owned(s) => s,
                                Cow::Borrowed(s) => s.to_owned(),
                            },
                            (gc_id, binding.ty),
                        );

                        // Create the value of the variable
                        let val = match binding.val {
                            BindingVal::Literal(literal) => match literal.val {
                                LiteralInner::Bool(b) => Value::Bool(b),
                                LiteralInner::Int(i) => Value::Int(i),
                                _ => unimplemented!(),
                            },
                            _ => unimplemented!(),
                        };

                        (val, gc_id)
                    };

                    // Add the cache instruction to the current function
                    self.current_function.push(Instruction::Cache(gc_id, val));
                }

                // Compiler builtin functions
                FuncExpr::Builtin(builtin) => match builtin {
                    // GC Collection cycle
                    Builtin::Collect => {
                        self.current_function.push(Instruction::Collect);
                    }

                    // Halt execution
                    Builtin::Halt => {
                        self.current_function.push(Instruction::Halt);
                    }

                    // Print values
                    Builtin::Print(params) => {
                        for param in params {
                            match param {
                                // For literals fed into the print function, load them, print them, and drop them
                                IdentLiteral::Literal(literal) => {
                                    let (val, gc_id) = (
                                        match literal.val {
                                            LiteralInner::Bool(b) => Value::Bool(b),
                                            LiteralInner::Int(i) => Value::Int(i),
                                            _ => unimplemented!(),
                                        },
                                        next_gc_id!(self.gc),
                                    );

                                    let reg_addr = pos!(self.current_scope.registers, gc_id);

                                    self.current_function.extend_from_slice(&[
                                        Instruction::Cache(gc_id, val),
                                        Instruction::Load(gc_id, reg_addr),
                                        Instruction::Print(reg_addr),
                                        Instruction::DropReg(reg_addr),
                                        Instruction::Drop(gc_id),
                                    ]);

                                    self.current_scope.registers[*reg_addr as usize] = None;
                                }

                                // For existing variables, fetch them and print them
                                IdentLiteral::Variable(ident) => {
                                    // Get the gc address, variable type, and fault status of fetching the requested variable
                                    // If a fault occurs and fault_tolerant is true, then a null value will be used in place of
                                    // the requested variable. If fault_tolerant is false, then a Runtime Error will be thrown
                                    let (var_id, _var_type, faulted) = if let Some(var) =
                                        self.current_scope.variables.get(&*ident.name)
                                    {
                                        (var.0, var.1.clone(), false)
                                    } else {
                                        // If fault tolerant, a fault just occurred, so pretend it didn't
                                        if self.options.fault_tolerant {
                                            let gc_id = next_gc_id!(self.gc);

                                            self.current_function
                                                .push(Instruction::Cache(gc_id, Value::None));

                                            (gc_id, Type::Void, true)

                                        // If we aren't fault tolerant, scream
                                        } else {
                                            return Err(RuntimeError {
                                                ty: RuntimeErrorTy::NullVar,
                                                message: format!(
                                                    "The variable {:?} does not exist",
                                                    &*ident.name
                                                ),
                                            });
                                        }
                                    };

                                    // If the value is currently loaded into a register, print it directly
                                    if let Some(reg_addr) = self
                                        .current_scope
                                        .registers
                                        .iter()
                                        .position(|reg| *reg == Some(var_id))
                                    {
                                        let reg_addr = (reg_addr as u8).into();
                                        self.current_function.push(Instruction::Print(reg_addr));

                                        // Note: If the value was already loaded, we probably don't want to drop it yet

                                        // If the value was magically loaded in fault-tolerant mode with a fault,
                                        // drop it from the registers
                                        if self.options.fault_tolerant && faulted {
                                            self.current_function
                                                .push(Instruction::DropReg(reg_addr));

                                            self.current_scope.registers[*reg_addr as usize] = None;
                                        }

                                    // If the value is not currently loaded, load, print, and drop it from the registers
                                    } else {
                                        let reg_addr = pos!(self.current_scope.registers, var_id);

                                        self.current_function.extend_from_slice(&[
                                            Instruction::Load(var_id, reg_addr),
                                            Instruction::Print(reg_addr),
                                            Instruction::DropReg(reg_addr),
                                        ]);

                                        self.current_scope.registers[*reg_addr as usize] = None;
                                    }

                                    // Drop the null value from the registers if a fault occurred
                                    if self.options.fault_tolerant && faulted {
                                        self.current_function.push(Instruction::Drop(var_id));
                                    }
                                }
                            }
                        }
                    }
                },

                FuncExpr::FuncCall(_) | FuncExpr::Assign(_) => unimplemented!(),
            }
        }

        // Enter a new scope for the next function
        self.enter_scope();

        Ok(())
    }

    /// Enters a new scope
    fn enter_scope(&mut self) {
        let mut new = Scope::new();
        std::mem::swap(&mut new, &mut self.current_scope);
        self.scopes.push(new);
    }
}
