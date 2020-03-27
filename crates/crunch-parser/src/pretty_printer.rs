use crate::{parser::*, Interner};

#[cfg(feature = "std")]
use std::io::Write;
#[cfg(feature = "std")]
type Result = std::io::Result<()>;

#[cfg(not(feature = "std"))]
use core::fmt::{Result, Write};

pub struct PrettyPrinter {
    indent_level: usize,
    interner: Interner,
}

macro_rules! resolve {
    ($self:expr, $sym:expr) => {
        $self.interner.read().resolve($sym).unwrap()
    };
}

impl<'expr, 'stmt> PrettyPrinter {
    pub fn new(interner: Interner) -> Self {
        Self {
            indent_level: 0,
            interner,
        }
    }

    pub fn pretty_print(&mut self, f: &mut dyn Write, ast: &[Ast<'expr, 'stmt>]) -> Result {
        for node in ast {
            self.print_ast(f, node)?;
        }

        Ok(())
    }

    fn print_indent(&self, f: &mut dyn Write) -> Result {
        write!(f, "{}", "    ".repeat(self.indent_level))
    }

    fn print_ast(&mut self, f: &mut dyn Write, node: &Ast<'expr, 'stmt>) -> Result {
        match node {
            Ast::Function { .. } => self.print_func(f, node),
            _ => todo!(),
        }
    }

    fn print_func(&mut self, f: &mut dyn Write, node: &Ast<'expr, 'stmt>) -> Result {
        let (decorators, attributes, name, generics, args, returns, body) = if let Ast::Function {
            decorators,
            attributes,
            name,
            generics,
            args,
            returns,
            body,
        } = node
        {
            (decorators, attributes, name, generics, args, returns, body)
        } else {
            panic!()
        };

        for dec in decorators {
            self.print_decorator(f, dec)?;
        }

        for attr in attributes {
            self.print_attr(f, attr)?;
        }

        write!(f, "fn {}", resolve!(self, *name))?;

        if generics.len() != 0 {
            write!(f, "<")?;
            for gen in generics {
                write!(f, "{}", resolve!(self, *gen))?;
            }
            write!(f, ">")?;
        }

        if args.len() != 0 {
            write!(f, "(")?;
            for (name, arg) in args {
                write!(f, "{}: ", resolve!(self, *name))?;
                self.print_ty(f, arg)?;
            }
            write!(f, ")")?;
        } else {
            write!(f, "()")?;
        }

        write!(f, " -> ")?;
        self.print_ty(f, returns)?;
        write!(f, "\n")?;

        self.indent_level += 1;
        for stmt in body {
            self.print_stmt(f, stmt)?;
        }
        self.indent_level -= 1;

        self.print_indent(f)?;
        write!(f, "end\n")
    }

    fn print_decorator(&mut self, f: &mut dyn Write, dec: &Decorator<'expr>) -> Result {
        self.print_indent(f)?;

        if dec.args.len() != 0 {
            write!(f, "@{}(", resolve!(self, dec.name))?;
            for (i, arg) in dec.args.iter().enumerate() {
                self.print_expr(f, arg)?;

                if i != dec.args.len() - 1 {
                    write!(f, ", ")?;
                }
            }
            write!(f, ")\n")
        } else {
            write!(f, "@{}", resolve!(self, dec.name))
        }
    }

    fn print_expr(&mut self, f: &mut dyn Write, expr: &Expr<'expr>) -> Result {
        match &**expr {
            Expression::Variable(v) => write!(f, "{}", resolve!(self, *v)),

            Expression::UnaryExpr(op, expr) => {
                match op {
                    UnaryOperand::Positive => write!(f, "+"),
                    UnaryOperand::Negative => write!(f, "-"),
                    UnaryOperand::Not => write!(f, "!"),
                }?;

                self.print_expr(f, expr)
            }

            Expression::BinaryOp(left, op, right) => {
                self.print_expr(f, left)?;
                write!(
                    f,
                    " {} ",
                    match op {
                        BinaryOperand::Add => "+",
                        BinaryOperand::Sub => "-",
                        BinaryOperand::Mult => "*",
                        BinaryOperand::Div => "/",
                        BinaryOperand::Mod => "%",
                        BinaryOperand::Pow => "**",
                        BinaryOperand::BitAnd => "&",
                        BinaryOperand::BitOr => "|",
                        BinaryOperand::BitXor => "^",
                        BinaryOperand::Shl => "<<",
                        BinaryOperand::Shr => ">>",
                    }
                )?;
                self.print_expr(f, right)
            }

            Expression::InlineConditional {
                true_arm,
                condition,
                false_arm,
            } => {
                self.print_expr(f, true_arm)?;
                write!(f, " if ")?;
                self.print_expr(f, condition)?;
                write!(f, " else ")?;
                self.print_expr(f, false_arm)
            }

            Expression::Parenthesised(expr) => {
                write!(f, "(")?;
                self.print_expr(f, expr)?;
                write!(f, ")")
            }

            Expression::FunctionCall { caller, arguments } => {
                self.print_expr(f, caller)?;
                write!(f, "(")?;
                for arg in arguments {
                    self.print_expr(f, arg)?;
                }
                write!(f, ")")
            }

            Expression::MemberFunctionCall { member, function } => {
                self.print_expr(f, member)?;
                write!(f, ".")?;
                self.print_expr(f, function)
            }

            Expression::Literal(lit) => match lit {
                Literal::I32(i) => write!(f, "{:?}", i),
                Literal::Bool(b) => write!(f, "{:?}", b),
                Literal::String(s) => write!(f, "{:?}", s),
                Literal::ByteVec(v) => write!(f, "{:?}", v),
                Literal::F32(fl) => write!(f, "{:?}", fl),
            },

            Expression::Comparison(left, comp, right) => {
                self.print_expr(f, left)?;
                match comp {
                    ComparisonOperand::Greater => write!(f, ">"),
                    ComparisonOperand::Less => write!(f, "<"),
                    ComparisonOperand::GreaterEqual => write!(f, ">="),
                    ComparisonOperand::LessEqual => write!(f, "<="),
                    ComparisonOperand::Equal => write!(f, "=="),
                    ComparisonOperand::NotEqual => write!(f, "!="),
                }?;
                self.print_expr(f, right)
            }

            Expression::IndexArray { array, index } => {
                self.print_expr(f, array)?;
                write!(f, "[")?;
                self.print_expr(f, index)?;
                write!(f, "]")
            }

            Expression::Array(arr) => {
                write!(f, "[")?;
                for elm in arr {
                    self.print_expr(f, elm)?;
                }
                write!(f, "]")
            }

            Expression::Assignment(left, ty, right) => {
                self.print_expr(f, left)?;
                match ty {
                    AssignmentType::Normal => write!(f, "="),
                    AssignmentType::BinaryOp(op) => write!(
                        f,
                        "{}=",
                        match op {
                            BinaryOperand::Add => "+",
                            BinaryOperand::Sub => "-",
                            BinaryOperand::Mult => "*",
                            BinaryOperand::Div => "/",
                            BinaryOperand::Mod => "%",
                            BinaryOperand::Pow => "**",
                            BinaryOperand::BitAnd => "&",
                            BinaryOperand::BitOr => "|",
                            BinaryOperand::BitXor => "^",
                            BinaryOperand::Shl => "<<",
                            BinaryOperand::Shr => ">>",
                        }
                    ),
                }?;
                self.print_expr(f, right)
            }

            Expression::Range(start, end) => {
                self.print_expr(f, start)?;
                write!(f, "..")?;
                self.print_expr(f, end)
            }
        }
    }

    fn print_ty(&mut self, f: &mut dyn Write, ty: &Type) -> Result {
        match ty {
            Type::Infer => write!(f, "infer"),
            Type::Builtin(b) => match b {
                BuiltinType::Integer => write!(f, "int"),
                BuiltinType::Float => write!(f, "float"),
                BuiltinType::Boolean => write!(f, "bool"),
                BuiltinType::String => write!(f, "str"),
                BuiltinType::Unit => write!(f, "unit"),
                BuiltinType::Vec(ty) => {
                    write!(f, "[")?;
                    self.print_ty(f, ty)?;
                    write!(f, "]")
                }
            },
            Type::Custom(c) => write!(f, "{}", resolve!(self, *c)),
        }
    }

    fn print_attr(&mut self, f: &mut dyn Write, attr: &Attribute) -> Result {
        match attr {
            Attribute::Visibility(vis) => match vis {
                Visibility::Exposed => write!(f, "exposed"),
                Visibility::Package => write!(f, "pkg"),
                Visibility::FileLocal => write!(f, ""),
            },
        }
    }

    fn print_stmt(&mut self, f: &mut dyn Write, stmt: &Stmt<'expr, 'stmt>) -> Result {
        self.print_indent(f)?;

        match &**stmt {
            Statement::Expression(expr) => {
                self.print_expr(f, expr)?;
                write!(f, "\n")
            }

            Statement::Empty => write!(f, "empty\n"),

            Statement::Continue => write!(f, "continue\n"),

            Statement::Return(ret) => {
                if let Some(ret) = ret {
                    write!(f, "return ")?;
                    self.print_expr(f, ret)?;
                    write!(f, "\n")
                } else {
                    write!(f, "return\n")
                }
            }

            Statement::Break(brk) => {
                if let Some(brk) = brk {
                    write!(f, "break ")?;
                    self.print_expr(f, brk)?;
                    write!(f, "\n")
                } else {
                    write!(f, "break\n")
                }
            }

            Statement::Loop { body, then } => {
                write!(f, "loop\n")?;

                self.indent_level += 1;
                for stmt in body {
                    self.print_stmt(f, stmt)?
                }
                self.indent_level -= 1;

                if let Some(then) = then {
                    self.print_indent(f)?;
                    write!(f, "then\n")?;

                    self.indent_level += 1;
                    for stmt in then {
                        self.print_stmt(f, stmt)?
                    }
                    self.indent_level -= 1;
                }

                self.print_indent(f)?;
                write!(f, "end\n")
            }

            Statement::While {
                condition,
                body,
                then,
            } => {
                write!(f, "while ")?;
                self.print_expr(f, condition)?;
                write!(f, "\n")?;

                self.indent_level += 1;
                for stmt in body {
                    self.print_stmt(f, stmt)?
                }
                self.indent_level -= 1;

                if let Some(then) = then {
                    self.print_indent(f)?;
                    write!(f, "then\n")?;

                    self.indent_level += 1;
                    for stmt in then {
                        self.print_stmt(f, stmt)?
                    }
                    self.indent_level -= 1;
                }

                self.print_indent(f)?;
                write!(f, "end\n")
            }

            Statement::For {
                var,
                condition,
                body,
                then,
            } => {
                write!(f, "for ")?;
                self.print_expr(f, var)?;
                write!(f, " in ")?;
                self.print_expr(f, condition)?;
                write!(f, "\n")?;

                self.indent_level += 1;
                for stmt in body {
                    self.print_stmt(f, stmt)?
                }
                self.indent_level -= 1;

                if let Some(then) = then {
                    self.print_indent(f)?;
                    write!(f, "then\n")?;

                    self.indent_level += 1;
                    for stmt in then {
                        self.print_stmt(f, stmt)?
                    }
                    self.indent_level -= 1;
                }

                self.print_indent(f)?;
                write!(f, "end\n")
            }

            Statement::VarDeclaration(name, expr) => {
                write!(f, "let {} = ", resolve!(self, *name))?;
                self.print_expr(f, expr)?;
                write!(f, "\n")
            }

            Statement::If {
                condition,
                body,
                arm,
            } => {
                write!(f, "if ")?;
                self.print_expr(f, condition)?;
                write!(f, "\n")?;

                self.indent_level += 1;
                for stmt in body {
                    self.print_stmt(f, stmt)?;
                }
                self.indent_level -= 1;

                if let Some(stmt) = arm {
                    write!(f, "else ")?;
                    self.print_stmt(f, stmt)?;
                }

                self.print_indent(f)?;
                write!(f, "end\n")
            }

            Statement::Match { var, arms } => {
                write!(f, "match ")?;
                self.print_expr(f, var)?;
                write!(f, "\n")?;

                self.indent_level += 1;
                for (name, whre, body) in arms {
                    self.print_indent(f)?;

                    write!(f, "{} ", resolve!(self, *name))?;
                    if let Some(whre) = whre {
                        write!(f, "where ")?;
                        self.print_expr(f, whre)?;
                        write!(f, " ")?;
                    }
                    write!(f, "=>\n")?;

                    self.indent_level += 1;
                    for stmt in body {
                        self.print_stmt(f, stmt)?;
                    }
                    self.indent_level -= 1;
                }
                self.indent_level -= 1;

                self.print_indent(f)?;
                write!(f, "end\n")
            }
        }
    }
}
