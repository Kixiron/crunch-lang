use crate::{parser::*, Interner};

use lasso::SmallSpur;

#[cfg(feature = "no-std")]
use alloc::{format, vec::Vec};
use core::fmt::{Result, Write};

pub struct PrettyPrinter {
    indent_level: usize,
    interner: Interner,
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

    pub(crate) fn print_ast(&mut self, f: &mut dyn Write, node: &Ast<'expr, 'stmt>) -> Result {
        match node {
            Ast::Function {
                decorators,
                attributes,
                name,
                generics,
                args,
                returns,
                body,
            } => self.print_func(
                f, decorators, attributes, name, generics, args, returns, body,
            ),
            Ast::Type {
                decorators,
                attributes,
                name,
                generics,
                members,
                methods,
            } => self.print_type(f, decorators, attributes, name, generics, members, methods),
            Ast::Enum {
                decorators,
                attributes,
                name,
                generics,
                variants,
            } => self.print_enum(f, decorators, attributes, name, generics, variants),
            Ast::Trait {
                decorators,
                attributes,
                name,
                generics,
                methods,
            } => self.print_trait(f, decorators, attributes, name, generics, methods),
            Ast::Import {
                file,
                dest,
                exposes,
            } => self.print_import(f, file, dest, exposes),
        }
    }

    fn print_import(
        &mut self,
        f: &mut dyn Write,
        file: &SmallSpur,
        dest: &ImportDest,
        exposes: &ImportExposure,
    ) -> Result {
        write!(
            f,
            "import{} \"{}\"",
            match dest {
                ImportDest::NativeLib => " lib",
                ImportDest::Package => " pkg",
                ImportDest::Relative => "",
            },
            self.interner.resolve(file)
        )?;

        match exposes {
            ImportExposure::All => write!(f, " exposing *")?,
            ImportExposure::None(name) => write!(f, " as {}", self.interner.resolve(name))?,
            ImportExposure::Members(members) => {
                write!(f, " exposing ")?;

                for (i, (member, alias)) in members.iter().enumerate() {
                    write!(f, "{}", self.interner.resolve(member))?;

                    if let Some(alias) = alias {
                        write!(f, " as {}", self.interner.resolve(alias))?;
                    }

                    if i != members.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
            }
        }

        writeln!(f)
    }

    fn print_trait(
        &mut self,
        f: &mut dyn Write,
        decorators: &Vec<Decorator<'expr>>,
        attributes: &Vec<Attribute>,
        name: &SmallSpur,
        generics: &Vec<SmallSpur>,
        methods: &Vec<Ast<'expr, 'stmt>>,
    ) -> Result {
        for dec in decorators {
            self.print_decorator(f, dec)?;
            writeln!(f)?;
        }

        self.print_indent(f)?;
        for attr in attributes {
            self.print_attr(f, attr)?;
        }

        write!(f, "trait {}", self.interner.resolve(name))?;

        if !generics.is_empty() {
            write!(f, "<")?;
            for (i, gen) in generics.iter().enumerate() {
                write!(f, "{}", self.interner.resolve(gen))?;

                if i != generics.len() - 1 {
                    write!(f, ", ")?;
                }
            }
            write!(f, ">")?;
        }
        writeln!(f)?;

        self.indent_level += 1;
        for method in methods {
            if let Ast::Function {
                decorators,
                attributes,
                name,
                generics,
                args,
                returns,
                body,
            } = method
            {
                self.print_func(
                    f, decorators, attributes, name, generics, args, returns, body,
                )?;
            } else {
                panic!();
            }
        }
        self.indent_level -= 1;

        writeln!(f, "end")
    }

    fn print_enum(
        &mut self,
        f: &mut dyn Write,
        decorators: &Vec<Decorator<'expr>>,
        attributes: &Vec<Attribute>,
        name: &SmallSpur,
        generics: &Vec<SmallSpur>,
        variants: &Vec<EnumVariant<'expr>>,
    ) -> Result {
        for dec in decorators {
            self.print_decorator(f, dec)?;
            writeln!(f)?;
        }

        self.print_indent(f)?;
        for attr in attributes {
            self.print_attr(f, attr)?;
        }

        write!(f, "enum {}", self.interner.resolve(name))?;

        if !generics.is_empty() {
            write!(f, "<")?;
            for (i, gen) in generics.iter().enumerate() {
                write!(f, "{}", self.interner.resolve(gen))?;

                if i != generics.len() - 1 {
                    write!(f, ", ")?;
                }
            }
            write!(f, ">")?;
        }
        writeln!(f)?;

        self.indent_level += 1;
        for variant in variants {
            match variant {
                EnumVariant::Unit { name, decorators } => {
                    for dec in decorators {
                        self.print_decorator(f, dec)?;
                        writeln!(f)?;
                    }
                    self.print_indent(f)?;
                    writeln!(f, "{}", self.interner.resolve(name))?;
                }

                EnumVariant::Tuple {
                    name,
                    elements,
                    decorators,
                } => {
                    for dec in decorators {
                        self.print_decorator(f, dec)?;
                        writeln!(f)?;
                    }
                    self.print_indent(f)?;
                    write!(f, "{}(", self.interner.resolve(name))?;
                    for (i, ty) in elements.iter().enumerate() {
                        self.print_ty(f, ty)?;

                        if i != elements.len() - 1 {
                            write!(f, ", ")?;
                        }
                    }
                    writeln!(f, ")")?;
                }
            }
        }
        self.indent_level -= 1;

        writeln!(f, "end")
    }

    fn print_type(
        &mut self,
        f: &mut dyn Write,
        decorators: &Vec<Decorator<'expr>>,
        attributes: &Vec<Attribute>,
        name: &SmallSpur,
        generics: &Vec<SmallSpur>,
        members: &Vec<TypeMember<'expr>>,
        methods: &Vec<Ast<'expr, 'stmt>>,
    ) -> Result {
        for dec in decorators {
            self.print_decorator(f, dec)?;
            writeln!(f)?;
        }

        self.print_indent(f)?;
        for attr in attributes {
            self.print_attr(f, attr)?;
        }

        write!(f, "type {}", self.interner.resolve(name))?;

        if !generics.is_empty() {
            write!(f, "<")?;
            for (i, gen) in generics.iter().enumerate() {
                write!(f, "{}", self.interner.resolve(gen))?;

                if i != generics.len() - 1 {
                    write!(f, ", ")?;
                }
            }
            write!(f, ">")?;
        }
        writeln!(f)?;

        self.indent_level += 1;
        for TypeMember {
            decorators,
            attributes,
            name,
            ty,
        } in members
        {
            for dec in decorators {
                self.print_decorator(f, dec)?;
                writeln!(f)?;
            }

            self.print_indent(f)?;
            for attr in attributes {
                self.print_attr(f, attr)?;
            }

            write!(f, "{}: ", self.interner.resolve(name))?;
            self.print_ty(f, ty)?;
            writeln!(f)?;
        }

        for node in methods {
            if let Ast::Function {
                decorators,
                attributes,
                name,
                generics,
                args,
                returns,
                body,
            } = node
            {
                self.print_func(
                    f, decorators, attributes, name, generics, args, returns, body,
                )?;
            } else {
                panic!();
            }
        }
        self.indent_level -= 1;

        writeln!(f, "end")
    }

    fn print_func(
        &mut self,
        f: &mut dyn Write,
        decorators: &Vec<Decorator<'expr>>,
        attributes: &Vec<Attribute>,
        name: &SmallSpur,
        generics: &Vec<SmallSpur>,
        args: &Vec<(SmallSpur, Type)>,
        returns: &Type,
        body: &Vec<Stmt<'expr, 'stmt>>,
    ) -> Result {
        for dec in decorators {
            self.print_decorator(f, dec)?;
            writeln!(f)?;
        }

        self.print_indent(f)?;
        for attr in attributes {
            self.print_attr(f, attr)?;
        }

        write!(f, "fn {}", self.interner.resolve(name))?;

        if !generics.is_empty() {
            write!(f, "<")?;
            for (i, gen) in generics.iter().enumerate() {
                write!(f, "{}", self.interner.resolve(gen))?;

                if i != generics.len() - 1 {
                    write!(f, ", ")?;
                }
            }
            write!(f, ">")?;
        }

        if !args.is_empty() {
            write!(f, "(")?;

            let args = args
                .iter()
                .map(|(name, arg)| {
                    let mut param = format!("{}: ", self.interner.resolve(name));
                    self.print_ty(&mut param, arg).unwrap();

                    param
                })
                .collect::<Vec<_>>()
                .join(", ");
            write!(f, "{})", args)?;
        } else {
            write!(f, "()")?;
        }

        write!(f, " -> ")?;
        self.print_ty(f, returns)?;
        writeln!(f)?;

        self.indent_level += 1;
        for stmt in body {
            self.print_stmt(f, stmt)?;
        }
        self.indent_level -= 1;

        self.print_indent(f)?;
        writeln!(f, "end")
    }

    fn print_decorator(&mut self, f: &mut dyn Write, dec: &Decorator<'expr>) -> Result {
        self.print_indent(f)?;

        if !dec.args.is_empty() {
            write!(f, "@{}(", self.interner.resolve(&dec.name))?;
            for (i, arg) in dec.args.iter().enumerate() {
                self.print_expr(f, arg)?;

                if i != dec.args.len() - 1 {
                    write!(f, ", ")?;
                }
            }
            write!(f, ")")
        } else {
            write!(f, "@{}", self.interner.resolve(&dec.name))
        }
    }

    pub(crate) fn print_expr(&mut self, f: &mut dyn Write, expr: &Expr<'expr>) -> Result {
        match &**expr {
            Expression::Variable(v) => write!(f, "{}", self.interner.resolve(v)),

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
                for (i, arg) in arguments.iter().enumerate() {
                    self.print_expr(f, arg)?;

                    if i != arguments.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ")")
            }

            Expression::MemberFunctionCall { member, function } => {
                self.print_expr(f, member)?;
                write!(f, ".")?;
                self.print_expr(f, function)
            }

            Expression::Literal(lit) => match lit {
                Literal::I32(i) => write!(f, "{}", i),
                Literal::Bool(b) => write!(f, "{}", b),
                Literal::String(s) => write!(f, "\"{}\"", s),
                Literal::ByteVec(v) => write!(f, "{:?}", v),
                Literal::F32(fl) => write!(f, "{}", fl),
            },

            Expression::Comparison(left, comp, right) => {
                self.print_expr(f, left)?;
                match comp {
                    ComparisonOperand::Greater => write!(f, " > "),
                    ComparisonOperand::Less => write!(f, " < "),
                    ComparisonOperand::GreaterEqual => write!(f, " >= "),
                    ComparisonOperand::LessEqual => write!(f, " <= "),
                    ComparisonOperand::Equal => write!(f, " == "),
                    ComparisonOperand::NotEqual => write!(f, " != "),
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
                for (i, elm) in arr.iter().enumerate() {
                    self.print_expr(f, elm)?;

                    if i != arr.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "]")
            }

            Expression::Assignment(left, ty, right) => {
                self.print_expr(f, left)?;
                match ty {
                    AssignmentType::Normal => write!(f, " := "),
                    AssignmentType::BinaryOp(op) => write!(
                        f,
                        " {}= ",
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
            Type::Custom(c) => write!(f, "{}", self.interner.resolve(c)),
        }
    }

    fn print_attr(&mut self, f: &mut dyn Write, attr: &Attribute) -> Result {
        match attr {
            Attribute::Visibility(vis) => match vis {
                Visibility::Exposed => write!(f, "exposed "),
                Visibility::Package => write!(f, "pkg "),
                Visibility::FileLocal => write!(f, ""),
            },
        }
    }

    pub(crate) fn print_stmt(&mut self, f: &mut dyn Write, stmt: &Stmt<'expr, 'stmt>) -> Result {
        self.print_indent(f)?;

        match &**stmt {
            Statement::Expression(expr) => {
                self.print_expr(f, expr)?;
                writeln!(f)
            }

            Statement::Empty => writeln!(f, "empty"),

            Statement::Continue => writeln!(f, "continue"),

            Statement::Return(ret) => {
                if let Some(ret) = ret {
                    write!(f, "return ")?;
                    self.print_expr(f, ret)?;
                    writeln!(f)
                } else {
                    writeln!(f, "return")
                }
            }

            Statement::Break(brk) => {
                if let Some(brk) = brk {
                    write!(f, "break ")?;
                    self.print_expr(f, brk)?;
                    writeln!(f)
                } else {
                    writeln!(f, "break")
                }
            }

            Statement::Loop { body, then } => {
                writeln!(f, "loop")?;

                self.indent_level += 1;
                for stmt in body {
                    self.print_stmt(f, stmt)?
                }
                self.indent_level -= 1;

                if let Some(then) = then {
                    self.print_indent(f)?;
                    writeln!(f, "then")?;

                    self.indent_level += 1;
                    for stmt in then {
                        self.print_stmt(f, stmt)?
                    }
                    self.indent_level -= 1;
                }

                self.print_indent(f)?;
                writeln!(f, "end")
            }

            Statement::While {
                condition,
                body,
                then,
            } => {
                write!(f, "while ")?;
                self.print_expr(f, condition)?;
                writeln!(f)?;

                self.indent_level += 1;
                for stmt in body {
                    self.print_stmt(f, stmt)?
                }
                self.indent_level -= 1;

                if let Some(then) = then {
                    self.print_indent(f)?;
                    writeln!(f, "then")?;

                    self.indent_level += 1;
                    for stmt in then {
                        self.print_stmt(f, stmt)?
                    }
                    self.indent_level -= 1;
                }

                self.print_indent(f)?;
                writeln!(f, "end")
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
                writeln!(f)?;

                self.indent_level += 1;
                for stmt in body {
                    self.print_stmt(f, stmt)?
                }
                self.indent_level -= 1;

                if let Some(then) = then {
                    self.print_indent(f)?;
                    writeln!(f, "then")?;

                    self.indent_level += 1;
                    for stmt in then {
                        self.print_stmt(f, stmt)?
                    }
                    self.indent_level -= 1;
                }

                self.print_indent(f)?;
                writeln!(f, "end")
            }

            Statement::VarDeclaration(name, expr) => {
                write!(f, "let {} = ", self.interner.resolve(name))?;
                self.print_expr(f, expr)?;
                writeln!(f)
            }

            Statement::If {
                condition,
                body,
                arm,
            } => {
                write!(f, "if ")?;
                self.print_expr(f, condition)?;
                writeln!(f)?;

                self.indent_level += 1;
                for stmt in body {
                    self.print_stmt(f, stmt)?;
                }
                self.indent_level -= 1;

                self.print_indent(f)?;
                if let Some(stmt) = arm {
                    write!(f, "else ")?;
                    self.print_stmt(f, stmt)
                } else {
                    writeln!(f, "end")
                }
            }

            Statement::Match { var, arms } => {
                write!(f, "match ")?;
                self.print_expr(f, var)?;
                writeln!(f)?;

                self.indent_level += 1;
                for (name, whre, body) in arms {
                    self.print_indent(f)?;

                    write!(f, "{} ", self.interner.resolve(name))?;
                    if let Some(whre) = whre {
                        write!(f, "where ")?;
                        self.print_expr(f, whre)?;
                        write!(f, " ")?;
                    }
                    writeln!(f, "=>")?;

                    self.indent_level += 1;
                    for stmt in body {
                        self.print_stmt(f, stmt)?;
                    }
                    self.indent_level -= 1;

                    self.print_indent(f)?;
                    writeln!(f, "end")?
                }
                self.indent_level -= 1;

                self.print_indent(f)?;
                writeln!(f, "end")
            }
        }
    }
}
