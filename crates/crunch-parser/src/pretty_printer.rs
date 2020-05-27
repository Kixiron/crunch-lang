use crate::{
    context::Context,
    parser::{
        Alias, AssignmentType, Ast, Attribute, BinaryOperand, Binding, ComparisonOperand,
        Decorator, Enum, EnumVariant, Expr, ExtendBlock, Function, Import, ImportDest,
        ImportExposure, Literal, Pattern, Signedness, Stmt, Trait, Type, TypeDecl, TypeMember,
        UnaryOperand, Visibility,
    },
};

#[cfg(feature = "no-std")]
use alloc::{format, vec::Vec};
use core::fmt::{Result, Write};

#[derive(Debug)]
pub struct PrettyPrinter<'ctx> {
    indent_level: usize,
    context: &'ctx Context<'ctx>,
}

impl<'ctx> PrettyPrinter<'ctx> {
    pub fn new(context: &'ctx Context<'ctx>) -> Self {
        Self {
            indent_level: 0,
            context,
        }
    }

    pub fn pretty_print(&mut self, f: &mut dyn Write, ast: &'ctx [Ast<'ctx>]) -> Result {
        for node in ast {
            self.print_ast(f, node)?;
        }

        Ok(())
    }

    fn print_indent(&self, f: &mut dyn Write) -> Result {
        write!(f, "{}", "    ".repeat(self.indent_level))
    }

    pub(crate) fn print_ast(&mut self, f: &mut dyn Write, node: &'ctx Ast<'ctx>) -> Result {
        match node {
            Ast::Function(func) => self.print_func(f, func.data()),
            Ast::Type(ty) => self.print_type(f, ty.data()),
            Ast::Enum(enum_decl) => self.print_enum(f, enum_decl.data()),
            Ast::Trait(trait_decl) => self.print_trait(f, trait_decl.data()),
            Ast::Import(import) => self.print_import(f, import.data()),
            Ast::Alias(alias) => self.print_alias(f, &*alias),
            Ast::ExtendBlock(block) => self.print_extend_block(f, &*block),
        }
    }

    fn print_extend_block(
        &mut self,
        f: &mut dyn Write,
        ExtendBlock {
            target,
            extender,
            nodes,
        }: &'ctx ExtendBlock,
    ) -> Result {
        self.print_indent(f)?;
        f.write_str("extend ")?;
        self.print_ty(f, target)?;

        if let Some(extender) = extender {
            f.write_str(" with ")?;
            self.print_ty(f, extender)?;
        }
        writeln!(f)?;

        self.indent_level += 1;
        for node in nodes {
            self.print_ast(f, node)?;
        }
        self.indent_level -= 1;

        writeln!(f, "end")
    }

    fn print_alias(
        &mut self,
        f: &mut dyn Write,
        Alias {
            alias,
            actual,
            decorators,
            attrs,
        }: &'ctx Alias,
    ) -> Result {
        self.print_indent(f)?;
        for dec in decorators {
            self.print_decorator(f, dec.data())?;
            writeln!(f)?;
        }

        for attr in attrs {
            self.print_attr(f, *attr.data())?;
        }

        f.write_str("alias ")?;
        self.print_ty(f, alias)?;
        f.write_str(" = ")?;
        self.print_ty(f, actual)?;
        writeln!(f)
    }

    fn print_import(
        &mut self,
        f: &mut dyn Write,
        Import {
            file,
            dest,
            exposes,
        }: &Import,
    ) -> Result {
        write!(
            f,
            "import{} \"{}\"",
            match dest {
                ImportDest::NativeLib => " lib",
                ImportDest::Package => " pkg",
                ImportDest::Relative => "",
            },
            self.context.resolve(*file.data())
        )?;

        match exposes {
            ImportExposure::All => f.write_str(" exposing *")?,
            ImportExposure::None(name) => write!(f, " as {}", self.context.resolve(*name.data()))?,
            ImportExposure::Members(members) => {
                f.write_str(" exposing ")?;

                for (i, member) in members.iter().enumerate() {
                    write!(f, "{}", self.context.resolve(member.data().0))?;

                    if let Some(alias) = member.data().1 {
                        write!(f, " as {}", self.context.resolve(alias))?;
                    }

                    if i != members.len() - 1 {
                        f.write_str(", ")?;
                    }
                }
            }
        }

        writeln!(f)
    }

    fn print_trait(
        &mut self,
        f: &mut dyn Write,
        Trait {
            decorators,
            attrs,
            name,
            generics,
            methods,
        }: &'ctx Trait,
    ) -> Result {
        for dec in decorators {
            self.print_decorator(f, dec.data())?;
            writeln!(f)?;
        }

        self.print_indent(f)?;
        for attr in attrs {
            self.print_attr(f, *attr.data())?;
        }

        write!(f, "trait {}", self.context.resolve(*name))?;

        if !generics.is_empty() {
            f.write_str("[")?;
            for (i, gen) in generics.iter().enumerate() {
                self.print_ty(f, gen.data())?;

                if i != generics.len() - 1 {
                    f.write_str(", ")?;
                }
            }
            f.write_str("]")?;
        }
        writeln!(f)?;

        self.indent_level += 1;
        for method in methods {
            self.print_func(f, method.data())?;
        }
        self.indent_level -= 1;

        writeln!(f, "end")
    }

    fn print_enum(
        &mut self,
        f: &mut dyn Write,
        Enum {
            decorators,
            attrs,
            name,
            generics,
            variants,
        }: &'ctx Enum,
    ) -> Result {
        for dec in decorators {
            self.print_decorator(f, dec.data())?;
            writeln!(f)?;
        }

        self.print_indent(f)?;
        for attr in attrs {
            self.print_attr(f, *attr.data())?;
        }

        write!(f, "enum {}", self.context.resolve(*name))?;

        if !generics.is_empty() {
            f.write_str("[")?;
            for (i, gen) in generics.iter().enumerate() {
                self.print_ty(f, gen.data())?;
                if i != generics.len() - 1 {
                    f.write_str(", ")?;
                }
            }
            f.write_str("]")?;
        }
        writeln!(f)?;

        self.indent_level += 1;
        for variant in variants {
            match variant.data() {
                EnumVariant::Unit { name, decorators } => {
                    for dec in decorators {
                        self.print_decorator(f, dec.data())?;
                        writeln!(f)?;
                    }
                    self.print_indent(f)?;
                    writeln!(f, "{}", self.context.resolve(*name))?;
                }

                EnumVariant::Tuple {
                    name,
                    elements,
                    decorators,
                } => {
                    for dec in decorators {
                        self.print_decorator(f, dec.data())?;
                        writeln!(f)?;
                    }
                    self.print_indent(f)?;
                    write!(f, "{}(", self.context.resolve(*name))?;
                    for (i, ty) in elements.iter().enumerate() {
                        self.print_ty(f, ty.data())?;

                        if i != elements.len() - 1 {
                            f.write_str(", ")?;
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
        TypeDecl {
            decorators,
            attrs,
            name,
            generics,
            members,
        }: &'ctx TypeDecl,
    ) -> Result {
        for dec in decorators {
            self.print_decorator(f, dec.data())?;
            writeln!(f)?;
        }

        self.print_indent(f)?;
        for attr in attrs {
            self.print_attr(f, *attr.data())?;
        }

        write!(f, "type {}", self.context.resolve(*name))?;

        if !generics.is_empty() {
            f.write_str("[")?;
            for (i, gen) in generics.iter().enumerate() {
                self.print_ty(f, gen.data())?;

                if i != generics.len() - 1 {
                    f.write_str(", ")?;
                }
            }
            f.write_str("]")?;
        }
        writeln!(f)?;

        self.indent_level += 1;
        for member in members {
            let TypeMember {
                decorators,
                attrs,
                name,
                ty,
            } = member.data();

            for dec in decorators {
                self.print_decorator(f, dec.data())?;
                writeln!(f)?;
            }

            self.print_indent(f)?;
            for attr in attrs {
                self.print_attr(f, *attr.data())?;
            }

            write!(f, "{}: ", self.context.resolve(*name))?;
            self.print_ty(f, ty.data())?;
            writeln!(f)?;
        }
        self.indent_level -= 1;

        writeln!(f, "end")
    }

    fn print_func(
        &mut self,
        f: &mut dyn Write,
        Function {
            decorators,
            attrs,
            name,
            args,
            returns,
            body,
            ..
        }: &'ctx Function,
    ) -> Result {
        for dec in decorators {
            self.print_decorator(f, dec.data())?;
            writeln!(f)?;
        }

        self.print_indent(f)?;
        for attr in attrs {
            self.print_attr(f, *attr.data())?;
        }

        write!(f, "fn {}", self.context.resolve(*name))?;

        if !args.is_empty() {
            f.write_str("(")?;

            let args = args
                .iter()
                .map(|arg| {
                    let mut param = format!(
                        "{}{}: ",
                        if arg.data().comptime { "comptime " } else { "" },
                        self.context.resolve(*arg.data().name.data())
                    );
                    self.print_ty(&mut param, arg.data().ty.data()).unwrap();

                    param
                })
                .collect::<Vec<_>>()
                .join(", ");
            write!(f, "{})", args)?;
        } else {
            f.write_str("()")?;
        }

        f.write_str(" -> ")?;
        self.print_ty(f, returns.data())?;
        writeln!(f)?;

        self.indent_level += 1;
        for stmt in body {
            self.print_stmt(f, stmt)?;
        }
        self.indent_level -= 1;

        self.print_indent(f)?;
        writeln!(f, "end")
    }

    fn print_decorator(&mut self, f: &mut dyn Write, dec: &'ctx Decorator<'ctx>) -> Result {
        self.print_indent(f)?;

        if !dec.args.is_empty() {
            write!(f, "@{}(", self.context.resolve(*dec.name.data()))?;
            for (i, arg) in dec.args.iter().enumerate() {
                self.print_expr(f, arg)?;

                if i != dec.args.len() - 1 {
                    f.write_str(", ")?;
                }
            }
            f.write_str(")")
        } else {
            write!(f, "@{}", self.context.resolve(*dec.name.data()))
        }
    }

    pub(crate) fn print_expr(&mut self, f: &mut dyn Write, expr: &Expr<'ctx>) -> Result {
        match &*expr {
            Expr::Variable(v) => write!(f, "{}", self.context.resolve(*v)),

            Expr::UnaryExpr(op, expr) => {
                match op {
                    UnaryOperand::Positive => f.write_str("+"),
                    UnaryOperand::Negative => f.write_str("-"),
                    UnaryOperand::Not => f.write_str("!"),
                }?;

                self.print_expr(f, expr)
            }

            Expr::BinaryOp(left, op, right) => {
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

            Expr::InlineConditional {
                true_arm,
                condition,
                false_arm,
            } => {
                self.print_expr(f, true_arm)?;
                f.write_str(" if ")?;
                self.print_expr(f, condition)?;
                f.write_str(" else ")?;
                self.print_expr(f, false_arm)
            }

            Expr::Parenthesised(expr) => {
                f.write_str("(")?;
                self.print_expr(f, expr)?;
                f.write_str(")")
            }

            Expr::FunctionCall { caller, arguments } => {
                self.print_expr(f, caller)?;
                f.write_str("(")?;
                for (i, arg) in arguments.iter().enumerate() {
                    self.print_expr(f, arg)?;

                    if i != arguments.len() - 1 {
                        f.write_str(", ")?;
                    }
                }
                f.write_str(")")
            }

            Expr::MemberFunctionCall { member, function } => {
                self.print_expr(f, member)?;
                f.write_str(".")?;
                self.print_expr(f, function)
            }

            Expr::Literal(lit) => self.print_literal(f, lit),

            Expr::Comparison(left, comp, right) => {
                self.print_expr(f, left)?;
                match comp {
                    ComparisonOperand::Greater => f.write_str(" > "),
                    ComparisonOperand::Less => f.write_str(" < "),
                    ComparisonOperand::GreaterEqual => f.write_str(" >= "),
                    ComparisonOperand::LessEqual => f.write_str(" <= "),
                    ComparisonOperand::Equal => f.write_str(" == "),
                    ComparisonOperand::NotEqual => f.write_str(" != "),
                }?;
                self.print_expr(f, right)
            }

            Expr::IndexArray { array, index } => {
                self.print_expr(f, array)?;
                f.write_str("[")?;
                self.print_expr(f, index)?;
                f.write_str("]")
            }

            Expr::Array(arr) => {
                f.write_str("arr[")?;
                for (i, elm) in arr.iter().enumerate() {
                    self.print_expr(f, elm)?;

                    if i != arr.len() - 1 {
                        f.write_str(", ")?;
                    }
                }
                f.write_str("]")
            }

            Expr::Tuple(tup) => {
                f.write_str("tup[")?;
                for (i, elm) in tup.iter().enumerate() {
                    self.print_expr(f, elm)?;

                    if i != tup.len() - 1 {
                        f.write_str(", ")?;
                    }
                }
                f.write_str("]")
            }

            Expr::Assignment(left, ty, right) => {
                self.print_expr(f, left)?;
                match ty {
                    AssignmentType::Normal => f.write_str(" := "),
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

            Expr::Range(start, end) => {
                self.print_expr(f, start)?;
                f.write_str("..")?;
                self.print_expr(f, end)
            }
        }
    }

    fn print_literal(&mut self, f: &mut dyn Write, literal: &Literal) -> Result {
        match literal {
            Literal::Integer(i) => write!(f, "{}", i),
            Literal::Bool(b) => write!(f, "{}", b),
            Literal::String(s) => write!(f, "{:?}", s),
            Literal::Rune(r) => write!(f, "'{}'", r),
            Literal::Array(arr) => {
                f.write_str("[")?;
                for (i, elm) in arr.iter().enumerate() {
                    self.print_literal(f, elm)?;

                    if i != arr.len() - 1 {
                        f.write_str(", ")?;
                    }
                }
                f.write_str("]")
            }
            Literal::Float(fl) => write!(f, "{}", fl),
        }
    }

    fn print_ty(&mut self, f: &mut dyn Write, ty: &Type) -> Result {
        match ty {
            Type::Infer => f.write_str("infer"),
            Type::Not(ty) => self.print_ty(f, ty.data()),
            Type::Parenthesised(ty) => {
                f.write_str("(")?;
                self.print_ty(f, ty.data())?;
                f.write_str(")")
            }
            Type::Const(ident, ty) => {
                write!(f, "const {}: ", self.context.resolve(*ident))?;
                self.print_ty(f, ty.data())
            }
            Type::Operand(left, op, right) => {
                self.print_ty(f, left.data())?;
                write!(f, "{}", op)?;
                self.print_ty(f, right.data())
            }
            Type::Function { params, returns } => {
                f.write_str("fn(")?;
                for (i, param) in params.iter().enumerate() {
                    self.print_ty(f, param.data())?;

                    if i != params.len() - 1 {
                        f.write_str(", ")?;
                    }
                }
                f.write_str(") -> ")?;
                self.print_ty(f, returns.data())
            }
            Type::TraitObj(traits) => {
                if traits.is_empty() {
                    f.write_str("type")
                } else {
                    f.write_str("type[")?;
                    for (i, ty) in traits.iter().enumerate() {
                        self.print_ty(f, ty.data())?;
                        if i != traits.len() - 1 {
                            f.write_str(", ")?;
                        }
                    }
                    f.write_str("]")
                }
            }
            Type::Bounded { path, bounds } => {
                write!(
                    f,
                    "{}[",
                    path.iter()
                        .map(|seg| self.context.resolve(*seg))
                        .collect::<Vec<&str>>()
                        .join(".")
                )?;
                for (i, ty) in bounds.iter().enumerate() {
                    self.print_ty(f, ty.data())?;

                    if i != bounds.len() - 1 {
                        f.write_str(", ")?;
                    }
                }
                f.write_str("]")
            }
            Type::Integer { sign, width } => write!(
                f,
                "{}{}",
                match sign {
                    Signedness::Signed => "i",
                    Signedness::Unsigned => "u",
                },
                width
            ),
            Type::IntPtr(sign) => write!(
                f,
                "{}ptr",
                match sign {
                    Signedness::Signed => "i",
                    Signedness::Unsigned => "u",
                },
            ),
            Type::IntReg(sign) => write!(
                f,
                "{}reg",
                match sign {
                    Signedness::Signed => "i",
                    Signedness::Unsigned => "u",
                },
            ),
            Type::Float { width } => write!(f, "f{}", width),
            Type::Boolean => f.write_str("bool"),
            Type::String => f.write_str("str"),
            Type::Rune => f.write_str("rune"),
            Type::Unit => f.write_str("unit"),
            Type::Absurd => f.write_str("absurd"),
            Type::Array(len, ty) => {
                write!(f, "arr[{}, ", len)?;
                self.print_ty(f, ty.data())?;
                f.write_str("]")
            }
            Type::Slice(ty) => {
                f.write_str("arr[")?;
                self.print_ty(f, ty.data())?;
                f.write_str("]")
            }
            Type::Tuple(types) => {
                f.write_str("tup[")?;
                for (i, ty) in types.iter().enumerate() {
                    self.print_ty(f, ty.data())?;

                    if i != types.len() - 1 {
                        f.write_str(", ")?;
                    }
                }
                f.write_str("]")
            }
            Type::ItemPath(path) => write!(
                f,
                "{}",
                path.iter()
                    .map(|seg| self.context.resolve(*seg))
                    .collect::<Vec<&str>>()
                    .join(".")
            ),
        }
    }

    fn print_attr(&mut self, f: &mut dyn Write, attr: Attribute) -> Result {
        match attr {
            Attribute::Visibility(vis) => match vis {
                Visibility::Exposed => f.write_str("exposed "),
                Visibility::Package => f.write_str("pkg "),
                Visibility::FileLocal => f.write_str(""),
            },
            Attribute::Const => f.write_str("const "),
        }
    }

    pub(crate) fn print_stmt(&mut self, f: &mut dyn Write, stmt: &'ctx Stmt<'ctx>) -> Result {
        self.print_indent(f)?;

        match &*stmt {
            Stmt::Expr(expr) => {
                self.print_expr(f, expr)?;
                writeln!(f)
            }

            Stmt::Empty => writeln!(f, "empty"),

            Stmt::Continue => writeln!(f, "continue"),

            Stmt::Return(None) => writeln!(f, "return"),

            Stmt::Return(Some(ret)) => {
                f.write_str("return ")?;
                self.print_expr(f, ret)?;
                writeln!(f)
            }

            Stmt::Break(None) => writeln!(f, "break"),

            Stmt::Break(Some(brk)) => {
                f.write_str("break ")?;
                self.print_expr(f, brk)?;
                writeln!(f)
            }

            Stmt::Loop { body, else_clause } => {
                writeln!(f, "loop")?;

                self.indent_level += 1;
                for stmt in body {
                    self.print_stmt(f, stmt)?
                }
                self.indent_level -= 1;

                if let Some(else_clause) = else_clause {
                    self.print_indent(f)?;
                    writeln!(f, "else")?;

                    self.indent_level += 1;
                    for stmt in else_clause {
                        self.print_stmt(f, stmt)?
                    }
                    self.indent_level -= 1;
                }

                self.print_indent(f)?;
                writeln!(f, "end")
            }

            Stmt::While {
                condition,
                body,
                then,
                else_clause,
            } => {
                f.write_str("while ")?;
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

                if let Some(else_clause) = else_clause {
                    self.print_indent(f)?;
                    writeln!(f, "else")?;

                    self.indent_level += 1;
                    for stmt in else_clause {
                        self.print_stmt(f, stmt)?
                    }
                    self.indent_level -= 1;
                }

                self.print_indent(f)?;
                writeln!(f, "end")
            }

            Stmt::For {
                var,
                condition,
                body,
                then,
                else_clause,
            } => {
                f.write_str("for ")?;
                self.print_expr(f, var)?;
                f.write_str(" in ")?;
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

                if let Some(else_clause) = else_clause {
                    self.print_indent(f)?;
                    writeln!(f, "else")?;

                    self.indent_level += 1;
                    for stmt in else_clause {
                        self.print_stmt(f, stmt)?
                    }
                    self.indent_level -= 1;
                }

                self.print_indent(f)?;
                writeln!(f, "end")
            }

            Stmt::VarDeclaration {
                name,
                ty,
                val,
                constant,
                mutable,
            } => {
                write!(
                    f,
                    "{}{}{}: ",
                    if *constant { "const" } else { "let" },
                    if *mutable { " mut " } else { " " },
                    self.context.resolve(*name)
                )?;
                self.print_ty(f, ty.data())?;
                f.write_str(" := ")?;
                self.print_expr(f, val)?;
                writeln!(f)
            }

            Stmt::If {
                condition,
                body,
                clauses,
                else_clause,
            } => {
                f.write_str("if ")?;
                self.print_expr(f, condition)?;
                writeln!(f)?;

                self.indent_level += 1;
                for stmt in body {
                    self.print_stmt(f, stmt)?;
                }
                self.indent_level -= 1;

                for (cond, body) in clauses {
                    self.print_indent(f)?;
                    f.write_str("else if ")?;
                    self.print_expr(f, cond)?;
                    writeln!(f)?;

                    self.indent_level += 1;
                    for stmt in body {
                        self.print_stmt(f, stmt)?;
                    }
                    self.indent_level -= 1;
                }

                self.print_indent(f)?;
                if let Some(body) = else_clause {
                    f.write_str("else ")?;

                    self.indent_level += 1;
                    for stmt in body {
                        self.print_stmt(f, stmt)?;
                    }
                    self.indent_level -= 1;

                    Ok(())
                } else {
                    writeln!(f, "end")
                }
            }

            Stmt::Match { var, arms } => {
                f.write_str("match ")?;
                self.print_expr(f, var)?;
                writeln!(f)?;

                self.indent_level += 1;
                for (binding, whre, body) in arms {
                    self.print_indent(f)?;

                    self.print_binding(f, binding)?;
                    if let Some(whre) = whre {
                        f.write_str("where ")?;
                        self.print_expr(f, whre)?;
                        f.write_str(" ")?;
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

    fn print_binding(
        &mut self,
        f: &mut dyn Write,
        Binding {
            reference,
            mutable,
            pattern,
            ty,
        }: &'ctx Binding<'ctx>,
    ) -> Result {
        if *reference {
            write!(f, "ref ")?;
        }
        if *mutable {
            write!(f, "mut ")?;
        }

        self.print_pattern(f, pattern)?;

        if let Some(ty) = ty {
            write!(f, ": ")?;
            self.print_ty(f, ty)?;
        }

        Ok(())
    }

    fn print_pattern(&mut self, f: &mut dyn Write, pattern: &Pattern) -> Result {
        match pattern {
            Pattern::Literal(lit) => self.print_literal(f, lit),
            Pattern::Ident(ident) => write!(f, "{} ", self.context.resolve(*ident)),
            Pattern::ItemPath(path) => {
                write!(
                    f,
                    "{}",
                    (&&*path)
                        .iter()
                        .map(|s| self.context.resolve(*s))
                        .collect::<Vec<&str>>()
                        .join(".")
                )?;

                Ok(())
            }
        }
    }
}
