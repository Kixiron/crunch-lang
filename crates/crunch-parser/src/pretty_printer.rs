use crate::{
    parser::{
        Alias, AssignmentType, Ast, Attribute, BinaryOperand, BuiltinType, ComparisonOperand,
        Decorator, Enum, EnumVariant, Expr, Expression, ExtendBlock, Function, Import, ImportDest,
        ImportExposure, Literal, Signedness, Statement, Stmt, Trait, Type, TypeDecl, TypeMember,
        UnaryOperand, Visibility,
    },
    Interner,
};

#[cfg(feature = "no-std")]
use alloc::{format, vec::Vec};
use core::fmt::{Result, Write};

#[derive(Debug, Clone)]
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
        }: &ExtendBlock,
    ) -> Result {
        self.print_indent(f)?;
        write!(f, "extend ")?;
        self.print_ty(f, target)?;

        if let Some(extender) = extender {
            write!(f, " with ")?;
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
        }: &Alias,
    ) -> Result {
        self.print_indent(f)?;
        for dec in decorators {
            self.print_decorator(f, dec.data())?;
            writeln!(f)?;
        }

        for attr in attrs {
            self.print_attr(f, *attr.data())?;
        }

        write!(f, "alias ")?;
        self.print_ty(f, alias)?;
        write!(f, " = ")?;
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
            self.interner.resolve(file.data())
        )?;

        match exposes {
            ImportExposure::All => write!(f, " exposing *")?,
            ImportExposure::None(name) => write!(f, " as {}", self.interner.resolve(name.data()))?,
            ImportExposure::Members(members) => {
                write!(f, " exposing ")?;

                for (i, member) in members.iter().enumerate() {
                    write!(f, "{}", self.interner.resolve(&member.data().0))?;

                    if let Some(alias) = member.data().1 {
                        write!(f, " as {}", self.interner.resolve(&alias))?;
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
        Trait {
            decorators,
            attrs,
            name,
            generics,
            methods,
        }: &Trait,
    ) -> Result {
        for dec in decorators {
            self.print_decorator(f, dec.data())?;
            writeln!(f)?;
        }

        self.print_indent(f)?;
        for attr in attrs {
            self.print_attr(f, *attr.data())?;
        }

        write!(f, "trait {}", self.interner.resolve(name))?;

        if !generics.is_empty() {
            write!(f, "[")?;
            for (i, gen) in generics.iter().enumerate() {
                self.print_ty(f, gen.data())?;

                if i != generics.len() - 1 {
                    write!(f, ", ")?;
                }
            }
            write!(f, "]")?;
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
        }: &Enum,
    ) -> Result {
        for dec in decorators {
            self.print_decorator(f, dec.data())?;
            writeln!(f)?;
        }

        self.print_indent(f)?;
        for attr in attrs {
            self.print_attr(f, *attr.data())?;
        }

        write!(f, "enum {}", self.interner.resolve(name))?;

        if !generics.is_empty() {
            write!(f, "[")?;
            for (i, gen) in generics.iter().enumerate() {
                self.print_ty(f, gen.data())?;
                if i != generics.len() - 1 {
                    write!(f, ", ")?;
                }
            }
            write!(f, "]")?;
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
                    writeln!(f, "{}", self.interner.resolve(name))?;
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
                    write!(f, "{}(", self.interner.resolve(name))?;
                    for (i, ty) in elements.iter().enumerate() {
                        self.print_ty(f, ty.data())?;

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
        TypeDecl {
            decorators,
            attrs,
            name,
            generics,
            members,
        }: &TypeDecl,
    ) -> Result {
        for dec in decorators {
            self.print_decorator(f, dec.data())?;
            writeln!(f)?;
        }

        self.print_indent(f)?;
        for attr in attrs {
            self.print_attr(f, *attr.data())?;
        }

        write!(f, "type {}", self.interner.resolve(name))?;

        if !generics.is_empty() {
            write!(f, "[")?;
            for (i, gen) in generics.iter().enumerate() {
                self.print_ty(f, gen.data())?;

                if i != generics.len() - 1 {
                    write!(f, ", ")?;
                }
            }
            write!(f, "]")?;
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

            write!(f, "{}: ", self.interner.resolve(name))?;
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
        }: &Function,
    ) -> Result {
        for dec in decorators {
            self.print_decorator(f, dec.data())?;
            writeln!(f)?;
        }

        self.print_indent(f)?;
        for attr in attrs {
            self.print_attr(f, *attr.data())?;
        }

        write!(f, "fn {}", self.interner.resolve(name))?;

        if !args.is_empty() {
            write!(f, "(")?;

            let args = args
                .iter()
                .map(|arg| {
                    let mut param = format!(
                        "{}{}: ",
                        if arg.data().comptime { "comptime " } else { "" },
                        self.interner.resolve(arg.data().name.data())
                    );
                    self.print_ty(&mut param, arg.data().ty.data()).unwrap();

                    param
                })
                .collect::<Vec<_>>()
                .join(", ");
            write!(f, "{})", args)?;
        } else {
            write!(f, "()")?;
        }

        write!(f, " -> ")?;
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

    fn print_decorator(&mut self, f: &mut dyn Write, dec: &Decorator<'expr>) -> Result {
        self.print_indent(f)?;

        if !dec.args.is_empty() {
            write!(f, "@{}(", self.interner.resolve(&dec.name.data()))?;
            for (i, arg) in dec.args.iter().enumerate() {
                self.print_expr(f, arg)?;

                if i != dec.args.len() - 1 {
                    write!(f, ", ")?;
                }
            }
            write!(f, ")")
        } else {
            write!(f, "@{}", self.interner.resolve(&dec.name.data()))
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

            Expression::Literal(lit) => self.print_literal(f, lit),

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
                write!(f, "arr[")?;
                for (i, elm) in arr.iter().enumerate() {
                    self.print_expr(f, elm)?;

                    if i != arr.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "]")
            }

            Expression::Tuple(tup) => {
                write!(f, "tup[")?;
                for (i, elm) in tup.iter().enumerate() {
                    self.print_expr(f, elm)?;

                    if i != tup.len() - 1 {
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

    fn print_literal(&mut self, f: &mut dyn Write, literal: &Literal) -> Result {
        match literal {
            Literal::Integer(i) => write!(f, "{}", i),
            Literal::Bool(b) => write!(f, "{}", b),
            Literal::String(s) => write!(f, "{:?}", s),
            Literal::Rune(r) => write!(f, "'{}'", r),
            Literal::Array(arr) => {
                write!(f, "[")?;
                for (i, elm) in arr.iter().enumerate() {
                    self.print_literal(f, elm)?;

                    if i != arr.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "]")
            }
            Literal::Float(fl) => write!(f, "{}", fl),
        }
    }

    fn print_ty(&mut self, f: &mut dyn Write, ty: &Type) -> Result {
        match ty {
            Type::Infer => write!(f, "infer"),
            Type::Not(ty) => self.print_ty(f, ty.data()),
            Type::Parenthesised(ty) => {
                write!(f, "(")?;
                self.print_ty(f, ty.data())?;
                write!(f, ")")
            }
            Type::Const(ident, ty) => {
                write!(f, "const {}: ", self.interner.resolve(ident))?;
                self.print_ty(f, ty.data())
            }
            Type::Operand(left, op, right) => {
                self.print_ty(f, left.data())?;
                write!(f, "{}", op)?;
                self.print_ty(f, right.data())
            }
            Type::Function { params, returns } => {
                write!(f, "fn(")?;
                for (i, param) in params.iter().enumerate() {
                    self.print_ty(f, param.data())?;

                    if i != params.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ") -> ")?;
                self.print_ty(f, returns.data())
            }
            Type::TraitObj(traits) => {
                if traits.is_empty() {
                    write!(f, "type")
                } else {
                    write!(f, "type[")?;
                    for (i, ty) in traits.iter().enumerate() {
                        self.print_ty(f, ty.data())?;
                        if i != traits.len() - 1 {
                            write!(f, ", ")?;
                        }
                    }
                    write!(f, "]")
                }
            }
            Type::Bounded { path, bounds } => {
                write!(
                    f,
                    "{}[",
                    path.iter()
                        .map(|seg| self.interner.resolve(seg))
                        .collect::<Vec<&str>>()
                        .join(".")
                )?;
                for (i, ty) in bounds.iter().enumerate() {
                    self.print_ty(f, ty.data())?;

                    if i != bounds.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "]")
            }
            Type::Builtin(b) => match b {
                BuiltinType::Integer { sign, width } => write!(
                    f,
                    "{}{}",
                    match sign {
                        Signedness::Signed => "i",
                        Signedness::Unsigned => "u",
                    },
                    width
                ),
                BuiltinType::IntPtr(sign) => write!(
                    f,
                    "{}ptr",
                    match sign {
                        Signedness::Signed => "i",
                        Signedness::Unsigned => "u",
                    },
                ),
                BuiltinType::IntReg(sign) => write!(
                    f,
                    "{}reg",
                    match sign {
                        Signedness::Signed => "i",
                        Signedness::Unsigned => "u",
                    },
                ),
                BuiltinType::Float { width } => write!(f, "f{}", width),
                BuiltinType::Boolean => write!(f, "bool"),
                BuiltinType::String => write!(f, "str"),
                BuiltinType::Rune => write!(f, "rune"),
                BuiltinType::Unit => write!(f, "unit"),
                BuiltinType::Absurd => write!(f, "absurd"),
                BuiltinType::Array(len, ty) => {
                    write!(f, "arr[{}, ", len)?;
                    self.print_ty(f, ty.data())?;
                    write!(f, "]")
                }
                BuiltinType::Slice(ty) => {
                    write!(f, "arr[")?;
                    self.print_ty(f, ty.data())?;
                    write!(f, "]")
                }
                BuiltinType::Tuple(types) => {
                    write!(f, "tup[")?;
                    for (i, ty) in types.iter().enumerate() {
                        self.print_ty(f, ty.data())?;

                        if i != types.len() - 1 {
                            write!(f, ", ")?;
                        }
                    }
                    write!(f, "]")
                }
            },
            Type::ItemPath(path) => write!(
                f,
                "{}",
                path.iter()
                    .map(|seg| self.interner.resolve(seg))
                    .collect::<Vec<&str>>()
                    .join(".")
            ),
        }
    }

    fn print_attr(&mut self, f: &mut dyn Write, attr: Attribute) -> Result {
        match attr {
            Attribute::Visibility(vis) => match vis {
                Visibility::Exposed => write!(f, "exposed "),
                Visibility::Package => write!(f, "pkg "),
                Visibility::FileLocal => write!(f, ""),
            },
            Attribute::Const => write!(f, "const "),
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

            Statement::VarDeclaration {
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
                    self.interner.resolve(name)
                )?;
                self.print_ty(f, ty.data())?;
                write!(f, " := ")?;
                self.print_expr(f, val)?;
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
