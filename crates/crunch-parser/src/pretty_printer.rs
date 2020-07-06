#[cfg(feature = "no-std")]
use alloc::{format, vec::Vec};
use core::fmt::{Result, Write};
use crunch_shared::{
    context::Context,
    crunch_proc::instrument,
    strings::StrT,
    trace,
    trees::{
        ast::{
            Arm, Binding, Block, Decorator, Dest, Exposure, Expr, ExprKind, For, FuncArg, If,
            IfCond, Item, ItemKind, ItemPath, Literal, Loop, Match, Pattern, Stmt, StmtKind, Type,
            TypeMember, VarDecl, Variant, Vis, While,
        },
        Sided,
    },
};

#[derive(Debug)]
pub struct PrettyPrinter {
    indent_level: usize,
    context: Context,
}

impl PrettyPrinter {
    pub fn new(context: Context) -> Self {
        trace!("Constructed PrettyPrinter");

        Self {
            indent_level: 0,
            context,
        }
    }

    #[instrument(name = "pretty printing")]
    pub fn pretty_print(&mut self, f: &mut dyn Write, items: &[Item]) -> Result {
        for item in items {
            self.print_item(f, item)?;
        }

        Ok(())
    }

    fn print_indent(&self, f: &mut dyn Write) -> Result {
        f.write_str(&"    ".repeat(self.indent_level))
    }

    pub(crate) fn print_item(&mut self, f: &mut dyn Write, item: &Item) -> Result {
        self.decorators(f, &item.decorators)?;

        self.print_indent(f)?;
        for attr in item.attrs.iter() {
            write!(f, "{}", attr)?;
        }

        self.print_indent(f)?;
        match &item.kind {
            ItemKind::Func {
                generics,
                args,
                body,
                ret,
                sig: _,
            } => {
                self.vis(f, item.vis.as_ref().unwrap())?;
                write!(
                    f,
                    "fn {}",
                    self.context.strings.resolve(item.name.unwrap()).as_ref()
                )?;
                self.print_func(f, generics, args, body, ret)
            }

            ItemKind::Type { generics, members } => {
                self.vis(f, item.vis.as_ref().unwrap())?;
                f.write_str("type ")?;
                f.write_str(self.context.strings.resolve(item.name.unwrap()).as_ref())?;
                self.print_type(f, generics, members)
            }

            ItemKind::Enum { generics, variants } => {
                self.vis(f, item.vis.as_ref().unwrap())?;
                f.write_str("enum ")?;
                f.write_str(self.context.strings.resolve(item.name.unwrap()).as_ref())?;
                self.print_enum(f, generics, variants)
            }

            ItemKind::Trait { generics, methods } => {
                self.vis(f, item.vis.as_ref().unwrap())?;
                f.write_str("trait ")?;
                f.write_str(self.context.strings.resolve(item.name.unwrap()).as_ref())?;
                self.print_trait(f, generics, methods)
            }

            ItemKind::Import {
                file,
                dest,
                exposes,
            } => self.print_import(f, file, dest, exposes),

            ItemKind::Alias { alias, actual } => {
                self.vis(f, item.vis.as_ref().unwrap())?;
                f.write_str("alias ")?;
                self.print_alias(f, alias, actual)
            }

            ItemKind::ExtendBlock {
                target,
                extender,
                items,
            } => {
                f.write_str("extend ")?;
                self.print_extend_block(f, target, extender.as_ref().map(|t| t.as_ref()), items)
            }

            _ => todo!(),
        }
    }

    fn print_extend_block(
        &mut self,
        f: &mut dyn Write,
        target: &Type,
        extender: Option<&Type>,
        items: &[Item],
    ) -> Result {
        self.print_ty(f, target)?;

        if let Some(extender) = extender {
            f.write_str(" with ")?;
            self.print_ty(f, extender)?;
        }
        f.write_char('\n')?;

        self.indent_level += 1;
        for item in items {
            self.print_item(f, item)?;
        }
        self.indent_level -= 1;

        f.write_str("end\n")
    }

    fn print_alias(&mut self, f: &mut dyn Write, alias: &Type, actual: &Type) -> Result {
        self.print_ty(f, alias)?;
        f.write_str(" = ")?;
        self.print_ty(f, actual)?;
        f.write_char('\n')
    }

    fn item_path(&mut self, f: &mut dyn Write, path: &ItemPath) -> Result {
        let mut path = path.iter();
        let last = path.next_back();

        for segment in path {
            f.write_str(self.context.strings.resolve(*segment).as_ref())?;
            f.write_char('.')?;
        }

        if let Some(segment) = last {
            f.write_str(self.context.strings.resolve(*segment).as_ref())?;
        }

        Ok(())
    }

    fn vis(&mut self, f: &mut dyn Write, vis: &Vis) -> Result {
        if *vis != Vis::FileLocal {
            match vis {
                Vis::Package => f.write_str("pkg")?,
                Vis::Exposed => f.write_str("exposed")?,
                Vis::FileLocal => unreachable!(),
            }
        }

        Ok(())
    }

    fn generics(&mut self, f: &mut dyn Write, generics: &[Type]) -> Result {
        if !generics.is_empty() {
            let mut generics = generics.iter();
            let last = generics.next_back();

            f.write_char('[')?;
            for generic in generics {
                self.print_ty(f, generic)?;
                f.write_str(", ")?;
            }

            if let Some(generic) = last {
                self.print_ty(f, generic)?;
            }

            f.write_str("] ")?;
        }

        Ok(())
    }

    fn decorators(&mut self, f: &mut dyn Write, decorators: &[Decorator]) -> Result {
        for dec in decorators {
            self.print_indent(f)?;
            self.print_decorator(f, dec)?;
            f.write_char('\n')?;
        }

        Ok(())
    }

    fn block(&mut self, f: &mut dyn Write, block: &Block) -> Result {
        self.indent_level += 1;
        for stmt in block.iter() {
            self.print_stmt(f, stmt)?
        }
        self.indent_level -= 1;

        Ok(())
    }

    fn loop_else(&mut self, f: &mut dyn Write, else_: &Option<Block>) -> Result {
        if let Some(else_) = else_ {
            self.print_indent(f)?;
            f.write_str("else\n")?;
            self.block(f, else_)?
        }

        Ok(())
    }

    fn loop_then(&mut self, f: &mut dyn Write, then: &Option<Block>) -> Result {
        if let Some(then) = then {
            self.print_indent(f)?;
            f.write_str("else\n")?;
            self.block(f, then)?
        }

        Ok(())
    }

    fn print_import(
        &mut self,
        f: &mut dyn Write,
        file: &ItemPath,
        dest: &Dest,
        exposes: &Exposure,
    ) -> Result {
        write!(
            f,
            "import{} \"",
            match dest {
                Dest::NativeLib => " lib",
                Dest::Package => " pkg",
                Dest::Relative => "",
            }
        )?;
        self.item_path(f, file)?;
        f.write_char('"')?;

        match exposes {
            Exposure::All => f.write_str(" exposing *")?,
            Exposure::None(name) => {
                write!(f, " as {}", self.context.strings.resolve(*name).as_ref())?
            }
            Exposure::Items(items) => {
                let write_item =
                    |fmt: &mut Self, f: &mut dyn Write, (name, alias): &(ItemPath, StrT)| {
                        fmt.item_path(f, name)?;
                        f.write_str(" as ")?;
                        f.write_str(fmt.context.strings.resolve(*alias).as_ref())
                    };

                f.write_str(" exposing ")?;
                let mut items = items.iter();
                let last = items.next_back();

                for item in items {
                    write_item(self, f, item)?;
                    f.write_str(", ")?;
                }

                if let Some(last) = last {
                    write_item(self, f, last)?;
                }
            }
        }

        f.write_char('\n')
    }

    fn print_trait(&mut self, f: &mut dyn Write, generics: &[Type], items: &[Item]) -> Result {
        self.generics(f, generics)?;
        f.write_char('\n')?;

        self.indent_level += 1;
        for item in items {
            self.print_item(f, item)?;
        }
        self.indent_level -= 1;

        f.write_str("end\n")
    }

    fn print_enum(&mut self, f: &mut dyn Write, generics: &[Type], variants: &[Variant]) -> Result {
        self.generics(f, generics)?;

        self.indent_level += 1;
        for variant in variants {
            match variant {
                Variant::Unit { name, decorators } => {
                    self.decorators(f, decorators)?;

                    self.print_indent(f)?;
                    f.write_str(self.context.strings.resolve(*name).as_ref())?;
                    f.write_char('\n')?;
                }

                Variant::Tuple {
                    name,
                    elms,
                    decorators,
                } => {
                    self.decorators(f, decorators)?;

                    self.print_indent(f)?;
                    f.write_str(self.context.strings.resolve(*name).as_ref())?;
                    f.write_char('(')?;

                    let mut elms = elms.iter();
                    let last = elms.next_back();

                    for elm in elms {
                        self.print_ty(f, elm)?;
                        f.write_str(", ")?;
                    }

                    if let Some(elm) = last {
                        self.print_ty(f, elm)?;
                    }

                    f.write_str(")\n")?;
                }
            }
        }
        self.indent_level -= 1;

        writeln!(f, "end")
    }

    fn print_type(
        &mut self,
        f: &mut dyn Write,
        generics: &[Type],
        members: &[TypeMember],
    ) -> Result {
        self.generics(f, generics)?;
        f.write_char('\n')?;

        self.indent_level += 1;
        for TypeMember {
            decorators,
            attrs,
            name,
            ty,
        } in members
        {
            self.decorators(f, decorators)?;

            self.print_indent(f)?;
            for attr in attrs {
                write!(f, "{}", attr)?;
            }

            f.write_str(self.context.strings.resolve(*name).as_ref())?;
            f.write_str(": ")?;

            self.print_ty(f, ty)?;
            f.write_char('\n')?;
        }
        self.indent_level -= 1;

        f.write_str("end\n")
    }

    fn print_func<'a>(
        &mut self,
        f: &mut dyn Write,
        generics: &[Type],
        args: &[FuncArg],
        body: &Block,
        ret: &Type,
    ) -> Result {
        self.generics(f, generics)?;

        if !args.is_empty() {
            f.write_str("(")?;

            let args = args
                .iter()
                .map(|arg| {
                    let mut param =
                        format!("{}: ", self.context.strings.resolve(arg.name).as_ref());
                    self.print_ty(&mut param, &arg.ty).unwrap();

                    param
                })
                .collect::<Vec<_>>()
                .join(", ");
            write!(f, "{})", args)?;
        } else {
            f.write_str("()")?;
        }

        f.write_str(" -> ")?;
        self.print_ty(f, ret)?;
        f.write_char('\n')?;

        self.block(f, body)?;

        self.print_indent(f)?;
        f.write_str("end\n")
    }

    fn print_decorator(&mut self, f: &mut dyn Write, dec: &Decorator) -> Result {
        self.print_indent(f)?;

        if !dec.args.is_empty() {
            write!(f, "@{}(", self.context.strings.resolve(*dec.name).as_ref())?;

            let mut args = dec.args.iter();
            let last = args.next_back();

            for arg in args {
                self.print_expr(f, arg)?;
                f.write_str(", ")?;
            }

            if let Some(arg) = last {
                self.print_expr(f, arg)?;
            }

            f.write_char(')')
        } else {
            write!(f, "@{}", self.context.strings.resolve(*dec.name).as_ref())
        }
    }

    pub(crate) fn print_expr(&mut self, f: &mut dyn Write, expr: &Expr) -> Result {
        match &expr.kind {
            ExprKind::Variable(name) => f.write_str(self.context.strings.resolve(**name).as_ref()),

            ExprKind::UnaryOp(op, expr) => {
                write!(f, "{}", op)?;
                self.print_expr(f, expr)
            }

            ExprKind::BinaryOp(Sided { lhs, op, rhs }) => {
                self.print_expr(f, lhs)?;
                write!(f, " {} ", op)?;
                self.print_expr(f, rhs)
            }

            ExprKind::Paren(expr) => {
                f.write_char('(')?;
                self.print_expr(f, expr)?;
                f.write_char(')')
            }

            ExprKind::FuncCall { caller, args } => {
                self.print_expr(f, caller)?;
                f.write_char('(')?;

                let mut args = args.iter();
                let last = args.next_back();

                for arg in args {
                    self.print_expr(f, arg)?;
                    f.write_str(", ")?;
                }

                if let Some(arg) = last {
                    self.print_expr(f, arg)?;
                }

                f.write_char(')')
            }

            ExprKind::MemberFuncCall { member, func } => {
                self.print_expr(f, member)?;
                f.write_char('.')?;
                self.print_expr(f, func)
            }

            ExprKind::Literal(lit) => self.print_literal(f, lit),

            ExprKind::Comparison(Sided { lhs, op, rhs }) => {
                self.print_expr(f, lhs)?;
                write!(f, " {} ", op)?;
                self.print_expr(f, rhs)
            }

            ExprKind::Index { var, index } => {
                self.print_expr(f, var)?;
                f.write_char('[')?;
                self.print_expr(f, index)?;
                f.write_char(']')
            }

            ExprKind::Array(array) => {
                f.write_str("arr[")?;

                let mut array = array.iter();
                let last = array.next_back();

                for elm in array {
                    self.print_expr(f, elm)?;
                    f.write_str(", ")?;
                }

                if let Some(elm) = last {
                    self.print_expr(f, elm)?;
                }

                f.write_char(']')
            }

            ExprKind::Tuple(tuple) => {
                f.write_str("tup[")?;

                let mut tuple = tuple.iter();
                let last = tuple.next_back();

                for elm in tuple {
                    self.print_expr(f, elm)?;
                    f.write_str(", ")?;
                }

                if let Some(elm) = last {
                    self.print_expr(f, elm)?;
                }

                f.write_char(']')
            }

            ExprKind::Assign(Sided { lhs, op, rhs }) => {
                self.print_expr(f, lhs)?;
                write!(f, " {} ", op)?;
                self.print_expr(f, rhs)
            }

            ExprKind::Range(start, end) => {
                self.print_expr(f, start)?;
                f.write_str("..")?;
                self.print_expr(f, end)
            }

            ExprKind::Continue => f.write_str("continue\n"),

            ExprKind::Return(None) => f.write_str("return\n"),
            ExprKind::Return(Some(ret)) => {
                f.write_str("return ")?;
                self.print_expr(f, ret)?;
                f.write_char('\n')
            }

            ExprKind::Break(None) => f.write_str("break\n"),
            ExprKind::Break(Some(brk)) => {
                f.write_str("break ")?;
                self.print_expr(f, brk)?;
                f.write_char('\n')
            }

            ExprKind::Loop(Loop { body, else_ }) => {
                f.write_str("loop\n")?;
                self.block(f, body)?;
                self.loop_else(f, else_)?;

                self.print_indent(f)?;
                f.write_str("end\n")
            }

            ExprKind::While(While {
                cond,
                body,
                then,
                else_,
            }) => {
                f.write_str("while ")?;
                self.print_expr(f, cond)?;
                f.write_char('\n')?;

                self.block(f, body)?;
                self.loop_then(f, then)?;
                self.loop_else(f, else_)?;

                self.print_indent(f)?;
                f.write_str("end\n")
            }

            ExprKind::For(For {
                var,
                cond,
                body,
                then,
                else_,
            }) => {
                f.write_str("for ")?;
                self.print_expr(f, var)?;
                f.write_str(" in ")?;
                self.print_expr(f, cond)?;
                f.write_char('\n')?;

                self.block(f, body)?;
                self.loop_then(f, then)?;
                self.loop_else(f, else_)?;

                self.print_indent(f)?;
                f.write_str("end\n")
            }

            ExprKind::If(If { clauses, else_ }) => {
                let mut clauses = clauses.iter();

                if let Some(IfCond { cond, body }) = clauses.next() {
                    f.write_str("if ")?;
                    self.print_expr(f, cond)?;
                    f.write_char('\n')?;
                    self.block(f, body)?;
                }

                for IfCond { cond, body } in clauses {
                    self.print_indent(f)?;
                    f.write_str("else if ")?;
                    self.print_expr(f, cond)?;

                    f.write_char('\n')?;
                    self.block(f, body)?;
                }

                self.print_indent(f)?;
                if let Some(body) = else_ {
                    f.write_str("else ")?;
                    self.block(f, body)?;

                    Ok(())
                } else {
                    f.write_str("end\n")
                }
            }

            ExprKind::Match(Match { var, arms }) => {
                f.write_str("match ")?;
                self.print_expr(f, var)?;
                f.write_char('\n')?;

                self.indent_level += 1;
                for Arm { bind, guard, body } in arms {
                    self.print_indent(f)?;

                    self.print_binding(f, bind)?;
                    if let Some(guard) = guard {
                        f.write_str("where ")?;
                        self.print_expr(f, guard)?;
                        f.write_char(' ')?;
                    }
                    f.write_str("=>")?;

                    self.block(f, body)?;

                    self.print_indent(f)?;
                    f.write_str("end\n")?;
                }
                self.indent_level -= 1;

                self.print_indent(f)?;
                f.write_str("end\n")
            }
        }
    }

    fn print_literal(&mut self, f: &mut dyn Write, literal: &Literal) -> Result {
        match literal {
            Literal::Integer(i) => write!(f, "{}", i),
            Literal::Bool(b) => write!(f, "{}", b),
            Literal::String(s) => write!(f, "{:?}", s),
            Literal::Rune(r) => write!(f, "'{}'", r),
            Literal::Array(array) => {
                f.write_str("arr[")?;
                let mut array = array.iter();
                let last = array.next_back();

                for elm in array {
                    self.print_literal(f, elm)?;
                    f.write_str(", ")?;
                }

                if let Some(elm) = last {
                    self.print_literal(f, elm)?;
                }

                f.write_char(']')
            }
            Literal::Float(fl) => write!(f, "{}", fl),
        }
    }

    fn print_ty(&mut self, f: &mut dyn Write, ty: &Type) -> Result {
        match ty {
            Type::Infer => f.write_str("infer"),
            Type::Not(ty) => self.print_ty(f, ty),
            Type::Paren(ty) => {
                f.write_char('(')?;
                self.print_ty(f, ty)?;
                f.write_char(')')
            }
            Type::Const(ident, ty) => {
                write!(
                    f,
                    "const {}: ",
                    self.context.strings.resolve(*ident).as_ref()
                )?;
                self.print_ty(f, ty)
            }
            Type::Operand(Sided { lhs, op, rhs }) => {
                self.print_ty(f, lhs)?;
                write!(f, " {} ", op)?;
                self.print_ty(f, rhs)
            }
            Type::Func { params, ret } => {
                f.write_str("fn(")?;

                let mut params = params.iter();
                let last = params.next_back();

                for param in params {
                    self.print_ty(f, param)?;
                    f.write_str(", ")?;
                }

                if let Some(param) = last {
                    self.print_ty(f, param)?;
                }

                f.write_str(") -> ")?;
                self.print_ty(f, ret)
            }
            Type::Trait(traits) => {
                if traits.is_empty() {
                    f.write_str("type")
                } else {
                    f.write_str("type[")?;

                    let mut traits = traits.iter();
                    let last = traits.next_back();

                    for trait_ in traits {
                        self.print_ty(f, trait_)?;
                        f.write_str(", ")?;
                    }

                    if let Some(trait_) = last {
                        self.print_ty(f, trait_)?;
                    }

                    f.write_char(']')
                }
            }
            Type::Bounded { path, bounds } => {
                self.item_path(f, path)?;
                f.write_char('[')?;

                let mut bounds = bounds.iter();
                let last = bounds.next_back();

                for bound in bounds {
                    self.print_ty(f, bound)?;
                    f.write_str(", ")?;
                }

                if let Some(bound) = last {
                    self.print_ty(f, bound)?;
                }

                f.write_char(']')
            }
            Type::Integer { sign, width } => write!(f, "{}{}", sign, width),
            Type::IntPtr(sign) => write!(f, "{}ptr", sign),
            Type::IntReg(sign) => write!(f, "{}reg", sign),
            Type::Float { width } => write!(f, "f{}", width),
            Type::Bool => f.write_str("bool"),
            Type::String => f.write_str("str"),
            Type::Rune => f.write_str("rune"),
            Type::Unit => f.write_str("unit"),
            Type::Absurd => f.write_str("absurd"),
            Type::Array(len, ty) => {
                write!(f, "arr[{}, ", len)?;
                self.print_ty(f, ty)?;
                f.write_char(']')
            }
            Type::Slice(ty) => {
                f.write_str("arr[")?;
                self.print_ty(f, ty)?;
                f.write_char(']')
            }
            Type::Tuple(types) => {
                f.write_str("tup[")?;
                for (i, ty) in types.iter().enumerate() {
                    self.print_ty(f, ty)?;

                    if i != types.len() - 1 {
                        f.write_str(", ")?;
                    }
                }

                f.write_char(']')
            }
            Type::ItemPath(path) => self.item_path(f, path),

            _ => todo!(),
        }
    }

    pub(crate) fn print_stmt(&mut self, f: &mut dyn Write, stmt: &Stmt) -> Result {
        match &stmt.kind {
            StmtKind::Item(item) => self.print_item(f, item),

            StmtKind::VarDecl(decl) => {
                let VarDecl {
                    name,
                    ty,
                    val,
                    constant,
                    mutable,
                } = decl.as_ref();
                self.print_indent(f)?;

                write!(
                    f,
                    "{}{}{}: ",
                    if *constant { "const" } else { "let" },
                    if *mutable { " mut " } else { " " },
                    self.context.strings.resolve(*name).as_ref()
                )?;

                self.print_ty(f, ty)?;
                f.write_str(" := ")?;

                self.print_expr(f, val)?;
                f.write_char('\n')
            }

            StmtKind::Expr(expr) => {
                self.print_indent(f)?;

                self.print_expr(f, expr)?;
                f.write_char('\n')
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
        }: &Binding,
    ) -> Result {
        if *reference {
            f.write_str("ref ")?;
        }
        if *mutable {
            f.write_str("mut ")?;
        }

        self.print_pattern(f, pattern)?;

        if let Some(ty) = ty {
            f.write_str(": ")?;
            self.print_ty(f, ty)?;
        }

        Ok(())
    }

    fn print_pattern(&mut self, f: &mut dyn Write, pattern: &Pattern) -> Result {
        match pattern {
            Pattern::Literal(lit) => self.print_literal(f, lit),
            Pattern::Ident(ident) => f.write_str(self.context.strings.resolve(*ident).as_ref()),
            Pattern::ItemPath(path) => self.item_path(f, path),
            Pattern::Wildcard => f.write_char('_'),
        }
    }
}
