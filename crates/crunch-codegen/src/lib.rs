use crunch_parser::{parser::Literal, Context};
use ladder::{Expr, Function, Hir, Stmt};
use std::{fmt::Write as WriteFmt, io::Write};

#[test]
fn test() {
    use crunch_parser::{parser::ItemPath, CurrentFile, FileId, Parser};
    use ladder::Ladder;
    use std::io::BufWriter;

    let source = r#"
    fn main()
        let mut greeting := "Hello from Crunch!"
        println(greeting)
    end
    "#;

    let ctx = Context::new();
    let mut files = crunch_parser::Files::new();
    files.add("<test>", source);

    match Parser::new(source, CurrentFile::new(FileId::new(0), source.len()), &ctx).parse() {
        Ok((tree, mut warnings, module_table, module_scope)) => {
            warnings.emit(&files);

            println!("{:#?}", &module_scope);

            let ladder = Ladder::new(
                module_table,
                module_scope,
                ItemPath::new(ctx.intern("package")),
            );

            let hir = ladder.lower(&*tree);
            println!("{:#?}", &hir);

            {
                let mut file = BufWriter::new(std::fs::File::create("crunch.ll").unwrap());
                let mut gen = Generator::new(&mut file, &ctx);
                gen.add_puts();
                gen.from_hir(&hir[0]);
                gen.finish();
            }

            dbg!(std::process::Command::new("llc")
                .args(&["crunch.ll", "-o", "crunch.s"])
                .output()
                .unwrap());
            dbg!(std::process::Command::new("clang")
                .args(&["crunch.s", "-o", "crunch.exe"])
                .output()
                .unwrap());
        }

        Err(mut err) => {
            err.emit(&files);
        }
    }
}

struct Generator<'ctx, 'clt, W: Write> {
    out: W,
    ctx: &'clt Context<'ctx>,
    constants: String,
}

impl<'ctx, 'clt, W: Write> Generator<'ctx, 'clt, W> {
    pub fn new(out: W, ctx: &'clt Context<'ctx>) -> Self {
        Self {
            out,
            ctx,
            constants: String::new(),
        }
    }

    pub fn finish(mut self) {
        self.out.write_all(self.constants.as_bytes()).unwrap();
    }

    pub fn add_puts(&mut self) {
        self.out
            .write_all(b"declare i32 @puts(i8* nocapture) nounwind\n")
            .unwrap();
    }

    pub fn from_hir(&mut self, node: &Hir) {
        match node {
            Hir::Function(func) => self.function(func),
        }
    }

    fn function(&mut self, function: &Function) {
        self.out.write_all(b"define i32 @main() {\n").unwrap();

        for stmt in function.body.iter() {
            self.statement(stmt);
        }

        self.out.write_all(b"ret i32 0\n}\n").unwrap();
    }

    fn statement(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::VarDecl(decl) => {
                self.constants
                    .write_fmt(format_args!(
                        "@.{} = ",
                        decl.name
                            .iter()
                            .map(|s| self.ctx.resolve(*s))
                            .collect::<Vec<&str>>()
                            .join(".")
                    ))
                    .unwrap();
                if let Expr::Literal(Literal::String(ref text)) = decl.value {
                    self.constants
                        .write_fmt(format_args!(
                            "private unnamed_addr constant [19 x i8] c\"{}\\00\"",
                            text.to_string()
                        ))
                        .unwrap();
                } else {
                    todo!()
                }

                self.out.write_all(b"\n").unwrap();
            }
            Stmt::Expr(Expr::FnCall(func)) => {
                if func
                    .func
                    .iter()
                    .map(|s| self.ctx.resolve(*s))
                    .collect::<Vec<&str>>()
                    .join(".")
                    == "println"
                {
                    self.out
                        .write_fmt(format_args!(
                            "%cast210 = getelementptr [19 x i8], [19 x i8]* @.{}, i64 0, i64 0\n",
                            if let Expr::Var(var) = func.params.first().unwrap() {
                                var.iter()
                                    .map(|s| self.ctx.resolve(*s))
                                    .collect::<Vec<&str>>()
                                    .join(".")
                            } else {
                                todo!()
                            }
                        ))
                        .unwrap();

                    self.out
                        .write_all(b"call i32 @puts(i8* %cast210)\n")
                        .unwrap();
                } else {
                    todo!()
                }
            }
            Stmt::Return(ret) => {
                self.out.write_all(b"ret i32 0\n").unwrap();
            }
            _ => todo!(),
        }
    }
}
