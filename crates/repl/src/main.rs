#![warn(
    missing_copy_implementations,
    missing_debug_implementations,
    clippy::dbg_macro,
    clippy::missing_safety_doc,
    clippy::wildcard_imports,
    clippy::shadow_unrelated
)]

use std::borrow::Cow;

fn main() -> rustyline::Result<()> {
    let config = rustyline::Config::builder()
        .history_ignore_space(true)
        .completion_type(rustyline::CompletionType::List)
        .edit_mode(rustyline::EditMode::Emacs)
        .output_stream(rustyline::OutputStreamType::Stdout)
        .build();

    let h = MyHelper {
        hinter: rustyline::hint::HistoryHinter {},
        colored_prompt: "".to_owned(),
    };

    let mut rl = rustyline::Editor::with_config(config);
    rl.set_helper(Some(h));
    rl.bind_sequence(
        rustyline::KeyPress::Meta('N'),
        rustyline::Cmd::HistorySearchForward,
    );
    rl.bind_sequence(
        rustyline::KeyPress::Meta('P'),
        rustyline::Cmd::HistorySearchBackward,
    );
    rl.bind_sequence(
        rustyline::KeyPress::Tab,
        rustyline::Cmd::Insert(1, "    ".to_string()),
    );

    let _ = rl.load_history("history.txt");

    println!(
        r"  ________  ________  ___  ___  ________   ________  ___  ___     
 |\   ____\|\   __  \|\  \|\  \|\   ___  \|\   ____\|\  \|\  \    
 \ \  \___|\ \  \|\  \ \  \\\  \ \  \\ \  \ \  \___|\ \  \\\  \     {} v{}
  \ \  \    \ \   _  _\ \  \\\  \ \  \\ \  \ \  \    \ \   __  \    {}
   \ \  \____\ \  \\  \\ \  \\\  \ \  \\ \  \ \  \____\ \  \ \  \   {}
    \ \_______\ \__\\ _\\ \_______\ \__\\ \__\ \_______\ \__\ \__\  .help for help
     \|_______|\|__|\|__|\|_______|\|__| \|__|\|_______|\|__|\|__|",
        env!("CARGO_PKG_NAME"),
        env!("CARGO_PKG_VERSION"),
        env!("CARGO_PKG_AUTHORS"),
        env!("CARGO_PKG_REPOSITORY"),
    );
    println!();

    let (mut new, mut code, mut eval_ty, mut last_was_newline) =
        (true, String::new(), EvalType::None, false);
    loop {
        let prompt = if new { ">> " } else { "|> " };

        rl.helper_mut().expect("No helper").colored_prompt = format!("\x1b[1;32m{}\x1b[0m", prompt);

        let readline = rl.readline(prompt);
        match readline {
            Ok(line) if last_was_newline && line.trim().is_empty() => {
                let mut files = crunch_parser::Files::new();
                files.add("<repl>", code.clone());

                let parser = crunch_parser::Parser::new(
                    &code,
                    crunch_parser::CurrentFile::new(crunch_parser::FileId::new(0), code.len()),
                    &crunch_parser::Context::new(),
                );

                match eval_ty {
                    EvalType::None | EvalType::Ast => match parser.parse() {
                        Ok((mut warns, graph, _module)) => {
                            crunch_semantics::SemanticAnalyzer::default()
                                .analyze_all(&*tree, &graph, &interner, &mut warns);

                            warns.emit(&files);

                            if !warns.is_fatal() {
                                println!("{:#?}", graph);
                            }
                        }

                        Err(mut err) => err.emit(&files),
                    },

                    EvalType::Symbol => match parser.parse() {
                        Ok((mut warns, graph, _module)) => {
                            crunch_semantics::SemanticAnalyzer::default()
                                .analyze_all(&*tree, &graph, &ctx, &mut warns);
                            warns.emit(&files);

                            if !warns.is_fatal() {
                                println!("{:#?}", graph);
                            }
                        }

                        Err(mut err) => err.emit(&files),
                    },

                    EvalType::Pretty => match parser.parse() {
                        Ok((mut warns, graph, _module)) => {
                            crunch_semantics::SemanticAnalyzer::default()
                                .analyze_all(&*tree, &graph, &ctx, &mut warns);
                            warns.emit(&files);

                            /*
                            let mut pretty = String::new();
                            crunch_parser::PrettyPrinter::new(interner)
                                .pretty_print(&mut pretty, &ast)
                                .unwrap();

                            if !warns.is_fatal() {
                                println!("{}", pretty);
                            }
                            */
                            todo!()
                        }

                        Err(mut err) => err.emit(&files),
                    },
                }

                eval_ty = EvalType::None;
                last_was_newline = false;
                new = true;
                code.clear();
            }

            Ok(line) if line.trim().is_empty() => {
                code.push_str(&line);
                last_was_newline = true;
                new = false;
            }

            Ok(line) if line.starts_with(".ast") => {
                rl.add_history_entry(line.as_str());
                code.push_str(&line.trim_start_matches(".ast"));
                code.push('\n');
                eval_ty = EvalType::Ast;
                new = false;
            }

            Ok(line) if line.starts_with(".symbol") => {
                rl.add_history_entry(line.as_str());
                code.push_str(&line.trim_start_matches(".symbol"));
                code.push('\n');
                eval_ty = EvalType::Symbol;
                new = false;
            }

            Ok(line) if line.starts_with(".pretty") => {
                rl.add_history_entry(line.as_str());
                code.push_str(&line.trim_start_matches(".pretty"));
                code.push('\n');
                eval_ty = EvalType::Pretty;
                new = false;
            }

            Ok(line) if line.starts_with(".stmt") => {
                rl.add_history_entry(line.as_str());
                let mut files = crunch_parser::Files::new();
                files.add("<repl>", line.clone());

                let mut line = line.trim_start_matches(".stmt").to_owned();
                line.push('\n');
                let mut parser = crunch_parser::Parser::new(
                    &line,
                    crunch_parser::CurrentFile::new(crunch_parser::FileId::new(0), line.len()),
                    &crunch_parser::Context::new(),
                );

                match parser.stmt() {
                    Ok(expr) => {
                        parser.error_handler_mut().emit(&files);
                        println!("{:#?}", expr);
                    }
                    Err(err) => crunch_parser::ErrorHandler::from(err).emit(&files),
                }
            }

            Ok(line) if line.starts_with(".expr") => {
                rl.add_history_entry(line.as_str());
                let mut files = crunch_parser::Files::new();
                files.add("<repl>", line.clone());

                let line = line.trim_start_matches(".expr");
                let mut parser = crunch_parser::Parser::new(
                    &line,
                    crunch_parser::CurrentFile::new(crunch_parser::FileId::new(0), line.len()),
                    &crunch_parser::Context::new(),
                );

                match parser.expr() {
                    Ok(expr) => {
                        parser.error_handler_mut().emit(&files);
                        print!("{:#?}", expr);
                    }
                    Err(err) => crunch_parser::ErrorHandler::from(err).emit(&files),
                }
            }

            Ok(line) if line.starts_with(".help") => {
                rl.add_history_entry(line.as_str());
                println!(
                    "Crunch Repl v{}\n  \
                      .help   See this message\n  \
                      .exit   Quit the repl\n  \
                      .clear  Clear the screen\n  \
                      .ast    See the AST\n  \
                      .expr   See an expression's ast\n  \
                      .symbol See the generated symbol table\n  \
                      .pretty Pretty-print the AST",
                    env!("CARGO_PKG_VERSION")
                );
            }

            Ok(line) if line.starts_with(".exit") => {
                rl.add_history_entry(line.as_str());
                break;
            }

            Ok(line) if line.starts_with(".clear") => {
                rl.add_history_entry(line.as_str());
                println!("\x1B[2J\x1B[H");
            }

            Ok(line) => {
                rl.add_history_entry(line.as_str());
                code.push_str(&line);
                code.push('\n');
                last_was_newline = false;
                new = false;
            }

            Err(rustyline::error::ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break;
            }

            Err(rustyline::error::ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }

            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }

        if !(eval_ty == EvalType::None && !last_was_newline && new) {
            new = false;
        }
    }

    rl.save_history("history.txt")
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum EvalType {
    None,
    Symbol,
    Ast,
    Pretty,
}

#[derive(rustyline_derive::Helper)]
struct MyHelper {
    hinter: rustyline::hint::HistoryHinter,
    colored_prompt: String,
}

impl rustyline::completion::Completer for MyHelper {
    type Candidate = rustyline::completion::Pair;
}

impl rustyline::hint::Hinter for MyHelper {
    fn hint(&self, line: &str, pos: usize, ctx: &rustyline::Context<'_>) -> Option<String> {
        self.hinter.hint(line, pos, ctx)
    }
}

impl rustyline::highlight::Highlighter for MyHelper {
    fn highlight_hint<'h>(&self, hint: &'h str) -> Cow<'h, str> {
        format!("\x1B[31;32m{}\x1B[0m", hint).into()
    }
}

impl rustyline::validate::Validator for MyHelper {}
