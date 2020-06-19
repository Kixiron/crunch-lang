#![warn(
    missing_copy_implementations,
    missing_debug_implementations,
    clippy::dbg_macro,
    clippy::missing_safety_doc,
    clippy::wildcard_imports,
    clippy::shadow_unrelated
)]

use crunch_parser::{Parser, PrettyPrinter};
use crunch_semantics::{Correctness, SemanticAnalyzer};
use crunch_shared::{
    context::Context,
    error::ErrorHandler,
    files::{CurrentFile, FileId, Files},
};
use crunch_typecheck::Engine;
use ladder::Ladder;
use rustyline::{
    completion::{Completer, Pair},
    highlight::Highlighter,
    hint::{Hinter, HistoryHinter},
    validate::Validator,
    Context as RustyLineContext, Editor,
};
use rustyline_derive::Helper;
use std::borrow::Cow;

fn main() -> rustyline::Result<()> {
    print_greeting();
    let mut rusty_line = init_rusty_line();

    let (mut new, mut code, mut eval_ty, mut last_was_newline) =
        (true, String::new(), EvalType::None, false);
    loop {
        let prompt = prompt(new, &mut rusty_line);
        let line = rusty_line.readline(prompt);
        // Workaround because `rustyline::ReadlineError` doesn't implement Clone for some reason
        let history_line = line
            .as_ref()
            .map_or_else(|_| Err(()), |line| Ok(line.clone()));

        match line {
            Ok(line) if last_was_newline && line.trim().is_empty() => {
                let mut files = Files::new();
                files.add("<repl>", code.clone());

                let context = Context::new();
                let parser = Parser::new(
                    &code,
                    CurrentFile::new(FileId::new(0), code.len()),
                    context.clone(),
                );

                match parser.parse() {
                    Ok((items, mut warnings)) => {
                        semantic_analyzer().analyze(&items, &context, &mut warnings);

                        // The semantic analyzer will set an error if one occurs
                        if !warnings.is_fatal() {
                            let mut hir = Ladder::new().lower(&items);

                            warnings.extend(
                                Engine::new(context.strings.clone())
                                    .walk(&mut hir)
                                    .map_or_else(|warn| warn, |err| err),
                            );

                            // The type engine will set an error if one occurs
                            if !warnings.is_fatal() {
                                match eval_ty {
                                    EvalType::None | EvalType::Ast => {
                                        if items.len() == 1 {
                                            println!("{:#?}", items[0])
                                        } else {
                                            println!("{:#?}", items)
                                        }
                                    }
                                    EvalType::Hir => {
                                        if hir.len() == 1 {
                                            println!("{:#?}", hir[0])
                                        } else {
                                            println!("{:#?}", hir)
                                        }
                                    }
                                    EvalType::Symbol => todo!(),
                                    EvalType::Pretty => {
                                        let mut pretty_printed = String::new();
                                        PrettyPrinter::new(context.clone())
                                            .pretty_print(&mut pretty_printed, &items)
                                            .unwrap();

                                        println!("{}", pretty_printed);
                                    }
                                }

                                continue;
                            }
                        }

                        warnings.emit(&files);
                    }

                    Err(mut errors) => errors.emit(&files),
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
                code.push_str(&line.trim_start_matches(".ast"));
                code.push('\n');
                eval_ty = EvalType::Ast;
                new = false;
            }

            Ok(line) if line.starts_with(".hir") => {
                code.push_str(&line.trim_start_matches(".hir"));
                code.push('\n');
                eval_ty = EvalType::Hir;
                new = false;
            }

            Ok(line) if line.starts_with(".symbol") => {
                code.push_str(&line.trim_start_matches(".symbol"));
                code.push('\n');
                eval_ty = EvalType::Symbol;
                new = false;
            }

            Ok(line) if line.starts_with(".pretty") => {
                code.push_str(&line.trim_start_matches(".pretty"));
                code.push('\n');
                eval_ty = EvalType::Pretty;
                new = false;
            }

            Ok(line) if line.starts_with(".stmt") => {
                let context = Context::new();
                let mut files = Files::new();
                files.add("<repl>", line.clone());

                let mut line = line.trim_start_matches(".stmt").to_owned();
                line.push('\n');
                let mut parser = Parser::new(
                    &line,
                    CurrentFile::new(FileId::new(0), line.len()),
                    context.clone(),
                );

                match parser.stmt() {
                    Ok(expr) => {
                        parser.error_handler_mut().emit(&files);
                        println!("{:#?}", expr);
                    }

                    Err(error) => ErrorHandler::from(error).emit(&files),
                }
            }

            Ok(line) if line.starts_with(".expr") => {
                let context = Context::new();
                let mut files = Files::new();
                files.add("<repl>", line.clone());

                let line = line.trim_start_matches(".expr");
                let mut parser = Parser::new(
                    &line,
                    CurrentFile::new(FileId::new(0), line.len()),
                    context.clone(),
                );

                match parser.expr() {
                    Ok(expr) => {
                        parser.error_handler_mut().emit(&files);
                        print!("{:#?}", expr);
                    }

                    Err(error) => ErrorHandler::from(error).emit(&files),
                }
            }

            Ok(line) if line.starts_with(".help") => {
                println!(
                    "Crunch Repl v{}\n  \
                      .help      See this message\n  \
                      .ast       See the AST\n  \
                      .hir       See the HIR\n  \
                      .stmt      See a statement's ast\n  \
                      .expr      See an expression's ast\n  \
                      .symbol    See the generated symbol table\n  \
                      .pretty    Pretty-print the AST\n  \
                      .exit      Quit the repl\n  \
                      .clear     Clear the screen",
                    env!("CARGO_PKG_VERSION")
                );
            }

            Ok(line) if line.starts_with(".exit") => {
                rusty_line.add_history_entry(line.as_str());
                break;
            }

            Ok(line) if line.starts_with(".clear") => {
                println!("\x1B[2J\x1B[H");
            }

            Ok(line) => {
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

        rusty_line.add_history_entry(history_line.unwrap().as_str());

        if !(eval_ty == EvalType::None && !last_was_newline && new) {
            new = false;
        }
    }

    rusty_line.save_history("history.txt")
}

fn print_greeting() {
    println!(
        r" \
  ________  ________  ___  ___  ________   ________  ___  ___
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
}

fn init_rusty_line() -> Editor<CrunchHelper> {
    use rustyline::{Cmd, CompletionType, Config, EditMode, KeyPress, OutputStreamType};

    let config = Config::builder()
        .history_ignore_space(true)
        .completion_type(CompletionType::List)
        .edit_mode(EditMode::Emacs)
        .output_stream(OutputStreamType::Stdout)
        .build();

    let helper = CrunchHelper {
        hinter: HistoryHinter {},
        colored_prompt: "".to_owned(),
    };

    let mut rusty_line = Editor::with_config(config);
    rusty_line.set_helper(Some(helper));
    rusty_line.bind_sequence(KeyPress::Meta('N'), Cmd::HistorySearchForward);
    rusty_line.bind_sequence(KeyPress::Meta('P'), Cmd::HistorySearchBackward);
    rusty_line.bind_sequence(KeyPress::Tab, Cmd::Insert(1, "    ".to_string()));

    let _ = rusty_line.load_history("history.txt");

    rusty_line
}

fn prompt(new: bool, rusty_line: &mut Editor<CrunchHelper>) -> &'static str {
    let prompt = if new { ">> " } else { "|> " };
    rusty_line.helper_mut().expect("No helper").colored_prompt =
        format!("\x1b[1;32m{}\x1b[0m", prompt);

    prompt
}

fn semantic_analyzer() -> SemanticAnalyzer {
    SemanticAnalyzer::new().pass(Correctness::new())
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum EvalType {
    None,
    Symbol,
    Ast,
    Hir,
    Pretty,
}

#[derive(Helper)]
struct CrunchHelper {
    hinter: HistoryHinter,
    colored_prompt: String,
}

impl Completer for CrunchHelper {
    type Candidate = Pair;
}

impl Hinter for CrunchHelper {
    fn hint(&self, line: &str, pos: usize, ctx: &RustyLineContext<'_>) -> Option<String> {
        self.hinter.hint(line, pos, ctx)
    }
}

impl Highlighter for CrunchHelper {
    fn highlight_hint<'h>(&self, hint: &'h str) -> Cow<'h, str> {
        format!("\x1B[31;32m{}\x1B[0m", hint).into()
    }
}

impl Validator for CrunchHelper {}
