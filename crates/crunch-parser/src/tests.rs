use super::{ErrorHandler, FileId, Interner, Parser, SyntaxTree};

fn run(src: &str) -> Result<(SyntaxTree, ErrorHandler), ErrorHandler> {
    // let _ = simple_logger::init();

    Parser::new(src, FileId::new(0), Interner::new()).parse()
}

#[test]
fn hmpss() {
    let src = include_str!("../crashes/hmpss.fuzz");
    let _ = run(src);
}

#[test]
fn importnt_business() {
    let src = include_str!("../crashes/importnt_business.fuzz");
    let _ = run(src);
}

#[test]
fn more_yodeling() {
    let src = include_str!("../crashes/more_yodeling.fuzz");
    let _ = run(src);
}

#[test]
fn numero_spamo() {
    let src = include_str!("../crashes/numero_spamo.fuzz");
    let _ = run(src);
}

#[test]
fn rafadas() {
    let src = include_str!("../crashes/rafadas.fuzz");
    let _ = run(src);
}

#[test]
fn rafadas2_boogaloo() {
    let src = include_str!("../crashes/rafadas2_boogaloo.fuzz");
    let _ = run(src);
}

#[test]
fn unicode_yodeling() {
    let src = include_str!("../crashes/unicode_yodeling.fuzz");
    let _ = run(src);
}

#[test]
fn yodelin_imports() {
    let src = include_str!("../crashes/yodelin_imports.fuzz");
    let _ = run(src);
}

#[test]
fn unicode_slicin() {
    let src = include_str!("../crashes/unicode_slicin.fuzz");
    let _ = run(src);
}
