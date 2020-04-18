use super::{ErrorHandler, FileId, Interner, Parser, SymbolTable, SyntaxTree};

fn run(src: &str) -> Result<(SyntaxTree, Interner, ErrorHandler), ErrorHandler> {
    // let _ = simple_logger::init();
    Parser::new(src, FileId::new(0), Interner::new(), SymbolTable::new()).parse()
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

#[test]
fn gobbledegook() {
    let src =
        unsafe { core::str::from_utf8_unchecked(include_bytes!("../crashes/gobbledegook.fuzz")) };
    let _ = run(src);
}

#[test]
fn horrible_style() {
    let src = include_str!("../crashes/horrible_style.fuzz");
    let _ = run(src);
}

#[test]
fn main_24601() {
    let src = include_str!("../crashes/main_24601.fuzz");
    let _ = run(src);
}

#[test]
fn squares() {
    let src = include_str!("../crashes/squares.fuzz");
    let _ = run(src);
}

#[test]
fn floatnnnnnn() {
    let src = include_str!("../crashes/floatnnnnnn.fuzz");
    let _ = run(src);
}

#[test]
fn qic_fatish() {
    let src = include_str!("../crashes/qic_fatish.fuzz");
    let _ = run(src);
}
