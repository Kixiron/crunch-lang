//! Regression tests for found crashes

use crate::{
    parser::{CurrentFile, Parser},
    symbol_table::{Graph, MaybeSym, NodeId, Scope},
    Context,
};
use crunch_shared::{ast::Item, error::ErrorHandler, files::FileId};

fn run(
    src: &str,
    ctx: Context,
) -> Result<(Vec<Item>, ErrorHandler, Graph<Scope, MaybeSym>, NodeId), ErrorHandler> {
    // let _ = simple_logger::init();
    Parser::new(src, CurrentFile::new(FileId::new(0), 0), ctx).parse()
}

#[test]
fn hmpss() {
    let ctx = Context::new();
    let src = include_str!("../crashes/hmpss.fuzz");
    let _ = run(src, ctx);
}

#[test]
fn importnt_business() {
    let ctx = Context::new();
    let src = include_str!("../crashes/importnt_business.fuzz");
    let _ = run(src, ctx);
}

#[test]
fn more_yodeling() {
    let ctx = Context::new();
    let src = include_str!("../crashes/more_yodeling.fuzz");
    let _ = run(src, ctx);
}

#[test]
fn numero_spamo() {
    let ctx = Context::new();
    let src = include_str!("../crashes/numero_spamo.fuzz");
    let _ = run(src, ctx);
}

#[test]
fn rafadas() {
    let ctx = Context::new();
    let src = include_str!("../crashes/rafadas.fuzz");
    let _ = run(src, ctx);
}

#[test]
fn rafadas2_boogaloo() {
    let ctx = Context::new();
    let src = include_str!("../crashes/rafadas2_boogaloo.fuzz");
    let _ = run(src, ctx);
}

#[test]
fn unicode_yodeling() {
    let ctx = Context::new();
    let src = include_str!("../crashes/unicode_yodeling.fuzz");
    let _ = run(src, ctx);
}

#[test]
fn yodelin_imports() {
    let ctx = Context::new();
    let src = include_str!("../crashes/yodelin_imports.fuzz");
    let _ = run(src, ctx);
}

#[test]
fn unicode_slicin() {
    let ctx = Context::new();
    let src = include_str!("../crashes/unicode_slicin.fuzz");
    let _ = run(src, ctx);
}

#[test]
fn gobbledegook() {
    let ctx = Context::new();
    let src =
        unsafe { core::str::from_utf8_unchecked(include_bytes!("../crashes/gobbledegook.fuzz")) };
    let _ = run(src, ctx);
}

#[test]
fn horrible_style() {
    let ctx = Context::new();
    let src = include_str!("../crashes/horrible_style.fuzz");
    let _ = run(src, ctx);
}

#[test]
fn main_24601() {
    let ctx = Context::new();
    let src = include_str!("../crashes/main_24601.fuzz");
    let _ = run(src, ctx);
}

#[test]
fn squares() {
    let ctx = Context::new();
    let src = include_str!("../crashes/squares.fuzz");
    let _ = run(src, ctx);
}

#[test]
fn floatnnnnnn() {
    let ctx = Context::new();
    let src = include_str!("../crashes/floatnnnnnn.fuzz");
    let _ = run(src, ctx);
}

#[test]
fn qic_fatish() {
    let ctx = Context::new();
    let src = include_str!("../crashes/qic_fatish.fuzz");
    let _ = run(src, ctx);
}

#[test]
fn matchend() {
    let ctx = Context::new();
    let src = include_str!("../crashes/matchend.fuzz");
    let _ = run(src, ctx);
}

#[test]
fn enbum() {
    let ctx = Context::new();
    let src = include_str!("../crashes/enbum.fuzz");
    let _ = run(src, ctx);
}
