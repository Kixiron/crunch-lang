//! Regression tests for found crashes

use crate::parser::{ParseConfig, Parser};
use crunch_shared::{
    context::{Arenas, Context, OwnedArenas},
    error::ErrorHandler,
    files::{CurrentFile, FileId},
    trees::ast::Item,
};

fn run<'ctx>(
    src: &str,
    ctx: &'ctx Context<'ctx>,
) -> Result<(Vec<&'ctx Item<'ctx>>, ErrorHandler), ErrorHandler> {
    Parser::new(
        src,
        ParseConfig::default(),
        CurrentFile::new(FileId::new(0), 0),
        ctx,
    )
    .parse()
}

#[test]
fn hmpss() {
    let owned_arenas = OwnedArenas::default();
    let arenas = Arenas::from(&owned_arenas);

    let ctx = Context::new(arenas);
    let src = include_str!("../crashes/hmpss.fuzz");
    let _ = run(src, &ctx);
}

#[test]
fn importnt_business() {
    let owned_arenas = OwnedArenas::default();
    let arenas = Arenas::from(&owned_arenas);

    let ctx = Context::new(arenas);
    let src = include_str!("../crashes/importnt_business.fuzz");
    let _ = run(src, &ctx);
}

#[test]
fn more_yodeling() {
    let owned_arenas = OwnedArenas::default();
    let arenas = Arenas::from(&owned_arenas);

    let ctx = Context::new(arenas);
    let src = include_str!("../crashes/more_yodeling.fuzz");
    let _ = run(src, &ctx);
}

#[test]
fn numero_spamo() {
    let owned_arenas = OwnedArenas::default();
    let arenas = Arenas::from(&owned_arenas);

    let ctx = Context::new(arenas);
    let src = include_str!("../crashes/numero_spamo.fuzz");
    let _ = run(src, &ctx);
}

#[test]
fn rafadas() {
    let owned_arenas = OwnedArenas::default();
    let arenas = Arenas::from(&owned_arenas);

    let ctx = Context::new(arenas);
    let src = include_str!("../crashes/rafadas.fuzz");
    let _ = run(src, &ctx);
}

#[test]
fn rafadas2_boogaloo() {
    let owned_arenas = OwnedArenas::default();
    let arenas = Arenas::from(&owned_arenas);

    let ctx = Context::new(arenas);
    let src = include_str!("../crashes/rafadas2_boogaloo.fuzz");
    let _ = run(src, &ctx);
}

#[test]
fn unicode_yodeling() {
    let owned_arenas = OwnedArenas::default();
    let arenas = Arenas::from(&owned_arenas);

    let ctx = Context::new(arenas);
    let src = include_str!("../crashes/unicode_yodeling.fuzz");
    let _ = run(src, &ctx);
}

#[test]
fn yodelin_imports() {
    let owned_arenas = OwnedArenas::default();
    let arenas = Arenas::from(&owned_arenas);

    let ctx = Context::new(arenas);
    let src = include_str!("../crashes/yodelin_imports.fuzz");
    let _ = run(src, &ctx);
}

#[test]
fn unicode_slicin() {
    let owned_arenas = OwnedArenas::default();
    let arenas = Arenas::from(&owned_arenas);

    let ctx = Context::new(arenas);
    let src = include_str!("../crashes/unicode_slicin.fuzz");
    let _ = run(src, &ctx);
}

#[test]
fn gobbledegook() {
    let owned_arenas = OwnedArenas::default();
    let arenas = Arenas::from(&owned_arenas);

    let ctx = Context::new(arenas);
    let src =
        unsafe { core::str::from_utf8_unchecked(include_bytes!("../crashes/gobbledegook.fuzz")) };
    let _ = run(src, &ctx);
}

#[test]
fn horrible_style() {
    let owned_arenas = OwnedArenas::default();
    let arenas = Arenas::from(&owned_arenas);

    let ctx = Context::new(arenas);
    let src = include_str!("../crashes/horrible_style.fuzz");
    let _ = run(src, &ctx);
}

#[test]
fn main_24601() {
    let owned_arenas = OwnedArenas::default();
    let arenas = Arenas::from(&owned_arenas);

    let ctx = Context::new(arenas);
    let src = include_str!("../crashes/main_24601.fuzz");
    let _ = run(src, &ctx);
}

#[test]
fn squares() {
    let owned_arenas = OwnedArenas::default();
    let arenas = Arenas::from(&owned_arenas);

    let ctx = Context::new(arenas);
    let src = include_str!("../crashes/squares.fuzz");
    let _ = run(src, &ctx);
}

#[test]
fn floatnnnnnn() {
    let owned_arenas = OwnedArenas::default();
    let arenas = Arenas::from(&owned_arenas);

    let ctx = Context::new(arenas);
    let src = include_str!("../crashes/floatnnnnnn.fuzz");
    let _ = run(src, &ctx);
}

#[test]
fn qic_fatish() {
    let owned_arenas = OwnedArenas::default();
    let arenas = Arenas::from(&owned_arenas);

    let ctx = Context::new(arenas);
    let src = include_str!("../crashes/qic_fatish.fuzz");
    let _ = run(src, &ctx);
}

#[test]
fn matchend() {
    let owned_arenas = OwnedArenas::default();
    let arenas = Arenas::from(&owned_arenas);

    let ctx = Context::new(arenas);
    let src = include_str!("../crashes/matchend.fuzz");
    let _ = run(src, &ctx);
}

#[test]
fn enbum() {
    let owned_arenas = OwnedArenas::default();
    let arenas = Arenas::from(&owned_arenas);

    let ctx = Context::new(arenas);
    let src = include_str!("../crashes/enbum.fuzz");
    let _ = run(src, &ctx);
}
