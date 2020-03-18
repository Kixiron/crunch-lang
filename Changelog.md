# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- Added AST -> HIR translation
- Added HIR -> VIR translation

### Changed

- Split all components into crates
- Moved a lot of documentation and details to `/docs`
- `Compactor` now stores a `Box<&mut dyn CrunchWrite>` as stdout
- `Crunch` now requires a `Box<&mut dyn CrunchWrite>` for all its standalone operations
- The Crunch binary now uses a `BufWriter<Stdout>` for its stdout

### Deprecated

- `CodeBuilder` will be completely reworked soon

### Fixed

- Fixed failing tests that were broken by the crate-breakdown
- Fixed benchmarks broken by the crate-breakdown and the transition to `&mut dyn CrunchWrite`

### Removed

- Removed the `I128` integer size

## [0.0.0] - 2020-03-09

[Unreleased]: https://github.com/Kixiron/crunch-lang/compare/v0.0.0...HEAD
[0.0.0]: https://github.com/Kixiron/crunch-lang/compare/v0.0.0
