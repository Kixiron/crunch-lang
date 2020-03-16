# Crunch

![CI](https://github.com/Kixiron/crunch-lang/workflows/CI/badge.svg)
![Security Audit](https://github.com/Kixiron/crunch-lang/workflows/Security%20Audit/badge.svg)
[![Coverage Status](https://coveralls.io/repos/github/Kixiron/crunch-lang/badge.svg?branch=master)](https://coveralls.io/github/Kixiron/crunch-lang?branch=master)
![Lines of Code](https://tokei.rs/b1/github/Kixiron/crunch-lang)
![GitHub Issues](https://img.shields.io/github/issues/Kixiron/crunch-lang)

Crunch is a strongly & statically typed scripting language focused on ease of use, portability and speed, built for the modern age.

## Building Crunch

First, have [`rustup`] and `cargo` installed, then run the following commands

```text
git clone https://github.com/Kixiron/crunch-lang
cd crunch-lang
git submodule update --init --recursive
cargo build
```

## Building Documentation

First, install [zola] and then run the following commands

```text
git clone https://github.com/Kixiron/crunch-lang
cd crunch-lang/docs
zola serve
```

[`rustup`]: https://rustup.rs/
[zola]: https://www.getzola.org/documentation/getting-started/installation/
