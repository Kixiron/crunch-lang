#![feature(track_caller, proc_macro_hygiene, vec_remove_item)]
#![deny(missing_debug_implementations, deprecated, unused_must_use)]
#![warn(
    clippy::cargo,
    clippy::pedantic,
    clippy::perf,
    clippy::complexity,
    clippy::style,
    clippy::correctness,
    rust_2018_idioms
)]
#![allow(
    clippy::too_many_lines,
    clippy::cast_lossless,
    clippy::shadow_unrelated,
    clippy::cast_possible_truncation,
    clippy::cast_possible_wrap,
    clippy::cast_sign_loss,
    clippy::needless_pass_by_value,
    clippy::unused_self,
    clippy::debug_assert_with_mut_call,
    clippy::cast_precision_loss,
    clippy::module_name_repetitions
)]

//! # Crunch
//!
//! ## Usage
//!
//! Executing a source code file
//!
//! ```rust
//! use crunch::{OptionBuilder, Crunch};
//!
//! Crunch::run_source_file(OptionBuilder::new("./source_file.crunch").build());
//! ```
//!
//! Reading bytecode from a file and executing
//!
//! ```rust
//! use crunch::{OptionBuilder, Crunch};
//!
//! Crunch::run_byte_file(OptionBuilder::new("./bytecode_file.crunched").build());
//! ```
//!
//! This, however, is an incredibly high-level usage of the language, akin to the behind-the-scenes of the CLI
//!
//! ## Target Syntax
//!
//! The entry point of every Crunch program is a `main` function that returns `void`
//!
//! ```crunch
//! :: Declare a type
//! type Syven
//!     :: `name` will be of the `str` type
//!     str name
//!     :: `age` will be of the `int` type
//!     int age
//!
//!     fn new(str name, int age) -> Self
//!         :: Construct a type
//!         Syven {
//!             name: name,
//!             age: age,
//!         }
//!
//!     fn greet(self) -> void
//!         println("Hello {}! You are {} years old!", self.name, self.age)
//!
//! fn hello(str name, int age) -> void
//!     println("Hello {}! You are {} years old!", name, age)
//!
//! :: Functions that do not specify a return type default to `void`
//! fn main()
//!     let name: str = "Syven"
//!     let age: int = null
//!     age = 22
//!
//!     hello(name, age)
//!
//!     let syv = Syven {
//!         name: "Syven",
//!         age: 22,
//!     }
//!     syv.greet()
//!
//!     let syv = Syven.new("Syven", 22)
//!     syv.greet()
//! ```
//!
//! To specify information about your package, use the `Compacter.crunch` file in the root of the project
//!
//! ```crunch
//! :: Specify package information
//! let package: Package = {
//!     name: "package-name",                   :: The name of the package
//!     description: "A Crunch package!",       :: The description of the package (Optional)
//!     authors: ["Author <email@domain.com>"], :: The author(s) of the package (Optional)
//!     version: "0.1.0",                       :: The version of your package (Optional)
//!     homepage: "yourwebsite.com",            :: A link to the homepage of your package (Optional)
//!     repository: "github.com/yourpackage",   :: A link to the repository of your package (Optional)
//!     license: "MIT",                         :: The license your package is under (Optional)
//!     license_file: "./MIT",                  :: The license file of your package (Optional, only needed for non-standard licenses)
//!     readme: "README.md",                    :: The link you your readme file (Optional)
//!     build: "build.crunch",                  :: The build script of your package (Optional)
//! }
//!
//! :: Specify dependencies
//! :: Dependencies can also be specified in their own variables, to be inserted into the `dependencies` vector
//! let dependency: Dependency = {
//!     name: "some-package",          :: The package's name (Required)
//!     version: "1.0.0",              :: The version of the package to use (Required)
//!     git: "github.com/package.git", :: A link to the repository of the package
//! }
//!
//! :: The dependencies vector, where all dependencies will be inserted in order to be included
//! let dependencies: [Dependency] = [
//!     { name: "dependency-name", version: "0.1.0" }, :: Inline dependency
//!     dependency,                                    :: Previously declared dependency
//! ]
//! ```
//!
//! Primitive types
//! -----
//!
//! `str`: A dynamically growable string stored on the Heap  
//! `int`: A semi-dynamically sized signed integer of up to 128 bits  
//! `float`: A semi-dynamically sized float of up to 64 bits  
//! `void`: Nothing  
//! `nullable<ty>`: Makes a type able to be null. `ty` is the contained type. The contained value can be `ty` or `null`  
//! `result<ty: Ok, ty: Err>: TODO  
//! `bool`: A boolean value of either `true` or `false`  
//!
//! ## Language Builtins
//! -----
//!
//! `@print` Prints to stdout  
//! `@collect` Forces a GC collection cycle  
//! `@assert` Assertions  
//! `@try` Unwrap an error or null type  
//! `@await` Async code  
//! `@spawn` Spawns a thread  
//!

/// The number of available registers for the VM
const NUMBER_REGISTERS: usize = 32;

#[macro_use]
extern crate log;

mod assembler;
/// Encoding and decoding bytecode
mod bytecode;
mod code_builder;
/// The main Crunch interface
mod crunch;
/// The Garbage Collector
mod gc;
/// Instruction definitions and executions
mod instruction;
/// The Interpreter
mod interpreter;
mod jit;
mod native_lib;
/// Helper types
mod newtypes;
/// Front-end language parsing
mod parser;
/// Syscalls
mod syscall;
/// Values contained within the VM
mod value;
/// The main VM
mod vm;

pub use crate::crunch::Crunch;
pub use bytecode::*;
pub use gc::*;
pub use instruction::*;
pub use newtypes::*;
pub use parser::*;
pub use value::*;
pub use vm::*;

use std::path::PathBuf;

#[derive(Debug, structopt::StructOpt, Clone)]
#[structopt(rename_all = "kebab")]
pub struct Options {
    /// The target file
    #[structopt(parse(from_os_str))]
    pub file: PathBuf,
    /// Activates a GC Collection cycle at every opportunity
    #[structopt(long = "--burn-gc")]
    pub burn_gc: bool,
    /// Activates detailed debug logging
    #[structopt(long = "--debug-log")]
    pub debug_log: bool,
    /// Allows some runtime errors to be ignored
    #[structopt(long = "--fault-tolerant")]
    pub fault_tolerant: bool,
    /// Overwrites the heap on a side swap
    #[structopt(long = "--overwrite-heap")]
    pub overwrite_heap: bool,
    #[structopt(long = "--heap-size", default_value = "1024")]
    pub heap_size: usize,
}

// TODO: Document the option builder

#[derive(Debug, Clone)]
pub struct OptionBuilder {
    file: PathBuf,
    burn_gc: bool,
    debug_log: bool,
    fault_tolerant: bool,
    overwrite_heap: bool,
    heap_size: usize,
}

impl OptionBuilder {
    pub fn new(file: impl Into<PathBuf>) -> Self {
        Self {
            file: file.into(),
            burn_gc: false,
            debug_log: false,
            fault_tolerant: false,
            overwrite_heap: false,
            heap_size: 1024,
        }
    }

    #[must_use]
    pub const fn burn_gc(mut self, b: bool) -> Self {
        self.burn_gc = b;
        self
    }

    #[must_use]
    pub const fn debug_log(mut self, b: bool) -> Self {
        self.debug_log = b;
        self
    }

    #[must_use]
    pub const fn fault_tolerant(mut self, b: bool) -> Self {
        self.fault_tolerant = b;
        self
    }

    #[must_use]
    pub const fn overwrite_heap(mut self, b: bool) -> Self {
        self.overwrite_heap = b;
        self
    }

    #[must_use]
    pub const fn heap_size(mut self, heap_size: usize) -> Self {
        self.heap_size = heap_size;
        self
    }

    // Cannot make destructors const fns
    #[allow(clippy::missing_const_for_fn)]
    #[must_use]
    pub fn build(self) -> Options {
        Options {
            file: self.file,
            burn_gc: self.burn_gc,
            debug_log: self.debug_log,
            fault_tolerant: self.fault_tolerant,
            overwrite_heap: self.overwrite_heap,
            heap_size: self.heap_size,
        }
    }
}

// TODO: Add disassembly and maybe a GC view? Debugging capabilities?
#[derive(structopt::StructOpt, Debug, Copy, Clone, PartialEq, Eq)]
pub enum ReplOutput {
    Ast,
    Bytecode,
    None,
}

impl Default for ReplOutput {
    fn default() -> Self {
        Self::None
    }
}

impl From<&str> for ReplOutput {
    fn from(string: &str) -> Self {
        match &*string.to_lowercase() {
            "ast" => Self::Ast,
            "bytecode" => Self::Bytecode,
            "none" | _ => Self::None,
        }
    }
}
