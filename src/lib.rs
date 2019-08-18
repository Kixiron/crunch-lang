#![deny(
    missing_debug_implementations,
    deprecated,
    unused_must_use,
    missing_docs
)]
#![warn(
    clippy::cargo,
    clippy::nursery,
    clippy::pedantic,
    clippy::perf,
    clippy::complexity,
    clippy::style,
    clippy::correctness,
    rust_2018_idioms
)]

//! # Crunch
//!
//! ## Usage
//!
//! Reading bytecode from a file and executing
//!
//! ```rust
//! use std::{fs::File, io::Read, convert::TryFrom};
//! use crunch::Crunch;
//!
//! let mut file = File::open("examples/hello_world.crunch").unwrap();
//! let mut bytes: Vec<u8> = Vec::new();
//! file.read_to_end(&mut bytes);
//!
//! let mut crunch = Crunch::try_from(&bytes).unwrap(); // Will panic if the bytecode has an invalid format
//! crunch.execute(); // Executes the program
//! ```
//!
//! ## Target Syntax
//!
//! The entry point of every Crunch program is a `main` function that returns `void`
//!
//! ```crunch
//! type Syven {
//!     str name,
//!     int age,
//!
//!     fn new(str name, int age) -> Self {
//!         Syven {
//!             name: name,
//!             age: age,
//!         }
//!     }
//!
//!     fn greet(self) -> void {
//!         println("Hello {}! You are {} years old!", self.name, self.age);
//!     }
//! }
//!
//! fn hello(str name, int age) -> void {
//!     println("Hello {}! You are {} years old!", name, age);
//! }
//!
//! // Functions that do not specify a return type default to void
//! fn main() {
//!     let name: str = "Syven";
//!     let age: int = null;
//!     age = 22;
//!
//!     hello(name, age);
//!
//!     let syv = Syven {
//!         name: "Syven",
//!         age: 22,
//!     };
//!     syv.greet();
//!
//!     let syv = Syven.new("Syven", 22);
//!     syv.greet();
//! }
//! ```
//!
//! To specify information about your package, use the `Compacter.crunch` file in the root of the project
//!
//! ```crunch
//! // Specify package information
//! let package: Package = {
//!     name: "package-name",                   // The name of the package
//!     description: "A Crunch package!",       // The description of the package (Optional)
//!     authors: ["Author <email@domain.com>"], // The author(s) of the package (Optional)
//!     version: "0.1.0",                       // The version of your package (Optional)
//!     homepage: "yourwebsite.com",            // A link to the homepage of your package (Optional)
//!     repository: "github.com/yourpackage",   // A link to the repository of your package (Optional)
//!     license: "MIT",                         // The license your package is under (Optional)
//!     license_file: "./MIT",                  // The license file of your package (Optional, only needed for non-standard licenses)
//!     readme: "README.md",                    // The link you your readme file (Optional)
//!     build: "build.crunch",                  // The build script of your package (Optional)
//! };
//!
//! // Specify dependencies
//! // Dependencies can also be specified in their own variables, to be inserted into the `dependencies` vector
//! let dependency: Dependency = {
//!     name: "some-package",          // The package's name (Required)
//!     version: "1.0.0",              // The version of the package to use (Required)
//!     git: "github.com/package.git", // A link to the repository of the package
//! };
//!
//! // The dependencies vector, where all dependencies will be inserted in order to be included
//! let dependencies: [Dependency] = [
//!     { name: "dependency-name", version: "0.1.0" }, // Inline dependency
//!     dependency,                                    // Previously declared dependency
//! ];
//! ```

/// The number of available registers for the VM
const NUMBER_REGISTERS: usize = 10;
/// The number of available handoff registers for the VM
const NUMBER_HANDOFF_REGISTERS: usize = 10;
/// The number of stored strings for the VM
const NUMBER_STRINGS: usize = 10;

/// Encoding and decoding bytecode
#[cfg(feature = "bytecode")]
mod bytecode;
/// The main Crunch interface
mod crunch;
/// Instruction definitions and executions
mod instruction;
/// Helper types
mod newtypes;
/// Front-end language parsing
#[cfg(feature = "parser")]
mod parser;
/// The main VM
mod registers;
/// Values contained within the VM
mod value;

pub use crate::crunch::Crunch;

cfg_if::cfg_if! {
    if #[cfg(any(test, bench))] {
        #[cfg(feature = "bytecode")]
        pub use bytecode::*;
        pub use instruction::*;
        pub use newtypes::*;
        #[cfg(feature = "parser")]
        pub use parser::*;
        pub use registers::*;
        pub use value::*;
    } else {
        #[cfg(feature = "bytecode")]
        use bytecode::*;
        use instruction::*;
        use newtypes::*;
        #[cfg(feature = "parser")]
        use parser::*;
        use registers::*;
        use value::*;
    }
}
