# Crunch

[![LOC](https://tokei.rs/b1/github/Kixiron/crunch-lang)](https://github.com/Kixiron/crunch-lang)
![Codecov](https://img.shields.io/codecov/c/github/Kixiron/crunch-lang)

## Overarching TODOs

- [ ] Finish Parser
- [ ] Finish Interpreter
- [ ] Functions
- [ ] FFI
- [ ] WASM Backend
- [ ] Tooling
- [ ] Testing
- [ ] CLI flow (Codecov, Clippy, Rustfmt, Windows & Linux Testing)
- [ ] [Reduce Dependencies](#dependencies-overview)
- [ ] [String Formatting](https://docs.rs/runtime-fmt/0.4.1/runtime_fmt/)

## Checklist
-----

- [ ] FFI with Rust toolkit for developing native plugins
- [ ] JIT
- [ ] Type System
    - [ ] Custom Types
    - [ ] Type Safe
- [ ] Tooling
    - [ ] Package Manager
    - [ ] Formatter
    - [ ] Documenter
        - [ ] Doc Comments
    - [ ] Linter
    - [ ] Tester
        - [ ] Unit Testing integration
    - [ ] Benchmarking
- [ ] Better Error Messages
    - [ ] Friendly messages with compiler 'personality'
    - [ ] Specific errors
    - [ ] Possible solutions
    - [ ] Error codes with lookups that explain the error code
- [ ] Standard Library
    - [ ] Importing Files
        - [ ] Source Code Files
        - [ ] Native Files
        - [ ] Packages
- [ ] Semantics/Syntax
    - [ ] Explicit Mutability
    - [ ] Types
        - [ ] Type Variables
        - [ ] Type Methods
    - [ ] Logic
    - [ ] Operands
        - [ ] Overloadable?
- [ ] Runtime Reflection
- [ ] Variables
    - [ ] Scoping
    - [X] Strings
    - [ ] Integers
        - [ ] Signed
        - [ ] Floats
    - [ ] Vectors
        - [ ] Indexing
        - [ ] Iteration
            - [ ] For loop integration
    - [X] Booleans
    - [ ] Uninitialized variables
    - [ ] Global Variables
        - `global` keyword
        - Declared outside any scope
    - [ ] Manipulation (Adding, indexing, removing, type info, etc.)
    - [ ] Constants
        - Strictly enforced to be unchanging
    - [ ] Calling variables

# Crunch
-----

## Target Syntax

The entry point of every Crunch program is a `main` function that returns `void`

```crunch
:: Declare a type
type Syven
    :: `name` will be of the `str` type
    name: str
    :: `age` will be of the `int` type
    age: int

    fn new(name: str, age: int) -> Self
        :: Construct a type
        Syven {
            name: name,
            age: age,
        }

    fn greet(self) -> void
        @print "Hello {}! You are {} years old!\n", self.name, self.age

:: Functions can also be untyped
fn hello(name, age)
    @print "Hello {}! You are {} years old!\n", name, age
    
:: This untyped function will be desugared into a generic function like this
fn hello<T, E>(name: T, age E) -> void
    @print "Hello {}! You are {} years old!\n", name, age

:: Functions that do not specify a return type default to `void`
fn main()
    let name: str = "Syven"
    let age: nullable<int> = null
    age = 22

    hello(name, age)

    let syv = Syven {
        name: "Syven",
        age: 22,
    }
    syv.greet()

    let syv = Syven.new("Syven", 22)
    syv.greet()
```

For imports, follow this structure

```crunch
:: Importing a file (Note: Uses a relative path delimited by `.`)
import 'directory.file'
:: This will expose `file`, allowing usage like `file.foo()`

:: Aliasing imports
import 'directory.file' as Bar
:: Access is now aliased into `Bar`, allowing usage like `Bar.foo()`

:: Importing from a file
import 'directory.file' exposing Fizz
:: Now the only thing exposed from `file` is `Fizz`

:: Importing multiple things from a file
import 'directory.file' exposing Fizz, Buzz, Bar
:: This also allows aliasing
import 'directory.file' exposing Fizz as MyFizz, Buzz as MyBuzz, Bar as MyBar

:: Importing everything from a file
import 'directory.file' exposing *
:: Now everything from `file` is exposed

:: Importing a package/dependency
:: The syntax is the same as for importing files, except the keyword `pkg` is used after `import`
import pkg 'rand'

:: Importing a dynamic library
:: The syntax is the same as for importing files, except the keyword `lib` is used after `import`
import lib 'my_library'
```

To specify information about your package, use the `Compacter.crunch` file in the root of the project

```crunch
import build exposing *

:: Specify package information
let package: Package = {
    name: "package-name",                   :: The name of the package
    description: "A Crunch package!",       :: The description of the package (Optional)
    authors: ["Author <email@domain.com>"], :: The author(s) of the package (Optional)
    version: "0.1.0",                       :: The version of your package (Optional)
    homepage: "yourwebsite.com",            :: A link to the homepage of your package (Optional)
    repository: "github.com/yourpackage",   :: A link to the repository of your package (Optional)
    license: "MIT",                         :: The license your package is under (Optional)
    license_file: "./MIT",                  :: The license file of your package (Optional, only needed for non-standard licenses)
    readme: "README.md",                    :: The link you your readme file (Optional)
    build: "build.crunch",                  :: The build script of your package (Optional)
}

:: Specify dependencies
:: Dependencies can also be specified in their own variables, to be inserted into the `dependencies` vector
let dependency: Dependency = {
    name: "some-package",          :: The package's name (Required)
    version: "1.0.0",              :: The version of the package to use (Required)
    git: "github.com/package.git", :: A link to the repository of the package
}

:: The dependencies vector, where all dependencies will be inserted in order to be included
let dependencies: [Dependency] = [
    { name: "dependency-name", version: "0.1.0" }, :: Inline dependency
    dependency,                                    :: Previously declared dependency
]
```

## String Escapes:  

Unicode Escape Codes: `\u{0000}`  
Hex Escape Codes: `\x{00}`  
Bit Escape Codes: `\b{00000000}`  

## Primitive types

`str`: A dynamically growable string stored on the Heap  
`int`: A semi-dynamically sized signed integer ~~of up to 128 bits~~  
`float`: A semi-dynamically sized float ~~of up to 64 bits~~  
`void`: Nothing. The void consumes all foolish enough to attempt usage, for it is naught, zilch, nada. A purely typesystem-sided construct that is clobbered by the compiler  
`nullable<ty>`: Makes a type able to be null. `ty` is the contained type. The contained value can be `ty` or `null`  
`bool`: A boolean value of either `true` or `false`  
`vector<ty>`: A vector of values. `ty` is the contained type  
`NoneType`: An immediate error, it means that the compiler broke somewhere  

## Language Builtins

`@print` Prints to stdout  
`@collect` Forces a GC collection cycle  
`@halt` Halts program execution  

## Syntax TODOs

- `::: Doc Comments`  
- Mandatory Bracing: `a + b * c` is a syntax error, `a + (b * c)` is not  
- Zig-style multiline string literals with `#'` or `#"`:
    ```
    let string = 
        #' Multi
        #'     Line
        #'         String
        #'             Literals

    :: Alternatively,
    let string = 
        #" Multi
        #"     Line
        #"         String
        #"             Literals
    ```
- [Poni-style](https://tutorial.ponylang.io/gotchas/divide-by-zero.html) operators  
    `+` vs `+?` vs `+!`  
    `int / int = int` Normal Divide (Division by zero results in zero)  
    `int /? int = result<int>` Checked Division (Dividing by zero will result in an error)  
    `int /! int = int` Crashing Division (Division by zero will result in a program halt)  
- `<ret> if <cond> else <ret>`

## CLI Options

`--burn-gc` Preforms a GC collect at every opportunity  
`--debug-log` Activates verbose logging  
`--fault-tolerant` Allows minor errors to occur without triggering program shutdown  
`--output [ast|bytecode]` Outputs the produced ast and bytecode  

## TODOs Waiting on Rust

- [ ] Refractor `next()` and `peek()` in parser to use `#[track_caller]` when [Rust Issue #47809](https://github.com/rust-lang/rust/issues/47809) is merged
- [ ] Remove lazy_static dependency once [Rust Issue #51910](https://github.com/rust-lang/rust/issues/51910) is merged

## Dependencies Overview
- log: Logging
- simple_logger: Logger usage
- color-backtrace: Readable backtraces
- logos: Lexer generator
- structopt: CLI interaction
- codespan: Parser error construction
- codespan_reporting: Parser error reporting
- string-interner: String Interning
- rand: Random number generation
- winapi: Windows system interaction
- libc: Unix system interaction
- array_init: Initializing arrays with non-copy types, one usage
- lazy_static: Only used for the syscall table until [Rust Issue #51910](https://github.com/rust-lang/rust/issues/51910) is merged
- human-panic: Need to write own panic handler, because panics are an ICE
- libloading: Load dynamic libraries
- num-bigint: Arbitrary-precision integers
- crunch_proc_macro: Proc macros for internal use

## Dev Dependencies Overview
- criterion: Benchmarking
- proptest: Property Testing
