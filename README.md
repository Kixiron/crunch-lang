# Crunch

## Checklist
-----

- [ ] Variables
    - [ ] Scoping
    - [ ] Support for typing
    - [ ] Naming
    - [ ] Values
        - [ ] Strings
        - [ ] Integers
            - [ ] Signed
            - [ ] Floats
            - [ ] Dynamic re-sizing when needed
                - [ ] When parsing
                - [ ] When operating on
        - [ ] Vectors
            - [ ] Indexing
            - [ ] Iteration
                - [ ] For loop integration
        - [ ] Booleans
    - [ ] Uninitialized variables
    - [ ] Global Variables
    - [ ] Semi-static typing
    - [ ] Manipulation (Adding, indexing, removing, type info, etc.)
    - [ ] Runtime reflection and metaprogramming?
    - [ ] Constants
        - Strictly enforced to be unchanging
    - [ ] Calling variables
- [ ] Comments
    - [ ] Single Line
    - [ ] Multiline
    - [ ] Documentation Comments
- [ ] Vet user input
    - [ ] Make sure files exist
    - [ ] Make sure files are valid crunch files
        - [ ] Valid extension
        - [ ] Valid encoding
- [ ] Methods
    - [ ] Scoping
        - Spaces and tabs supported
    - [ ] Naming
    - [ ] Parameters
    - [ ] Method Calls
- [ ] Loops
    - [ ] For
    - [ ] While
    - [ ] Loops
- [ ] Logic
    - [ ] If
    - [ ] Else
    - [ ] Else If
    - [ ] And
    - [ ] Or
    - [ ] Not
- [ ] Errors
    - [ ] Ergonomic Error Messages
    - [ ] Full Error Reporting
    - [ ] Better Error Handling
- [ ] Classes
    - [ ] Class Methods
    - [ ] Class Variables
    - [ ] Class instance comparison
- [ ] Custom Lexer
    - Focus on speed and correctness
- [ ] Standard Library
- [ ] Allow imports
    - [ ] Import system
    - [ ] Import functions/variables/classes from other files
    - [ ] Public/Private access
- [ ] Mutability with `mut` keyword
- [ ] Static analysis
- [ ] Static(er) typing
- [X] Bytecode
- [ ] Optimization for bytecode compilation
- [ ] JIT
- [ ] Tooling
    - [ ] All commandline-friendly tools
    - [ ] Package Manager
    - [ ] Build Manager
    - [ ] Formatter
    - [ ] Documenter
    - Names
        - Cruncher
        - Compacter
- [ ] Comprehensive Testing
- [ ] Comprehensive Benchmarking
- [ ] Stabilize and assure using `no-panic`
- [ ] Add better user experience using `human-panic`
- [ ] Custom, randomized friendly error messages
- [ ] Standardize and comment codebase
- [ ] FFI

# Crunch

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

String Escapes:  

Unicode Escape Codes: `\u{0000}`  
Byte Escape Codes: `\x{00}`  
Bit Escape Codes: `\b{00000000}`

## Primitive types

`str`: A dynamically growable string stored on the Heap
`int`: A semi-dynamically sized signed integer of up to 128 bits
`float`: A semi-dynamically sized float of up to 64 bits
`void`: Nothing. The void consumes all foolish enough to attempt usage, for it is naught, zilch, nada. A purely typesystem-sided construct that is clobbered by the compiler
`nullable<ty>`: Makes a type able to be null. `ty` is the contained type. The contained value can be `ty` or `null`
`bool`: A boolean value of either `true` or `false`
`vector<ty>`: A vector of values. `ty` is the contained type

## Language Builtins

`@print` Prints to stdout
`@collect` Forces a GC collection cycle
`@halt` Halts program execution

## CLI Options

`--burn-gc` Preforms a GC collect at every opportunity
`--debug-log` Activates verbose logging
`--fault-tolerant` Allows minor errors to occur without triggering runtime errors
