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
    - [ ] Constants
        - Strictly enforced to be unchanging
    - [ ] Calling variables
- [ ] Comments
    - [ ] Single Line
    - [ ] Multiline
    - [ ] Documentation Comments
- [ ] Char support
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
- [ ] Bytecode
    - `.compact` extension?
    - `.crunched` extension?
    - Still requires interpreter of some sort
- [ ] Optimization for bytecode compilation
- [ ] JIT
- [ ] GC
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

## Target Syntax
-----

The entry point of every Crunch program is a `main` function that returns `void`

```crunch
type Syven {
    str name,
    int age,

    fn new(str name, int age) -> Self {
        Syven {
            name: name,
            age: age,
        }
    }
    
    fn greet(self) -> void {
        println("Hello {}! You are {} years old!", self.name, self.age);
    }
}

fn hello(str name, int age) -> void {
    println("Hello {}! You are {} years old!", name, age);
}

// Functions that do not specify a return type default to void
fn main() {
    let name: str = "Syven";
    let age: int = null;
    age = 22;

    hello(name, age);

    let syv = Syven {
        name: "Syven",
        age: 22,
    };
    syv.greet();

    let syv = Syven.new("Syven", 22);
    syv.greet();
}
```

To specify information about your package, use the `Compacter.crunch` file in the root of the project

```crunch
// Specify package information
let package: Package = {
    name: "package-name",                   // The name of the package
    description: "A Crunch package!",       // The description of the package (Optional)
    authors: ["Author <email@domain.com>"], // The author(s) of the package (Optional)
    version: "0.1.0",                       // The version of your package (Optional)
    homepage: "yourwebsite.com",            // A link to the homepage of your package (Optional)
    repository: "github.com/yourpackage",   // A link to the repository of your package (Optional)
    license: "MIT",                         // The license your package is under (Optional)
    license_file: "./MIT",                  // The license file of your package (Optional, only needed for non-standard licenses)
    readme: "README.md",                    // The link you your readme file (Optional)
    build: "build.crunch",                  // The build script of your package (Optional)
};

// Specify dependencies
// Dependencies can also be specified in their own variables, to be inserted into the `dependencies` vector
let dependency: Dependency = {
    name: "some-package",          // The package's name (Required)
    version: "1.0.0",              // The version of the package to use (Required)
    git: "github.com/package.git", // A link to the repository of the package
};

// The dependencies vector, where all dependencies will be inserted in order to be included
let dependencies: [Dependency] = [
    { name: "dependency-name", version: "0.1.0" }, // Inline dependency
    dependency,                                    // Previously declared dependency
];
```
