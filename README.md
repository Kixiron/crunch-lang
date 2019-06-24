<<<<<<< HEAD
# Crunch

## Checklist

- [ ] Sublexing
    - Possibly not needed, careful vetting of all regexes might mitigate problems
- [ ] Variables
    - [ ] Scoping
    - [X] Support for typing
    - [X] Naming
    - [X] Values
        - [X] Strings
        - [X] Integers
            - [X] Signed
            - [X] Floats
            - [ ] Dynamic re-sizing when needed
                - [X] When parsing
                - [ ] When operating on
        - [ ] Vectors
            - [X] Multi-typed
            - [ ] Indexing
            - [ ] Iteration
                - [ ] For loop integration
        - [X] Booleans
    - [X] Uninitialized variables
    - [ ] Global Variables
    - [ ] Semi-static typing
    - [ ] Manipulation (Adding, indexing, removing, type info, etc.)
    - [ ] Constants
        - Strictly enforced to be unchanging
    - [ ] Calling variables
- [X] Comments
    - [X] Single Line
    - [X] Multiline
        - Also applied the regex logic in order to capture everything from `/*` to `*/` as a comment.
    - [X] Fix string breakout
        - Using a string declaration char (' or ") within a comment will cause it to find the next matching char, creating a long and wrong string, messing everything up
        - Fixed by changing the comment pattern to `#[regex = "//[^\n]+\n"]`. This makes the Comment everything from `//` to a newline, mitigating any breakout
    - [X] Documentation Comments
        - Doc comments exist, but do nothing special (Yet)
- [X] Char support
    - Some strange chars will cause the program to panic
    - Was strange interaction with the old method of parsing comments
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
    - [X] For
    - [X] While
    - [X] Loops
- [ ] Logic
    - [X] If
    - [X] Else
    - [X] Else If
    - [X] And
    - [X] Or
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
- [ ] Static analysis
- [ ] Strong(er) typing
- [ ] Compilation
    - Bytecode?
        - `.compact` extension
        - `.crunched` extension
        - Still requires interpreter of some sort
    - LLVM?
        - Multi-target machine code compilation
    - Both?
- [ ] Garbage Collector
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
    - Improve off of benchmark results to increase speed
- [ ] Stabilize and assure using `no-panic`
- [ ] Add better user experience using `human-panic`
    - [ ] Custom, randomized friendly error messages
- [ ] Decide on variable mutability by default
    - `mut` for mutable like Rust
- [ ] Standardize and comment codebase

## Target Syntax

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
=======
# Crunch

## Checklist

- [ ] Sublexing
    - Possibly not needed, careful vetting of all regexes might mitigate problems
- [ ] Variables
    - [ ] Scoping
    - [X] Support for typing
    - [X] Naming
    - [X] Values
        - [X] Strings
        - [X] Integers
            - [X] Signed
            - [X] Floats
            - [ ] Dynamic re-sizing when needed
                - [X] When parsing
                - [ ] When operating on
        - [ ] Vectors
            - [X] Multi-typed
            - [ ] Indexing
            - [ ] Iteration
                - [ ] For loop integration
        - [X] Booleans
    - [X] Uninitialized variables
    - [ ] Global Variables
    - [ ] Semi-static typing
    - [ ] Manipulation (Adding, indexing, removing, type info, etc.)
    - [ ] Constants
        - Strictly enforced to be unchanging
    - [ ] Calling variables
- [X] Comments
    - [X] Single Line
    - [X] Multiline
        - Also applied the regex logic in order to capture everything from `/*` to `*/` as a comment.
    - [X] Fix string breakout
        - Using a string declaration char (' or ") within a comment will cause it to find the next matching char, creating a long and wrong string, messing everything up
        - Fixed by changing the comment pattern to `#[regex = "//[^\n]+\n"]`. This makes the Comment everything from `//` to a newline, mitigating any breakout
    - [X] Documentation Comments
        - Doc comments exist, but do nothing special (Yet)
- [X] Char support
    - Some strange chars will cause the program to panic
    - Was strange interaction with the old method of parsing comments
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
    - [X] For
    - [X] While
    - [X] Loops
- [ ] Logic
    - [X] If
    - [X] Else
    - [X] Else If
    - [X] And
    - [X] Or
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
- [ ] Static analysis
- [ ] Strong(er) typing
- [ ] Compilation
    - Bytecode?
        - `.compact` extension
        - `.crunched` extension
        - Still requires interpreter of some sort
    - LLVM?
        - Multi-target machine code compilation
    - Both?
- [ ] Garbage Collector
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
    - Improve off of benchmark results to increase speed
- [ ] Stabilize and assure using `no-panic`
- [ ] Add better user experience using `human-panic`
    - [ ] Custom, randomized friendly error messages
- [ ] Decide on variable mutability by default
    - `mut` for mutable like Rust
- [ ] Standardize and comment codebase

## Target Syntax

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
>>>>>>> Refractored, fixed and tested int parser
