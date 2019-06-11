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

The following would be the contents of a normal `.crunch` file in the `type_checks: false` mode  
Files with `type_checks` enabled must have a `main` method

```crunch
// Strings can use the " syntax or the ' syntax
let string_variable = "String"
let string_variable = 'String'

// Integers can be signed, unsigned or floats
let integer_variable = 123
let integer_variable = -123
let integer_variable = 1.23

// You change the value of a variable directly
integer_variable = 2.34

// Vectors can store any variable type and are dynamically allocated
let vector_variable = ["one", 'two', "three"]
let vector_variable = ["one", 2, "three"]
let vector_variable = [1, 'two', 3.0]
// Vectors can rely on commas or line breaks to separate items
let vector_variable = [
    "one"
    2
    'three'
]

// Uninitialized variables or variables with no current value must be initialized with the `null` keyword
let no_value = null
no_value = "Value stored!"

// Will iterate over each number in the vector
for number in vector_variable
    // Calls the std function `println` with `number` as the parameter, printing `number`
    println(number)

// Define a method with three parameters
method test_method: param_one, param_two, param_three
    println(param_one)
    println(param_two)
    println(param_three)

/// This is a doc comment!
/// It will be compiled into *valid* `markdown`!
method typed_method: param_one? str, param_two? int, param_three? bool
    println(param_one)
    println(param_two)
    println(param_three)

// An example of scoping
method scoping_method:
    let i = 0          // <-------- 'a   i declared in scope 'a'
        println(i)     // <----- 'b  |   i does not exist, error
        let i = 0      //         |  |   i declared in scope 'b'
        println(i)     // <--------  |   print 'b i, no error
    println(i)         // <-----------   print 'a i, no error

// Call our scoping method
scoping_method()
```

Here's that same file, but adjusted for `type_checks: true`

```crunch
// Program execution starts at the main method
method main
    let string_variable = "String"
    let string_variable = 'String'
    
    // Type annotations are optional in both modes
    let integer_variable: int = 123
    let integer_variable = -123
    let integer_variable = 1.23

    // You change the value of a variable directly, but type checks are strictly enforced
    integer_variable = 2.34
    integer_variable = "some string" // Error!
    
    // Vectors can only hold one type with type checks enabled
    let vector_variable = ["one", 'two', "three"]
    let vector_variable = ["one", 2, "three"] // Error!
    let vector_variable = [
        "one"
        "two"
        'three'
    ]

    // Type annotations are required on values initialized with `null`
    // The type given to the variable will be strictly enforced
    let no_value: str = null
    no_value = "Value stored!"
    
    for number in vector_variable
        println(number)

    // Call our scoping method
    scoping_method()

// Type annotations are required on methods when type checks are enabled, so this method would cause an error!
method test_method: param_one, param_two, param_three
    println(param_one)
    println(param_two)
    println(param_three)

// This method would compile perfectly
method typed_method: param_one? str, param_two? int, param_three? bool
    println(param_one)
    println(param_two)
    println(param_three)

method scoping_method:
    let i = 0
        let i = 0
        println(i)
    println(i)
```

To specify information about your package, use the `Compacter.crunch` file in the root of the project

```crunch
// Specify package information
struct Package
    name: "package-name"                  // The name of the package
    description: "A Crunch package!"      // The description of the package (Optional)
    authors: ["Author <email@domain.com>] // The author(s) of the package (Optional)
    version: "0.1.0"                      // The version of your package (Optional)
    homepage: "yourwebsite.com"           // A link to the homepage of your package (Optional)
    repository: "github.com/yourpackage"  // A link to the repository of your package (Optional)
    license: "MIT"                        // The license your package is under (Optional)
    license_file: "./MIT"                 // The license file of your package (Optional, only needed for non-standard licenses)
    readme: "README.md"                   // The link you your readme file (Optional)
    build: "build.crunch"                 // The build script of your package (Optional)
    type_checks: true                     // Whether or not strict type checking should be applied to your package (Defaults to false)

// Specify dependencies
// Dependencies can also be specified in their own variables, to be inserted into the `dependencies` vector
let dependency = {
    name: "some-package"          // The package's name (Required)
    version: "1.0.0"              // The version of the package to use (Required)
    git: "github.com/package.git" // A link to the repository of the package
}

// The dependencies vector, where all dependencies will be inserted in order to be included
let dependencies = [
    { name: "dependency-name", version: "0.1.0" }
]
```
