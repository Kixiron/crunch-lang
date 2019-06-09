# Crunch

## Checklist (In order of importance- kinda)

- [ ] Sublexing
    - Possibly not needed, careful vetting of all regexes might mitigate problems
- [ ] Variables
    - [ ] Scoping
    - [X] Naming
    - [X] Values
        - [X] Strings
        - [X] Integers
            - [X] Signed
            - [X] Floats
            - [ ] Dynamic re-sizing when needed
        - [ ] Vectors
            - [X] Multi-typed
            - [ ] Indexing
            - [ ] Iteration
                - [ ] For loop integration
        - [ ] Booleans
    - [ ] Semi-static typing
- [ ] Comments
    - [X] Single Line
    - [X] Multiline
        - Also applied the regex logic in order to capture everything from `/*` to `*/` as a comment.
    - [X] Fix string breakout
        - Using a string declaration char (' or ") within a comment will cause it to find the next matching char, creating a long and wrong string, messing everything up
        - Fixed by changing the comment pattern to `#[regex = "//[^\n]+\n"]`. This makes the Comment everything from `//` to a newline, mitigating any breakout
    - [ ] Documentation Comments
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
- [ ] Static analysis
- [ ] Strong(er) typing
- [ ] Bytecode compilation
- [ ] Garbage Collector
- [ ] Tooling
    - [ ] All commandline-friendly tools
    - [ ] Package Manager (Cruncher?)
    - [ ] Build Manager
    - [ ] Formatter
    - [ ] Documenter
- [ ] Comprehensive Testing
- [ ] Comprehensive Benchmarking
    - Improve off of benchmark results to increase speed
- [ ] Stabilize and assure using `no_panic`

## Target Syntax

```crunch
// Strings can use the " syntax or the ' syntax
let string_variable = "String"
let string_variable = 'String'

// Integers can be signed, unsigned or floats
let integer_variable = 123
let integer_variable = -123
let integer_variable = 1.23

// Vectors can store any variable type and are dynamically allocated
let vector_variable = ["one", 'two', "three"]
let vector_variable = ["one", 2, "three"]
let vector_variable = [1, 'two', 3.0]

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
    let i = 0       // <-------- 'a   i declared in scope 'a'
        println(i)  // <----- 'b  |   i does not exist, error
        let i = 0   //         |  |   i declared in scope 'b'
        println(i)  // <--------  |   print 'b i, no error
    println(i)      // <-----------   print 'a i, no error
```
