# Crunch

![CI](https://github.com/Kixiron/crunch-lang/workflows/CI/badge.svg)
![Security Audit](https://github.com/Kixiron/crunch-lang/workflows/Security%20Audit/badge.svg)
[![Coverage Status](https://coveralls.io/repos/github/Kixiron/crunch-lang/badge.svg?branch=master)](https://coveralls.io/github/Kixiron/crunch-lang?branch=master)
![Lines of Code](https://tokei.rs/b1/github/Kixiron/crunch-lang)
![GitHub Issues](https://img.shields.io/github/issues/Kixiron/crunch-lang)

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

## Target Syntax

The entry point of every Crunch program is a `main` function that returns `unit`

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
    end

    fn greet(self) -> unit
        println("Hello {}! You are {} years old!\n", self.name, self.age)
    end
end

:: Functions can also be untyped
fn hello(name, age)
    println("Hello {}! You are {} years old!\n", name, age)
end

:: This untyped function will be desugared into a generic function like this
fn hello<T, E>(name: T, age E) -> unit
    println("Hello {}! You are {} years old!\n", name, age)
end

:: Functions that do not specify a return type default to `unit`
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

    if syv.name != 'Syven'
        println("You aren't Syven!")
    else
        syv.greet()
    end

    let syv = Syven.new("Syven", 22)
    syv.greet()
end
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
import build exposing * :: Note: `build` is an intrinsic package, and so no decorator or quotes are needed

:: Specify package information
let package = Package
    name: "package-name",                   :: The name of the package
    description: "A Crunch package!",       :: The description of the package (Optional)
    authors: ["Author <email@domain.com>"], :: The author(s) of the package (Optional)
    version: "0.1.0",                       :: The version of your package (Optional)
    homepage: "yourwebsite.com",            :: A link to the homepage of your package (Optional)
    repository: "github.com/yourpackage",   :: A link to the repository of your package (Optional)
    license: "MIT",                         :: The license your package is under (Optional)
    license_file: "./LICENSE-MIT",                  :: The license file of your package (Optional, only needed for non-standard licenses)
    readme: "README.md",                    :: The link you your readme file (Optional)
    build: "build.crunch",                  :: The build script of your package (Optional)
end

:: Specify dependencies
:: Dependencies can also be specified in their own variables, to be inserted into the `dependencies` vector
let dependency = Dependency
    name: "some-package",          :: The package's name (Required)
    version: "1.0.0",              :: The version of the package to use (Required)
    git: "github.com/package.git", :: A link to the repository of the package
end

:: The dependencies vector, where all dependencies will be inserted in order to be included
let dependencies: [Dependency] = [
    { name: "dependency-name", version: "0.1.0" }, :: Inline dependency
    dependency,                                    :: Previously declared dependency
]

build(package, dependencies) :: `build` takes in a `Package` and a `[Dependency]`
```

## String Escapes  

Unicode Escape Codes: `\u{0000}`  
Hex Escape Codes: `\x{00}`  
Bit Escape Codes: `\b{00000000}`  

## Primitive types

`str`: A dynamically growable string stored on the Heap  
`int`: A semi-dynamically sized signed integer ~~of up to 128 bits~~  
`float`: A semi-dynamically sized float ~~of up to 64 bits~~  
`unit`: Nothing. The void consumes all foolish enough to attempt usage, for it is naught, zilch, nada. A purely typesystem-sided construct that is clobbered by the compiler  
`nullable<ty>`: Makes a type able to be null. `ty` is the contained type. The contained value can be `ty` or `null`  
`bool`: A boolean value of either `true` or `false`  
`vector<ty>`: A vector of values. `ty` is the contained type  
`NoneType`: An immediate error, it means that the compiler broke somewhere  

## Grammar Specification

```ebnf
Program ::= ( FunctionDeclaration | TypeDeclaration | Import )*

FunctionDeclaration ::= Decorator* Visibility? 'fn' Ident ( '<' Ident ','? '>' )? '(' FunctionArguments* ')' ( '->' Ident )? '\n' Body End
FunctionArguments ::= ( Ident ( ':' Ident )? ) | ( FunctionArguments ',' )

TypeDeclaration ::= Visibility? 'type' Ident ( '<' Ident ',' '>' )? '\n' TypeArguments* Function* End
TypeArguments ::= ( ( Ident ':' Ident ) | TypeArguments ',' ) '\n'

Import ::= 'import' ImportDestination? String ( 'exposing' ( '*' | ( Ident ( 'as' Ident )? ',' ? )+ ) )? '\n'
ImportDestination ::= 'lib' | 'pkg'

VarDeclaration ::= 'let' Ident ( ':' Ident )? '=' Expr '\n'
Assignment ::= Ident Assigner Expr '\n'
FunctionCall ::= Ident ( '.' Ident )? '(' FunctionCallArgs* ')' '\n'
FunctionCallArgs ::= Expr | ( FunctionCallArgs ',' Expr )

If ::= 'if' Comparison '\n' Body ElseIf* Else? End
ElseIf ::= 'else if' Comparison '\n' Body
Else ::= 'else' '\n' Body
InlineIf ::= 'if' Expr 'then' Expr 'else' Expr /* Usage of an inline if statement is still tbd */

While ::= 'while' Expr '\n' Body ( 'then' '\n' Body )? End
Loop ::= 'loop' Body 'end'
For ::= 'for' Ident 'in' Expr '\n' Body ( 'then' '\n' Body )?  End

BinaryOperation ::= Expr BinaryOperand Expr
Comparison ::= Expr Comparator Expr

BinaryOperand ::= ( '+' | '-' | '*' | '/' | '^' | '|' | '&' ) ( '?' | '!' )?
Comparator ::= '==' | '<=' | '>=' | '<' | '>'
Assigner ::= '=' | ( BinaryOperand '=' )

Ident ::= [a-zA-Z]+
Literal ::= String | Integer | Boolean
String ::= '"' [^"]* '"' | "'" [^']* "'"
Integer ::= [0-9]+
Boolean ::= 'true' | 'false'
Range ::= Expr '..' Expr

Return ::= 'return' Expr '\n'
Continue ::= 'continue' '\n'
Break ::= 'break' '\n'

Decorator ::= '@' Ident ( '(' DecoratorArgs* ')' )? '\n'
DecoratorArgs ::= ( Ident | Literal ) | ( DecoratorArgs ',' ( Ident | Literal ) )
Visibility ::= 'exposed' | 'lib'
End ::= 'end'

Expr ::= Literal | Range | Comparison | BinaryOperation | Ident | ( '(' Expr ')' )
Array ::= '[' (( Expr ';' Integer ) | ArrayInner ) ']'
ArrayInner ::= Expr | ( ArrayInner ',' Expr )

Statement ::= If | While | Loop | For | FunctionCall | Assignment | VarDeclaration | Return | Continue | Break | ( Expr '\n' )

Body ::= Statement+ | 'empty'
```

## Checklist

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
  - [ ] Traits
```crunch
trait TraitName
    fn trait_function()
        println("Hello from TraitName!")
    end
end
```
  - [ ] [Poni-style](https://tutorial.ponylang.io/gotchas/divide-by-zero.html) operators  
        `+` vs `+?` vs `+!`  
        `int / int = int` Normal Divide (Division by zero results in zero)  
        `int /? int = result<int>` Checked Division (Dividing by zero will result in an error)  
        `int /! int = int` Crashing Division (Division by zero will result in a program halt)  
  - [ ] `<ret> if <cond> else <ret>`
  - [ ] `then` clauses on loops for if the loop executes un-broken
```crunch
while true
    :: ..snip
then
    :: ..snip
end
```
- pattern matching
- [ ] Runtime Reflection
- [ ] Variables
  - [ ] Tuples
  - [ ] Enums
```crunch
enum EnumName
    Variant
    TupleVariant
    OrphanType =>
        orphan_type_field: int
    end
end
```
  - [ ] Scoping
  - [X] Strings
  - [X] Integers
    - [X] Signed
    - [X] Floats
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
- [ ] `::: Doc Comments`  
- [ ] Mandatory Bracing: `a + b * c` is a syntax error, `a + (b * c)` is not  
- [ ] Zig-style multiline string literals with `#'` or `#"`:
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
<details>
<summary>Possible alternative syntax</summary>

```
let string = 
    ' Multi
    '     Line
    '         String
    '             Literals
end

:: Alternatively,
let string = 
    " Multi
    "     Line
    "         String
    "             Literals
end
```
</details>
- [ ] Decorators (`@decorator`)
