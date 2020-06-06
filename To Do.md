# To Do

## Syntax/Parser

- [ ] Graphing ASTs
- [ ] `loop` doesn't need `then` clauses
- [ ] Else clause for loops that executes on breakage?
- [x] Fix if parsing
- [ ] Type construction `Type is \n field := val \n field2 := val2 \n end`
  - [ ] Commas for fields
- [ ] Declaring/constructing unit types with `struct Type is unit`
- [ ] Make everything an expression
  - [ ] Nested declarations
- [x] Add patterns for match bindings
  - http://noamz.org/thesis.pdf
  - [ ] Tuple patterns
  - [ ] Slice patterns
  - [ ] Destructure patterns
  - [ ] Binding on patterns `<ident> @ <pattern>`
  - [ ] Array patterns?
- [ ] `if let <pattern> = <expr>` for oneshot matches
  - [ ] `while let <pattern> = <expr>`
- [ ] Bit-level matching http://erlang.org/doc/programming_examples/bit_syntax.html
- [ ] Named function arguments
- [ ] Commas with enum decls
- [ ] Commas with struct field decls
- [ ] Typed variadic function arguments `fn t(variadic: ..i32)` (Either `..` or `...`)
  - [ ] Sugar for slices
  - [ ] Limited to the last function arg
  - [ ] Add a "spread" operator (Reuse `..`, add `...`?) to allow passing slices to variadics
  - [ ] Named args will also allow passing slices in a variadic position
- [ ] Dependent typing
  - [ ] Types as values
- [ ] Change imports into `paths.like.this` instead of `"this.bullshit"`
- [ ] Add reference types `&` and `&mut`
- [x] Add mutability to decls `let mut Ident`
- [x] Remove usages of `.data()` since `Deref` is implemented for `Locatable`
  - [x] Implement `DerefMut` for `Locatable`
  - [x] `AsRef` & `AsMut` for `Locatable`
- [ ] Destructure anonymous enums (`&str | i32`) via matches e.g. `match val: &str | i32 string: &str => ..` or `if let`
  - [ ] Part of patterns
- [x] Macro for inserting stack frame counters
- [ ] Unique types, each instance is incompatible with any other
- [ ] Postfix macros
- [ ] Unions
- [ ] Re-add function generics
  - [ ] `fn t[T, E, F](t: T, e: E, f: F)`
- [ ] Raw pointers
  - [ ] `*mut T`/`*const T`?
- [ ] Blocks `block \n <stmt>* \n end`
- [ ] Async
  - [ ] `async fn`
  - [ ] `async do`
  - [ ] `async block`
- [ ] Const
  - [ ] `const var: T = ...`
  - [ ] `const do`
  - [ ] `const block`
- [ ] In-file modules `module <ident> \n <ast>* \n end`
- [ ] With blocks `with <expr> as <ident> \n <stmt>* \n end`
  - [ ] Sugar for a normal block
  - [ ] Useful for scoped allocators, locks, etc.
- [ ] Closures `do (<param>*) \n <stmt>* \n end`
- [ ] Use something more ergonomic for holding statements and expressions
  - [ ] Look at what rust does https://rustc-dev-guide.rust-lang.org/memory.html
  - [ ] Possibly use `lasso` with arbitrary internment?
- [ ] Reflection/Metaprogramming
- [ ] String Formatting
  - [ ] Inlined string variables e.g. `"{var}"`
  - [ ] String format specifiers e.g. `"{:?}"`
- [ ] Char escapes in char literals
- [ ] Switch to `lexical-core` for all float parsing
- [ ] Allow one-liner match arms delimited by commas
- [ ] Effects?
- [ ] Annotate *all* parse functions with grammar rules
- [ ] Split Pratt sub-functions into methods on `Parser`
- [ ] {De}serializable arena
- [ ] Stop using `.deref()`, instead use `*`
- [ ] Achievements
- [ ] Bidirectional type checking

## AST -> HIR

- [ ] Separate namespace for variables and types/enums/traits/functions
- [ ] Add all fields of structs and variants of enums to the symbol table

## Backend

- [ ] SMT Solver
  - [ ] DYI or Z3?
  - [ ] Could do double-duty for optimization and trait/type solving/inference
- [ ] LLVM wrapper
  - [ ] DIY?
- [ ] File checksums for knowing what needs to be recompiled https://apenwarr.ca/log/20181113
- [ ] Symbolic execution
- [ ] No dependencies on crt or libc
- [ ] MIR for majority of passes
- [ ] SSA IR for compilation to LLVM

### Optimizations

- [ ] Unused field removal on an instance basis

## Architecture

- [ ] Look into a query-based arch
  - [ ] Enables incremental compilation
  - [ ] Can reduce workload
  - [ ] Makes concurrency easier
    - [ ] With mutexes lock contention is incredibly high, requires more advanced data structures to be efficient (concurrent skiplist?)
- [ ] Look into actor-based compiler
  - [ ] Works well with query system
  - [ ] Distributed compilation
- [ ] Incremental compilation
- [ ] Concurrent compilation
- [ ] First-class async
  - [ ] Streams & Channels
  - [ ] Structured concurrency
  - [ ] Std-provided runtime
    - [ ] Executor/Stream/Future traits for modularity to allow seamless switching of runtimes
- [ ] Effects?
