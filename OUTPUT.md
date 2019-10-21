# Source Code  

```
fn main(test: int, test2: str) -> void :: Main Function
    let i: str = '10'
    let i: int = 11

```

# AST  

```
(
    [
        Func {
            name: Ident {
                name: "main",
                info: LocInfo { file: 0, span: 3..7 },
            },
            params: [
                FuncParam {
                    name: Ident {
                        name: "test",
                        info: LocInfo { file: 0, span: 8..12 },
                    },
                    ty: Int,
                    info: LocInfo { file: 0, span: 8..14 },
                },
                FuncParam {
                    name: Ident {
                        name: "test2",
                        info: LocInfo { file: 0, span: 19..24 },
                    },
                    ty: String,
                    info: LocInfo { file: 0, span: 19..26 },
                },
            ],
            returns: Void,
            body: [
                FuncBody {
                    expr: Binding {
                        name: Ident {
                            name: "i",
                            info: LocInfo { file: 0, span: 64..65 },
                        },
                        val: Literal {
                            val: String("10"),
                            info: LocInfo { file: 0, span: 73..77 },
                        },
                        ty: String,
                        info: LocInfo { file: 0, span: 56..78 },
                    },
                    info: LocInfo { file: 0, span: 56..78 },
                },
                FuncBody {
                    expr: Binding {
                        name: Ident {
                            name: "i",
                            info: LocInfo { file: 0, span: 86..87 },
                        },
                        val: Literal {
                            val: Int(11),
                            info: LocInfo { file: 0, span: 95..97 },
                        },
                        ty: Int,
                        info: LocInfo { file: 0, span: 78..98 },
                    },
                    info: LocInfo { file: 0, span: 78..98 },
                },
            ],
            info: LocInfo { file: 0, span: 0..82 },
        },
    ],
    [],
)
```

# Bytecode IR  

```
(
    [
        Halt,
    ],
    [],
)
```

# Raw Bytecode  

```
00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
00 00 00 00 00 00 00 01 16 00 00 00 00 00 00 00
```
