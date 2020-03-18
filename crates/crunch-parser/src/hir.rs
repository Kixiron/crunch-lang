use crate::ast::*;

use randomize::PCG32;
use string_interner::Sym;

use std::collections::HashMap;

const MANGLED_ENTROPY_LENGTH: usize = 10;

pub struct Mangler {
    rng: PCG32,
}

impl Mangler {
    pub fn new(seed: u64, inc: u64) -> Self {
        Self {
            rng: PCG32::seed(seed, inc),
        }
    }

    pub fn mangle(&mut self, path: &[&str]) -> Vec<u8> {
        let mut mangled = Vec::with_capacity(
            path.iter().map(|s| s.len() + 1).sum::<usize>() + 4 + MANGLED_ENTROPY_LENGTH,
        );
        mangled.extend_from_slice(&[0x00; MANGLED_ENTROPY_LENGTH]);
        self.rng.fill_bytes(&mut mangled[..MANGLED_ENTROPY_LENGTH]);

        mangled.extend_from_slice(&(path.len() as u32).to_ne_bytes());

        for seg in path {
            let bytes = seg.as_bytes();
            match bytes.len() {
                len if len <= u16::max_value() as usize => {
                    mangled.extend_from_slice(&(len as u16).to_ne_bytes())
                }
                _ => panic!("Segment too long"),
            }

            mangled.extend_from_slice(bytes);
        }

        dbg!(mangled)
    }

    pub fn unmangle(mut mangled: &[u8]) -> Vec<String> {
        use core::convert::TryInto;

        mangled = &mangled[MANGLED_ENTROPY_LENGTH..]; // Remove entropy

        let number_segments =
            u32::from_ne_bytes(mangled[..4].try_into().unwrap_or_else(|_| unreachable!())) as usize;
        let mut unmangled = Vec::with_capacity(number_segments);
        mangled = &mangled[4..];

        for _ in 0..number_segments {
            let segment_len = dbg!(u16::from_ne_bytes(
                mangled[..2].try_into().unwrap_or_else(|_| unreachable!())
            ) as usize);
            mangled = &mangled[2..];

            let segment = String::from_utf8(mangled[..segment_len].to_vec())
                .unwrap_or_else(|_| unreachable!());
            mangled = &mangled[segment_len..];

            unmangled.push(segment);
        }

        debug_assert!(mangled.is_empty());

        unmangled
    }
}

impl Default for Mangler {
    fn default() -> Self {
        const INC: u64 = 3;
        // TODO: WASM and ASLR may not mix
        // TODO: Maybe use the `getrandom` crate on WASM and/or all arches
        Self::new(&INC as *const u64 as u64, INC)
    }
}

#[derive(Debug, Clone)]
pub enum HirValue {
    Var(usize),
    Function(Sym),
    Literal(HirLiteral),
}

#[derive(Debug, Clone)]
pub enum OneOf<L, M, R> {
    Left(L),
    Middle(M),
    Right(R),
}

#[derive(Debug, Clone)]
pub enum HirLiteral {
    String(Sym),
    Integer(i32),
    Boolean(bool),
    Array(Vec<HirValue>),
}

#[derive(Debug, Clone)]
pub enum Hir {
    Loop {
        body: Vec<Hir>,
    },
    Assign {
        target: usize,
        value: HirValue,
    },
    BinOp {
        target: usize,
        left: HirValue,
        right: HirValue,
        operand: BinaryOp,
    },
    Conditional {
        conditions: Vec<Hir>,
        else_body: Option<Vec<Hir>>,
    },
    If {
        condition: Box<Hir>,
        body: Vec<Hir>,
    },
    Comparison {
        left: HirValue,
        right: Option<HirValue>,
        comparator: Option<Comparator>,
    },
    Break {
        value: Option<HirValue>,
    },
    Return {
        value: Option<HirValue>,
    },
    Continue,
    Expr(Expr),
}

pub fn compile_hir(
    mangler: &mut Mangler,
    interner: &string_interner::StringInterner<Sym>,
    FunctionDecl { body, abs_path, .. }: FunctionDecl,
) -> (Vec<u8>, Vec<Hir>, usize) {
    let mangled_name = mangler.mangle(
        &abs_path
            .iter()
            .map(|seg| interner.resolve(*seg).unwrap())
            .collect::<Vec<&str>>(),
    );
    let mut hir = Vec::with_capacity(body.len());
    let mut next_orphan = 0;
    let mut symbols = HashMap::with_capacity(50);

    for stmt in body {
        hir.extend_from_slice(&compile_statement(stmt, &mut next_orphan, &mut symbols));
    }

    (mangled_name, hir, next_orphan)
}

/*
TODO: State based translator

enum State {
    TopLevel,
    Conditional {
        number_conditions: usize,
        else_body: usize,
    },
    Statement(Statement),
    Hir(Hir),
}

impl State {
    pub fn to_hir(self) -> Option<Hir> {
        match self {
            Self::Hir(hir) => Some(hir),
            _ => None,
        }
    }

    pub fn to_statement(self) -> Option<Statement> {
        match self {
            Self::Statement(stmt) => Some(stmt),
            _ => None,
        }
    }
}

fn compile_to_hir(
    mangler: &mut Mangler,
    interner: &string_interner::StringInterner<Sym>,
    FunctionDecl { body, abs_path, .. }: FunctionDecl,
) -> (Vec<u8>, Vec<Hir>) {
    let mangled_name = mangler.mangle(
        &abs_path
            .iter()
            .map(|seg| interner.resolve(*seg).unwrap())
            .collect::<Vec<&str>>(),
    );
    let mut hir = Vec::with_capacity(body.len());
    let mut next_orphan = 0;

    let mut body = body.into_iter();
    let mut current_hir = Vec::with_capacity(20);

    let mut states = Vec::with_capacity(20);
    states.push(State::TopLevel);

    while let Some(state) = states.pop() {
        match state {
            State::TopLevel => {
                states.push(State::Statement(body.next().unwrap()));
            }

            State::Statement(stmt) => match stmt {
                Statement::Conditional(Conditional { _if, _else, .. }) => {
                    states.push(State::Conditional {
                        number_conditions: _if.iter().map(|i| i.body.len()).sum::<usize>(),
                        else_body: _else.map(|e| e.body.len()).unwrap_or(0),
                    });

                    for i in _if.into_iter().map(|i| i) {}

                }

                _ => todo!(),
            },

            State::Conditional {
                number_conditions,
                else_body,
            } => {
                let mut conditions = Vec::with_capacity(number_conditions);
                for _ in 0..number_conditions {
                    conditions.push(current_hir.pop().unwrap());
                }

                let else_body = if else_body > 0 {
                    let mut body = Vec::with_capacity(else_body);
                    for _ in 0..else_body {
                        body.push(current_hir.pop().unwrap());
                    }

                    Some(body)
                } else {
                    None
                };

                hir.push(Hir::Conditional {
                    conditions,
                    else_body,
                });
                states.push(State::TopLevel);
            }

            _ => {}
        }
    }

    (mangled_name, hir)
}
*/

fn comparison_from_expr(
    expr: Expr,
    next_orphan: &mut usize,
    symbols: &mut HashMap<Sym, usize>,
) -> Hir {
    match expr {
        Expr::Literal(lit) => Hir::Comparison {
            left: HirValue::Literal(match lit {
                Literal::Boolean(b) => HirLiteral::Boolean(b),
                _ => todo!(),
            }),
            right: None,
            comparator: None,
        },

        Expr::Ident(var) => Hir::Comparison {
            left: HirValue::Var(if let Some(id) = symbols.get(&var) {
                *id
            } else {
                *next_orphan += 1;
                symbols.insert(var, *next_orphan - 1);
                *next_orphan - 1
            }),
            right: None,
            comparator: None,
        },

        Expr::Comparison(Comparison {
            left,
            comparison,
            right,
            ..
        }) => Hir::Comparison {
            left: hir_value_from_expr(*left, next_orphan, symbols),
            right: Some(hir_value_from_expr(*right, next_orphan, symbols)),
            comparator: Some(comparison),
        },

        _ => todo!(),
    }
}

fn hir_value_from_expr(
    expr: Expr,
    next_orphan: &mut usize,
    symbols: &mut HashMap<Sym, usize>,
) -> HirValue {
    match expr {
        Expr::Literal(lit) => HirValue::Literal(match lit {
            Literal::Boolean(b) => HirLiteral::Boolean(b),
            Literal::String(s) => HirLiteral::String(s),
            Literal::Integer(i) => HirLiteral::Integer(i),
        }),

        Expr::Ident(var) => HirValue::Var(if let Some(id) = symbols.get(&var) {
            *id
        } else {
            *next_orphan += 1;
            symbols.insert(var, *next_orphan - 1);
            *next_orphan - 1
        }),

        Expr::Array(arr) => {
            let mut array = Vec::with_capacity(arr.len());
            for elm in arr {
                array.push(hir_value_from_expr(elm, next_orphan, symbols));
            }

            HirValue::Literal(HirLiteral::Array(array))
        }

        Expr::Expr(expr) => hir_value_from_expr(*expr, next_orphan, symbols),

        _ => todo!(),
    }
}

fn compile_statement(
    stmt: Statement,
    next_orphan: &mut usize,
    symbols: &mut HashMap<Sym, usize>,
) -> Vec<Hir> {
    let mut hir = Vec::with_capacity(10);

    match stmt {
        Statement::Conditional(Conditional { _if, _else, .. }) => {
            let mut conditions = Vec::with_capacity(_if.len());
            for If {
                condition, body, ..
            } in _if
            {
                conditions.push(Hir::If {
                    condition: Box::new(comparison_from_expr(condition, next_orphan, symbols)),

                    body: body
                        .into_iter()
                        .map(|s| compile_statement(s, next_orphan, symbols))
                        .flatten()
                        .collect(),
                });
            }

            hir.push(Hir::Conditional {
                conditions,
                else_body: _else.map(|e| {
                    e.body
                        .into_iter()
                        .map(|s| compile_statement(s, next_orphan, symbols))
                        .flatten()
                        .collect()
                }),
            });
        }

        // Transform while loops to a desugared form
        // ```
        // while 1 < 2
        //      <body>
        // end
        // ```
        // becomes
        // ```
        // loop
        //     if 1 < 2
        //         break
        //     end
        //
        //     <body>
        // end
        // ```
        // And for `then` clauses
        // ```
        // while 1 < 2
        //      <body>
        // then
        //     <then body>
        // end
        // ```
        // becomes
        // ```
        // let broke_loop = false
        // loop
        //     if 1 < 2
        //         broke_loop = true
        //         break
        //     end
        //
        //     <body>
        // end
        //
        // if broke_loop
        //     <then body>
        // end
        // ```
        Statement::While(While {
            body,
            condition,
            then,
            ..
        }) => {
            let then_boolean = if then.is_some() {
                *next_orphan += 1;
                hir.push(Hir::Assign {
                    target: *next_orphan - 1,
                    value: HirValue::Literal(HirLiteral::Boolean(false)),
                });

                Some(*next_orphan - 1)
            } else {
                None
            };

            let mut loop_body: Vec<Hir> = body
                .into_iter()
                .map(|s| compile_statement(s, next_orphan, symbols))
                .flatten()
                .collect();

            let loaded_cond = {
                *next_orphan += 1;
                *next_orphan - 1
            };
            let loaded_false = {
                *next_orphan += 1;
                *next_orphan - 1
            };

            match condition {
                Expr::Literal(lit) => loop_body.insert(
                    0,
                    Hir::Conditional {
                        conditions: vec![
                            Hir::Assign {
                                target: loaded_cond,
                                value: HirValue::Literal(match lit {
                                    Literal::Boolean(b) => HirLiteral::Boolean(b),
                                    Literal::String(s) => HirLiteral::String(s),
                                    Literal::Integer(i) => HirLiteral::Integer(i),
                                }),
                            },
                            Hir::If {
                                condition: Box::new(Hir::Comparison {
                                    left: HirValue::Var(loaded_cond),
                                    right: Some(HirValue::Var(loaded_false)),
                                    comparator: Some(Comparator::Equal),
                                }),
                                body: vec![
                                    Hir::Assign {
                                        target: then_boolean.unwrap(),
                                        value: HirValue::Literal(HirLiteral::Boolean(true)),
                                    },
                                    Hir::Break { value: None },
                                ],
                            },
                        ],
                        else_body: None,
                    },
                ),

                Expr::Range(_) => todo!(),
                Expr::Comparison(Comparison {
                    left,
                    comparison,
                    right,
                    ..
                }) => loop_body.insert(
                    0,
                    Hir::Conditional {
                        conditions: vec![Hir::If {
                            condition: Box::new(Hir::Comparison {
                                left: hir_value_from_expr(*left, next_orphan, symbols),
                                right: Some(hir_value_from_expr(*right, next_orphan, symbols)),
                                comparator: Some(comparison),
                            }),
                            body: vec![
                                Hir::Assign {
                                    target: then_boolean.unwrap(),
                                    value: HirValue::Literal(HirLiteral::Boolean(true)),
                                },
                                Hir::Break { value: None },
                            ],
                        }],
                        else_body: None,
                    },
                ),

                _ => todo!(),
            }

            hir.push(Hir::Loop { body: loop_body });

            if let Some(Else { body, .. }) = then {
                hir.push(Hir::Conditional {
                    conditions: vec![Hir::Comparison {
                        left: HirValue::Var(then_boolean.unwrap()),
                        right: None,
                        comparator: None,
                    }],
                    else_body: None,
                });
                hir.extend_from_slice(
                    &body
                        .into_iter()
                        .map(|s| compile_statement(s, next_orphan, symbols))
                        .flatten()
                        .collect::<Vec<_>>(),
                )
            }
        }

        Statement::Loop(Loop { body, .. }) => {
            hir.push(Hir::Loop {
                body: body
                    .into_iter()
                    .map(|s| compile_statement(s, next_orphan, symbols))
                    .flatten()
                    .collect(),
            });
        }

        Statement::For(_) => todo!(),

        // Desugar assignments
        // ```
        // i = 10
        // ```
        // stays the same
        // ```
        // i += 10
        // ```
        // becomes
        // ```
        // let value = 10
        // i = i + value
        // ```
        Statement::Assign(Assign { var, expr, ty, .. }) => match ty {
            AssignType::Normal => {
                hir.push(Hir::Assign {
                    target: if let Some(id) = symbols.get(&var) {
                        *id
                    } else {
                        *next_orphan += 1;
                        symbols.insert(var, *next_orphan - 1);
                        *next_orphan - 1
                    },
                    value: hir_value_from_expr(expr, next_orphan, symbols),
                });
            }

            AssignType::BinaryOp(op) => {
                let value = {
                    *next_orphan += 1;
                    *next_orphan - 1
                };

                hir.push(Hir::Assign {
                    target: value,
                    value: hir_value_from_expr(expr, next_orphan, symbols),
                });
                hir.push(Hir::BinOp {
                    target: if let Some(id) = symbols.get(&var) {
                        *id
                    } else {
                        *next_orphan += 1;
                        symbols.insert(var, *next_orphan - 1);
                        *next_orphan - 1
                    },
                    left: HirValue::Var(if let Some(id) = symbols.get(&var) {
                        *id
                    } else {
                        *next_orphan += 1;
                        symbols.insert(var, *next_orphan - 1);
                        *next_orphan - 1
                    }),
                    right: HirValue::Var(value),
                    operand: op,
                });
            }
        },

        Statement::VarDecl(VarDecl { name, expr, .. }) => {
            hir.push(Hir::Assign {
                target: if let Some(id) = symbols.get(&name) {
                    *id
                } else {
                    *next_orphan += 1;
                    symbols.insert(name, *next_orphan - 1);
                    *next_orphan - 1
                },
                value: hir_value_from_expr(expr, next_orphan, symbols),
            });
        }

        // Desugar return statements
        // ```
        // return
        // ```
        // is unchanged
        // ```
        // return true
        // ```
        // becomes
        // ```
        // let return_value = true
        // return return_value
        // ```
        Statement::Return(Return { expr, .. }) => {
            let value = if let Some(expr) = expr {
                let val = {
                    *next_orphan += 1;
                    *next_orphan - 1
                };
                hir.push(Hir::Assign {
                    target: val,
                    value: hir_value_from_expr(expr, next_orphan, symbols),
                });

                Some(HirValue::Var(val))
            } else {
                None
            };

            hir.push(Hir::Return { value });
        }

        Statement::Continue => hir.push(Hir::Continue),

        // TODO: Implement breaking with values
        Statement::Break => {
            // let value = if let Some(expr) = expr {
            //     let val = {
            //         *next_orphan += 1;
            //         *next_orphan - 1
            //     };
            //     hir.push(Hir::Assign {
            //         target: HirValue::OrphanVar(val),
            //         value: OneOf::Middle(Box::new(Hir::Expr(expr))),
            //     });
            //
            //     Some(HirValue::OrphanVar(val))
            // } else {
            //     None
            // };

            hir.push(Hir::Break { value: None });
        }

        Statement::Expr(expr) => hir.push(Hir::Expr(expr)),

        Statement::Empty => {}
    }

    hir
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn mangling() {
        let path = &["std", "io", "print"];
        let mut mangler = Mangler::default();

        let mangled = mangler.mangle(path);
        assert_eq!(Mangler::unmangle(&mangled), path);
    }
}
