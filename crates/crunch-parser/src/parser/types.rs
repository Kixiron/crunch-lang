use crate::{
    parser::{CurrentFile, Parser},
    token::{Token, TokenType},
};
use alloc::{format, vec::Vec};
use core::convert::TryFrom;
use crunch_shared::{
    crunch_proc::recursion_guard,
    error::{Error, Locatable, Location, ParseResult, Span, SyntaxError},
    trees::{
        ast::{Integer, Type, TypeOp},
        Ref, Sided,
    },
};

type PrefixParselet<'src> = fn(&mut Parser<'src>, Token<'src>) -> ParseResult<Locatable<Type>>;
type PostfixParselet<'src> =
    fn(&mut Parser<'src>, Token<'src>, Locatable<Type>) -> ParseResult<Locatable<Type>>;
type InfixParselet<'src> =
    fn(&mut Parser<'src>, Token<'src>, Locatable<Type>) -> ParseResult<Locatable<Type>>;

// REFACTOR: Split all this into separate functions and share code across them
impl<'src> Parser<'src> {
    /// ```ebnf
    /// Type ::=
    ///     'str' | 'rune' | 'bool' | 'unit' | 'absurd'
    ///     | 'ureg' | 'ireg' | 'uptr' | 'iptr' | Path
    ///     | 'i' [0-9]+ | 'u' [0-9]+ | 'f' [0-9]+
    ///     | '!' Type | 'infer' | '&' 'mut'? Type
    ///     | 'ref'? 'mut'? Type | '*' ('const' | 'mut') Type
    ///     | '(' Type ')'
    ///     | Type '&' Type
    ///     | Type '|' Type
    ///     | Path '[' Type ']'
    ///     | 'type' '[' Type ']'
    ///     | 'slice' '[' Type ']'
    ///     | 'const' Ident (':' Type)?
    ///     | Type 'if' Expr 'else' Type
    ///     | 'arr' '[' Literal, Type ']'
    ///     | 'tup' '[' (Type | Type ',' Type)+ ']'
    ///     | 'fn' '(' (Type | Type ',' Type)? ')' ('->' Type)?
    /// ```
    // TODO: Decide about `Self` and `self`
    // TODO: Add `'&' 'mut'? Type` and `'ref'? 'mut'? Type`
    #[recursion_guard]
    pub(super) fn ascribed_type(&mut self) -> ParseResult<Locatable<Type>> {
        self.ascribed_type_internal(0)
    }

    #[inline(always)]
    fn type_precedence(&self) -> usize {
        self.peek
            .map(|p| {
                TypePrecedence::try_from(p.ty())
                    .map(|p| p.precedence())
                    .unwrap_or(0)
            })
            .unwrap_or(0)
    }

    #[recursion_guard]
    fn ascribed_type_internal(&mut self, precedence: usize) -> ParseResult<Locatable<Type>> {
        let mut token = self.next()?;

        let prefix = Self::type_prefix(token.ty());
        if let Some(prefix) = prefix {
            let mut left = prefix(self, token)?;

            if let Ok(peek) = self.peek() {
                let postfix = Self::type_postfix(peek.ty());
                if let Some(postfix) = postfix {
                    token = self.next()?;
                    left = postfix(self, token, left)?;
                }
            }

            while precedence < self.type_precedence() {
                token = self.next()?;

                let infix = Self::type_infix(token.ty());
                if let Some(infix) = infix {
                    left = infix(self, token, left)?;
                } else {
                    break;
                }
            }

            Ok(left)
        } else {
            Err(Locatable::new(
                Error::Syntax(SyntaxError::Generic(format!(
                    "Could not parse `{}`",
                    token.ty()
                ))),
                Location::new(&token, self.current_file),
            ))
        }
    }

    fn type_prefix(ty: TokenType) -> Option<PrefixParselet<'src>> {
        let prefix: PrefixParselet = match ty {
            // Types and builtins
            TokenType::Ident => |parser, token| {
                let _frame = parser.add_stack_frame()?;

                let (ty, end) = match token.source() {
                    "str" => (Type::String, None),
                    "rune" => (Type::Rune, None),
                    "bool" => (Type::Bool, None),
                    "unit" => (Type::Unit, None),
                    "absurd" => (Type::Absurd, None),

                    "ureg" | "ireg" => (
                        Type::IntReg {
                            signed: token.source() == "ireg",
                        },
                        None,
                    ),

                    "uptr" | "iptr" => (
                        Type::IntPtr {
                            signed: token.source() == "iptr",
                        },
                        None,
                    ),

                    "arr" => {
                        parser.eat(TokenType::LeftBrace, [TokenType::Newline])?;

                        let element = Ref::new(parser.ascribed_type()?);
                        parser.eat(TokenType::Semicolon, [TokenType::Newline])?;

                        let int = parser.eat(TokenType::Int, [TokenType::Newline])?;
                        let Integer { sign, bits: length } = parser
                            .literal(&int, parser.current_file)?
                            .val
                            .into_integer()
                            .expect("An integer was eaten, so it should be parsed as well");

                        if sign.is_negative() {
                            return Err(Locatable::new(
                                Error::Syntax(SyntaxError::NegativeArrayLen),
                                Location::new(int.span(), parser.current_file),
                            ));
                        }

                        let end = parser
                            .eat(TokenType::RightBrace, [TokenType::Newline])?
                            .span();

                        crunch_shared::warn!(
                            "Array lengths will be truncated from a u128 to a u64 without warning, add an error if there's an overflow",
                        );

                        (
                            Type::Array {
                                element,
                                length: length as u64,
                            },
                            Some(end),
                        )
                    }

                    "slice" => {
                        parser.eat(TokenType::LeftBrace, [TokenType::Newline])?;
                        let element = Ref::new(parser.ascribed_type()?);
                        let end = parser
                            .eat(TokenType::RightBrace, [TokenType::Newline])?
                            .span();

                        (Type::Slice { element }, Some(end))
                    }

                    "tup" => {
                        parser.eat(TokenType::LeftBrace, [TokenType::Newline])?;

                        let mut types = Vec::with_capacity(3);
                        while parser.peek()?.ty() != TokenType::RightBrace {
                            if token.ty() == TokenType::Newline {
                                parser.eat(TokenType::Newline, [])?;
                                continue;
                            }

                            types.push(parser.ascribed_type()?);

                            // TODO: Helpful hint
                            if parser.peek()?.ty() == TokenType::Comma {
                                parser.eat(TokenType::Comma, [])?;
                            } else {
                                break;
                            }
                        }
                        let end = parser
                            .eat(TokenType::RightBrace, [TokenType::Newline])?
                            .span();

                        (Type::Tuple(types), Some(end))
                    }

                    uint if uint.starts_with('u')
                        && uint.len() > 1
                        && uint.chars().skip(1).all(char::is_numeric) =>
                    {
                        // TODO: Return error if it's an empty string
                        let uint = uint.get(1..).unwrap_or("");
                        let width: u16 = lexical_core::parse(uint.as_bytes()).map_err(|_| {
                                Locatable::new(
                                    Error::Syntax(SyntaxError::Generic(format!(
                                        "Unsigned integer types must be between `u1` and `u65536`, `{}` is out of range",
                                        uint,
                                    ))),
                                    Location::new(token.span(), parser.current_file),
                                )
                            })?;

                        (
                            Type::Integer {
                                signed: Some(false),
                                width: Some(width),
                            },
                            None,
                        )
                    }

                    int if int.starts_with('i')
                        && int.len() > 1
                        && int.chars().skip(1).all(char::is_numeric) =>
                    {
                        // TODO: Return error if it's an empty string
                        let int = int.get(1..).unwrap_or("");
                        let width: u16 = lexical_core::parse(int.as_bytes()).map_err(|_| {
                                Locatable::new(
                                    Error::Syntax(SyntaxError::Generic(format!(
                                        "Integer types must be between `i1` and `i65536`, `{}` is out of range",
                                        int,
                                    ))),
                                    Location::new(token.span(), parser.current_file),
                                )
                            })?;

                        (
                            Type::Integer {
                                signed: Some(true),
                                width: Some(width),
                            },
                            None,
                        )
                    }

                    float
                        if float.starts_with('f')
                            && float.len() > 1
                            && float.chars().skip(1).all(char::is_numeric) =>
                    {
                        // TODO: Return error if it's an empty string
                        let float = float.get(1..).unwrap_or("");
                        let width: u16 = lexical_core::parse(float.as_bytes()).map_err(|_| {
                                Locatable::new(
                                    Error::Syntax(SyntaxError::Generic(format!(
                                        "Float types must be between `f1` and `f65536`, `{}` is out of range",
                                        float,
                                    ))),
                                    Location::new(token.span(), parser.current_file),
                                )
                            })?;

                        (Type::Float { width }, None)
                    }

                    custom => {
                        let custom = parser.context.strings.intern(custom);
                        let path = parser.item_path(custom)?;

                        if parser.peek().map(|t| t.ty()) == Ok(TokenType::LeftBrace) {
                            parser.eat(TokenType::LeftBrace, [])?;

                            let mut bounds = Vec::with_capacity(3);
                            while parser.peek()?.ty() != TokenType::RightBrace {
                                if token.ty() == TokenType::Newline {
                                    parser.eat(TokenType::Newline, [])?;
                                    continue;
                                }

                                bounds.push(parser.ascribed_type()?);

                                // TODO: Helpful hint
                                if parser.peek()?.ty() == TokenType::Comma {
                                    parser.eat(TokenType::Comma, [])?;
                                } else {
                                    break;
                                }
                            }

                            let end = parser.eat(TokenType::RightBrace, [])?.span();

                            (Type::Bounded { path, bounds }, Some(end))
                        } else {
                            (Type::ItemPath(path), None)
                        }
                    }
                };

                Ok(Locatable::new(
                    ty,
                    Location::new(
                        Span::merge(token.span(), end.unwrap_or(token.span())),
                        parser.current_file,
                    ),
                ))
            },

            // Negation
            TokenType::Bang => |parser, bang| {
                let _frame = parser.add_stack_frame()?;
                let ty = Ref::new(parser.ascribed_type()?);
                let loc = Location::new(Span::merge(bang.span(), ty.span()), parser.current_file);

                Ok(Locatable::new(Type::Not(ty), loc))
            },

            // Grouping via parentheses
            TokenType::LeftParen => |parser, paren| {
                let _frame = parser.add_stack_frame()?;
                let ty = Ref::new(parser.ascribed_type()?);
                let end = parser
                    .eat(TokenType::RightParen, [TokenType::Newline])?
                    .span();

                Ok(Locatable::new(
                    Type::Paren(ty),
                    Location::new(Span::merge(paren.span(), end), parser.current_file),
                ))
            },

            // Function pointers
            TokenType::Function => |parser, func| {
                let _frame = parser.add_stack_frame()?;
                parser.eat(TokenType::LeftParen, [TokenType::Newline])?;

                let mut params = Vec::with_capacity(5);
                while parser.peek()?.ty() != TokenType::RightParen {
                    if parser.peek()?.ty() == TokenType::Newline {
                        parser.eat(TokenType::Newline, [])?;
                        continue;
                    }

                    params.push(parser.ascribed_type()?);
                }

                let end = parser
                    .eat(TokenType::RightParen, [TokenType::Newline])?
                    .span();
                let ret = if parser.peek().map(|t| t.ty()) == Ok(TokenType::RightArrow) {
                    parser.ascribed_type()?
                } else {
                    Locatable::new(
                        Type::Unit,
                        Location::new(Span::merge(func.span(), end), parser.current_file),
                    )
                };

                let loc = Location::new(Span::merge(func.span(), ret.span()), parser.current_file);
                let ty = Type::Func {
                    params,
                    ret: Ref::new(ret),
                };

                Ok(Locatable::new(ty, loc))
            },

            // Trait objects
            TokenType::Type => |parser, ty| {
                let _frame = parser.add_stack_frame()?;

                parser.eat(TokenType::LeftBrace, [TokenType::Newline])?;

                let mut types = Vec::with_capacity(3);
                while parser.peek()?.ty() != TokenType::RightBrace {
                    if parser.peek()?.ty() == TokenType::Newline {
                        parser.eat(TokenType::Newline, [])?;
                        continue;
                    }

                    types.push(parser.ascribed_type()?);

                    // TODO: Nice error
                    if parser.peek()?.ty() == TokenType::Comma {
                        parser.eat(TokenType::Comma, [])?;
                    } else {
                        break;
                    }
                }

                let end = parser.eat(TokenType::RightBrace, [])?.span();

                Ok(Locatable::new(
                    Type::Trait(types),
                    Location::new(Span::merge(ty.span(), end), parser.current_file),
                ))
            },

            // Const parameters
            TokenType::Const => |parser, cons| {
                let _frame = parser.add_stack_frame()?;

                let ident = parser.eat(TokenType::Ident, [TokenType::Newline])?;
                let ident = parser.context.strings.intern(ident.source());

                parser.eat(TokenType::Colon, [])?;
                let ty = parser.ascribed_type()?;
                let loc = Location::new(Span::merge(cons.span(), ty.span()), parser.current_file);

                Ok(Locatable::new(Type::Const(ident, Ref::new(ty)), loc))
            },

            // References
            TokenType::Ampersand => |parser, star| {
                let _frame = parser.add_stack_frame()?;

                let mutable = if [TokenType::Const, TokenType::Mut].contains(&parser.peek()?.ty()) {
                    parser.eat_of([TokenType::Const, TokenType::Mut], [])?.ty() == TokenType::Mut
                } else {
                    false
                };
                let referee = Ref::new(parser.ascribed_type()?);
                let loc = Location::new(
                    Span::merge(star.span(), referee.span()),
                    parser.current_file,
                );

                Ok(Locatable::new(Type::Reference { referee, mutable }, loc))
            },

            // Raw pointers
            TokenType::Star => |parser, star| {
                let _frame = parser.add_stack_frame()?;

                let mutable =
                    parser.eat_of([TokenType::Const, TokenType::Mut], [])?.ty() == TokenType::Mut;
                let pointee = Ref::new(parser.ascribed_type()?);
                let loc = Location::new(
                    Span::merge(star.span(), pointee.span()),
                    parser.current_file,
                );

                Ok(Locatable::new(Type::Pointer { pointee, mutable }, loc))
            },

            _ => return None,
        };

        Some(prefix)
    }

    fn type_postfix(_ty: TokenType) -> Option<PostfixParselet<'src>> {
        None
    }

    fn type_infix(ty: TokenType) -> Option<InfixParselet<'src>> {
        let infix: InfixParselet = match ty {
            TokenType::Ampersand | TokenType::Pipe => |parser, operand, left| {
                let _frame = parser.add_stack_frame()?;

                let rhs = Ref::new(parser.ascribed_type()?);

                let loc = Location::new(Span::merge(left.span(), rhs.span()), parser.current_file);
                let ty = Type::Operand(Sided {
                    lhs: Ref::new(left),
                    op: parser.type_op(&operand, parser.current_file)?,
                    rhs,
                });

                Ok(Locatable::new(ty, loc))
            },

            // TokenType::If => |parser, _if, true_arm| {
            //     let _frame = parser.add_stack_frame()?;
            //
            //     let condition = parser.expr()?;
            //     parser.eat(TokenType::Else, [TokenType::Newline])?;
            //
            //     let false_arm = parser.ascribed_type()?;
            //     let ty = Type::InlineConditional {
            //         true_arm,
            //         condition,
            //         false_arm,
            //     };
            //
            //     Ok(ty)
            // },
            _ => return None,
        };

        Some(infix)
    }

    #[recursion_guard]
    fn type_op(&self, token: &Token<'_>, file: CurrentFile) -> ParseResult<TypeOp> {
        match token.ty() {
            TokenType::Ampersand => Ok(TypeOp::And),
            TokenType::Pipe => Ok(TypeOp::Or),

            ty => Err(Locatable::new(
                Error::Syntax(SyntaxError::Generic(format!(
                    "Expected one of {}, got '{}'",
                    [TokenType::Ampersand, TokenType::Pipe]
                        .iter()
                        .map(|t| t.to_str())
                        .collect::<Vec<_>>()
                        .join(", "),
                    ty,
                ))),
                Location::new(token, file),
            )),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TypePrecedence {
    And,
    Or,
    Ternary,
}

impl TypePrecedence {
    pub fn precedence(self) -> usize {
        match self {
            Self::And => 6,
            Self::Or => 4,
            Self::Ternary => 1,
        }
    }
}

impl TryFrom<TokenType> for TypePrecedence {
    type Error = ();

    fn try_from(t: TokenType) -> Result<TypePrecedence, ()> {
        Ok(match t {
            TokenType::Ampersand => Self::And,
            TokenType::Pipe => Self::Or,
            TokenType::If | TokenType::Function => Self::Ternary,

            _ => return Err(()),
        })
    }
}

// TODO: Test the rest
#[cfg(test)]
mod tests {
    /*
    use super::*;
    use crunch_shared::{context::Context, files::FileId};

    #[test]
    fn builtins() {
        let types = [
            ("str", Type::String),
            ("rune", Type::Rune),
            ("bool", Type::Bool),
            ("unit", Type::Unit),
            ("absurd", Type::Absurd),
            ("ureg", Type::IntReg(Signedness::Unsigned)),
            ("ireg", Type::IntReg(Signedness::Signed)),
            ("uptr", Type::IntPtr(Signedness::Unsigned)),
            ("iptr", Type::IntPtr(Signedness::Signed)),
        ];
        let context = Context::new();

        for (src, correct) in types.iter() {
            let current_file = CurrentFile::new(FileId::new(0), src.len());
            let ty = Parser::new(src, current_file, context.clone())
                .ascribed_type()
                .unwrap();

            assert_eq!(ty.as_ref(), correct);
        }
    }

    #[test]
    fn ints() {
        let types = [
            (
                "i0",
                Type::Integer {
                    sign: Signedness::Signed,
                    width: 0,
                },
            ),
            (
                "i65535",
                Type::Integer {
                    sign: Signedness::Signed,
                    width: 65535,
                },
            ),
            (
                "u0",
                Type::Integer {
                    sign: Signedness::Unsigned,
                    width: 0,
                },
            ),
            (
                "u65535",
                Type::Integer {
                    sign: Signedness::Unsigned,
                    width: 65535,
                },
            ),
            ("f0", Type::Float { width: 0 }),
            ("f65535", Type::Float { width: 65535 }),
        ];
        let context = Context::new();

        for (src, correct) in types.iter() {
            let current_file = CurrentFile::new(FileId::new(0), src.len());
            let ty = Parser::new(src, current_file, context.clone())
                .ascribed_type()
                .unwrap();

            assert_eq!(ty.as_ref(), correct);
        }
    }

    #[test]
    fn not() {
        let context = Context::new();
        let types = [
            ("!str", Type::Not(Ref::new(Type::String))),
            ("!rune", Type::Not(Ref::new(Type::Rune))),
        ];

        for (src, correct) in types.iter() {
            let current_file = CurrentFile::new(FileId::new(0), src.len());
            let ty = Parser::new(src, current_file, context.clone())
                .ascribed_type()
                .unwrap();

            assert_eq!(ty.as_ref(), correct);
        }
    }

    #[test]
    fn paren() {
        let context = Context::new();
        let types = [
            ("(str)", Type::Paren(Ref::new(Type::String))),
            ("(rune)", Type::Paren(Ref::new(Type::Rune))),
        ];

        for (src, correct) in types.iter() {
            let current_file = CurrentFile::new(FileId::new(0), src.len());
            let ty = Parser::new(src, current_file, context.clone())
                .ascribed_type()
                .unwrap();

            assert_eq!(ty.as_ref(), correct);
        }
    }

    #[test]
    fn operands() {
        let context = Context::new();

        let types = [
            (
                "str & rune",
                Type::Operand(Sided {
                    lhs: Ref::new(Type::String),
                    op: TypeOp::And,
                    rhs: Ref::new(Type::Rune),
                }),
            ),
            (
                "str | rune",
                Type::Operand(Sided {
                    lhs: Ref::new(Type::String),
                    op: TypeOp::Or,
                    rhs: Ref::new(Type::Rune),
                }),
            ),
        ];

        for (src, correct) in types.iter() {
            let current_file = CurrentFile::new(FileId::new(0), src.len());
            let ty = Parser::new(src, current_file, context.clone())
                .ascribed_type()
                .unwrap();

            assert_eq!(&ty, correct);
        }
    }

    #[cfg(not(any(target_arch = "wasm32", miri)))]
    mod proptests {
        use super::*;
        use proptest::prelude::*;

        proptest! {
            #[test]
            fn int_types(s in r#"i[0-9]+"#) {
                let context = Context::default();
                let mut parser = Parser::new(&s, CurrentFile::new(FileId(0), s.len()), context);

                let ty = parser.ascribed_type();
                match ty.as_ref().map(|t| &*t) {
                    Ok(Type::Integer { sign: Signedness::Signed, .. }) => {},
                    Err(Locatable { data: _data @ Error::Syntax(SyntaxError::Generic(..)), .. }) => {}

                    _ => prop_assert!(false),
                }
            }

            #[test]
            fn uint_types(s in r#"u[0-9]+"#) {
                let context = Context::default();
                let mut parser = Parser::new(&s, CurrentFile::new(FileId(0), s.len()), context);

                let ty = parser.ascribed_type();
                match ty.as_ref().map(|t| &*t) {
                    Ok(Type::Integer { sign: Signedness::Unsigned, .. }) => {},
                    Err(Locatable { data: _data @ Error::Syntax(SyntaxError::Generic(..)), .. }) => {}

                    _ => prop_assert!(false),
                }
            }

            #[test]
            fn float(s in r#"f[0-9]+"#) {
                let context = Context::default();
                let mut parser = Parser::new(&s, CurrentFile::new(FileId(0), s.len()), context);

                let ty = parser.ascribed_type();
                match ty.as_ref().map(|t| &*t) {
                    Ok(Type::Float { .. }) => {},
                    Err(Locatable { data: _data @ Error::Syntax(SyntaxError::Generic(..)), .. }) => {}

                    _ => prop_assert!(false),
                }
            }
        }
    }
    */
}
