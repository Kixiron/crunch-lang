use crate::{
    parser::{CurrentFile, Parser},
    token::{Token, TokenType},
};
use alloc::{format, string::String, vec::Vec};
use core::convert::TryFrom;
use crunch_proc::recursion_guard;
use crunch_shared::{
    error::{Error, Locatable, Location, ParseResult, SyntaxError},
    trees::{
        ast::{Integer, Signedness, Type, TypeOp},
        Ref, Sided,
    },
};

type PrefixParselet<'src> = fn(&mut Parser<'src>, Token<'src>) -> ParseResult<Type>;
type PostfixParselet<'src> = fn(&mut Parser<'src>, Token<'src>, Type) -> ParseResult<Type>;
type InfixParselet<'src> = fn(&mut Parser<'src>, Token<'src>, Type) -> ParseResult<Type>;

impl<'src> Parser<'src> {
    /// ```ebnf
    /// Type ::=
    ///     'str' | 'rune' | 'bool' | 'unit' | 'absurd'
    ///     | 'ureg' | 'ireg' | 'uptr' | 'iptr' | Path
    ///     | 'i' [0-9]+ | 'u' [0-9]+ | 'f' [0-9]+
    ///     | '!' Type | 'infer'
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
    #[recursion_guard]
    pub(super) fn ascribed_type(&mut self) -> ParseResult<Type> {
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
    fn ascribed_type_internal(&mut self, precedence: usize) -> ParseResult<Type> {
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
                Location::concrete(&token, self.current_file),
            ))
        }
    }

    fn type_prefix(ty: TokenType) -> Option<PrefixParselet<'src>> {
        let prefix: PrefixParselet = match ty {
            // Types and builtins
            TokenType::Ident => |parser, token| {
                let _frame = parser.add_stack_frame()?;

                let (ty, _end) = match token.source() {
                    "str" => (Type::String, None),
                    "rune" => (Type::Rune, None),
                    "bool" => (Type::Bool, None),
                    "unit" => (Type::Unit, None),
                    "absurd" => (Type::Absurd, None),
                    "infer" => (Type::Infer, None),

                    "ureg" | "ireg" => {
                        let sign = if token.source() == "ureg" {
                            Signedness::Unsigned
                        } else {
                            Signedness::Signed
                        };

                        (Type::IntReg(sign), None)
                    }

                    "uptr" | "iptr" => {
                        let sign = if token.source() == "uptr" {
                            Signedness::Unsigned
                        } else {
                            Signedness::Signed
                        };

                        (Type::IntPtr(sign), None)
                    }

                    "arr" => {
                        parser.eat(TokenType::LeftBrace, [TokenType::Newline])?;

                        let int = parser.eat(TokenType::Int, [TokenType::Newline])?;
                        let Integer { sign, bits: len } = parser
                            .literal(&int, parser.current_file)?
                            .into_integer()
                            .expect("An integer was eaten, so it should be parsed as well");

                        if sign.is_negative() {
                            return Err(Locatable::new(
                                Error::Syntax(SyntaxError::NegativeArrayLen),
                                Location::concrete(int.span(), parser.current_file),
                            ));
                        }

                        parser.eat(TokenType::Comma, [TokenType::Newline])?;

                        let ty = Ref::new(parser.ascribed_type()?);
                        let end = parser
                            .eat(TokenType::RightBrace, [TokenType::Newline])?
                            .span();

                        (Type::Array(len, ty), Some(end))
                    }

                    "slice" => {
                        parser.eat(TokenType::LeftBrace, [TokenType::Newline])?;
                        let ty = Ref::new(parser.ascribed_type()?);
                        let end = parser
                            .eat(TokenType::RightBrace, [TokenType::Newline])?
                            .span();

                        (Type::Slice(ty), Some(end))
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
                        let int = uint.chars().skip(1).collect::<String>();
                        let width: u16 = lexical_core::parse(int.as_bytes()).map_err(|_| {
                                Locatable::new(
                                    Error::Syntax(SyntaxError::Generic(format!(
                                        "Unsigned integer types must be between `u1` and `u65536`, `{}` is out of range",
                                        uint,
                                    ))),
                                    Location::concrete(token.span(), parser.current_file),
                                )
                            })?;

                        (
                            Type::Integer {
                                sign: Signedness::Unsigned,
                                width,
                            },
                            None,
                        )
                    }

                    int if int.starts_with('i')
                        && int.len() > 1
                        && int.chars().skip(1).all(char::is_numeric) =>
                    {
                        let int = int.chars().skip(1).collect::<String>();
                        let width: u16 = lexical_core::parse(int.as_bytes()).map_err(|_| {
                                Locatable::new(
                                    Error::Syntax(SyntaxError::Generic(format!(
                                        "Integer types must be between `i1` and `i65536`, `{}` is out of range",
                                        int,
                                    ))),
                                    Location::concrete(token.span(), parser.current_file),
                                )
                            })?;

                        (
                            Type::Integer {
                                sign: Signedness::Signed,
                                width,
                            },
                            None,
                        )
                    }

                    float
                        if float.starts_with('f')
                            && float.len() > 1
                            && float.chars().skip(1).all(char::is_numeric) =>
                    {
                        let float = float.chars().skip(1).collect::<String>();
                        let width: u16 = lexical_core::parse(float.as_bytes()).map_err(|_| {
                                Locatable::new(
                                    Error::Syntax(SyntaxError::Generic(format!(
                                        "Float types must be between `f1` and `f65536`, `{}` is out of range",
                                        float,
                                    ))),
                                    Location::concrete(token.span(), parser.current_file),
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

                Ok(ty)
            },

            // Negation
            TokenType::Bang => |parser, _bang| {
                let _frame = parser.add_stack_frame()?;
                let ty = Ref::new(parser.ascribed_type()?);

                Ok(Type::Not(ty))
            },

            // Grouping via parentheses
            TokenType::LeftParen => |parser, _paren| {
                let _frame = parser.add_stack_frame()?;
                let ty = Ref::new(parser.ascribed_type()?);
                parser.eat(TokenType::RightParen, [TokenType::Newline])?;

                Ok(Type::Paren(ty))
            },

            // Function pointers
            TokenType::Function => |parser, _func| {
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

                parser.eat(TokenType::RightParen, [TokenType::Newline])?;
                let ret = if parser.peek().map(|t| t.ty()) == Ok(TokenType::RightArrow) {
                    parser.ascribed_type()?
                } else {
                    Type::Infer
                };

                let ty = Type::Func {
                    params,
                    ret: Ref::new(ret),
                };

                Ok(ty)
            },

            // Trait objects
            TokenType::Type => |parser, _ty| {
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

                parser.eat(TokenType::RightBrace, [])?;

                Ok(Type::Trait(types))
            },

            // Const parameters
            TokenType::Const => |parser, _cons| {
                let _frame = parser.add_stack_frame()?;

                let ident = parser.eat(TokenType::Ident, [TokenType::Newline])?;
                let ident = parser.context.strings.intern(ident.source());

                let ty = if parser.peek()?.ty() == TokenType::Colon {
                    parser.eat(TokenType::Colon, [])?;
                    parser.ascribed_type()?
                } else {
                    Type::Infer
                };

                Ok(Type::Const(ident, Ref::new(ty)))
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
                let ty = Type::Operand(Sided {
                    lhs: Ref::new(left),
                    op: parser.type_op(&operand, parser.current_file)?,
                    rhs,
                });

                Ok(ty)
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
                Location::concrete(token, file),
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
    use super::*;
    use crunch_shared::{
        context::Context,
        error::{Error, Locatable, SyntaxError},
        files::FileId,
    };

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

            assert_eq!(&ty, correct);
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

            assert_eq!(&ty, correct);
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

            assert_eq!(&ty, correct);
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

            assert_eq!(&ty, correct);
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
}
