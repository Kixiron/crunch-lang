use crate::{
    parser::Parser,
    token::{Token, TokenType},
};
use alloc::{format, vec::Vec};
use core::convert::TryFrom;
use crunch_proc::recursion_guard;
use crunch_shared::{
    ast::{Arm, Block, Expr, ExprKind, For, If, IfCond, Loop, Match, Ref, Sided, While},
    error::{Error, Locatable, Location, ParseResult, SyntaxError},
};

type PrefixParselet<'src> = fn(&mut Parser<'src>, Token<'src>) -> ParseResult<Expr>;
type PostfixParselet<'src> = fn(&mut Parser<'src>, Token<'src>, Expr) -> ParseResult<Expr>;
type InfixParselet<'src> = fn(&mut Parser<'src>, Token<'src>, Expr) -> ParseResult<Expr>;

/// Expr Parsing
impl<'src> Parser<'src> {
    #[recursion_guard]
    pub fn expr(&mut self) -> ParseResult<Expr> {
        self.parse_expr(0)
    }

    #[inline(always)]
    fn expr_precedence(&self) -> usize {
        self.peek
            .map(|p| {
                ExprPrecedence::try_from(p.ty())
                    .map(|p| p.precedence())
                    .unwrap_or(0)
            })
            .unwrap_or(0)
    }

    #[recursion_guard]
    fn parse_expr(&mut self, precedence: usize) -> ParseResult<Expr> {
        let mut token = self.next()?;

        let prefix = Self::expr_prefix(token);
        if let Some(prefix) = prefix {
            let mut left = prefix(self, token)?;

            if let Ok(peek) = self.peek() {
                let postfix = Self::expr_postfix(peek);
                if let Some(postfix) = postfix {
                    token = self.next()?;
                    left = postfix(self, token, left)?;
                }
            }

            while precedence < self.expr_precedence() {
                token = self.next()?;

                let infix = Self::expr_infix(token);
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

    fn expr_prefix(token: Token) -> Option<PrefixParselet<'src>> {
        let prefix: PrefixParselet = match token.ty() {
            // Array and tuple literals
            TokenType::Ident if token.source() == "arr" || token.source() == "tup" => {
                |parser, token| {
                    let _frame = parser.add_stack_frame()?;

                    parser.eat(TokenType::LeftBrace, [TokenType::Newline])?;

                    let mut elements = Vec::with_capacity(5);
                    while parser.peek()?.ty() != TokenType::RightBrace {
                        let elm = parser.expr()?;
                        elements.push(elm);

                        if parser.peek()?.ty() == TokenType::Comma {
                            parser.eat(TokenType::Comma, [TokenType::Newline])?;
                        } else {
                            break;
                        }
                    }

                    parser.eat(TokenType::RightBrace, [TokenType::Newline])?;

                    let kind = if token.source() == "arr" {
                        ExprKind::Array(elements)
                    } else {
                        ExprKind::Tuple(elements)
                    };

                    Ok(Expr { kind })
                }
            }

            TokenType::If => |parser, _token| Ok(parser.if_stmt()?),

            TokenType::Match => |parser, _token| Ok(parser.match_stmt()?),

            TokenType::While => |parser, _token| Ok(parser.while_stmt()?),

            TokenType::Loop => |parser, _token| Ok(parser.loop_stmt()?),

            TokenType::For => |parser, _token| Ok(parser.for_stmt()?),

            TokenType::Return => |parser, _token| {
                if parser.peek()?.ty() == TokenType::Newline {
                    parser.eat(TokenType::Newline, [])?;

                    Ok(Expr {
                        kind: ExprKind::Return(None),
                    })
                } else {
                    let expr = Ref::new(parser.expr()?);
                    parser.eat(TokenType::Newline, [])?;

                    Ok(Expr {
                        kind: ExprKind::Return(Some(expr)),
                    })
                }
            },

            TokenType::Break => |parser, _token| {
                if parser.peek()?.ty() == TokenType::Newline {
                    parser.eat(TokenType::Newline, [])?;

                    Ok(Expr {
                        kind: ExprKind::Break(None),
                    })
                } else {
                    let expr = Ref::new(parser.expr()?);
                    parser.eat(TokenType::Newline, [])?;

                    Ok(Expr {
                        kind: ExprKind::Break(Some(expr)),
                    })
                }
            },

            TokenType::Continue => |parser, _token| {
                parser.eat(TokenType::Newline, [])?;

                let stmt = Expr {
                    kind: ExprKind::Continue,
                };

                Ok(stmt)
            },

            // Variables
            TokenType::Ident => |parser, token| {
                use alloc::borrow::Cow;
                use unicode_normalization::{IsNormalized, UnicodeNormalization};

                let _frame = parser.add_stack_frame()?;

                // Performs zero temp allocations if it's already NFKC-normalised.
                let normalized = match unicode_normalization::is_nfkc_quick(token.source().chars())
                {
                    IsNormalized::Yes => Cow::Borrowed(token.source()),
                    _ => Cow::Owned(token.source().nfkc().collect()),
                };

                let ident = parser.context.strings.intern(&normalized);
                let kind = ExprKind::Variable(ident);

                Ok(Expr { kind })
            },

            // Literals
            TokenType::Int
            | TokenType::Bool
            | TokenType::Float
            | TokenType::String
            | TokenType::Rune => |parser, lit| {
                let _frame = parser.add_stack_frame()?;

                let literal = ExprKind::Literal(parser.literal(&lit, parser.current_file)?);

                Ok(Expr { kind: literal })
            },

            // Prefix Operators
            TokenType::Minus | TokenType::Bang | TokenType::Plus => |parser, token| {
                let _frame = parser.add_stack_frame()?;

                let operand = Ref::new(parser.expr()?);
                let kind =
                    ExprKind::UnaryOp(parser.unary_op(&token, parser.current_file)?, operand);

                Ok(Expr { kind })
            },

            // Grouping via parentheses
            TokenType::LeftParen => |parser, _paren| {
                let _frame = parser.add_stack_frame()?;

                let expr = Ref::new(parser.expr()?);
                parser.eat(TokenType::RightParen, [TokenType::Newline])?;
                let kind = ExprKind::Paren(expr);

                Ok(Expr { kind })
            },

            _ => return None,
        };

        Some(prefix)
    }

    fn expr_postfix(token: Token) -> Option<PostfixParselet<'src>> {
        let postfix: PostfixParselet = match token.ty() {
            // Function calls
            TokenType::LeftParen => |parser, _left_paren, caller| {
                let _frame = parser.add_stack_frame()?;

                let mut args = Vec::with_capacity(5);
                while parser.peek()?.ty() != TokenType::RightParen {
                    let arg = parser.expr()?;
                    args.push(arg);

                    if parser.peek()?.ty() == TokenType::Comma {
                        parser.eat(TokenType::Comma, [TokenType::Newline])?;
                    } else {
                        break;
                    }
                }

                parser.eat(TokenType::RightParen, [TokenType::Newline])?;
                let kind = ExprKind::FuncCall {
                    caller: Ref::new(caller),
                    args,
                };

                Ok(Expr { kind })
            },

            // Dotted function calls
            TokenType::Dot => |parser, _dot, member| {
                let _frame = parser.add_stack_frame()?;

                let func = Ref::new(parser.expr()?);
                let kind = ExprKind::MemberFuncCall {
                    member: Ref::new(member),
                    func,
                };

                Ok(Expr { kind })
            },

            // Ranges
            TokenType::DoubleDot => |parser, _, start| {
                let _frame = parser.add_stack_frame()?;

                let end = Ref::new(parser.expr()?);
                let kind = ExprKind::Range(Ref::new(start), end);

                Ok(Expr { kind })
            },

            // Array indexing
            TokenType::LeftBrace => Self::index_array,

            // Assignments
            TokenType::AddAssign
            | TokenType::SubAssign
            | TokenType::MultAssign
            | TokenType::DivAssign
            | TokenType::ModAssign
            | TokenType::PowAssign
            | TokenType::ShlAssign
            | TokenType::ShrAssign
            | TokenType::OrAssign
            | TokenType::AndAssign
            | TokenType::XorAssign => |parser, assign, left| {
                let _frame = parser.add_stack_frame()?;

                let assign = parser.assign_kind(&assign, parser.current_file)?;
                let rhs = Ref::new(parser.expr()?);
                let kind = ExprKind::Assign(Sided {
                    lhs: Ref::new(left),
                    op: assign,
                    rhs,
                });

                Ok(Expr { kind })
            },

            TokenType::Colon => |parser, _colon, left| {
                let _frame = parser.add_stack_frame()?;

                let equal = parser.eat(TokenType::Equal, [TokenType::Newline])?;
                let assign = parser.assign_kind(&equal, parser.current_file)?;
                let rhs = Ref::new(parser.expr()?);
                let kind = ExprKind::Assign(Sided {
                    lhs: Ref::new(left),
                    op: assign,
                    rhs,
                });

                Ok(Expr { kind })
            },

            _ => return None,
        };

        Some(postfix)
    }

    fn expr_infix(token: Token) -> Option<InfixParselet<'src>> {
        let infix: InfixParselet = match token.ty() {
            // Binary Operations
            TokenType::Plus
            | TokenType::Minus
            | TokenType::Star
            | TokenType::Divide
            | TokenType::Modulo
            | TokenType::DoubleStar
            | TokenType::Ampersand
            | TokenType::Pipe
            | TokenType::Caret
            | TokenType::Shl
            | TokenType::Shr => |parser, operand, left| {
                let _frame = parser.add_stack_frame()?;

                let rhs = Ref::new(parser.expr()?);
                let kind = ExprKind::BinaryOp(Sided {
                    lhs: Ref::new(left),
                    op: parser.bin_op(&operand, parser.current_file)?,
                    rhs,
                });

                Ok(Expr { kind })
            },

            // Comparisons
            TokenType::RightCaret
            | TokenType::LeftCaret
            | TokenType::GreaterThanEqual
            | TokenType::LessThanEqual
            | TokenType::IsEqual
            | TokenType::IsNotEqual => |parser, comparison, left| {
                let _frame = parser.add_stack_frame()?;

                let rhs = Ref::new(parser.expr()?);
                let kind = ExprKind::Comparison(Sided {
                    lhs: Ref::new(left),
                    op: parser.comp_op(&comparison, parser.current_file)?,
                    rhs,
                });

                Ok(Expr { kind })
            },

            // Array indexing
            TokenType::LeftBrace => Self::index_array,

            _ => return None,
        };

        Some(infix)
    }

    #[recursion_guard]
    fn index_array(&mut self, _left_bracket: Token<'src>, var: Expr) -> ParseResult<Expr> {
        let index = self.expr()?;
        self.eat(TokenType::RightBrace, [TokenType::Newline])?;

        let expr = Expr {
            kind: ExprKind::Index {
                var: Ref::new(var),
                index: Ref::new(index),
            },
        };

        Ok(expr)
    }

    #[recursion_guard]
    fn if_stmt(&mut self) -> ParseResult<Expr> {
        let cond = Ref::new(self.expr()?);
        self.eat(TokenType::Newline, [])?;

        let body = self.block(&[TokenType::End, TokenType::Else], 10)?;

        let mut clauses = Vec::new();
        let mut else_ = None;
        loop {
            let delimiter = self.eat_of([TokenType::Else, TokenType::End], [TokenType::Newline])?;

            match delimiter.ty() {
                TokenType::Else if self.peek()?.ty() == TokenType::If => {
                    self.eat(TokenType::If, [])?;
                    let cond = Ref::new(self.expr()?);
                    self.eat(TokenType::Newline, [])?;

                    let body = self.block(&[TokenType::End, TokenType::Else], 10)?;
                    self.eat(TokenType::End, [TokenType::Newline])?;

                    clauses.push(IfCond { cond, body });
                }

                TokenType::Else => {
                    self.eat(TokenType::Newline, [])?;
                    let body = self.block(&[TokenType::End], 10)?;

                    else_ = Some(body);
                    self.eat(TokenType::End, [TokenType::Newline])?;

                    break;
                }

                TokenType::End => break,

                _ => unreachable!(),
            }
        }

        let kind = ExprKind::If(If {
            cond: IfCond { cond, body },
            clauses,
            else_,
        });

        Ok(Expr { kind })
    }

    #[recursion_guard]
    fn match_stmt(&mut self) -> ParseResult<Expr> {
        let var = Ref::new(self.expr()?);
        self.eat(TokenType::Newline, [])?;

        let mut arms = Vec::with_capacity(3);
        while self.peek()?.ty() != TokenType::End {
            let bind = self.binding()?;

            let guard = if self.peek()?.ty() == TokenType::Where {
                self.eat(TokenType::Where, [TokenType::Newline])?;
                let expr = Ref::new(self.expr()?);

                Some(expr)
            } else {
                None
            };
            self.eat(TokenType::RightRocket, [TokenType::Newline])?;
            self.eat(TokenType::Newline, [])?;

            let body = self.block(&[TokenType::End], 5)?;

            self.eat(TokenType::End, [TokenType::Newline])?;
            self.eat(TokenType::Newline, [])?;

            arms.push(Arm { bind, guard, body });
        }

        self.eat(TokenType::End, [TokenType::Newline])?;

        let expr = Expr {
            kind: ExprKind::Match(Match { var, arms }),
        };

        Ok(expr)
    }

    #[recursion_guard]
    fn while_stmt(&mut self) -> ParseResult<Expr> {
        let cond = Ref::new(self.expr()?);
        self.eat(TokenType::Newline, [])?;

        let body = self.block(&[TokenType::End, TokenType::Then], 10)?;
        let then = self.then_stmt()?;
        let else_ = self.else_stmt()?;

        self.eat(TokenType::End, [TokenType::Newline])?;

        let expr = Expr {
            kind: ExprKind::While(While {
                cond,
                body,
                then,
                else_,
            }),
        };

        Ok(expr)
    }

    #[recursion_guard]
    fn loop_stmt(&mut self) -> ParseResult<Expr> {
        self.eat(TokenType::Newline, [])?;

        let body = self.block(&[TokenType::End, TokenType::Then], 10)?;
        let else_ = self.else_stmt()?;
        self.eat(TokenType::End, [TokenType::Newline])?;

        let expr = Expr {
            kind: ExprKind::Loop(Loop { body, else_ }),
        };

        Ok(expr)
    }

    #[recursion_guard]
    fn for_stmt(&mut self) -> ParseResult<Expr> {
        let var = Ref::new(self.expr()?);
        self.eat(TokenType::In, [TokenType::Newline])?;
        let cond = Ref::new(self.expr()?);
        self.eat(TokenType::Newline, [])?;

        let body = self.block(&[TokenType::End, TokenType::Then], 10)?;
        let then = self.then_stmt()?;
        let else_ = self.else_stmt()?;

        let expr = Expr {
            kind: ExprKind::For(For {
                var,
                cond,
                body,
                then,
                else_,
            }),
        };

        Ok(expr)
    }

    #[recursion_guard]
    fn then_stmt(&mut self) -> ParseResult<Option<Block>> {
        if self.peek()?.ty() == TokenType::Then {
            self.eat(TokenType::Then, [TokenType::Newline])?;
            self.eat(TokenType::Newline, [])?;

            let then = self.block(&[TokenType::End, TokenType::Else], 3)?;

            Ok(Some(then))
        } else {
            Ok(None)
        }
    }

    #[recursion_guard]
    fn else_stmt(&mut self) -> ParseResult<Option<Block>> {
        if self.peek()?.ty() == TokenType::Else {
            self.eat(TokenType::Else, [TokenType::Newline])?;
            self.eat(TokenType::Newline, [])?;

            let else_clause = self.block(&[TokenType::End], 3)?;

            Ok(Some(else_clause))
        } else {
            Ok(None)
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[rustfmt::skip]
pub enum ExprPrecedence {
    Mul, Div, Mod, Pow,
    Add, Sub,
    Shl, Shr,
    Less, Greater, LessEq, GreaterEq,
    Eq, Ne,
    BitAnd,
    BitXor,
    BitOr,
    LogAnd,
    LogOr,
    Ternary,
    Assignment,
}

impl ExprPrecedence {
    pub fn precedence(self) -> usize {
        match self {
            Self::Mul | Self::Div | Self::Mod | Self::Pow => 11,
            Self::Add | Self::Sub => 10,
            Self::Shl | Self::Shr => 9,
            Self::Less | Self::Greater | Self::LessEq | Self::GreaterEq => 8,
            Self::Eq | Self::Ne => 7,
            Self::BitAnd => 6,
            Self::BitXor => 5,
            Self::BitOr => 4,
            Self::LogAnd => 3,
            Self::LogOr => 2,
            Self::Ternary => 1,
            Self::Assignment => 0,
        }
    }
}

impl TryFrom<TokenType> for ExprPrecedence {
    type Error = ();

    fn try_from(t: TokenType) -> Result<ExprPrecedence, ()> {
        Ok(match t {
            TokenType::Star => Self::Mul,
            TokenType::Divide => Self::Div,
            TokenType::Modulo => Self::Mod,
            TokenType::DoubleStar => Self::Pow,
            TokenType::Plus => Self::Add,
            TokenType::Minus => Self::Sub,
            TokenType::Shl => Self::Shl,
            TokenType::Shr => Self::Shr,
            TokenType::LeftCaret => Self::Less,
            TokenType::RightCaret => Self::Greater,
            TokenType::LessThanEqual => Self::LessEq,
            TokenType::GreaterThanEqual => Self::GreaterEq,
            TokenType::IsEqual => Self::Eq,
            TokenType::IsNotEqual => Self::Ne,
            TokenType::Ampersand => Self::BitAnd,
            TokenType::Caret => Self::BitXor,
            TokenType::Pipe => Self::BitOr,
            TokenType::And => Self::LogAnd,
            TokenType::Or => Self::LogOr,
            TokenType::Colon
            | TokenType::AddAssign
            | TokenType::SubAssign
            | TokenType::MultAssign
            | TokenType::DivAssign
            | TokenType::ModAssign
            | TokenType::ShlAssign
            | TokenType::ShrAssign
            | TokenType::OrAssign
            | TokenType::AndAssign
            | TokenType::XorAssign => Self::Assignment,
            TokenType::If => Self::Ternary,

            _ => return Err(()),
        })
    }
}
