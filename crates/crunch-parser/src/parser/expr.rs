use crate::{
    parser::Parser,
    token::{Token, TokenType},
};
use alloc::{format, vec::Vec};
use core::convert::TryFrom;
use crunch_proc::recursion_guard;
use crunch_shared::{
    error::{Error, Locatable, Location, ParseResult, Span, SyntaxError},
    trees::{
        ast::{Arm, Block, Expr, ExprKind, For, If, IfCond, Loop, Match, While},
        Ref, Sided,
    },
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

                    let end = parser
                        .eat(TokenType::RightBrace, [TokenType::Newline])?
                        .span();

                    let kind = if token.source() == "arr" {
                        ExprKind::Array(elements)
                    } else {
                        ExprKind::Tuple(elements)
                    };

                    Ok(Expr {
                        kind,
                        loc: Location::concrete(
                            Span::merge(token.span(), end),
                            parser.current_file,
                        ),
                    })
                }
            }

            TokenType::If => |parser, _token| Ok(parser.if_stmt()?),

            TokenType::Match => |parser, _token| Ok(parser.match_stmt()?),

            TokenType::While => |parser, _token| Ok(parser.while_stmt()?),

            TokenType::Loop => |parser, _token| Ok(parser.loop_stmt()?),

            TokenType::For => |parser, _token| Ok(parser.for_stmt()?),

            TokenType::Return => |parser, token| {
                if parser.peek()?.ty() == TokenType::Newline {
                    let end = parser.eat(TokenType::Newline, [])?.span();

                    Ok(Expr {
                        kind: ExprKind::Return(None),
                        loc: Location::concrete(
                            Span::merge(token.span(), end),
                            parser.current_file,
                        ),
                    })
                } else {
                    let expr = Ref::new(parser.expr()?);
                    let end = parser.eat(TokenType::Newline, [])?.span();

                    Ok(Expr {
                        kind: ExprKind::Return(Some(expr)),
                        loc: Location::concrete(
                            Span::merge(token.span(), end),
                            parser.current_file,
                        ),
                    })
                }
            },

            TokenType::Break => |parser, token| {
                if parser.peek()?.ty() == TokenType::Newline {
                    let end = parser.eat(TokenType::Newline, [])?.span();

                    Ok(Expr {
                        kind: ExprKind::Break(None),
                        loc: Location::concrete(
                            Span::merge(token.span(), end),
                            parser.current_file,
                        ),
                    })
                } else {
                    let expr = Ref::new(parser.expr()?);
                    let loc = Location::concrete(
                        Span::merge(token.span(), expr.span()),
                        parser.current_file,
                    );

                    Ok(Expr {
                        kind: ExprKind::Break(Some(expr)),
                        loc,
                    })
                }
            },

            TokenType::Continue => |parser, token| {
                parser.eat(TokenType::Newline, [])?;

                let stmt = Expr {
                    kind: ExprKind::Continue,
                    loc: Location::concrete(token.span(), parser.current_file),
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

                Ok(Expr {
                    kind,
                    loc: Location::concrete(token.span(), parser.current_file),
                })
            },

            // Literals
            TokenType::Int
            | TokenType::Bool
            | TokenType::Float
            | TokenType::String
            | TokenType::Rune => |parser, lit| {
                let _frame = parser.add_stack_frame()?;

                let literal = ExprKind::Literal(parser.literal(&lit, parser.current_file)?);

                Ok(Expr {
                    kind: literal,
                    loc: Location::concrete(lit.span(), parser.current_file),
                })
            },

            // Prefix Operators
            TokenType::Minus | TokenType::Bang | TokenType::Plus => |parser, token| {
                let _frame = parser.add_stack_frame()?;

                let operand = Ref::new(parser.expr()?);
                let loc = Location::concrete(
                    Span::merge(token.span(), operand.span()),
                    parser.current_file,
                );
                let kind =
                    ExprKind::UnaryOp(parser.unary_op(&token, parser.current_file)?, operand);

                Ok(Expr { kind, loc })
            },

            // Grouping via parentheses
            TokenType::LeftParen => |parser, paren| {
                let _frame = parser.add_stack_frame()?;

                let expr = Ref::new(parser.expr()?);
                let end = parser
                    .eat(TokenType::RightParen, [TokenType::Newline])?
                    .span();

                let loc = Location::concrete(Span::merge(paren.span(), end), parser.current_file);
                let kind = ExprKind::Paren(expr);

                Ok(Expr { kind, loc })
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

                let end = parser
                    .eat(TokenType::RightParen, [TokenType::Newline])?
                    .span();

                let loc = Location::concrete(Span::merge(caller.span(), end), parser.current_file);
                let kind = ExprKind::FuncCall {
                    caller: Ref::new(caller),
                    args,
                };

                Ok(Expr { kind, loc })
            },

            // Dotted function calls
            TokenType::Dot => |parser, _dot, member| {
                let _frame = parser.add_stack_frame()?;

                let func = Ref::new(parser.expr()?);

                let loc = Location::concrete(
                    Span::merge(member.span(), func.span()),
                    parser.current_file,
                );
                let kind = ExprKind::MemberFuncCall {
                    member: Ref::new(member),
                    func,
                };

                Ok(Expr { kind, loc })
            },

            // Ranges
            TokenType::DoubleDot => |parser, _, start| {
                let _frame = parser.add_stack_frame()?;

                let end = Ref::new(parser.expr()?);

                let loc =
                    Location::concrete(Span::merge(start.span(), end.span()), parser.current_file);
                let kind = ExprKind::Range(Ref::new(start), end);

                Ok(Expr { kind, loc })
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

                let loc =
                    Location::concrete(Span::merge(left.span(), rhs.span()), parser.current_file);
                let kind = ExprKind::Assign(Sided {
                    lhs: Ref::new(left),
                    op: assign,
                    rhs,
                });

                Ok(Expr { kind, loc })
            },

            TokenType::Colon => |parser, _colon, left| {
                let _frame = parser.add_stack_frame()?;

                let equal = parser.eat(TokenType::Equal, [TokenType::Newline])?;
                let assign = parser.assign_kind(&equal, parser.current_file)?;
                let rhs = Ref::new(parser.expr()?);

                let loc =
                    Location::concrete(Span::merge(left.span(), rhs.span()), parser.current_file);
                let kind = ExprKind::Assign(Sided {
                    lhs: Ref::new(left),
                    op: assign,
                    rhs,
                });

                Ok(Expr { kind, loc })
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

                let loc =
                    Location::concrete(Span::merge(left.span(), rhs.span()), parser.current_file);
                let kind = ExprKind::BinaryOp(Sided {
                    lhs: Ref::new(left),
                    op: parser.bin_op(&operand, parser.current_file)?,
                    rhs,
                });

                Ok(Expr { kind, loc })
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

                let loc =
                    Location::concrete(Span::merge(left.span(), rhs.span()), parser.current_file);
                let kind = ExprKind::Comparison(Sided {
                    lhs: Ref::new(left),
                    op: parser.comp_op(&comparison, parser.current_file)?,
                    rhs,
                });

                Ok(Expr { kind, loc })
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
        let end = self
            .eat(TokenType::RightBrace, [TokenType::Newline])?
            .span();

        let loc = Location::concrete(Span::merge(index.span(), end), self.current_file);
        let expr = Expr {
            kind: ExprKind::Index {
                var: Ref::new(var),
                index: Ref::new(index),
            },
            loc,
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
        let end;
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
                    end = self.eat(TokenType::End, [TokenType::Newline])?.span();

                    break;
                }

                TokenType::End => {
                    end = delimiter.span();
                    break;
                }

                _ => unreachable!(),
            }
        }

        let loc = Location::concrete(Span::merge(cond.span(), end), self.current_file);
        clauses.push(IfCond { cond, body });
        let kind = ExprKind::If(If { clauses, else_ });

        Ok(Expr { kind, loc })
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

        let end = self.eat(TokenType::End, [TokenType::Newline])?.span();

        let loc = Location::concrete(Span::merge(var.span(), end), self.current_file);
        let expr = Expr {
            kind: ExprKind::Match(Match { var, arms }),
            loc,
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

        let end = self.eat(TokenType::End, [TokenType::Newline])?.span();

        let loc = Location::concrete(Span::merge(cond.span(), end), self.current_file);
        let expr = Expr {
            kind: ExprKind::While(While {
                cond,
                body,
                then,
                else_,
            }),
            loc,
        };

        Ok(expr)
    }

    #[recursion_guard]
    fn loop_stmt(&mut self) -> ParseResult<Expr> {
        let start = self.eat(TokenType::Newline, [])?.span();

        let body = self.block(&[TokenType::End, TokenType::Then], 10)?;
        let else_ = self.else_stmt()?;
        let end = self.eat(TokenType::End, [TokenType::Newline])?.span();

        let expr = Expr {
            kind: ExprKind::Loop(Loop { body, else_ }),
            loc: Location::concrete(Span::merge(start, end), self.current_file),
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
        let end = self.eat(TokenType::End, [TokenType::Newline])?.span();

        let loc = Location::concrete(Span::merge(var.span(), end), self.current_file);
        let expr = Expr {
            kind: ExprKind::For(For {
                var,
                cond,
                body,
                then,
                else_,
            }),
            loc,
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
