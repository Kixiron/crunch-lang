use crate::{
    parser::Parser,
    token::{Token, TokenType},
};
use alloc::{format, vec::Vec};
use core::convert::TryFrom;
use crunch_shared::{
    crunch_proc::recursion_guard,
    error::{Error, Locatable, Location, ParseResult, Span, SyntaxError},
    tracing,
    trees::{
        ast::{Arm, Block, Expr, ExprKind, For, If, IfCond, Loop, Match, While},
        Sided,
    },
};

type PrefixParselet<'src, 'ctx> =
    fn(&mut Parser<'src, 'ctx>, Token<'src>) -> ParseResult<&'ctx Expr<'ctx>>;
type PostfixParselet<'src, 'ctx> =
    fn(&mut Parser<'src, 'ctx>, Token<'src>, &'ctx Expr<'ctx>) -> ParseResult<&'ctx Expr<'ctx>>;
type InfixParselet<'src, 'ctx> =
    fn(&mut Parser<'src, 'ctx>, Token<'src>, &'ctx Expr<'ctx>) -> ParseResult<&'ctx Expr<'ctx>>;

/// Expr Parsing
impl<'src, 'ctx> Parser<'src, 'ctx> {
    #[crunch_shared::instrument(name = "expression", skip(self))]
    #[recursion_guard]
    pub fn expr(&mut self) -> ParseResult<&'ctx Expr<'ctx>> {
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
    fn parse_expr(&mut self, precedence: usize) -> ParseResult<&'ctx Expr<'ctx>> {
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
                Location::new(&token, self.current_file),
            ))
        }
    }

    fn expr_prefix(token: Token<'_>) -> Option<PrefixParselet<'src, 'ctx>> {
        #[rustfmt::skip]
        let prefix: PrefixParselet<'_, '_> = match token.ty() {
            TokenType::Ident if token.source() == "arr" => Self::array_or_tuple,
            TokenType::Ident if token.source() == "tup" => Self::array_or_tuple,
            TokenType::Ident     => Self::variable,
            TokenType::If        => Self::if_expr,
            TokenType::Match     => Self::match_expr,
            TokenType::While     => Self::while_expr,
            TokenType::Loop      => Self::loop_expr,
            TokenType::For       => Self::for_expr,
            TokenType::Return    => Self::return_expr,
            TokenType::Break     => Self::break_expr,
            TokenType::Continue  => Self::continue_expr,
            TokenType::LeftParen => Self::paren_expr,
            TokenType::Ampersand => Self::reference,
            TokenType::Minus
            | TokenType::Bang
            | TokenType::Plus    => Self::postfix_expr,
            TokenType::Int
            | TokenType::Bool
            | TokenType::Float
            | TokenType::String
            | TokenType::Rune    => Self::literal_expr,
            _                    => return None,
        };

        Some(prefix)
    }

    fn expr_postfix(token: Token<'_>) -> Option<PostfixParselet<'src, 'ctx>> {
        #[rustfmt::skip]
        let postfix: PostfixParselet<'_, '_> = match token.ty() {
            TokenType::LeftParen   => Self::function_call,
            TokenType::Dot         => Self::dotted_call,
            TokenType::DoubleDot   => Self::ranges,
            TokenType::LeftBrace   => Self::index_array,
            TokenType::As          => Self::as_cast,
            TokenType::Colon       => Self::assignment,
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
            | TokenType::XorAssign => Self::exotic_assignment,
            _                      => return None,
        };

        Some(postfix)
    }

    fn expr_infix(token: Token<'_>) -> Option<InfixParselet<'src, 'ctx>> {
        #[rustfmt::skip]
        let infix: InfixParselet<'_, '_> = match token.ty() {
            TokenType::LeftBrace    => Self::index_array,
            TokenType::As           => Self::as_cast,
            TokenType::RightCaret
            | TokenType::LeftCaret
            | TokenType::GreaterThanEqual
            | TokenType::LessThanEqual
            | TokenType::IsEqual
            | TokenType::IsNotEqual => Self::comparison,
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
            | TokenType::Shr        => Self::binary_operation,
            TokenType::Colon        => Self::assignment,
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
            | TokenType::XorAssign  => Self::exotic_assignment,
            _                       => return None,
        };

        Some(infix)
    }

    #[recursion_guard]
    fn comparison(
        &mut self,
        comparison: Token<'src>,
        lhs: &'ctx Expr<'ctx>,
    ) -> ParseResult<&'ctx Expr<'ctx>> {
        let rhs = self.expr()?;

        let loc = Location::new(Span::merge(lhs.span(), rhs.span()), self.current_file);
        let kind = ExprKind::Comparison(Sided {
            lhs,
            op: self.comp_op(&comparison, self.current_file)?,
            rhs,
        });

        Ok(self.context.ast_expr(Expr { kind, loc }))
    }

    #[recursion_guard]
    fn binary_operation(
        &mut self,
        operand: Token<'src>,
        lhs: &'ctx Expr<'ctx>,
    ) -> ParseResult<&'ctx Expr<'ctx>> {
        let rhs = self.expr()?;

        let loc = Location::new(Span::merge(lhs.span(), rhs.span()), self.current_file);
        let kind = ExprKind::BinaryOp(Sided {
            lhs,
            op: self.bin_op(&operand, self.current_file)?,
            rhs,
        });

        Ok(self.context.ast_expr(Expr { kind, loc }))
    }

    #[recursion_guard]
    fn assignment(
        &mut self,
        _colon: Token<'src>,
        lhs: &'ctx Expr<'ctx>,
    ) -> ParseResult<&'ctx Expr<'ctx>> {
        let equal = self.eat(TokenType::Equal, [TokenType::Newline])?;
        let assign = self.assign_kind(&equal, self.current_file)?;
        let rhs = self.expr()?;

        let loc = Location::new(Span::merge(lhs.span(), rhs.span()), self.current_file);
        let kind = ExprKind::Assign(Sided {
            lhs,
            op: assign,
            rhs,
        });

        Ok(self.context.ast_expr(Expr { kind, loc }))
    }

    #[recursion_guard]
    fn exotic_assignment(
        &mut self,
        assign: Token<'src>,
        lhs: &'ctx Expr<'ctx>,
    ) -> ParseResult<&'ctx Expr<'ctx>> {
        let assign = self.assign_kind(&assign, self.current_file)?;
        let rhs = self.expr()?;

        let loc = Location::new(Span::merge(lhs.span(), rhs.span()), self.current_file);
        let kind = ExprKind::Assign(Sided {
            lhs,
            op: assign,
            rhs,
        });

        Ok(self.context.ast_expr(Expr { kind, loc }))
    }

    #[recursion_guard]
    fn ranges(
        &mut self,
        _double_dot: Token<'src>,
        start: &'ctx Expr<'ctx>,
    ) -> ParseResult<&'ctx Expr<'ctx>> {
        let end = self.expr()?;

        let loc = Location::new(Span::merge(start.span(), end.span()), self.current_file);
        let kind = ExprKind::Range(start, end);

        Ok(self.context.ast_expr(Expr { kind, loc }))
    }

    #[recursion_guard]
    fn dotted_call(
        &mut self,
        _dot: Token<'src>,
        member: &'ctx Expr<'ctx>,
    ) -> ParseResult<&'ctx Expr<'ctx>> {
        let func = self.expr()?;

        let loc = Location::new(Span::merge(member.span(), func.span()), self.current_file);
        let kind = ExprKind::MemberFuncCall { member, func };

        Ok(self.context.ast_expr(Expr { kind, loc }))
    }

    #[recursion_guard]
    fn function_call(
        &mut self,
        _left_paren: Token<'src>,
        caller: &'ctx Expr<'ctx>,
    ) -> ParseResult<&'ctx Expr<'ctx>> {
        let mut args = Vec::with_capacity(5);
        while self.peek()?.ty() == TokenType::Newline {
            self.eat(TokenType::Newline, [])?;
        }

        while self.peek()?.ty() != TokenType::RightParen {
            let arg = self.expr()?;
            args.push(arg);

            if self.peek()?.ty() == TokenType::Comma {
                self.eat(TokenType::Comma, [TokenType::Newline])?;
            } else {
                break;
            }

            while self.peek()?.ty() == TokenType::Newline {
                self.eat(TokenType::Newline, [])?;
            }
        }

        let end = self
            .eat(TokenType::RightParen, [TokenType::Newline])?
            .span();

        let loc = Location::new(Span::merge(caller.span(), end), self.current_file);
        let kind = ExprKind::FuncCall { caller, args };

        Ok(self.context.ast_expr(Expr { kind, loc }))
    }

    #[recursion_guard]
    fn reference(&mut self, amp: Token<'src>) -> ParseResult<&'ctx Expr<'ctx>> {
        let mutable = if self.peek()?.ty() == TokenType::Mut {
            self.eat(TokenType::Mut, [])?;
            true
        } else {
            false
        };
        let expr = self.expr()?;
        let loc = Location::new(Span::merge(amp.span(), expr.span()), self.current_file);

        Ok(self.context.ast_expr(Expr {
            kind: ExprKind::Reference { mutable, expr },
            loc,
        }))
    }

    #[recursion_guard]
    fn paren_expr(&mut self, paren: Token<'src>) -> ParseResult<&'ctx Expr<'ctx>> {
        let expr = self.expr()?;
        let end = self
            .eat(TokenType::RightParen, [TokenType::Newline])?
            .span();

        let loc = Location::new(Span::merge(paren.span(), end), self.current_file);
        let kind = ExprKind::Paren(expr);

        Ok(self.context.ast_expr(Expr { kind, loc }))
    }

    #[recursion_guard]
    fn postfix_expr(&mut self, token: Token<'src>) -> ParseResult<&'ctx Expr<'ctx>> {
        let operand = self.expr()?;
        let loc = Location::new(Span::merge(token.span(), operand.span()), self.current_file);
        let kind = ExprKind::UnaryOp(self.unary_op(&token, self.current_file)?, operand);

        Ok(self.context.ast_expr(Expr { kind, loc }))
    }

    #[recursion_guard]
    fn literal_expr(&mut self, lit: Token<'src>) -> ParseResult<&'ctx Expr<'ctx>> {
        let literal = ExprKind::Literal(Locatable::new(
            self.literal(&lit, self.current_file)?,
            Location::new(lit.span(), self.current_file),
        ));

        Ok(self.context.ast_expr(Expr {
            kind: literal,
            loc: Location::new(lit.span(), self.current_file),
        }))
    }

    #[recursion_guard]
    fn variable(&mut self, ident_tok: Token<'src>) -> ParseResult<&'ctx Expr<'ctx>> {
        let ident = Locatable::new(
            self.intern_ident(ident_tok),
            Location::new(ident_tok.span(), self.current_file),
        );

        Ok(self.context.ast_expr(Expr {
            kind: ExprKind::Variable(ident),
            loc: Location::new(ident_tok.span(), self.current_file),
        }))
    }

    #[recursion_guard]
    fn continue_expr(&mut self, token: Token<'src>) -> ParseResult<&'ctx Expr<'ctx>> {
        self.eat(TokenType::Newline, [])?;

        let expr = Expr {
            kind: ExprKind::Continue,
            loc: Location::new(token.span(), self.current_file),
        };

        Ok(self.context.ast_expr(expr))
    }

    #[recursion_guard]
    fn break_expr(&mut self, token: Token<'src>) -> ParseResult<&'ctx Expr<'ctx>> {
        if self.peek()?.ty() == TokenType::Newline {
            Ok(self.context.ast_expr(Expr {
                kind: ExprKind::Break(None),
                loc: Location::new(token.span(), self.current_file),
            }))
        } else {
            let expr = self.expr()?;
            let loc = Location::new(Span::merge(token.span(), expr.span()), self.current_file);

            Ok(self.context.ast_expr(Expr {
                kind: ExprKind::Break(Some(expr)),
                loc,
            }))
        }
    }

    #[recursion_guard]
    fn return_expr(&mut self, token: Token<'src>) -> ParseResult<&'ctx Expr<'ctx>> {
        if self.peek()?.ty() == TokenType::Newline {
            Ok(self.context.ast_expr(Expr {
                kind: ExprKind::Return(None),
                loc: Location::new(token.span(), self.current_file),
            }))
        } else {
            let expr = self.expr()?;
            let loc = Location::new(Span::merge(token.span(), expr.span()), self.current_file);

            Ok(self.context.ast_expr(Expr {
                kind: ExprKind::Return(Some(expr)),
                loc,
            }))
        }
    }

    #[recursion_guard]
    fn array_or_tuple(&mut self, token: Token<'src>) -> ParseResult<&'ctx Expr<'ctx>> {
        self.eat(TokenType::LeftBrace, [TokenType::Newline])?;

        let mut elements = Vec::with_capacity(5);
        while self.peek()?.ty() != TokenType::RightBrace {
            let elm = self.expr()?;
            elements.push(elm);

            if self.peek()?.ty() == TokenType::Comma {
                self.eat(TokenType::Comma, [TokenType::Newline])?;
            } else {
                break;
            }
        }

        let end = self
            .eat(TokenType::RightBrace, [TokenType::Newline])?
            .span();

        let kind = if token.source() == "arr" {
            ExprKind::Array(elements)
        } else {
            ExprKind::Tuple(elements)
        };

        Ok(self.context.ast_expr(Expr {
            kind,
            loc: Location::new(Span::merge(token.span(), end), self.current_file),
        }))
    }

    #[recursion_guard]
    fn as_cast(
        &mut self,
        _as: Token<'src>,
        casted: &'ctx Expr<'ctx>,
    ) -> ParseResult<&'ctx Expr<'ctx>> {
        let ty = self.ascribed_type()?;

        let loc = Location::new(Span::merge(casted.span(), ty.span()), self.current_file);
        let kind = ExprKind::Cast { expr: casted, ty };

        Ok(self.context.ast_expr(Expr { kind, loc }))
    }

    #[recursion_guard]
    fn index_array(
        &mut self,
        _left_bracket: Token<'src>,
        var: &'ctx Expr<'ctx>,
    ) -> ParseResult<&'ctx Expr<'ctx>> {
        let index = self.expr()?;
        let end = self
            .eat(TokenType::RightBrace, [TokenType::Newline])?
            .span();

        let loc = Location::new(Span::merge(index.span(), end), self.current_file);
        let expr = Expr {
            kind: ExprKind::Index { var, index },
            loc,
        };

        Ok(self.context.ast_expr(expr))
    }

    #[recursion_guard]
    fn if_expr(&mut self, _token: Token<'src>) -> ParseResult<&'ctx Expr<'ctx>> {
        let cond = self.expr()?;
        self.eat(TokenType::Newline, [])?;

        let (body, mut delimiter) = self.block_returning(&[TokenType::End, TokenType::Else], 10)?;

        let mut clauses = Vec::new();
        let mut else_ = None;
        let end;
        loop {
            match delimiter.ty() {
                TokenType::Else if self.peek()?.ty() == TokenType::If => {
                    self.eat(TokenType::If, [])?;
                    let cond = self.expr()?;
                    self.eat(TokenType::Newline, [])?;

                    let (body, delim) =
                        self.block_returning(&[TokenType::End, TokenType::Else], 10)?;
                    delimiter = delim;

                    clauses.push(IfCond { cond, body });
                }

                TokenType::Else => {
                    let body = self.block(&[TokenType::End], 10)?;

                    end = body.location().span();
                    else_ = Some(body);

                    break;
                }

                TokenType::End => {
                    end = delimiter.span();
                    break;
                }

                _ => unreachable!(),
            }
        }

        let loc = Location::new(Span::merge(cond.span(), end), self.current_file);
        clauses.push(IfCond { cond, body });
        let kind = ExprKind::If(If { clauses, else_ });

        Ok(self.context.ast_expr(Expr { kind, loc }))
    }

    #[recursion_guard]
    fn match_expr(&mut self, _token: Token<'src>) -> ParseResult<&'ctx Expr<'ctx>> {
        let var = self.expr()?;
        self.eat(TokenType::Newline, [])?;

        let mut arms = Vec::with_capacity(3);
        while self.peek()?.ty() != TokenType::End {
            if self.peek()?.ty() == TokenType::Newline {
                self.eat(TokenType::Newline, [])?;
                continue;
            }

            let bind = self.binding()?;

            let guard = if self.peek()?.ty() == TokenType::Where {
                self.eat(TokenType::Where, [TokenType::Newline])?;
                let expr = self.expr()?;

                Some(expr)
            } else {
                None
            };
            self.eat(TokenType::RightRocket, [TokenType::Newline])?;
            // TODO: Make match arms an expression
            let body = self.block(&[TokenType::End], 5)?;

            arms.push(Arm { bind, guard, body });
        }
        let end = self.eat(TokenType::End, [TokenType::Newline])?.span();

        let loc = Location::new(Span::merge(var.span(), end), self.current_file);
        let expr = Expr {
            kind: ExprKind::Match(Match { var, arms }),
            loc,
        };

        Ok(self.context.ast_expr(expr))
    }

    #[recursion_guard]
    fn while_expr(&mut self, _token: Token<'src>) -> ParseResult<&'ctx Expr<'ctx>> {
        let cond = self.expr()?;
        self.eat(TokenType::Newline, [])?;

        let body = self.block(&[TokenType::End, TokenType::Then], 10)?;
        let then = self.then_block()?;
        let else_ = self.else_block()?;
        let end = else_
            .as_ref()
            .map(|e| e.location())
            .unwrap_or_else(|| {
                then.as_ref()
                    .map(|t| t.location())
                    .unwrap_or_else(|| body.location())
            })
            .span();

        let loc = Location::new(Span::merge(cond.span(), end), self.current_file);
        let expr = Expr {
            kind: ExprKind::While(While {
                cond,
                body,
                then,
                else_,
            }),
            loc,
        };

        Ok(self.context.ast_expr(expr))
    }

    #[recursion_guard]
    fn loop_expr(&mut self, _token: Token<'src>) -> ParseResult<&'ctx Expr<'ctx>> {
        let start = self.eat(TokenType::Newline, [])?.span();

        let body = self.block(&[TokenType::End, TokenType::Then], 10)?;
        let else_ = self.else_block()?;
        let end = else_
            .as_ref()
            .map(|e| e.location())
            .unwrap_or_else(|| body.location())
            .span();

        let expr = Expr {
            kind: ExprKind::Loop(Loop { body, else_ }),
            loc: Location::new(Span::merge(start, end), self.current_file),
        };

        Ok(self.context.ast_expr(expr))
    }

    #[recursion_guard]
    fn for_expr(&mut self, _token: Token<'src>) -> ParseResult<&'ctx Expr<'ctx>> {
        let var = self.expr()?;
        self.eat(TokenType::In, [TokenType::Newline])?;
        let cond = self.expr()?;
        self.eat(TokenType::Newline, [])?;

        let body = self.block(&[TokenType::End, TokenType::Then], 10)?;
        let then = self.then_block()?;
        let else_ = self.else_block()?;
        let end = else_
            .as_ref()
            .map(|e| e.location())
            .unwrap_or_else(|| {
                then.as_ref()
                    .map(|t| t.location())
                    .unwrap_or_else(|| body.location())
            })
            .span();

        let loc = Location::new(Span::merge(var.span(), end), self.current_file);
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

        Ok(self.context.ast_expr(expr))
    }

    #[recursion_guard]
    fn then_block(&mut self) -> ParseResult<Option<Block<'ctx>>> {
        if self.peek()?.ty() == TokenType::Then {
            let then = self.block(&[TokenType::End, TokenType::Else], 3)?;

            Ok(Some(then))
        } else {
            Ok(None)
        }
    }

    #[recursion_guard]
    fn else_block(&mut self) -> ParseResult<Option<Block<'ctx>>> {
        if self.peek()?.ty() == TokenType::Else {
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
    As,
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
    #[rustfmt::skip]
    pub fn precedence(self) -> usize {
        match self {
            Self::As              => 13,
            Self::Mul
            | Self::Div
            | Self::Mod
            | Self::Pow           => 12,
            Self::Add | Self::Sub => 11,
            Self::Shl | Self::Shr => 10,
            Self::Less
            | Self::Greater
            | Self::LessEq
            | Self::GreaterEq     => 9,
            Self::Eq | Self::Ne   => 8,
            Self::BitAnd          => 7,
            Self::BitXor          => 6,
            Self::BitOr           => 5,
            Self::LogAnd          => 4,
            Self::LogOr           => 3,
            Self::Ternary         => 2,
            Self::Assignment      => 1,
        }
    }
}

impl TryFrom<TokenType> for ExprPrecedence {
    type Error = ();

    fn try_from(t: TokenType) -> Result<ExprPrecedence, ()> {
        #[rustfmt::skip]
        let precedence = match t {
            TokenType::As               => Self::As,
            TokenType::Star             => Self::Mul,
            TokenType::Divide           => Self::Div,
            TokenType::Modulo           => Self::Mod,
            TokenType::DoubleStar       => Self::Pow,
            TokenType::Plus             => Self::Add,
            TokenType::Minus            => Self::Sub,
            TokenType::Shl              => Self::Shl,
            TokenType::Shr              => Self::Shr,
            TokenType::LeftCaret        => Self::Less,
            TokenType::RightCaret       => Self::Greater,
            TokenType::LessThanEqual    => Self::LessEq,
            TokenType::GreaterThanEqual => Self::GreaterEq,
            TokenType::IsEqual          => Self::Eq,
            TokenType::IsNotEqual       => Self::Ne,
            TokenType::Ampersand        => Self::BitAnd,
            TokenType::Caret            => Self::BitXor,
            TokenType::Pipe             => Self::BitOr,
            TokenType::And              => Self::LogAnd,
            TokenType::Or               => Self::LogOr,
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
            | TokenType::XorAssign      => Self::Assignment,
            TokenType::If               => Self::Ternary,
            _                           => return Err(()),
        };

        Ok(precedence)
    }
}
