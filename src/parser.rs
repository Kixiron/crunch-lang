use crate::{Instruction, NUMBER_REGISTERS, NUMBER_STRINGS};
use codespan::{FileId, Files};
use codespan_reporting::diagnostic::{Diagnostic, Label, Severity};
use logos::{Lexer, Logos};
use std::borrow::Cow;

pub fn fast_interpret<'a>(ast: Vec<Node<'a>>) -> Vec<Instruction> {
    let mut instructions = Vec::new();

    let mut regs: [Option<()>; NUMBER_REGISTERS] = [None; NUMBER_REGISTERS];
    let mut str_regs: [Option<()>; NUMBER_STRINGS] = [None; NUMBER_STRINGS];
    for node in ast {
        fast_interp_node(node, &mut instructions, &mut regs, &mut str_regs);
    }

    instructions
}

macro_rules! pos {
    ($var:expr) => {{
        let pos = $var.iter().position(|val| val.is_none()).unwrap();
        $var[pos] = Some(());
        (pos as u8).into()
    }};
}

fn fast_interp_node<'a>(
    node: Node<'a>,
    mut instructions: &mut Vec<Instruction>,
    mut regs: &mut [Option<()>; NUMBER_REGISTERS],
    mut str_regs: &mut [Option<()>; NUMBER_STRINGS],
) {
    match node {
        Node::FunctionDecl(decl) => {
            for node in decl.body {
                fast_interp_node(node, &mut instructions, &mut regs, &mut str_regs)
            }
        }
        Node::Binding(binding) => match binding.ty {
            Type::Bool => instructions.push(Instruction::LoadBool(
                if let Literal::Bool(b) = binding.val {
                    b
                } else {
                    panic!()
                },
                pos!(regs),
            )),
            Type::Int => instructions.push(Instruction::LoadInt(
                if let Literal::Int(i) = binding.val {
                    i
                } else {
                    panic!()
                },
                pos!(regs),
            )),
            Type::String => instructions.push(Instruction::LoadStr(
                if let Literal::String(s) = binding.val {
                    let s = Box::leak(Box::new(s.into_owned()));
                    unsafe { std::mem::transmute::<&str, &'static str>(&s[1..s.len() - 1]) }
                } else {
                    panic!()
                },
                pos!(str_regs),
                pos!(regs),
            )),
            Type::Infer => instructions.push(match binding.val {
                Literal::String(s) => Instruction::LoadStr(
                    {
                        let s = Box::leak(Box::new(s.into_owned()));
                        unsafe { std::mem::transmute::<&str, &'static str>(&s[1..s.len() - 1]) }
                    },
                    pos!(str_regs),
                    pos!(regs),
                ),
                Literal::Bool(b) => Instruction::LoadBool(b, pos!(regs)),
                Literal::Int(i) => Instruction::LoadInt(i, pos!(regs)),
                _ => unimplemented!(),
            }),

            _ => unimplemented!(),
        },
        _ => unimplemented!(),
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
struct Ident<'a>(Cow<'a, str>);

#[derive(Debug, PartialEq)]
pub enum Node<'a> {
    FunctionDecl(FunctionDecl<'a>),
    Binding(Binding<'a>),
    BinaryOp {
        op: BinOp,
        left: Box<Node<'a>>,
        right: Box<Node<'a>>,
    },
}

#[derive(Debug, PartialEq)]
pub struct Binding<'a> {
    name: Ident<'a>,
    ty: Type<'a>,
    val: Literal<'a>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
}

#[derive(Debug, PartialEq)]
pub struct FunctionDecl<'a> {
    name: Ident<'a>,
    parameters: Vec<ParameterDecl<'a>>,
    returns: Type<'a>,
    body: Vec<Node<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct ParameterDecl<'a> {
    name: Ident<'a>,
    ty: Type<'a>,
}

#[derive(Debug, PartialEq, Eq)]
enum Type<'a> {
    String,
    Bool,
    Int,
    Infer,
    Void,
    Custom(Cow<'a, str>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
enum Literal<'a> {
    String(Cow<'a, str>),
    Bool(bool),
    Int(i32),
    Variable(Ident<'a>),
}

type Result<T> = std::result::Result<T, Diagnostic>;

#[allow(missing_debug_implementations)]
pub struct Parser<'a> {
    token_stream: TokenStream<'a>,
    next: Option<Token<'a>>,
    peek: Option<Token<'a>>,
    codespan: Files,
    files: Vec<FileId>,
}

impl<'a> Parser<'a> {
    pub fn new(filename: Option<&'a str>, input: &'a str) -> Self {
        let mut token_stream = TokenStream::new(input);
        let peek = token_stream.next();
        let next = token_stream.next();
        let (codespan, files) = {
            let mut files = Files::new();
            let ids = vec![if let Some(filename) = filename {
                files.add(filename, input)
            } else {
                files.add("Crunch Source File", input)
            }];
            (files, ids)
        };

        Self {
            token_stream,
            next,
            peek,
            codespan,
            files,
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Node<'a>>> {
        println!("parsing");
        let mut ast = Vec::new();

        loop {
            if let Some(token) = &self.next {
                match token.ty {
                    TokenType::Function => ast.push(Node::FunctionDecl(self.parse_function()?)),
                    TokenType::End => break,
                    TokenType::Space | TokenType::Comment | TokenType::Newline => {
                        self.next(line!())?;
                        continue;
                    }
                    TokenType::Error => {
                        return Err(Diagnostic::new(
                            Severity::Error,
                            "Invalid token",
                            Label::new(
                                self.files[0],
                                token.range.0 as u32..token.range.1 as u32, // What?
                                format!("{:?} is not a valid token", token.source),
                            ),
                        ));
                    }
                    _ => {
                        return Err(Diagnostic::new(
                            Severity::Error,
                            "Invalid top-level token",
                            Label::new(
                                self.files[0],
                                token.range.0 as u32..token.range.1 as u32, // What?
                                format!("Expected Function, got {:?}", token.ty),
                            ),
                        ));
                    }
                }
            } else {
                break;
            }

            if let Err(_) = self.next(line!()) {
                return Ok(ast);
            }
        }

        Ok(ast)
    }

    fn parse_function(&mut self) -> Result<FunctionDecl<'a>> {
        let ident = self.eat(TokenType::Ident)?;
        self.eat(TokenType::LeftParen)?;

        let mut parameters = Vec::new();
        while self.peek()?.ty != TokenType::RightParen {
            parameters.push(self.parse_named_parameter()?);

            if self.peek()?.ty == TokenType::Comma {
                self.eat(TokenType::Comma)?;
            }
        }

        self.eat(TokenType::RightParen)?;

        self.eat_w()?;
        let returns = if self.next(line!())?.ty == TokenType::RightArrow {
            self.parse_type()?
        } else {
            Type::Infer
        };

        self.eat_of(&[TokenType::Newline, TokenType::Comment])?;

        let mut body = Vec::new();
        while let Ok(token) = self.next(line!()) {
            if token.ty == TokenType::Indent {
                body.push(self.parse_expr()?);
            } else {
                break;
            }
        }

        Ok(FunctionDecl {
            name: Ident(ident.source),
            parameters,
            returns,
            body,
        })
    }

    fn parse_expr(&mut self) -> Result<Node<'a>> {
        self.eat_w()?;
        match &self.next(line!())?.ty {
            &TokenType::Let => {
                let name = self.eat(TokenType::Ident)?;

                let ty = if self.peek().unwrap().ty == TokenType::Colon {
                    self.eat(TokenType::Colon)?;
                    self.parse_type()?
                } else {
                    Type::Infer
                };

                self.eat(TokenType::Equal)?;

                let val = self.parse_variable()?;

                self.eat(TokenType::Newline)?;

                Ok(Node::Binding(Binding {
                    name: Ident(name.source),
                    ty,
                    val,
                }))
            }
            _ => unimplemented!(),
        }
    }

    #[inline]
    fn parse_variable(&mut self) -> Result<Literal<'a>> {
        self.eat_w()?;
        let token = self.next(line!())?;
        Ok(match token.ty {
            TokenType::Int => Literal::Int(token.source.parse().unwrap()),
            TokenType::String => Literal::String(token.source),
            TokenType::Bool => Literal::Bool(token.source.parse().unwrap()),
            TokenType::Ident => Literal::Variable(Ident(token.source)),
            _ => {
                eprintln!("{:?}", token.ty);
                unimplemented!()
            }
        })
    }

    #[inline]
    fn parse_named_parameter(&mut self) -> Result<ParameterDecl<'a>> {
        let ident = self.eat(TokenType::Ident)?;
        self.eat(TokenType::Colon)?;
        let ty = self.parse_type()?;

        Ok(ParameterDecl {
            name: Ident(ident.source),
            ty,
        })
    }

    #[inline]
    fn parse_type(&mut self) -> Result<Type<'a>> {
        let ident = self.eat(TokenType::Ident)?;
        Ok(match ident.source {
            Cow::Borrowed("void") => Type::Void,
            Cow::Borrowed("str") => Type::String,
            Cow::Borrowed("int") => Type::Int,
            Cow::Borrowed("bool") => Type::Bool,
            custom => Type::Custom(custom),
        })
    }

    #[inline]
    fn next(&mut self, line: u32) -> Result<Token<'a>> {
        let mut next = self.token_stream.next();
        std::mem::swap(&mut next, &mut self.peek);
        self.next = next.clone();

        if let Some(next) = next {
            Ok(next)
        } else {
            eprintln!("Failed to get next token on line {}", line);
            Err(Diagnostic::new(
                Severity::Error,
                "Unexpected End Of File",
                Label::new(
                    self.files[0],
                    self.codespan.source_span(self.files[0]),
                    format!("Unexpected End Of File"),
                ),
            ))
        }
    }

    #[inline]
    fn peek(&mut self) -> Result<Token<'a>> {
        if let Some(next) = self.peek.clone() {
            Ok(next)
        } else {
            eprintln!("Failed to peek");
            Err(Diagnostic::new(
                Severity::Error,
                "Unexpected End Of File",
                Label::new(
                    self.files[0],
                    self.codespan.source_span(self.files[0]),
                    format!("Unexpected End Of File"),
                ),
            ))
        }
    }

    #[inline]
    fn eat_w(&mut self) -> Result<()> {
        while let Some(token) = &self.peek {
            if token.ty != TokenType::Space {
                break;
            } else {
                self.next(line!())?;
            }
        }
        Ok(())
    }

    #[inline]
    fn eat(&mut self, expected: TokenType) -> Result<Token<'a>> {
        self.eat_w()?;

        let token = self.next(line!())?;

        if token.ty == expected {
            Ok(token)
        } else {
            eprintln!(
                "Failed to eat token: Expected {:?}, got {:?}",
                expected, token.ty
            );
            Err(Diagnostic::new(
                Severity::Error,
                format!(
                    "Unexpected Token: Expected {:?}, got {:?}",
                    expected, token.ty
                ),
                Label::new(
                    self.files[0],
                    token.range.0 as u32..token.range.1 as u32,
                    format!("Expected {:?}", expected),
                ),
            ))
        }
    }

    #[inline]
    fn eat_of(&mut self, expected: &[TokenType]) -> Result<Token<'a>> {
        self.eat_w()?;

        let token = self.next(line!())?;

        if expected.contains(&token.ty) {
            Ok(token)
        } else {
            eprintln!(
                "Failed to eat from tokens: Expected {:?}, got {:?}",
                expected, token.ty
            );

            Err(Diagnostic::new(
                Severity::Error,
                format!(
                    "Unexpected Token: Expected one of {}, got {:?}",
                    expected
                        .iter()
                        .map(|t| format!("{:?}", t))
                        .collect::<Vec<String>>()
                        .join(", "),
                    token.ty
                ),
                Label::new(
                    self.files[0],
                    self.codespan.source_span(self.files[0]),
                    format!("Unexpected Token: {:?}", token.ty),
                ),
            ))
        }
    }
}

#[test]
fn parse_test() {
    const CODE: &str = "fn main(test: int, test2: str) -> void :: Main Function\n    \
                        let i = \'10\'\n    \
                        let i = 11\n\
                        \n\
                        fn main(test: int, test2: str) -> void\n    \
                        let i: int = 15\n    \
                        let i: int = 120\n";
    const FILENAME: &str = "parse_test.crunch";

    let mut parser = Parser::new(Some(FILENAME), CODE);

    let writer = codespan_reporting::term::termcolor::StandardStream::stderr(
        codespan_reporting::term::termcolor::ColorChoice::Auto,
    );
    let config = codespan_reporting::term::Config::default();
    match parser.parse() {
        Ok(ast) => {
            println!("{:#?}", ast);

            println!("{:?}", fast_interpret(ast));
        }
        Err(err) => {
            codespan_reporting::term::emit(
                &mut writer.lock(),
                &config,
                &{
                    let mut files = Files::new();
                    files.add(FILENAME, CODE);
                    files
                },
                &err,
            )
            .unwrap();
        }
    }
}

#[derive(Logos, Debug, PartialEq, Eq, Clone, Copy)]
pub enum TokenType {
    #[end]
    End,
    #[error]
    Error,
    #[token = ","]
    Comma,
    #[token = "let"]
    Let,
    #[token = "+"]
    Plus,
    #[token = ":"]
    Colon,
    #[token = "-"]
    Minus,
    #[token = "="]
    Equal,
    #[token = "=="]
    IsEqual,
    #[token = "["]
    LeftBrace,
    #[token = "]"]
    RightBrace,
    #[token = "/"]
    Divide,
    #[token = "*"]
    Star,
    #[token = "loop"]
    Loop,
    #[token = "while"]
    While,
    #[token = "if"]
    If,
    #[token = "else"]
    Else,
    #[token = "fn"]
    Function,
    #[token = "("]
    LeftParen,
    #[token = ")"]
    RightParen,
    #[token = "{"]
    LeftBracket,
    #[token = "}"]
    RightBracket,
    #[regex = "[^ \t\n\r\"\'!@#$%\\^&*()-+=,.<>/?;:\\[\\]{}\\\\|`~]+"]
    Ident,
    #[regex = "[1234567890]+"]
    Int,
    #[regex = "'[^']*'"]
    String,
    #[token = " "]
    Space,
    #[regex = "::.*\n"]
    Comment,
    #[token = "    "]
    Indent,
    #[token = "\n"]
    Newline,
    #[token = "->"]
    RightArrow,
    #[token = "<-"]
    LeftArrow,
    #[token = "true"]
    #[token = "false"]
    Bool,
}

#[allow(missing_debug_implementations)]
pub struct TokenStream<'a> {
    lexer: Lexer<TokenType, &'a str>,
    current: Token<'a>,
}

impl<'a> TokenStream<'a> {
    pub fn new(input: &'a str) -> Self {
        let lexer = TokenType::lexer(input);
        let current = Token::new(lexer.token, lexer.slice(), lexer.range());

        Self { current, lexer }
    }

    fn advance(&mut self) {
        self.current = Token::new(self.lexer.token, self.lexer.slice(), self.lexer.range());

        self.lexer.advance();
    }
}

impl<'a> Iterator for TokenStream<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.advance();

        match self.current.ty {
            TokenType::End => None,
            _ => Some(self.current.clone()),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Token<'a> {
    pub ty: TokenType,
    pub source: Cow<'a, str>,
    pub range: (usize, usize),
}

impl<'a> Token<'a> {
    pub fn new(ty: TokenType, source: &'a str, range: std::ops::Range<usize>) -> Self {
        Self {
            ty,
            source: Cow::Borrowed(source),
            range: (range.start, range.end),
        }
    }
}
