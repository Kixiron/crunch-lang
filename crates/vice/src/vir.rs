//! The Vice Intermediate Representation

// TODO: Finish Stadium and use it here

use crunch_parser::{ast, hir::*, string_interner::Sym};

pub fn compile(hir: Hir, mut next_id: usize) -> Vec<Block> {
    let mut next_block = 0;

    let mut blocks = Vec::with_capacity(50);
    let mut current = Block::next(&mut next_block);

    compile_hir(
        &mut blocks,
        hir,
        &mut next_block,
        None,
        None,
        &mut current,
        &mut next_id,
    );
    blocks.push(current);

    blocks
}

fn compile_hir(
    blocks: &mut Vec<Block>,
    hir: Hir,
    next_block: &mut usize,
    continue_block: Option<usize>,
    break_block: Option<usize>,
    current_block: &mut Block,
    next_id: &mut usize,
) {
    match hir {
        Hir::Loop { body } => {
            let (mut start_at, mut continue_to, mut break_to) = (
                Block::next(next_block),
                Block::next(next_block),
                Block::next(next_block),
            );
            let start_id = start_at.id();
            core::mem::swap(current_block, &mut start_at);
            blocks.push(start_at);

            for b in body {
                compile_hir(
                    blocks,
                    b,
                    next_block,
                    Some(continue_to.id()),
                    Some(break_to.id()),
                    current_block,
                    next_id,
                );
            }

            *continue_to.next_mut() = Some(Continuation::Branch(start_id, break_to.id()));

            blocks.push(continue_to);
            core::mem::swap(&mut break_to, current_block);
            blocks.push(break_to);
        }

        Hir::Assign { target, value } => {
            current_block.push(Instruction {
                target,
                left: match value {
                    HirValue::Function(f) => Side::FunctionCall(f),
                    HirValue::Var(var) => Side::Variable(var),
                    HirValue::Literal(lit) => Side::Immediate(lit),
                },
                right: None,
                operation: None,
            });
        }

        Hir::BinOp {
            target,
            left,
            right,
            operand,
        } => current_block.push(Instruction {
            target,
            left: Side::from(left),
            right: Some(Side::from(right)),
            operation: Some(Operation::from(operand)),
        }),

        Hir::Conditional {
            conditions,
            else_body,
        } => {}

        Hir::Break { value } => {
            if let Some(val) = value {
                current_block.push(Instruction {
                    target: {
                        *next_id += 1;
                        *next_id - 1
                    },
                    left: Side::from(val),
                    right: None,
                    operation: Some(Operation::Push),
                })
            }
            *current_block.next_mut() = Some(Continuation::Block(break_block.unwrap()));

            let mut temp = Block::next(next_block);
            core::mem::swap(&mut temp, current_block);

            blocks.push(temp);
        }
        Hir::Return { value } => {
            *current_block.next_mut() = Some(Continuation::Return);

            let mut temp = Block::next(next_block);
            core::mem::swap(&mut temp, current_block);

            blocks.push(temp);
        }
        Hir::Continue => {
            *current_block.next_mut() = Some(Continuation::Block(continue_block.unwrap()));

            let mut temp = Block::next(next_block);
            core::mem::swap(&mut temp, current_block);

            blocks.push(temp);
        }

        _ => todo!(),
    }
}

#[derive(Debug, Clone)]
pub struct Block {
    id: usize,
    ops: Vec<Instruction>,
    next: Option<Continuation>,
    next_var: usize,
}

impl Block {
    pub fn next(id: &mut usize) -> Self {
        *id += 1;

        Self {
            id: *id - 1,
            ops: Vec::with_capacity(10),
            next: None,
            next_var: 0,
        }
    }

    pub const fn id(&self) -> usize {
        self.id
    }

    pub fn next_mut(&mut self) -> &mut Option<Continuation> {
        &mut self.next
    }

    pub fn next_var(&mut self) -> usize {
        self.next_var += 1;
        self.next_var - 1
    }

    pub fn push(&mut self, inst: Instruction) {
        self.ops.push(inst);
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum Operation {
    Push,
    Plus,
    Minus,
    Mult,
    Div,
    Xor,
    Or,
    And,
}

impl From<ast::BinaryOp> for Operation {
    fn from(op: ast::BinaryOp) -> Self {
        match op {
            ast::BinaryOp::Plus => Self::Plus,
            ast::BinaryOp::Minus => Self::Minus,
            ast::BinaryOp::Mult => Self::Mult,
            ast::BinaryOp::Div => Self::Div,
            ast::BinaryOp::Xor => Self::Xor,
            ast::BinaryOp::Or => Self::Or,
            ast::BinaryOp::And => Self::And,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Instruction {
    target: usize,
    left: Side,
    right: Option<Side>,
    operation: Option<Operation>,
}

impl Instruction {
    pub fn pop(target: usize) -> Self {
        Self {
            target,
            left: Side::Pop,
            right: None,
            operation: None,
        }
    }

    pub fn load_immediate(target: usize, immediate: HirLiteral) -> Self {
        Self {
            target,
            left: Side::Immediate(immediate),
            right: None,
            operation: None,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Side {
    Immediate(HirLiteral),
    Variable(usize),
    FunctionCall(Sym),
    Pop,
}

impl From<HirValue> for Side {
    fn from(lit: HirValue) -> Self {
        match lit {
            HirValue::Function(f) => Side::FunctionCall(f),
            HirValue::Var(var) => Side::Variable(var),
            HirValue::Literal(lit) => Side::Immediate(lit),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Continuation {
    Return,
    Halt,
    Block(usize),
    Branch(usize, usize),
    TailCall,
}
