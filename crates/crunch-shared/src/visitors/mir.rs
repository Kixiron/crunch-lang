use crate::trees::mir::{Block, Constant, Function, Instruction, RightValue, Type};

pub trait MirVisitor {
    type FunctionOutput;
    type BlockOutput;
    type InstructionOutput;
    type RvalOutput;
    type ConstantOutput;
    type TypeOutput;

    fn visit_function(&mut self, func: &Function) -> Self::FunctionOutput;
    fn visit_block(&mut self, block: &Block) -> Self::BlockOutput;
    fn visit_instruction(&mut self, instruction: &Instruction) -> Self::InstructionOutput;
    fn visit_rval(&mut self, rval: &RightValue) -> Self::RvalOutput;
    fn visit_constant(&mut self, constant: &Constant) -> Self::ConstantOutput;
    fn visit_type(&mut self, ty: &Type) -> Self::TypeOutput;
}
