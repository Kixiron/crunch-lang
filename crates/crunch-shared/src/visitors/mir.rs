use crate::trees::mir::{BasicBlock, Constant, Function, Instruction, Rval, Terminator, Type};

pub trait MirVisitor {
    type FunctionOutput;
    type BlockOutput;
    type TerminatorOutput;
    type InstructionOutput;
    type RvalOutput;
    type ConstantOutput;
    type TypeOutput;

    fn visit_function(&mut self, func: &Function) -> Self::FunctionOutput;
    fn visit_block(&mut self, block: &BasicBlock) -> Self::BlockOutput;
    fn visit_terminator(&mut self, terminator: &Terminator) -> Self::TerminatorOutput;
    fn visit_instruction(&mut self, instruction: &Instruction) -> Self::InstructionOutput;
    fn visit_rval(&mut self, rval: &Rval) -> Self::RvalOutput;
    fn visit_constant(&mut self, constant: &Constant, ty: &Type) -> Self::ConstantOutput;
    fn visit_type(&mut self, ty: &Type) -> Self::TypeOutput;
}
