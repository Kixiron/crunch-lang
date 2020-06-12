use llvm_sys::LLVMValueKind;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum ValueKind {
    Argument,
    BasicBlock,
    MemoryUse,
    MemoryDef,
    MemoryPhi,
    Function,
    GlobalAlias,
    GlobalIFunc,
    GlobalVariable,
    BlockAddress,
    ConstExpr,
    ConstArray,
    ConstStruct,
    ConstVector,
    ConstInt,
    ConstFloat,
    ConstAggregateZero,
    ConstDataArray,
    ConstDataVector,
    ConstPointerNull,
    ConstTokenNone,
    Undef,
    MetadataAsValue,
    InlineAsm,
    Instruction,
}

#[rustfmt::skip]
impl From<LLVMValueKind> for ValueKind {
    fn from(value: LLVMValueKind) -> Self {
        match value {
            LLVMValueKind::LLVMArgumentValueKind              => Self::Argument,
            LLVMValueKind::LLVMBasicBlockValueKind            => Self::BasicBlock,
            LLVMValueKind::LLVMMemoryUseValueKind             => Self::MemoryUse,
            LLVMValueKind::LLVMMemoryDefValueKind             => Self::MemoryDef,
            LLVMValueKind::LLVMMemoryPhiValueKind             => Self::MemoryPhi,
            LLVMValueKind::LLVMFunctionValueKind              => Self::Function,
            LLVMValueKind::LLVMGlobalAliasValueKind           => Self::GlobalAlias,
            LLVMValueKind::LLVMGlobalIFuncValueKind           => Self::GlobalIFunc,
            LLVMValueKind::LLVMGlobalVariableValueKind        => Self::GlobalVariable,
            LLVMValueKind::LLVMBlockAddressValueKind          => Self::BlockAddress,
            LLVMValueKind::LLVMConstantExprValueKind          => Self::ConstExpr,
            LLVMValueKind::LLVMConstantArrayValueKind         => Self::ConstArray,
            LLVMValueKind::LLVMConstantStructValueKind        => Self::ConstStruct,
            LLVMValueKind::LLVMConstantVectorValueKind        => Self::ConstVector,
            LLVMValueKind::LLVMConstantAggregateZeroValueKind => Self::ConstAggregateZero,
            LLVMValueKind::LLVMConstantDataArrayValueKind     => Self::ConstDataArray,
            LLVMValueKind::LLVMConstantDataVectorValueKind    => Self::ConstDataVector,
            LLVMValueKind::LLVMConstantIntValueKind           => Self::ConstInt,
            LLVMValueKind::LLVMConstantFPValueKind            => Self::ConstFloat,
            LLVMValueKind::LLVMConstantPointerNullValueKind   => Self::ConstPointerNull,
            LLVMValueKind::LLVMConstantTokenNoneValueKind     => Self::ConstTokenNone,
            LLVMValueKind::LLVMUndefValueValueKind            => Self::Undef,
            LLVMValueKind::LLVMMetadataAsValueValueKind       => Self::MetadataAsValue,
            LLVMValueKind::LLVMInlineAsmValueKind             => Self::InlineAsm,
            LLVMValueKind::LLVMInstructionValueKind           => Self::Instruction,
        }
    }
}

#[rustfmt::skip]
impl Into<LLVMValueKind> for ValueKind {
    fn into(self) -> LLVMValueKind {
        match self {
            Self::Argument           => LLVMValueKind::LLVMArgumentValueKind,
            Self::BasicBlock         => LLVMValueKind::LLVMBasicBlockValueKind,
            Self::MemoryUse          => LLVMValueKind::LLVMMemoryUseValueKind,
            Self::MemoryDef          => LLVMValueKind::LLVMMemoryDefValueKind,
            Self::MemoryPhi          => LLVMValueKind::LLVMMemoryPhiValueKind,
            Self::Function           => LLVMValueKind::LLVMFunctionValueKind,
            Self::GlobalAlias        => LLVMValueKind::LLVMGlobalAliasValueKind,
            Self::GlobalIFunc        => LLVMValueKind::LLVMGlobalIFuncValueKind,
            Self::GlobalVariable     => LLVMValueKind::LLVMGlobalVariableValueKind,
            Self::BlockAddress       => LLVMValueKind::LLVMBlockAddressValueKind,
            Self::ConstExpr          => LLVMValueKind::LLVMConstantExprValueKind,
            Self::ConstArray         => LLVMValueKind::LLVMConstantArrayValueKind,
            Self::ConstStruct        => LLVMValueKind::LLVMConstantStructValueKind,
            Self::ConstVector        => LLVMValueKind::LLVMConstantVectorValueKind,
            Self::ConstAggregateZero => LLVMValueKind::LLVMConstantAggregateZeroValueKind,
            Self::ConstDataArray     => LLVMValueKind::LLVMConstantDataArrayValueKind,
            Self::ConstDataVector    => LLVMValueKind::LLVMConstantDataVectorValueKind,
            Self::ConstInt           => LLVMValueKind::LLVMConstantIntValueKind,
            Self::ConstFloat         => LLVMValueKind::LLVMConstantFPValueKind,
            Self::ConstPointerNull   => LLVMValueKind::LLVMConstantPointerNullValueKind,
            Self::ConstTokenNone     => LLVMValueKind::LLVMConstantTokenNoneValueKind,
            Self::Undef              => LLVMValueKind::LLVMUndefValueValueKind,
            Self::MetadataAsValue    => LLVMValueKind::LLVMMetadataAsValueValueKind,
            Self::InlineAsm          => LLVMValueKind::LLVMInlineAsmValueKind,
            Self::Instruction        => LLVMValueKind::LLVMInstructionValueKind,
        }
    }
}
