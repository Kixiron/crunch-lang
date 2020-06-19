use llvm_sys::LLVMTypeKind;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
#[allow(non_camel_case_types)]
pub enum TypeKind {
    Void,
    Half,
    Float,
    Double,
    X86_FP80,
    FP128,
    PC_FP128,
    Label,
    Integer,
    Function,
    Struct,
    Array,
    Pointer,
    Vector,
    Metadata,
    X86_MMX,
    Token,
}

#[rustfmt::skip]
impl From<LLVMTypeKind> for TypeKind {
    fn from(kind: LLVMTypeKind) -> Self {
        match kind {
            LLVMTypeKind::LLVMVoidTypeKind      => Self::Void,
            LLVMTypeKind::LLVMHalfTypeKind      => Self::Half,
            LLVMTypeKind::LLVMFloatTypeKind     => Self::Float,
            LLVMTypeKind::LLVMDoubleTypeKind    => Self::Double,
            LLVMTypeKind::LLVMX86_FP80TypeKind  => Self::X86_FP80,
            LLVMTypeKind::LLVMFP128TypeKind     => Self::FP128,
            LLVMTypeKind::LLVMPPC_FP128TypeKind => Self::PC_FP128,
            LLVMTypeKind::LLVMLabelTypeKind     => Self::Label,
            LLVMTypeKind::LLVMIntegerTypeKind   => Self::Integer,
            LLVMTypeKind::LLVMFunctionTypeKind  => Self::Function,
            LLVMTypeKind::LLVMStructTypeKind    => Self::Struct,
            LLVMTypeKind::LLVMArrayTypeKind     => Self::Array,
            LLVMTypeKind::LLVMPointerTypeKind   => Self::Pointer,
            LLVMTypeKind::LLVMVectorTypeKind    => Self::Vector,
            LLVMTypeKind::LLVMMetadataTypeKind  => Self::Metadata,
            LLVMTypeKind::LLVMX86_MMXTypeKind   => Self::X86_MMX,
            LLVMTypeKind::LLVMTokenTypeKind     => Self::Token,
        }
    }
}
