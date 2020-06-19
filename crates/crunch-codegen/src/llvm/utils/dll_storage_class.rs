use llvm_sys::LLVMDLLStorageClass;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum DLLStorageClass {
    Default,
    Import,
    Export,
}

#[rustfmt::skip]
impl From<LLVMDLLStorageClass> for DLLStorageClass {
    fn from(class: LLVMDLLStorageClass) -> Self {
        match class {
            LLVMDLLStorageClass::LLVMDefaultStorageClass   => Self::Default,
            LLVMDLLStorageClass::LLVMDLLImportStorageClass => Self::Import,
            LLVMDLLStorageClass::LLVMDLLExportStorageClass => Self::Export,
        }
    }
}

#[rustfmt::skip]
impl Into<LLVMDLLStorageClass> for DLLStorageClass {
    fn into(self) -> LLVMDLLStorageClass {
        match self {
            Self::Default => LLVMDLLStorageClass::LLVMDefaultStorageClass,
            Self::Import  => LLVMDLLStorageClass::LLVMDLLImportStorageClass,
            Self::Export  => LLVMDLLStorageClass::LLVMDLLExportStorageClass,
        }
    }
}
