use llvm_sys::LLVMThreadLocalMode;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum ThreadLocalMode {
    GeneralDynamicTLS,
    LocalDynamicTLS,
    InitialExecTLS,
    LocalExecTLS,
}

impl ThreadLocalMode {
    #[rustfmt::skip]
    pub(crate) fn from_llvm_mode(mode: LLVMThreadLocalMode) -> Option<Self> {
        match mode {
            LLVMThreadLocalMode::LLVMNotThreadLocal         => None,
            LLVMThreadLocalMode::LLVMGeneralDynamicTLSModel => Some(Self::GeneralDynamicTLS),
            LLVMThreadLocalMode::LLVMLocalDynamicTLSModel   => Some(Self::LocalDynamicTLS),
            LLVMThreadLocalMode::LLVMInitialExecTLSModel    => Some(Self::InitialExecTLS),
            LLVMThreadLocalMode::LLVMLocalExecTLSModel      => Some(Self::LocalExecTLS),
        }
    }

    #[rustfmt::skip]
    pub(crate) fn into_llvm_mode(mode: Option<Self>) -> LLVMThreadLocalMode {
        match mode {
            None                          => LLVMThreadLocalMode::LLVMNotThreadLocal,
            Some(Self::GeneralDynamicTLS) => LLVMThreadLocalMode::LLVMGeneralDynamicTLSModel,
            Some(Self::LocalDynamicTLS)   => LLVMThreadLocalMode::LLVMLocalDynamicTLSModel,
            Some(Self::InitialExecTLS)    => LLVMThreadLocalMode::LLVMInitialExecTLSModel,
            Some(Self::LocalExecTLS)      => LLVMThreadLocalMode::LLVMLocalExecTLSModel,
        }
    }
}
