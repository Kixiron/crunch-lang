use llvm_sys::LLVMLinkage;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum Linkage {
    External,
    AvailableExternally,
    LinkOnceAny,
    LinkOnceODR,
    LinkOnceODRAutoHide,
    WeakAny,
    WeakODR,
    Appending,
    Internal,
    Private,
    DLLImport,
    DLLExport,
    ExternalWeak,
    Ghost,
    Common,
    LinkerPrivate,
    LinkerPrivateWeak,
}

#[rustfmt::skip]
impl From<LLVMLinkage> for Linkage {
    fn from(linkage: LLVMLinkage) -> Self {
        match linkage {
            LLVMLinkage::LLVMExternalLinkage            => Self::External,
            LLVMLinkage::LLVMAvailableExternallyLinkage => Self::AvailableExternally,
            LLVMLinkage::LLVMLinkOnceAnyLinkage         => Self::LinkOnceAny,
            LLVMLinkage::LLVMLinkOnceODRLinkage         => Self::LinkOnceODR,
            LLVMLinkage::LLVMLinkOnceODRAutoHideLinkage => Self::LinkOnceODRAutoHide,
            LLVMLinkage::LLVMWeakAnyLinkage             => Self::WeakAny,
            LLVMLinkage::LLVMWeakODRLinkage             => Self::WeakODR,
            LLVMLinkage::LLVMAppendingLinkage           => Self::Appending,
            LLVMLinkage::LLVMInternalLinkage            => Self::Internal,
            LLVMLinkage::LLVMPrivateLinkage             => Self::Private,
            LLVMLinkage::LLVMDLLImportLinkage           => Self::DLLImport,
            LLVMLinkage::LLVMDLLExportLinkage           => Self::DLLExport,
            LLVMLinkage::LLVMExternalWeakLinkage        => Self::ExternalWeak,
            LLVMLinkage::LLVMGhostLinkage               => Self::Ghost,
            LLVMLinkage::LLVMCommonLinkage              => Self::Common,
            LLVMLinkage::LLVMLinkerPrivateLinkage       => Self::LinkerPrivate,
            LLVMLinkage::LLVMLinkerPrivateWeakLinkage   => Self::LinkerPrivateWeak,
        }
    }
}

#[rustfmt::skip]
impl Into<LLVMLinkage> for Linkage {
    fn into(self) -> LLVMLinkage {
        match self {
            Self::External            => LLVMLinkage::LLVMExternalLinkage,
            Self::AvailableExternally => LLVMLinkage::LLVMAvailableExternallyLinkage,
            Self::LinkOnceAny         => LLVMLinkage::LLVMLinkOnceAnyLinkage,
            Self::LinkOnceODR         => LLVMLinkage::LLVMLinkOnceODRLinkage,
            Self::LinkOnceODRAutoHide => LLVMLinkage::LLVMLinkOnceODRAutoHideLinkage,
            Self::WeakAny             => LLVMLinkage::LLVMWeakAnyLinkage,
            Self::WeakODR             => LLVMLinkage::LLVMWeakODRLinkage,
            Self::Appending           => LLVMLinkage::LLVMAppendingLinkage,
            Self::Internal            => LLVMLinkage::LLVMInternalLinkage,
            Self::Private             => LLVMLinkage::LLVMPrivateLinkage,
            Self::DLLImport           => LLVMLinkage::LLVMDLLImportLinkage,
            Self::DLLExport           => LLVMLinkage::LLVMDLLExportLinkage,
            Self::ExternalWeak        => LLVMLinkage::LLVMExternalWeakLinkage,
            Self::Ghost               => LLVMLinkage::LLVMGhostLinkage,
            Self::Common              => LLVMLinkage::LLVMCommonLinkage,
            Self::LinkerPrivate       => LLVMLinkage::LLVMLinkerPrivateLinkage,
            Self::LinkerPrivateWeak   => LLVMLinkage::LLVMLinkerPrivateWeakLinkage,
        }
    }
}
