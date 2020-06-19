use crate::llvm::{Error, ErrorKind, Result};
use std::convert::TryFrom;

// From https://llvm.org/doxygen/group__LLVMCCoreTypes.html#ga6bd315e1c1c05eb625e59a9f748e924f
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
#[rustfmt::skip]
pub enum CallingConvention {
    C             = 0,
    Fast          = 8,
    Cold          = 9,
    GHC           = 10,
    HiPE          = 11,
    WebKitJS      = 12,
    AnyReg        = 13,
    PreserveMost  = 14,
    PreserveAll   = 15,
    Swift         = 16,
    CXXFASTTLS    = 17,
    X86Stdcall    = 64,
    X86Fastcall   = 65,
    ARMAPCS       = 66,
    ARMAAPCS      = 67,
    ARMAAPCSVFP   = 68,
    MSP430INTR    = 69,
    X86ThisCall   = 70,
    PTXKernel     = 71,
    PTXDevice     = 72,
    SPIRFUNC      = 75,
    SPIRKERNEL    = 76,
    IntelOCLBI    = 77,
    X8664SysV     = 78,
    Win64         = 79,
    X86VectorCall = 80,
    HHVM          = 81,
    HHVMC         = 82,
    X86INTR       = 83,
    AVRINTR       = 84,
    AVRSIGNAL     = 85,
    AVRBUILTIN    = 86,
    AMDGPUVS      = 87,
    AMDGPUGS      = 88,
    AMDGPUPS      = 89,
    AMDGPUCS      = 90,
    AMDGPUKERNEL  = 91,
    X86RegCall    = 92,
    AMDGPUHS      = 93,
    MSP430BUILTIN = 94,
    AMDGPULS      = 95,
    AMDGPUES      = 96,
}

#[rustfmt::skip]
impl TryFrom<u32> for CallingConvention {
    type Error = Error;

    fn try_from(convention: u32) -> Result<Self> {
        match convention {
            0  => Ok(Self::C),
            8  => Ok(Self::Fast),
            9  => Ok(Self::Cold),
            10 => Ok(Self::GHC),
            11 => Ok(Self::HiPE),
            12 => Ok(Self::WebKitJS),
            13 => Ok(Self::AnyReg),
            14 => Ok(Self::PreserveMost),
            15 => Ok(Self::PreserveAll),
            16 => Ok(Self::Swift),
            17 => Ok(Self::CXXFASTTLS),
            64 => Ok(Self::X86Stdcall),
            65 => Ok(Self::X86Fastcall),
            66 => Ok(Self::ARMAPCS),
            67 => Ok(Self::ARMAAPCS),
            68 => Ok(Self::ARMAAPCSVFP),
            69 => Ok(Self::MSP430INTR),
            70 => Ok(Self::X86ThisCall),
            71 => Ok(Self::PTXKernel),
            72 => Ok(Self::PTXDevice),
            75 => Ok(Self::SPIRFUNC),
            76 => Ok(Self::SPIRKERNEL),
            77 => Ok(Self::IntelOCLBI),
            78 => Ok(Self::X8664SysV),
            79 => Ok(Self::Win64),
            80 => Ok(Self::X86VectorCall),
            81 => Ok(Self::HHVM),
            82 => Ok(Self::HHVMC),
            83 => Ok(Self::X86INTR),
            84 => Ok(Self::AVRINTR),
            85 => Ok(Self::AVRSIGNAL),
            86 => Ok(Self::AVRBUILTIN),
            87 => Ok(Self::AMDGPUVS),
            88 => Ok(Self::AMDGPUGS),
            89 => Ok(Self::AMDGPUPS),
            90 => Ok(Self::AMDGPUCS),
            91 => Ok(Self::AMDGPUKERNEL),
            92 => Ok(Self::X86RegCall),
            93 => Ok(Self::AMDGPUHS),
            94 => Ok(Self::MSP430BUILTIN),
            95 => Ok(Self::AMDGPULS),
            96 => Ok(Self::AMDGPUES),

            _ => Err(Error::new(
                "LLVM returned an unrecognized AddressSpace",
                ErrorKind::LLVMError,
            )),
        }
    }
}
