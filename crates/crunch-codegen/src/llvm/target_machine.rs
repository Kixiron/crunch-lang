use crate::llvm::{
    error::ErrorString,
    module::Module,
    utils::{to_non_nul, LLVMString},
    Error, ErrorKind, Result,
};
use llvm_sys::target_machine::{
    LLVMCodeGenFileType, LLVMCodeGenOptLevel, LLVMCodeModel, LLVMCreateTargetMachine,
    LLVMDisposeTargetMachine, LLVMGetHostCPUFeatures, LLVMGetHostCPUName, LLVMGetTargetFromTriple,
    LLVMGetTargetMachineCPU, LLVMGetTargetMachineFeatureString, LLVMGetTargetMachineTarget,
    LLVMGetTargetMachineTriple, LLVMOpaqueTargetMachine, LLVMRelocMode, LLVMTarget,
    LLVMTargetMachineEmitToFile,
};
use std::{
    ffi::{CStr, CString},
    mem::MaybeUninit,
    ptr::NonNull,
};

pub struct Target {
    target: NonNull<LLVMTarget>,
}

impl Target {
    #[inline]
    pub fn from_triple(triple: &str) -> Result<Self> {
        let mut target = MaybeUninit::zeroed();
        let mut err_message = MaybeUninit::zeroed();

        let failed = {
            unsafe {
                LLVMGetTargetFromTriple(
                    CString::new(triple)?.as_ptr(),
                    target.as_mut_ptr(),
                    err_message.as_mut_ptr(),
                ) == 1
            }
        };

        if failed {
            let err_message = unsafe { LLVMString::from_raw(err_message.assume_init())? };

            Err(Error::new(err_message, ErrorKind::InvalidTriple))
        } else {
            let target = unsafe { target.assume_init() };
            // `from_raw` checks this, just want to make sure
            debug_assert!(!target.is_null());

            unsafe { Self::from_raw(target) }
        }
    }

    #[inline]
    pub fn init_native(conf: TargetConf) -> Result<()> {
        use llvm_sys::target::{
            LLVM_InitializeNativeAsmParser, LLVM_InitializeNativeAsmPrinter,
            LLVM_InitializeNativeDisassembler, LLVM_InitializeNativeTarget,
        };

        let err = || {
            Err(Error::new(
                "Unknown error in initializing native target",
                ErrorKind::FailedTargetInit,
            ))
        };

        unsafe {
            if conf.base {
                let failed = LLVM_InitializeNativeTarget() == 1;

                if failed {
                    return err();
                }
            }

            if conf.asm_parser {
                let failed = LLVM_InitializeNativeAsmParser() == 1;

                if failed {
                    return err();
                }
            }

            if conf.asm_printer {
                let failed = LLVM_InitializeNativeAsmPrinter() == 1;

                if failed {
                    return err();
                }
            }

            if conf.disassembler {
                let failed = LLVM_InitializeNativeDisassembler() == 1;

                if failed {
                    return err();
                }
            }
        }

        Ok(())
    }

    #[inline]
    pub fn init_x86(conf: TargetConf) {
        use llvm_sys::target::{
            LLVMInitializeX86AsmParser, LLVMInitializeX86AsmPrinter, LLVMInitializeX86Disassembler,
            LLVMInitializeX86Target, LLVMInitializeX86TargetInfo, LLVMInitializeX86TargetMC,
        };

        unsafe {
            if conf.base {
                LLVMInitializeX86Target();
            }

            if conf.info {
                LLVMInitializeX86TargetInfo();
            }

            if conf.asm_printer {
                LLVMInitializeX86AsmPrinter();
            }

            if conf.asm_parser {
                LLVMInitializeX86AsmParser();
            }

            if conf.disassembler {
                LLVMInitializeX86Disassembler();
            }

            if conf.machine_code {
                LLVMInitializeX86TargetMC();
            }
        }
    }
}

impl Target {
    #[inline]
    pub(crate) unsafe fn from_raw(raw: *mut LLVMTarget) -> Result<Self> {
        let target = to_non_nul(raw, "Failed to create Target")?;

        Ok(Self { target })
    }

    #[inline]
    pub(crate) const fn as_mut_ptr(&self) -> *mut LLVMTarget {
        self.target.as_ptr()
    }
}

pub struct TargetMachine {
    machine: NonNull<LLVMOpaqueTargetMachine>,
}

impl TargetMachine {
    #[inline]
    pub fn new(
        target: &Target,
        triple: &str,
        cpu: Option<&str>,
        features: Option<&str>,
        opt_level: Option<OptLevel>,
        reloc: Option<RelocMode>,
        code_model: Option<CodeModel>,
    ) -> Result<Self> {
        let (cpu, features, opt_level, reloc, code_model) = dbg!((
            // FIXME: Here be hacky
            if let Some(cpu) = cpu {
                ErrorString::from(cpu)
            } else {
                ErrorString::from(Self::host_cpu()?)
            },
            if let Some(features) = features {
                ErrorString::from(features)
            } else {
                ErrorString::from(Self::host_cpu_features()?)
            },
            opt_level.unwrap_or_default(),
            reloc.unwrap_or_default(),
            code_model.unwrap_or_default(),
        ));

        unsafe {
            Self::from_raw(LLVMCreateTargetMachine(
                target.as_mut_ptr(),
                CString::new(triple)?.as_ptr(),
                // FIXME: I hate this too
                CString::new(cpu.to_string())?.as_ptr(),
                CString::new(features.to_string())?.as_ptr(),
                opt_level.into(),
                reloc.into(),
                code_model.into(),
            ))
        }
    }

    #[inline]
    pub fn emit_to_file(
        &self,
        module: &Module<'_>,
        file_name: &str,
        codegen: CodegenFileKind,
    ) -> Result<()> {
        let file_name = CString::new(file_name)?;
        let mut err_message = MaybeUninit::zeroed();

        let failed = unsafe {
            LLVMTargetMachineEmitToFile(
                self.as_mut_ptr(),
                module.as_mut_ptr(),
                file_name.as_ptr() as *mut i8,
                codegen.into(),
                err_message.as_mut_ptr(),
            ) == 1
        };

        if failed {
            let err_message = unsafe { LLVMString::from_raw(err_message.assume_init())? };

            Err(Error::new(err_message, ErrorKind::FailedEmission))
        } else {
            Ok(())
        }
    }

    #[inline]
    pub fn target<'a>(&'a self) -> Result<Target> {
        unsafe { Target::from_raw(LLVMGetTargetMachineTarget(self.as_mut_ptr())) }
    }

    #[inline]
    pub fn target_triple<'a>(&'a self) -> Result<&'a CStr> {
        unsafe {
            to_non_nul(
                LLVMGetTargetMachineTriple(self.as_mut_ptr()),
                "Failed to get Target Triple",
            )
            .map(|cpu| CStr::from_ptr(cpu.as_ptr()))
        }
    }

    #[inline]
    pub fn target_cpu<'a>(&'a self) -> Result<&'a CStr> {
        unsafe {
            to_non_nul(
                LLVMGetTargetMachineCPU(self.as_mut_ptr()),
                "Failed to get Target CPU",
            )
            .map(|cpu| CStr::from_ptr(cpu.as_ptr()))
        }
    }

    #[inline]
    pub fn target_features<'a>(&'a self) -> Result<&'a CStr> {
        unsafe {
            to_non_nul(
                LLVMGetTargetMachineFeatureString(self.as_mut_ptr()),
                "Failed to get Target Features",
            )
            .map(|cpu| CStr::from_ptr(cpu.as_ptr()))
        }
    }

    #[inline]
    pub fn host_cpu() -> Result<LLVMString> {
        unsafe { LLVMString::from_raw(LLVMGetHostCPUName()) }
    }

    #[inline]
    pub fn host_cpu_features() -> Result<LLVMString> {
        unsafe { LLVMString::from_raw(LLVMGetHostCPUFeatures()) }
    }
}

impl TargetMachine {
    #[inline]
    pub(crate) unsafe fn from_raw(raw: *mut LLVMOpaqueTargetMachine) -> Result<Self> {
        let machine = to_non_nul(raw, "Failed to create TargetMachine")?;

        Ok(Self { machine })
    }

    #[inline]
    pub(crate) const fn as_mut_ptr(&self) -> *mut LLVMOpaqueTargetMachine {
        self.machine.as_ptr()
    }
}

impl Drop for TargetMachine {
    #[inline]
    fn drop(&mut self) {
        unsafe { LLVMDisposeTargetMachine(self.as_mut_ptr()) };
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum CodegenFileKind {
    Assembly,
    Object,
}

#[rustfmt::skip]
impl From<LLVMCodeGenFileType> for CodegenFileKind {
    #[inline]
    fn from(codegen: LLVMCodeGenFileType) -> Self {
        match codegen {
            LLVMCodeGenFileType::LLVMAssemblyFile => Self::Assembly,
            LLVMCodeGenFileType::LLVMObjectFile   => Self::Object,
        }
    }
}

#[rustfmt::skip]
impl Into<LLVMCodeGenFileType> for CodegenFileKind {
    #[inline]
    fn into(self) -> LLVMCodeGenFileType {
        match self {
            Self::Assembly => LLVMCodeGenFileType::LLVMAssemblyFile,
            Self::Object   => LLVMCodeGenFileType::LLVMObjectFile,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum OptLevel {
    None,
    Less,
    Default,
    Aggressive,
}

#[rustfmt::skip]
impl From<LLVMCodeGenOptLevel> for OptLevel {
    #[inline]
    fn from(opt_level: LLVMCodeGenOptLevel) -> Self {
        match opt_level {
            LLVMCodeGenOptLevel::LLVMCodeGenLevelNone       => Self::None,
            LLVMCodeGenOptLevel::LLVMCodeGenLevelLess       => Self::Less,
            LLVMCodeGenOptLevel::LLVMCodeGenLevelDefault    => Self::Default,
            LLVMCodeGenOptLevel::LLVMCodeGenLevelAggressive => Self::Aggressive,
        }
    }
}

#[rustfmt::skip]
impl Into<LLVMCodeGenOptLevel> for OptLevel {
    #[inline]
    fn into(self) -> LLVMCodeGenOptLevel {
        match self {
            Self::None       => LLVMCodeGenOptLevel::LLVMCodeGenLevelNone,
            Self::Less       => LLVMCodeGenOptLevel::LLVMCodeGenLevelLess,
            Self::Default    => LLVMCodeGenOptLevel::LLVMCodeGenLevelDefault,
            Self::Aggressive => LLVMCodeGenOptLevel::LLVMCodeGenLevelAggressive,
        }
    }
}

impl Default for OptLevel {
    #[inline]
    fn default() -> Self {
        Self::Default
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
#[allow(non_camel_case_types)]
pub enum RelocMode {
    Default,
    Static,
    PIC,
    DynamicNoPic,
    ROPI,
    RWPI,
    ROPI_RWPI,
}

#[rustfmt::skip]
impl From<LLVMRelocMode> for RelocMode {
    #[inline]
    fn from(mode: LLVMRelocMode) -> Self {
        match mode {
            LLVMRelocMode::LLVMRelocDefault      => Self::Default,
            LLVMRelocMode::LLVMRelocStatic       => Self::Static,
            LLVMRelocMode::LLVMRelocPIC          => Self::PIC,
            LLVMRelocMode::LLVMRelocDynamicNoPic => Self::DynamicNoPic,
            LLVMRelocMode::LLVMRelocROPI         => Self::ROPI,
            LLVMRelocMode::LLVMRelocRWPI         => Self::RWPI,
            LLVMRelocMode::LLVMRelocROPI_RWPI    => Self::ROPI_RWPI,
        }
    }
}

#[rustfmt::skip]
impl Into<LLVMRelocMode> for RelocMode {
    #[inline]
    fn into(self) ->LLVMRelocMode {
        match self {
            Self::Default      => LLVMRelocMode::LLVMRelocDefault,
            Self::Static       => LLVMRelocMode::LLVMRelocStatic,
            Self::PIC          => LLVMRelocMode::LLVMRelocPIC,
            Self::DynamicNoPic => LLVMRelocMode::LLVMRelocDynamicNoPic,
            Self::ROPI         => LLVMRelocMode::LLVMRelocROPI,
            Self::RWPI         => LLVMRelocMode::LLVMRelocRWPI,
            Self::ROPI_RWPI    => LLVMRelocMode::LLVMRelocROPI_RWPI,
        }
    }
}

impl Default for RelocMode {
    #[inline]
    fn default() -> Self {
        Self::Default
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum CodeModel {
    Default,
    JITDefault,
    Tiny,
    Small,
    Kernel,
    Medium,
    Large,
}

#[rustfmt::skip]
impl From<LLVMCodeModel> for CodeModel {
    #[inline]
    fn from(model: LLVMCodeModel) -> Self {
        match model {
            LLVMCodeModel::LLVMCodeModelDefault    => Self::Default,
            LLVMCodeModel::LLVMCodeModelJITDefault => Self::JITDefault,
            LLVMCodeModel::LLVMCodeModelTiny       => Self::Tiny,
            LLVMCodeModel::LLVMCodeModelSmall      => Self::Small,
            LLVMCodeModel::LLVMCodeModelKernel     => Self::Kernel,
            LLVMCodeModel::LLVMCodeModelMedium     => Self::Medium,
            LLVMCodeModel::LLVMCodeModelLarge      => Self::Large,
        }
    }
}

#[rustfmt::skip]
impl Into<LLVMCodeModel> for CodeModel {
    #[inline]
    fn into(self) -> LLVMCodeModel {
        match self {
            Self::Default    => LLVMCodeModel::LLVMCodeModelDefault,
            Self::JITDefault => LLVMCodeModel::LLVMCodeModelJITDefault,
            Self::Tiny       => LLVMCodeModel::LLVMCodeModelTiny,
            Self::Small      => LLVMCodeModel::LLVMCodeModelSmall,
            Self::Kernel     => LLVMCodeModel::LLVMCodeModelKernel,
            Self::Medium     => LLVMCodeModel::LLVMCodeModelMedium,
            Self::Large      => LLVMCodeModel::LLVMCodeModelLarge,
        }
    }
}

impl Default for CodeModel {
    #[inline]
    fn default() -> Self {
        Self::Default
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TargetConf {
    pub base: bool,
    pub info: bool,
    pub asm_printer: bool,
    pub asm_parser: bool,
    pub disassembler: bool,
    pub machine_code: bool,

    #[doc(hidden)]
    pub __private: (),
}

impl TargetConf {
    pub const fn all() -> Self {
        Self {
            base: true,
            info: true,
            asm_printer: true,
            asm_parser: true,
            disassembler: true,
            machine_code: true,
            __private: (),
        }
    }
}

impl Default for TargetConf {
    fn default() -> Self {
        Self {
            base: true,
            info: true,
            asm_printer: false,
            asm_parser: false,
            disassembler: false,
            machine_code: true,
            __private: (),
        }
    }
}
