use crate::{
    llvm::{context::Context, Result, LLVM},
    null,
};
use llvm_sys::{
    core::{
        LLVMGetTypeKind, LLVMGetUndef, LLVMInt16TypeInContext, LLVMInt32TypeInContext,
        LLVMInt64TypeInContext, LLVMInt8TypeInContext, LLVMIsNull, LLVMIsUndef,
        LLVMValueIsBasicBlock, LLVMVoidTypeInContext,
    },
    LLVMType, LLVMTypeKind, LLVMValue,
};
use std::{marker::PhantomData, ptr::NonNull};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[repr(transparent)]
pub struct Value<'a>(NonNull<LLVMValue>, PhantomData<&'a Context>);

// Public interface
impl<'a> Value<'a> {
    #[inline]
    pub fn is_undef(self) -> bool {
        unsafe { LLVMIsUndef(self.as_mut_ptr()) != 0 }
    }

    #[inline]
    pub fn is_null(self) -> bool {
        unsafe { LLVMIsNull(self.as_mut_ptr()) != 0 }
    }

    #[inline]
    pub fn is_basic_block(self) -> bool {
        unsafe { LLVMValueIsBasicBlock(self.as_mut_ptr()) != 0 }
    }
}

// Private interface
impl<'a> Value<'a> {
    #[inline]
    pub(crate) const fn from_raw(value: NonNull<LLVMValue>) -> Self {
        Self(value, PhantomData)
    }

    #[inline]
    pub(crate) const fn as_mut_ptr(self) -> *mut LLVMValue {
        self.0.as_ptr() as *mut LLVMValue
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[repr(transparent)]
pub struct Type<'a>(NonNull<LLVMType>, PhantomData<&'a Context>);

// Public interface
impl<'a> Type<'a> {
    #[inline]
    pub fn into_undef(self) -> Result<Value<'a>> {
        let undef = Value::from_raw(null!(
            unsafe { LLVMGetUndef(self.as_mut_ptr()) },
            "Failed to turn type into undef value",
        )?);
        debug_assert!(undef.is_undef());

        Ok(undef)
    }
}

// Private interface
impl<'a> Type<'a> {
    #[inline]
    pub(crate) const fn from_raw(raw_type: NonNull<LLVMType>) -> Self {
        Self(raw_type, PhantomData)
    }

    #[inline]
    pub(crate) const fn as_mut_ptr(self) -> *mut LLVMType {
        self.0.as_ptr() as *mut LLVMType
    }

    #[inline]
    pub(crate) fn type_kind(self) -> TypeKind {
        TypeKind::from(unsafe { LLVMGetTypeKind(self.as_mut_ptr()) })
    }
}

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
    PPC_FP128,
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
            LLVMTypeKind::LLVMPPC_FP128TypeKind => Self::PPC_FP128,
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

pub trait Typeable: sealed::Sealed {
    unsafe fn create_type<'a>(llvm: &'a LLVM) -> Result<Type<'a>>;
}

mod sealed {
    pub trait Sealed {}
}

macro_rules! typeable {
    ($($ty:ty => $func:path : $expected:path),* $(,)?) => {
        $(
            impl Typeable for $ty {
                unsafe fn create_type<'a>(llvm: &'a LLVM) -> Result<Type<'a>> {
                    let ty = Type::from_raw($crate::null!(
                        $func(llvm.context().as_mut_ptr()),
                        concat!("Failed to create `", stringify!($ty), "` type"),
                    )?);

                    // Make sure the type we got was the one we wanted
                    debug_assert_eq!(ty.type_kind(), $expected);

                    Ok(ty)
                }
            }

            impl sealed::Sealed for $ty {}
        )*
    };
}

typeable! {
    ()    => LLVMVoidTypeInContext  : TypeKind::Void,
    i8    => LLVMInt8TypeInContext  : TypeKind::Integer,
    i16   => LLVMInt16TypeInContext : TypeKind::Integer,
    i32   => LLVMInt32TypeInContext : TypeKind::Integer,
    i64   => LLVMInt64TypeInContext : TypeKind::Integer,
}

#[cfg(test)]
mod tests {
    use crate::llvm::LLVM;

    #[test]
    fn create_value() {
        let llvm = LLVM::new().unwrap();
        let module = llvm.create_module("module").unwrap();
        let sig = module
            .add_function_type(llvm.get_type::<()>().unwrap(), &[], false)
            .unwrap();
        let _value = module.build_function("function", &sig, |_| Ok(())).unwrap();
    }

    #[test]
    fn undef_is_undef() {
        let llvm = LLVM::new().unwrap();

        let undef = llvm.get_type::<i32>().unwrap().into_undef().unwrap();
        assert!(undef.is_undef());
    }

    #[test]
    fn use_undef() {
        let llvm = LLVM::new().unwrap();
        let module = llvm.create_module("ub_for_me").unwrap();

        let hei_32 = llvm.get_type::<i32>().unwrap();
        let sig = module
            .add_function_type(hei_32, &[hei_32, hei_32], false)
            .unwrap();

        module
            .build_function("add_undef", &sig, |builder| {
                let undef = hei_32.into_undef()?;

                let block = builder.append_block("ub_goes_here")?;
                let add = block.add(
                    builder.args()[0].value(),
                    builder.args()[1].value(),
                    "ub_goes_here.0",
                )?;
                let add = block.add(add, undef, "ub_goes_here.1")?;
                block.ret(add)?;

                Ok(())
            })
            .unwrap();

        println!("{:?}", module);
    }
}
