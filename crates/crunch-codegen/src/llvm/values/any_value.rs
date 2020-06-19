use crate::llvm::{
    types::{Type, TypeKind},
    values::{SealedAnyValue, Value, ValueUsage},
    Result,
};
use llvm_sys::core::{
    LLVMGetFirstUse, LLVMGetValueName2, LLVMIsAAllocaInst, LLVMIsABinaryOperator,
    LLVMIsABranchInst, LLVMIsACallInst, LLVMIsACastInst, LLVMIsACmpInst, LLVMIsAConstant,
    LLVMIsAConstantInt, LLVMIsAFenceInst, LLVMIsAFunction, LLVMIsAInlineAsm, LLVMIsALoadInst,
    LLVMIsAReturnInst, LLVMIsAStoreInst, LLVMIsATruncInst, LLVMIsAUnreachableInst,
    LLVMIsAVAArgInst, LLVMIsNull, LLVMIsUndef, LLVMReplaceAllUsesWith, LLVMSetValueName2,
    LLVMTypeOf, LLVMValueIsBasicBlock,
};
use std::{borrow::Cow, convert::TryFrom, ptr::NonNull};

// TODO: Docs for both the macro and the produced functions
macro_rules! value_is {
    ($($name:ident => $func:path),* $(,)?) => {
        $(
            #[inline]
            fn $name(&self) -> bool {
                unsafe { $func(self.as_mut_ptr()) as usize != 0 }
            }
        )*
    };
}

pub trait AnyValue<'ctx>: SealedAnyValue<'ctx> {
    /// Replace all uses of one `Value` with another
    // https://llvm.org/doxygen/classllvm_1_1Value.html#a3ab5fc45117b450e8bb04e564cb6e5f2
    fn replace_all_uses(self, new: Self) -> Self {
        unsafe { LLVMReplaceAllUsesWith(self.as_mut_ptr(), new.as_mut_ptr()) };

        new
    }

    fn as_value(&self) -> Value<'ctx> {
        Value::from_val(self.as_val())
    }

    fn as_type(&self) -> Result<Type<'ctx>> {
        unsafe { Type::from_raw(LLVMTypeOf(self.as_mut_ptr())) }
    }

    fn kind(&self) -> Result<TypeKind> {
        Ok(self.as_type()?.kind())
    }

    fn name<'a>(&'a self) -> Option<Result<&'a str>> {
        self.get_name_raw()
            .map(|name| std::str::from_utf8(name).map_err(|err| err.into()))
    }

    fn name_lossy<'a>(&'a self) -> Option<Cow<'a, str>> {
        self.get_name_raw().map(String::from_utf8_lossy)
    }

    fn set_name(&self, name: &str) {
        self.set_name_raw(name.as_bytes());
    }

    // https://github.com/rust-lang/rust/pull/67033
    fn get_name_raw<'a>(&'a self) -> Option<&'a [u8]> {
        unsafe {
            let mut len = 0;
            let data = NonNull::new(LLVMGetValueName2(self.as_mut_ptr(), &mut len) as *mut u8)?;

            Some(std::slice::from_raw_parts(data.as_ptr(), len))
        }
    }

    // https://github.com/rust-lang/rust/pull/67033
    fn set_name_raw<'a>(&self, name: &[u8]) {
        unsafe {
            let data = name.as_ptr().cast();
            LLVMSetValueName2(self.as_mut_ptr(), data, name.len());
        }
    }

    fn get_first_use(&self) -> Option<ValueUsage<'ctx>> {
        unsafe {
            let usage = LLVMGetFirstUse(self.as_mut_ptr());

            NonNull::new(usage).map(|usage| ValueUsage::from_non_nul(usage))
        }
    }

    fn downcast<T: TryFrom<Value<'ctx>> + 'ctx>(&self) -> Option<T> {
        T::try_from(self.as_value()).ok()
    }

    // TODO: There are more of these...
    value_is! {
        is_undef       => LLVMIsUndef,
        is_null        => LLVMIsNull,
        is_basic_block => LLVMValueIsBasicBlock,
        is_function    => LLVMIsAFunction,
        is_load        => LLVMIsALoadInst,
        is_cmp         => LLVMIsACmpInst,
        is_call        => LLVMIsACallInst,
        is_cast        => LLVMIsACastInst,
        is_branch      => LLVMIsABranchInst,
        is_fence       => LLVMIsAFenceInst,
        is_inline_asm  => LLVMIsAInlineAsm,
        is_store       => LLVMIsAStoreInst,
        is_trunc       => LLVMIsATruncInst,
        is_var_arg     => LLVMIsAVAArgInst,
        is_alloc       => LLVMIsAAllocaInst,
        is_binary_op   => LLVMIsABinaryOperator,
        is_return      => LLVMIsAReturnInst,
        is_unreachable => LLVMIsAUnreachableInst,
        is_const_int   => LLVMIsAConstantInt,
        is_constant    => LLVMIsAConstant,
    }
}
