use crate::llvm::{
    // instructions::instruction::{Add, IntValue, Mul, SDiv, Sub, UDiv, Valued},
    module::Builder,
    types::{IntType, Type, TypeKind},
    utils::EMPTY_CSTR,
    values::{
        AnyValue, ArrayValue, BasicBlock, CallSiteValue, FunctionOrPointer, FunctionValue, Global,
        InstructionValue, PointerValue, SealedAnyValue, Value,
    },
    Error,
    ErrorKind,
    Result,
};
use llvm_sys::{
    core::{
        LLVMAddCase, LLVMBuildAdd, LLVMBuildBitCast, LLVMBuildBr, LLVMBuildCall2, LLVMBuildFDiv,
        LLVMBuildGlobalString, LLVMBuildGlobalStringPtr, LLVMBuildMul, LLVMBuildPointerCast,
        LLVMBuildRet, LLVMBuildRetVoid, LLVMBuildSDiv, LLVMBuildSub, LLVMBuildSwitch,
        LLVMBuildUDiv,
        LLVMBuildUnreachable,
        /* LLVMConstAdd, LLVMConstMul, LLVMConstSDiv, LLVMConstSub, LLVMConstUDiv, */
    },
    LLVMValue,
};
use std::{
    cmp::Ordering,
    ffi::CString,
    fmt::{Debug, Formatter, Result as FmtResult},
    hash::{Hash, Hasher},
    ops::Deref,
};

pub struct BuildingBlock<'ctx> {
    block: BasicBlock<'ctx>,
    builder: Builder<'ctx>,
}

// Public interface
impl<'ctx> BuildingBlock<'ctx> {
    #[inline]
    pub const fn basic_block(&self) -> BasicBlock<'ctx> {
        self.block
    }

    // TODO: Take in addable values
    #[inline]
    pub fn add(
        &self,
        lhs: impl Into<Value<'ctx>>,
        rhs: impl Into<Value<'ctx>>,
    ) -> Result<Value<'ctx>> {
        let add = unsafe {
            Value::from_raw(LLVMBuildAdd(
                self.builder.as_mut_ptr(),
                lhs.into().as_mut_ptr(),
                rhs.into().as_mut_ptr(),
                EMPTY_CSTR,
            ))?
        };

        Ok(add)
    }

    // TODO: Take in subtractable values
    #[inline]
    pub fn sub(
        &self,
        lhs: impl Into<Value<'ctx>>,
        rhs: impl Into<Value<'ctx>>,
    ) -> Result<Value<'ctx>> {
        let sub = unsafe {
            Value::from_raw(LLVMBuildSub(
                self.builder.as_mut_ptr(),
                lhs.into().as_mut_ptr(),
                rhs.into().as_mut_ptr(),
                EMPTY_CSTR,
            ))?
        };

        Ok(sub)
    }

    #[inline]
    pub fn mult(
        &self,
        lhs: impl Into<Value<'ctx>>,
        rhs: impl Into<Value<'ctx>>,
    ) -> Result<Value<'ctx>> {
        let mult = unsafe {
            Value::from_raw(LLVMBuildMul(
                self.builder.as_mut_ptr(),
                lhs.into().as_mut_ptr(),
                rhs.into().as_mut_ptr(),
                EMPTY_CSTR,
            ))?
        };

        Ok(mult)
    }

    #[inline]
    pub fn signed_div(
        &self,
        lhs: impl Into<Value<'ctx>>,
        rhs: impl Into<Value<'ctx>>,
    ) -> Result<Value<'ctx>> {
        let mult = unsafe {
            Value::from_raw(LLVMBuildSDiv(
                self.builder.as_mut_ptr(),
                lhs.into().as_mut_ptr(),
                rhs.into().as_mut_ptr(),
                EMPTY_CSTR,
            ))?
        };

        Ok(mult)
    }

    #[inline]
    pub fn unsigned_div(
        &self,
        lhs: impl Into<Value<'ctx>>,
        rhs: impl Into<Value<'ctx>>,
    ) -> Result<Value<'ctx>> {
        let mult = unsafe {
            Value::from_raw(LLVMBuildUDiv(
                self.builder.as_mut_ptr(),
                lhs.into().as_mut_ptr(),
                rhs.into().as_mut_ptr(),
                EMPTY_CSTR,
            ))?
        };

        Ok(mult)
    }

    /*
    #[inline]
    pub fn add<T, L, R>(&self, lhs: L, rhs: R) -> Result<Add<'ctx, T>>
    where
        L: Into<IntValue<'ctx, T>>,
        R: Into<IntValue<'ctx, T>>,
    {
        let (lhs, rhs) = (lhs.into(), rhs.into());

        let add = unsafe {
            Add::from_raw(if lhs.is_const() && rhs.is_const() {
                LLVMConstAdd(lhs.llvm_ptr().as_ptr(), rhs.llvm_ptr().as_ptr())
            } else {
                LLVMBuildAdd(
                    self.builder.as_mut_ptr(),
                    lhs.llvm_ptr().as_ptr(),
                    rhs.llvm_ptr().as_ptr(),
                    EMPTY_CSTR,
                )
            })?
        };

        Ok(add)
    }

    #[inline]
    pub fn sub<T, L, R>(&self, lhs: L, rhs: R) -> Result<Sub<'ctx, T>>
    where
        L: Into<IntValue<'ctx, T>>,
        R: Into<IntValue<'ctx, T>>,
    {
        let (lhs, rhs) = (lhs.into(), rhs.into());

        let sub = unsafe {
            Sub::from_raw(if lhs.is_const() && rhs.is_const() {
                LLVMConstSub(lhs.llvm_ptr().as_ptr(), rhs.llvm_ptr().as_ptr())
            } else {
                LLVMBuildSub(
                    self.builder.as_mut_ptr(),
                    lhs.llvm_ptr().as_ptr(),
                    rhs.llvm_ptr().as_ptr(),
                    EMPTY_CSTR,
                )
            })?
        };

        Ok(sub)
    }

    #[inline]
    pub fn mul<T, L, R>(&self, lhs: L, rhs: R) -> Result<Mul<'ctx, T>>
    where
        L: Into<IntValue<'ctx, T>>,
        R: Into<IntValue<'ctx, T>>,
    {
        let (lhs, rhs) = (lhs.into(), rhs.into());

        let mul = unsafe {
            Mul::from_raw(if lhs.is_const() && rhs.is_const() {
                LLVMConstMul(lhs.llvm_ptr().as_ptr(), rhs.llvm_ptr().as_ptr())
            } else {
                LLVMBuildMul(
                    self.builder.as_mut_ptr(),
                    lhs.llvm_ptr().as_ptr(),
                    rhs.llvm_ptr().as_ptr(),
                    EMPTY_CSTR,
                )
            })?
        };

        Ok(mul)
    }

    #[inline]
    pub fn sdiv<T, L, R>(&self, lhs: L, rhs: R) -> Result<SDiv<'ctx, T>>
    where
        L: Into<IntValue<'ctx, T>>,
        R: Into<IntValue<'ctx, T>>,
    {
        let (lhs, rhs) = (lhs.into(), rhs.into());

        let div = unsafe {
            SDiv::from_raw(if lhs.is_const() && rhs.is_const() {
                LLVMConstSDiv(lhs.llvm_ptr().as_ptr(), rhs.llvm_ptr().as_ptr())
            } else {
                LLVMBuildSDiv(
                    self.builder.as_mut_ptr(),
                    lhs.llvm_ptr().as_ptr(),
                    rhs.llvm_ptr().as_ptr(),
                    EMPTY_CSTR,
                )
            })?
        };

        Ok(div)
    }

    #[inline]
    pub fn udiv<T, L, R>(&self, lhs: L, rhs: R) -> Result<UDiv<'ctx, T>>
    where
        L: Into<IntValue<'ctx, T>>,
        R: Into<IntValue<'ctx, T>>,
    {
        let (lhs, rhs) = (lhs.into(), rhs.into());

        let div = unsafe {
            UDiv::from_raw(if lhs.is_const() && rhs.is_const() {
                LLVMConstUDiv(lhs.llvm_ptr().as_ptr(), rhs.llvm_ptr().as_ptr())
            } else {
                LLVMBuildUDiv(
                    self.builder.as_mut_ptr(),
                    lhs.llvm_ptr().as_ptr(),
                    rhs.llvm_ptr().as_ptr(),
                    EMPTY_CSTR,
                )
            })?
        };

        Ok(div)
    }
    */

    #[inline]
    pub fn float_div(
        &self,
        lhs: impl Into<Value<'ctx>>,
        rhs: impl Into<Value<'ctx>>,
    ) -> Result<Value<'ctx>> {
        let mult = unsafe {
            Value::from_raw(LLVMBuildFDiv(
                self.builder.as_mut_ptr(),
                lhs.into().as_mut_ptr(),
                rhs.into().as_mut_ptr(),
                EMPTY_CSTR,
            ))?
        };

        Ok(mult)
    }

    // TODO: Take in a returnable value
    #[inline]
    pub fn ret(&self, value: Option<Value<'ctx>>) -> Result<InstructionValue<'ctx>> {
        // TODO: Verify return type is correct

        let ret = unsafe {
            let ret = if let Some(value) = value {
                LLVMBuildRet(self.builder.as_mut_ptr(), value.as_mut_ptr())
            } else {
                LLVMBuildRetVoid(self.builder.as_mut_ptr())
            };

            InstructionValue::from_raw(ret)?
        };

        debug_assert!(ret.is_return());

        Ok(ret)
    }

    #[inline]
    pub fn branch(&self, block: impl AsRef<BasicBlock<'ctx>>) -> Result<InstructionValue<'ctx>> {
        let branch = unsafe {
            InstructionValue::from_raw(LLVMBuildBr(
                self.builder.as_mut_ptr(),
                (*block.as_ref()).as_mut_ptr(),
            ))?
        };

        debug_assert!(branch.is_branch());

        Ok(branch)
    }

    pub fn conditional_branch(
        &self,
        condition: Value<'ctx>,
        true_branch: impl AsRef<BasicBlock<'ctx>>,
        false_branch: impl AsRef<BasicBlock<'ctx>>,
    ) -> Result<InstructionValue<'ctx>> {
        let cond_branch = unsafe {
            InstructionValue::from_raw(llvm_sys::core::LLVMBuildCondBr(
                self.builder.as_mut_ptr(),
                condition.as_mut_ptr(),
                (*true_branch.as_ref()).as_mut_ptr(),
                (*false_branch.as_ref()).as_mut_ptr(),
            ))?
        };

        Ok(cond_branch)
    }

    #[inline]
    pub fn unreachable(&self) -> Result<InstructionValue<'ctx>> {
        let unreachable =
            unsafe { InstructionValue::from_raw(LLVMBuildUnreachable(self.builder.as_mut_ptr()))? };

        debug_assert!(unreachable.is_unreachable());

        Ok(unreachable)
    }

    #[inline]
    pub fn bitcast(&self, value: Value<'ctx>, dest_ty: Type<'ctx>) -> Result<Value<'ctx>> {
        unsafe {
            Value::from_raw(LLVMBuildBitCast(
                self.builder.as_mut_ptr(),
                value.as_mut_ptr(),
                dest_ty.as_mut_ptr(),
                EMPTY_CSTR,
            ))
        }
    }

    #[inline]
    pub fn switch(
        &self,
        condition: Value<'ctx>,
        default: BasicBlock<'ctx>,
        jumps: &[(Value<'ctx>, BasicBlock<'ctx>)],
    ) -> Result<InstructionValue<'ctx>> {
        let switch = unsafe {
            InstructionValue::from_raw(LLVMBuildSwitch(
                self.builder.as_mut_ptr(),
                condition.as_mut_ptr(),
                default.as_mut_ptr(),
                jumps.len() as u32,
            ))?
        };

        for (condition, block) in jumps {
            unsafe {
                LLVMAddCase(
                    switch.as_mut_ptr(),
                    condition.as_mut_ptr(),
                    (*block).as_mut_ptr(),
                );
            }
        }

        Ok(switch)
    }

    pub fn call<F>(&self, function: F, args: &[Value<'ctx>]) -> Result<CallSiteValue<'ctx>>
    where
        F: Into<FunctionOrPointer<'ctx>>,
    {
        let (value, sig) = match function.into() {
            FunctionOrPointer::Function(value) => {
                (value, FunctionValue::from_val(value).signature()?)
            }

            FunctionOrPointer::Pointer(value) => {
                let ty = value.as_type()?;
                let kind = ty.element_type()?.kind();

                if kind != TypeKind::Function {
                    return Err(Error::new(
                        "Called a pointer that was not a function",
                        ErrorKind::MismatchedTypes,
                    ));
                }

                (value, FunctionValue::from_val(value).signature()?)
            }
        };

        let mut args = self.cast_call_args(args, &sig.args()?)?;

        unsafe {
            let value = LLVMBuildCall2(
                self.builder.as_mut_ptr(),
                sig.as_mut_ptr(),
                value.as_mut_ptr(),
                args.as_mut_ptr(),
                args.len() as u32,
                EMPTY_CSTR,
            );

            CallSiteValue::from_raw(value)
        }
    }

    pub fn create_global_string_ptr(
        &self,
        value: &str,
        name: &str,
    ) -> Result<PointerValue<'ctx, Global<IntType<'ctx, i8>>>> {
        let c_string_value = CString::new(value)?;
        let c_string_name = CString::new(name)?;

        unsafe {
            let value = LLVMBuildGlobalStringPtr(
                self.builder.as_mut_ptr(),
                c_string_value.as_ptr(),
                c_string_name.as_ptr(),
            );

            PointerValue::from_raw(value)
        }
    }

    pub fn create_global_string(&self, value: &str, name: &str) -> Result<ArrayValue<'ctx>> {
        let c_string_value = CString::new(value)?;
        let c_string_name = CString::new(name)?;

        unsafe {
            let value = LLVMBuildGlobalString(
                self.builder.as_mut_ptr(),
                c_string_value.as_ptr(),
                c_string_name.as_ptr(),
            );

            ArrayValue::from_raw(value)
        }
    }

    pub fn ptr_cast(
        &self,
        value: Value<'ctx>,
        cast_ty: Type<'ctx>,
        name: &str,
    ) -> Result<Value<'ctx>> {
        let name = CString::new(name)?;

        unsafe {
            let value = LLVMBuildPointerCast(
                self.builder.as_mut_ptr(),
                value.as_mut_ptr(),
                cast_ty.as_mut_ptr(),
                name.as_ptr(),
            );

            Value::from_raw(value)
        }
    }
}

// Private interface
impl<'ctx> BuildingBlock<'ctx> {
    pub(crate) const fn new(block: BasicBlock<'ctx>, builder: Builder<'ctx>) -> Self {
        Self { block, builder }
    }

    pub(crate) const fn builder(&self) -> &Builder<'ctx> {
        &self.builder
    }

    pub(crate) fn cast_call_args(
        &self,
        values: &[Value<'ctx>],
        types: &[Type<'ctx>],
    ) -> Result<Vec<*mut LLVMValue>> {
        let casted_args: Vec<*mut LLVMValue> = types
            .iter()
            .zip(values.iter())
            .map(|(expected_ty, &val)| {
                let ty = val.as_type().unwrap();

                if *expected_ty != ty {
                    self.bitcast(val, *expected_ty).unwrap().as_mut_ptr()
                } else {
                    val.as_mut_ptr()
                }
            })
            .collect();

        Ok(casted_args)
    }
}

impl Debug for BuildingBlock<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        Debug::fmt(&self.block, f)
    }
}

impl<'ctx> PartialEq for BuildingBlock<'ctx> {
    fn eq(&self, other: &Self) -> bool {
        self.block.eq(&other.block)
    }
}

impl<'ctx> Eq for BuildingBlock<'ctx> {}

impl<'ctx> PartialOrd for BuildingBlock<'ctx> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.block.partial_cmp(&other.block)
    }
}

impl<'ctx> Ord for BuildingBlock<'ctx> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.block.cmp(&other.block)
    }
}

impl<'ctx> Hash for BuildingBlock<'ctx> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.block.hash(state)
    }
}

impl<'ctx> AsRef<BasicBlock<'ctx>> for BuildingBlock<'ctx> {
    fn as_ref(&self) -> &BasicBlock<'ctx> {
        &self.block
    }
}

impl<'ctx> Deref for BuildingBlock<'ctx> {
    type Target = BasicBlock<'ctx>;

    fn deref(&self) -> &Self::Target {
        &self.block
    }
}
