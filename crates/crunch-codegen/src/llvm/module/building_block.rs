use crate::llvm::{
    module::Builder,
    types::{FunctionSig, IntType, SealedAnyType, Type, TypeKind},
    utils::UNNAMED_CSTR,
    values::{
        AnyValue, ArrayValue, BasicBlock, CallSiteValue, FunctionOrPointer, FunctionValue, Global,
        InstructionValue, PointerValue, SealedAnyValue, Value,
    },
    Error, ErrorKind, Result,
};
use llvm_sys::{
    core::{
        LLVMAddCase, LLVMBuildAdd, LLVMBuildBitCast, LLVMBuildBr, LLVMBuildCall2, LLVMBuildFDiv,
        LLVMBuildGlobalString, LLVMBuildGlobalStringPtr, LLVMBuildMul, LLVMBuildPointerCast,
        LLVMBuildRet, LLVMBuildRetVoid, LLVMBuildSDiv, LLVMBuildSub, LLVMBuildSwitch,
        LLVMBuildUDiv, LLVMBuildUnreachable,
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
                UNNAMED_CSTR,
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
                UNNAMED_CSTR,
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
                UNNAMED_CSTR,
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
                UNNAMED_CSTR,
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
                UNNAMED_CSTR,
            ))?
        };

        Ok(mult)
    }

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
                UNNAMED_CSTR,
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
                UNNAMED_CSTR,
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
        let (value, ty, _kind) = match function.into() {
            FunctionOrPointer::Function(value) => {
                let ty = value.as_type()?;
                let kind = ty.element_type()?.kind();

                (value, ty, kind)
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

                (value, ty, kind)
            }
        };

        let mut args: Vec<*mut LLVMValue> =
            self.check_call(FunctionValue::from_val(value), args)?;

        unsafe {
            let value = LLVMBuildCall2(
                self.builder.as_mut_ptr(),
                ty.as_mut_ptr(),
                value.as_mut_ptr(),
                args.as_mut_ptr(),
                args.len() as u32,
                UNNAMED_CSTR,
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

    pub(crate) fn check_call<'a>(
        &self,
        function: FunctionValue<'ctx>,
        args: &'a [Value<'ctx>],
    ) -> Result<Vec<*mut LLVMValue>> {
        let mut function = function.as_type()?;
        while function.kind() == TypeKind::Pointer {
            function = function.element_type()?;
        }

        let param_types = FunctionSig::from_ty(function).args()?;
        let all_args_match = param_types
            .iter()
            .zip(args.iter().filter_map(|&v| v.as_type().ok()))
            .all(|(expected_ty, actual_ty)| *expected_ty == actual_ty);

        if all_args_match {
            return Ok(args.into_iter().map(|a| a.as_mut_ptr()).collect());
        }

        let casted_args: Vec<*mut LLVMValue> = param_types
            .into_iter()
            .zip(args.iter())
            .filter_map(|(expected_ty, &actual_val)| {
                let actual_ty = actual_val.as_type().ok()?;

                if expected_ty != actual_ty {
                    self.bitcast(actual_val, expected_ty)
                        .ok()
                        .map(|a| a.as_mut_ptr())
                } else {
                    Some(actual_val.as_mut_ptr())
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

#[cfg(test)]
#[allow(non_snake_case)]
mod tests {
    use crate::llvm::{module::Linkage, types::IntType, Context};

    #[test]
    fn addition() {
        let ctx = Context::new().unwrap();
        let module = ctx.module("main").unwrap();

        let I32 = IntType::i32(&ctx).unwrap();
        let sig = module
            .function_ty(I32.into(), &[I32.into(), I32.into()], false)
            .unwrap();

        let builder = module.build_function("main", sig).unwrap();
        builder.set_linkage(Linkage::External);

        let block = builder.append_block().unwrap();
        let add = block.add(builder.args()[0], builder.args()[1]).unwrap();
        block.ret(Some(add)).unwrap();
    }
}
