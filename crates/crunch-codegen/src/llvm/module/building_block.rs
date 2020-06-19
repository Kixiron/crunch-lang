use crate::llvm::{
    module::Builder,
    types::{FunctionSig, IntType, Type, TypeKind},
    utils::UNNAMED,
    values::{
        AnyValue, ArrayValue, BasicBlock, CallSiteValue, FunctionOrPointer, FunctionValue, Global,
        Instruction, PointerValue, SealedAnyValue, Value,
    },
    Error, ErrorKind, Result,
};
use llvm_sys::{
    core::{
        LLVMBuildAdd, LLVMBuildBitCast, LLVMBuildBr, LLVMBuildCall, LLVMBuildGlobalString,
        LLVMBuildGlobalStringPtr, LLVMBuildPointerCast, LLVMBuildRet, LLVMBuildRetVoid,
        LLVMBuildSub, LLVMBuildUnreachable,
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
        name: &str,
    ) -> Result<Value<'ctx>> {
        let cname = CString::new(name).expect("Rust strings cannot have null bytes");
        self.builder.move_to_end(self);

        let add = unsafe {
            Value::from_raw(LLVMBuildAdd(
                self.builder.as_mut_ptr(),
                lhs.into().as_mut_ptr(),
                rhs.into().as_mut_ptr(),
                cname.as_ptr(),
            ))?
        };

        Ok(add)
    }

    // TODO: Take in subtractable values
    pub fn sub(
        &self,
        lhs: impl Into<Value<'ctx>>,
        rhs: impl Into<Value<'ctx>>,
        name: &str,
    ) -> Result<Value<'ctx>> {
        let cname = CString::new(name).expect("Rust strings cannot have null bytes");
        self.builder.move_to_end(self);

        let sub = unsafe {
            Value::from_raw(LLVMBuildSub(
                self.builder.as_mut_ptr(),
                lhs.into().as_mut_ptr(),
                rhs.into().as_mut_ptr(),
                cname.as_ptr(),
            ))?
        };

        Ok(sub)
    }

    // TODO: Take in a returnable value
    #[inline]
    pub fn ret(&self, value: Option<Value<'ctx>>) -> Result<Instruction<'ctx>> {
        self.builder.move_to_end(self);

        // TODO: Verify return type is correct

        let ret = unsafe {
            let ret = if let Some(value) = value {
                LLVMBuildRet(self.builder.as_mut_ptr(), value.as_mut_ptr())
            } else {
                LLVMBuildRetVoid(self.builder.as_mut_ptr())
            };

            Instruction::from_raw(ret)?
        };

        debug_assert!(ret.is_return());

        Ok(ret)
    }

    #[inline]
    pub fn branch(&self, block: impl AsRef<BasicBlock<'ctx>>) -> Result<Instruction<'ctx>> {
        self.builder.move_to_end(self);

        let branch = unsafe {
            Instruction::from_raw(LLVMBuildBr(
                self.builder.as_mut_ptr(),
                (*block.as_ref()).as_mut_ptr(),
            ))?
        };

        debug_assert!(branch.is_branch());

        Ok(branch)
    }

    #[inline]
    pub fn unreachable(&self) -> Result<Instruction<'ctx>> {
        self.builder.move_to_end(self);

        let unreachable =
            unsafe { Instruction::from_raw(LLVMBuildUnreachable(self.builder.as_mut_ptr()))? };

        debug_assert!(unreachable.is_unreachable());

        Ok(unreachable)
    }

    pub fn bitcast(&self, value: Value<'ctx>, dest_ty: Type<'ctx>) -> Result<Value<'ctx>> {
        unsafe {
            Value::from_raw(LLVMBuildBitCast(
                self.builder.as_mut_ptr(),
                value.as_mut_ptr(),
                dest_ty.as_mut_ptr(),
                UNNAMED,
            ))
        }
    }

    pub fn call<F>(
        &self,
        function: F,
        args: &[Value<'ctx>],
        name: &str,
    ) -> Result<CallSiteValue<'ctx>>
    where
        F: Into<FunctionOrPointer<'ctx>>,
    {
        dbg!();
        let (value, ty, kind) = match function.into() {
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

        let name = if let TypeKind::Void = kind { "" } else { name };
        let c_string = CString::new(name)?;

        let mut args: Vec<*mut LLVMValue> =
            self.check_call(FunctionValue::from_val(value), args)?;

        unsafe {
            let value = LLVMBuildCall(
                self.builder.as_mut_ptr(),
                value.as_mut_ptr(),
                args.as_mut_ptr(),
                args.len() as u32,
                c_string.as_ptr(),
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

        let param_types = unsafe { FunctionSig::from(function) }.args()?;

        let all_args_match = dbg!(&param_types)
            .iter()
            .zip(args.iter().filter_map(|&v| dbg!(v.as_type()).ok()))
            .all(|(expected_ty, actual_ty)| *dbg!(expected_ty) == dbg!(actual_ty));

        if all_args_match {
            return Ok(args.into_iter().map(|a| a.as_mut_ptr()).collect());
        }

        let casted_args: Vec<*mut LLVMValue> = param_types
            .into_iter()
            .zip(args.iter())
            .filter_map(|(expected_ty, &actual_val)| {
                dbg!(expected_ty);
                let actual_ty = dbg!(actual_val.as_type().ok()?);

                if expected_ty != actual_ty {
                    dbg!();
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

    pub(crate) const fn builder(&self) -> &Builder<'ctx> {
        &self.builder
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

        let _function = module
            .build_function("main", sig, |builder| {
                builder.set_linkage(Linkage::External);

                let block = builder.append_block("entry")?;
                let add = block.add(builder.args()[0], builder.args()[1], "add")?;
                block.ret(Some(add))?;

                Ok(())
            })
            .unwrap();
    }
}
