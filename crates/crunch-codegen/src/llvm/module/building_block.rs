use crate::llvm::{
    // instructions::instruction::{Add, ICmp, IntValue, Mul, Ret, SDiv, Sub, UDiv, Value, Valued},
    module::Builder,
    types::{IntType, Type, TypeKind},
    utils::{IntOperand, Wrapping, EMPTY_CSTR},
    values::{
        AnyValue, ArrayValue, BasicBlock, CallSiteValue, FunctionOrPointer, FunctionValue, Global,
        InstructionValue, PointerValue, SealedAnyValue, Value,
    },
    Error,
    ErrorKind,
    Result,
};
use core::{
    cmp::Ordering,
    fmt::{Debug, Formatter, Result as FmtResult},
    hash::{Hash, Hasher},
    iter::ExactSizeIterator,
    ops::Deref,
};
use llvm_sys::{
    core::{
        LLVMAddCase, LLVMBuildAdd, LLVMBuildBitCast, LLVMBuildBr, LLVMBuildCall2,
        LLVMBuildExactSDiv, LLVMBuildExactUDiv, LLVMBuildFDiv, LLVMBuildGlobalString,
        LLVMBuildGlobalStringPtr, LLVMBuildICmp, LLVMBuildMul, LLVMBuildNSWAdd, LLVMBuildNSWMul,
        LLVMBuildNSWSub, LLVMBuildNUWAdd, LLVMBuildNUWMul, LLVMBuildNUWSub, LLVMBuildPointerCast,
        LLVMBuildRet, LLVMBuildRetVoid, LLVMBuildSDiv, LLVMBuildSub, LLVMBuildSwitch,
        LLVMBuildUDiv, LLVMBuildUnreachable, LLVMConstAdd, LLVMConstExactSDiv, LLVMConstExactUDiv,
        LLVMConstICmp, LLVMConstMul, LLVMConstNSWAdd, LLVMConstNSWMul, LLVMConstNSWSub,
        LLVMConstNUWAdd, LLVMConstNUWMul, LLVMConstNUWSub, LLVMConstSDiv, LLVMConstSub,
        LLVMConstUDiv,
    },
    LLVMValue,
};
use std::ffi::CString;

pub struct BuildingBlock<'ctx> {
    block: BasicBlock<'ctx>,
    builder: Builder<'ctx>,
}

// Public interface
impl<'ctx> BuildingBlock<'ctx> {
    pub const fn basic_block(&self) -> BasicBlock<'ctx> {
        self.block
    }

    pub fn insert_instruction<V: Into<Value<'ctx>>>(&self, val: V) {
        unsafe {
            llvm_sys::core::LLVMInsertIntoBuilder(
                self.builder.as_mut_ptr(),
                val.into().as_mut_ptr(),
            );
        }
    }

    /// Add two integers together, returning the result
    ///
    /// Note: If both provided arguments are constants, then the `*ConstAdd` functions will be used
    ///
    /// [Docs](https://llvm.org/docs/LangRef.html#add-instruction)
    pub fn add<Left, Right>(&self, lhs: Left, rhs: Right) -> Result<Value<'ctx>>
    where
        Left: Into<Value<'ctx>>,
        Right: Into<Value<'ctx>>,
    {
        self.add_flagged(lhs, rhs, Wrapping::None)
    }

    /// Add two integers together using [flags], returning the result
    ///
    /// Note: If both provided arguments are constants, then the `*ConstAdd` functions will be used
    ///
    /// [Docs](https://llvm.org/docs/LangRef.html#add-instruction)
    ///
    /// [flags]: https://llvm.org/docs/LangRef.html#id86
    // FIXME: https://github.com/paholg/typenum/issues/154
    pub fn add_flagged<Left, Right>(
        &self,
        lhs: Left,
        rhs: Right,
        flags: Wrapping,
    ) -> Result<Value<'ctx>>
    where
        Left: Into<Value<'ctx>>,
        Right: Into<Value<'ctx>>,
    {
        let (lhs, rhs) = (lhs.into(), rhs.into());
        let is_const = lhs.is_const() && rhs.is_const();

        unsafe {
            let (lhs, rhs) = (lhs.as_mut_ptr(), rhs.as_mut_ptr());
            let builder = self.builder.as_mut_ptr();

            let add = match flags {
                Wrapping::NoSignedWrap => {
                    if is_const {
                        LLVMConstNSWAdd(lhs, rhs)
                    } else {
                        LLVMBuildNSWAdd(builder, lhs, rhs, EMPTY_CSTR)
                    }
                }

                Wrapping::NoUnsignedWrap => {
                    if is_const {
                        LLVMConstNUWAdd(lhs, rhs)
                    } else {
                        LLVMBuildNUWAdd(builder, lhs, rhs, EMPTY_CSTR)
                    }
                }

                Wrapping::None => {
                    if is_const {
                        LLVMConstAdd(lhs, rhs)
                    } else {
                        LLVMBuildAdd(builder, lhs, rhs, EMPTY_CSTR)
                    }
                }
            };

            Value::from_raw(add)
        }
    }

    /// Subtract two integers, returning the result
    ///
    /// Note: If both provided arguments are constants, then the `*ConstSub` functions will be used
    ///
    /// [Docs](https://llvm.org/docs/LangRef.html#sub-instruction)
    // FIXME: https://github.com/paholg/typenum/issues/154
    pub fn sub<Left, Right>(&self, lhs: Left, rhs: Right) -> Result<Value<'ctx>>
    where
        Left: Into<Value<'ctx>>,
        Right: Into<Value<'ctx>>,
    {
        self.sub_flagged(lhs, rhs, Wrapping::None)
    }

    /// Subtract two integers using [flags], returning the result
    ///
    /// Note: If both provided arguments are constants, then the `*ConstSub` functions will be used
    ///
    /// [Docs](https://llvm.org/docs/LangRef.html#sub-instruction)
    ///
    /// [flags]: https://llvm.org/docs/LangRef.html#id86
    // FIXME: https://github.com/paholg/typenum/issues/154
    pub fn sub_flagged<Left, Right>(
        &self,
        lhs: Left,
        rhs: Right,
        flags: Wrapping,
    ) -> Result<Value<'ctx>>
    where
        Left: Into<Value<'ctx>>,
        Right: Into<Value<'ctx>>,
    {
        let (lhs, rhs) = (lhs.into(), rhs.into());
        let is_const = lhs.is_const() && rhs.is_const();

        unsafe {
            let (lhs, rhs) = (lhs.as_mut_ptr(), rhs.as_mut_ptr());
            let builder = self.builder.as_mut_ptr();

            let sub = match flags {
                Wrapping::NoSignedWrap => {
                    if is_const {
                        LLVMConstNSWSub(lhs, rhs)
                    } else {
                        LLVMBuildNSWSub(builder, lhs, rhs, EMPTY_CSTR)
                    }
                }

                Wrapping::NoUnsignedWrap => {
                    if is_const {
                        LLVMConstNUWSub(lhs, rhs)
                    } else {
                        LLVMBuildNUWSub(builder, lhs, rhs, EMPTY_CSTR)
                    }
                }

                Wrapping::None => {
                    if is_const {
                        LLVMConstSub(lhs, rhs)
                    } else {
                        LLVMBuildSub(builder, lhs, rhs, EMPTY_CSTR)
                    }
                }
            };

            Value::from_raw(sub)
        }
    }

    /// Multiply two integers, returning the result
    ///
    /// Note: If both provided arguments are constants, then the `*ConstMul` functions will be used
    ///
    /// [Docs](https://llvm.org/docs/LangRef.html#mul-instruction)
    // FIXME: https://github.com/paholg/typenum/issues/154
    pub fn mul<Left, Right>(&self, lhs: Left, rhs: Right) -> Result<Value<'ctx>>
    where
        Left: Into<Value<'ctx>>,
        Right: Into<Value<'ctx>>,
    {
        self.mul_flagged(lhs, rhs, Wrapping::None)
    }

    /// Multiply two integers using [flags], returning the result
    ///
    /// Note: If both provided arguments are constants, then the `*ConstMul` functions will be used
    ///
    /// [Docs](https://llvm.org/docs/LangRef.html#mul-instruction)
    ///
    /// [flags]: https://llvm.org/docs/LangRef.html#id86
    // FIXME: https://github.com/paholg/typenum/issues/154
    pub fn mul_flagged<Left, Right>(
        &self,
        lhs: Left,
        rhs: Right,
        flags: Wrapping,
    ) -> Result<Value<'ctx>>
    where
        Left: Into<Value<'ctx>>,
        Right: Into<Value<'ctx>>,
    {
        let (lhs, rhs) = (lhs.into(), rhs.into());
        let is_const = lhs.is_const() && rhs.is_const();

        unsafe {
            let (lhs, rhs) = (lhs.as_mut_ptr(), rhs.as_mut_ptr());
            let builder = self.builder.as_mut_ptr();

            let mul = match flags {
                Wrapping::NoSignedWrap => {
                    if is_const {
                        LLVMConstNSWMul(lhs, rhs)
                    } else {
                        LLVMBuildNSWMul(builder, lhs, rhs, EMPTY_CSTR)
                    }
                }

                Wrapping::NoUnsignedWrap => {
                    if is_const {
                        LLVMConstNUWMul(lhs, rhs)
                    } else {
                        LLVMBuildNUWMul(builder, lhs, rhs, EMPTY_CSTR)
                    }
                }

                Wrapping::None => {
                    if is_const {
                        LLVMConstMul(lhs, rhs)
                    } else {
                        LLVMBuildMul(builder, lhs, rhs, EMPTY_CSTR)
                    }
                }
            };

            Value::from_raw(mul)
        }
    }

    /// Divide two unsigned integers, returning the result
    ///
    /// Note: If both provided arguments are constants, then the `*ConstUDiv` function will be used
    ///
    /// [Docs](https://llvm.org/docs/LangRef.html#udiv-instruction)
    // FIXME: https://github.com/paholg/typenum/issues/154
    pub fn udiv<Left, Right>(&self, lhs: Left, rhs: Right) -> Result<Value<'ctx>>
    where
        Left: Into<Value<'ctx>>,
        Right: Into<Value<'ctx>>,
    {
        let (lhs, rhs) = (lhs.into(), rhs.into());
        let is_const = lhs.is_const() && rhs.is_const();

        unsafe {
            let (lhs, rhs) = (lhs.as_mut_ptr(), rhs.as_mut_ptr());
            let builder = self.builder.as_mut_ptr();

            let udiv = if is_const {
                LLVMConstUDiv(lhs, rhs)
            } else {
                LLVMBuildUDiv(builder, lhs, rhs, EMPTY_CSTR)
            };

            Value::from_raw(udiv)
        }
    }

    /// Divide two unsigned integers with the `exact` flag, returning the result
    ///
    /// Note: If both provided arguments are constants, then the `*ConstUDiv` function will be used
    ///
    /// [Docs](https://llvm.org/docs/LangRef.html#udiv-instruction)
    // FIXME: https://github.com/paholg/typenum/issues/154
    pub fn udiv_exact<Left, Right>(&self, lhs: Left, rhs: Right) -> Result<Value<'ctx>>
    where
        Left: Into<Value<'ctx>>,
        Right: Into<Value<'ctx>>,
    {
        let (lhs, rhs) = (lhs.into(), rhs.into());
        let is_const = lhs.is_const() && rhs.is_const();

        unsafe {
            let (lhs, rhs) = (lhs.as_mut_ptr(), rhs.as_mut_ptr());
            let builder = self.builder.as_mut_ptr();

            let udiv = if is_const {
                LLVMConstExactUDiv(lhs, rhs)
            } else {
                LLVMBuildExactUDiv(builder, lhs, rhs, EMPTY_CSTR)
            };

            Value::from_raw(udiv)
        }
    }

    /// Divide two signed integers, returning the result
    ///
    /// Note: If both provided arguments are constants, then the `ConstSDiv` function will be used
    ///
    /// [Docs](https://llvm.org/docs/LangRef.html#udiv-instruction)
    // FIXME: https://github.com/paholg/typenum/issues/154
    pub fn sdiv<Left, Right>(&self, lhs: Left, rhs: Right) -> Result<Value<'ctx>>
    where
        Left: Into<Value<'ctx>>,
        Right: Into<Value<'ctx>>,
    {
        let (lhs, rhs) = (lhs.into(), rhs.into());
        let is_const = lhs.is_const() && rhs.is_const();

        unsafe {
            let (lhs, rhs) = (lhs.as_mut_ptr(), rhs.as_mut_ptr());
            let builder = self.builder.as_mut_ptr();

            let udiv = if is_const {
                LLVMConstSDiv(lhs, rhs)
            } else {
                LLVMBuildSDiv(builder, lhs, rhs, EMPTY_CSTR)
            };

            Value::from_raw(udiv)
        }
    }

    /// Divide two signed integers with the `exact` flag, returning the result
    ///
    /// Note: If both provided arguments are constants, then the `ConstSDivExact` function will be used
    ///
    /// [Docs](https://llvm.org/docs/LangRef.html#sdiv-instruction)
    // FIXME: https://github.com/paholg/typenum/issues/154
    pub fn sdiv_exact<Left, Right>(&self, lhs: Left, rhs: Right) -> Result<Value<'ctx>>
    where
        Left: Into<Value<'ctx>>,
        Right: Into<Value<'ctx>>,
    {
        let (lhs, rhs) = (lhs.into(), rhs.into());
        let is_const = lhs.is_const() && rhs.is_const();

        unsafe {
            let (lhs, rhs) = (lhs.as_mut_ptr(), rhs.as_mut_ptr());
            let builder = self.builder.as_mut_ptr();

            let udiv = if is_const {
                LLVMConstExactSDiv(lhs, rhs)
            } else {
                LLVMBuildExactSDiv(builder, lhs, rhs, EMPTY_CSTR)
            };

            Value::from_raw(udiv)
        }
    }

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

    /// Return a value from the current function
    ///
    /// If a value is provided, that will be returned. If the value is `None`,
    /// then `ret void` will be inserted.
    ///
    /// [Docs](https://llvm.org/docs/LangRef.html#ret-instruction)
    // TODO: Take in a returnable value
    // TODO: Verify return type is correct for the current function
    pub fn ret(&self, value: Option<Value<'ctx>>) -> Result<InstructionValue<'ctx>> {
        let ret = unsafe {
            let ret = if let Some(value) = value {
                LLVMBuildRet(self.builder.as_mut_ptr(), value.as_mut_ptr())
            } else {
                LLVMBuildRetVoid(self.builder.as_mut_ptr())
            };

            InstructionValue::from_raw(ret)?
        };

        Ok(ret)
    }

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

    pub fn unreachable(&self) -> Result<InstructionValue<'ctx>> {
        let unreachable =
            unsafe { InstructionValue::from_raw(LLVMBuildUnreachable(self.builder.as_mut_ptr()))? };

        debug_assert!(unreachable.is_unreachable());

        Ok(unreachable)
    }

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

    pub fn switch<J>(
        &self,
        condition: Value<'ctx>,
        default: BasicBlock<'ctx>,
        jumps: J,
    ) -> Result<InstructionValue<'ctx>>
    where
        J: ExactSizeIterator + Iterator<Item = (Value<'ctx>, BasicBlock<'ctx>)>,
    {
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
                    block.as_mut_ptr(),
                );
            }
        }

        Ok(switch)
    }

    pub fn call<F, A>(&self, function: F, args: A) -> Result<CallSiteValue<'ctx>>
    where
        F: Into<FunctionOrPointer<'ctx>>,
        A: Iterator<Item = Value<'ctx>>,
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

    pub fn ptr_cast(&self, value: Value<'ctx>, cast_ty: Type<'ctx>) -> Result<Value<'ctx>> {
        unsafe {
            let value = LLVMBuildPointerCast(
                self.builder.as_mut_ptr(),
                value.as_mut_ptr(),
                cast_ty.as_mut_ptr(),
                EMPTY_CSTR,
            );

            Value::from_raw(value)
        }
    }

    pub fn icmp<Left, Right>(
        &self,
        lhs: Left,
        operand: IntOperand,
        rhs: Right,
    ) -> Result<Value<'ctx>>
    where
        Left: Into<Value<'ctx>>,
        Right: Into<Value<'ctx>>,
    {
        let (lhs, rhs) = (lhs.into(), rhs.into());
        let is_const = lhs.is_const() && rhs.is_const();

        unsafe {
            let (lhs, rhs) = (lhs.as_mut_ptr(), rhs.as_mut_ptr());
            let builder = self.builder.as_mut_ptr();

            let icmp = if is_const {
                LLVMConstICmp(operand.into(), lhs, rhs)
            } else {
                LLVMBuildICmp(builder, operand.into(), lhs, rhs, EMPTY_CSTR)
            };

            Value::from_raw(icmp)
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

    pub(crate) fn cast_call_args<V>(
        &self,
        values: V,
        types: &[Type<'ctx>],
    ) -> Result<Vec<*mut LLVMValue>>
    where
        V: Iterator<Item = Value<'ctx>>,
    {
        let casted_args: Vec<*mut LLVMValue> = types
            .iter()
            .zip(values)
            .map(|(expected_ty, val)| {
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
