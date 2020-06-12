use crate::llvm::{
    module::Builder,
    values::{AnyValue, BasicBlock, Instruction, SealedAnyValue, Value},
    Result,
};
use llvm_sys::core::{
    LLVMBuildAdd, LLVMBuildBr, LLVMBuildRet, LLVMBuildRetVoid, LLVMBuildSub, LLVMBuildUnreachable,
};
use std::{
    cmp::Ordering,
    ffi::CString,
    fmt::{Debug, Formatter, Result as FmtResult},
    hash::{Hash, Hasher},
    ops::Deref,
};

#[derive(Copy, Clone)]
pub struct BuildingBlock<'ctx> {
    block: BasicBlock<'ctx>,
    builder: &'ctx Builder<'ctx>,
}

// Public interface
impl<'ctx> BuildingBlock<'ctx> {
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
}

// Private interface
impl<'ctx> BuildingBlock<'ctx> {
    pub(crate) const fn new(block: BasicBlock<'ctx>, builder: &'ctx Builder<'ctx>) -> Self {
        Self { block, builder }
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
    use crate::llvm::{module::Linkage, types::IntTy, Context};

    #[test]
    fn addition() {
        let ctx = Context::new().unwrap();
        let module = ctx.module("main").unwrap();

        let I32 = IntTy::i32(&ctx).unwrap();
        let sig = module.function_ty(I32, &[I32, I32], false).unwrap();

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
