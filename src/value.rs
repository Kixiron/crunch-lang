use super::{Gc, GcBigInt, GcBigUint, GcStr, GcVec, Result, RuntimeError, RuntimeErrorTy};
use std::fmt;

// TODO: Test all implemented operations

#[derive(Debug, Clone, Copy)]
pub enum RuntimeValue {
    // Unsigned integers
    Byte(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    U128(u128),
    GcUint(GcBigUint),

    // Signed integers
    IByte(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    I128(i128),
    GcInt(GcBigInt),

    // Floats
    F32(f32),
    F64(f64),

    // Strings
    Char(char),
    Str(&'static str),
    GcString(GcStr),

    // Boolean
    Bool(bool),
    // Pointer
    // Should this be a u32, u64 or a usize?
    Pointer(u64),
    // Vec
    GcVec(GcVec<RuntimeValue>),
    // Null
    Null,

    None,
}

impl RuntimeValue {
    pub fn name(&self) -> &'static str {
        match self {
            Self::Byte(_) => "byte",
            Self::U16(_) => "uint16",
            Self::U32(_) => "uint",
            Self::U64(_) => "uint64",
            Self::U128(_) => "uint128",
            Self::IByte(_) => "ibyte",
            Self::I16(_) => "int16",
            Self::I32(_) => "int",
            Self::I64(_) => "int64",
            Self::I128(_) => "int128",
            Self::F32(_) => "float",
            Self::F64(_) => "float64",
            Self::Bool(_) => "bool",
            Self::Pointer(_) => "ptr",
            Self::Char(_) => "char",
            Self::GcString(_) | Self::Str(_) => "str",
            Self::GcInt(_) => "bigint",
            Self::GcUint(_) => "biguint",
            Self::GcVec(_) => "vec",
            Self::Null => "null",
            Self::None => "NoneType",
        }
    }

    // TODO: Add similar-type eq
    pub fn is_equal(self, other: Self, gc: &Gc) -> Result<bool> {
        Ok(match (self, other) {
            (Self::Byte(left), Self::Byte(right)) => left == right,
            (Self::U16(left), Self::U16(right)) => left == right,
            (Self::U32(left), Self::U32(right)) => left == right,
            (Self::U64(left), Self::U64(right)) => left == right,
            (Self::U128(left), Self::U128(right)) => left == right,
            (Self::GcUint(left), Self::GcUint(right)) => {
                *left.to_uint(&gc)? == *right.to_uint(&gc)?
            }

            (Self::IByte(left), Self::IByte(right)) => left == right,
            (Self::I16(left), Self::I16(right)) => left == right,
            (Self::I32(left), Self::I32(right)) => left == right,
            (Self::I64(left), Self::I64(right)) => left == right,
            (Self::I128(left), Self::I128(right)) => left == right,
            (Self::GcInt(left), Self::GcInt(right)) => *left.to_int(&gc)? == *right.to_int(&gc)?,

            (Self::F32(_left), Self::F32(_right)) => unimplemented!("No idea how floats work"),
            (Self::F64(_left), Self::F64(_right)) => unimplemented!("No idea how floats work"),

            (Self::Pointer(left), Self::Pointer(right)) => left == right,

            (left, right) | (left, right) if left == Self::None || right == Self::None => {
                return Err(RuntimeError {
                    ty: RuntimeErrorTy::NullVar,
                    message: format!(
                        "Values of types '{}' and '{}' cannot be equal",
                        left.name(),
                        right.name()
                    ),
                });
            }
            (_, _) => false,
        })
    }

    pub fn to_string(&self, gc: &Gc) -> Result<String> {
        Ok(match self {
            Self::Byte(int) => int.to_string(),
            Self::U16(int) => int.to_string(),
            Self::U32(int) => int.to_string(),
            Self::U64(int) => int.to_string(),
            Self::U128(int) => int.to_string(),
            Self::IByte(int) => int.to_string(),
            Self::I16(int) => int.to_string(),
            Self::I32(int) => int.to_string(),
            Self::I64(int) => int.to_string(),
            Self::I128(int) => int.to_string(),
            Self::F32(int) => int.to_string(),
            Self::F64(int) => int.to_string(),
            Self::Bool(int) => int.to_string(),
            Self::Pointer(int) => format!("{:p}", int as *const _),
            Self::Char(c) => c.to_string(),
            Self::GcString(string) => string.to_str(&gc)?.to_string(),
            Self::Str(string) => string.to_string(),
            Self::GcInt(int) => int.to_int(&gc)?.to_string(),
            Self::GcUint(int) => int.to_uint(&gc)?.to_string(),
            Self::GcVec(vec) => format!("{:?}", *vec.to_vec(&gc)?),
            Self::Null => "null".to_string(),
            Self::None => "NoneType".to_string(),
        })
    }

    pub fn from_bytes(
        _bytes: &[u8],
        _strings: &mut std::collections::VecDeque<String>,
    ) -> Result<Self> {
        unimplemented!()
    }

    pub fn as_bytes(&self) -> (Vec<u8>, Option<String>) {
        unimplemented!()
    }
}

macro_rules! upflowing {
    ($ty:ty, $([$name:tt, $func:tt, $func_two:tt, $func_three:tt]),*) => {
        impl $ty {
            $(
                pub fn $name(self, other: Self, gc: &mut Gc) -> Result<Self> {
                    Ok(match (self, other) {
                        (Self::Byte(left), Self::Byte(right)) => {
                            if let Some(result) = left.$func(right) {
                                Self::Byte(result)
                            } else {
                                Self::U16(left as u16).$name(Self::U16(right as u16), gc)?
                            }
                        }
                        (Self::U16(left), Self::U16(right)) => {
                            if let Some(result) = left.$func(right) {
                                Self::U16(result)
                            } else {
                                Self::U32(left as u32).$name(Self::U32(right as u32), gc)?
                            }
                        }
                        (Self::U32(left), Self::U32(right)) => {
                            if let Some(result) = left.$func(right) {
                                Self::U32(result)
                            } else {
                                Self::U64(left as u64).$name(Self::U64(right as u64), gc)?
                            }
                        }
                        (Self::U64(left), Self::U64(right)) => {
                            if let Some(result) = left.$func(right) {
                                Self::U64(result)
                            } else {
                                Self::U128(left as u128).$name(Self::U128(right as u128), gc)?
                            }
                        }
                        (Self::U128(left), Self::U128(right)) => {
                            if let Some(result) = left.$func(right) {
                                Self::U128(result)
                            } else {
                                Self::GcInt(GcBigInt::$func_three(left, right, gc)?)
                            }
                        }
                        (Self::GcUint(left), Self::GcUint(right)) => Self::GcUint(left.$func_two(right, gc)?),

                        (Self::IByte(left), Self::IByte(right)) => {
                            if let Some(result) = left.$func(right) {
                                Self::IByte(result)
                            } else {
                                Self::I16(left as i16).$name(Self::I16(right as i16), gc)?
                            }
                        }
                        (Self::I16(left), Self::I16(right)) => {
                            if let Some(result) = left.$func(right) {
                                Self::I16(result)
                            } else {
                                Self::I32(left as i32).$name(Self::I32(right as i32), gc)?
                            }
                        }
                        (Self::I32(left), Self::I32(right)) => {
                            if let Some(result) = left.$func(right) {
                                Self::I32(result)
                            } else {
                                Self::I64(left as i64).$name(Self::I64(right as i64), gc)?
                            }
                        }
                        (Self::I64(left), Self::I64(right)) => {
                            if let Some(result) = left.$func(right) {
                                Self::I64(result)
                            } else {
                                Self::I128(left as i128).$name(Self::I128(right as i128), gc)?
                            }
                        }
                        (Self::I128(left), Self::I128(right)) => {
                            if let Some(result) = left.$func(right) {
                                Self::I128(result)
                            } else {
                                Self::GcInt(GcBigInt::$func_three(left, right, gc)?)
                            }
                        }
                        (Self::GcInt(left), Self::GcInt(right)) => Self::GcInt(left.$func_two(right, gc)?),

                        (Self::F32(_left), Self::F32(_right)) => unimplemented!("No idea how floats work"),
                        (Self::F64(_left), Self::F64(_right)) => unimplemented!("No idea how floats work"),

                        (Self::Pointer(left), Self::Pointer(right)) => {
                            if let Some(ptr) = left.$func(right) {
                                Self::Pointer(ptr)
                            } else {
                                return Err(RuntimeError {
                                    ty: RuntimeErrorTy::IntegerOverflow,
                                    message: format!(
                                        "The attempted subtract is too large to fit in a '{}'",
                                        self.name()
                                    ),
                                });
                            }
                        }

                        (left, right) | (left, right) if left == Self::None || right == Self::None => {
                            return Err(RuntimeError {
                                ty: RuntimeErrorTy::NullVar,
                                message: format!(
                                    "Values of types '{}' and '{}' cannot be subtracted",
                                    left.name(),
                                    right.name()
                                ),
                            });
                        }
                        (left, right) => {
                            return Err(RuntimeError {
                                ty: RuntimeErrorTy::IncompatibleTypes,
                                message: format!(
                                    "Values of types '{}' and '{}' cannot be subtracted",
                                    left.name(),
                                    right.name()
                                ),
                            });
                        }
                    })
                }
            )*
        }
    }
}

upflowing!(
    RuntimeValue,
    [add_upflowing, checked_add, add, new_adding],
    [sub_upflowing, checked_sub, sub, new_subtracting],
    [mult_upflowing, checked_mul, mult, new_multiplying],
    [div_upflowing, checked_div, div, new_dividing]
);

impl PartialEq for RuntimeValue {
    fn eq(&self, other: &Self) -> bool {
        use std::mem::discriminant;

        discriminant(self) == discriminant(other)
    }
}

impl Eq for RuntimeValue {}

impl fmt::Display for RuntimeValue {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.write_str(self.name())
    }
}

/*
#[derive(Debug, Clone, Copy)]
pub enum RuntimeValue {
    Cached(CachedValue),
    Register(RegisterValue),
    None,
}

impl std::fmt::Display for RuntimeValue {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Cached(inner) => write!(fmt, "{}", inner),
            Self::Register(inner) => write!(fmt, "{}", inner),
            Self::None => write!(fmt, "NoneType"),
        }
    }
}

impl RuntimeValue {
    pub fn fetch(&self, gc: &Gc) -> Result<RegisterValue> {
        match self {
            Self::Cached(cached) => cached.get_val(&gc),
            Self::Register(register) => Ok(register),
            Self::None => Err(RuntimeError {
                ty: RuntimeErrorTy::NullVar,
                message: "The requested variable does not exist".to_string(),
            }),
        }
    }

    pub fn from_val(val: Value) -> Self {
        if val == Value::None {
            Self::None
        } else {
            Self::Register(RegisterValue::from_val(val))
        }
    }

    pub fn eq(&self, other: &Self, gc: &Gc) -> Result<bool> {
        Ok(match (self, other) {
            (Self::Register(left), Self::Register(right)) => left == right,
            (Self::Cached(left), Self::Cached(right)) => left.eq(right, gc)?,
            (Self::Cached(cached), Self::Register(reg))
            | (Self::Register(reg), Self::Cached(cached)) => cached.eq_reg(reg, gc)?,
            (Self::None, Self::None) => true,
            (_, _) => false,
        })
    }

    pub fn not(self, gc: &Gc) -> Result<Self> {
        match self {
            Self::Cached(c) => Ok(Self::Cached(c.not(gc)?)),
            Self::Register(r) => Ok(Self::Register((!r)?)),
            Self::None => Err(RuntimeError {
                ty: RuntimeErrorTy::IncompatibleTypes,
                message: "Cannot apply not NoneType".to_string(),
            }),
        }
    }

    pub fn cmp(&self, other: &Self, gc: &Gc) -> Result<Option<std::cmp::Ordering>> {
        Ok(match (self, other) {
            (Self::Cached(l), Self::Cached(r)) => l.cmp(r, &gc)?,
            (Self::Register(l), Self::Register(r)) => l.partial_cmp(&r),
            (Self::Cached(l), Self::Register(r)) => l.get_val(&gc)?.partial_cmp(&r),
            (Self::Register(l), Self::Cached(r)) => l.partial_cmp(&r.get_val(&gc)?),
            (Self::None, Self::None) => None,
            (_, _) => {
                return Err(RuntimeError {
                    ty: RuntimeErrorTy::IncompatibleTypes,
                    message: "Those two types cannot be operated on".to_string(),
                })
            }
        })
    }

    pub fn is_register(&self) -> bool {
        if let Self::Register(_) = self {
            true
        } else {
            false
        }
    }

    pub fn is_cached(&self) -> bool {
        if let Self::Cached(_) = self {
            true
        } else {
            false
        }
    }

    pub fn is_none(&self) -> bool {
        if let Self::None = self {
            true
        } else {
            false
        }
    }

    pub fn register(&self) -> Option<RegisterValue> {
        if let Self::Register(r) = self {
            Some(r)
        } else {
            None
        }
    }

    pub fn cached(&self) -> Option<CachedValue> {
        if let Self::Cached(c) = self {
            Some(c)
        } else {
            None
        }
    }
}

macro_rules! generate_op {
    ($( [$t:ty, $token:tt, $name:tt] ),) => {
        $(
            impl $t {
                pub fn $name(&self, other: &Self, gc: &Gc) -> Result<Self> {
                    Ok(Self::Register(match (self, other) {
                        (Self::Cached(l), Self::Cached(r)) => (l.get_val(&gc)? $token r.get_val(&gc)?)?,
                        (Self::Register(l), Self::Register(r)) => (l $token r)?,
                        (Self::Cached(l), Self::Register(r)) => (l.get_val(&gc)? $token r)?,
                        (Self::Register(l), Self::Cached(r)) => (l $token r.get_val(&gc)?)?,
                        (Self::None, Self::None) => return Ok(Self::None),
                        (_, _) => return Err(RuntimeError {
                            ty: RuntimeErrorTy::IncompatibleTypes,
                            message: "Those two types cannot be operated on".to_string(),
                        })
                    }))
                }
            }
        )
    }
}

generate_op! {
    [RuntimeValue, +, add],
    [RuntimeValue, -, sub],
    [RuntimeValue, , mult],
    [RuntimeValue, /, div],
    [RuntimeValue, |, bit_or],
    [RuntimeValue, ^, bit_xor],
    [RuntimeValue, &, bit_and]
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RegisterValue {
    String(&'static str),
    Int(i32),
    Bool(bool),
    Pointer(usize),
}

impl std::fmt::Display for RegisterValue {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::String(inner) => write!(fmt, "{}", inner),
            Self::Int(inner) => write!(fmt, "{}", inner),
            Self::Bool(inner) => write!(fmt, "{}", inner),
            Self::Pointer(inner) => write!(fmt, "{:#p}", inner),
        }
    }
}

impl RegisterValue {
    #[inline]
    pub fn ty(&self) -> &'static str {
        match self {
            Self::Int(_) => "int",
            Self::Bool(_) => "bool",
            Self::String(_) => "str",
            Self::Pointer(_) => "ptr",
        }
    }

    pub fn from_val(val: Value) -> Self {
        match val {
            Value::Int(i) => Self::Int(i),
            Value::Bool(b) => Self::Bool(b),
            Value::String(s) => Self::String(unsafe { std::mem::transmute(&s) }),
            Value::Pointer(p) => Self::Pointer(p),
            _ => unreachable!(),
        }
    }

    pub fn to_value(self) -> Value {
        match self {
            Self::Int(i) => Value::Int(i),
            Self::Bool(b) => Value::Bool(b),
            Self::String(s) => Value::String(s.to_string()),
            Self::Pointer(p) => Value::Pointer(p),
        }
    }
}

impl ops::Add for RegisterValue {
    type Output = Result<Self>;

    #[inline]
    fn add(self, other: Self) -> Self::Output {
        match (self, other) {
            (Self::Int(left), Self::Int(right)) => Ok(Self::Int(left + right)),
            (Self::String(left), Self::String(right)) => {
                let mut output = Vec::with_capacity(left.as_bytes().len() + right.as_bytes().len());
                output.extend_from_slice(left.as_bytes());
                output.extend_from_slice(right.as_bytes());

                Ok(Self::String(unsafe {
                    std::mem::transmute(std::str::from_utf8_unchecked(&output))
                }))
            }

            (left, right) => Err(RuntimeError {
                ty: RuntimeErrorTy::IncompatibleTypes,
                message: format!(
                    "Cannot add variables of types {} and {}",
                    left.ty(),
                    right.ty()
                ),
            }),
        }
    }
}

impl ops::Sub for RegisterValue {
    type Output = Result<Self>;

    #[inline]
    fn sub(self, other: Self) -> Self::Output {
        match (self, other) {
            (Self::Int(left), Self::Int(right)) => Ok(Self::Int(left - right)),

            (left, right) => Err(RuntimeError {
                ty: RuntimeErrorTy::IncompatibleTypes,
                message: format!(
                    "Cannot subtract variables of types {} and {}",
                    left.ty(),
                    right.ty()
                ),
            }),
        }
    }
}

impl ops::Mul for RegisterValue {
    type Output = Result<Self>;

    #[inline]
    fn mul(self, other: Self) -> Self::Output {
        match (self, other) {
            (Self::Int(left), Self::Int(right)) => Ok(Self::Int(left  right)),

            (left, right) => Err(RuntimeError {
                ty: RuntimeErrorTy::IncompatibleTypes,
                message: format!(
                    "Cannot multiply variables of types {} and {}",
                    left.ty(),
                    right.ty()
                ),
            }),
        }
    }
}

impl ops::Div for RegisterValue {
    type Output = Result<Self>;

    #[inline]
    fn div(self, other: Self) -> Self::Output {
        match (self, other) {
            (Self::Int(left), Self::Int(right)) => {
                if right == 0 {
                    Err(RuntimeError {
                        ty: RuntimeErrorTy::DivideByZero,
                        message: "Cannot divide by zero".to_string(),
                    })
                } else {
                    Ok(Self::Int(left / right))
                }
            }

            (left, right) => Err(RuntimeError {
                ty: RuntimeErrorTy::IncompatibleTypes,
                message: format!(
                    "Cannot divide variables of types {} and {}",
                    left.ty(),
                    right.ty()
                ),
            }),
        }
    }
}

impl ops::BitAnd for RegisterValue {
    type Output = Result<Self>;

    #[inline]
    fn bitand(self, other: Self) -> Self::Output {
        match (self, other) {
            (Self::Int(left), Self::Int(right)) => Ok(Self::Int(left & right)),
            (Self::Bool(left), Self::Bool(right)) => Ok(Self::Bool(left & right)),

            (left, right) => Err(RuntimeError {
                ty: RuntimeErrorTy::IncompatibleTypes,
                message: format!(
                    "Cannot bitwise and variables of types {} and {}",
                    left.ty(),
                    right.ty()
                ),
            }),
        }
    }
}

impl ops::BitOr for RegisterValue {
    type Output = Result<Self>;

    #[inline]
    fn bitor(self, other: Self) -> Self::Output {
        match (self, other) {
            (Self::Int(left), Self::Int(right)) => Ok(Self::Int(left | right)),
            (Self::Bool(left), Self::Bool(right)) => Ok(Self::Bool(left | right)),

            (left, right) => Err(RuntimeError {
                ty: RuntimeErrorTy::IncompatibleTypes,
                message: format!(
                    "Cannot bitwise or variables of types {} and {}",
                    left.ty(),
                    right.ty()
                ),
            }),
        }
    }
}

impl ops::BitXor for RegisterValue {
    type Output = Result<Self>;

    #[inline]
    fn bitxor(self, other: Self) -> Self::Output {
        match (self, other) {
            (Self::Int(left), Self::Int(right)) => Ok(Self::Int(left ^ right)),
            (Self::Bool(left), Self::Bool(right)) => Ok(Self::Bool(left ^ right)),

            (left, right) => Err(RuntimeError {
                ty: RuntimeErrorTy::IncompatibleTypes,
                message: format!(
                    "Cannot bitwise xor variables of types {} and {}",
                    left.ty(),
                    right.ty()
                ),
            }),
        }
    }
}

impl ops::Not for RegisterValue {
    type Output = Result<Self>;

    #[inline]
    fn not(self) -> Self::Output {
        match self {
            Self::Int(int) => Ok(Self::Int(!int)),
            Self::Bool(boolean) => Ok(Self::Bool(!boolean)),

            val => Err(RuntimeError {
                ty: RuntimeErrorTy::IncompatibleTypes,
                message: format!("Cannot apply not to variable of type {}", val.ty()),
            }),
        }
    }
}

impl std::cmp::PartialOrd for RegisterValue {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Self::Int(left), Self::Int(right)) => Some(left.cmp(right)),
            (Self::Bool(left), Self::Bool(right)) => Some(left.cmp(right)),
            (Self::String(left), Self::String(right)) => Some(left.cmp(right)),
            (Self::Pointer(left), Self::Pointer(right)) => Some(left.cmp(right)),

            (_, _) => None,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum CachedValue {
    String(GcStr),
    Int(AllocId),
    Bool(AllocId),
    Pointer(AllocId),
}

impl std::fmt::Display for CachedValue {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::String(string) => write!(fmt, "{:?}", string),
            Self::Int(alloc) | Self::Bool(alloc) | Self::Pointer(alloc) => {
                write!(fmt, "{:#p}", alloc)
            }
        }
    }
}

impl CachedValue {
    // TODO: Make this compacting
    pub fn allocate_id(val: Value, id: impl Into<AllocId>, gc: &mut Gc) -> Result<Self> {
        let id = id.into();

        Ok(match val {
            Value::Bool(b) => Self::Bool(Self::alloc(id, gc, b, std::mem::size_of::<bool>())?),
            Value::String(s) => Self::String({ GcStr::new_id(s, id, gc)? }),
            Value::Int(i) => Self::Int(Self::alloc(id, gc, i, std::mem::size_of::<i32>())?),
            Value::Pointer(p) => {
                Self::Pointer(Self::alloc(id, gc, p, std::mem::size_of::<usize>())?)
            }
            _ => unreachable!("Shouldn't be allocating a null space?"),
        })
    }

    fn alloc<T>(
        id: impl Into<AllocId> + Copy,
        gc: &mut Gc,
        val: T,
        size: usize,
    ) -> Result<AllocId> {
        let id = id.into();

        let (alloc_val, alloc_id) = gc.allocate_id(size, id)?;
        assert_eq!(id, alloc_id);

        unsafe {
            gc.write(id, val, Some(&alloc_val))?;
        }

        gc.add_root(alloc_val);

        Ok(id)
    }

    pub fn get_val(&self, gc: &Gc) -> Result<RegisterValue> {
        Ok(match self {
            Self::String(l) => RegisterValue::String(unsafe { std::mem::transmute(l.to_str(gc)?) }),
            Self::Int(l) => RegisterValue::Int(gc.fetch::<i32, AllocId>(l)?),
            Self::Pointer(l) => RegisterValue::Pointer(gc.fetch::<usize, AllocId>(l)?),
            Self::Bool(l) => RegisterValue::Bool(gc.fetch::<bool, AllocId>(l)?),
        })
    }

    pub fn eq(&self, other: &Self, gc: &Gc) -> Result<bool> {
        Ok(match (self, other) {
            (Self::String(l), Self::String(r)) => l.to_str(gc)? == r.to_str(gc)?,
            (Self::Int(l), Self::Int(r)) => {
                gc.fetch::<i32, AllocId>(l)? == gc.fetch::<i32, AllocId>(r)?
            }
            (Self::Bool(l), Self::Bool(r)) => {
                gc.fetch::<bool, AllocId>(l)? == gc.fetch::<bool, AllocId>(r)?
            }
            (Self::Pointer(l), Self::Pointer(r)) => {
                gc.fetch::<usize, AllocId>(l)? == gc.fetch::<usize, AllocId>(r)?
            }
            (_, _) => false,
        })
    }

    pub fn eq_reg(&self, other: &RegisterValue, gc: &Gc) -> Result<bool> {
        Ok(match (self, other) {
            (Self::String(l), RegisterValue::String(r)) => l.to_str(gc)? == r,
            (Self::Int(l), RegisterValue::Int(r)) => gc.fetch::<i32, AllocId>(l)? == r,
            (Self::Bool(l), RegisterValue::Bool(r)) => gc.fetch::<bool, AllocId>(l)? == r,
            (Self::Pointer(l), RegisterValue::Pointer(r)) => gc.fetch::<usize, AllocId>(l)? == r,
            (_, _) => false,
        })
    }

    pub fn not(self, gc: &Gc) -> Result<Self> {
        match self {
            Self::Int(id) => {
                let int = gc.fetch::<i32, AllocId>(id)?;
                unsafe {
                    gc.write(id, !int, None)?;
                }
            }
            Self::Bool(id) => {
                let boolean = gc.fetch::<bool, AllocId>(id)?;
                unsafe {
                    gc.write(id, !boolean, None)?;
                }
            }
            Self::Pointer(id) => {
                let ptr = gc.fetch::<i32, AllocId>(id)?;
                unsafe {
                    gc.write(id, !ptr, None)?;
                }
            }

            val => {
                return Err(RuntimeError {
                    ty: RuntimeErrorTy::IncompatibleTypes,
                    message: format!("Cannot apply not to variable of type {}", val.ty()),
                })
            }
        }

        Ok(self)
    }

    pub fn ty(&self) -> &'static str {
        match self {
            Self::Int(_) => "int",
            Self::Bool(_) => "bool",
            Self::String(_) => "str",
            Self::Pointer(_) => "ptr",
        }
    }

    fn cmp(&self, other: &Self, gc: &Gc) -> Result<Option<std::cmp::Ordering>> {
        match (self, other) {
            (Self::Int(left), Self::Int(right)) => {
                let left = gc.fetch::<i32, AllocId>(left)?;
                let right = gc.fetch::<i32, AllocId>(right)?;

                Ok(Some(left.cmp(&right)))
            }
            (Self::Bool(left), Self::Bool(right)) => {
                let left = gc.fetch::<bool, AllocId>(left)?;
                let right = gc.fetch::<bool, AllocId>(right)?;

                Ok(Some(left.cmp(&right)))
            }
            (Self::String(left), Self::String(right)) => {
                let left = left.to_str(gc)?;
                let right = right.to_str(gc)?;

                Ok(Some(left.cmp(&right)))
            }
            (Self::Pointer(left), Self::Pointer(right)) => {
                let left = gc.fetch::<usize, AllocId>(left)?;
                let right = gc.fetch::<usize, AllocId>(right)?;

                Ok(Some(left.cmp(&right)))
            }

            (_, _) => Ok(None),
        }
    }
}
*/
